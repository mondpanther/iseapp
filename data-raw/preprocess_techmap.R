# This script runs once to create a lightweight tech choices file
library(fst)
library(data.table)

techmap <- fst::read_fst("inst/extdata/techmap.fst")
countrymap <- fst::read_fst("inst/extdata/countrymap.fst")

# Create "All" category
techmap_processed <- countrymap %>%
    select(docdb_family_id) %>%
    distinct() %>%
    mutate(technology = "All") %>%
    bind_rows(techmap)

setDT(techmap_processed)
techmap_processed[, technology := fcase(
    technology == "Any Green technology", "Green Technology",
    technology == "Any battery technology", "Battery Technology",
    technology == "Any Hard to Abate technology", "Hard to Abate Sector Decarbonization",
    default = technology
  )]

# Extract just the unique technology names for UI
tech_choices <- unique(techmap_processed$technology)
saveRDS(tech_choices, "inst/extdata/tech_choices.rds")

# Save processed techmap
fst::write_fst(techmap_processed, "inst/extdata/techmap_processed.fst")

# ==== Grouped Tech Choices ====
other_techs <- c("AI", "Green Technology", "Battery Technology", 
                 "Hard to Abate", "Agriculture & Food", "All Innovations", "Other")

green_classes_d <- unique(techmap_processed$technology[techmap_processed$technology %in% 
  c("Any Green Innovation", "Renewables", "Nuclear Energy", "Carbon Capture", 
    "Sustainable Buildings", "Electric Vehicles", "Other Green Technologies")])

ai_classes_d <- unique(techmap_processed$technology[grepl("^AI", techmap_processed$technology)])
battery_classes_d <- unique(techmap_processed$technology[grepl("Battery", techmap_processed$technology)])

hard_to_abate_classes_d <- unique(techmap_processed$technology[techmap_processed$technology %in%
  c("Steel and Iron", "Cement", "Chemicals", "Aluminum", "Glass and Ceramics")])

agrifood_classes_d <- unique(techmap_processed$technology[techmap_processed$technology %in%
  c("Any Agriculture & Food technology", "Input supply", 
    "Primary food and feed production", "Post-harvest handling & aggregation",
    "Processing", "Distribution/wholesale", "Retail/consumption", "Crosscutting")])

cpc_sections <- unique(techmap_processed$technology[grepl("^CPC Section", techmap_processed$technology)])

grouped_tech_choices <- list(
  "Broad Technology Categories" = as.list(setNames(other_techs, other_techs)),
  "Detailed Green technologies" = as.list(setNames(green_classes_d, green_classes_d)),
  "AI subcategories" = as.list(setNames(ai_classes_d, ai_classes_d)),
  "Detailed Battery technologies" = as.list(setNames(battery_classes_d, battery_classes_d)),
  "Detailed Hard to Abate Sector Decarbonization Technologies" = as.list(setNames(hard_to_abate_classes_d, hard_to_abate_classes_d)),
  "Agriculture & Food technology" = as.list(setNames(agrifood_classes_d, agrifood_classes_d)),
  "CPC Sections" = as.list(setNames(cpc_sections, cpc_sections))
)

saveRDS(grouped_tech_choices, "inst/extdata/grouped_tech_choices.rds")

# ==== Grouped Country Choices ====
available_iso2 <- sort(unique(na.omit(countrycode::codelist$iso2c)))
iso_ref <- unique(countrycode::codelist[, c("iso2c", "country.name.en")])
match_idx <- match(available_iso2, iso_ref$iso2c)
valid <- !is.na(match_idx)
vals <- available_iso2[valid]
labs <- iso_ref$country.name.en[match_idx[valid]]
ord <- order(tolower(labs))
country_choices <- setNames(vals[ord], labs[ord])

# Define country groups (EXACTLY as in old app)
all_countries <- sort(unique(na.omit(countrycode::codelist$iso2c)))
lmics <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG",
           "BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI",
           "CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE",
           "GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ",
           "KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML",
           "MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG",
           "MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC",
           "SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO",
           "TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")

lmics_excl_china <- setdiff(lmics, "CN")
eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
                  "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
                  "SI","ES","SE")
hic <- setdiff(all_countries, lmics)

# Create grouped choices EXACTLY as in old app
grouped_choices <- list(
  "Predefined Groups" = lapply(c("All countries", "LMICs", "LMICs (excl. China)", "EU countries", "High income countries"), 
                               function(name) setNames(name, name)),
  "Individual Countries" = as.list(country_choices)
)
names(grouped_choices[["Predefined Groups"]]) <- c("All countries", "LMICs", "LMICs (excl. China)", "EU countries", "High income countries")

saveRDS(grouped_choices, "inst/extdata/grouped_country_choices.rds")
