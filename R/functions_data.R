
# Get prepdata path for pre-computed data
prepdata_path <- "inst/extdata/prepdata"

localpath_fname <- function(fname) {
  if (!is.null(iseapp_local_path)) {
    fname <- sub("^/+", "", fname)
    return(file.path(iseapp_local_path, fname))
  }
  return("")
}

# Initialize placeholders for deferred data
# These will be populated by the server on session start
techmap <- NULL
countrymap <- NULL
regionmap <- NULL
regionmap_available <- FALSE

#' Load big datasets
#'
#' @return A list containing techmap, countrymap, and regionmap
#' @export
#' @keywords internal
load_big_datasets <- function() {
  result <- list()

  # Load techmap
  # pp <- "inst/extdata/techmap.fst"
  pp <- "inst/extdata/techmap_processed.fst"
  result$techmap <- read_fst(pp)

  # Load countrymap
  pp <- "inst/extdata/countrymap.fst"
  result$countrymap <- read_fst(pp)

  # Load regionmap
  pp_region <- "inst/extdata/regionmap.fst"
  result$regionmap <- read_fst(pp_region)

  result
}

# Check if we have precomputed data - if so, we can defer big data loading
has_precomputed_data <- !is.null(prepdata_path) && dir.exists(prepdata_path)

if (has_precomputed_data) {
  techmap_placeholder <- data.frame(
    docdb_family_id = integer(0),
    technology = character(0)
  )
  techmap <- techmap_placeholder
  countrymap <- data.frame(
    docdb_family_id = integer(0),
    ctry_code = character(0)
  )
  regionmap <- NULL
  regionmap_available <- FALSE
}

green_classes <- c("Green Technology","Green Energy", "Green Transport", "Circular Economy", "Green Manufacturing",
                   "Adaptation", "Green Housing", "Green ICT", "Green Agriculture",
                   "GHG Capture")

battery_classes=c("Battery Technology", "Lithium Extraction & Processing", "Graphite & Carbon Materials", "Cathode Materials", "Anode Materials",
                  "Electrolytes & Additives", "Separators", "Battery Cell Design & Assembly", "Battery Management Systems (BMS)", 
                  "Electric Vehicles & Mobility", "Battery Recycling & Recovery")


hard_to_abate_classes=c("Hard to Abate Sector Decarbonization", "Aviation Decarbonisation", "Cement & Concrete Decarbonisation",
                        "Chemicals & Plastics Decarbonisation", "Shipping Decarbonisation",
                        "Steel & Iron Decarbonisation")

ai_classes=c("AI", "Machine Learning", "Deep Learning", "Natural Language Processing (NLP)", 
             "Computer Vision", "Speech Recognition & Synthesis", "Robotics & Autonomous Systems",
             "Knowledge Representation & Reasoning", "Planning & Decision Making", "Generative AI",
             "Semiconductors", "Cloud & Data Infrastructure", "Data Rettrieval & Processing System", "Platform & Frameworks", "Deployment & Support")


cpc_sections=c( "Human Necessities",
                "Performing Operations; Transporting ",
                "Chemistry; Metallurgy ",
                "Textiles; Paper",
                "Fixed Constructions",
                "Mechanical Engineering; Lighting; Heating; Weapons; Blasting",
                "Physics",
                "Electricity",
                 "General tagging of new or cross-sectional technology" )

agrifood_classes=c("Any Agriculture & Food technology", "Input supply", "Primary food and feed production",
                   "Post-harvest handling & aggregation", "Processing", "Distribution/wholesale",
                   "Retail/consumption", "Crosscutting")

# Get all unique technologies from techmap
# When using deferred loading with precomputed data, we use known technology categories
# instead of extracting from techmap (which may be a placeholder)
if (has_precomputed_data) {
  # Known technology categories - these match what's in the precomputed data
  all_techs <- c("All", green_classes, battery_classes, hard_to_abate_classes, ai_classes, cpc_sections, agrifood_classes)
  all_techs <- unique(all_techs)
}

# Create grouped technology choices
# Separate technologies into green, battery, and other categories
green_classes_d        =setdiff(green_classes,"Green Technology")
battery_classes_d      =setdiff(battery_classes,"Battery Technology")
hard_to_abate_classes_d=setdiff(hard_to_abate_classes,"Hard to Abate Sector Decarbonization")
ai_classes_d           =setdiff(ai_classes,"AI")
agrifood_classes_d     =setdiff(agrifood_classes,"Any Agriculture & Food technology")
colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes,cpcsecs=cpc_sections,agrifood=agrifood_classes)
other_techs <- c(setdiff(all_techs, c(green_classes, battery_classes,hard_to_abate_classes,cpc_sections,agrifood_classes)),"Green Technology","Battery Technology","Hard to Abate Sector Decarbonization","Any Agriculture & Food technology")

grouped_techs <- list(
  "Broad Technology Categories"                         = as.list(setNames(other_techs, other_techs)),
  "Detailed Green technologies"                         = as.list(setNames(green_classes_d, green_classes_d)),
  "AI subcategories"                                    = as.list(setNames(ai_classes_d, ai_classes_d)),
  "Detailed Battery technologies"                       = as.list(setNames(battery_classes_d, battery_classes_d)),
  "Detailed Hard to Abate Sector Decarbonization Technologies" = as.list(setNames(hard_to_abate_classes_d, hard_to_abate_classes_d)),
  "Agriculture & Food technology"                       = as.list(setNames(agrifood_classes_d, agrifood_classes_d)),
  "CPC Sections"                                        = as.list(setNames(cpc_sections, cpc_sections))
)

marginal  = list(
  "Marginal Global Returns"   = "istrax_global",
  "Marginal National Returns" = "istrax_nationalkey_2009_2018",
  "Marginal Returns to LMICs" = "istrax_EMDE",
  "Marginal Returns to LMICs (excl. China)" = "istrax_EMDENOCN",
  "Marginal Returns to LMICs (excl. China & India)" = "istrax_EMDENOCNIN",
  "Marginal Returns to HICs"    = "istrax_HIC",
  "Marginal Returns to the EU"  = "istrax_EU",
  "Marginal Returns to the EU"  = "istrax_EU",
  "Marginal Returns to US"      = "istrax_US",
  "Marginal Returns to China"   = "istrax_CN",
  "Marginal Returns to UK"      = "istrax_GB",
  "Marginal Returns to Austria" = "istrax_AT",
  "Marginal Returns to France"  = "istrax_FR"
)

average   = list(
  "Average Global Returns"   = "avstrax_global",
  "Average National Returns" = "avstrax_nationalkey_2009_2018",
  "Average Returns to LMICs" = "avstrax_EMDE",
  "Average Returns to LMICs (excl. China)" = "avstrax_EMDENOCN",
  "Average Returns to LMICs (excl. China & India)" = "avstrax_EMDENOCNIN",
  "Average Returns to HICs"    = "avstrax_HIC",
  "Average Returns to the EU"  = "avstrax_EU",
  "Average Returns to the EU"  = "avstrax_EU",
  "Average Returns to US"      = "avstrax_US",
  "Average Returns to China"   = "avstrax_CN",
  "Average Returns to UK"      = "avstrax_GB",
  "Average Returns to Austria" = "avstrax_AT",
  "Average Returns to France"  = "avstrax_FR"
)
  
spillovers = list(
  "Average Global Spillovers"   = "ev_global",
  "Average National Spillovers" = "ev_nationalkey_2009_2018",
  "Average Spillovers to LMICs" = "ev_EMDE",
  "Average Spillovers to LMICs (excl. China)"         = "ev_EMDENOCN",
  "Average Spillovers to LMICs (excl. China & India)" = "ev_EMDENOCNIN",
  "Average Spillovers to HICs"    = "ev_HIC",
  "Average Spillovers to the EU"  = "ev_EU",
  "Average Spillovers to the EU"  = "ev_EU",
  "Average Spillovers to US"      = "ev_US",
  "Average Spillovers to China"   = "ev_CN",
  "Average Spillovers to UK"      = "ev_GB",
  "Average Spillovers to Austria" = "ev_AT",
  "Average Spillovers to France"  = "ev_FR"
  
)

toflow_choices <- list("Marginal Returns"= marginal,"Average Returns"=average,
                       "Average Spillovers"=spillovers)

get_available_iso2 <- function() {
  candidates <- c("country_code", "iso2c", "iso2")
  if (exists("patchar_countrymap")) {
    for (cn in candidates) {
      if (!is.null(patchar_countrymap[[cn]])) {
        return(sort(unique(na.omit(patchar_countrymap[[cn]]))))
      }
    }
  }
  if (exists("techmap")) {
    for (cn in candidates) {
      if (!is.null(techmap[[cn]])) {
        return(sort(unique(na.omit(techmap[[cn]]))))
      }
    }
  }
  sort(unique(na.omit(countrycode::codelist$iso2c)))
}

available_iso2 <- get_available_iso2()
iso_ref <- unique(countrycode::codelist[, c("iso2c", "country.name.en", "region")])
match_idx <- match(available_iso2, iso_ref$iso2c)
valid <- !is.na(match_idx)
vals <- available_iso2[valid]
labs <- iso_ref$country.name.en[match_idx[valid]]
ord <- order(tolower(labs))
country_choices <- setNames(vals[ord], labs[ord])

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

group_definitions <- list(
  "All countries" = all_countries,
  "LMICs" = lmics,
  "LMICs (excl. China)" = lmics_excl_china,
  "EU countries" = eu_countries,
  "High income countries" = hic
)

grouped_choices <- list(
  "Predefined Groups" = lapply(names(group_definitions), function(name) setNames(name, name)),
  "Individual Countries" = as.list(country_choices)
)
names(grouped_choices[["Predefined Groups"]]) <- names(group_definitions)

default_country <- if ("VN" %in% vals) "VN" else if (length(vals) > 0) vals[1] else NA_character_


expand_country_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(group_definitions)) {
      return(group_definitions[[x]])
    } else {
      return(x)
    }
  }))
  unique(expanded)
}

# Region definitions for Region Explorer tab
# UK NUTS1 regions (can be expanded in the future)
uk_regions <- c(
  "UKC" = "North East England",
  "UKD" = "North West England",
  "UKE" = "Yorkshire and The Humber",
  "UKF" = "East Midlands",
  "UKG" = "West Midlands",
  "UKH" = "East of England",
  "UKI" = "London",
  "UKJ" = "South East England",
  "UKK" = "South West England",
  "UKL" = "Wales",
  "UKM" = "Scotland",
  "UKN" = "Northern Ireland"
)

# Create region choices (code -> name mapping)
region_choices <- setNames(names(uk_regions), uk_regions)

# Region group definitions
region_group_definitions <- list(
  "All UK regions" = names(uk_regions)
)

# Create grouped region choices
grouped_region_choices <- list(
  "Predefined Groups" = list("All UK regions" = "All UK regions"),
  "Individual Regions" = as.list(region_choices)
)

# Function to expand region selection (similar to country)
expand_region_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(region_group_definitions)) {
      return(region_group_definitions[[x]])
    } else {
      return(x)
    }
  }))
  unique(expanded)
}

# Function to get region display name
get_region_name <- function(code) {
  if (code %in% names(uk_regions)) {
    return(uk_regions[code])
  }
  return(code)
}