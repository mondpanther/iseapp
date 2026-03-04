# data-raw/build_ui_data.R
# Run this after build_parquet_database.R
# Reads the full parquet database and builds all UI lookup objects,
# then saves them as internal package data via usethis::use_data(internal = TRUE).
# The resulting R/sysdata.rda is auto-loaded for all package functions.

library(dplyr)
library(arrow)
library(countrycode)
library(DBI)
library(duckdb)
library(sf)

use_s3 <- FALSE  # set TRUE for S3, FALSE for local
parquet_path <- if (use_s3) {
  "https://iseapp-database.s3.us-east-2.amazonaws.com/patent_database.parquet"
} else {
  normalizePath("inst/extdata/patent_database.parquet")
}
# Open via DuckDB — same pattern as the app
con <- duckdb::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf(
  "CREATE VIEW patent_database AS SELECT * FROM read_parquet('%s')",
  parquet_path
))

# ============================================================
# 1. FIRMS
# Pull the firm names directly from the parquet
# (already filtered at build time, but we read what's there)
# ============================================================
cat("Extracting firms...\n")

firm_lookup       <- arrow::read_parquet("inst/extdata/firm_lookup.parquet")
patents_x_firm    <- arrow::read_parquet("inst/extdata/patents_x_firm.parquet")
firm_values       <- sort(unique(firm_lookup$firm))
firm_sector_values <- sort(unique(na.omit(firm_lookup$firm_sector)))

firm_choices <- c(
  list("All Firms" = "All", "No Firm" = "None"),
  setNames(as.list(firm_values), firm_values)
)
# firm_sector_choices <- setNames(firm_sector_values, firm_sector_values)

cat("  ✓", length(firm_values), "firms\n")
cat("  ✓", length(firm_sector_values), "firm sectors\n")

# ============================================================
# 2. COUNTRIES
# Pull ISO2 codes actually present in the data
# ============================================================
cat("Extracting countries...\n")
available_iso2 <- DBI::dbGetQuery(con,
  "SELECT DISTINCT ctry_code FROM patent_database WHERE ctry_code IS NOT NULL ORDER BY ctry_code"
) |> dplyr::pull(ctry_code)

iso_ref <- unique(countrycode::codelist[, c("iso2c", "country.name.en", "region")])
match_idx <- match(available_iso2, iso_ref$iso2c)
valid     <- !is.na(match_idx)
vals      <- available_iso2[valid]
labs      <- iso_ref$country.name.en[match_idx[valid]]
ord       <- order(tolower(labs))
country_choices <- setNames(vals[ord], labs[ord])

cat("  ✓", length(vals), "countries matched to ISO names\n")

# ============================================================
# 3. REGIONS
# Pull NUTS1 region codes actually present in the data
# ============================================================
cat("Extracting regions...\n")

patents_x_region <- arrow::read_parquet("inst/extdata/patents_x_region.parquet")
region_lookup <- arrow::read_parquet("inst/extdata/region_lookup.parquet")
available_regions <- patents_x_region |>
  distinct(region_code) |>
  dplyr::left_join(region_lookup, by = "region_code") |>
  arrange(region_code)

# Named vector: code -> display name (for selectize label=name, value=code)
uk_regions <- setNames(available_regions$region_name, available_regions$region_code)
# gives: c(UKC = "North East England", ...)  ✓
region_choices <- setNames(names(uk_regions), uk_regions)
# gives: c("North East England" = "UKC", ...)  ✓

cat("  ✓", length(uk_regions), "regions\n")

# ============================================================
# 4. TOFLOW COLUMNS
# Detect which istrax/avstrax/ev columns are actually in the parquet
# ============================================================
cat("Detecting return flow columns...\n")

parquet_cols <- DBI::dbGetQuery(con, 
  "SELECT column_name FROM information_schema.columns WHERE table_name = 'patent_database'"
) |> dplyr::pull(column_name)
flow_cols <- parquet_cols[grepl("^(is|av|ev)_", parquet_cols)]

cat("  ✓", length(flow_cols), "flow columns found\n")

# ============================================================
# 5. TECHNOLOGY GROUPINGS
# Hand-crafted umbrella groups stay as-is.
# Any technology in the DB not in a known group -> "Broad Technology Categories"
# ============================================================
cat("Building technology groupings...\n")

patents_x_tech <- arrow::read_parquet("inst/extdata/patents_x_tech.parquet")
all_db_techs <- sort(unique(patents_x_tech$technology))
tech_lookup <- arrow::read_parquet("inst/extdata/tech_lookup.parquet")
novel_techs <- tech_lookup |> filter(tech_group == "Other") |> pull(technology) |> unique() |> sort()

cat("  ✓", length(all_db_techs), "distinct technologies in database\n")

# Umbrella labels that appear as top-level choices
umbrella_labels <- c(
  "All",
  "Green Technology", "Battery Technology",
  "Hard to Abate Sector Decarbonization", "AI",
  "Any Agriculture & Food technology"
)

# Derive class vectors from tech_lookup
green_classes <- tech_lookup |> filter(tech_group == "Green Technology") |>
  pull(technology)
battery_classes <- tech_lookup |> filter(tech_group == "Battery Technology") |>
  pull(technology)
hard_to_abate_classes <- tech_lookup |> filter(tech_group == "Hard to Abate Sector Decarbonization") |>
  pull(technology)
ai_classes <- tech_lookup |> filter(tech_group == "AI") |> pull(technology)
agrifood_classes <- tech_lookup |> filter(tech_group == "Any Agriculture & Food technology") |> pull(technology)
cpc_sections <- tech_lookup |>
  dplyr::filter(tech_group == technology) |>
  dplyr::filter(!technology %in% umbrella_labels) |>
  dplyr::pull(technology)
novel_techs <- tech_lookup |>
  filter(tech_group == "Other") |>
  pull(technology) |>
  sort()

# Any DB technology not in an assigned group -> broad category
# Read novel techs directly from parquet - tech_group = "Other" was assigned at build time
cat("  ✓", length(novel_techs), "novel/unassigned technologies -> Broad Technology Categories\n")

# Subclass vectors (umbrella label removed)
green_classes_d <- setdiff(green_classes, c("Green Technology", "Any Green technology"))
battery_classes_d <- setdiff(battery_classes, "Battery Technology")
hard_to_abate_classes_d <- setdiff(hard_to_abate_classes, "Hard to Abate Sector Decarbonization")
ai_classes_d <- setdiff(ai_classes, "AI")
agrifood_classes_d <- setdiff(agrifood_classes, "Any Agriculture & Food technology")

# Broad category = umbrella labels + anything novel from DB
broad_techs <- unique(c(
  umbrella_labels,
  ai_classes_d,
  novel_techs
))

grouped_techs <- list(
  "Broad Technology Categories" =
    as.list(setNames(broad_techs, broad_techs)),
  "Detailed Green technologies" =
    as.list(setNames(green_classes_d, green_classes_d)),
  "AI subcategories" =
    as.list(setNames(ai_classes_d, ai_classes_d)),
  "Detailed Battery technologies" =
    as.list(setNames(battery_classes_d, battery_classes_d)),
  "Detailed Hard to Abate Sector Decarbonization Technologies" =
    as.list(setNames(hard_to_abate_classes_d, hard_to_abate_classes_d)),
  "Agriculture & Food technology" =
    as.list(setNames(agrifood_classes_d, agrifood_classes_d)),
  "CPC Sections" =
    as.list(setNames(cpc_sections, cpc_sections))
)

colorings <- list(
  green       = green_classes,
  battery     = battery_classes,
  hard_to_abate = hard_to_abate_classes,
  ai          = ai_classes,
  cpcsecs     = cpc_sections,
  agrifood    = agrifood_classes
)

# Reverse map: sub-technology -> umbrella name (used in server aggregation)
tech_umbrella_map <- c(
  setNames(rep("Green Technology",                    length(green_classes_d)),         green_classes_d),
  setNames(rep("Battery Technology",                  length(battery_classes_d)),        battery_classes_d),
  setNames(rep("Hard to Abate Sector Decarbonization",length(hard_to_abate_classes_d)), hard_to_abate_classes_d),
  setNames(rep("AI",                                  length(ai_classes_d)),             ai_classes_d),
  setNames(rep("Any Agriculture & Food technology",   length(agrifood_classes_d)),       agrifood_classes_d),
  setNames(cpc_sections, cpc_sections)
)

# ============================================================
# 6. COUNTRY GROUPS
# ============================================================
cat("Building country groups...\n")

country_groups_raw <- arrow::read_parquet("inst/extdata/country_lookup.parquet")

group_definitions <- list(
  "All countries"         = country_choices |> unname(),
  "LMICs"                 = country_groups_raw |> dplyr::filter(is_lmic)            |> dplyr::pull(ctry_code),
  "LMICs (excl. China)"   = country_groups_raw |> dplyr::filter(is_lmic_excl_china) |> dplyr::pull(ctry_code),
  "EU countries"          = country_groups_raw |> dplyr::filter(is_eu)              |> dplyr::pull(ctry_code),
  "High income countries" = country_groups_raw |> dplyr::filter(is_hic)             |> dplyr::pull(ctry_code)
)

grouped_choices <- list(
  "Predefined Groups" = as.list(setNames(names(group_definitions), names(group_definitions))),
  "Individual Countries" = as.list(country_choices)
)

# ============================================================
# 7. REGION GROUPS
# ============================================================
region_group_definitions <- list(
  "All UK regions" = names(uk_regions)
)

grouped_region_choices <- list(
  "Predefined Groups"  = list("All UK regions" = "All UK regions"),
  "Individual Regions" = as.list(region_choices)
)

# ============================================================
# 8. TOFLOW CHOICES (labelled lists)
# Values validated against parquet cols detected above.
# NOTE: Duplicate "Marginal/Average Returns to the EU" keys fixed here.
# ============================================================
cat("Building toflow choices...\n")



marginal <- list(
  "Marginal Global Returns"                              = "is_global",
  "Marginal National Returns"                            = "is_nationalkey_2009_2018",
  "Marginal Returns to LMICs"                            = "is_emde",
  "Marginal Returns to LMICs (excl. China)"              = "is_emdenocn",
  "Marginal Returns to LMICs (excl. China & India)"      = "is_emdenocnin",
  "Marginal Returns to HICs"                             = "is_hic",
  "Marginal Returns to the EU"                           = "is_eu",
  "Marginal Returns to US"                               = "is_us",
  "Marginal Returns to China"                            = "is_cn",
  "Marginal Returns to UK"                               = "is_gb",
  "Marginal Returns to Austria"                          = "is_at",
  "Marginal Returns to France"                           = "is_fr",
  "Marginal Returns to India"                            = "is_in"
)

average <- list(
  "Average Global Returns"                               = "av_global",
  "Average National Returns"                             = "av_nationalkey_2009_2018",
  "Average Returns to LMICs"                             = "av_emde",
  "Average Returns to LMICs (excl. China)"               = "av_emdenocn",
  "Average Returns to LMICs (excl. China & India)"       = "av_emdenocnin",
  "Average Returns to HICs"                              = "av_hic",
  "Average Returns to G7"                                = "av_g7",
  "Average Returns to the EU"                            = "av_eu",
  "Average Returns to US"                                = "av_us",
  "Average Returns to China"                             = "av_cn",
  "Average Returns to UK"                                = "av_gb",
  "Average Returns to Austria"                           = "av_at",
  "Average Returns to France"                            = "av_fr",
  "Average Returns to India"                             = "av_in"
)

spillovers <- list(
  "Average Global Spillovers"                            = "ev_global",
  "Average National Spillovers"                          = "ev_nationalkey_2009_2018",
  "Average Spillovers to LMICs"                          = "ev_emde",
  "Average Spillovers to LMICs (excl. China)"            = "ev_emdenocn",
  "Average Spillovers to LMICs (excl. China & India)"    = "ev_emdenocnin",
  "Average Spillovers to HICs"                           = "ev_hic",
  "Average Spillovers to the EU"                         = "ev_eu",
  "Average Spillovers to US"                             = "ev_us",
  "Average Spillovers to China"                          = "ev_cn",
  "Average Spillovers to UK"                             = "ev_gb",
  "Average Spillovers to Austria"                        = "ev_at",
  "Average Spillovers to France"                         = "ev_fr",
  "Average Spillovers to India"                          = "ev_in"
)

toflow_choices <- list(
  "Marginal Returns"  = marginal,
  "Average Returns"   = average,
  "Average Spillovers" = spillovers
)

# Warn if any declared value is missing from the actual parquet schema
all_declared_cols <- unlist(toflow_choices, use.names = FALSE)
missing_cols <- setdiff(all_declared_cols, flow_cols)
if (length(missing_cols) > 0) {
  warning(
    "These toflow columns are declared but NOT found in parquet:\n  ",
    paste(missing_cols, collapse = "\n  ")
  )
} else {
  cat("  ✓ All toflow columns validated against parquet schema\n")
}

# ============================================================
# 9. HELPER VALUES
# Miscellaneous scalars used in the UI / server
# ============================================================
cat("Building helper values...\n")

default_country <- if ("VN" %in% vals) "VN" else vals[1]
default_region  <- if (length(names(uk_regions)) > 0) names(uk_regions)[1] else NA_character_

cat("  ✓ default_country:", default_country, "\n")
cat("  ✓ default_region: ", default_region,  "\n")

# ============================================================
# 10. PRE-COMPUTED RTA BASELINES
# ============================================================
cat("Pre-computing allinnos baselines...\n")

patent_database <- arrow::read_parquet("inst/extdata/patent_database.parquet",
                                        col_select = c("docdb_family_id", "ctry_code"))

allinnos_baseline <- patent_database |>
  left_join(patents_x_firm, by = "docdb_family_id", relationship = "many-to-many") |>
  count(ctry_code, firm, name = "allinnos")

sum_allinnos_baseline <- allinnos_baseline |>
  group_by(ctry_code) |>
  summarise(sum_allinnos = sum(allinnos), .groups = "drop")

sum_allinnos_firm_baseline <- allinnos_baseline |>
  group_by(firm) |>
  summarise(sum_allinnos = sum(allinnos), .groups = "drop")

cat("  ✓", nrow(allinnos_baseline), "ctry/firm combinations pre-computed\n")
cat("  ✓", nrow(sum_allinnos_baseline), "ctry combinations pre-computed\n")

# ============================================================
# 11. PRE-COMPUTED REGION RTA BASELINES
# ============================================================
cat("Pre-computing region allinnos baselines...\n")

allinnos_region_baseline <- patents_x_region |>
  left_join(patents_x_firm, by = "docdb_family_id", relationship = "many-to-many") |>
  count(region_code, firm, name = "allinnos")

sum_allinnos_region_baseline <- allinnos_region_baseline |>
  group_by(region_code) |>
  summarise(sum_allinnos = sum(allinnos), .groups = "drop")

sum_allinnos_region_firm_baseline <- allinnos_region_baseline |>
  group_by(firm) |>
  summarise(sum_allinnos = sum(allinnos), .groups = "drop")

cat("  ✓", nrow(allinnos_region_baseline), "region/firm combinations pre-computed\n")
cat("  ✓", nrow(sum_allinnos_region_baseline), "region combinations pre-computed\n")

# ============================================================
# 12. UK NUTS1 BOUNDARIES
# Download from GitHub and merge all four nations
# ============================================================
# cat("Downloading UK NUTS1 boundaries...\n")

# # England & Wales from martinjc/UK-GeoJSON
# ew_url <- "https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/eurostat/ew/nuts1.json"
# uk_nuts1_sf <- sf::st_read(ew_url, quiet = TRUE)
# cat("  ✓ England & Wales loaded\n")

# # Standardise NUTS code column to NUTS1CD
# nuts_col <- names(uk_nuts1_sf)[grepl("NUTS.*CD", names(uk_nuts1_sf), ignore.case = TRUE)][1]
# if (nuts_col != "NUTS1CD") {
#   uk_nuts1_sf <- uk_nuts1_sf |> dplyr::rename(NUTS1CD = !!nuts_col)
# }

# existing_codes <- uk_nuts1_sf$NUTS1CD

# # Scotland (UKM) and Northern Ireland (UKN) from ONS Open Geography Portal
# if (!"UKM" %in% existing_codes || !"UKN" %in% existing_codes) {
#   ons_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/NUTS_Level_1_January_2018_Boundaries_UK/FeatureServer/0/query?where=1%3D1&outFields=nuts118cd,nuts118nm&f=geojson"
#   tryCatch({
#     ons_sf <- sf::st_read(ons_url, quiet = TRUE) |>
#       dplyr::rename(NUTS1CD = nuts118cd, NUTS1NM = nuts118nm) |>
#       dplyr::filter(NUTS1CD %in% c("UKM", "UKN"))

#     # Keep only columns present in uk_nuts1_sf to allow rbind
#     shared_cols <- intersect(names(uk_nuts1_sf), names(ons_sf))
#     uk_nuts1_sf <- rbind(
#       uk_nuts1_sf[, shared_cols],
#       ons_sf[, shared_cols]
#     )
#     cat("  ✓ Scotland and Northern Ireland added from ONS\n")
#   }, error = function(e) {
#     stop("Could not load Scotland/NI from ONS: ", e$message,
#          "\nAll 12 NUTS1 regions are required.")
#   })
# }

# # Ensure consistent CRS
# uk_nuts1_sf <- sf::st_transform(uk_nuts1_sf, 4326)
# cat("  ✓ uk_nuts1_sf ready with", nrow(uk_nuts1_sf), "regions\n")

# ============================================================
# 13. SAVE AS INTERNAL PACKAGE DATA
# usethis::use_data(internal = TRUE) writes ALL listed objects
# into R/sysdata.rda, which is auto-loaded for every package function.
# Re-running this script will overwrite R/sysdata.rda.
# ============================================================
cat("\nSaving to R/sysdata.rda via usethis::use_data()...\n")

usethis::use_data(
  # Firms
  firm_choices,
  # firm_sector_choices,
  # Countries
  country_choices,
  group_definitions,
  grouped_choices,
  default_country,
  # Regions
  uk_regions,
  region_choices,
  region_group_definitions,
  grouped_region_choices,
  default_region,
  # Technologies
  all_db_techs,
  novel_techs,
  green_classes,        battery_classes,   hard_to_abate_classes,
  ai_classes,           cpc_sections,      agrifood_classes,
  green_classes_d,      battery_classes_d, hard_to_abate_classes_d,
  ai_classes_d,         agrifood_classes_d,
  grouped_techs,
  colorings,
  tech_umbrella_map,
  # Flow choices
  toflow_choices,
  flow_cols,
  allinnos_baseline,
  sum_allinnos_baseline,
  sum_allinnos_firm_baseline,
  allinnos_region_baseline,
  sum_allinnos_region_baseline,
  sum_allinnos_region_firm_baseline,
  # Spatial
  # uk_nuts1_sf,
  internal  = TRUE,
  overwrite = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)

cat("\n=== DONE ===\n")
cat("R/sysdata.rda written. pkgload::load_all() then innovationStrategyExplorer::runAppPackage() to launch the app.\n")
