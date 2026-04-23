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

# Build grouped choices for selectizeInput (sector -> firm hierarchy, like countries)
firm_sector_groups <- firm_lookup |>
  dplyr::select(firm_sector, firm) |>
  dplyr::distinct() |>
  dplyr::arrange(firm_sector, firm) |>
  split(~firm_sector) |>
  lapply(function(df) as.list(setNames(df$firm, df$firm)))

firm_grouped_choices <- c(
  list("Filter Options" = list("No firm filter" = "No firm filter")),
  list("Sector Groups" = as.list(setNames(firm_sector_values, firm_sector_values))),
  firm_sector_groups
)

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
  "All categories",
  "All innovations",
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

# Broad category = umbrella labels + anything novel from DB + CPC sections
broad_techs <- unique(c(
  umbrella_labels,
  ai_classes_d,
  novel_techs,
  cpc_sections
))

# "All categories" expands to every selectable broad tech (minus "All categories" itself)
all_broad_techs <- setdiff(broad_techs, "All categories")

# Remove "All categories" from the main Broad group — it's re-added as a
# relabelled "Include all categories" entry in the Actions group at the end.
broad_techs_display <- setdiff(broad_techs, "All categories")

grouped_techs <- list(
  "Broad Technology Categories" =
    as.list(setNames(broad_techs_display, broad_techs_display)),
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
  "CPC Sections & Other Categories" =
    as.list(setNames(cpc_sections, cpc_sections)),
  # Bulk-action pseudo-group at the very end. selectize renders the group
  # name as a bold header, which visually separates it from the categories.
  "\u2500\u2500\u2500 Actions" = list(
    # Label shown in dropdown -> internal value used by the server.
    # Keeping the internal value "All categories" preserves the existing
    # expansion logic in build_tech_filter_v2 / build_tech_bool_v2.
    "Include all categories" = "All categories",
    "Clear all categories"   = "__CLEAR_TECHS__"
  )
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
# 11. PRE-COMPUTED Some inglobe tab stuff
# ============================================================

df_processed <- arrow::read_parquet("inst/extdata/inglobe_processed.parquet")

# Precompute metadata
metadata <- df_processed |>
  dplyr::distinct(sce_country, sce_tech_display, tech_group, sample_size)

# Precompute wave range
wave_range <- data.frame(
  min_wave = min(df_processed$wave),
  max_wave = max(df_processed$wave)
)

# Precompute tech group definitions
tech_group_definitions <- list(
  "All" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "All"])),
  "Green" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "Green"])),
  "Non-Green" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "Non-Green"]))
)

# Precompute distinct docdb count — used in the welcome-page ticker.
cat("Counting distinct docdbs in patent_database...\n")
n_docdbs_total <- as.numeric(
  arrow::open_dataset("inst/extdata/patent_database.parquet") |>
    dplyr::summarise(n = dplyr::n_distinct(docdb_family_id)) |>
    dplyr::collect() |>
    dplyr::pull(n)
)
cat(sprintf("  ✓ n_docdbs_total = %s\n",
            format(n_docdbs_total, big.mark = ",")))

# ============================================================
# 12. PRE-COMPUTED TECH x CPC-SUBCLASS COUNTS (About page wordclouds)
# ============================================================
# For each technology label in the UI tree, count distinct docdbs per CPC
# 4-letter subclass (e.g. "H01M") by joining techmap x cpcs bridge tables
# from .bigdata/. Restricted to families that actually appear in the app's
# patent_database.parquet. Subclass titles are shortened to <= 4 words for
# wordcloud rendering.

cat("Pre-computing tech x CPC-subclass counts...\n")

# Manual overrides for subclasses whose CPC title is uninformative
# (mostly umbrella codes that say "TECHNICAL SUBJECTS COVERED BY ...").
.cpc_title_overrides <- c(
  Y10S = "Legacy USPC cross-reference art",
  Y10T = "Legacy USPC classification subjects",
  Y02A = "Climate change adaptation",
  Y02B = "Climate mitigation in buildings",
  Y02C = "GHG capture, storage, sequestration",
  Y02D = "ICT climate mitigation",
  Y02E = "Clean energy generation",
  Y02P = "Climate mitigation in production",
  Y02T = "Transport climate mitigation",
  Y02W = "Waste treatment climate mitigation",
  Y04S = "Smart grids / power systems"
)

# Shorten a CPC title to a readable cloud label: drop boilerplate prefixes
# like "TECHNICAL SUBJECTS COVERED BY", cut at the first semicolon/colon,
# title-case, and cap at ~55 chars on a word boundary (append ellipsis if
# truncated). Falls back to the subclass code for empty/NA titles.
.short_title <- function(code, raw) {
  if (!is.na(code) && code %in% names(.cpc_title_overrides))
    return(unname(.cpc_title_overrides[code]))
  if (is.na(raw) || !nzchar(raw)) return(code)
  s <- raw
  # Drop bracketed editorial content (already done at CSV-build, defensive).
  s <- gsub("\\{[^{}]*\\}", "", s)
  s <- gsub("\\([^()]*\\)", "", s)
  # Cut at the first semicolon/colon — rest is usually notes/refs.
  s <- sub("[;:].*$", "", s)
  # Drop generic leading boilerplate.
  s <- sub("^\\s*TECHNICAL SUBJECTS COVERED BY[^A-Z]*", "", s,
           ignore.case = TRUE, perl = TRUE)
  s <- sub("^\\s*SPECIFIC USE[S]? OF\\s+", "", s, ignore.case = TRUE)
  s <- gsub("\\s+", " ", trimws(s))
  if (!nzchar(s)) return(code)
  # Title case, but leave short all-caps acronyms (<=4 chars) alone.
  parts <- strsplit(s, " ", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(w) {
    if (nchar(w) <= 4 && toupper(w) == w && grepl("[A-Z]", w)) w
    else paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
  }, character(1))
  s <- paste(parts, collapse = " ")
  # Cap at ~55 chars on a word boundary.
  if (nchar(s) > 55) {
    cut <- regmatches(s, regexpr("^.{1,55}\\b", s, perl = TRUE))
    s <- paste0(trimws(cut), "\u2026")
  }
  s
}

subclass_titles <- data.table::fread(
  "classifications/cpc_subclass_titles.csv",
  data.table = TRUE
)
subclass_titles[, title_short := mapply(.short_title, subclass, title,
                                        USE.NAMES = FALSE)]

# Families in the app's final universe
final_fams <- arrow::open_dataset("inst/extdata/patent_database.parquet") |>
  dplyr::select(docdb_family_id) |>
  dplyr::distinct() |>
  dplyr::collect()
final_fams <- final_fams$docdb_family_id

techmap_dt <- fst::read_fst(".bigdata/techmap.fst", as.data.table = TRUE)
cpcs_dt    <- fst::read_fst(".bigdata/cpcs.fst",    as.data.table = TRUE)
techmap_dt <- techmap_dt[docdb_family_id %in% final_fams]
cpcs_dt    <- cpcs_dt[docdb_family_id %in% final_fams]
cpcs_dt[, subclass := substr(cpc_class_symbol, 1, 4)]
cpcs_dt <- unique(cpcs_dt[, .(docdb_family_id, subclass)])

setkey(techmap_dt, docdb_family_id)
setkey(cpcs_dt,    docdb_family_id)
joined <- cpcs_dt[techmap_dt, on = "docdb_family_id", allow.cartesian = TRUE]
joined <- joined[!is.na(subclass)]

# Per-technology counts (sub-techs and CPC sections as they appear in techmap).
tech_subclass_counts <- joined[, .(n_docdb = data.table::uniqueN(docdb_family_id)),
                               by = .(technology, subclass)]

# Umbrella-level rollups: techmap only stores sub-technologies (Green Energy,
# Machine Learning, ...), so labels like "Green Technology", "Battery
# Technology", "AI" — shown in the UI's Broad group — need synthetic rows.
# Use tech_umbrella_map (sub_tech -> umbrella) and keep distinct-family
# counts (docdbs mapped by multiple sub-techs in the same umbrella count
# once, not sum-of-subs).
umbrella_pairs <- data.table::data.table(
  technology = names(tech_umbrella_map),
  umbrella   = unname(tech_umbrella_map)
)
umbrella_pairs <- umbrella_pairs[umbrella != technology]  # skip self-maps
umbrella_counts <- umbrella_pairs[joined, on = "technology",
                                   nomatch = 0L, allow.cartesian = TRUE]
umbrella_counts <- umbrella_counts[
  !is.na(umbrella),
  .(n_docdb = data.table::uniqueN(docdb_family_id)),
  by = .(technology = umbrella, subclass)
]
tech_subclass_counts <- rbind(tech_subclass_counts, umbrella_counts)
# Dedupe: techmap may already carry an umbrella-shaped label (e.g.
# "Any Agriculture & Food technology") so per-tech + rollup can produce two
# rows. Both represent the same docdb set; take the max.
tech_subclass_counts <- tech_subclass_counts[,
  .(n_docdb = max(n_docdb)), by = .(technology, subclass)]

# Diagnostic: which UI labels from grouped_techs still lack coverage?
ui_labels <- unique(unlist(lapply(grouped_techs, names), use.names = FALSE))
ui_labels <- setdiff(ui_labels,
                     c("All categories", "All innovations",
                       "Include all categories", "Clear all categories"))
uncovered <- setdiff(ui_labels, unique(tech_subclass_counts$technology))
if (length(uncovered))
  cat("  NOTE: ", length(uncovered),
      " UI technology labels have no tech_subclass_counts rows:\n    ",
      paste(uncovered, collapse = ", "), "\n", sep = "")

# Attach titles
tech_subclass_counts <- subclass_titles[, .(subclass, title_short)][
  tech_subclass_counts, on = "subclass"]
# Keep at most 50 subclasses per technology (wordcloud readability cap)
data.table::setorder(tech_subclass_counts, technology, -n_docdb)
tech_subclass_counts <- tech_subclass_counts[,
  utils::head(.SD, 50), by = technology]
data.table::setcolorder(tech_subclass_counts,
  c("technology", "subclass", "title_short", "n_docdb"))
# Fall back to the subclass code when no title matched
tech_subclass_counts[is.na(title_short) | title_short == "",
                     title_short := subclass]

rm(techmap_dt, cpcs_dt, joined, final_fams,
   umbrella_pairs, umbrella_counts); gc()

# ----------------------------------------------------------------------------
# tech_defining_cpcs — the CPC codes / prefixes that DEFINE each technology
# (read directly from the classification sources in classifications/). This
# is distinct from tech_subclass_counts: that one says what subclasses the
# docdbs IN a category happen to be co-tagged with; this says what codes
# constitute the category's definition.
# ----------------------------------------------------------------------------
cat("Building tech_defining_cpcs...\n")

# --- ifcreport: (technology, cpc_prefix) rows. Labels get the same renames
#     as 01-build-app-parquets.R so UI labels match techmap keys.
ifc_def <- readxl::read_excel("classifications/ifcreport.xlsx", skip = 1)
names(ifc_def) <- c("technology", "cpc_code", "source")
ifc_def <- ifc_def[!is.na(ifc_def$technology) & !is.na(ifc_def$cpc_code), ]
ifc_def$technology <- trimws(ifc_def$technology)
ifc_def$technology[ifc_def$technology == "Green Technology"]        <- "Any Green technology"
ifc_def$technology[ifc_def$technology == "Green Buildings"]         <- "Green Housing"
ifc_def$technology[ifc_def$technology == "Green Transport"]         <- "Green Transport"
ifc_def$technology[ifc_def$technology == "Artificial Intelligence"] <- "AI"
ifc_def$cpc_code <- gsub("YO2", "Y02", ifc_def$cpc_code)
ifc_def <- ifc_def |>
  tidyr::separate_rows(cpc_code, sep = "\\|") |>
  dplyr::mutate(cpc_code = stringi::stri_replace_all_fixed(trimws(cpc_code), " ", "")) |>
  dplyr::filter(nchar(cpc_code) > 0) |>
  dplyr::select(technology, cpc_code)

# --- Battery (same logic as 01-build)
source("R/functions_extrasectorshelper.R")
bat_def <- battery_df |>
  tidyr::separate_rows(CPC, sep = ";") |>
  dplyr::mutate(cpc_code = stringi::stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  dplyr::filter(nchar(cpc_code) > 0) |>
  dplyr::select(technology, cpc_code)

# --- Hard-to-Abate
hta_def <- readxl::read_excel("classifications/New_Sector_Mapping.xlsx",
                               sheet = "hta_sector") |>
  dplyr::rename(detail = technology, technology = sector) |>
  dplyr::mutate(technology = paste0(technology, " Decarbonisation")) |>
  tidyr::separate_rows(CPC, sep = ";") |>
  dplyr::mutate(cpc_code = stringi::stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  dplyr::filter(nchar(cpc_code) > 0) |>
  dplyr::select(technology, cpc_code)

# --- AI
ai_def <- readxl::read_excel("classifications/New_Sector_Mapping.xlsx",
                              sheet = "AI") |>
  dplyr::rename(CPC = `CPC/IPC Codes`, technology = `Sub-Technology`) |>
  dplyr::filter(!is.na(technology), !is.na(CPC)) |>
  tidyr::separate_rows(CPC, sep = ",") |>
  dplyr::mutate(cpc_code = stringi::stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  dplyr::filter(nchar(cpc_code) > 0) |>
  dplyr::select(technology, cpc_code)

# --- Agriculture & Food
agri_def <- readxl::read_excel(
    "classifications/Agriculture_Food_CPC_Patents_2026-01-22.xlsx", sheet = 1) |>
  dplyr::rename(CPC = `CPC Group/Subgroup`, technology = `Value Chain`) |>
  dplyr::filter(!is.na(technology), !is.na(CPC)) |>
  tidyr::separate_rows(CPC, sep = ";") |>
  dplyr::mutate(cpc_code = stringi::stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  dplyr::filter(nchar(cpc_code) > 0) |>
  dplyr::select(technology, cpc_code)

# --- CPC section umbrellas map to their single-letter prefix.
cpc_section_letter <- c(
  `Human Necessities`                                            = "A",
  `Performing Operations; Transporting`                          = "B",
  `Chemistry; Metallurgy`                                        = "C",
  `Textiles; Paper`                                              = "D",
  `Fixed Constructions`                                          = "E",
  `Mechanical Engineering; Lighting; Heating; Weapons; Blasting` = "F",
  `Physics`                                                      = "G",
  `Electricity`                                                  = "H",
  `General tagging of new or cross-sectional technology`         = "Y"
)
sec_def <- data.frame(
  technology = names(cpc_section_letter),
  cpc_code   = unname(cpc_section_letter),
  stringsAsFactors = FALSE
)

tech_defining_cpcs <- data.table::rbindlist(list(
  ifc_def, bat_def, hta_def, ai_def, agri_def, sec_def
), use.names = TRUE)
data.table::setDT(tech_defining_cpcs)
tech_defining_cpcs <- unique(tech_defining_cpcs[, .(technology, cpc_code)])

# Umbrella rollup: for each sub-tech mapping to an umbrella, add an umbrella
# row with the sub-tech's cpc_code. Uses tech_umbrella_map.
umb_map_dt <- data.table::data.table(
  technology = names(tech_umbrella_map),
  umbrella   = unname(tech_umbrella_map)
)[umbrella != technology]
umb_roll <- umb_map_dt[tech_defining_cpcs, on = "technology",
                        nomatch = 0L, allow.cartesian = TRUE]
umb_roll <- umb_roll[, .(technology = umbrella, cpc_code)]
tech_defining_cpcs <- unique(rbind(tech_defining_cpcs, umb_roll))

# Attach a short title for each code. Two cases:
#   * 4-char subclass match ("A01B", "Y02E"): use subclass_titles directly.
#   * Deeper codes ("Y02P10/14"): derive the 4-char parent subclass and reuse
#     the parent title (the CPC titles dataset only goes to subclass level).
tech_defining_cpcs[, subclass := substr(cpc_code, 1, 4)]
tech_defining_cpcs <- subclass_titles[, .(subclass, title_short)][
  tech_defining_cpcs, on = "subclass"]
tech_defining_cpcs[is.na(title_short) | title_short == "",
                   title_short := subclass]
data.table::setorder(tech_defining_cpcs, technology, cpc_code)
data.table::setcolorder(tech_defining_cpcs,
  c("technology", "cpc_code", "title_short", "subclass"))

cat("  ✓", nrow(tech_defining_cpcs),
    "defining CPC code rows across",
    data.table::uniqueN(tech_defining_cpcs$technology), "technologies\n")

rm(ifc_def, bat_def, hta_def, ai_def, agri_def, sec_def,
   umb_map_dt, umb_roll, subclass_titles); gc()
cat("  ✓", nrow(tech_subclass_counts),
    "tech × subclass rows across",
    data.table::uniqueN(tech_subclass_counts$technology), "technologies\n")

# ============================================================
# 13. SAVE AS INTERNAL PACKAGE DATA
# usethis::use_data(internal = TRUE) writes ALL listed objects
# into R/sysdata.rda, which is auto-loaded for every package function.
# Re-running this script will overwrite R/sysdata.rda.
# ============================================================
cat("\nSaving to R/sysdata.rda via usethis::use_data()...\n")

usethis::use_data(
  # Firms
  firm_grouped_choices,
  firm_sector_groups,
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
  all_broad_techs,
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
  # About-page tech definitions
  tech_subclass_counts,
  tech_defining_cpcs,
  # Welcome-page ticker stats
  n_docdbs_total,
  # InGlobe
  metadata,
  wave_range,
  tech_group_definitions,
  # Spatial
  # uk_nuts1_sf,
  internal  = TRUE,
  overwrite = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)

cat("\n=== DONE ===\n")
cat("R/sysdata.rda written. pkgload::load_all() then innovationStrategyExplorer::runAppPackage() to launch the app.\n")
