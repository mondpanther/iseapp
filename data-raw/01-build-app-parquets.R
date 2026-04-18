# Build Single Parquet Database for ISE App
# This script builds the complete database from raw PATSTAT and WATSON data.
#
# Primary data source: patbis2021/data/fromPATSTAT and fromWATSON
# Classification files: classifications/ folder in this repo
# Legacy data (regionmap, firmmap, inglobe): Dropbox/apps/iseapp
#
# Intermediate big files are cached in .bigdata/

library(data.table)
library(dplyr)
library(arrow)
library(fst)
library(readxl)
library(countrycode)
library(stringr)
library(stringi)
library(tidyr)
library(tictoc)
library(jsonlite)

# ============================================================================
# STEP 0: Setup paths
# ============================================================================
# Locate the user's Dropbox root without hardcoding a machine-specific path.
# Dropbox writes its root location to info.json; see
# https://help.dropbox.com/installs/locate-dropbox-folder
# Override with env var ISEAPP_DROPBOX_DIR if your setup differs.
find_dropbox_dir <- function() {
  override <- Sys.getenv("ISEAPP_DROPBOX_DIR", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    if (!dir.exists(override))
      stop("ISEAPP_DROPBOX_DIR is set but does not exist: ", override)
    return(normalizePath(override, winslash = "/", mustWork = TRUE))
  }

  info_candidates <- if (.Platform$OS.type == "windows") {
    c(file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json"),
      file.path(Sys.getenv("APPDATA"),      "Dropbox", "info.json"))
  } else {
    c("~/.dropbox/info.json", "~/.config/dropbox/info.json")
  }
  info_path <- Filter(file.exists, path.expand(info_candidates))
  if (!length(info_path))
    stop("Could not find Dropbox info.json. Is Dropbox installed? ",
         "Set ISEAPP_DROPBOX_DIR to override.")

  info <- jsonlite::fromJSON(info_path[[1]])
  root <- info$personal$path %||% info$business$path
  if (is.null(root))
    stop("Dropbox info.json did not contain a personal or business path.")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

dropbox_dir <- find_dropbox_dir()
patbis_dir  <- file.path(dropbox_dir, "patbis2021", "data")
fromPATSTAT <- file.path(patbis_dir, "fromPATSTAT")
fromWATSON  <- file.path(patbis_dir, "fromWATSON")
iseapp_dir  <- file.path(dropbox_dir, "apps", "iseapp")  # regionmap, firmmap, inglobe only
bigdata_dir <- ".bigdata"

for (d in c(patbis_dir, fromPATSTAT, fromWATSON, iseapp_dir)) {
  if (!dir.exists(d)) stop("Expected folder not found: ", d)
}

dir.create(bigdata_dir, showWarnings = FALSE)
dir.create(file.path(bigdata_dir, "istraxes"), showWarnings = FALSE)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

cat("=== ISE App Data Build ===\n")
cat("dropbox_dir:", dropbox_dir, "\n")
cat("fromPATSTAT:", fromPATSTAT, "\n")
cat("fromWATSON: ", fromWATSON, "\n")
cat("iseapp_dir: ", iseapp_dir, "\n\n")

# ============================================================================
# STEP 1: Build CPC base table from BigQuery
# Source: patbis.fromPATSTAT2021.tls225_docdb_fam_cpc
# ============================================================================

cpcs_cache <- file.path(bigdata_dir, "cpcs.fst")

if (file.exists(cpcs_cache)) {
  cat("Loading cached CPC data from", cpcs_cache, "...\n")
  cpcs <- read_fst(cpcs_cache, as.data.table = TRUE)
} else {
  cat("Downloading CPC base table from BigQuery...\n")
  tic("CPC base (BigQuery)")

  library(bigrquery)
  library(DBI)

  project_id <- "patbis"
  dataset    <- "fromPATSTAT2021"
  table      <- "tls225_docdb_fam_cpc"

  cpcs <- bq_table_download(
    bq_table(project_id, dataset, table),
    page_size = 50000
  ) |>
    dplyr::select(docdb_family_id, cpc_class_symbol)

  setDT(cpcs)

  # Strip all spaces from CPC symbols for matching
  cpcs[, cpc_class_symbol := stri_replace_all_fixed(cpc_class_symbol, " ", "")]

  cat("  Saving cache to", cpcs_cache, "...\n")
  write_fst(cpcs, cpcs_cache, compress = 100)

  gc()
  toc()
}

cat("  CPC base:", nrow(cpcs), "rows,",
    uniqueN(cpcs$docdb_family_id), "unique innovations\n\n")


# ============================================================================
# STEP 2: Build techmap from ifcreport.xlsx (replaces docdb_tech_class.csv + Y02)
# ============================================================================

cat("Building techmap from ifcreport.xlsx...\n")
tic("ifcreport techmap")

ifc <- read_excel("classifications/ifcreport.xlsx", skip = 1)
names(ifc) <- c("technology", "cpc_code", "source")
ifc <- ifc[!is.na(ifc$technology) & !is.na(ifc$cpc_code), ]

# Name mapping to match existing app conventions
ifc$technology <- trimws(ifc$technology)
ifc$technology[ifc$technology == "Green Technology"]        <- "Any Green technology"
ifc$technology[ifc$technology == "Green Buildings"]         <- "Green Housing"
ifc$technology[ifc$technology == "Green Transport"]         <- "Green Transport"
ifc$technology[ifc$technology == "Artificial Intelligence"] <- "AI"

# Fix CPC typo: "YO2" (letter O) -> "Y02" (zero)
ifc$cpc_code <- gsub("YO2", "Y02", ifc$cpc_code)

# Expand rows by splitting on "|"
ifc_expanded <- ifc |>
  separate_rows(cpc_code, sep = "\\|") |>
  mutate(cpc_prefix = trimws(cpc_code)) |>
  mutate(cpc_prefix = stri_replace_all_fixed(cpc_prefix, " ", "")) |>
  filter(nchar(cpc_prefix) > 0) |>
  select(technology, cpc_prefix)

# Prefix matching: find all innovations whose CPC symbol starts with each prefix
techmap_ifc <- rbindlist(lapply(seq_len(nrow(ifc_expanded)), function(i) {
  prefix <- ifc_expanded$cpc_prefix[i]
  tech   <- ifc_expanded$technology[i]
  matched <- cpcs[startsWith(cpc_class_symbol, prefix),
                  .(docdb_family_id, technology = tech)]
  unique(matched)
}))

techmap <- unique(techmap_ifc)
cat("  ifcreport techmap:", nrow(techmap), "rows,",
    uniqueN(techmap$technology), "technologies\n")
rm(techmap_ifc, ifc_expanded, ifc)
toc()


# ============================================================================
# STEP 3: Add Battery classifications
# ============================================================================

cat("Adding Battery classifications...\n")

source("R/functions_extrasectorshelper.R")  # provides battery_df

battery_expanded <- battery_df |>
  separate_rows(CPC, sep = ";") |>
  mutate(cpc_class_symbol = stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  select(technology, cpc_class_symbol)

setDT(battery_expanded)
battery_classes_map <- cpcs[battery_expanded, on = "cpc_class_symbol",
                            nomatch = 0L][, .(docdb_family_id, technology)]
battery_classes_map <- unique(battery_classes_map)

# Add umbrella
battery_umbrella <- battery_classes_map[, .(docdb_family_id = unique(docdb_family_id),
                                            technology = "Any battery technology")]

techmap <- rbindlist(list(techmap, battery_classes_map, battery_umbrella))
cat("  Battery:", nrow(battery_classes_map), "sub-category rows +",
    nrow(battery_umbrella), "umbrella rows\n")
rm(battery_expanded, battery_classes_map, battery_umbrella)


# ============================================================================
# STEP 4: Add Hard-to-Abate classifications
# ============================================================================

cat("Adding Hard-to-Abate classifications...\n")

hta_df <- read_excel("classifications/New_Sector_Mapping.xlsx", sheet = "hta_sector") |>
  rename(detail = technology, technology = sector) |>
  mutate(technology = paste0(technology, " Decarbonisation"))

hta_expanded <- hta_df |>
  separate_rows(CPC, sep = ";") |>
  mutate(cpc_class_symbol = stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  filter(nchar(cpc_class_symbol) > 0) |>
  select(technology, cpc_class_symbol)

setDT(hta_expanded)
hta_classes_map <- cpcs[hta_expanded, on = "cpc_class_symbol",
                        nomatch = 0L][, .(docdb_family_id, technology)]
hta_classes_map <- unique(hta_classes_map)

hta_umbrella <- hta_classes_map[, .(docdb_family_id = unique(docdb_family_id),
                                    technology = "Any Hard to Abate technology")]

techmap <- rbindlist(list(techmap, hta_classes_map, hta_umbrella))
cat("  HTA:", nrow(hta_classes_map), "sub-category rows +",
    nrow(hta_umbrella), "umbrella rows\n")
rm(hta_df, hta_expanded, hta_classes_map, hta_umbrella)


# ============================================================================
# STEP 5: Add AI sub-categories
# ============================================================================

cat("Adding AI sub-categories...\n")

ai_df <- read_excel("classifications/New_Sector_Mapping.xlsx", sheet = "AI") |>
  rename(CPC = `CPC/IPC Codes`, technology = `Sub-Technology`) |>
  filter(!is.na(technology), !is.na(CPC))

ai_expanded <- ai_df |>
  separate_rows(CPC, sep = ",") |>
  mutate(cpc_class_symbol = stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  filter(nchar(cpc_class_symbol) > 0) |>
  select(technology, cpc_class_symbol)

setDT(ai_expanded)
ai_classes_map <- cpcs[ai_expanded, on = "cpc_class_symbol",
                       nomatch = 0L][, .(docdb_family_id, technology)]
ai_classes_map <- unique(ai_classes_map)

# Note: The umbrella "AI" is already created in Step 2 from ifcreport.xlsx
techmap <- rbindlist(list(techmap, ai_classes_map))
cat("  AI sub-categories:", nrow(ai_classes_map), "rows\n")
rm(ai_df, ai_expanded, ai_classes_map)


# ============================================================================
# STEP 6: Add Agriculture & Food classifications
# ============================================================================

cat("Adding Agriculture & Food classifications...\n")

agri_df <- read_excel("classifications/Agriculture_Food_CPC_Patents_2026-01-22.xlsx",
                       sheet = 1) |>
  rename(CPC = `CPC Group/Subgroup`, technology = `Value Chain`) |>
  filter(!is.na(technology), !is.na(CPC))

agri_expanded <- agri_df |>
  separate_rows(CPC, sep = ";") |>
  mutate(cpc_class_symbol = stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  filter(nchar(cpc_class_symbol) > 0) |>
  select(technology, cpc_class_symbol)

setDT(agri_expanded)
agri_classes_map <- cpcs[agri_expanded, on = "cpc_class_symbol",
                         nomatch = 0L][, .(docdb_family_id, technology)]
agri_classes_map <- unique(agri_classes_map)

agri_umbrella <- agri_classes_map[, .(docdb_family_id = unique(docdb_family_id),
                                      technology = "Any Agriculture & Food technology")]

techmap <- rbindlist(list(techmap, agri_classes_map, agri_umbrella))
cat("  Agri-food:", nrow(agri_classes_map), "sub-category rows +",
    nrow(agri_umbrella), "umbrella rows\n")
rm(agri_df, agri_expanded, agri_classes_map, agri_umbrella)


# ============================================================================
# STEP 7: Add CPC section mappings
# ============================================================================

cat("Adding CPC section mappings...\n")

cpc_section_names <- c(
  A = "Human Necessities",
  B = "Performing Operations; Transporting",
  C = "Chemistry; Metallurgy",
  D = "Textiles; Paper",
  E = "Fixed Constructions",
  F = "Mechanical Engineering; Lighting; Heating; Weapons; Blasting",
  G = "Physics",
  H = "Electricity",
  Y = "General tagging of new or cross-sectional technology"
)

section_map <- cpcs[, .(section = substr(cpc_class_symbol, 1, 1)), by = docdb_family_id]
section_map <- unique(section_map)
section_map <- section_map[section %in% names(cpc_section_names)]
section_map[, technology := cpc_section_names[section]]
section_map <- section_map[, .(docdb_family_id, technology)]

techmap <- rbindlist(list(techmap, section_map))
cat("  CPC sections:", nrow(section_map), "rows\n")
rm(section_map)


# ============================================================================
# STEP 8: Deduplicate techmap
# ============================================================================

cat("Deduplicating techmap...\n")
techmap <- unique(techmap)
cat("  Final techmap:", nrow(techmap), "rows,",
    uniqueN(techmap$technology), "distinct technologies,",
    uniqueN(techmap$docdb_family_id), "distinct innovations\n")

techmap_cache <- file.path(bigdata_dir, "techmap.fst")
write_fst(techmap, techmap_cache, compress = 100)
cat("  Saved to", techmap_cache, "\n\n")


# ============================================================================
# STEP 9: Build countrymap from harmonized inventor + holder country files
# ============================================================================
# Assign each docdb_family to one or more countries based on the union of
# harmonized inventor and holder country mappings (produced by
# data-raw/build_inventor_countries_harm.R).
#
# The harmonized files come from PATSTAT persons joined with the inglobe
# inventor/holder bridges, then reduced to a single "best" country per
# psn_name. Using their union means a family is attributed to a country
# if either an inventor or a holder on that family is based there.
#
# Dedup: a (docdb_family_id, ctry_code) pair that appears in both the
# inventor and holder files is kept only once.

inv_harm_path  <- file.path(bigdata_dir, "inventor_countries_harm.parquet")
hold_harm_path <- file.path(bigdata_dir, "holder_countries_harm.parquet")
for (p in c(inv_harm_path, hold_harm_path)) {
  if (!file.exists(p)) {
    stop("Missing ", p,
         "\nRun data-raw/build_inventor_countries_harm.R first to generate it.")
  }
}

cat("Building countrymap from harmonized inventor + holder countries...\n")
tic("countrymap")

read_harm <- function(path, role) {
  dt <- arrow::read_parquet(
    path,
    col_select = c("docdb_family_id", "person_ctry_code")
  ) |> data.table::as.data.table()
  data.table::setnames(dt, "person_ctry_code", "ctry_code")
  cat(sprintf("  %-10s rows: %d  (%d distinct (family, country) pairs)\n",
              role, nrow(dt), uniqueN(dt, by = c("docdb_family_id", "ctry_code"))))
  dt
}

inv_cm  <- read_harm(inv_harm_path,  "inventor")
hold_cm <- read_harm(hold_harm_path, "holder")

# Union + dedup on (docdb_family_id, ctry_code)
countrymap <- unique(rbindlist(list(inv_cm, hold_cm), use.names = TRUE))
countrymap <- countrymap[!is.na(ctry_code) & nzchar(ctry_code)]
countrymap <- countrymap[ctry_code != "KP"]  # Exclude North Korea

rm(inv_cm, hold_cm); gc()

cat("  countrymap:", nrow(countrymap), "rows,",
    uniqueN(countrymap$docdb_family_id), "innovations,",
    uniqueN(countrymap$ctry_code), "countries\n")
toc()


# ============================================================================
# STEP 10: Compute istraxes from fromWATSON
# ============================================================================

cat("\nComputing istraxes from fromWATSON...\n")
tic("istraxes total")

# 10a: Build patchar (global-level per innovation)
cat("  10a: Reading global istrax data...\n")
patchar <- fread(file.path(fromWATSON, "innos_istraxfield_global_2009_2018.dsv"))
patchar <- patchar[, .(docdb_family_id, pv, ev, costpvyear_2009_2018,
                       alphapvyear_2009_2018, istrax)]
setnames(patchar, c("istrax", "costpvyear_2009_2018", "alphapvyear_2009_2018", "ev"),
                  c("istrax_global", "cost", "alpha", "ev_global"))
patchar[, avstrax_global := (ev_global + pv) / cost]

cat("    patchar:", nrow(patchar), "innovations\n")

# 10b: Build patchar_countrymap using nationalkey ev file
# innos_ev_nationalkey has broader country coverage than innos_ctry_indicators
# (e.g. Argentina exists in nationalkey but not in ctry_indicators)
cat("  10b: Reading national ev values...\n")
ev_natl <- fread(file.path(fromWATSON, "innos_ev_nationalkey_2009_2018.dsv"))
ev_natl_ev_col <- grep("^ev", names(ev_natl), value = TRUE)[1]
ev_natl <- ev_natl[, .(docdb_family_id, ctry_code,
                       ev_nationalkey_2009_2018 = get(ev_natl_ev_col))]

# Join with patchar to get pv, cost, alpha for istrax formula
pcm <- patchar[ev_natl, on = "docdb_family_id", nomatch = 0L]
pcm[, `:=`(
  istrax_nationalkey_2009_2018  = ((alpha + 1) / cost) * ev_nationalkey_2009_2018 * as.integer(pv <= 2 * cost),
  avstrax_nationalkey_2009_2018 = (pv + ev_nationalkey_2009_2018) / cost
)]

cat("    patchar_countrymap:", nrow(pcm), "rows,",
    uniqueN(pcm$ctry_code), "countries\n")

# 10c: Add country/group EV columns
# Note: ev files have inconsistent structures:
#   Per-country files (AT, GB...): (row_index, docdb_family_id, pv, ev, v)
#   Group files (emde, hic...):    (row_index, docdb_family_id, ev_xxx_2009_2018)
# We read all columns and find the ev column by name pattern.
ev_variants <- c("CN", "emde", "eu", "euplusuk", "g7", "IN",
                 "emdenocn", "emdenocnin", "hic",
                 "AT", "GB", "DE", "FR", "US")

for (ff in ev_variants) {
  ev_file <- file.path(fromWATSON,
                       paste0("innos_ev_", tolower(ff), "_2009_2018.dsv"))
  if (!file.exists(ev_file)) {
    cat("    WARNING: Missing", ev_file, "- skipping\n")
    next
  }
  cat("    Reading ev_", toupper(ff), "...\n", sep = "")
  ev_data <- fread(ev_file)
  # Find the ev column (starts with "ev")
  ev_src_col <- grep("^ev", names(ev_data), value = TRUE)[1]
  if (is.na(ev_src_col)) {
    cat("    WARNING: No ev column found in", basename(ev_file), "- skipping\n")
    next
  }
  ev_col_name <- paste0("ev_", toupper(ff))
  ev_data <- ev_data[, .(docdb_family_id, ev_val = get(ev_src_col))]
  setnames(ev_data, "ev_val", ev_col_name)
  patchar <- patchar[ev_data, on = "docdb_family_id", (ev_col_name) := get(paste0("i.", ev_col_name))]
  rm(ev_data)
}

# 10d: Compute istrax and avstrax for each variant
cat("  10d: Computing istrax and avstrax formulas...\n")

ev_cols <- grep("^ev_", names(patchar), value = TRUE)
ev_cols <- setdiff(ev_cols, "ev_global")  # global already computed

for (evc in ev_cols) {
  suffix <- sub("^ev_", "", evc)
  is_col <- paste0("istrax_", suffix)
  av_col <- paste0("avstrax_", suffix)

  patchar[, (is_col) := ((alpha + 1) / cost) * get(evc) * as.integer(pv <= 2 * cost)]
  patchar[, (av_col) := (pv + get(evc)) / cost]
}

# Correct global as sum of HIC + EMDE (matching original pipeline behavior)
if (all(c("ev_HIC", "ev_EMDE") %in% names(patchar))) {
  patchar[, ev_global      := ev_HIC + ev_EMDE]
  patchar[, istrax_global  := istrax_HIC + istrax_EMDE]
  patchar[, avstrax_global := (pv + ev_global) / cost]
}

# 10e: Build innovation x country measures in memory (no intermediate .fst files)
# Global/per-group measures (from patchar) are per-innovation — same value for all
# countries. National measures (from pcm) vary by country.
cat("  10e: Building innovation x country measures in memory...\n")

# Per-innovation measures (same for all countries of an innovation)
measure_cols <- grep("^(istrax_|avstrax_|ev_)", names(patchar), value = TRUE)
patchar_slim <- patchar[, c("docdb_family_id", measure_cols), with = FALSE]

# Cross with countrymap to get innovation x country level
patent_data <- patchar_slim[countrymap, on = "docdb_family_id", nomatch = 0L]

# Merge in national measures (only exist for a subset of innovation x country)
national_cols <- c("ev_nationalkey_2009_2018", "istrax_nationalkey_2009_2018",
                   "avstrax_nationalkey_2009_2018")
pcm_national <- pcm[, c("docdb_family_id", "ctry_code", national_cols), with = FALSE]
patent_data <- pcm_national[patent_data, on = c("docdb_family_id", "ctry_code")]

# Replace NAs in national columns with 0 (innovations/countries without national data)
for (nc in national_cols) {
  patent_data[is.na(get(nc)), (nc) := 0]
}

# Clean up any i. columns from joins
dup_cols <- grep("^i\\.", names(patent_data), value = TRUE)
for (dc in dup_cols) patent_data[, (dc) := NULL]

cat("    patent_data:", nrow(patent_data), "rows,",
    uniqueN(patent_data$ctry_code), "countries\n")

rm(patchar, pcm, pcm_national, patchar_slim)
gc()
toc()


# ============================================================================
# STEP 11: Finalize patent_database.parquet
# ============================================================================

cat("\nFinalizing patent_database.parquet...\n")
tic("patent_database")

# Rename measure columns to the short prefixes used by the app:
#   istrax_*  -> is_*
#   avstrax_* -> av_*
#   ev_*      -> ev_*   (no change)
old_names <- names(patent_data)
new_names <- old_names |>
  stringr::str_replace("^istrax_",  "is_") |>
  stringr::str_replace("^avstrax_", "av_")
# All column names go to lowercase to match downstream expectations
new_names <- tolower(new_names)
setnames(patent_data, old_names, new_names)

# Get appln_id: build an Espacenet-searchable publication number per family
# from PATSTAT innos_pub.dsv.
#
# Espacenet's `pn=` search accepts publication numbers in the form
# `<publn_auth><publn_nr>` (e.g. EP1234567, WO2020123456, US20200123456).
# Publication numbers are what Espacenet always displays and are definitively
# searchable there. PATSTAT's internal integer appln_id is NOT searchable,
# which is why the previous implementation (storing innos_pub$appln_id) gave
# broken URLs.
#
# Priority per family: EP > WO > US > any other office (most internationally
# recognizable publication). Within a preferred office, first row wins
# (matches the dev_test "first per family" pattern).
cat("  Building Espacenet-searchable appln_id from innos_pub.dsv...\n")
pubs <- fread(file.path(fromPATSTAT, "innos_pub.dsv"),
              select = c("docdb_family_id", "publn_auth", "publn_nr"))
pubs <- pubs[!is.na(publn_auth) & nzchar(publn_auth) &
             !is.na(publn_nr)   & nzchar(publn_nr)]

office_rank <- c(EP = 1L, WO = 2L, US = 3L)
pubs[, prio := fcoalesce(office_rank[publn_auth], 99L)]
setorder(pubs, docdb_family_id, prio)
appln_ids <- pubs[, .(appln_id = paste0(publn_auth[1], publn_nr[1])),
                  by = docdb_family_id]
rm(pubs); gc()

# Add appln_id to patent_data
patent_data <- appln_ids[patent_data, on = "docdb_family_id"]

n_missing <- sum(is.na(patent_data$appln_id))
if (n_missing > 0) {
  cat("  WARNING:", n_missing, "rows have no publication match in innos_pub.dsv (",
      round(100 * n_missing / nrow(patent_data), 2), "%)\n")
}

# Replace NAs with 0 and round numeric measures
num_cols <- names(patent_data)[sapply(patent_data, is.double)]
for (nc in num_cols) {
  patent_data[is.na(get(nc)), (nc) := 0]
  patent_data[, (nc) := round(get(nc), 4)]
}

cat("  patent_data:", nrow(patent_data), "rows,", ncol(patent_data), "columns\n")

# Sort by ctry_code for optimal parquet predicate pushdown
setorder(patent_data, ctry_code)

# Convert to data.frame for arrow
patent_data_df <- as.data.frame(patent_data)

# Build schema
float_schema <- arrow::schema(
  purrr::map(names(patent_data_df), \(col) {
    if (col == "docdb_family_id") {
      arrow::field(col, arrow::int32())
    } else if (is.double(patent_data_df[[col]])) {
      arrow::field(col, arrow::float32())
    } else {
      arrow::field(col, arrow::infer_type(patent_data_df[[col]]))
    }
  })
)

output_file <- "inst/extdata/patent_database.parquet"
patent_data_df |>
  arrow::as_arrow_table(schema = float_schema) |>
  arrow::write_parquet(output_file, compression = "zstd", compression_level = 3)

file_size <- file.info(output_file)$size / 1024^3
cat("  Saved to", output_file, "(", round(file_size, 2), "GB)\n")
toc()


# ============================================================================
# STEP 12: Build bridge & lookup tables
# ============================================================================

cat("\nBuilding bridge and lookup tables...\n")

# ---- Technology group mapping ----
green_classes         <- c("Any Green technology", "Green Technology", "Green Energy", "Green Transport", "Circular Economy", "Green Manufacturing", "Adaptation", "Green Housing", "Green ICT", "Green Agriculture", "GHG Capture")
battery_classes       <- c("Any battery technology", "Battery Technology", "Lithium Extraction & Processing", "Graphite & Carbon Materials", "Cathode Materials", "Anode Materials", "Electrolytes & Additives", "Separators", "Battery Cell Design & Assembly", "Battery Management Systems (BMS)", "Electric Vehicles & Mobility", "Battery Recycling & Recovery")
hard_to_abate_classes <- c("Any Hard to Abate technology", "Hard to Abate Sector Decarbonization", "Aviation Decarbonisation", "Cement & Concrete Decarbonisation", "Chemicals & Plastics Decarbonisation", "Shipping Decarbonisation", "Steel & Iron Decarbonisation")
ai_classes            <- c("AI", "Machine Learning", "Deep Learning", "Natural Language Processing (NLP)", "Computer Vision", "Speech Recognition & Synthesis", "Robotics & Autonomous Systems", "Knowledge Representation & Reasoning", "Planning & Decision Making", "Generative AI", "Semiconductors", "Cloud & Data Infrastructure", "Data Rettrieval & Processing System", "Platform & Frameworks", "Deployment & Support")
cpc_sections          <- c("Human Necessities", "Performing Operations; Transporting", "Chemistry; Metallurgy", "Textiles; Paper", "Fixed Constructions", "Mechanical Engineering; Lighting; Heating; Weapons; Blasting", "Physics", "Electricity", "General tagging of new or cross-sectional technology")
agrifood_classes      <- c("Any Agriculture & Food technology", "Input supply", "Primary food and feed production", "Post-harvest handling & aggregation", "Processing", "Distribution/wholesale", "Retail/consumption", "Crosscutting")
ifc_standalone        <- c("Fossil Fuel", "Aerospace", "Biotechnology", "Blockchain", "Healthtech", "Wireless")

tech_group_map <- c(
  setNames(rep("Green Technology",                     length(green_classes)),         green_classes),
  setNames(rep("Battery Technology",                   length(battery_classes)),        battery_classes),
  setNames(rep("Hard to Abate Sector Decarbonization", length(hard_to_abate_classes)), hard_to_abate_classes),
  setNames(rep("AI",                                   length(ai_classes)),             ai_classes),
  setNames(rep("Any Agriculture & Food technology",    length(agrifood_classes)),       agrifood_classes),
  setNames(cpc_sections,                                                                cpc_sections),
  setNames(ifc_standalone,                                                              ifc_standalone)
)

# ---- Country group boolean flags ----
all_iso2     <- unique(na.omit(countrycode::codelist$iso2c))
lmics        <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG","BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI","CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE","GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ","KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML","MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG","MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC","SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO","TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")
eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
hic          <- setdiff(all_iso2, lmics)

# -- patents_x_tech --
cat("  Writing patents_x_tech.parquet...\n")
patents_x_tech <- techmap[, .(docdb_family_id, technology)]
patents_x_tech <- unique(patents_x_tech)
write_parquet(as.data.frame(patents_x_tech),
              "inst/extdata/patents_x_tech.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_tech), "rows\n")

# -- tech_lookup --
cat("  Writing tech_lookup.parquet...\n")
tech_lookup <- patents_x_tech[, .(technology = unique(technology))]
tech_lookup[, tech_group := ifelse(technology %in% names(tech_group_map),
                                   tech_group_map[technology], "Other")]
write_parquet(as.data.frame(tech_lookup),
              "inst/extdata/tech_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(tech_lookup), "rows\n")

# -- patents_x_region (from old iseapp) --
cat("  Writing patents_x_region.parquet...\n")
regionmap <- read_fst(file.path(iseapp_dir, "regionmap.fst"))
patents_x_region <- regionmap |>
  dplyr::select(docdb_family_id, region_code) |>
  dplyr::distinct()
write_parquet(patents_x_region, "inst/extdata/patents_x_region.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_region), "rows\n")

# -- region_lookup --
cat("  Writing region_lookup.parquet...\n")
region_lookup <- regionmap |>
  dplyr::select(region_code, region_name) |>
  dplyr::distinct()
write_parquet(region_lookup, "inst/extdata/region_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(region_lookup), "rows\n")

# -- patents_x_firm (from old iseapp) --
cat("  Writing patents_x_firm.parquet...\n")
top_companies <- arrow::read_parquet(file.path(iseapp_dir, "firmmap.parquet")) |>
  dplyr::group_by(company_raw) |>
  dplyr::count() |>
  dplyr::arrange(desc(n)) |>
  dplyr::ungroup() |>
  dplyr::slice_head(n = 100) |>
  dplyr::pull(company_raw)

firmmap_top100 <- arrow::read_parquet(file.path(iseapp_dir, "firmmap.parquet")) |>
  dplyr::filter(company_raw %in% top_companies) |>
  dplyr::rename(firm = company_raw) |>
  dplyr::select(docdb_family_id, firm)

patents_x_firm <- firmmap_top100 |>
  dplyr::select(docdb_family_id, firm) |>
  dplyr::distinct()
write_parquet(patents_x_firm, "inst/extdata/patents_x_firm.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_firm), "rows\n")

# -- firm_lookup --
cat("  Writing firm_lookup.parquet...\n")
firmsectormap <- arrow::read_parquet(file.path(iseapp_dir, "firmsectormap.parquet")) |>
  dplyr::select(firm = company_raw, firm_sector = sector)
firm_lookup <- firmsectormap |> dplyr::filter(firm %in% top_companies)
write_parquet(firm_lookup, "inst/extdata/firm_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(firm_lookup), "rows\n")

# -- country_lookup --
cat("  Writing country_lookup.parquet...\n")
country_lookup <- countrymap[, .(ctry_code = unique(ctry_code))]
country_lookup[, `:=`(
  is_lmic            = ctry_code %in% lmics,
  is_lmic_excl_china = ctry_code %in% setdiff(lmics, "CN"),
  is_eu              = ctry_code %in% eu_countries,
  is_hic             = ctry_code %in% hic
)]
write_parquet(as.data.frame(country_lookup),
              "inst/extdata/country_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(country_lookup), "rows\n")


# ============================================================================
# STEP 13: InGlobe (from old iseapp, unchanged)
# ============================================================================

cat("\nProcessing InGlobe data...\n")
df_raw <- fst::read_fst(file.path(iseapp_dir, "inglobe", "data", "long_final.fst"))

df_processed <- df_raw |>
  dplyr::arrange(sce_country, tech_group, tech_subgroup, source_id, wave) |>
  dplyr::mutate(
    wave = as.integer(wave),
    chain_id = paste0(
      sce_country, "_", tech_group, "_",
      ifelse(is.na(tech_subgroup), "ALL", tech_subgroup), "_",
      sample_size, "_", source_id
    )
  )

df_processed <- df_processed |>
  dplyr::select(
    sce_country, sce_tech_display, tech_group,
    sample_size, wave, source_lon, source_lat,
    target_lon, target_lat, chain_id
  )

arrow::write_parquet(df_processed, "inst/extdata/inglobe_processed.parquet")
cat("  Written inglobe_processed.parquet:", nrow(df_processed), "rows\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== BUILD COMPLETE ===\n")
cat("Output files in inst/extdata/:\n")
for (f in list.files("inst/extdata", pattern = "\\.parquet$")) {
  sz <- file.info(file.path("inst/extdata", f))$size
  cat("  ", f, ":", round(sz / 1024^2, 1), "MB\n")
}
cat("\nIntermediate files in .bigdata/:\n")
for (f in list.files(bigdata_dir, pattern = "\\.fst$")) {
  sz <- file.info(file.path(bigdata_dir, f))$size
  cat("  ", f, ":", round(sz / 1024^2, 1), "MB\n")
}
cat("  istraxes/:", length(list.files(file.path(bigdata_dir, "istraxes"))), "files\n")
