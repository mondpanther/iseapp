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
library(DBI)
library(duckdb)
library(bigrquery)

# ---- Optional build-time family-size filter --------------------------------
# Default is NO filter (keep every family regardless of docdb_family_size).
# Override by EDITING this line (hard-coded reset on each source so stale
# values from earlier R sessions can't silently keep a filter active):
#
#   FAMILY_SIZE_MIN <- 2L   # restrict to multi-application families
#
# Cascades to countrymap BEFORE the patchar join, so every downstream
# parquet inherits the filter for free. Eligible-family set is pulled from
# BigQuery (tls201_appln, fromPATSTAT2021) and cached in .bigdata/.
#
# The granted filter is NOT applied at build time — instead a per-family
# `granted` boolean column is attached to patent_database, so the Shiny app
# can filter by grant status at query time via a UI checkbox.
FAMILY_SIZE_MIN <- 1L

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

# Prefix matching: find all innovations whose CPC symbol starts with each
# prefix. Handled by a single DuckDB query instead of an R loop over
# prefixes — DuckDB builds a prefix lookup structure and scans cpcs once in
# parallel, which is ~10-50x faster than the previous O(N_prefixes × 25M)
# approach.
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
dbExecute(con, sprintf("PRAGMA threads = %d",
                       max(1L, parallel::detectCores() - 1L)))
dbWriteTable(con, "cpcs",         as.data.frame(cpcs),         overwrite = TRUE)
dbWriteTable(con, "ifc_expanded", as.data.frame(ifc_expanded), overwrite = TRUE)

techmap <- setDT(dbGetQuery(con, "
  SELECT DISTINCT c.docdb_family_id, p.technology
  FROM cpcs c
  JOIN ifc_expanded p
    ON starts_with(c.cpc_class_symbol, p.cpc_prefix)
"))
dbDisconnect(con, shutdown = TRUE)

cat("  ifcreport techmap:", nrow(techmap), "rows,",
    uniqueN(techmap$technology), "technologies\n")
rm(ifc_expanded, ifc)
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

# cpcs is no longer needed downstream — free it before building the huge
# patent_data table in step 10.
rm(cpcs); gc()


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
# STEP 9: Load pre-built countrymap
# ============================================================================
# countrymap is produced by data-raw/build_nationalkey.R as the union of
# harmonized inventor and holder country files, restricted to countries that
# have an innos_ev_XX_2009_2018.dsv file in fromWATSON. Each (docdb_family_id,
# ctry_code) pair is unique.

countrymap_path  <- file.path(bigdata_dir, "countrymap.fst")
nationalkey_path <- file.path(bigdata_dir, "nationalkey.fst")
for (p in c(countrymap_path, nationalkey_path)) {
  if (!file.exists(p)) {
    stop("Missing ", p,
         "\nRun data-raw/build_nationalkey.R first to generate it (which in\n",
         "turn requires data-raw/build_inventor_countries_harm.R).")
  }
}

cat("Loading pre-built countrymap...\n")
tic("countrymap")
countrymap <- read_fst(countrymap_path, as.data.table = TRUE)
cat("  countrymap:", nrow(countrymap), "rows,",
    uniqueN(countrymap$docdb_family_id), "innovations,",
    uniqueN(countrymap$ctry_code), "countries\n")
toc()

# ---- Optional: filter to docdb_family_size >= FAMILY_SIZE_MIN --------------
if (FAMILY_SIZE_MIN > 1L) {
  cat(sprintf("\nApplying docdb_family_size >= %d filter ...\n",
              FAMILY_SIZE_MIN))
  tic("family-size filter")

  cache <- file.path(bigdata_dir,
                     sprintf("fam_size_min%d.parquet", FAMILY_SIZE_MIN))
  if (file.exists(cache)) {
    cat("  Reading cached eligible families from ", cache, "\n", sep = "")
    eligible <- as.data.table(arrow::read_parquet(cache))
  } else {
    cat("  Querying BigQuery patbis.fromPATSTAT2021.tls201_appln ...\n")
    sql <- sprintf("
      SELECT DISTINCT docdb_family_id
      FROM `patbis.fromPATSTAT2021.tls201_appln`
      WHERE docdb_family_size >= %d
        AND docdb_family_id IS NOT NULL
    ", as.integer(FAMILY_SIZE_MIN))
    eligible <- as.data.table(bq_table_download(
      bq_project_query("patbis", sql),
      page_size = 200000, bigint = "integer"
    ))
    arrow::write_parquet(eligible, cache,
                         compression = "zstd", compression_level = 3)
    cat("  Cached ", nrow(eligible), " eligible families -> ", cache, "\n",
        sep = "")
  }

  before_fams <- uniqueN(countrymap$docdb_family_id)
  countrymap  <- countrymap[docdb_family_id %in% eligible$docdb_family_id]
  after_fams  <- uniqueN(countrymap$docdb_family_id)
  cat(sprintf("  countrymap: %s -> %s families (%.1f%% kept), %s rows\n",
              format(before_fams, big.mark = ","),
              format(after_fams,  big.mark = ","),
              100 * after_fams / before_fams,
              format(nrow(countrymap), big.mark = ",")))
  rm(eligible)
  toc()
} else {
  cat("FAMILY_SIZE_MIN = 1 (no family-size filter applied).\n")
}

# ---- Pull granted-family set from BigQuery ---------------------------------
# Used later to attach a per-family `granted` boolean column to
# patent_database, so the Shiny UI can filter by grant status at query time.
cat("\nFetching granted-family set from BigQuery ...\n")
tic("granted-family fetch")
granted_cache <- file.path(bigdata_dir, "fam_granted.parquet")
if (file.exists(granted_cache)) {
  cat("  Reading cached granted families from ", granted_cache, "\n", sep = "")
  granted_fams <- as.data.table(arrow::read_parquet(granted_cache))
} else {
  cat("  Querying patbis.fromPATSTAT2021.tls201_appln ...\n")
  granted_fams <- as.data.table(bq_table_download(
    bq_project_query("patbis", "
      SELECT DISTINCT docdb_family_id
      FROM `patbis.fromPATSTAT2021.tls201_appln`
      WHERE granted = TRUE
        AND docdb_family_id IS NOT NULL
    "),
    page_size = 200000, bigint = "integer"
  ))
  arrow::write_parquet(granted_fams, granted_cache,
                       compression = "zstd", compression_level = 3)
  cat("  Cached ", nrow(granted_fams), " granted families -> ", granted_cache,
      "\n", sep = "")
}
toc()


# ============================================================================
# STEP 10: Compute istraxes from fromWATSON
# ============================================================================

cat("\nComputing istraxes from fromWATSON...\n")
tic("istraxes total")

# Helper: read a Watson file, preferring its parquet sibling if it exists
# (produced by data-raw/convert_watson_dsvs.R). Parquet reads with
# col_select are an order of magnitude faster than fread on the wide DSVs.
read_watson <- function(dsv_path, columns = NULL) {
  pq_path <- sub("\\.dsv$", ".parquet", dsv_path)
  if (file.exists(pq_path)) {
    cat("    [parquet] ", basename(pq_path), "\n", sep = "")
    if (!is.null(columns)) {
      out <- arrow::read_parquet(pq_path, col_select = all_of(columns))
    } else {
      out <- arrow::read_parquet(pq_path)
    }
    return(as.data.table(out))
  }
  if (!file.exists(dsv_path)) return(NULL)
  cat("    [dsv]     ", basename(dsv_path), "\n", sep = "")
  fread(dsv_path, showProgress = FALSE,
        select = if (is.null(columns)) NULL else columns)
}

# 10a: Build patchar (global-level per innovation)
cat("  10a: Reading global istrax data...\n")
patchar <- read_watson(
  file.path(fromWATSON, "innos_istraxfield_global_2009_2018.dsv"),
  columns = c("docdb_family_id", "pv", "ev",
              "costpvyear_2009_2018", "alphapvyear_2009_2018", "istrax")
)
setnames(patchar, c("istrax", "costpvyear_2009_2018", "alphapvyear_2009_2018", "ev"),
                  c("istrax_global", "cost", "alpha", "ev_global"))
patchar[, avstrax_global := (ev_global + pv) / cost]

cat("    patchar:", nrow(patchar), "innovations\n")

# 10b: Load the custom-built nationalkey (produced by
# data-raw/build_nationalkey.R) and compute istrax/avstrax from it.
# This uses per-country innos_ev_XX files rather than the precomputed
# innos_ev_nationalkey_2009_2018.dsv.
cat("  10b: Loading custom-built nationalkey...\n")
ev_natl <- read_fst(nationalkey_path, as.data.table = TRUE)

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
  ev_dsv <- file.path(fromWATSON,
                      paste0("innos_ev_", tolower(ff), "_2009_2018.dsv"))
  cat("    Reading ev_", toupper(ff), "...\n", sep = "")
  ev_data <- read_watson(ev_dsv)  # prefers parquet sibling
  if (is.null(ev_data)) {
    cat("    WARNING: Missing ", ev_dsv, " - skipping\n", sep = "")
    next
  }
  # Find the ev column (starts with "ev"). Per-country parquets store the
  # value as "ev"; group DSVs/parquets store it as "ev_<group>_2009_2018".
  ev_src_col <- grep("^ev", names(ev_data), value = TRUE)[1]
  if (is.na(ev_src_col)) {
    cat("    WARNING: No ev column found in", basename(ev_dsv), "- skipping\n")
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

# Attach per-family granted flag — allows runtime UI filter by grant status
# without losing any docdbs from the base universe.
patent_data[, granted := docdb_family_id %in% granted_fams$docdb_family_id]
cat(sprintf("    granted families in patent_data: %s / %s (%.1f%%)\n",
            format(uniqueN(patent_data[granted == TRUE]$docdb_family_id),
                   big.mark = ","),
            format(uniqueN(patent_data$docdb_family_id), big.mark = ","),
            100 * uniqueN(patent_data[granted == TRUE]$docdb_family_id) /
                  uniqueN(patent_data$docdb_family_id)))

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

# Capture the final docdb + country universe (intersection of patchar and
# countrymap). Bridge tables and lookups downstream are filtered to these
# sets so they only reference entities that actually appear in the main
# database — avoids shipping dead rows to the app.
final_docdbs     <- unique(patent_data$docdb_family_id)
final_ctry_codes <- unique(patent_data$ctry_code)
cat("    final docdb universe:   ",
    format(length(final_docdbs),     big.mark=","), "families\n")
cat("    final country universe: ",
    format(length(final_ctry_codes), big.mark=","), "countries\n")

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
cat("  Building Espacenet-searchable appln_id from innos_pub...\n")
pubs <- read_watson(
  file.path(fromPATSTAT, "innos_pub.dsv"),
  columns = c("docdb_family_id", "publn_auth", "publn_nr")
)
# Filter to families that will actually end up in the database. Skips
# ~10M publications that would otherwise be sorted + grouped for no reason.
pubs <- pubs[docdb_family_id %in% final_docdbs]
pubs <- pubs[!is.na(publn_auth) & nzchar(publn_auth) &
             !is.na(publn_nr)   & nzchar(publn_nr)]

office_rank <- c(EP = 1L, WO = 2L, US = 3L)
pubs[, prio := fcoalesce(office_rank[publn_auth], 99L)]
setorder(pubs, docdb_family_id, prio)
appln_ids <- pubs[, .(appln_id = paste0(publn_auth[1], publn_nr[1])),
                  by = docdb_family_id]
rm(pubs); gc()

# Add appln_id to patent_data; free the small lookup
patent_data <- appln_ids[patent_data, on = "docdb_family_id"]
rm(appln_ids); gc()

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

# ---- Write parquet with minimal memory overhead ----
# Build the float32 schema from the data.table directly (data.table inherits
# from data.frame so `[[col]]` works identically). Avoids the prior
# `as.data.frame(patent_data)` duplication.
output_file <- "inst/extdata/patent_database.parquet"
float_schema <- arrow::schema(
  purrr::map(names(patent_data), \(col) {
    if (col == "docdb_family_id") {
      arrow::field(col, arrow::int32())
    } else if (is.double(patent_data[[col]])) {
      arrow::field(col, arrow::float32())
    } else {
      arrow::field(col, arrow::infer_type(patent_data[[col]]))
    }
  })
)

# Convert to an Arrow Table, then drop the R-side data.table BEFORE writing
# so we only hold one full copy during the zstd compression pass.
at <- arrow::as_arrow_table(patent_data, schema = float_schema)
rm(patent_data); gc()

arrow::write_parquet(at, output_file,
                     compression = "zstd", compression_level = 3)
rm(at); gc()

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
# Filter to families that actually end up in the main database. The full
# techmap.fst cached in .bigdata/ contains mappings for every family in the
# cpcs source, but only those in the countrymap x patchar intersection can
# ever be queried by the app.
cat("  Writing patents_x_tech.parquet...\n")
patents_x_tech <- unique(techmap[docdb_family_id %in% final_docdbs,
                                 .(docdb_family_id, technology)])
write_parquet(as.data.frame(patents_x_tech),
              "inst/extdata/patents_x_tech.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_tech), "rows (restricted to final docdbs)\n")

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
  dplyr::filter(docdb_family_id %in% final_docdbs) |>
  dplyr::select(docdb_family_id, region_code) |>
  dplyr::distinct()
write_parquet(patents_x_region, "inst/extdata/patents_x_region.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_region), "rows (restricted to final docdbs)\n")

# -- region_lookup --
# Restrict to regions that actually appear in patents_x_region (which is
# already filtered to final_docdbs). Drops regions with zero families.
cat("  Writing region_lookup.parquet...\n")
final_regions <- unique(patents_x_region$region_code)
region_lookup <- regionmap |>
  dplyr::filter(region_code %in% final_regions) |>
  dplyr::select(region_code, region_name) |>
  dplyr::distinct()
write_parquet(region_lookup, "inst/extdata/region_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(region_lookup), "rows (restricted to regions with final docdbs)\n")

# -- patents_x_firm (from old iseapp) --
# Read firmmap ONCE (it's not tiny) and derive both top_companies and the
# filtered bridge from the same in-memory copy.
cat("  Writing patents_x_firm.parquet...\n")
firmmap_full <- arrow::read_parquet(file.path(iseapp_dir, "firmmap.parquet"))

top_companies <- firmmap_full |>
  dplyr::count(company_raw, name = "n") |>
  dplyr::slice_max(n, n = 100, with_ties = FALSE) |>
  dplyr::pull(company_raw)

patents_x_firm <- firmmap_full |>
  dplyr::filter(company_raw %in% top_companies,
                docdb_family_id %in% final_docdbs) |>
  dplyr::rename(firm = company_raw) |>
  dplyr::select(docdb_family_id, firm) |>
  dplyr::distinct()
rm(firmmap_full); gc()

write_parquet(patents_x_firm, "inst/extdata/patents_x_firm.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_firm), "rows (restricted to final docdbs)\n")

# -- firm_lookup --
# Restrict to firms that actually appear in patents_x_firm (top companies
# with at least one final-docdb family). Drops any top-100 company whose
# families were filtered out by the Watson / countrymap intersection.
cat("  Writing firm_lookup.parquet...\n")
final_firms <- unique(patents_x_firm$firm)
firmsectormap <- arrow::read_parquet(file.path(iseapp_dir, "firmsectormap.parquet")) |>
  dplyr::select(firm = company_raw, firm_sector = sector)
firm_lookup <- firmsectormap |> dplyr::filter(firm %in% final_firms)
write_parquet(firm_lookup, "inst/extdata/firm_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("    ", nrow(firm_lookup), "rows\n")

# -- country_lookup --
# Use final_ctry_codes (countries surviving the patchar x countrymap inner
# join) rather than countrymap's full 181-country set, so the lookup only
# lists countries the app can actually query.
cat("  Writing country_lookup.parquet...\n")
country_lookup <- data.table::data.table(ctry_code = sort(final_ctry_codes))
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
  ) |>
  dplyr::select(
    sce_country, sce_tech_display, tech_group,
    sample_size, wave, source_lon, source_lat,
    target_lon, target_lat, chain_id
  )
rm(df_raw); gc()

arrow::write_parquet(df_processed, "inst/extdata/inglobe_processed.parquet")
cat("  Written inglobe_processed.parquet:", nrow(df_processed), "rows\n")
rm(df_processed); gc()

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
