# Build Single Parquet Database for ISE App
# This script builds the complete database from raw PATSTAT and WATSON data.
#
# Primary data source (2025): patbis2025/data/fromWATSON +
# PATSTAT autumn 2025 data/patstat_clean (parquets from the 2025 PATSTAT export)
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

# Raise R's per-process vector memory ceiling. The patchar_slim x countrymap
# cross-join transiently materialises ~230M rows x ~25 doubles, which exceeds
# the macOS default (64 GB on Apple Silicon). Bumping to 128 GB gives headroom
# for data.table's working copies during the join.
if (exists("mem.maxVSize")) mem.maxVSize(128 * 1024)

# ---- Optional build-time family-size filter --------------------------------
# Default is NO filter (keep every family regardless of docdb_family_size).
# Override by EDITING this line (hard-coded reset on each source so stale
# values from earlier R sessions can't silently keep a filter active):
#
#   FAMILY_SIZE_MIN <- 2L   # restrict to multi-application families
#
# Cascades to countrymap BEFORE the patchar join, so every downstream
# parquet inherits the filter for free. Eligible-family set is pulled from
# BigQuery (tls201_appln, fromPATSTAT2025) and cached in .bigdata/.
#
# The granted filter is NOT applied at build time — instead a per-family
# `granted` boolean column is attached to patent_database, so the Shiny app
# can filter by grant status at query time via a UI checkbox.
FAMILY_SIZE_MIN <- 1L

# ---- Atomic parquet writer -------------------------------------------------
# Write to a temp sibling first, then rename on success. Prevents running
# Shiny sessions (which hold DuckDB views over these parquets) from reading
# a half-written file mid-build.
write_parquet_atomic <- function(x, path, ...) {
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  arrow::write_parquet(x, tmp, ...)
  if (!file.rename(tmp, path)) {
    for (i in 1:5) {
      Sys.sleep(1)
      if (file.rename(tmp, path)) return(invisible(path))
    }
    if (file.exists(tmp))
      stop("Could not rename ", tmp, " to ", path,
           " (is another process holding the file?).")
  }
  invisible(path)
}

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

# ---------------------------------------------------------------------------
# BigQuery cache helper
# ---------------------------------------------------------------------------
# Wraps the read-or-fetch idiom we use for every BQ pull. A plain
# `if (file.exists(cache)) ...` gate is fragile: when the source dataset name
# changes (e.g. fromPATSTAT2021 -> fromPATSTAT2025), the local cache silently
# wins and the BigQuery refresh never fires. To guard against that, we write
# a sidecar `<cache>.meta.json` recording the `source_id` (table + filter
# description) the cache was built from. A subsequent run only reuses the
# cache when `source_id` matches; otherwise it re-pulls.
#
# Manual override:
#   ISEAPP_REFRESH_BQ=1 Rscript data-raw-2025/01-build-app-parquets.R
# forces every cache to be refreshed regardless of metadata match.
#
# Supported cache extensions: .fst, .parquet.
bq_cache <- function(cache_path, source_id, fetch_fn) {
  ext <- tools::file_ext(cache_path)
  reader <- switch(ext,
    fst     = function(p) fst::read_fst(p, as.data.table = TRUE),
    parquet = function(p) as.data.table(arrow::read_parquet(p)),
    stop("bq_cache: unsupported extension '", ext, "' for ", cache_path))
  writer <- switch(ext,
    fst     = function(x, p) fst::write_fst(x, p, compress = 100),
    parquet = function(x, p) arrow::write_parquet(
                               x, p,
                               compression = "zstd", compression_level = 3),
    stop("bq_cache: unsupported extension '", ext, "' for ", cache_path))

  meta_path <- paste0(cache_path, ".meta.json")
  force_env <- toupper(Sys.getenv("ISEAPP_REFRESH_BQ", "")) %in%
                 c("1", "TRUE", "T", "YES", "Y")

  if (force_env) {
    message("  ISEAPP_REFRESH_BQ set; ignoring any cached copy of ",
            basename(cache_path), ".")
  } else if (file.exists(cache_path) && file.exists(meta_path)) {
    meta <- jsonlite::fromJSON(meta_path)
    if (identical(meta$source, source_id)) {
      cat("  Loading cache (", source_id, ") from ", cache_path, "\n",
          sep = "")
      return(reader(cache_path))
    }
    message("  Cache at ", cache_path, " was built from '", meta$source,
            "' (expected '", source_id, "'); refreshing.")
  } else if (file.exists(cache_path) && !file.exists(meta_path)) {
    message("  Cache at ", cache_path,
            " has no sidecar metadata; treating as stale and refreshing.")
  }

  cat("  Pulling from BigQuery: ", source_id, " ...\n", sep = "")
  x <- fetch_fn()
  writer(x, cache_path)
  jsonlite::write_json(
    list(source = source_id,
         built  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
         rows   = nrow(x)),
    meta_path, auto_unbox = TRUE, pretty = TRUE)
  cat("  Cached ", nrow(x), " rows -> ", cache_path,
      " (meta: ", meta_path, ")\n", sep = "")
  x
}

dropbox_dir <- find_dropbox_dir()
# ---- 2025 data locations ----
# fromWATSON: Watson-built value files (2013-2022 window).
# patstat_clean: PATSTAT Autumn 2025 tables exported as parquet.
# iseapp_dir: regionmap / firmmap / inglobe inputs (unchanged from 2021).
#
# RUN_VERSION ties this build to a specific patbis2025 experimental run.
# patbis2025 writes every per-run output into data/fromWATSON_<RUN_VERSION>/
# (e.g. fromWATSON_basic for the inaugural run with the is_um / UM=0 PV rule
# applied, and an inventor-based country attribution).  Set RUN_VERSION to ""
# to read the unversioned data/fromWATSON/ produced by the legacy 2021/2024
# pipelines.  Override at the shell with PATBIS_RUN_VERSION=v2 Rscript ...
RUN_VERSION  <- Sys.getenv("PATBIS_RUN_VERSION", "basic")
patbis_dir   <- file.path(dropbox_dir, "patbis2025", "data")
fromWATSON   <- if (nzchar(RUN_VERSION)) {
  file.path(patbis_dir, paste0("fromWATSON_", RUN_VERSION))
} else {
  file.path(patbis_dir, "fromWATSON")
}
patstat_clean <- file.path(dropbox_dir,
                           "PATSTAT autumn 2025 data",
                           "patstat_clean")
iseapp_dir   <- file.path(dropbox_dir, "apps", "iseapp")
bigdata_dir  <- ".bigdata"

# PATSTAT table parquet paths (patstat_clean) — used in place of the old
# legacy fromPATSTAT DSV reads. Each is one row per appln / family.
patstat_tls201 <- file.path(patstat_clean, "tls201_appln.parquet")
patstat_tls211 <- file.path(patstat_clean, "tls211_pat_publn.parquet")

for (d in c(patbis_dir, fromWATSON, patstat_clean, iseapp_dir)) {
  if (!dir.exists(d)) stop("Expected folder not found: ", d)
}

dir.create(bigdata_dir, showWarnings = FALSE)
dir.create(file.path(bigdata_dir, "istraxes"), showWarnings = FALSE)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

cat("=== ISE App Data Build (2025, 2013-2022 window) ===\n")
cat("RUN_VERSION:  ", if (nzchar(RUN_VERSION)) RUN_VERSION else "(none, reading bare fromWATSON/)", "\n")
cat("dropbox_dir:  ", dropbox_dir,  "\n")
cat("fromWATSON:   ", fromWATSON,   "\n")
cat("patstat_clean:", patstat_clean, "\n")
cat("iseapp_dir:   ", iseapp_dir,   "\n\n")

# ============================================================================
# STEP 1: Build CPC base table from patstat_clean
# Source: <dropbox>/PATSTAT autumn 2025 data/patstat_clean/tls225_docdb_fam_cpc.parquet
# (was BigQuery patbis.fromPATSTAT2025.tls225_docdb_fam_cpc -- the same table,
#  read straight off the shared Dropbox parquet so no Google credentials are
#  needed. The space-stripping is pushed down into DuckDB.)
# ============================================================================

cpcs_cache <- file.path(bigdata_dir, "cpcs.fst")

# One DuckDB connection reused by every patstat_clean query in this script.
patstat_parquet <- function(table)
  file.path(patstat_clean, paste0(table, ".parquet"))

pq_lit <- function(x) gsub("'", "''", x, fixed = TRUE)

pq_con <- dbConnect(duckdb::duckdb())
dbExecute(pq_con, sprintf("SET memory_limit='%s'",
                          Sys.getenv("ISEAPP_DUCK_MEMORY", "8GB")))
dbExecute(pq_con, sprintf("SET temp_directory='%s'",
                          normalizePath(bigdata_dir, mustWork = FALSE)))

patstat_query <- function(sql) as.data.table(dbGetQuery(pq_con, sql))

cpcs <- bq_cache(
  cache_path = cpcs_cache,
  source_id  = "patstat_clean.tls225_docdb_fam_cpc",
  fetch_fn   = function() {
    tic("CPC base (patstat_clean parquet)")
    x <- patstat_query(sprintf("
      SELECT docdb_family_id,
             replace(cpc_class_symbol, ' ', '') AS cpc_class_symbol
      FROM read_parquet('%s')",
      pq_lit(patstat_parquet("tls225_docdb_fam_cpc"))))
    gc()
    toc()
    x
  }
)

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
# STEP 5b: Add Defence Technology classifications
# ============================================================================
# CPC scope follows F41/F42 (weapons + ammunition) plus the cross-cutting
# subclasses commonly used in the patent-economics literature on defence,
# see refs in R/functions_extrasectorshelper.R::defence_df.
cat("Adding Defence Technology classifications...\n")

defence_expanded <- defence_df |>
  separate_rows(CPC, sep = ";") |>
  mutate(cpc_class_symbol = stri_replace_all_fixed(trimws(CPC), " ", "")) |>
  filter(nchar(cpc_class_symbol) > 0) |>
  select(technology, cpc_class_symbol)

setDT(defence_expanded)

# Defence prefixes are a mix of subclasses (e.g. F41A, F42B) and full
# main groups (e.g. B64D7/00, G01S7/41) — match by `starts_with` so a
# subclass like F41A picks up F41A0017/00 etc., and a main-group code
# like B64D7/00 also picks up its subgroups (B64D7/02, B64D7/04, ...).
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
DBI::dbExecute(con, sprintf("PRAGMA threads = %d",
                            max(1L, parallel::detectCores() - 1L)))
DBI::dbWriteTable(con, "cpcs",             as.data.frame(cpcs),             overwrite = TRUE)
DBI::dbWriteTable(con, "defence_expanded", as.data.frame(defence_expanded), overwrite = TRUE)

defence_classes_map <- as.data.table(DBI::dbGetQuery(con, "
  SELECT DISTINCT c.docdb_family_id, p.technology
  FROM cpcs c
  JOIN defence_expanded p
    ON starts_with(c.cpc_class_symbol, p.cpc_class_symbol)
"))
DBI::dbDisconnect(con, shutdown = TRUE)

defence_umbrella <- defence_classes_map[, .(docdb_family_id = unique(docdb_family_id),
                                            technology = "Any Defence technology")]

techmap <- rbindlist(list(techmap, defence_classes_map, defence_umbrella))
cat("  Defence:", nrow(defence_classes_map), "sub-category rows +",
    nrow(defence_umbrella), "umbrella rows\n")
rm(defence_expanded, defence_classes_map, defence_umbrella)


# ============================================================================
# STEP 6: Add Agriculture & Food classifications (newagrie value-chain leaves)
# ============================================================================
# Source: classifications/newagrie_value_chain_leaf_codes.xlsx
# One row per CPC leaf code, with `YES_count` (1-3) recording how many of 3
# LLM review iterations voted the code AgriFood-relevant, plus 12 per-segment
# value-chain agreement scores (0-3) in columns I..T (`1_AgriInputs` ...
# `12_DigitalAg`).
#
# Umbrella ("Any Agriculture & Food technology") = every family with any CPC
#   that has YES_count > 1 (>= 2 of 3 iterations voting AgriFood).
# Sub-categories (12) = for each value-chain segment, families with any CPC
#   whose segment score is >= 2. A score of 1 is treated as insufficient
#   evidence and does NOT confer sub-category membership (per project spec).
#
# This replaces the prior STEP 6 (which built from the older
# Agriculture_Food_CPC_Patents_2026-01-22.xlsx with a different 8-class
# scheme: "Input supply", "Primary food and feed production", ...). The
# display labels below flow through `agrifood_classes` (line ~1108) into
# `tech_lookup` and ultimately the `grouped_techs` menu in sysdata.rda.

cat("Adding Agriculture & Food classifications (newagrie value-chain leaves)...\n")
tic("newagrie agri")

agri_raw <- read_excel("classifications/newagrie_value_chain_leaf_codes.xlsx",
                        sheet = 1, .name_repair = "minimal")
setDT(agri_raw)
agri_raw[, cpc_class_symbol := stri_replace_all_fixed(cpc_code, " ", "")]
agri_raw[, YES_count := as.integer(YES_count)]

# Map xlsx column I..T -> app-facing display label. Stable ordering so the
# menu and any downstream colourings group naturally along the value chain.
agri_sub_cols   <- c("1_AgriInputs", "2_SoilLand",      "3_PrimaryProduction",
                     "4_PostHarvest", "5_FoodBevProcessing", "6_PackagingColdChain",
                     "7_Distribution", "8_FoodSafetyQuality", "9_FoodServicesRetail",
                     "10_WasteCircular", "11_WaterEnergy",  "12_DigitalAg")
agri_sub_labels <- c("AgriFood Inputs",
                     "Soil & Land Management",
                     "Primary Production",
                     "Post-Harvest Handling",
                     "Food & Beverage Processing",
                     "Packaging & Cold Chain",
                     "Distribution & Wholesale",
                     "Food Safety & Quality",
                     "Food Services & Retail",
                     "Waste & Circular Economy",
                     "Water & Energy",
                     "Digital Agriculture")
names(agri_sub_labels) <- agri_sub_cols

# Sub-category map: per segment, CPC symbols with score >= 2 joined to cpcs.
agri_classes_map <- rbindlist(lapply(agri_sub_cols, function(col) {
  expanded <- agri_raw[as.integer(get(col)) >= 2L,
                       .(cpc_class_symbol,
                         technology = agri_sub_labels[[col]])]
  if (!nrow(expanded)) return(data.table())
  cpcs[expanded, on = "cpc_class_symbol", nomatch = 0L][
       , .(docdb_family_id, technology)]
}))
agri_classes_map <- unique(agri_classes_map)

# Umbrella: a CPC enters the umbrella if EITHER YES_count > 1 (>= 2 of 3
# iterations voting AgriFood-relevant overall) OR any value-chain segment
# scores >= 2. The union criterion captures both broadly-AgriFood codes
# without specific value-chain placement (e.g. 4 YES_count=3 CPCs with
# no segment >= 2) and segment-strong codes that happened to get only one
# YES vote on the overall label (e.g. ~1,433 combine-harvester-style CPCs
# like A01D 41/* tagged Primary Production = 3 but YES_count = 1). The
# union therefore covers essentially the entire xlsx (10,013 CPCs).
#
# Note: the umbrella is now a (very small) superset of the sub-category
# union — the few broadly-agri YES_count>1 codes without any segment >= 2
# end up in the umbrella but in no sub-category. In the app this means
# "select Any AgriFood" returns at most a few hundred more families than
# the union of all 12 sub-category selections — a reasonable behaviour
# for a catch-all umbrella.
um_expanded <- agri_raw[
  YES_count > 1L |
    (as.integer(`1_AgriInputs`)        >= 2L) |
    (as.integer(`2_SoilLand`)          >= 2L) |
    (as.integer(`3_PrimaryProduction`) >= 2L) |
    (as.integer(`4_PostHarvest`)       >= 2L) |
    (as.integer(`5_FoodBevProcessing`) >= 2L) |
    (as.integer(`6_PackagingColdChain`) >= 2L) |
    (as.integer(`7_Distribution`)      >= 2L) |
    (as.integer(`8_FoodSafetyQuality`) >= 2L) |
    (as.integer(`9_FoodServicesRetail`) >= 2L) |
    (as.integer(`10_WasteCircular`)    >= 2L) |
    (as.integer(`11_WaterEnergy`)      >= 2L) |
    (as.integer(`12_DigitalAg`)        >= 2L),
  .(cpc_class_symbol,
    technology = "Any Agriculture & Food technology")]
agri_umbrella <- unique(cpcs[um_expanded, on = "cpc_class_symbol",
                              nomatch = 0L][, .(docdb_family_id, technology)])

techmap <- rbindlist(list(techmap, agri_classes_map, agri_umbrella))
cat("  Agri-food (newagrie):", nrow(agri_classes_map),
    "sub-category rows (", uniqueN(agri_classes_map$technology),
    "sub-categories ) +", nrow(agri_umbrella), "umbrella rows\n")
rm(agri_raw, agri_classes_map, agri_umbrella, um_expanded)
toc()


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
# STEP 9: Load countrymap + nationalkey from Watson 2025
# ============================================================================
# Watson's two nationalkey-relevant files have different universes:
#   innos_ev_nationalkey_2013_2022.parquet           (~27.2M fams)
#     — (docdb_family_id, ctry_code, pv, ev, v): the broad per-country
#        expected-value surface.
#   innos_istraxsubclass_nationalkey_2013_2022.parquet  (~32M fams)
#     — same columns plus pre-computed cost / alpha / istrax at IPC
#        SUBCLASS level (cost/alpha averaged across the family's
#        IPC subclasses).
# Why subclass and not field?  The Hidden Giants field taxonomy
# (innos_field) only covers ~5M families and is concentrated in 6
# of 41 catids (Robotics, 3D Printing, AI, Aerospace, Clean Cars,
# Clean) — the WIPO-Schmoch traditional fields (catid 1-33) have
# essentially no rows.  See README "Switch to subclass-level istrax
# (2026-05-10)" and the GitHub issue tracking the Hidden Giants
# field-coverage gap.  Subclass-level alphacost has 6,345 cells vs
# 60 cells for field, recovering ~25M+ families.
# Since cost and alpha are family-level parameters (verified: zero
# within-family variance across ctry_code rows), we recover the full
# 27M universe by:
#   1. Taking per-country (pv, ev) from innos_ev_nationalkey.
#   2. Joining cost/alpha from innos_istraxsubclass_global (family-level).
#   3. Computing istrax/avstrax with the standard formulas.

watson_ev_national_path <- file.path(
  fromWATSON, "innos_ev_nationalkey_2013_2022.parquet"
)
watson_global_path <- file.path(
  fromWATSON, "innos_istraxsubclass_global_2013_2022.parquet"
)
for (p in c(watson_ev_national_path, watson_global_path)) {
  if (!file.exists(p))
    stop("Missing ", p,
         "\nExpected in ", fromWATSON,
         "\n(RUN_VERSION='", RUN_VERSION, "' — set PATBIS_RUN_VERSION env var to switch)")
}

cat("Loading Watson 2025 nationalkey (ev + joined global cost/alpha)...\n")
tic("nationalkey")

# (1) family x ctry ev surface
ev_natl <- as.data.table(arrow::read_parquet(
  watson_ev_national_path,
  col_select = c("docdb_family_id", "ctry_code", "pv", "ev")
))

# (2) family-level cost / alpha from the global istrax file
fam_params <- as.data.table(arrow::read_parquet(
  watson_global_path,
  col_select = c("docdb_family_id",
                 "costpvyear_2013_2022", "alphapvyear_2013_2022")
))
data.table::setnames(fam_params,
                     c("costpvyear_2013_2022", "alphapvyear_2013_2022"),
                     c("cost",                 "alpha"))

# (3) join and compute istrax/avstrax per (family, ctry_code)
ev_natl <- fam_params[ev_natl, on = "docdb_family_id"]
rm(fam_params); gc()

ev_natl[, `:=`(
  istrax_nationalkey_2013_2022  = ((alpha + 1) / cost) * ev *
                                   as.integer(pv <= 2 * cost),
  avstrax_nationalkey_2013_2022 = (pv + ev) / cost,
  ev_nationalkey_2013_2022      = ev
)]

# Rows where cost/alpha were missing (family absent from global istrax) will
# have NA istrax/avstrax; keep the ev column regardless.
cat("  nationalkey rows:", format(nrow(ev_natl), big.mark = ","),
    ", distinct families:",
    format(uniqueN(ev_natl$docdb_family_id), big.mark = ","),
    ", distinct ctries:", uniqueN(ev_natl$ctry_code), "\n")
cat("  families with cost/alpha available for istrax:",
    format(uniqueN(ev_natl[!is.na(cost)]$docdb_family_id), big.mark = ","), "\n")
toc()

# ---------------------------------------------------------------------------
# SHORT-TERM PATCHES — Watson nationalkey misclassifications.
#
# Watson's `innos_ev_nationalkey_2013_2022.parquet` occasionally emits
# (docdb, ctry_code) rows whose country tag has no support in PATSTAT
# person records (every person_ctry_code / person_address is NULL or
# points elsewhere). The first documented case is family 49823679
# (CN103483016A) tagged as AR even though every inventor is Chinese.
# See data-raw-2025/patches/watson_ctry_misclass.csv for the running
# list and tracking GitHub issue.
#
# We anti-join the patch list against ev_natl HERE so the filter
# cascades to every downstream artefact: the in-memory `countrymap`
# variable (line below), the patent_database INNER JOIN further down,
# and the `pcm` (patents x country measures) bridge in step 11. A
# blanket `ev_natl[ev > 0]` filter was rejected — legitimate rows can
# have ev = 0.
# ---------------------------------------------------------------------------
patch_path <- "data-raw-2025/patches/watson_ctry_misclass.csv"
if (file.exists(patch_path)) {
  ctry_patches <- as.data.table(read.csv(patch_path,
                                         stringsAsFactors = FALSE,
                                         strip.white      = TRUE))
  ctry_patches <- ctry_patches[
    !is.na(docdb_family_id) & nzchar(ctry_code),
    .(docdb_family_id = as.integer(docdb_family_id),
      ctry_code       = as.character(ctry_code))
  ]
  if (nrow(ctry_patches) > 0L) {
    n_before <- nrow(ev_natl)
    ev_natl  <- ev_natl[!ctry_patches, on = .(docdb_family_id, ctry_code)]
    cat("  applied", nrow(ctry_patches),
        "Watson misclassification patches; dropped",
        format(n_before - nrow(ev_natl), big.mark = ","),
        "row(s) from ev_natl.\n")
  } else {
    cat("  patch file present but empty; nothing to drop from ev_natl.\n")
  }
} else {
  cat("  no patch file at", patch_path, "— skipping.\n")
}

countrymap <- unique(ev_natl[, .(docdb_family_id, ctry_code)])
cat("  countrymap (distinct family x ctry from nationalkey):",
    format(nrow(countrymap), big.mark = ","), "rows,",
    format(uniqueN(countrymap$docdb_family_id), big.mark = ","), "innovations,",
    uniqueN(countrymap$ctry_code), "countries\n")

# ---- Optional: filter to docdb_family_size >= FAMILY_SIZE_MIN --------------
if (FAMILY_SIZE_MIN > 1L) {
  cat(sprintf("\nApplying docdb_family_size >= %d filter ...\n",
              FAMILY_SIZE_MIN))
  tic("family-size filter")

  cache <- file.path(bigdata_dir,
                     sprintf("fam_size_min%d.parquet", FAMILY_SIZE_MIN))
  eligible <- bq_cache(
    cache_path = cache,
    source_id  = sprintf(
      "patstat_clean.tls201_appln (docdb_family_size>=%d)",
      as.integer(FAMILY_SIZE_MIN)),
    fetch_fn   = function() {
      patstat_query(sprintf("
        SELECT DISTINCT docdb_family_id
        FROM read_parquet('%s')
        WHERE docdb_family_size >= %d
          AND docdb_family_id IS NOT NULL",
        pq_lit(patstat_parquet("tls201_appln")),
        as.integer(FAMILY_SIZE_MIN)))
    }
  )

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
cat("\nFetching granted-family set from patstat_clean ...\n")
tic("granted-family fetch")
granted_cache <- file.path(bigdata_dir, "fam_granted.parquet")
granted_fams <- bq_cache(
  cache_path = granted_cache,
  source_id  = "patstat_clean.tls201_appln (granted='Y')",
  fetch_fn   = function() {
    patstat_query(sprintf("
      SELECT DISTINCT docdb_family_id
      FROM read_parquet('%s')
      WHERE granted = 'Y'
        AND docdb_family_id IS NOT NULL",
      pq_lit(patstat_parquet("tls201_appln"))))
  }
)
toc()

# ---- Pull multi-application family set from BigQuery -----------------------
# Used to attach a per-family `fam_size_min2` boolean column so the Shiny UI
# can filter to families with PATSTAT docdb_family_size >= 2 at query time.
# We always materialise this list, regardless of the build-time
# FAMILY_SIZE_MIN setting, so the runtime filter can be toggled
# independently of the build-time scope.
cat("\nFetching fam_size>=2 set from patstat_clean ...\n")
tic("multifam-family fetch")
multifam_cache <- file.path(bigdata_dir, "fam_size_min2.parquet")
multifam_fams <- bq_cache(
  cache_path = multifam_cache,
  source_id  = "patstat_clean.tls201_appln (docdb_family_size>=2)",
  fetch_fn   = function() {
    patstat_query(sprintf("
      SELECT DISTINCT docdb_family_id
      FROM read_parquet('%s')
      WHERE docdb_family_size >= 2
        AND docdb_family_id IS NOT NULL",
      pq_lit(patstat_parquet("tls201_appln"))))
  }
)
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
# Switched 2026-05-10 from innos_istraxfield_* (4.9M fams, 6 catids) to
# innos_istraxsubclass_* (35M fams, ~648 IPC subclasses). See README and
# the GitHub issue tracking the Hidden Giants field-coverage gap.
cat("  10a: Reading global istrax data (subclass-level)...\n")
patchar <- as.data.table(arrow::read_parquet(
  file.path(fromWATSON, "innos_istraxsubclass_global_2013_2022.parquet")
))[, .(docdb_family_id, pv, ev,
       costpvyear_2013_2022, alphapvyear_2013_2022, istrax)]
setnames(patchar,
         c("istrax", "costpvyear_2013_2022", "alphapvyear_2013_2022", "ev"),
         c("istrax_global", "cost",           "alpha",                 "ev_global"))
patchar[, avstrax_global := (ev_global + pv) / cost]

# Attach is_um (utility-model flag) from the patbis2025 per-run lookup so the
# UI can offer an "Exclude utility model patents" toggle without rebuilding
# patent_database for each run. Families without a row in innos_um.parquet
# default to is_um=FALSE (conservative — keep them).
um_path <- file.path(fromWATSON, "innos_um.parquet")
if (file.exists(um_path)) {
  um <- as.data.table(arrow::read_parquet(um_path))[, .(docdb_family_id, is_um)]
  before <- nrow(patchar)
  patchar <- um[patchar, on = "docdb_family_id"]   # left join on patchar
  patchar[is.na(is_um), is_um := FALSE]
  stopifnot(nrow(patchar) == before)
  cat(sprintf("    is_um attached: %s of %s patchar fams flagged UM (%.2f%%)\n",
              format(sum(patchar$is_um), big.mark = ","),
              format(before,             big.mark = ","),
              100 * mean(patchar$is_um)))
} else {
  patchar[, is_um := FALSE]
  cat("    WARNING: ", um_path, " not found — is_um defaulting to FALSE for all\n")
}

cat("    patchar:", nrow(patchar), "innovations\n")

# 10b: Nationalkey: already loaded in Step 9 from Watson 2025's
# innos_istraxsubclass_nationalkey_2013_2022.parquet (was: istraxfield).
# ev/istrax/avstrax for the nationalkey window were computed there; we now
# carry pcm forward as (docdb_family_id, ctry_code,
# ev/istrax/avstrax_nationalkey_2013_2022).
pcm <- ev_natl[, .(
  docdb_family_id,
  ctry_code,
  ev_nationalkey_2013_2022,
  istrax_nationalkey_2013_2022,
  avstrax_nationalkey_2013_2022
)]
rm(ev_natl); gc()

cat("    patchar_countrymap:", nrow(pcm), "rows,",
    uniqueN(pcm$ctry_code), "countries\n")

# 10c: Add per-target EV columns from Watson's innos_ev_to_<target>_2013_2022
# parquets. `<target>` is either a 2-letter ISO country code (US, CN, GB, ...)
# or a group code (emde, hic, eu, g7, euplusuk, oecd, ...). One file per
# target, each (docdb_family_id, ev_to_<target>). We explicitly do NOT
# pivot from innos_ev_supranational — individual files are the authoritative
# source.
ev_targets <- c(
  # Supranational / group cuts
  "emde", "emdenocn", "hic", "eu", "euplusuk", "g7", "oecd",
  "cenasia", "ceneur", "easteur", "eca", "southcau", "wesbal",
  # Individual countries (upper-case to match toflow_choices keys)
  "US", "CN", "GB", "DE", "FR", "AT", "IN"
)
cat("  10c: Reading per-target ev files (",
    length(ev_targets), " targets)...\n", sep = "")
for (tgt in ev_targets) {
  f <- file.path(fromWATSON,
                 sprintf("innos_ev_to_%s_2013_2022.parquet", tgt))
  if (!file.exists(f)) {
    cat("    SKIP missing: ", basename(f), "\n", sep = "")
    next
  }
  ev_tgt  <- as.data.table(arrow::read_parquet(f))
  src_col <- grep("^ev", names(ev_tgt), value = TRUE)[1]
  if (is.na(src_col)) {
    cat("    SKIP (no ev column): ", basename(f), "\n", sep = "")
    next
  }
  new_col <- paste0("ev_", tgt)
  ev_tgt <- ev_tgt[, .(docdb_family_id, val = get(src_col))]
  setnames(ev_tgt, "val", new_col)
  patchar <- ev_tgt[patchar, on = "docdb_family_id"]
  cat("    +", new_col, "(",
      format(sum(!is.na(patchar[[new_col]])), big.mark = ","),
      "non-null)\n")
  rm(ev_tgt)
}

# 10d: Correct ev_global as sum of HIC + EMDE (matching original pipeline
# behaviour). The runtime view in R/runAppPackage.R derives is_*/av_* from
# cost/alpha/pv/ev_* on every query, so we no longer materialise istrax_*
# or avstrax_* here — that work blows up peak RAM during the cross-join
# with countrymap (~22 GB worth of derived columns we'd just drop again).
if (all(c("ev_HIC", "ev_EMDE") %in% names(patchar))) {
  patchar[, ev_global := ev_HIC + ev_EMDE]
}

# 10e: Build innovation x country measures in memory (no intermediate .fst files)
# Global/per-group measures (from patchar) are per-innovation — same value for all
# countries. National measures (from pcm) vary by country.
cat("  10e: Building innovation x country measures in memory...\n")

# Per-innovation measures (same for all countries of an innovation).
# Only ev_* + cost/alpha/pv go into the parquet — the runtime view in
# R/runAppPackage.R derives is_*/av_* from those four. Any leftover
# istrax_*/avstrax_* columns from earlier steps (istrax_global,
# avstrax_global) are deliberately excluded.
measure_cols <- grep("^ev_", names(patchar), value = TRUE)
patchar_slim <- patchar[, c("docdb_family_id", measure_cols,
                             "cost", "alpha", "pv", "is_um"), with = FALSE]

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

# Same idea for the multi-application flag — every (docdb, ctry) row
# inherits the family-level boolean so the Shiny UI can filter to
# docdb_family_size >= 2 at query time.
patent_data[, fam_size_min2 :=
              docdb_family_id %in% multifam_fams$docdb_family_id]
cat(sprintf("    fam_size>=2 families in patent_data: %s / %s (%.1f%%)\n",
            format(uniqueN(patent_data[fam_size_min2 == TRUE]$docdb_family_id),
                   big.mark = ","),
            format(uniqueN(patent_data$docdb_family_id), big.mark = ","),
            100 * uniqueN(patent_data[fam_size_min2 == TRUE]$docdb_family_id) /
                  uniqueN(patent_data$docdb_family_id)))

# Merge in national ev measure (only exists for a subset of innovation x
# country). is_/av_ nationalkey are derived from this + cost/alpha/pv at
# runtime, so we no longer carry their materialised forms.
national_cols <- c("ev_nationalkey_2013_2022")
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
# STEP 10f: Citation counts per family
# ----------------------------------------------------------------------------
# Read the source citenet file (one row per docdb-family citation, self-
# citations already removed). Restrict both endpoints to the final docdb
# universe (i.e. the Shiny app's main database) and count incoming
# citations per cited_docdb_family_id. Merged into patent_data at the
# docdb_family_id level — every (docdb, ctry) row for the same family
# carries the same citation count, exactly like ev_global / global flow
# values do. Stored as the new column `cit_count` so it can be exposed in
# the toflow dropdown.
# ============================================================================
cat("\nComputing citation counts from citenet_noself...\n")
tic("citation counts")
citenet_src <- file.path(patbis_dir, "fromPATSTAT", "citenet_noself.parquet")
if (!file.exists(citenet_src)) {
  warning("citenet_noself.parquet not found at ", citenet_src,
          " — adding cit_count = 0 for every family.")
  patent_data[, cit_count := 0L]
} else {
  con_cit <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con_cit, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  DBI::dbWriteTable(con_cit, "fams",
                    data.frame(docdb_family_id = final_docdbs),
                    overwrite = TRUE)
  cit_counts <- as.data.table(DBI::dbGetQuery(con_cit, sprintf("
    SELECT c.cited_docdb_family_id AS docdb_family_id,
           COUNT(*)                AS cit_count
    FROM read_parquet('%s') c
    INNER JOIN fams f_cited
      ON f_cited.docdb_family_id = c.cited_docdb_family_id
    INNER JOIN fams f_citing
      ON f_citing.docdb_family_id = c.docdb_family_id
    GROUP BY c.cited_docdb_family_id
  ", citenet_src)))
  DBI::dbDisconnect(con_cit, shutdown = TRUE)

  cat(sprintf("  %s families have citations (%.1f%% of universe)\n",
              format(nrow(cit_counts), big.mark = ","),
              100 * nrow(cit_counts) / length(final_docdbs)))

  patent_data <- cit_counts[patent_data, on = "docdb_family_id"]
  # cit_count is BIGINT/INTEGER from DuckDB so the existing is.double loop
  # below skips it; NA-fill it here explicitly.
  patent_data[is.na(cit_count), cit_count := 0L]
}
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
# from the PATSTAT 2025 parquet tables in patstat_clean.
#
# Espacenet's `pn=` search accepts publication numbers in the form
# `<publn_auth><publn_nr>` (e.g. EP1234567, WO2020123456, US20200123456).
# Publication numbers are what Espacenet always displays and are
# definitively searchable there. PATSTAT's internal integer appln_id is
# NOT searchable.
#
# Priority per family: EP > WO > US > any other office (most internationally
# recognizable publication). Within a preferred office, first row wins.
cat("  Building Espacenet-searchable appln_id from PATSTAT 2025...\n")
# Replaces the 2021 innos_pub.dsv read: join tls211_pat_publn (one row per
# publication) with tls201_appln (one row per appln, carries
# docdb_family_id) directly via DuckDB over the parquet files in
# patstat_clean. DuckDB filters on docdb_family_id push down into the
# parquet scan, so we only materialise the ~10-15M publications for
# families in final_docdbs.
con_pub <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
DBI::dbExecute(con_pub, sprintf("PRAGMA threads = %d",
                                 max(1L, parallel::detectCores() - 1L)))
DBI::dbWriteTable(con_pub, "final_fams",
                  data.frame(docdb_family_id = final_docdbs),
                  overwrite = TRUE)

pubs <- as.data.table(DBI::dbGetQuery(con_pub, sprintf("
  SELECT a.docdb_family_id, p.publn_auth, p.publn_nr
  FROM read_parquet('%s') a
  INNER JOIN read_parquet('%s') p ON a.appln_id = p.appln_id
  INNER JOIN final_fams f        ON a.docdb_family_id = f.docdb_family_id
  WHERE p.publn_auth IS NOT NULL
    AND LENGTH(TRIM(p.publn_auth)) > 0
    AND p.publn_nr   IS NOT NULL
    AND LENGTH(TRIM(p.publn_nr))   > 0
", patstat_tls201, patstat_tls211)))
DBI::dbDisconnect(con_pub, shutdown = TRUE)

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
  cat("  WARNING:", n_missing, "rows have no publication match in tls211_pat_publn (",
      round(100 * n_missing / nrow(patent_data), 2), "%)\n")
}

# Replace NAs with 0 and round numeric measures. `cost`, `alpha`, `pv` keep
# their full double precision because the runtime view derives is_*/av_*
# from them on every query — quantising them to 4 decimals would surface
# as a small but real numerical drift versus the previously precomputed
# is/av columns (especially for tiny cost values where 1/cost amplifies
# rounding error). NA→0 still applies so the view's CASE-on-cost guard
# can short-circuit cleanly.
num_cols       <- names(patent_data)[sapply(patent_data, is.double)]
no_round_cols  <- c("cost", "alpha", "pv")
for (nc in num_cols) {
  patent_data[is.na(get(nc)), (nc) := 0]
  if (!nc %in% no_round_cols)
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

write_parquet_atomic(at, output_file,
                     compression = "zstd", compression_level = 3)
rm(at); gc()

file_size <- file.info(output_file)$size / 1024^3
cat("  Saved to", output_file, "(", round(file_size, 2), "GB) [atomic rename]\n")
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
agrifood_classes      <- c("Any Agriculture & Food technology",
                           # 12 value-chain sub-categories (newagrie xlsx
                           # columns I..T, score >= 2 threshold). Order
                           # mirrors the xlsx column order so the menu
                           # groups them naturally along the value chain.
                           "AgriFood Inputs", "Soil & Land Management",
                           "Primary Production", "Post-Harvest Handling",
                           "Food & Beverage Processing", "Packaging & Cold Chain",
                           "Distribution & Wholesale", "Food Safety & Quality",
                           "Food Services & Retail", "Waste & Circular Economy",
                           "Water & Energy", "Digital Agriculture")
defence_classes       <- c("Any Defence technology", "Defence Technology",
                            unique(defence_df$technology))
ifc_standalone        <- c("Fossil Fuel", "Aerospace", "Biotechnology", "Blockchain", "Healthtech", "Wireless")

tech_group_map <- c(
  setNames(rep("Green Technology",                     length(green_classes)),         green_classes),
  setNames(rep("Battery Technology",                   length(battery_classes)),        battery_classes),
  setNames(rep("Hard to Abate Sector Decarbonization", length(hard_to_abate_classes)), hard_to_abate_classes),
  setNames(rep("AI",                                   length(ai_classes)),             ai_classes),
  setNames(rep("Any Agriculture & Food technology",    length(agrifood_classes)),       agrifood_classes),
  setNames(rep("Defence Technology",                   length(defence_classes)),       defence_classes),
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
write_parquet_atomic(as.data.frame(patents_x_tech),
                     "inst/extdata/patents_x_tech.parquet",
                     compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_tech), "rows (restricted to final docdbs)\n")

# -- tech_lookup --
cat("  Writing tech_lookup.parquet...\n")
tech_lookup <- patents_x_tech[, .(technology = unique(technology))]
tech_lookup[, tech_group := ifelse(technology %in% names(tech_group_map),
                                   tech_group_map[technology], "Other")]
write_parquet_atomic(as.data.frame(tech_lookup),
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
write_parquet_atomic(patents_x_region, "inst/extdata/patents_x_region.parquet",
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
write_parquet_atomic(region_lookup, "inst/extdata/region_lookup.parquet",
                     compression = "zstd", compression_level = 3)
cat("    ", nrow(region_lookup), "rows (restricted to regions with final docdbs)\n")

# -- patents_x_firm (from old iseapp) --
# Carry every harmonised firm in firmmap that maps to a final-docdb
# family. The previous build slice_max'd to the top-100 firms by raw
# count, which silently kicked thousands of mid-tier firms out of
# the bundle and made HiGGlo / Country Explorer firm filters miss
# the long tail. The cap was a holdover from the original 2021 code
# path — we want the full bridge.
cat("  Writing patents_x_firm.parquet...\n")
firmmap_full <- arrow::read_parquet(file.path(iseapp_dir, "firmmap.parquet"))

# -- Extra firms not in the upstream firmmap/firmsectormap --
# Firmmap upstream only covers ICB-listed multinationals. We graft additional
# firms here by looking up their patent holdings via the persons + holders
# bridge in <iseapp>/inglobe/. Each entry specifies the canonical firm name
# used in the selector, an ICB-style sector, and a regex matched against
# `psn_name` in persons.parquet (the harmonised PATSTAT person name).
extra_firms <- list(
  list(firm    = "TATA CHEMICALS",
       sector  = "Chemicals",
       pattern = "^TATA CHEMICALS( |$)"),
  list(firm    = "TATA POWER SOLAR SYSTEMS",
       sector  = "Alternative Energy",
       pattern = "^TATA POWER SOLAR SYSTEMS( |$)")
)

cat("  Augmenting firmmap with", length(extra_firms), "extra firm(s)...\n")
extra_persons_path <- file.path(iseapp_dir, "inglobe", "persons.parquet")
extra_holders_path <- file.path(iseapp_dir, "inglobe", "data", "holders.parquet")

if (file.exists(extra_persons_path) && file.exists(extra_holders_path)) {
  extra_con <- duckdb::dbConnect(duckdb::duckdb())
  extra_rows <- lapply(extra_firms, function(ef) {
    q <- sprintf("
      SELECT DISTINCT h.docdb_family_id
      FROM read_parquet('%s') p
      JOIN read_parquet('%s') h ON h.person_id = p.person_id
      WHERE regexp_matches(upper(p.psn_name), '%s')
    ", extra_persons_path, extra_holders_path, ef$pattern)
    fams <- DBI::dbGetQuery(extra_con, q)
    cat("    ", ef$firm, ": ", nrow(fams), " distinct docdb_family_ids\n",
        sep = "")
    if (!nrow(fams)) return(NULL)
    data.frame(company_raw     = ef$firm,
               docdb_family_id = fams$docdb_family_id,
               stringsAsFactors = FALSE)
  })
  DBI::dbDisconnect(extra_con, shutdown = TRUE)
  extra_firmmap_rows <- do.call(rbind, Filter(Negate(is.null), extra_rows))
  if (!is.null(extra_firmmap_rows) && nrow(extra_firmmap_rows) > 0) {
    firmmap_full <- dplyr::bind_rows(firmmap_full, extra_firmmap_rows)
    cat("    Bound", nrow(extra_firmmap_rows),
        "extra firmmap rows.\n")
  }
} else {
  warning("Skipping extra-firm augmentation: ",
          extra_persons_path, " or ", extra_holders_path, " missing.")
}

patents_x_firm <- firmmap_full |>
  dplyr::filter(docdb_family_id %in% final_docdbs) |>
  dplyr::rename(firm = company_raw) |>
  dplyr::select(docdb_family_id, firm) |>
  dplyr::distinct()
rm(firmmap_full); gc()

write_parquet_atomic(patents_x_firm, "inst/extdata/patents_x_firm.parquet",
                     compression = "zstd", compression_level = 3)
cat("    ", nrow(patents_x_firm), "rows (restricted to final docdbs)\n")
cat("    ", dplyr::n_distinct(patents_x_firm$firm), "distinct firms\n")

# -- firm_lookup --
# Restrict to firms that actually appear in patents_x_firm (top companies
# with at least one final-docdb family). Drops any top-100 company whose
# families were filtered out by the Watson / countrymap intersection.
cat("  Writing firm_lookup.parquet...\n")
final_firms <- unique(patents_x_firm$firm)
firmsectormap <- arrow::read_parquet(file.path(iseapp_dir, "firmsectormap.parquet")) |>
  dplyr::select(firm = company_raw, firm_sector = sector)

# Attach sectors for the augmented firms defined above so they survive the
# `firm %in% final_firms` filter and appear in selectors grouped by sector.
# Skip any that are already present in the upstream firmsectormap so the
# block stays idempotent if those firms are added there later.
extra_firm_sectors <- do.call(rbind, lapply(extra_firms, function(ef)
  data.frame(firm = ef$firm, firm_sector = ef$sector,
             stringsAsFactors = FALSE)))
extra_firm_sectors <- extra_firm_sectors |>
  dplyr::anti_join(firmsectormap, by = "firm")
firmsectormap <- dplyr::bind_rows(firmsectormap, extra_firm_sectors)

firm_lookup <- firmsectormap |> dplyr::filter(firm %in% final_firms)
write_parquet_atomic(firm_lookup, "inst/extdata/firm_lookup.parquet",
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
write_parquet_atomic(as.data.frame(country_lookup),
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

write_parquet_atomic(df_processed, "inst/extdata/inglobe_processed.parquet")
cat("  Written inglobe_processed.parquet:", nrow(df_processed), "rows\n")
rm(df_processed); gc()

# ============================================================================
# STEP N: Publish countrymap.parquet (with city/lat/lon) to inst/extdata
# ----------------------------------------------------------------------------
# Source: <dropbox>/PATSTAT autumn 2025 data/patstat_clean/countrymap.parquet
#   — produced by data-raw-2025/build_inventor_countries_harm_bq.R (step 9).
#   One row per (docdb_family_id, ctry_code), with mode city + lat/lon
#   + geocode_missing flag.
#
# Augmentation:
#   patent_database may carry (docdb, ctry) pairs that have no person record
#   in PATSTAT (e.g. families pulled in by Watson nationalkey augmentation
#   without an inventor address), so they are absent from the harmonisation-
#   derived countrymap above. Without coordinates the HiGGlobe view drops
#   them silently. To avoid that, we backfill any such missing pair with the
#   capital-city coordinates of its country and geocode_missing = TRUE.
#   The capital lookup is rebuilt from maps::world.cities so this step has
#   no BigQuery dependency.
# ============================================================================
cat("\nPublishing countrymap.parquet to inst/extdata/...\n")
cm_src <- file.path(patstat_clean, "countrymap.parquet")
cm_dst <- "inst/extdata/countrymap.parquet"
pdb_pq <- "inst/extdata/patent_database.parquet"

if (!file.exists(cm_src)) {
  warning("countrymap.parquet not found at ", cm_src,
          " — run build_inventor_countries_harm_bq.R first. Skipping.")
} else if (!file.exists(pdb_pq)) {
  warning("patent_database.parquet not found at ", pdb_pq,
          " — countrymap will be published without capital-fallback ",
          "augmentation. Skipping.")
  file.copy(cm_src, cm_dst, overwrite = TRUE)
} else {
  # Build a capital-coordinates lookup keyed on ISO2 from maps::world.cities.
  # If multiple capital rows exist for a country (rare), keep the most
  # populous. Countries without a capital row get no fallback.
  if (!requireNamespace("maps", quietly = TRUE))
    stop("Package 'maps' is required for the countrymap capital fallback.")
  wc <- as.data.table(maps::world.cities)
  wc[, iso2 := countrycode::countrycode(country.etc, origin = "country.name",
                                        destination = "iso2c", warn = FALSE)]
  caps <- wc[capital == 1L & !is.na(iso2)][order(iso2, -as.integer(pop))]
  caps <- caps[!duplicated(iso2),
               .(ctry_code = iso2,
                 cap_city  = name,
                 cap_lat   = as.numeric(lat),
                 cap_lon   = as.numeric(long))]
  cat(sprintf("  Capital lookup: %d countries\n", nrow(caps)))

  con_cm <- dbConnect(duckdb::duckdb())
  on.exit(try(dbDisconnect(con_cm, shutdown = TRUE), silent = TRUE), add = TRUE)
  dbWriteTable(con_cm, "caps", as.data.frame(caps), overwrite = TRUE)

  cm_tmp <- paste0(cm_dst, ".tmp-", Sys.getpid())
  # `cm` is filtered to docdbs that actually appear in patent_database.
  # The source patstat_clean/countrymap.parquet covers all PATSTAT
  # families across every filing year; >95% of the docdbs outside the
  # 2013-2022 Watson window never get queried by the app anyway because
  # they're absent from patent_database, so dropping them here shrinks
  # the published parquet from ~154 MB to ~101 MB without losing a
  # single joinable (docdb, ctry) pair.
  dbExecute(con_cm, sprintf("
    COPY (
      WITH cm AS (
        SELECT c.*
        FROM read_parquet('%s') c
        INNER JOIN (
          SELECT DISTINCT docdb_family_id FROM read_parquet('%s')
        ) pdb ON pdb.docdb_family_id = c.docdb_family_id
      ),
      pairs AS (
        SELECT DISTINCT docdb_family_id, ctry_code
        FROM read_parquet('%s')
        WHERE docdb_family_id IS NOT NULL
          AND ctry_code IS NOT NULL
      ),
      missing AS (
        SELECT p.docdb_family_id, p.ctry_code,
               c.cap_city AS city,
               c.cap_lat  AS lat,
               c.cap_lon  AS lon,
               TRUE       AS geocode_missing
        FROM pairs p
        LEFT JOIN cm
          ON cm.docdb_family_id = p.docdb_family_id
         AND cm.ctry_code       = p.ctry_code
        LEFT JOIN caps c
          ON c.ctry_code = p.ctry_code
        WHERE cm.docdb_family_id IS NULL
          AND c.cap_lat IS NOT NULL
      )
      SELECT docdb_family_id, ctry_code, city, lat, lon, geocode_missing
      FROM cm
      UNION ALL
      SELECT docdb_family_id, ctry_code, city, lat, lon, geocode_missing
      FROM missing
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)
  ", cm_src, pdb_pq, pdb_pq, gsub("'", "''", cm_tmp)))

  diag_cm <- dbGetQuery(con_cm, sprintf("
    SELECT
      COUNT(*)                  AS n_rows,
      COUNTIF(geocode_missing)  AS n_capital_fallback,
      COUNTIF(NOT geocode_missing) AS n_with_city
    FROM read_parquet('%s')
  ", gsub("'", "''", cm_tmp)))

  dbDisconnect(con_cm, shutdown = TRUE)

  if (!file.rename(cm_tmp, cm_dst)) {
    for (i in 1:5) {
      Sys.sleep(1); if (file.rename(cm_tmp, cm_dst)) break
    }
    if (file.exists(cm_tmp))
      stop("Could not rename ", cm_tmp, " to ", cm_dst)
  }
  cm_sz <- round(file.info(cm_dst)$size / 1024^2, 1)
  cat(sprintf("  Written countrymap.parquet: %.1f MB (%s rows; %s with city, %s capital-fallback)\n",
              cm_sz,
              format(diag_cm$n_rows,             big.mark = ","),
              format(diag_cm$n_with_city,        big.mark = ","),
              format(diag_cm$n_capital_fallback, big.mark = ",")))
}

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

# ---- Release the patstat_clean DuckDB connection ---------------------------
try(dbDisconnect(pq_con, shutdown = TRUE), silent = TRUE)
