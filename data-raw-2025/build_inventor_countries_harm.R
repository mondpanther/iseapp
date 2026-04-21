# Build harmonized inventor/holder country mappings.
#
# Many persons in the PATSTAT person table share the same standardized name
# (psn_name) but get assigned different country codes across different
# documents (often due to typos, OCR noise, or inconsistent data entry).
# This script produces harmonized versions of `inventor_countries.parquet`
# and `holder_countries.parquet` by collapsing each psn_name to a single
# "best" country code using the following rule:
#
#   1. For each psn_name, pick the country code that is most often associated
#      with that name across all (inventor ∪ holder) × family rows.
#   2. If two or more countries tie at the per-name max count, resolve
#      per (name × family):
#        - if the family-mode country is one of the tied candidates, pick it;
#        - otherwise (the family mode is not among the candidates and
#          therefore not informative), pick uniformly at random from the
#          tied candidates.
#   3. As a final safety net, any row that is somehow still unresolved
#      keeps its original person_ctry_code.
#
# Outputs:
#   .bigdata/inventor_countries_harm.parquet
#   .bigdata/holder_countries_harm.parquet
#
# Intermediate cache (to avoid repeated BigQuery downloads):
#   .bigdata/tls206_person.fst

library(bigrquery)
library(DBI)
library(duckdb)
library(data.table)
library(arrow)
library(fst)
library(tictoc)
library(jsonlite)
library(countrycode)

# ---- Paths ----
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
iseapp_dir  <- file.path(dropbox_dir, "apps", "iseapp")
if (!dir.exists(iseapp_dir))
  stop("Expected iseapp folder not found at: ", iseapp_dir)
cat("Using iseapp_dir:", iseapp_dir, "\n")

bigdata_dir <- ".bigdata"
dir.create(bigdata_dir, showWarnings = FALSE)

persons_cache_parquet <- file.path(bigdata_dir, "tls206_person.parquet")
persons_cache_fst     <- file.path(bigdata_dir, "tls206_person.fst")    # legacy
persons_local         <- file.path(iseapp_dir, "inglobe", "persons.parquet")
out_inv   <- file.path(bigdata_dir, "inventor_countries_harm.parquet")
out_hold  <- file.path(bigdata_dir, "holder_countries_harm.parquet")

# Input parquet paths for DuckDB (bridges already live as parquets in Dropbox)
inv_bridge_parquet  <- file.path(iseapp_dir, "inglobe", "data", "inventors.parquet")
hold_bridge_parquet <- file.path(iseapp_dir, "inglobe", "data", "holders.parquet")

cat("=== Building harmonized inventor/holder country mappings ===\n")

# ============================================================================
# 1. Persons table
#    Priority:
#      (a) local .bigdata cache (from prior run)
#      (b) local persons.parquet in inglobe/
#      (c) BigQuery patbis.fromPATSTAT2021.tls206_person
# ============================================================================

load_persons_from_parquet <- function(path) {
  cat("  Reading local persons parquet:", path, "\n")
  p <- arrow::read_parquet(
    path,
    col_select = c("person_id", "psn_name", "person_ctry_code")
  )
  setDT(p)
  p
}

load_persons_from_bigquery <- function() {
  cat("  Downloading tls206_person from BigQuery...\n")
  p <- bq_table_download(
    bq_table("patbis", "fromPATSTAT2021", "tls206_person"),
    page_size = 50000
  )
  setDT(p)
  keep_cols <- intersect(c("person_id", "psn_name", "person_ctry_code"),
                         names(p))
  p <- p[, ..keep_cols]
  p
}

# Load persons (cached parquet, legacy fst cache, local parquet, or BigQuery).
# The cleaning/filter step runs UNCONDITIONALLY below so that changes to the
# filter logic always take effect even when a stale cache exists.
loaded_from_cache <- FALSE
if (file.exists(persons_cache_parquet)) {
  cat("Loading cached persons parquet:", persons_cache_parquet, "...\n")
  persons <- as.data.table(arrow::read_parquet(persons_cache_parquet))
  loaded_from_cache <- TRUE
} else if (file.exists(persons_cache_fst)) {
  cat("Loading legacy persons fst cache:", persons_cache_fst, "...\n")
  persons <- read_fst(persons_cache_fst, as.data.table = TRUE)
  loaded_from_cache <- TRUE
} else {
  tic("persons load")
  if (file.exists(persons_local)) {
    persons <- load_persons_from_parquet(persons_local)
  } else {
    cat("Local persons parquet not found at", persons_local, "\n")
    cat("Falling back to BigQuery.\n")
    persons <- load_persons_from_bigquery()
  }
  toc()
}

# --- Cleaning + filtering (always runs) ---
# Aggressive psn_name normalization: strip ALL punctuation (commas, periods,
# semicolons, hyphens, etc.), collapse whitespace, trim, uppercase. This
# merges spellings that PATSTAT/DOCDB leave separate (e.g.
#   "KHATRI, HIMAL"   vs "KHATRI HIMAL"
#   "AWONIYI, OLUFUNMILOLA O." vs "AWONIYI OLUFUNMILOLA O"
# would otherwise be treated as different persons and their per-name
# country votes would not combine.
persons[, psn_name := toupper(trimws(psn_name))]
persons[, psn_name := gsub("[[:punct:]]+", " ", psn_name)]
persons[, psn_name := gsub("\\s+", " ", psn_name)]
persons[, psn_name := trimws(psn_name)]
persons[, person_ctry_code := trimws(person_ctry_code)]

n_before <- nrow(persons)

# Drop rows with no usable identifier or name
persons <- persons[!is.na(person_id) & nzchar(psn_name)]

# Restrict person_ctry_code to VALID ISO2 codes only. This prevents the
# harmonization step from selecting blanks (" ", "  ") or non-ISO codes
# (e.g. "ZZ", "XH", old "SU"/"DD") as the mode country for a name.
valid_iso2 <- unique(na.omit(countrycode::codelist$iso2c))
persons <- persons[person_ctry_code %in% valid_iso2]

cat("  persons rows before filter:", n_before, "\n")
cat("  persons rows after filter: ", nrow(persons),
    sprintf(" (dropped %d)\n", n_before - nrow(persons)))

# Cache as parquet — this is the format DuckDB reads directly via
# read_parquet() in the aggregation pipeline below.
needs_write <- !file.exists(persons_cache_parquet) ||
               !loaded_from_cache ||
               nrow(persons) != n_before
if (needs_write) {
  cat("  Caching to", persons_cache_parquet, "...\n")
  arrow::write_parquet(persons, persons_cache_parquet,
                       compression = "zstd", compression_level = 3)
  # If we upgraded from legacy fst, remove it to avoid future confusion.
  if (file.exists(persons_cache_fst) && file.exists(persons_cache_parquet)) {
    unlink(persons_cache_fst)
    cat("  Removed legacy cache ", persons_cache_fst, "\n")
  }
}
# Free the R-side persons — the DuckDB pipeline reads from parquet.
rm(persons); gc()

# ============================================================================
# 2-7. DuckDB pipeline: bridges + persons -> per-name counts -> family mode
#      -> tie resolution -> harmonized rows, all in SQL.
#
# Avoids materializing the ~100M-row `work = persons JOIN bridges` table in
# R (which used to dominate memory). DuckDB streams the join through the
# aggregations, keeping peak RSS to a few hundred MB instead of ~10-15 GB.
# ============================================================================

cat("Setting up DuckDB pipeline...\n")
tic("duckdb pipeline")

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
dbExecute(con, sprintf("PRAGMA threads = %d",
                       max(1L, parallel::detectCores() - 1L)))
# Reproducible random tie-breaking: DuckDB's RANDOM() is deterministic once
# SETSEED is called (seed must be in [-1, 1]).
dbExecute(con, "SELECT SETSEED(0.42)")

# --- Source tables (as views, no copy) ---
dbExecute(con, sprintf(
  "CREATE VIEW persons AS SELECT * FROM read_parquet(%s)",
  dbQuoteString(con, persons_cache_parquet)
))
dbExecute(con, sprintf(
  "CREATE VIEW bridges AS
     SELECT person_id, docdb_family_id, 'inventor' AS type
     FROM read_parquet(%s)
     UNION ALL
     SELECT person_id, docdb_family_id, 'holder' AS type
     FROM read_parquet(%s)",
  dbQuoteString(con, inv_bridge_parquet),
  dbQuoteString(con, hold_bridge_parquet)
))

# --- work view: persons JOIN bridges (streamed, never materialized) ---
dbExecute(con, "
  CREATE VIEW work AS
    SELECT p.psn_name, b.docdb_family_id, p.person_ctry_code, b.type
    FROM persons p
    JOIN bridges b USING (person_id)
")

# Row count (cheap aggregate; just for the log)
work_rows <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM work")$n
cat("  work rows (virtual):", format(work_rows, big.mark=","), "\n")

# --- Per-name country counts + top candidate(s) per name ---
cat("Computing per-name country counts...\n")
dbExecute(con, "
  CREATE TABLE name_ctry_counts AS
    SELECT psn_name, person_ctry_code, COUNT(*) AS N
    FROM work
    GROUP BY psn_name, person_ctry_code
")
dbExecute(con, "
  CREATE TABLE name_top AS
    SELECT psn_name, person_ctry_code, N
    FROM (
      SELECT *, MAX(N) OVER (PARTITION BY psn_name) AS max_N
      FROM name_ctry_counts
    )
    WHERE N = max_N
")
dbExecute(con, "
  CREATE TABLE cand_counts AS
    SELECT psn_name, COUNT(*) AS n_candidates
    FROM name_top
    GROUP BY psn_name
")

diag <- dbGetQuery(con, "
  SELECT
    (SELECT COUNT(*) FROM cand_counts)                         AS unique_names,
    (SELECT COUNT(*) FROM cand_counts WHERE n_candidates = 1)  AS unambig_names,
    (SELECT COUNT(*) FROM cand_counts WHERE n_candidates > 1)  AS tied_names
")
cat("  unique names:           ", format(diag$unique_names,  big.mark=","), "\n")
cat("  unambiguous names:      ", format(diag$unambig_names, big.mark=","), "\n")
cat("  tied names (need break):", format(diag$tied_names,    big.mark=","), "\n")

# Unambiguous: single top candidate per name
dbExecute(con, "
  CREATE TABLE name_best_unambig AS
    SELECT nt.psn_name, nt.person_ctry_code AS harm_ctry
    FROM name_top nt
    JOIN cand_counts cc USING (psn_name)
    WHERE cc.n_candidates = 1
")

# --- Family-level mode country ---
cat("Computing family-level mode country...\n")
dbExecute(con, "
  CREATE TABLE fam_mode AS
    SELECT docdb_family_id, person_ctry_code AS fam_mode
    FROM (
      SELECT docdb_family_id, person_ctry_code,
             ROW_NUMBER() OVER (PARTITION BY docdb_family_id
                                ORDER BY COUNT(*) DESC) AS rnk
      FROM work
      GROUP BY docdb_family_id, person_ctry_code
    )
    WHERE rnk = 1
")
fam_mode_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM fam_mode")$n
cat("  family modes:", format(fam_mode_n, big.mark=","), "\n")

# --- Tie resolution per (tied name × family) ---
cat("Resolving tied names...\n")
# Candidates for each tied name × each family the name appears in.
# Tie-breaking rule:
#   1. Prefer the candidate that equals the family mode (match_flag = 1).
#   2. Otherwise (no candidate matches), pick uniformly at random among the
#      tied candidates. The family mode is uninformative in this case.
dbExecute(con, "
  CREATE TABLE tied_best AS
    WITH tied_names AS (
      SELECT psn_name FROM cand_counts WHERE n_candidates > 1
    ),
    tied_fam AS (
      SELECT DISTINCT w.psn_name, w.docdb_family_id
      FROM work w
      JOIN tied_names tn USING (psn_name)
    ),
    tied_cross AS (
      SELECT tf.psn_name, tf.docdb_family_id,
             nt.person_ctry_code,
             CASE WHEN nt.person_ctry_code = fm.fam_mode THEN 1 ELSE 0 END
               AS match_flag,
             RANDOM() AS rnd
      FROM tied_fam tf
      JOIN name_top nt USING (psn_name)
      LEFT JOIN fam_mode fm USING (docdb_family_id)
    ),
    ranked AS (
      SELECT *, ROW_NUMBER() OVER (
                  PARTITION BY psn_name, docdb_family_id
                  ORDER BY match_flag DESC, rnd
                ) AS rnk
      FROM tied_cross
    )
    SELECT psn_name, docdb_family_id, person_ctry_code AS harm_ctry
    FROM ranked WHERE rnk = 1
")
tied_diag <- dbGetQuery(con, "
  WITH by_group AS (
    SELECT psn_name, docdb_family_id, MAX(match_flag) AS any_match
    FROM (
      SELECT tf.psn_name, tf.docdb_family_id,
             CASE WHEN nt.person_ctry_code = fm.fam_mode THEN 1 ELSE 0 END
               AS match_flag
      FROM (SELECT DISTINCT w.psn_name, w.docdb_family_id
            FROM work w
            JOIN cand_counts cc USING (psn_name)
            WHERE cc.n_candidates > 1) tf
      JOIN name_top nt USING (psn_name)
      LEFT JOIN fam_mode fm USING (docdb_family_id)
    )
    GROUP BY psn_name, docdb_family_id
  )
  SELECT COUNT(*)                             AS total,
         SUM(CASE WHEN any_match=1 THEN 1 ELSE 0 END) AS by_mode,
         SUM(CASE WHEN any_match=0 THEN 1 ELSE 0 END) AS by_random
  FROM by_group
")
cat(sprintf("  tied (name, family) resolutions: %s\n",
            format(tied_diag$total, big.mark=",")))
if (tied_diag$total > 0) {
  cat(sprintf("    via family mode : %s (%.1f%%)\n",
              format(tied_diag$by_mode,  big.mark=","),
              100 * tied_diag$by_mode   / tied_diag$total))
  cat(sprintf("    via random pick : %s (%.1f%%)\n",
              format(tied_diag$by_random, big.mark=","),
              100 * tied_diag$by_random / tied_diag$total))
}

# --- Apply harmonization + emit distinct (family, country) per type ---
# Use a view so DuckDB can stream work_harm through the final DISTINCT.
dbExecute(con, "
  CREATE VIEW work_harm AS
    SELECT w.docdb_family_id, w.type,
           COALESCE(nu.harm_ctry, tb.harm_ctry, w.person_ctry_code)
             AS harm_ctry
    FROM work w
    LEFT JOIN name_best_unambig nu
      ON w.psn_name = nu.psn_name
    LEFT JOIN tied_best tb
      ON w.psn_name       = tb.psn_name
     AND w.docdb_family_id = tb.docdb_family_id
")

cat("Applying harmonization & collecting outputs...\n")
# Row-level change diagnostic (cheap single-pass aggregate)
change_diag <- dbGetQuery(con, "
  SELECT COUNT(*) AS total,
         SUM(CASE WHEN harm_ctry != person_ctry_code THEN 1 ELSE 0 END) AS changed
  FROM (
    SELECT w.person_ctry_code,
           COALESCE(nu.harm_ctry, tb.harm_ctry, w.person_ctry_code) AS harm_ctry
    FROM work w
    LEFT JOIN name_best_unambig nu ON w.psn_name = nu.psn_name
    LEFT JOIN tied_best tb
      ON w.psn_name = tb.psn_name AND w.docdb_family_id = tb.docdb_family_id
  )
")
cat("  rows with country changed:", format(change_diag$changed, big.mark=","),
    sprintf(" (%.2f%%)\n", 100 * change_diag$changed / change_diag$total))

# Bring back the (much smaller) distinct (family, country) outputs.
inventor_countries_harm <- as.data.table(dbGetQuery(con, "
  SELECT DISTINCT docdb_family_id, harm_ctry AS person_ctry_code
  FROM work_harm WHERE type = 'inventor'
"))
holder_countries_harm <- as.data.table(dbGetQuery(con, "
  SELECT DISTINCT docdb_family_id, harm_ctry AS person_ctry_code
  FROM work_harm WHERE type = 'holder'
"))

dbDisconnect(con, shutdown = TRUE)
toc()

# ============================================================================
# 8. Write parquet outputs
# ============================================================================

cat("Writing harmonized outputs...\n")
tic("write")

write_parquet(inventor_countries_harm, out_inv,
              compression = "zstd", compression_level = 3)
write_parquet(holder_countries_harm, out_hold,
              compression = "zstd", compression_level = 3)

cat("  inventor_countries_harm rows:", nrow(inventor_countries_harm),
    "  ->", out_inv, "\n")
cat("  holder_countries_harm rows:  ", nrow(holder_countries_harm),
    "  ->", out_hold, "\n")
toc()

cat("=== Done ===\n")
