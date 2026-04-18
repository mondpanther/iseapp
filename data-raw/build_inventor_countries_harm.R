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
#      per (name × family) using the mode country across all persons
#      (inventors + holders) in that family.
#   3. If still tied at the family level, keep the original country code.
#
# Outputs:
#   .bigdata/inventor_countries_harm.parquet
#   .bigdata/holder_countries_harm.parquet
#
# Intermediate cache (to avoid repeated BigQuery downloads):
#   .bigdata/tls206_person.fst

library(bigrquery)
library(DBI)
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

persons_cache <- file.path(bigdata_dir, "tls206_person.fst")
persons_local <- file.path(iseapp_dir, "inglobe", "persons.parquet")
out_inv  <- file.path(bigdata_dir, "inventor_countries_harm.parquet")
out_hold <- file.path(bigdata_dir, "holder_countries_harm.parquet")

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

if (file.exists(persons_cache)) {
  cat("Loading cached persons from", persons_cache, "...\n")
  persons <- read_fst(persons_cache, as.data.table = TRUE)
} else {
  tic("persons load")
  if (file.exists(persons_local)) {
    persons <- load_persons_from_parquet(persons_local)
  } else {
    cat("Local persons parquet not found at", persons_local, "\n")
    cat("Falling back to BigQuery.\n")
    persons <- load_persons_from_bigquery()
  }

  # Light normalization: trim + uppercase to absorb trivial whitespace/case differences
  persons[, psn_name := toupper(trimws(psn_name))]
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
  cat("  persons rows after filter:", nrow(persons),
      sprintf(" (dropped %d)\n", n_before - nrow(persons)))
  cat("  Caching to", persons_cache, "...\n")
  write_fst(persons, persons_cache, compress = 100)
  toc()
}

# ============================================================================
# 2. Load inventor + holder bridges and combine
# ============================================================================

cat("Loading inventors and holders bridges...\n")
tic("bridges")

inventors <- read_parquet(
  file.path(iseapp_dir, "inglobe", "data", "inventors.parquet")
)
holders <- read_parquet(
  file.path(iseapp_dir, "inglobe", "data", "holders.parquet")
)
setDT(inventors)
setDT(holders)
inventors[, type := "inventor"]
holders[,   type := "holder"]

both <- rbindlist(list(inventors, holders), use.names = TRUE)
rm(inventors, holders)
cat("  combined rows:", nrow(both), "\n")
toc()

# ============================================================================
# 3. Join persons onto bridges
# ============================================================================

cat("Joining persons onto bridges...\n")
tic("person join")

# Use data.table merge; drop rows with no person match
setkey(persons, person_id)
work <- persons[both, on = "person_id", nomatch = 0L]
rm(both)
setnames(work, c("person_ctry_code"), c("person_ctry_code"))  # no-op, explicit
cat("  work rows:", nrow(work), "\n")
toc()

# ============================================================================
# 4. Per-name country counts -> candidate sets
# ============================================================================

cat("Computing per-name country counts...\n")
tic("name counts")

name_ctry_counts <- work[, .(N = .N), by = .(psn_name, person_ctry_code)]
name_max <- name_ctry_counts[, .(max_N = max(N)), by = psn_name]
name_top <- name_ctry_counts[name_max, on = "psn_name"][N == max_N]
# name_top: rows of (psn_name, person_ctry_code, N, max_N); tied names have
# multiple rows per psn_name

cand_counts <- name_top[, .(n_candidates = .N), by = psn_name]

unambig_names <- cand_counts[n_candidates == 1, psn_name]
tied_names    <- cand_counts[n_candidates >  1, psn_name]

name_best_unambig <- name_top[psn_name %in% unambig_names,
                              .(psn_name, harm_ctry = person_ctry_code)]
setkey(name_best_unambig, psn_name)

cat("  unique names:           ", nrow(cand_counts), "\n")
cat("  unambiguous names:      ", length(unambig_names), "\n")
cat("  tied names (need break):", length(tied_names), "\n")
toc()

# ============================================================================
# 5. Family-level mode country (for tie-breaking)
# ============================================================================

cat("Computing family-level mode country...\n")
tic("family mode")

fam_ctry_counts <- work[, .(N = .N), by = .(docdb_family_id, person_ctry_code)]
setorder(fam_ctry_counts, docdb_family_id, -N)
fam_mode <- fam_ctry_counts[, .SD[1], by = docdb_family_id][
  , .(docdb_family_id, fam_mode = person_ctry_code)
]
setkey(fam_mode, docdb_family_id)
cat("  family modes:", nrow(fam_mode), "\n")
toc()

# ============================================================================
# 6. Resolve ties per (tied name × family)
# ============================================================================

cat("Resolving tied names via family mode...\n")
tic("tie resolution")

if (length(tied_names) > 0) {
  # Which families does each tied name appear in?
  tied_name_fams <- unique(work[psn_name %in% tied_names,
                                .(psn_name, docdb_family_id)])

  # Candidate countries per tied name
  tied_candidates <- name_top[psn_name %in% tied_names,
                              .(psn_name, person_ctry_code)]

  # Cross candidates × (name, family) and attach fam_mode
  tied_cross <- tied_candidates[tied_name_fams, on = "psn_name",
                                allow.cartesian = TRUE]
  tied_cross <- fam_mode[tied_cross, on = "docdb_family_id"]
  tied_cross[, match_flag := as.integer(person_ctry_code == fam_mode)]
  # Replace NAs (no fam_mode match) with 0
  tied_cross[is.na(match_flag), match_flag := 0L]

  # Prefer rows whose candidate matches the family mode, else first candidate
  setorder(tied_cross, psn_name, docdb_family_id, -match_flag)
  tied_best <- tied_cross[, .SD[1], by = .(psn_name, docdb_family_id)][
    , .(psn_name, docdb_family_id, harm_ctry = person_ctry_code)
  ]
  setkeyv(tied_best, c("psn_name", "docdb_family_id"))
  cat("  tied (name, family) resolutions:", nrow(tied_best), "\n")
} else {
  tied_best <- data.table(psn_name = character(),
                          docdb_family_id = integer(),
                          harm_ctry = character())
}
toc()

# ============================================================================
# 7. Apply harmonization to the work table
# ============================================================================

cat("Applying harmonization...\n")
tic("apply")

# Unambiguous: same country for every family the name appears in
work[name_best_unambig, on = "psn_name", harm_ctry := i.harm_ctry]

# Tied: resolved per (name, family)
work[tied_best, on = c("psn_name", "docdb_family_id"),
     harm_ctry := ifelse(is.na(harm_ctry), i.harm_ctry, harm_ctry)]

# Fallback for any residual NA -> original person_ctry_code
work[is.na(harm_ctry), harm_ctry := person_ctry_code]

n_changed <- sum(work$harm_ctry != work$person_ctry_code, na.rm = TRUE)
cat("  rows with country changed:", n_changed,
    sprintf(" (%.2f%%)\n", 100 * n_changed / nrow(work)))
toc()

# ============================================================================
# 8. Build outputs and write parquet
# ============================================================================

cat("Writing harmonized outputs...\n")
tic("write")

inventor_countries_harm <- unique(
  work[type == "inventor", .(docdb_family_id, person_ctry_code = harm_ctry)]
)
holder_countries_harm <- unique(
  work[type == "holder", .(docdb_family_id, person_ctry_code = harm_ctry)]
)

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
