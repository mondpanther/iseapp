# Build a per-country-file-based nationalkey table.
#
# Instead of relying on the precomputed `innos_ev_nationalkey_2009_2018.dsv`
# in fromWATSON, this script constructs the same notion by:
#   1. Building a countrymap from the harmonized inventor file (and
#      optionally also holders — see INCLUDE_HOLDERS below).
#   2. Restricting that countrymap to countries for which a
#      innos_ev_XX_2009_2018.dsv file exists in fromWATSON.
#   3. Joining the surviving (docdb_family_id, ctry_code) pairs against the
#      per-country ev parquets (built by data-raw/convert_watson_dsvs.R) to
#      attach the ev value.
#
# Output:
#   .bigdata/countrymap.fst   — one row per (docdb_family_id, ctry_code)
#                               that survives the filter
#   .bigdata/nationalkey.fst  — countrymap + ev_nationalkey_2009_2018 column
#
# Set INCLUDE_HOLDERS <- TRUE before sourcing this script if you want to
# attribute families to the union of inventor + holder countries.

library(bigrquery)   # (loaded so the shared find_dropbox_dir helper stays consistent)
library(DBI)
library(duckdb)
library(data.table)
library(arrow)
library(fst)
library(tictoc)
library(jsonlite)

# ---- Paths ----
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
    stop("Could not find Dropbox info.json. Set ISEAPP_DROPBOX_DIR to override.")
  info <- jsonlite::fromJSON(info_path[[1]])
  root <- info$personal$path %||% info$business$path
  if (is.null(root))
    stop("Dropbox info.json did not contain a personal or business path.")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

dropbox_dir <- find_dropbox_dir()
fromWATSON  <- file.path(dropbox_dir, "patbis2021", "data", "fromWATSON")
if (!dir.exists(fromWATSON))
  stop("Expected fromWATSON folder not found: ", fromWATSON)

bigdata_dir <- ".bigdata"
dir.create(bigdata_dir, showWarnings = FALSE)

# Toggle: include holder countries in addition to inventor countries.
# Default FALSE — countrymap reflects where the *inventors* are based.
if (!exists("INCLUDE_HOLDERS")) INCLUDE_HOLDERS <- FALSE

inv_harm_path  <- file.path(bigdata_dir, "inventor_countries_harm.parquet")
hold_harm_path <- file.path(bigdata_dir, "holder_countries_harm.parquet")
required_paths <- if (INCLUDE_HOLDERS) c(inv_harm_path, hold_harm_path) else inv_harm_path
for (p in required_paths) {
  if (!file.exists(p)) {
    stop("Missing ", p,
         "\nRun data-raw/build_inventor_countries_harm.R first to generate it.")
  }
}

countrymap_out  <- file.path(bigdata_dir, "countrymap.fst")
nationalkey_out <- file.path(bigdata_dir, "nationalkey.fst")

cat("=== Building nationalkey from per-country innos_ev_XX files ===\n")

# ============================================================================
# 1. Build candidate countrymap from harmonized inventor (+ optionally holder)
# ============================================================================

src_label <- if (INCLUDE_HOLDERS) "inventor + holder" else "inventor only"
cat("Loading harmonized country mappings (", src_label, ")...\n", sep = "")
tic("load harmonized")

inv <- arrow::read_parquet(inv_harm_path) |> as.data.table()
setnames(inv, "person_ctry_code", "ctry_code")
parts <- list(inv[, .(docdb_family_id, ctry_code)])

if (INCLUDE_HOLDERS) {
  hold <- arrow::read_parquet(hold_harm_path) |> as.data.table()
  setnames(hold, "person_ctry_code", "ctry_code")
  parts <- c(parts, list(hold[, .(docdb_family_id, ctry_code)]))
  rm(hold)
}
rm(inv)

countrymap <- unique(rbindlist(parts, use.names = TRUE))
rm(parts)
countrymap <- countrymap[!is.na(ctry_code) & nzchar(ctry_code)]
countrymap <- countrymap[ctry_code != "KP"]  # exclude North Korea

cat("  countrymap (pre-filter):", nrow(countrymap), "rows,",
    uniqueN(countrymap$docdb_family_id), "families,",
    uniqueN(countrymap$ctry_code), "countries\n")
gc()
toc()

# ============================================================================
# 2. Restrict to countries for which an innos_ev_XX file exists
# ============================================================================

# Prefer parquet versions (produced by data-raw/convert_watson_dsvs.R) over the
# raw DSVs. Parquet reads are ~10x smaller on disk and DuckDB can scan them
# in parallel without going through R memory.
ev_parquet_files <- list.files(
  fromWATSON,
  pattern = "^innos_ev_[A-Z]{2}_2009_2018\\.parquet$",
  full.names = FALSE
)
ev_dsv_files <- list.files(
  fromWATSON,
  pattern = "^innos_ev_[A-Z]{2}_2009_2018\\.dsv$",
  full.names = FALSE
)
# Use parquet whenever any exist — DuckDB queries them in parallel and we
# don't need 100% coverage to benefit. Countries that have a DSV but not a
# parquet will simply not be present in the DuckDB join (the script reports
# them so you can decide whether to convert them).
use_parquet <- length(ev_parquet_files) > 0
if (use_parquet) {
  parquet_cc <- sort(unique(sub("^innos_ev_([A-Z]{2})_2009_2018\\.parquet$",
                                "\\1", ev_parquet_files)))
  dsv_cc     <- sort(unique(sub("^innos_ev_([A-Z]{2})_2009_2018\\.dsv$",
                                "\\1", ev_dsv_files)))
  dsv_only   <- setdiff(dsv_cc, parquet_cc)
  cat("Found", length(parquet_cc),
      "per-country parquet files — using DuckDB path.\n")
  if (length(dsv_only)) {
    cat("  NOTE: ", length(dsv_only),
        " countr", if (length(dsv_only) == 1) "y" else "ies",
        " have a DSV but no parquet (will be dropped from the join):\n  ",
        paste(dsv_only, collapse = ", "), "\n", sep = "")
    cat("  Re-run data-raw/convert_watson_dsvs.R to convert them",
        "(make sure Dropbox has them downloaded locally first).\n")
  }
  avail_cc <- parquet_cc
} else {
  cat("No per-country parquets found — falling back to DSV path.\n")
  cat("Run data-raw/convert_watson_dsvs.R to enable the faster DuckDB path.\n")
  avail_cc <- sort(unique(sub("^innos_ev_([A-Z]{2})_2009_2018\\.dsv$",
                              "\\1", ev_dsv_files)))
}
map_cc     <- sort(unique(countrymap$ctry_code))

missing_cc <- setdiff(map_cc, avail_cc)
extra_cc   <- setdiff(avail_cc, map_cc)

cat("\nCountries in countrymap:            ", length(map_cc), "\n")
cat("Countries with innos_ev_XX files:   ", length(avail_cc), "\n")
cat("Dropped (no per-country file):      ", length(missing_cc), "\n")
if (length(missing_cc))
  cat("  ", paste(missing_cc, collapse = ", "), "\n")
cat("Available but not in countrymap:    ", length(extra_cc), "\n")
if (length(extra_cc) && length(extra_cc) < 50)
  cat("  ", paste(extra_cc, collapse = ", "), "\n")

countrymap <- countrymap[ctry_code %in% avail_cc]
cat("\ncountrymap (post-filter):", nrow(countrymap), "rows,",
    uniqueN(countrymap$docdb_family_id), "families,",
    uniqueN(countrymap$ctry_code), "countries\n\n")

setkey(countrymap, docdb_family_id, ctry_code)
write_fst(countrymap, countrymap_out, compress = 100)
cat("Wrote", countrymap_out, "\n\n")

# ============================================================================
# 3. Join countrymap with all innos_ev_XX files
# ============================================================================

cat("\nJoining countrymap to per-country ev data...\n")

if (use_parquet) {
  # Fast path: one DuckDB query over all parquet files, joined against
  # countrymap in-memory. Replaces a 185-iteration R loop that previously
  # read ~240 GB of DSV data through fread + rbindlist.
  tic("duckdb join")
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con, sprintf("PRAGMA threads = %d",
                         max(1L, parallel::detectCores() - 1L)))
  # Register countrymap as a temp table for the join
  dbWriteTable(con, "countrymap_tbl",
               as.data.frame(countrymap),
               overwrite = TRUE)
  parquet_glob <- file.path(fromWATSON, "innos_ev_*_2009_2018.parquet")
  sql <- sprintf(
    "SELECT e.docdb_family_id,
            e.ctry_code,
            e.ev AS ev_nationalkey_2009_2018
     FROM read_parquet(%s) e
     INNER JOIN countrymap_tbl c
       ON e.docdb_family_id = c.docdb_family_id
      AND e.ctry_code       = c.ctry_code",
    dbQuoteString(con, parquet_glob)
  )
  nationalkey <- setDT(dbGetQuery(con, sql))
  dbDisconnect(con, shutdown = TRUE)
  toc()
} else {
  # Fallback path: the original per-country R loop (DSV fread + data.table join).
  countries <- sort(unique(countrymap$ctry_code))
  pieces <- vector("list", length(countries))
  names(pieces) <- countries

  tic("all per-country joins (DSV fallback)")
  for (i in seq_along(countries)) {
    cc <- countries[i]
    ev_path <- file.path(fromWATSON, sprintf("innos_ev_%s_2009_2018.dsv", cc))
    cat(sprintf("  [%3d/%3d] %s ... ", i, length(countries), cc))

    ev_data <- fread(ev_path, showProgress = FALSE)
    ev_src_col <- if ("ev" %in% names(ev_data)) "ev" else
                  grep("^ev", names(ev_data), value = TRUE)[1]
    if (is.na(ev_src_col)) {
      warning("No ev column in ", ev_path, "; skipping")
      next
    }
    ev_data <- ev_data[, .(docdb_family_id,
                           ev_nationalkey_2009_2018 = get(ev_src_col))]

    fams  <- countrymap[ctry_code == cc, .(docdb_family_id)]
    piece <- ev_data[fams, on = "docdb_family_id", nomatch = 0L]
    piece[, ctry_code := cc]

    pieces[[cc]] <- piece
    cat(sprintf("%d rows\n", nrow(piece)))
    rm(ev_data, fams, piece); gc()
  }
  toc()

  nationalkey <- rbindlist(pieces, use.names = TRUE)
  rm(pieces)
}

# ============================================================================
# 4. Write
# ============================================================================

cat("\nFinalising nationalkey...\n")
tic("write")

setcolorder(nationalkey,
            c("docdb_family_id", "ctry_code", "ev_nationalkey_2009_2018"))
setkey(nationalkey, docdb_family_id, ctry_code)

cat("  nationalkey:", nrow(nationalkey), "rows,",
    uniqueN(nationalkey$docdb_family_id), "families,",
    uniqueN(nationalkey$ctry_code), "countries\n")

write_fst(nationalkey, nationalkey_out, compress = 100)
cat("Wrote", nationalkey_out, "\n")
toc()

cat("=== Done ===\n")
