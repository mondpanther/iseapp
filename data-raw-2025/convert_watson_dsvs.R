# One-shot conversion of Watson per-country innos_ev_XX_2009_2018.dsv files
# to compact parquet files for DuckDB-backed queries downstream.
#
# Input:  <dropbox>/patbis2021/data/fromWATSON/innos_ev_XX_2009_2018.dsv
# Output: <dropbox>/patbis2021/data/fromWATSON/innos_ev_XX_2009_2018.parquet
#         (one per country, alongside the DSV)
#
# Only the columns needed by the app pipeline are kept:
#   ctry_code        (added here as a literal per source file)
#   docdb_family_id
#   ev
#
# The rewrite of data-raw/build_nationalkey.R step 3 reads all per-country
# parquets in a single DuckDB query instead of looping in R, which avoids
# loading ~185 × 1.3 GB of DSV data through R memory.
#
# Safe to re-run: existing parquet files are skipped. Delete any file to
# force re-conversion.

library(DBI)
library(duckdb)
library(jsonlite)

# ---- Dropbox auto-discovery (same helper used elsewhere) ----
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

# Parquets are written next to the DSVs so Dropbox keeps them in sync.
out_dir <- fromWATSON

ev_files <- list.files(fromWATSON,
                       pattern = "^innos_ev_[A-Z]{2}_2009_2018\\.dsv$")
avail_cc <- sub("^innos_ev_([A-Z]{2})_2009_2018\\.dsv$", "\\1", ev_files)

cat("=== Converting Watson per-country ev files to parquet ===\n")
cat("Input/output dir:", fromWATSON, "\n")
cat("Files found:", length(ev_files), "\n\n")

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
dbExecute(con, sprintf("PRAGMA threads = %d",
                       max(1L, parallel::detectCores() - 1L)))

t0 <- Sys.time()
n_converted <- 0L
n_skipped   <- 0L
n_failed    <- 0L
failed_cc   <- character()

# Force Dropbox to hydrate a cloud-only placeholder by touching the first few
# bytes. Returns TRUE if the file is readable afterwards, FALSE otherwise.
hydrate_dropbox_file <- function(path, bytes = 4096L, wait_sec = 30L) {
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  t_end <- Sys.time() + wait_sec
  repeat {
    ok <- tryCatch({ readBin(con, "raw", n = bytes); TRUE },
                   error = function(e) FALSE)
    if (ok) return(TRUE)
    if (Sys.time() >= t_end) return(FALSE)
    Sys.sleep(2)
    seek(con, 0L)
  }
}

for (i in seq_along(ev_files)) {
  cc  <- avail_cc[i]
  src <- file.path(fromWATSON, ev_files[i])
  dst <- file.path(out_dir, sprintf("innos_ev_%s_2009_2018.parquet", cc))

  if (file.exists(dst)) {
    cat(sprintf("  [%3d/%3d] %s  (exists, skipping)\n",
                i, length(ev_files), cc))
    n_skipped <- n_skipped + 1L
    next
  }

  cat(sprintf("  [%3d/%3d] %s -> %s ... ",
              i, length(ev_files), cc, basename(dst)))
  t_start <- Sys.time()

  # DuckDB streams from DSV -> parquet without loading the file into R memory.
  sql <- sprintf(
    "COPY (
       SELECT '%s' AS ctry_code,
              docdb_family_id::INTEGER AS docdb_family_id,
              ev::DOUBLE              AS ev
       FROM read_csv_auto(%s, delim='\t', header=true)
     ) TO %s (FORMAT PARQUET, COMPRESSION 'zstd')",
    cc,
    dbQuoteString(con, src),
    dbQuoteString(con, dst)
  )

  run_copy <- function() dbExecute(con, sql)
  err <- tryCatch(run_copy(), error = function(e) e)

  # Retry once after forcing Dropbox to hydrate the file — helps with
  # "cloud operation was unsuccessful" errors on online-only files.
  if (inherits(err, "error") &&
      grepl("cloud operation|IO Error|Could not read",
            conditionMessage(err), ignore.case = TRUE)) {
    cat("cloud file not local, hydrating... ")
    if (hydrate_dropbox_file(src)) {
      err <- tryCatch(run_copy(), error = function(e) e)
    }
  }

  if (inherits(err, "error")) {
    cat("FAILED (", conditionMessage(err), ")\n", sep = "")
    n_failed  <- n_failed + 1L
    failed_cc <- c(failed_cc, cc)
    # Best-effort cleanup of a partial parquet
    if (file.exists(dst)) unlink(dst)
    next
  }

  dt <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
  sz <- round(file.info(dst)$size / 1024^2, 1)
  cat(sprintf("done (%s sec, %.1f MB)\n", dt, sz))
  n_converted <- n_converted + 1L
}

# ----------------------------------------------------------------------------
# Additional Watson files used by data-raw/01-build-app-parquets.R step 10:
#   - innos_istraxfield_global_2009_2018.dsv (global pv/ev/cost/alpha/istrax)
#   - innos_ev_nationalkey_2009_2018.dsv     (legacy nationalkey ev)
#   - innos_ev_<group>_2009_2018.dsv         for emde, eu, g7, euplusuk,
#                                            emdenocn, emdenocnin, hic
# These are converted with SELECT * (all columns kept). The 5 country files
# already used by step 10c (AT, GB, DE, FR, US) were converted in the
# per-country loop above and are not duplicated here.
# ----------------------------------------------------------------------------

extra_files <- c(
  "innos_istraxfield_global_2009_2018.dsv",
  "innos_ev_nationalkey_2009_2018.dsv",
  paste0("innos_ev_",
         c("emde", "eu", "g7", "euplusuk",
           "emdenocn", "emdenocnin", "hic"),
         "_2009_2018.dsv")
)

cat("\n=== Converting additional Watson files (step 10 sources) ===\n")

for (fn in extra_files) {
  src <- file.path(fromWATSON, fn)
  dst <- sub("\\.dsv$", ".parquet", src)

  if (!file.exists(src)) {
    cat(sprintf("  Skip %s (source missing)\n", fn))
    next
  }
  if (file.exists(dst)) {
    cat(sprintf("  Skip %s (parquet already exists)\n", fn))
    n_skipped <- n_skipped + 1L
    next
  }

  cat(sprintf("  Converting %s ... ", fn))
  t_start <- Sys.time()
  sql <- sprintf(
    "COPY (SELECT * FROM read_csv_auto(%s, delim='\t', header=true))
     TO %s (FORMAT PARQUET, COMPRESSION 'zstd')",
    dbQuoteString(con, src),
    dbQuoteString(con, dst)
  )

  run_copy <- function() dbExecute(con, sql)
  err <- tryCatch(run_copy(), error = function(e) e)

  if (inherits(err, "error") &&
      grepl("cloud operation|IO Error|Could not read",
            conditionMessage(err), ignore.case = TRUE)) {
    cat("cloud file not local, hydrating... ")
    if (hydrate_dropbox_file(src, wait_sec = 60L)) {
      err <- tryCatch(run_copy(), error = function(e) e)
    }
  }

  if (inherits(err, "error")) {
    cat("FAILED (", conditionMessage(err), ")\n", sep = "")
    n_failed  <- n_failed + 1L
    failed_cc <- c(failed_cc, fn)
    if (file.exists(dst)) unlink(dst)
    next
  }

  dt <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
  sz <- round(file.info(dst)$size / 1024^2, 1)
  cat(sprintf("done (%s sec, %.1f MB)\n", dt, sz))
  n_converted <- n_converted + 1L
}

# ----------------------------------------------------------------------------
# PATSTAT files used by data-raw/01-build-app-parquets.R:
#   - innos_pub.dsv (~7 GB) — used to build the Espacenet-searchable appln_id
#     per family. Only docdb_family_id, publn_auth, publn_nr are read by
#     01-build, so we narrow the conversion to those three columns to keep
#     the parquet small (~300-500 MB instead of >1 GB).
# ----------------------------------------------------------------------------

fromPATSTAT <- file.path(dropbox_dir, "patbis2021", "data", "fromPATSTAT")

patstat_files <- list(
  list(name = "innos_pub.dsv",
       columns = c("docdb_family_id", "publn_auth", "publn_nr"))
)

if (dir.exists(fromPATSTAT)) {
  cat("\n=== Converting PATSTAT files ===\n")
  con2 <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con2, sprintf("PRAGMA threads = %d",
                          max(1L, parallel::detectCores() - 1L)))

  for (spec in patstat_files) {
    src <- file.path(fromPATSTAT, spec$name)
    dst <- file.path(fromPATSTAT, sub("\\.dsv$", ".parquet", spec$name))

    if (!file.exists(src)) {
      cat(sprintf("  Skip %s (source missing)\n", spec$name))
      next
    }
    if (file.exists(dst)) {
      cat(sprintf("  Skip %s (parquet already exists)\n", spec$name))
      n_skipped <- n_skipped + 1L
      next
    }

    cat(sprintf("  Converting %s (cols: %s) ... ",
                spec$name, paste(spec$columns, collapse = ", ")))
    t_start <- Sys.time()
    cols_sql <- paste(spec$columns, collapse = ", ")
    sql <- sprintf(
      "COPY (SELECT %s FROM read_csv_auto(%s, delim='\t', header=true))
       TO %s (FORMAT PARQUET, COMPRESSION 'zstd')",
      cols_sql,
      dbQuoteString(con2, src),
      dbQuoteString(con2, dst)
    )

    run_copy <- function() dbExecute(con2, sql)
    err <- tryCatch(run_copy(), error = function(e) e)

    if (inherits(err, "error") &&
        grepl("cloud operation|IO Error|Could not read",
              conditionMessage(err), ignore.case = TRUE)) {
      cat("cloud file not local, hydrating... ")
      if (hydrate_dropbox_file(src, wait_sec = 120L)) {
        err <- tryCatch(run_copy(), error = function(e) e)
      }
    }

    if (inherits(err, "error")) {
      cat("FAILED (", conditionMessage(err), ")\n", sep = "")
      n_failed  <- n_failed + 1L
      failed_cc <- c(failed_cc, spec$name)
      if (file.exists(dst)) unlink(dst)
      next
    }

    dt <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
    sz <- round(file.info(dst)$size / 1024^2, 1)
    cat(sprintf("done (%s sec, %.1f MB)\n", dt, sz))
    n_converted <- n_converted + 1L
  }
  dbDisconnect(con2, shutdown = TRUE)
} else {
  cat("\nfromPATSTAT folder not found at ", fromPATSTAT,
      " — skipping PATSTAT conversions.\n", sep = "")
}

dbDisconnect(con, shutdown = TRUE)

total_sec <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
total_mb  <- round(sum(file.info(list.files(out_dir, pattern = "\\.parquet$",
                                            full.names = TRUE))$size) / 1024^2, 1)
cat(sprintf("\nConverted: %d  Skipped: %d  Failed: %d  Elapsed: %s sec\n",
            n_converted, n_skipped, n_failed, total_sec))
cat(sprintf("Total parquet output size: %.1f MB  (%s)\n",
            total_mb, out_dir))
if (n_failed > 0) {
  cat("Failed country codes: ", paste(failed_cc, collapse = ", "), "\n")
  cat("These are usually Dropbox 'online-only' files. Right-click the\n")
  cat("fromWATSON folder in Explorer and choose 'Always keep on this device',\n")
  cat("wait for them to download, then re-run this script (it will pick up\n")
  cat("where it left off).\n")
}
cat("Done.\n")
