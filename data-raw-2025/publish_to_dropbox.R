# publish_to_dropbox.R
# ---------------------------------------------------------------------------
# Publish the built app database to the shared Dropbox folder.
#
#   inst/extdata/*.parquet   ->  <dropbox>/iseapp/database/
#   .bigdata/<shared>.fst    ->  <dropbox>/iseapp/bigdata/
#
# Note the destination is <dropbox>/iseapp/, NOT <dropbox>/Apps/iseapp/.
# Those are two different folders:
#   Apps/iseapp/   legacy assets - istraxes/, inglobe/, duck/, firmmap.parquet,
#                  LMICinnovation_repo/
#   iseapp/        the collaboration folder. Its database/ is what
#                  LMICinnovation/code2025 and code_linkedin read.
#
# Until now this copy was manual, which is why nothing guaranteed the shared
# copy matched the build. Call publish_iseapp_database() at the end of a
# rebuild and it is guaranteed.
#
# Standalone use, from the iseapp project root:
#   source("data-raw-2025/publish_to_dropbox.R"); publish_iseapp_database()
#   publish_iseapp_database(dry_run = TRUE)   # show what would change
# ---------------------------------------------------------------------------

suppressMessages({ library(jsonlite) })

# .bigdata intermediates worth sharing. Currently none: cpcs.fst used to be
# here, but LMICinnovation now reads tls225_docdb_fam_cpc from patstat_clean
# directly (and caches only the per-family count), so publishing a 1.9 GB
# duplicate bought nothing. Add a filename here if something downstream starts
# needing one.
ISEAPP_SHARED_BIGDATA <- character(0)

.pub_dropbox_root <- function() {
  # Reuse the caller's resolver when sourced from 01-build-app-parquets.R,
  # otherwise read Dropbox's own info.json.
  if (exists("dropbox_dir", inherits = TRUE) &&
      is.character(get("dropbox_dir", inherits = TRUE)))
    return(get("dropbox_dir", inherits = TRUE))
  if (exists("find_dropbox_dir", inherits = TRUE))
    return(get("find_dropbox_dir", inherits = TRUE)())
  info <- if (.Platform$OS.type == "windows")
    c(file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json"),
      file.path(Sys.getenv("APPDATA"),      "Dropbox", "info.json"))
  else path.expand("~/.dropbox/info.json")
  for (p in info) if (file.exists(p)) {
    j <- fromJSON(p, simplifyVector = FALSE)
    for (k in c("business", "personal")) if (!is.null(j[[k]]$path)) return(j[[k]]$path)
  }
  stop("Could not locate the Dropbox folder (no info.json).")
}

iseapp_shared_dir <- function() {
  override <- Sys.getenv("ISEAPP_SHARED_DIR", "")
  if (nzchar(override)) return(override)
  file.path(.pub_dropbox_root(), "iseapp")
}

.same_file <- function(a, b) {
  file.exists(a) && file.exists(b) &&
    isTRUE(file.size(a) == file.size(b)) &&
    isTRUE(abs(as.numeric(difftime(file.mtime(a), file.mtime(b), units = "secs"))) < 2)
}

.publish_one <- function(src, dst, dry_run) {
  if (.same_file(src, dst)) return("current")
  if (dry_run) return(if (file.exists(dst)) "would update" else "would add")
  dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(dst, ".tmp")
  if (!file.copy(src, tmp, overwrite = TRUE, copy.date = TRUE)) {
    unlink(tmp); return("FAILED")
  }
  file.rename(tmp, dst)
  if (file.exists(dst)) "published" else "FAILED"
}

publish_iseapp_database <- function(extdata  = "inst/extdata",
                                    bigdata  = ".bigdata",
                                    shared   = NULL,
                                    bigdata_files = ISEAPP_SHARED_BIGDATA,
                                    dry_run  = FALSE) {
  shared <- if (is.null(shared)) iseapp_shared_dir() else shared
  db_dst <- file.path(shared, "database")
  bd_dst <- file.path(shared, "bigdata")

  cat("\n=== Publishing iseapp database ===\n")
  cat("  from : ", normalizePath(extdata, mustWork = FALSE), "\n", sep = "")
  cat("  to   : ", db_dst, "\n", sep = "")
  if (dry_run) cat("  (dry run - nothing will be written)\n")

  if (!dir.exists(shared))
    stop("Shared folder not found: ", shared,
         "\nIs the Dropbox 'iseapp' folder synced on this machine?",
         "\nOverride with ISEAPP_SHARED_DIR if it lives elsewhere.")

  manifest <- list(); counts <- c(published = 0, current = 0, failed = 0)
  publish_set <- function(files, src_dir, dst_dir, kind) {
    for (f in files) {
      src <- file.path(src_dir, f); dst <- file.path(dst_dir, f)
      st  <- .publish_one(src, dst, dry_run)
      cat(sprintf("  %-28s %8.1f MB  %s\n", f, file.size(src) / 2^20, st))
      if (st == "published")      counts["published"] <<- counts["published"] + 1
      else if (st == "current")   counts["current"]   <<- counts["current"]   + 1
      else if (st == "FAILED")    counts["failed"]    <<- counts["failed"]    + 1
      manifest[[f]] <<- list(kind = kind,
                             bytes = as.numeric(file.size(src)),
                             mtime = format(file.mtime(src), "%Y-%m-%dT%H:%M:%S"))
    }
  }

  pq <- list.files(extdata, pattern = "\\.parquet$")
  if (!length(pq)) stop("No parquets in ", extdata, " - has the build run?")
  publish_set(pq, extdata, db_dst, "database")

  bd <- bigdata_files[file.exists(file.path(bigdata, bigdata_files))]
  if (length(bd)) {
    cat("\n  shared .bigdata intermediates ->", bd_dst, "\n")
    publish_set(bd, bigdata, bd_dst, "bigdata")
  }

  if (!dry_run) {
    meta <- list(published_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                 published_by = paste0(Sys.info()[["user"]], "@",
                                       Sys.info()[["nodename"]]),
                 files = manifest)
    tmp <- file.path(shared, "manifest.json.tmp")
    write_json(meta, tmp, auto_unbox = TRUE, pretty = TRUE)
    file.rename(tmp, file.path(shared, "manifest.json"))
  }

  cat(sprintf("\n  %d published, %d already current, %d failed\n",
              counts["published"], counts["current"], counts["failed"]))
  if (counts["failed"] > 0)
    warning("publish_iseapp_database: ", counts["failed"], " file(s) failed to copy.")
  invisible(counts)
}
