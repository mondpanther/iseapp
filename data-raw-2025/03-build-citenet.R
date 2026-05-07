# Build filtered citation network parquet for the ISE app.
#
# Source:  patbis2025/data/fromPATSTAT/citenet_noself.parquet
#          (one row per citation between two docdb families, self-citations
#           already removed; ~278M rows)
# Output:  inst/extdata/citenet.parquet
#          Restricted to citations where BOTH endpoints (citing
#          docdb_family_id and cited_docdb_family_id) are present in
#          inst/extdata/patent_database.parquet — i.e. the family is in
#          the Shiny app's main database. Since 01-build-app-parquets.R
#          augments countrymap so every (docdb, ctry) in patent_database
#          has coordinates, this single check is sufficient: every kept
#          citation has plottable endpoints in HiGGlobe.
#
# Depends on 01-build-app-parquets.R having published patent_database.parquet
# (and the augmented countrymap.parquet).

library(DBI)
library(duckdb)

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    stop("Could not find Dropbox info.json. ",
         "Set ISEAPP_DROPBOX_DIR to override.")
  info <- jsonlite::fromJSON(info_path[[1]])
  root <- info$personal$path %||% info$business$path
  if (is.null(root))
    stop("Dropbox info.json did not contain a personal or business path.")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

dropbox_dir <- find_dropbox_dir()
citenet_src <- file.path(dropbox_dir, "patbis2025", "data",
                         "fromPATSTAT", "citenet_noself.parquet")
if (!file.exists(citenet_src))
  stop("Source citenet parquet not found: ", citenet_src)

patent_db_pq <- "inst/extdata/patent_database.parquet"
if (!file.exists(patent_db_pq))
  stop(patent_db_pq, " not found — run 01-build-app-parquets.R first.")

out_path  <- "inst/extdata/citenet.parquet"
tmp_path  <- paste0(out_path, ".tmp-", Sys.getpid())

cat("=== Build filtered citenet.parquet ===\n")
cat("  source: ", citenet_src, "\n")
cat("  output: ", out_path,    "\n")

t0 <- Sys.time()
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
dbExecute(con, sprintf("PRAGMA threads = %d",
                       max(1L, parallel::detectCores() - 1L)))

# Distinct families present in the Shiny app's main database.
dbExecute(con, sprintf("
  CREATE TEMP TABLE fam AS
  SELECT DISTINCT docdb_family_id
  FROM read_parquet('%s')
  WHERE docdb_family_id IS NOT NULL
", patent_db_pq))
n_fam <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM fam")$n
cat(sprintf("  patent_database families: %s\n", format(n_fam, big.mark = ",")))

# Keep only citations where both endpoints are in that family set.
dbExecute(con, sprintf("
  COPY (
    SELECT c.docdb_family_id, c.cited_docdb_family_id
    FROM read_parquet('%s') c
    WHERE c.docdb_family_id       IN (SELECT docdb_family_id FROM fam)
      AND c.cited_docdb_family_id IN (SELECT docdb_family_id FROM fam)
  ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)
", citenet_src, tmp_path))

if (!file.rename(tmp_path, out_path)) {
  for (i in 1:5) {
    Sys.sleep(1)
    if (file.rename(tmp_path, out_path)) break
  }
  if (file.exists(tmp_path))
    stop("Could not rename ", tmp_path, " to ", out_path,
         " (is another process holding the file?).")
}

n_rows <- dbGetQuery(con, sprintf(
  "SELECT COUNT(*) AS n FROM read_parquet('%s')", out_path))$n
sz_mb <- round(file.info(out_path)$size / 1024 / 1024, 1)
mins  <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
cat(sprintf("  wrote %s rows (%.1f MB) in %.2f min\n",
            format(n_rows, big.mark = ","), sz_mb, mins))

dbDisconnect(con, shutdown = TRUE)
