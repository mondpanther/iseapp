# Build a per-country-file-based nationalkey table.
#
# Instead of relying on the precomputed `innos_ev_nationalkey_2009_2018.dsv`
# in fromWATSON, this script constructs the same notion by:
#   1. Building a countrymap as the UNION of harmonized inventor and holder
#      countries (produced by data-raw/build_inventor_countries_harm.R).
#   2. Restricting that countrymap to countries for which a
#      innos_ev_XX_2009_2018.dsv file exists in fromWATSON.
#   3. Looping over each remaining country, reading its per-country ev file,
#      and attaching the ev value to every (docdb_family_id, ctry_code)
#      row in countrymap.
#
# Output:
#   .bigdata/countrymap.fst   — one row per (docdb_family_id, ctry_code)
#                               that survives the filter
#   .bigdata/nationalkey.fst  — countrymap + ev_nationalkey_2009_2018 column

library(bigrquery)   # (loaded so the shared find_dropbox_dir helper stays consistent)
library(DBI)
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

inv_harm_path  <- file.path(bigdata_dir, "inventor_countries_harm.parquet")
hold_harm_path <- file.path(bigdata_dir, "holder_countries_harm.parquet")
for (p in c(inv_harm_path, hold_harm_path)) {
  if (!file.exists(p)) {
    stop("Missing ", p,
         "\nRun data-raw/build_inventor_countries_harm.R first to generate it.")
  }
}

countrymap_out  <- file.path(bigdata_dir, "countrymap.fst")
nationalkey_out <- file.path(bigdata_dir, "nationalkey.fst")

cat("=== Building nationalkey from per-country innos_ev_XX files ===\n")

# ============================================================================
# 1. Build candidate countrymap from harmonized inventor + holder union
# ============================================================================

cat("Loading harmonized inventor + holder mappings...\n")
tic("load harmonized")

inv  <- arrow::read_parquet(inv_harm_path)  |> as.data.table()
hold <- arrow::read_parquet(hold_harm_path) |> as.data.table()
setnames(inv,  "person_ctry_code", "ctry_code")
setnames(hold, "person_ctry_code", "ctry_code")

countrymap <- unique(rbindlist(list(
  inv [, .(docdb_family_id, ctry_code)],
  hold[, .(docdb_family_id, ctry_code)]
)))
countrymap <- countrymap[!is.na(ctry_code) & nzchar(ctry_code)]
countrymap <- countrymap[ctry_code != "KP"]  # exclude North Korea

cat("  countrymap (pre-filter):", nrow(countrymap), "rows,",
    uniqueN(countrymap$docdb_family_id), "families,",
    uniqueN(countrymap$ctry_code), "countries\n")
rm(inv, hold); gc()
toc()

# ============================================================================
# 2. Restrict to countries for which an innos_ev_XX file exists
# ============================================================================

ev_files   <- list.files(
  fromWATSON,
  pattern = "^innos_ev_[A-Z]{2}_2009_2018\\.dsv$",
  full.names = FALSE
)
avail_cc   <- sub("^innos_ev_([A-Z]{2})_2009_2018\\.dsv$", "\\1", ev_files)
avail_cc   <- sort(unique(avail_cc))
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
# 3. For each country, join countrymap with its innos_ev_XX file
# ============================================================================

countries <- sort(unique(countrymap$ctry_code))
pieces <- vector("list", length(countries))
names(pieces) <- countries

tic("all per-country joins")
for (i in seq_along(countries)) {
  cc <- countries[i]
  ev_path <- file.path(fromWATSON, sprintf("innos_ev_%s_2009_2018.dsv", cc))
  cat(sprintf("  [%3d/%3d] %s ... ", i, length(countries), cc))

  ev_data <- fread(ev_path, showProgress = FALSE)
  # Find the ev column (per-country files have columns: (index), docdb_family_id, pv, ev, v)
  ev_src_col <- if ("ev" %in% names(ev_data)) "ev" else
                grep("^ev", names(ev_data), value = TRUE)[1]
  if (is.na(ev_src_col)) {
    warning("No ev column in ", ev_path, "; skipping")
    next
  }
  ev_data <- ev_data[, .(docdb_family_id, ev_nationalkey_2009_2018 = get(ev_src_col))]

  # Inner-join with the families in countrymap for this country
  fams <- countrymap[ctry_code == cc, .(docdb_family_id)]
  piece <- ev_data[fams, on = "docdb_family_id", nomatch = 0L]
  piece[, ctry_code := cc]

  pieces[[cc]] <- piece
  cat(sprintf("%d rows\n", nrow(piece)))

  rm(ev_data, fams, piece); gc()
}
toc()

# ============================================================================
# 4. Collate and write
# ============================================================================

cat("\nCollating per-country pieces...\n")
tic("collate + write")

nationalkey <- rbindlist(pieces, use.names = TRUE)
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
