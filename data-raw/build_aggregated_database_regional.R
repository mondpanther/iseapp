# Build Aggregated Patent Database - Regional (UK Regions)
#
# Produces ONE parquet file:
#   - inst/extdata/aggregated_by_region.parquet
#     (by-region aggregation via compute_avstrax_for_techs, one row per UK region)
#
# Calls existing compute_avstrax_for_techs() directly.
# Expands beyond old script: ALL individual UK regions + ALL individual techs + firm dimension.
# Batches writes to parquet every 10 toflows to avoid Windows file locking.

library(fst)
library(dplyr)
library(tidyr)
library(arrow)
library(data.table)
library(collapse)
library(rlang)

cat("=== BUILDING AGGREGATED PATENT DATABASE - REGIONAL ===\n\n")

# ============================================================================
# SOURCE EXISTING FUNCTIONS
# ============================================================================

source("R/functions_istraxfunctions.R")

# ============================================================================
# LOAD BASE DATA
# ============================================================================

cat("Loading base data...\n")

regionmap_path <- "data-raw/big_files/regionmap.fst"

if (!file.exists(regionmap_path)) {
  stop("regionmap.fst not found. Regional aggregation cannot proceed.\n",
       "Expected location: ", regionmap_path)
}

regionmap <- fst::read_fst(regionmap_path)
cat("  regionmap:", nrow(regionmap), "rows\n")

techmap <- fst::read_fst("data-raw/big_files/techmap.fst")
cat("  techmap:", nrow(techmap), "rows\n")

firmmap <- fst::read_fst("data-raw/big_files/firmmap.fst")
cat("  firmmap:", nrow(firmmap), "rows\n\n")

# ============================================================================
# PREPARE TECHMAP (same logic as build_aggregated_database_v2.R)
# ============================================================================

cat("Preparing techmap...\n")

# Add "All" category from distinct patents in regionmap
techmap <- regionmap |>
  dplyr::select(docdb_family_id) |>
  dplyr::distinct() |>
  dplyr::mutate(technology = "All") |>
  dplyr::bind_rows(techmap)

# Standardize technology names
data.table::setDT(techmap)
techmap[, technology := data.table::fcase(
  technology == "Any Green technology", "Green Technology",
  technology == "Any battery technology", "Battery Technology",
  technology == "Any Hard to Abate technology", "Hard to Abate Sector Decarbonization",
  default = technology
)]

cat("  Unique technologies:", dplyr::n_distinct(techmap$technology), "\n\n")

# ============================================================================
# FIND ALL TOFLOW FILES
# ============================================================================

cat("Finding toflow measures...\n")

istrax_dir <- "data-raw/big_files/istraxes"
istrax_files <- list.files(istrax_dir, pattern = "\\.fst$", full.names = FALSE)
istrax_files <- istrax_files[!grepl("_joined", istrax_files)]
toflows <- tools::file_path_sans_ext(istrax_files)

cat("  Found", length(toflows), "toflow measures\n")
cat("  Examples:", paste(head(toflows, 5), collapse = ", "), "...\n\n")

# ============================================================================
# DEFINE ALL TECHNOLOGY SELECTIONS
# ============================================================================

# Start with the 6 broad categories
tech_selections <- list(
  "All" = "All",
  "Green_Technology" = "Green Technology",
  "Battery_Technology" = "Battery Technology",
  "Hard_to_Abate" = "Hard to Abate Sector Decarbonization",
  "AI" = "AI",
  "Other" = "Other"
)

# Add every individual technology as its own selection
all_tech_names <- sort(unique(techmap$technology))
all_tech_names <- setdiff(all_tech_names, "All")

for (tech_name in all_tech_names) {
  safe_key <- gsub("[^a-zA-Z0-9]", "_", tech_name)
  safe_key <- gsub("_+", "_", safe_key)
  safe_key <- gsub("^_|_$", "", safe_key)
  if (!safe_key %in% names(tech_selections)) {
    tech_selections[[safe_key]] <- tech_name
  }
}

cat("Technology selections:", length(tech_selections), "\n\n")

# Pre-compute the "Other" exclusion set
other_exclusions <- c("Green Technology", "Battery Technology",
                      "Hard to Abate Sector Decarbonization", "AI")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Load an istrax FST file and join it onto regionmap rows
#'
#' @param toflow Character name of the toflow measure
#' @return data.frame ready for compute_avstrax_for_techs
load_istrax_regional <- function(toflow) {
  istrax_path <- file.path(istrax_dir, paste0(toflow, ".fst"))
  if (!file.exists(istrax_path)) return(NULL)

  istrax_data <- fst::read_fst(istrax_path)

  # Replace NAs with 0 in the value column
  if (toflow %in% names(istrax_data)) {
    istrax_data[[toflow]][is.na(istrax_data[[toflow]])] <- 0
  }

  # Join regionmap with istrax data
  # regionmap has: docdb_family_id, appln_id, ctry_code, region_code, region_name
  # istrax_data has: docdb_family_id, ctry_code, <toflow>
  data <- regionmap |>
    dplyr::left_join(istrax_data, by = c("docdb_family_id", "ctry_code"))

  data
}

#' Run aggregation function for "No firm" (all patents) and each firm
#'
#' @param data Joined regionmap+istrax data (full, all patents)
#' @param toflow Toflow name
#' @param aggregate_fn A function(data, toflow) that returns the aggregated df
#' @return data.frame with an added `firm` column
run_with_firms <- function(data, toflow, aggregate_fn) {
  results <- list()

  # "No firm" = all patents (unfiltered)
  res_all <- tryCatch(aggregate_fn(data, toflow), error = function(e) NULL)
  if (!is.null(res_all) && nrow(res_all) > 0) {
    res_all$firm <- "No firm"
    results[[length(results) + 1L]] <- res_all
  }

  # Per-firm: inner-join with firmmap to restrict to that firm's patents
  firm_names <- sort(unique(firmmap$firm))
  for (fname in firm_names) {
    firm_ids <- firmmap |>
      dplyr::filter(firm == fname) |>
      dplyr::select(docdb_family_id) |>
      dplyr::distinct()

    data_firm <- data |>
      dplyr::inner_join(firm_ids, by = "docdb_family_id")

    if (nrow(data_firm) == 0) next

    res_firm <- tryCatch(aggregate_fn(data_firm, toflow), error = function(e) NULL)
    if (!is.null(res_firm) && nrow(res_firm) > 0) {
      res_firm$firm <- fname
      results[[length(results) + 1L]] <- res_firm
    }
  }

  dplyr::bind_rows(results)
}

#' Resolve a technology selection name to a classes data.frame
#'
#' @param tech_filter The human-readable technology name or special keyword
#' @return data.frame with a `docdb_family_id` column (or empty for "All")
resolve_classes <- function(tech_filter) {
  if (tech_filter == "All") {
    return(data.frame())
  }
  if (tech_filter == "Other") {
    ids_to_exclude <- techmap |>
      dplyr::filter(technology %in% other_exclusions) |>
      dplyr::select(docdb_family_id) |>
      dplyr::distinct()
    return(
      techmap |>
        dplyr::filter(technology == "All") |>
        dplyr::anti_join(ids_to_exclude, by = "docdb_family_id") |>
        dplyr::select(docdb_family_id) |>
        dplyr::distinct()
    )
  }
  # Specific technology: filter techmap to that tech
  techmap |>
    dplyr::filter(technology == tech_filter) |>
    dplyr::select(docdb_family_id) |>
    dplyr::distinct()
}

#' Write batch results to a parquet file, appending to existing data if present
#'
#' @param batch_list List of data.frames to combine and write
#' @param output_path File path for the parquet file
flush_to_parquet <- function(batch_list, output_path) {
  if (length(batch_list) == 0L) return(invisible(NULL))

  combined <- dplyr::bind_rows(batch_list)

  if (file.exists(output_path)) {
    existing <- arrow::read_parquet(output_path)
    combined <- dplyr::bind_rows(existing, combined)
    rm(existing)
  }

  arrow::write_parquet(combined, output_path)
  gc(verbose = FALSE)
  invisible(NULL)
}

# ============================================================================
# AGGREGATED BY REGION (compute_avstrax_for_techs)
# ============================================================================
#
# compute_avstrax_for_techs() keeps region_code in distinct():
#   select(docdb_family_id, appln_id, istrax, ctry_code) |> distinct()
# But for regions, we need to use region_code instead of ctry_code
# The function will group by region_code automatically when it's present
#
# We iterate over: toflow x tech_selection x firm

cat("=== AGGREGATED BY REGION ===\n\n")

output_by_region <- "inst/extdata/aggregated_by_region.parquet"
dir.create(dirname(output_by_region), recursive = TRUE, showWarnings = FALSE)
if (file.exists(output_by_region)) file.remove(output_by_region)

cat("Processing", length(toflows), "toflows x",
    length(tech_selections), "tech selections...\n\n")

start_time <- Sys.time()
batch_results_region <- list()
total_rows_region <- 0
batch_counter_region <- 0

for (i in seq_along(toflows)) {
  toflow <- toflows[i]
  cat(sprintf("[%d/%d] toflow: %s\n", i, length(toflows), toflow))

  # Load full regional data once per toflow
  data_full <- load_istrax_regional(toflow)
  if (is.null(data_full) || nrow(data_full) == 0) next

  for (ts_name in names(tech_selections)) {
    tech_filter <- tech_selections[[ts_name]]
    classes <- resolve_classes(tech_filter)

    # Aggregation function for compute_avstrax_for_techs (by region)
    # The key difference: we need region_code to be treated like ctry_code
    # We'll rename region_code to ctry_code before passing to the function
    agg_by_region <- function(d, tf) {
      # Rename region_code to ctry_code so compute_avstrax_for_techs groups correctly
      d_prepared <- d |>
        dplyr::rename(ctry_code_orig = ctry_code) |>
        dplyr::rename(ctry_code = region_code)
      
      result <- compute_avstrax_for_techs(d_prepared, tf, classes)
      
      # Rename back: ctry_code -> region_code, and add region_name
      if (!is.null(result) && nrow(result) > 0) {
        result <- result |>
          dplyr::rename(region_code = ctry_code) |>
          dplyr::left_join(
            regionmap |> 
              dplyr::select(region_code, region_name) |> 
              dplyr::distinct(),
            by = "region_code"
          )
      }
      
      result
    }

    result <- run_with_firms(data_full, toflow, agg_by_region)

    if (!is.null(result) && nrow(result) > 0) {
      result$toflow <- toflow
      result$tech_selection <- ts_name
      batch_results_region[[length(batch_results_region) + 1L]] <- result
      total_rows_region <- total_rows_region + nrow(result)
    }
  }

  batch_counter_region <- batch_counter_region + 1

  # Write every 10 toflows
  if (batch_counter_region >= 10) {
    cat(sprintf("  Flushing batch (%d accumulated results)...\n", length(batch_results_region)))
    flush_to_parquet(batch_results_region, output_by_region)
    batch_results_region <- list()
    batch_counter_region <- 0
  }
}

# Final flush
if (length(batch_results_region) > 0) {
  cat(sprintf("  Final flush (%d accumulated results)...\n", length(batch_results_region)))
  flush_to_parquet(batch_results_region, output_by_region)
  batch_results_region <- list()
}

elapsed_region <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nRegional aggregation complete: %d total rows in %.1f minutes\n", 
            total_rows_region, elapsed_region))
cat("  Written to:", output_by_region, "\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("=== BUILD COMPLETE ===\n\n")

if (file.exists(output_by_region)) {
  info_region <- file.info(output_by_region)
  cat(sprintf("  %s: %.1f MB\n", output_by_region, info_region$size / 1e6))
}

cat(sprintf("\nTotal elapsed: %.1f minutes\n",
            difftime(Sys.time(), start_time, units = "mins")))
