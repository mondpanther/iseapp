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

source("R/functions_istraxfunctions_processing.R")

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

firmmap <- fst::read_fst("data-raw/big_files/firmmap.fst") |>
  dplyr::filter(firm == "Hitachi")
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
# DEFINE COLORINGS (must match old script exactly)
# ============================================================================

green_classes <- c("Green Technology", "Green Energy", "Green Transport",
                   "Circular Economy", "Green Manufacturing", "Adaptation",
                   "Green Housing", "Green ICT", "Green Agriculture", "GHG Capture")

battery_classes <- c("Battery Technology", "Lithium Extraction & Processing",
                     "Graphite & Carbon Materials", "Cathode Materials", "Anode Materials",
                     "Electrolytes & Additives", "Separators", "Battery Cell Design & Assembly",
                     "Battery Management Systems (BMS)", "Electric Vehicles & Mobility",
                     "Battery Recycling & Recovery")

hard_to_abate_classes <- c("Hard to Abate Sector Decarbonization", "Aviation Decarbonisation",
                           "Cement & Concrete Decarbonisation", "Chemicals & Plastics Decarbonisation",
                           "Shipping Decarbonisation", "Steel & Iron Decarbonisation")

ai_classes <- c("AI", "Machine Learning", "Deep Learning", "Natural Language Processing (NLP)",
                "Computer Vision", "Speech Recognition & Synthesis", "Robotics & Autonomous Systems",
                "Knowledge Representation & Reasoning", "Planning & Decision Making", "Generative AI",
                "Semiconductors", "Cloud & Data Infrastructure", "Data Rettrieval & Processing System",
                "Platform & Frameworks", "Deployment & Support")

cpc_sections <- c("Human Necessities", "Performing Operations; Transporting ",
                  "Chemistry; Metallurgy ", "Textiles; Paper", "Fixed Constructions",
                  "Mechanical Engineering; Lighting; Heating; Weapons; Blasting",
                  "Physics", "Electricity", "General tagging of new or cross-sectional technology")

agrifood_classes <- c("Any Agriculture & Food technology", "Input supply",
                      "Primary food and feed production", "Post-harvest handling & aggregation",
                      "Processing", "Distribution/wholesale", "Retail/consumption", "Crosscutting")

colorings <- list(
  green = green_classes,
  battery = battery_classes,
  hard_to_abate = hard_to_abate_classes,
  ai = ai_classes,
  cpcsecs = cpc_sections,
  agrifood = agrifood_classes
)

# ============================================================================
# DEFINE ALL TECHNOLOGY SELECTIONS
# ============================================================================

# Start with the 6 broad categories from the old script
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
# Skip "All" — already covered above as a broad category
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
  res_all <- tryCatch(aggregate_fn(data, toflow), error = function(e) {
    cat("  ERROR in aggregate_fn:", e$message, "\n")
    NULL
  })

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
    gc(verbose = FALSE)  # Force garbage collection to release file handles
  }

  # Remove existing file before writing to avoid file lock
  if (file.exists(output_path)) {
    Sys.sleep(0.1)  # Brief pause to ensure file handle is released
    unlink(output_path)
  }

  arrow::write_parquet(combined, output_path)
  rm(combined)
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
  # batch_results_region <- list()
}

elapsed_region <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nRegional aggregation complete: %d total rows in %.1f minutes\n", 
            total_rows_region, elapsed_region))
cat("  Written to:", output_by_region, "\n\n")

# ============================================================================
# AGGREGATED BY TECH (for Plot 1 - returns by technology)
# ============================================================================

cat("=== AGGREGATED BY TECH (ACROSS REGIONS) ===\n\n")

output_by_tech <- "inst/extdata/aggregated_by_tech_region.parquet"
if (file.exists(output_by_tech)) file.remove(output_by_tech)

cat("Processing", length(toflows), "toflows x",
    length(tech_selections), "tech selections...\n\n")

start_time_tech <- Sys.time()
batch_results_tech <- list()
total_rows_tech <- 0
batch_counter_tech <- 0

for (i in seq_along(toflows)) {
  toflow <- toflows[i]
  cat(sprintf("[%d/%d] toflow: %s\n", i, length(toflows), toflow))

  # Load full regional data once per toflow (joined with regionmap, has appln_id)
  data_full <- load_istrax_regional(toflow)
  if (is.null(data_full) || nrow(data_full) == 0) next

  # Deduplicate to one row per patent for tech aggregation
  # (regionmap creates multiple rows per patent - one per region)
  data_for_tech <- data_full |>
    dplyr::select(docdb_family_id, appln_id, dplyr::all_of(toflow)) |>
    dplyr::distinct(docdb_family_id, appln_id, .keep_all = TRUE)

  # compute_avstrax returns one row per technology in techmap
  agg_by_tech <- function(d, tf) {
    compute_avstrax(d, tf, techmap, colorings = colorings)
  }

  result <- run_with_firms(data_for_tech, toflow, agg_by_tech)

  if (!is.null(result) && nrow(result) > 0) {
    result$toflow <- toflow
    batch_results_tech[[length(batch_results_tech) + 1L]] <- result
    total_rows_tech <- total_rows_tech + nrow(result)
  }

  batch_counter_tech <- batch_counter_tech + 1

  # Write every 10 toflows
  if (batch_counter_tech >= 10) {
    cat(sprintf("  Flushing batch (%d accumulated results)...\n", length(batch_results_tech)))
    flush_to_parquet(batch_results_tech, output_by_tech)
    batch_results_tech <- list()
    batch_counter_tech <- 0
  }
}

# Final flush
if (length(batch_results_tech) > 0) {
  cat(sprintf("  Final flush (%d accumulated results)...\n", length(batch_results_tech)))
  flush_to_parquet(batch_results_tech, output_by_tech)
  batch_results_tech <- list()
}

elapsed_tech <- difftime(Sys.time(), start_time_tech, units = "mins")
cat(sprintf("\nTech aggregation complete: %d total rows in %.1f minutes\n", 
            total_rows_tech, elapsed_tech))
cat("  Written to:", output_by_tech, "\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("=== BUILD COMPLETE ===\n\n")

if (file.exists(output_by_tech)) {
  info_tech <- file.info(output_by_tech)
  cat(sprintf("  %s: %.1f MB\n", output_by_tech, info_tech$size / 1e6))
}

if (file.exists(output_by_region)) {
  info_region <- file.info(output_by_region)
  cat(sprintf("  %s: %.1f MB\n", output_by_region, info_region$size / 1e6))
}

cat(sprintf("\nTotal elapsed: %.1f minutes\n",
            difftime(Sys.time(), start_time, units = "mins")))
