# Build Aggregated Patent Database
# 
# This script pre-computes aggregated statistics for all combinations of:
# - Countries (147 individual countries)
# - Technologies (65 individual technologies)  
# - Firms (10 firms + "No firm" category)
# - Toflows (42 return flow measures)
#
# Output: Single parquet file (~500MB-2GB) with ~4M rows of aggregated stats
# Can be queried with DuckDB for instant results in the Shiny app

library(fst)
library(dplyr)
library(tidyr)
library(arrow)
library(collapse)

cat("=== BUILDING AGGREGATED PATENT DATABASE ===\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Build Espacenet search URL from application IDs
#' 
#' @param appln_ids Character string of comma-separated application IDs
#' @return Character string with JavaScript window.open() call for Espacenet
build_espacenet_search <- function(appln_ids) {
  if (is.na(appln_ids) || appln_ids == "") return("")
  
  # Split, trim, and create search terms
  ids <- strsplit(appln_ids, ",")[[1]]
  ids <- trimws(ids)
  search_terms <- paste0("ap%3D", ids, collapse = "%20OR%20")
  url <- paste0('window.open("https://worldwide.espacenet.com/patent/search?q=', 
                search_terms, '")')
  return(url)
}

#' Compute aggregated statistics for a group
#'
#' @param data data.frame with columns: docdb_family_id, appln_id, ctry_code, 
#'   technology, firm, and the toflow measure column
#' @param toflow_col Character name of the column containing the return measure
#' @param scaler Numeric value to multiply the measure by (100 for avstrax/istrax, 1 for ev)
#' @return data.frame with one row containing aggregated statistics
compute_aggregated_stats <- function(data, toflow_col, scaler = 1) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Create scaled column
  data$value_scaled <- data[[toflow_col]] * scaler
  
  # Remove NAs for aggregation
  data_clean <- data[!is.na(data$value_scaled), ]
  
  if (nrow(data_clean) == 0) return(NULL)
  
  # Compute basic statistics using collapse for speed
  stats <- data.frame(
    mean = collapse::fmean(data_clean$value_scaled, na.rm = TRUE),
    sd = collapse::fsd(data_clean$value_scaled, na.rm = TRUE),
    median = collapse::fmedian(data_clean$value_scaled, na.rm = TRUE),
    q1 = collapse::fquantile(data_clean$value_scaled, 0.25, na.rm = TRUE),
    q3 = collapse::fquantile(data_clean$value_scaled, 0.75, na.rm = TRUE),
    innos = nrow(data_clean),
    stringsAsFactors = FALSE
  )
  
  # Calculate sem
  stats$sem <- stats$sd / sqrt(stats$innos)
  
  # Compute top percentile bin means
  data_sorted <- data_clean[order(-data_clean$value_scaled), ]
  n_total <- nrow(data_sorted)
  n_top25 <- ceiling(n_total * 0.25)
  n_top50 <- ceiling(n_total * 0.50)
  
  stats$top25_bin_mean <- mean(data_sorted$value_scaled[1:n_top25], na.rm = TRUE)
  stats$top50_bin_mean <- mean(data_sorted$value_scaled[1:n_top50], na.rm = TRUE)
  
  # Get top 10 application IDs
  top_ids <- head(data_sorted$appln_id, 10)
  stats$top3_ids <- paste(top_ids, collapse = ", ")
  
  # Create Espacenet URL
  stats$top3_ids_url <- build_espacenet_search(stats$top3_ids)
  
  # Add compatibility columns
  stats$top25 <- 0.25
  stats$top50 <- 0.50
  
  return(stats)
}

#' Process one toflow and compute all combinations
#'
#' @param toflow Character name of the toflow measure (e.g., "avstrax_global")
#' @param countrymap data.frame with docdb_family_id, ctry_code, appln_id
#' @param techmap data.frame with docdb_family_id, technology
#' @param firmmap data.frame with docdb_family_id, firm
#' @param istrax_dir Character path to directory containing istrax FST files
#' @return data.frame with aggregated statistics for all combinations
process_toflow <- function(toflow, countrymap, techmap, firmmap, istrax_dir) {
  
  cat("  Processing toflow:", toflow, "\n")
  
  # Load istrax file
  istrax_path <- file.path(istrax_dir, paste0(toflow, ".fst"))
  
  if (!file.exists(istrax_path)) {
    cat("    WARNING: File not found, skipping\n")
    return(NULL)
  }
  
  istrax <- fst::read_fst(istrax_path)
  toflow_col <- names(istrax)[3]  # Third column is the measure value
  
  # Replace NAs with 0 (matching existing preprocessing logic)
  istrax[[toflow_col]][is.na(istrax[[toflow_col]])] <- 0
  
  # Determine scaler (100 for avstrax/istrax, 1 for ev)
  scaler <- ifelse(grepl("strax", toflow), 100, 1)
  
  cat("    Joining data...\n")
  
  # Join all data
  # Match existing logic: countrymap defines (patent × country) level
  # Then expand to technologies and firms
  full_data <- countrymap |>
    dplyr::inner_join(istrax, by = c("docdb_family_id", "ctry_code")) |>
    dplyr::left_join(techmap, by = "docdb_family_id", relationship = "many-to-many") |>
    dplyr::left_join(firmmap, by = "docdb_family_id", relationship = "many-to-many") |>
    dplyr::distinct()
  
  # Add "No firm" category for patents without firm assignment
  full_data$firm[is.na(full_data$firm)] <- "No firm"
  
  cat("    Aggregating by combinations...\n")
  
  # Group by all dimensions and aggregate
  results <- full_data |>
    dplyr::group_by(ctry_code, technology, firm) |>
    dplyr::summarise(
      aggregated = list(compute_aggregated_stats(
        dplyr::pick(dplyr::everything()), 
        toflow_col, 
        scaler
      )),
      .groups = "drop"
    )
  
  # Unnest the aggregated statistics
  results_expanded <- results |>
    dplyr::filter(!sapply(aggregated, is.null)) |>
    tidyr::unnest(aggregated)
  
  # Add toflow identifier
  results_expanded$toflow <- toflow
  
  cat("    Computed", nrow(results_expanded), "combinations\n")
  
  return(results_expanded)
}

# ============================================================================
# LOAD BASE DATA
# ============================================================================

cat("Loading base data...\n")

countrymap <- fst::read_fst("data-raw/big_files/countrymap.fst")
cat("  ✓ countrymap:", nrow(countrymap), "rows\n")

techmap <- fst::read_fst("data-raw/big_files/techmap.fst")
cat("  ✓ techmap:", nrow(techmap), "rows\n")

firmmap <- fst::read_fst("data-raw/big_files/firmmap.fst")
cat("  ✓ firmmap:", nrow(firmmap), "rows\n\n")

# ============================================================================
# FIND ALL TOFLOW FILES
# ============================================================================

cat("Finding toflow measures...\n")

istrax_dir <- "data-raw/big_files/istraxes"
istrax_files <- list.files(istrax_dir, pattern = "\\.fst$", full.names = FALSE)

# Filter out _joined files
istrax_files <- istrax_files[!grepl("_joined", istrax_files)]
toflows <- tools::file_path_sans_ext(istrax_files)

cat("  Found", length(toflows), "toflow measures\n")
cat("  Examples:", paste(head(toflows, 5), collapse = ", "), "...\n\n")

# ============================================================================
# ESTIMATE OUTPUT SIZE
# ============================================================================

n_countries <- dplyr::n_distinct(countrymap$ctry_code)
n_techs <- dplyr::n_distinct(techmap$technology)
n_firms <- dplyr::n_distinct(firmmap$firm) + 1  # +1 for "No firm"
n_toflows <- length(toflows)

expected_max_rows <- n_countries * n_techs * n_firms * n_toflows

cat("Expected dimensions:\n")
cat("  Countries:", n_countries, "\n")
cat("  Technologies:", n_techs, "\n")
cat("  Firms:", n_firms, "(including 'No firm')\n")
cat("  Toflows:", n_toflows, "\n")
cat("  Maximum possible combinations:", format(expected_max_rows, big.mark = ","), "\n")
cat("  (Actual will be less due to sparse combinations)\n\n")

# ============================================================================
# PROCESS ALL TOFLOWS
# ============================================================================

cat("Processing toflows (writing to parquet after each)...\n\n")

output_file <- "inst/extdata/aggregated_stats.parquet"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# Remove existing file if present
if (file.exists(output_file)) {
  file.remove(output_file)
  cat("Removed existing output file\n\n")
}

start_time <- Sys.time()
total_rows <- 0
all_results <- list()

for (i in seq_along(toflows)) {
  toflow <- toflows[i]
  
  cat(sprintf("[%d/%d] Processing %s\n", i, length(toflows), toflow))
  
  # Process this toflow
  batch_results <- process_toflow(
    toflow = toflow,
    countrymap = countrymap,
    techmap = techmap,
    firmmap = firmmap,
    istrax_dir = istrax_dir
  )
  
  if (!is.null(batch_results) && nrow(batch_results) > 0) {
    all_results[[length(all_results) + 1]] <- batch_results
    total_rows <- total_rows + nrow(batch_results)
    
    # Write every 10 toflows to avoid memory issues
    if (i %% 10 == 0 || i == length(toflows)) {
      combined <- dplyr::bind_rows(all_results)
      if (file.exists(output_file)) {
        existing <- arrow::read_parquet(output_file)
        combined <- dplyr::bind_rows(existing, combined)
        rm(existing)
        gc()
      }
      arrow::write_parquet(combined, output_file)
      all_results <- list()
      if (!file.exists(output_file)) gc()
    }
    cat("  ✓ Collected (total rows:", format(total_rows, big.mark = ","), ")\n\n")
  }
}

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== BUILD COMPLETE ===\n\n")

if (file.exists(output_file)) {
  file_size_gb <- file.info(output_file)$size / 1024^3
  
  cat("Output file:", output_file, "\n")
  cat("Total rows:", format(total_rows, big.mark = ","), "\n")
  cat("File size:", round(file_size_gb, 2), "GB\n")
  cat("Time taken:", round(duration, 2), "minutes\n\n")
  
  # Show sample
  cat("Sample of aggregated data:\n")
  sample_data <- arrow::read_parquet(output_file) |>
    dplyr::slice_head(n = 5)
  print(sample_data)
  
  cat("\n")
  cat("Unique values:\n")
  full_data <- arrow::read_parquet(output_file)
  cat("  Countries:", dplyr::n_distinct(full_data$ctry_code), "\n")
  cat("  Technologies:", dplyr::n_distinct(full_data$technology), "\n")
  cat("  Firms:", dplyr::n_distinct(full_data$firm), "\n")
  cat("  Toflows:", dplyr::n_distinct(full_data$toflow), "\n")
  
} else {
  cat("ERROR: Output file was not created\n")
}

cat("\n=== NEXT STEPS ===\n")
cat("1. Test querying with DuckDB (see data-raw/test_aggregated_query.R)\n")
cat("2. Validate against existing preprocessing\n")
cat("3. Update app to use this database\n")
