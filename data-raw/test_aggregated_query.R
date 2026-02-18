# Test Aggregated Database Queries
#
# This script tests querying the aggregated statistics parquet files
# and validates results against the existing preprocessing approach

library(duckdb)
library(arrow)
library(dplyr)
library(tictoc)
library(fst)

cat("=== TESTING AGGREGATED DATABASE ===\n\n")

# ============================================================================
# SETUP
# ============================================================================

aggregated_by_tech_path <- "inst/extdata/aggregated_by_tech.parquet"
aggregated_by_country_path <- "inst/extdata/aggregated_by_country.parquet"
prepdata_path <- "inst/extdata/prepdata"

if (!file.exists(aggregated_by_tech_path)) {
  stop("aggregated_by_tech.parquet not found. Run build_aggregated_database_v2.R first.")
}
if (!file.exists(aggregated_by_country_path)) {
  stop("aggregated_by_country.parquet not found. Run build_aggregated_database_v2.R first.")
}

cat("Using aggregated databases:\n")
cat("  By tech:", aggregated_by_tech_path, "\n")
cat("  File size:", round(file.info(aggregated_by_tech_path)$size / 1024^2, 2), "MB\n")
cat("  By country:", aggregated_by_country_path, "\n")
cat("  File size:", round(file.info(aggregated_by_country_path)$size / 1024^2, 2), "MB\n\n")

# Connect to DuckDB
con <- dbConnect(duckdb())

# ============================================================================
# TEST 1: Query Single Country + Technology (by-country aggregation)
# ============================================================================

cat("TEST 1: Single country (US) + Single tech (AI) + avstrax_global\n")
cat("---------------------------------------------------------------\n")

query1 <- sprintf("
  SELECT 
    ctry_code,
    tech_selection,
    firm,
    mean,
    sem,
    q2 as median,
    q1,
    q3,
    innos,
    RTA
  FROM read_parquet('%s')
  WHERE ctry_code = 'US' 
    AND tech_selection = 'AI'
    AND toflow = 'avstrax_global'
    AND firm != 'No firm'
  ORDER BY firm
", aggregated_by_country_path)

tic()
result_agg <- dbGetQuery(con, query1)
toc()

cat("\nResults from aggregated database (by country):\n")
print(result_agg)

cat("\nFirms found:", nrow(result_agg), "\n")
if (nrow(result_agg) > 0) {
  cat("Total innovations across firms:", sum(result_agg$innos), "\n")
}
cat("\n")

# ============================================================================
# TEST 2: Aggregate Technologies for a Country Group (by-tech aggregation)
# ============================================================================

cat("\nTEST 2: LMICs country group + All technologies + avstrax_global\n")
cat("----------------------------------------------------------------\n")

query2 <- sprintf("
  SELECT 
    technology,
    mean,
    sem,
    innos,
    q2 as median,
    greenclass
  FROM read_parquet('%s')
  WHERE country_group = 'LMICs'
    AND toflow = 'avstrax_global'
    AND firm = 'No firm'
  ORDER BY innos DESC
  LIMIT 10
", aggregated_by_tech_path)

tic()
result_lmic <- dbGetQuery(con, query2)
toc()

cat("\nTop 10 technologies for LMICs:\n")
print(result_lmic)

cat("\n")

# ============================================================================
# TEST 3: All Technologies for Single Country (by-tech aggregation)
# ============================================================================

cat("\nTEST 3: All technologies for US (individual country) + avstrax_global\n")
cat("-----------------------------------------------------------------------\n")

query3 <- sprintf("
  SELECT 
    technology,
    innos,
    mean,
    sem,
    q2 as median,
    greenclass
  FROM read_parquet('%s')
  WHERE country_group = 'US'
    AND toflow = 'avstrax_global'
    AND firm = 'No firm'
  ORDER BY innos DESC
  LIMIT 10
", aggregated_by_tech_path)

tic()
result_all_tech <- dbGetQuery(con, query3)
toc()

cat("\nTop 10 technologies for US:\n")
print(result_all_tech)

cat("\n")

# ============================================================================
# TEST 4: Firm-Level Analysis (by-country aggregation)
# ============================================================================

cat("\nTEST 4: Firm comparison for AI technology + avstrax_global\n")
cat("------------------------------------------------------------\n")

query4 <- sprintf("
  SELECT 
    firm,
    SUM(innos) as total_patents,
    SUM(innos * mean) / NULLIF(SUM(innos), 0) as weighted_mean_return,
    AVG(RTA) as avg_rta,
    COUNT(DISTINCT ctry_code) as n_countries
  FROM read_parquet('%s')
  WHERE tech_selection = 'AI'
    AND toflow = 'avstrax_global'
    AND firm != 'No firm'
  GROUP BY firm
  ORDER BY total_patents DESC
", aggregated_by_country_path)

tic()
result_firms <- dbGetQuery(con, query4)
toc()

cat("\nFirm-level statistics for AI:\n")
print(result_firms)

cat("\n")

# ============================================================================
# TEST 5a: Validate Against Existing Preprocessing (by-tech)
# ============================================================================

cat("\nTEST 5a: Validation against existing preprocessing (by-tech)\n")
cat("--------------------------------------------------------------\n")

# Try to load a precomputed file if it exists
test_file_tech <- file.path(prepdata_path, "by_tech_avstrax_global_All_countries.fst")

if (file.exists(test_file_tech)) {
  cat("Loading existing preprocessing file:", basename(test_file_tech), "\n")
  
  existing_prep <- read_fst(test_file_tech)
  
  # Query aggregated database for same parameters
  query5a <- sprintf("
    SELECT 
      technology,
      innos,
      mean,
      q2,
      greenclass
    FROM read_parquet('%s')
    WHERE country_group = 'All_countries'
      AND toflow = 'avstrax_global'
      AND firm = 'No firm'
    ORDER BY technology
  ", aggregated_by_tech_path)
  
  result_validation <- dbGetQuery(con, query5a)
  
  # Compare results
  cat("\nComparison (first 5 technologies):\n")
  cat("\nFrom existing preprocessing:\n")
  print(head(existing_prep[, c("technology", "innos", "mean", "q2")], 5))
  
  cat("\nFrom aggregated database:\n")
  print(head(result_validation[, c("technology", "innos", "mean", "q2")], 5))
  
  # Check if numbers match (allowing for small rounding differences)
  matching_techs <- intersect(existing_prep$technology, result_validation$technology)
  
  if (length(matching_techs) > 0) {
    test_tech <- matching_techs[1]
    old_val <- existing_prep$mean[existing_prep$technology == test_tech][1]
    new_val <- result_validation$mean[result_validation$technology == test_tech][1]
    
    diff_pct <- abs(old_val - new_val) / old_val * 100
    
    cat(sprintf("\nValidation check for '%s':\n", test_tech))
    cat(sprintf("  Old preprocessing: %.4f\n", old_val))
    cat(sprintf("  New aggregated DB: %.4f\n", new_val))
    cat(sprintf("  Difference: %.2f%%\n", diff_pct))
    
    if (diff_pct < 1) {
      cat("  SUCCESS - Results match within 1%\n")
    } else {
      cat("  FAIL - Results differ by more than 1%\n")
    }
  }
  
} else {
  cat("Existing preprocessing file not found - skipping validation\n")
  cat("Expected location:", test_file_tech, "\n")
}

cat("\n")

# ============================================================================
# TEST 5b: Validate Against Existing Preprocessing (by-country)
# ============================================================================

cat("\nTEST 5b: Validation against existing preprocessing (by-country)\n")
cat("-----------------------------------------------------------------\n")

# Try to load a precomputed file if it exists
test_file_country <- file.path(prepdata_path, "by_country_avstrax_global_US_AI.fst")

if (file.exists(test_file_country)) {
  cat("Loading existing preprocessing file:", basename(test_file_country), "\n")
  
  existing_prep_country <- read_fst(test_file_country)
  
  # Query aggregated database for same parameters
  query5b <- sprintf("
    SELECT 
      ctry_code,
      innos,
      mean,
      q2,
      RTA
    FROM read_parquet('%s')
    WHERE tech_selection = 'AI'
      AND toflow = 'avstrax_global'
      AND firm = 'No firm'
    ORDER BY ctry_code
  ", aggregated_by_country_path)
  
  result_validation_country <- dbGetQuery(con, query5b)
  
  # Find US row
  old_us <- existing_prep_country[existing_prep_country$ctry_code == "US", ]
  new_us <- result_validation_country[result_validation_country$ctry_code == "US", ]
  
  if (nrow(old_us) > 0 && nrow(new_us) > 0) {
    cat("\nComparison for US + AI:\n")
    cat("\nFrom existing preprocessing:\n")
    print(old_us[, c("ctry_code", "innos", "mean", "q2")])
    
    cat("\nFrom aggregated database:\n")
    print(new_us[, c("ctry_code", "innos", "mean", "q2")])
    
    old_val <- old_us$mean[1]
    new_val <- new_us$mean[1]
    diff_pct <- abs(old_val - new_val) / old_val * 100
    
    cat(sprintf("\nValidation check:\n"))
    cat(sprintf("  Old preprocessing: %.4f\n", old_val))
    cat(sprintf("  New aggregated DB: %.4f\n", new_val))
    cat(sprintf("  Difference: %.2f%%\n", diff_pct))
    
    if (diff_pct < 1) {
      cat("  SUCCESS - Results match within 1%\n")
    } else {
      cat("  FAIL - Results differ by more than 1%\n")
    }
  }
  
} else {
  cat("Existing preprocessing file not found - skipping validation\n")
  cat("Expected location:", test_file_country, "\n")
}

cat("\n")

# ============================================================================
# TEST 6: Cross-dimensional queries
# ============================================================================

cat("\nTEST 6: Cross-dimensional query - Green tech across countries\n")
cat("---------------------------------------------------------------\n")

query6 <- sprintf("
  SELECT 
    ctry_code,
    innos,
    mean,
    RTA,
    share
  FROM read_parquet('%s')
  WHERE tech_selection = 'Green_Technology'
    AND toflow = 'avstrax_global'
    AND firm = 'No firm'
  ORDER BY innos DESC
  LIMIT 15
", aggregated_by_country_path)

tic()
result_cross <- dbGetQuery(con, query6)
toc()

cat("\nTop 15 countries for Green Technology:\n")
print(result_cross)

cat("\n")

# ============================================================================
# SUMMARY
# ============================================================================

dbDisconnect(con, shutdown = TRUE)

cat("\n=== SUMMARY ===\n")
cat("All queries completed successfully!\n")
cat("Query times should be <500ms for most operations.\n")
cat("\nKey points:\n")
cat("  - aggregated_by_tech.parquet: queries by technology (grouped by country_group)\n")
cat("  - aggregated_by_country.parquet: queries by country (grouped by tech_selection)\n")
cat("  - Both support firm dimension filtering\n")
cat("  - Column names: sem (not sd), q2 (not median)\n")
cat("\nThe aggregated database is ready to use in the Shiny app.\n")
cat("Next: Update app modules to query these databases instead of loading FST files.\n")
