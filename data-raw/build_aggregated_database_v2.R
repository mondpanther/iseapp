library(rlang)
library(future)
library(future.apply)
library(progressr)

cat("=== BUILDING AGGREGATED PATENT DATABASE v2 ===\n\n")

# Set up parallel backend
plan(multisession, workers = max(1L, availableCores() - 1L))
cat(sprintf("Parallel workers: %d\n\n", nbrOfWorkers()))
handlers(global = TRUE)
handlers("txtprogressbar") Produces TWO parquet files:
#   - inst/extdata/aggregated_by_tech.parquet
#     (by-technology aggregation via compute_avstrax, one row per technology)
#   - inst/extdata/aggregated_by_country.parquet
#     (by-country aggregation via compute_avstrax_for_techs, one row per country)
#
# Calls existing compute_avstrax() and compute_avstrax_for_techs() directly.
# Expands beyond old script: ALL individual countries + ALL individual techs + firm dimension.
# Writes to parquet after each toflow with gc() cleanup to prevent Windows file locking.

library(fst)
library(dplyr)
library(tidyr)
library(arrow)
library(data.table)
library(collapse)
library(rlang)

cat("=== BUILDING AGGREGATED PATENT DATABASE v2 ===\n\n")

# ============================================================================
# SOURCE EXISTING FUNCTIONS
# ============================================================================

source("R/functions_istraxfunctions_processing.R")

# ============================================================================
# LOAD BASE DATA
# ============================================================================

cat("Loading base data...\n")

countrymap <- fst::read_fst("data-raw/big_files/countrymap.fst")
cat("  countrymap:", nrow(countrymap), "rows\n")

techmap <- fst::read_fst("data-raw/big_files/techmap.fst")
cat("  techmap:", nrow(techmap), "rows\n")

firmmap <- fst::read_fst("data-raw/big_files/firmmap.fst") |>
  dplyr::filter(firm == "Hitachi")
cat("  firmmap:", nrow(firmmap), "rows\n\n")

# ============================================================================
# PREPARE TECHMAP (same logic as data-raw/preprocess_techmap.R)
# ============================================================================

cat("Preparing techmap...\n")

# Add "All" category from distinct patents in countrymap
techmap <- countrymap |>
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
# DEFINE COUNTRY GROUPS
# ============================================================================

all_countries <- sort(unique(na.omit(countrycode::codelist$iso2c)))

lmics <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG",
            "BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI",
            "CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE",
            "GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ",
            "KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML",
            "MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG",
            "MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC",
            "SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO",
            "TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")

eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
                  "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
                  "SI","ES","SE")

hic <- setdiff(all_countries, lmics)

country_groups <- list(
  "All_countries" = all_countries,
  "LMICs" = lmics,
  "LMICs_excl_China" = setdiff(lmics, "CN"),
  "EU_countries" = eu_countries,
  "High_income_countries" = hic
)

# Individual countries present in the data
individual_countries <- sort(unique(countrymap$ctry_code))

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
# DEFINE ALL TECHNOLOGY SELECTIONS (for Part 2: by-country aggregation)
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
# HELPER: Load istrax and join with countrymap
# ============================================================================

#' Load an istrax FST file and join it onto countrymap rows
#'
#' @param toflow Character name of the toflow measure
#' @param country_codes Character vector of ISO-2 codes to keep (NULL = all)
#' @return data.frame ready for compute_avstrax / compute_avstrax_for_techs
load_istrax_data <- function(toflow, country_codes = NULL) {
  istrax_path <- file.path(istrax_dir, paste0(toflow, ".fst"))
  if (!file.exists(istrax_path)) return(NULL)

  istrax_data <- fst::read_fst(istrax_path)

  # Replace NAs with 0 in the value column (matches old preprocessing)
  if (toflow %in% names(istrax_data)) {
    istrax_data[[toflow]][is.na(istrax_data[[toflow]])] <- 0
  }

  if (!is.null(country_codes)) {
    data <- countrymap |>
      dplyr::filter(ctry_code %in% country_codes) |>
      dplyr::left_join(istrax_data, by = c("docdb_family_id", "ctry_code"))
  } else {
    data <- countrymap |>
      dplyr::left_join(istrax_data, by = c("docdb_family_id", "ctry_code"))
  }

  data
}

# ============================================================================
# HELPER: Add firm dimension to a result from compute_avstrax*
# ============================================================================

#' Run an aggregation function for "No firm" (all patents) and each firm
#'
#' For the firm dimension we pre-filter the joined data to patents belonging
#' to a given firm, then call the same aggregation function. "No firm" means
#' all patents (no firm filter), matching old behaviour.
#'
#' @param data Joined countrymap+istrax data (full, all patents)
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

# ============================================================================
# HELPER: Flush accumulated batch results to parquet (append-style)
# ============================================================================

#' Write batch results to a parquet file, appending to existing data if present.
#'
#' @param batch_list List of data.frames to combine and write
#' @param output_path File path for the parquet file
#' @return Invisible NULL; side-effect is writing to disk
flush_to_parquet <- function(batch_list, output_path) {
  if (length(batch_list) == 0L) return(invisible(NULL))

  combined <- dplyr::bind_rows(batch_list)

  if (file.exists(output_path)) {
    existing <- arrow::read_parquet(output_path)
    combined <- dplyr::bind_rows(existing, combined)
    rm(existing)
    gc(verbose = FALSE)
  }

  arrow::write_parquet(combined, output_path)
  rm(combined)
  gc(verbose = FALSE)
  invisible(NULL)
}

# ============================================================================
# PART 1: AGGREGATED BY TECH (compute_avstrax)
# ============================================================================
#
# compute_avstrax() drops ctry_code before distinct():
#   select(docdb_family_id, appln_id, istrax) |> distinct()
# This means one row per patent family per technology, regardless of country.
# The output is grouped by technology with columns:
#   technology, mean, innos, sem, q1, q2, q3, top25_bin_mean, top50_bin_mean,
#   top3_ids, top25, top50, top3_ids_url, greenclass
#
# We iterate over: toflow x country_selection x firm

cat("=== PART 1: Aggregated by Technology ===\n\n")

output_by_tech <- "inst/extdata/aggregated_by_tech.parquet"
dir.create(dirname(output_by_tech), recursive = TRUE, showWarnings = FALSE)
if (file.exists(output_by_tech)) file.remove(output_by_tech)

# Combine country groups + individual countries
all_country_selections <- country_groups
for (cc in individual_countries) {
  all_country_selections[[cc]] <- cc
}

cat("Country selections:", length(all_country_selections), "\n")
cat("Processing", length(toflows), "toflows x",
    length(all_country_selections), "country selections...\n")

n_firms <- length(unique(firmmap$firm)) + 1L  # +1 for "No firm"
total_iterations_tech <- length(toflows) * length(all_country_selections) * n_firms
cat(sprintf("Total iterations: %d toflows x %d country selections x %d firms = %d\n\n",
            length(toflows), length(all_country_selections), n_firms, total_iterations_tech))

start_time <- Sys.time()
batch_results_tech <- list()
total_rows_tech <- 0

for (i in seq_along(toflows)) {
  toflow <- toflows[i]
  toflow_start <- Sys.time()
  cat(sprintf("[%d/%d] Processing toflow: %s\n", i, length(toflows), toflow))

  for (j in seq_along(all_country_selections)) {
    cg_name <- names(all_country_selections)[j]
    country_codes <- all_country_selections[[j]]
    
    cat(sprintf("  • Country selection [%d/%d]: %s (%d countries)\n",
                j, length(all_country_selections), cg_name, length(country_codes)))

    data <- load_istrax_data(toflow, country_codes)
    if (is.null(data) || nrow(data) == 0) {
      cat("    ⚠ No data found, skipping\n")
      next
    }
    
    cat(sprintf("    Loading istrax data: %d rows\n", nrow(data)))

    # Aggregation function for compute_avstrax (by technology)
    agg_by_tech <- function(d, tf) {
      compute_avstrax(d, tf, techmap, colorings = colorings)
    }
    
    cat("    Computing aggregation with firm dimension...\n")
    result <- run_with_firms(data, toflow, agg_by_tech)

    if (!is.null(result) && nrow(result) > 0) {
      result$toflow <- toflow
      result$country_group <- cg_name
      batch_results_tech[[length(batch_results_tech) + 1L]] <- result
      total_rows_tech <- total_rows_tech + nrow(result)
      cat(sprintf("    ✓ Added %d rows (total: %d)\n", nrow(result), total_rows_tech))
    }
  }

  # Toflow timing and ETA
  toflow_elapsed <- difftime(Sys.time(), toflow_start, units = "mins")
  avg_time_per_toflow <- as.numeric(difftime(Sys.time(), start_time, units = "mins")) / i
  remaining_toflows <- length(toflows) - i
  estimated_remaining <- avg_time_per_toflow * remaining_toflows
  cat(sprintf("  Toflow completed in %.1f minutes\n", as.numeric(toflow_elapsed)))
  cat(sprintf("  Overall: %d/%d toflows (%.1f%%) | Est. remaining: %.1f minutes\n",
              i, length(toflows), (i / length(toflows)) * 100, estimated_remaining))

  # Flush to disk after each toflow
  if (length(batch_results_tech) > 0) {
    cat(sprintf("  >>> Flushing to disk (%d results, %d total rows)...\n",
                length(batch_results_tech), total_rows_tech))
    flush_to_parquet(batch_results_tech, output_by_tech)
    cat("  ✓ Written successfully\n")
    batch_results_tech <- list()
    gc(verbose = FALSE)
  }
  
  cat("\n")
}

elapsed_tech <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nPart 1 complete: %d total rows in %.1f minutes\n", total_rows_tech, elapsed_tech))
cat("  Written to:", output_by_tech, "\n\n")

# ============================================================================
# PART 2: AGGREGATED BY COUNTRY (compute_avstrax_for_techs)
# ============================================================================
#
# compute_avstrax_for_techs() keeps ctry_code in distinct():
#   select(docdb_family_id, appln_id, istrax, ctry_code) |> distinct()
# This means one row per patent family per country.
# The output is grouped by ctry_code with columns:
#   ctry_code, mean, innos, sem, q1, q2, q3, top25_bin_mean, top50_bin_mean,
#   top3_ids, top25, top50, top3_ids_url
#
# We iterate over: toflow x tech_selection x firm
#
# The tech_selection determines which classes are passed:
#   "All"   -> classes = data.frame() (empty = all innovations)
#   "Other" -> classes = techmap filtered to EXCLUDE green/battery/hard_to_abate/ai
#   specific tech -> classes = techmap filtered to that technology

cat("=== PART 2: Aggregated by Country ===\n\n")

output_by_country <- "inst/extdata/aggregated_by_country.parquet"
if (file.exists(output_by_country)) file.remove(output_by_country)

cat("Technology selections:", length(tech_selections), "\n")
cat("Processing", length(toflows), "toflows x",
    length(tech_selections), "tech selections...\n")

total_iterations_country <- length(toflows) * length(tech_selections) * n_firms
cat(sprintf("Total iterations: %d toflows x %d tech selections x %d firms = %d\n\n",
            length(toflows), length(tech_selections), n_firms, total_iterations_country))

# Pre-compute the "Other" exclusion set (matches old precompute_avstrax.Rmd)
other_exclusions <- c("Green Technology", "Battery Technology",
                      "Hard to Abate Sector Decarbonization", "AI")

#' Resolve a technology selection name to a classes data.frame
#' suitable for compute_avstrax_for_techs()
#'
#' @param tech_filter The human-readable technology name or special keyword
#' @return data.frame with a `docdb_family_id` column (or empty for "All")
resolve_classes <- function(tech_filter) {
  if (tech_filter == "All") {
    # Empty data.frame signals "all innovations" to compute_avstrax_for_techs
    return(data.frame())
  }
  if (tech_filter == "Other") {
    # Exclude green, battery, hard-to-abate, AI
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

start_time2 <- Sys.time()
batch_results_country <- list()
total_rows_country <- 0

for (i in seq_along(toflows)) {
  toflow <- toflows[i]
  toflow_start2 <- Sys.time()
  cat(sprintf("[%d/%d] Processing toflow: %s\n", i, length(toflows), toflow))

  # Load full data once per toflow (all countries)
  cat("  Loading full istrax data (all countries)...\n")
  data_full <- load_istrax_data(toflow, country_codes = NULL)
  if (is.null(data_full) || nrow(data_full) == 0) {
    cat("  ⚠ No data found, skipping\n\n")
    next
  }
  cat(sprintf("  ✓ Loaded %d rows\n", nrow(data_full)))

  for (j in seq_along(names(tech_selections))) {
    ts_name <- names(tech_selections)[j]
    tech_filter <- tech_selections[[ts_name]]
    cat(sprintf("  • Tech selection [%d/%d]: %s\n",
                j, length(tech_selections), ts_name))
    
    classes <- resolve_classes(tech_filter)
    cat(sprintf("    Resolved to %d patent families\n", nrow(classes)))

    # Aggregation function for compute_avstrax_for_techs (by country)
    agg_by_country <- function(d, tf) {
      compute_avstrax_for_techs(d, tf, classes)
    }
    
    cat("    Computing aggregation with firm dimension...\n")
    result <- run_with_firms(data_full, toflow, agg_by_country)

    if (!is.null(result) && nrow(result) > 0) {
      result$toflow <- toflow
      result$tech_selection <- ts_name
      batch_results_country[[length(batch_results_country) + 1L]] <- result
      total_rows_country <- total_rows_country + nrow(result)
      cat(sprintf("    ✓ Added %d rows (total: %d)\n", nrow(result), total_rows_country))
    }
  }

  # Toflow timing and ETA
  toflow_elapsed2 <- difftime(Sys.time(), toflow_start2, units = "mins")
  avg_time_per_toflow2 <- as.numeric(difftime(Sys.time(), start_time2, units = "mins")) / i
  remaining_toflows2 <- length(toflows) - i
  estimated_remaining2 <- avg_time_per_toflow2 * remaining_toflows2
  cat(sprintf("  Toflow completed in %.1f minutes\n", as.numeric(toflow_elapsed2)))
  cat(sprintf("  Overall: %d/%d toflows (%.1f%%) | Est. remaining: %.1f minutes\n",
              i, length(toflows), (i / length(toflows)) * 100, estimated_remaining2))

  # Flush to disk after each toflow
  if (length(batch_results_country) > 0) {
    cat(sprintf("  >>> Flushing to disk (%d results, %d total rows)...\n",
                length(batch_results_country), total_rows_country))
    flush_to_parquet(batch_results_country, output_by_country)
    cat("  ✓ Written successfully\n")
    batch_results_country <- list()
    gc(verbose = FALSE)
  }
  
  cat("\n")
}

elapsed_country <- difftime(Sys.time(), start_time2, units = "mins")
cat(sprintf("\nPart 2 complete: %d total rows in %.1f minutes\n", total_rows_country, elapsed_country))
cat("  Written to:", output_by_country, "\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("=== BUILD COMPLETE ===\n\n")

if (file.exists(output_by_tech)) {
  info_tech <- file.info(output_by_tech)
  cat(sprintf("  %s: %.1f MB\n", output_by_tech, info_tech$size / 1e6))
}
if (file.exists(output_by_country)) {
  info_country <- file.info(output_by_country)
  cat(sprintf("  %s: %.1f MB\n", output_by_country, info_country$size / 1e6))
}

total_elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nTotal elapsed: %.1f minutes\n", total_elapsed))
