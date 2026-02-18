# test_processing_functions.R
#
# Validates that the optimized functions in functions_istraxfunctions_processing.R
# produce IDENTICAL results to the originals in functions_istraxfunctions.R.
# Also compares execution time.
#
# Usage: source this file from the project root.
# Picks the smallest istrax file for a fast test.

library(fst)
library(dplyr)
library(data.table)
library(collapse)
library(rlang)

cat("=== VALIDATION TEST: Old vs New Processing Functions ===\n\n")

# ============================================================================
# LOAD BASE DATA (same as build script)
# ============================================================================

cat("Loading base data...\n")
countrymap <- fst::read_fst("data-raw/big_files/countrymap.fst")
techmap    <- fst::read_fst("data-raw/big_files/techmap.fst")

# Prepare techmap (same as build script)
techmap <- countrymap |>
  dplyr::select(docdb_family_id) |>
  dplyr::distinct() |>
  dplyr::mutate(technology = "All") |>
  dplyr::bind_rows(techmap)

data.table::setDT(techmap)
techmap[, technology := data.table::fcase(
  technology == "Any Green technology", "Green Technology",
  technology == "Any battery technology", "Battery Technology",
  technology == "Any Hard to Abate technology", "Hard to Abate Sector Decarbonization",
  default = technology
)]

# Colorings (same as build script)
colorings <- list(
  green = c("Green Technology", "Green Energy", "Green Transport",
            "Circular Economy", "Green Manufacturing", "Adaptation",
            "Green Housing", "Green ICT", "Green Agriculture", "GHG Capture"),
  battery = c("Battery Technology", "Lithium Extraction & Processing",
              "Graphite & Carbon Materials", "Cathode Materials", "Anode Materials",
              "Electrolytes & Additives", "Separators", "Battery Cell Design & Assembly",
              "Battery Management Systems (BMS)", "Electric Vehicles & Mobility",
              "Battery Recycling & Recovery"),
  hard_to_abate = c("Hard to Abate Sector Decarbonization", "Aviation Decarbonisation",
                    "Cement & Concrete Decarbonisation", "Chemicals & Plastics Decarbonisation",
                    "Shipping Decarbonisation", "Steel & Iron Decarbonisation"),
  ai = c("AI", "Machine Learning", "Deep Learning", "Natural Language Processing (NLP)",
         "Computer Vision", "Speech Recognition & Synthesis", "Robotics & Autonomous Systems",
         "Knowledge Representation & Reasoning", "Planning & Decision Making", "Generative AI",
         "Semiconductors", "Cloud & Data Infrastructure", "Data Rettrieval & Processing System",
         "Platform & Frameworks", "Deployment & Support"),
  cpcsecs = c("Human Necessities", "Performing Operations; Transporting ",
              "Chemistry; Metallurgy ", "Textiles; Paper", "Fixed Constructions",
              "Mechanical Engineering; Lighting; Heating; Weapons; Blasting",
              "Physics", "Electricity",
              "General tagging of new or cross-sectional technology"),
  agrifood = c("Any Agriculture & Food technology", "Input supply",
               "Primary food and feed production", "Post-harvest handling & aggregation",
               "Processing", "Distribution/wholesale", "Retail/consumption", "Crosscutting")
)

cat("  countrymap:", nrow(countrymap), "rows\n")
cat("  techmap:", nrow(techmap), "rows\n\n")

# ============================================================================
# PICK A TEST TOFLOW (smallest file for speed)
# ============================================================================

istrax_dir <- "data-raw/big_files/istraxes"
istrax_files <- list.files(istrax_dir, pattern = "\\.fst$", full.names = TRUE)
istrax_files <- istrax_files[!grepl("_joined", istrax_files)]
smallest <- istrax_files[which.min(file.info(istrax_files)$size)]
toflow <- tools::file_path_sans_ext(basename(smallest))

cat("Test toflow:", toflow, "\n")
cat("File size:", round(file.info(smallest)$size / 1e6, 1), "MB\n\n")

# ============================================================================
# LOAD ISTRAX DATA (same as build script's load_istrax_data)
# ============================================================================

istrax_data <- fst::read_fst(smallest)
if (toflow %in% names(istrax_data)) {
  istrax_data[[toflow]][is.na(istrax_data[[toflow]])] <- 0
}

# For Part 1 test: filter to a small country group (EU) for speed
eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
                  "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
                  "SI","ES","SE")

data_part1 <- countrymap |>
  dplyr::filter(ctry_code %in% eu_countries) |>
  dplyr::left_join(istrax_data, by = c("docdb_family_id", "ctry_code"))

# For Part 2 test: use all countries, filter to one tech
data_part2 <- countrymap |>
  dplyr::left_join(istrax_data, by = c("docdb_family_id", "ctry_code"))

# Tech classes for Part 2: Green Technology
classes_part2 <- techmap |>
  dplyr::filter(technology == "Green Technology") |>
  dplyr::select(docdb_family_id) |>
  dplyr::distinct()

cat("Part 1 test data:", nrow(data_part1), "rows (EU countries)\n")
cat("Part 2 test data:", nrow(data_part2), "rows (all countries)\n")
cat("Part 2 classes:", nrow(classes_part2), "patent families\n\n")

# ============================================================================
# SOURCE OLD FUNCTIONS (into a dedicated environment to avoid conflicts)
# ============================================================================

cat("Loading OLD functions from functions_istraxfunctions.R...\n")
old_env <- new.env(parent = globalenv())
source("R/functions_istraxfunctions.R", local = old_env)

cat("Loading NEW functions from functions_istraxfunctions_processing.R...\n")
new_env <- new.env(parent = globalenv())
source("R/functions_istraxfunctions_processing.R", local = new_env)
cat("\n")

# ============================================================================
# TEST 1: compute_avstrax (by-technology)
# ============================================================================

cat("=" |> strrep(60), "\n")
cat("TEST 1: compute_avstrax (by-technology, EU countries)\n")
cat("=" |> strrep(60), "\n\n")

# --- OLD ---
cat("Running OLD compute_avstrax...\n")
t_old_1 <- system.time({
  result_old_1 <- old_env$compute_avstrax(data_part1, toflow, techmap, colorings)
})
cat("  OLD time:", t_old_1["elapsed"], "seconds\n")
cat("  OLD result:", nrow(result_old_1), "rows x", ncol(result_old_1), "cols\n\n")

# --- NEW ---
cat("Running NEW compute_avstrax...\n")
t_new_1 <- system.time({
  result_new_1 <- new_env$compute_avstrax(data_part1, toflow, techmap, colorings)
})
cat("  NEW time:", t_new_1["elapsed"], "seconds\n")
cat("  NEW result:", nrow(result_new_1), "rows x", ncol(result_new_1), "cols\n\n")

# --- COMPARE ---
# Sort both by technology for stable comparison
result_old_1 <- result_old_1[order(result_old_1$technology), ]
result_new_1 <- result_new_1[order(result_new_1$technology), ]
rownames(result_old_1) <- NULL
rownames(result_new_1) <- NULL

# Compare numeric columns with tolerance, character columns exactly
numeric_cols_1 <- names(result_old_1)[sapply(result_old_1, is.numeric)]
char_cols_1 <- names(result_old_1)[sapply(result_old_1, is.character)]

cat("Comparing results...\n")

all_match_1 <- TRUE

# Check same columns exist
if (!setequal(names(result_old_1), names(result_new_1))) {
  cat("  ✗ COLUMN MISMATCH\n")
  cat("    Old only:", setdiff(names(result_old_1), names(result_new_1)), "\n")
  cat("    New only:", setdiff(names(result_new_1), names(result_old_1)), "\n")
  all_match_1 <- FALSE
} else {
  cat("  ✓ Same columns\n")
}

# Check same number of rows
if (nrow(result_old_1) != nrow(result_new_1)) {
  cat("  ✗ ROW COUNT MISMATCH:", nrow(result_old_1), "vs", nrow(result_new_1), "\n")
  all_match_1 <- FALSE
} else {
  cat("  ✓ Same row count:", nrow(result_old_1), "\n")
}

# Compare shared columns
shared_cols_1 <- intersect(names(result_old_1), names(result_new_1))
for (col in shared_cols_1) {
  cmp <- all.equal(result_old_1[[col]], result_new_1[[col]], tolerance = 1e-10)
  if (!isTRUE(cmp)) {
    cat("  ✗", col, ":", cmp, "\n")
    all_match_1 <- FALSE
  }
}

if (all_match_1) {
  cat("  ✓ ALL VALUES MATCH\n")
}

speedup_1 <- t_old_1["elapsed"] / t_new_1["elapsed"]
cat(sprintf("\n  Speedup: %.2fx (%s: %.2fs old → %.2fs new)\n\n",
            speedup_1, toflow, t_old_1["elapsed"], t_new_1["elapsed"]))

# ============================================================================
# TEST 2: compute_avstrax_for_techs (by-country)
# ============================================================================

cat("=" |> strrep(60), "\n")
cat("TEST 2: compute_avstrax_for_techs (by-country, Green Tech)\n")
cat("=" |> strrep(60), "\n\n")

# --- OLD ---
cat("Running OLD compute_avstrax_for_techs...\n")
t_old_2 <- system.time({
  result_old_2 <- old_env$compute_avstrax_for_techs(data_part2, toflow, classes_part2)
})
cat("  OLD time:", t_old_2["elapsed"], "seconds\n")
cat("  OLD result:", nrow(result_old_2), "rows x", ncol(result_old_2), "cols\n\n")

# --- NEW ---
cat("Running NEW compute_avstrax_for_techs...\n")
t_new_2 <- system.time({
  result_new_2 <- new_env$compute_avstrax_for_techs(data_part2, toflow, classes_part2)
})
cat("  NEW time:", t_new_2["elapsed"], "seconds\n")
cat("  NEW result:", nrow(result_new_2), "rows x", ncol(result_new_2), "cols\n\n")

# --- COMPARE ---
result_old_2 <- result_old_2[order(result_old_2$ctry_code), ]
result_new_2 <- result_new_2[order(result_new_2$ctry_code), ]
rownames(result_old_2) <- NULL
rownames(result_new_2) <- NULL

cat("Comparing results...\n")

all_match_2 <- TRUE

if (!setequal(names(result_old_2), names(result_new_2))) {
  cat("  ✗ COLUMN MISMATCH\n")
  cat("    Old only:", setdiff(names(result_old_2), names(result_new_2)), "\n")
  cat("    New only:", setdiff(names(result_new_2), names(result_old_2)), "\n")
  all_match_2 <- FALSE
} else {
  cat("  ✓ Same columns\n")
}

if (nrow(result_old_2) != nrow(result_new_2)) {
  cat("  ✗ ROW COUNT MISMATCH:", nrow(result_old_2), "vs", nrow(result_new_2), "\n")
  all_match_2 <- FALSE
} else {
  cat("  ✓ Same row count:", nrow(result_old_2), "\n")
}

shared_cols_2 <- intersect(names(result_old_2), names(result_new_2))
for (col in shared_cols_2) {
  cmp <- all.equal(result_old_2[[col]], result_new_2[[col]], tolerance = 1e-10)
  if (!isTRUE(cmp)) {
    cat("  ✗", col, ":", cmp, "\n")
    all_match_2 <- FALSE
  }
}

if (all_match_2) {
  cat("  ✓ ALL VALUES MATCH\n")
}

speedup_2 <- t_old_2["elapsed"] / t_new_2["elapsed"]
cat(sprintf("\n  Speedup: %.2fx (%s: %.2fs old → %.2fs new)\n\n",
            speedup_2, toflow, t_old_2["elapsed"], t_new_2["elapsed"]))

# ============================================================================
# SUMMARY
# ============================================================================

cat("=" |> strrep(60), "\n")
cat("SUMMARY\n")
cat("=" |> strrep(60), "\n\n")

cat(sprintf("Test toflow: %s\n\n", toflow))

cat("compute_avstrax (by-technology):\n")
cat(sprintf("  Values match: %s\n", ifelse(all_match_1, "YES ✓", "NO ✗")))
cat(sprintf("  Old: %.2fs | New: %.2fs | Speedup: %.2fx\n\n",
            t_old_1["elapsed"], t_new_1["elapsed"], speedup_1))

cat("compute_avstrax_for_techs (by-country):\n")
cat(sprintf("  Values match: %s\n", ifelse(all_match_2, "YES ✓", "NO ✗")))
cat(sprintf("  Old: %.2fs | New: %.2fs | Speedup: %.2fx\n\n",
            t_old_2["elapsed"], t_new_2["elapsed"], speedup_2))

if (all_match_1 && all_match_2) {
  cat("✓ ALL TESTS PASSED — safe to use optimized functions.\n")
} else {
  cat("✗ SOME TESTS FAILED — investigate before using optimized functions.\n")
}
