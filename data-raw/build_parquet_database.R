# Build Single Parquet Database for ISE App
# This script creates one parquet file containing all patent data
# with joins to countries, technologies, firms, and all istrax measures
# Can be queried with DuckDB for any UI selection combination

library(fst)
library(dplyr)
library(arrow)
library(purrr)

cat("=== BUILDING PARQUET DATABASE ===\n\n")

# 1. Load base maps
cat("Loading base data...\n")
countrymap <- read_fst("data-raw/big_files/countrymap.fst")
techmap <- read_fst("data-raw/big_files/techmap.fst")
firmmap <- read_fst("data-raw/big_files/firmmap.fst") |>
  dplyr::filter(firm == "Hitachi")

cat("  ✓ countrymap:", nrow(countrymap), "rows\n")
cat("  ✓ techmap:", nrow(techmap), "rows\n")
cat("  ✓ firmmap:", nrow(firmmap), "rows\n\n")

# 2. Find all istrax files
cat("Finding istrax files...\n")
istrax_files <- list.files(
  "data-raw/big_files/istraxes",
  pattern = "\\.fst$",
  full.names = TRUE
)

# Filter out _joined files (those are preprocessed)
istrax_files <- istrax_files[!grepl("_joined", basename(istrax_files))]

cat("  Found", length(istrax_files), "istrax files\n")
print(basename(istrax_files))
cat("\n")

# 3. Load and join all istrax files
cat("Loading istrax files...\n")
istrax_data <- countrymap |>
  select(docdb_family_id, ctry_code)

for (file in istrax_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  cat("  Loading", file_name, "...\n")
  
  istrax <- read_fst(file) |>
    select(docdb_family_id, ctry_code, value = 3) # Third column is the measure
  
  # Rename value column to the measure name
  istrax <- istrax |>
    rename(!!file_name := value)
  
  # Join to main dataset
  istrax_data <- istrax_data |>
    left_join(istrax, by = c("docdb_family_id", "ctry_code"))
}

cat("  ✓ All istrax measures joined\n")
cat("  Total columns:", ncol(istrax_data), "\n\n")

# 4. Join technologies
cat("Joining technologies...\n")
patent_data <- istrax_data |>
  left_join(
    techmap |>
      select(docdb_family_id, technology),
    by = "docdb_family_id",
    relationship = "many-to-many"
  )

cat("  ✓ Joined techmap\n")
cat("  Rows after tech join:", nrow(patent_data), "\n\n")

# 5. Join firms
cat("Joining firms...\n")
patent_data <- patent_data |>
  left_join(
    firmmap |>
      select(docdb_family_id, firm),
    by = "docdb_family_id",
    relationship = "many-to-many"
  )

cat("  ✓ Joined firmmap\n")
cat("  Rows after firm join:", nrow(patent_data), "\n\n")

# 6. Preview final structure
cat("=== FINAL DATABASE STRUCTURE ===\n")
cat("Total rows:", nrow(patent_data), "\n")
cat("Total columns:", ncol(patent_data), "\n")
cat("Memory size:", format(object.size(patent_data), units = "GB"), "\n\n")

cat("Columns:\n")
print(colnames(patent_data))

cat("\nSample data:\n")
print(head(patent_data, 10))

cat("\nUnique values:\n")
cat("  Patents:", n_distinct(patent_data$docdb_family_id), "\n")
cat("  Countries:", n_distinct(patent_data$ctry_code), "\n")
cat("  Technologies:", n_distinct(patent_data$technology, na.rm = TRUE), "\n")
cat("  Firms:", n_distinct(patent_data$firm, na.rm = TRUE), "\n\n")

# 7. Save as parquet
cat("Saving to parquet...\n")
output_file <- "inst/extdata/full_patent_database.parquet"

# Create directory if needed
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# Write parquet with compression
write_parquet(
  patent_data,
  output_file,
  compression = "zstd",
  compression_level = 3
)

cat("  ✓ Saved to", output_file, "\n")

# Check file size
file_size <- file.info(output_file)$size / 1024^3 # Convert to GB
cat("  File size:", round(file_size, 2), "GB\n\n")

cat("=== DONE ===\n")
cat("Next steps:\n")
cat("1. Test querying with DuckDB\n")
cat("2. Update app to use this database\n")
cat("3. Compare speed with preprocessed approach\n")
