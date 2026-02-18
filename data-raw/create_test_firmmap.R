# Create Test Firmmap
# This script creates a test firmmap.fst with sample firms
# Structure matches techmap.fst (docdb_family_id, firm)
# Later this will be replaced with real firmmap.fst

library(fst)
library(dplyr)

# Load countrymap to get real patent IDs
countrymap <- read_fst("data-raw/big_files/countrymap.fst")

# Define test firms (major tech companies)
test_firms <- c(
  "Hitachi",
  "Sony", 
  "Samsung",
  "Toyota",
  "Panasonic",
  "Siemens",
  "General Electric",
  "IBM",
  "Microsoft",
  "Apple"
)

# Sample patents for each firm
set.seed(42)
unique_patents <- unique(countrymap$docdb_family_id)

# Create firmmap - assign random patents to firms
# Each patent can belong to one firm (like techmap structure)
firmmap <- tibble(
  docdb_family_id = sample(unique_patents, size = 50000, replace = FALSE),
  firm = sample(test_firms, size = 50000, replace = TRUE)
)

# Sort by patent ID for consistency
firmmap <- firmmap |>
  arrange(docdb_family_id)

# Preview
cat("=== FIRMMAP PREVIEW ===\n")
cat("Rows:", nrow(firmmap), "\n")
cat("Unique patents:", n_distinct(firmmap$docdb_family_id), "\n")
cat("Unique firms:", n_distinct(firmmap$firm), "\n\n")
print(head(firmmap, 20))

cat("\n=== FIRM COUNTS ===\n")
firmmap |>
  count(firm, sort = TRUE) |>
  print()

# Save as FST
write_fst(firmmap, "data-raw/big_files/firmmap.fst")

cat("\n✓ Saved to data-raw/big_files/firmmap.fst\n")
