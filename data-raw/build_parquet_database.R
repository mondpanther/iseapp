# Build Single Parquet Database for ISE App
# This script creates one parquet file containing all patent data
# with joins to countries, technologies, firms, and all istrax measures
# Can be queried with DuckDB for any UI selection combination

library(fst)
library(dplyr)
library(arrow)
library(countrycode)

# 1. Load base maps
cat("Loading base data...\n")
countrymap <- read_fst("data-raw/big_files/countrymap.fst")
regionmap <- read_fst("data-raw/big_files/regionmap.fst")
techmap <- read_fst("data-raw/big_files/techmap.fst")

top_companies <- arrow::read_parquet("data-raw/big_files/firmmap.parquet") |>
  dplyr::group_by(company_raw) |>
  dplyr::count() |>
  dplyr::arrange(desc(n)) |>
  dplyr::ungroup() |>
  dplyr::slice_head(n = 100) |>
  dplyr::pull(company_raw)

firmmap_top100 <- arrow::read_parquet("data-raw/big_files/firmmap.parquet") |>
  dplyr::filter(company_raw %in% top_companies) |>
  dplyr::rename(firm = company_raw)

# firmmap_top100 |>
#   count(docdb_family_id) |>
#   arrange(desc(n))

# firmmap_top100 |>
#   filter(
#     docdb_family_id == "62562639"
#   )

# countrymap |> 
#   count(docdb_family_id) |>
#   arrange(desc(n))

# countrymap |>
#   dplyr::filter(
#     docdb_family_id =="58995202"
#   )

firmsectormap <- arrow::read_parquet("data-raw/big_files/firmsectormap.parquet") |>
  dplyr::select(company_raw, firm_sector = sector)

cat("  ✓ countrymap:", nrow(countrymap), "rows\n")
cat("  ✓ regionmap:", nrow(regionmap), "rows\n")
cat("  ✓ techmap:", nrow(techmap), "rows\n")
cat("  ✓ firmmap:", nrow(arrow::read_parquet("data-raw/big_files/firmmap.parquet")), "rows\n")
cat("  ✓ firmmap top 100:", nrow(firmmap_top100), "rows\n\n")
cat("  ✓ firmsectormap:", nrow(firmsectormap), "rows\n")

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

# foo <- read_fst("data-raw/big_files/istraxes/istrax_global.fst" )
# nrow(read_fst("data-raw/big_files/istraxes/istrax_FR.fst"))
# nrow(read_fst("data-raw/big_files/istraxes/istrax_global.fst"))

# foo |>
#   filter(
#     docdb_family_id =="58995202"
#   )

# names(foo)

# istrax_global <- read_fst("data-raw/big_files/istraxes/istrax_global.fst")
# istrax_global |>
#   count(docdb_family_id) |>
#   arrange(desc(n)) |>
#   head(10)
# istrax_global |>
#   add_count(docdb_family_id) |>
#   arrange(desc(n), docdb_family_id)|>
#   head(10)

# countrymap |>
#   dplyr::filter(
#     docdb_family_id == "58995202"
#   ) |>
#   head()

# arrow::read_parquet("data-raw/big_files/firmmap.parquet") |>
#   dplyr::filter(
#     docdb_family_id == "58995202"
#   ) |>
#   head(20)


# ev_global <- read_fst("data-raw/big_files/istraxes/ev_global.fst")
# ev_global |>
#   count(docdb_family_id) |>
#   arrange(desc(n)) |>
#   head(20)

# ev_global |>
#   add_count(docdb_family_id) |>
#   arrange(desc(n), docdb_family_id)|>
#   head(20)

# countrymap |>
#   dplyr::filter(
#     docdb_family_id == ""
#   ) |>
#   head()

# arrow::read_parquet("data-raw/big_files/firmmap.parquet") |>
#   dplyr::filter(
#     docdb_family_id == ""
#   ) |>
#   head(20)

# 3. Join BOTH country and region data
cat("Joining country and region mappings...\n")
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

# UK Region join
patent_data <- istrax_data

# Add region columns for GB patents
patent_data <- patent_data |>
  left_join(
    regionmap |>
      select(docdb_family_id, ctry_code, region_code, region_name),
    by = c("docdb_family_id", "ctry_code"),
    relationship = "many-to-many"
  )

cat("  ✓ Joined regionmap\n")
cat("  Rows after region join:", nrow(patent_data), "\n\n")

# 4. Join technologies
cat("Joining technologies...\n")
patent_data <- patent_data |>
  left_join(
    techmap |>
      select(docdb_family_id, technology),
    by = "docdb_family_id",
    relationship = "many-to-many"
  )

# techmap |>
#   count(docdb_family_id) |>
#   arrange(desc(n))

# techmap |>
#   filter(
#     docdb_family_id == "48872867"
#   )

cat("  ✓ Joined techmap\n")
cat("  Rows after tech join:", nrow(patent_data), "\n\n")

# 5. Join firms
cat("Joining firms...\n")
patent_data <- patent_data |>
  left_join(
    firmmap_top100 |>
      select(docdb_family_id, firm),
    by = c("docdb_family_id"),
    relationship = "many-to-many"
  )

cat("  ✓ Joined firmmap\n")
cat("  Rows after firm join:", nrow(patent_data), "\n\n")

# 6. Add classification columns
cat("Adding classification columns...\n")

# ---- Technology group mapping ----
green_classes         <- c("Green Technology","Green Energy","Green Transport","Circular Economy","Green Manufacturing","Adaptation","Green Housing","Green ICT","Green Agriculture","GHG Capture")
battery_classes       <- c("Battery Technology","Lithium Extraction & Processing","Graphite & Carbon Materials","Cathode Materials","Anode Materials","Electrolytes & Additives","Separators","Battery Cell Design & Assembly","Battery Management Systems (BMS)","Electric Vehicles & Mobility","Battery Recycling & Recovery")
hard_to_abate_classes <- c("Hard to Abate Sector Decarbonization","Aviation Decarbonisation","Cement & Concrete Decarbonisation","Chemicals & Plastics Decarbonisation","Shipping Decarbonisation","Steel & Iron Decarbonisation")
ai_classes            <- c("AI","Machine Learning","Deep Learning","Natural Language Processing (NLP)","Computer Vision","Speech Recognition & Synthesis","Robotics & Autonomous Systems","Knowledge Representation & Reasoning","Planning & Decision Making","Generative AI","Semiconductors","Cloud & Data Infrastructure","Data Retrieval & Processing System","Platform & Frameworks","Deployment & Support")
cpc_sections          <- c("Human Necessities","Performing Operations; Transporting ","Chemistry; Metallurgy ","Textiles; Paper","Fixed Constructions","Mechanical Engineering; Lighting; Heating; Weapons; Blasting","Physics","Electricity","General tagging of new or cross-sectional technology")
agrifood_classes      <- c("Any Agriculture & Food technology","Input supply","Primary food and feed production","Post-harvest handling & aggregation","Processing","Distribution/wholesale","Retail/consumption","Crosscutting")

# Named vector: sub-technology -> umbrella group name
tech_group_map <- c(
  setNames(rep("Green Technology",                     length(green_classes)),         green_classes),
  setNames(rep("Battery Technology",                   length(battery_classes)),        battery_classes),
  setNames(rep("Hard to Abate Sector Decarbonization", length(hard_to_abate_classes)), hard_to_abate_classes),
  setNames(rep("AI",                                   length(ai_classes)),             ai_classes),
  setNames(rep("Any Agriculture & Food technology",    length(agrifood_classes)),       agrifood_classes),
  setNames(cpc_sections,                                                                cpc_sections)
)

# NOTE: tech_group_map and class vectors are duplicated in build_ui_data.R.
# TODO: consolidate into R/data_classifications.R as a single source of truth.

# ---- Country group boolean flags ----
all_iso2      <- unique(na.omit(countrycode::codelist$iso2c))
lmics         <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG","BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI","CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE","GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ","KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML","MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG","MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC","SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO","TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")
eu_countries  <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
hic           <- setdiff(all_iso2, lmics)

# ---- Apply classification columns to patent_data ----
patent_data <- patent_data |>
  dplyr::mutate(
    # Technology umbrella group
    tech_group = dplyr::coalesce(tech_group_map[technology], "Other"),
    # Country group boolean flags
    is_lmic             = ctry_code %in% lmics,
    is_lmic_excl_china  = ctry_code %in% setdiff(lmics, "CN"),
    is_eu               = ctry_code %in% eu_countries,
    is_hic              = ctry_code %in% hic
  ) |>
  # Firm sector — join on firm name
  dplyr::left_join(
    firmsectormap,
    by = c("firm" = "company_raw")
  )

cat("  ✓ tech_group: ", n_distinct(patent_data$tech_group, na.rm = TRUE), "groups\n")
cat("  ✓ is_lmic, is_eu, is_hic flags added\n")
cat("  ✓ firm_sector: ", n_distinct(patent_data$firm_sector, na.rm = TRUE), "sectors\n")

# 7. Preview final structure
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
cat("  Regions:", n_distinct(patent_data$region_code), "\n")
cat("  Technologies:", n_distinct(patent_data$technology, na.rm = TRUE), "\n")
cat("  Firms:", n_distinct(patent_data$firm, na.rm = TRUE), "\n\n")

# 8. Save as parquet
cat("Saving to parquet...\n")
output_file <- "inst/extdata/full_patent_database.parquet"

# Create directory if needed
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# unused_cols <- c("avstrax_DE", "avstrax_EUPLUSUK", "avstrax_G7", "avstrax_IN", "ev_IN", "istrax_IN")


cat("Sorting data for optimal parquet predicate pushdown...\n")
patent_data <- patent_data |>
  # dplyr::select(-dplyr::any_of(unused_cols)) |>
  dplyr::arrange(ctry_code, tech_group, firm)
cat("  ✓ Sorted by ctry_code, tech_group, firm\n")

# Write parquet with compression
write_parquet(
  patent_data,
  output_file,
  compression = "zstd",
  compression_level = 3,
  chunk_size = 122070  # ~14.5M / 119 groups ≈ 100KB-200KB per group compressed
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
