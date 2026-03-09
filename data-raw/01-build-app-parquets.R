# Build Single Parquet Database for ISE App
# This script creates one parquet file containing all patent data
# with joins to countries, technologies, firms, and all istrax measures
# Can be queried with DuckDB for any UI selection combination

library(fst)
library(dplyr)
library(arrow)
library(countrycode)
library(stringr)

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
  dplyr::rename(firm = company_raw) |>
  dplyr::select(docdb_family_id, firm)


firmsectormap <- arrow::read_parquet("data-raw/big_files/firmsectormap.parquet") |>
  dplyr::select(firm = company_raw, firm_sector = sector)

cat("  ✓ countrymap:", nrow(countrymap), "rows\n")
cat("  ✓ regionmap:", nrow(regionmap), "rows\n")
cat("  ✓ techmap:", nrow(techmap), "rows\n")
cat("  ✓ firmmap:", nrow(arrow::read_parquet("data-raw/big_files/firmmap.parquet")), "rows\n")
cat("  ✓ firmmap top 100:", nrow(firmmap_top100), "rows\n\n")
cat("  ✓ firmsectormap:", nrow(firmsectormap), "rows\n")

# 2. Find all istrax files
cat("Finding istrax files...\n")
toflow_files <- list.files(
  "data-raw/big_files/istraxes",
  pattern = "\\.fst$",
  full.names = TRUE
)

# Filter out _joined files (those are preprocessed)
toflow_files <- toflow_files[!grepl("_joined", basename(toflow_files))]

cat("  Found", length(toflow_files), "istrax files\n")
print(basename(toflow_files))
cat("\n")

# 3. Build and write slim bridge: patents_x_region
cat("Building patents_x_region bridge...\n")
patents_x_region <- regionmap |>
  dplyr::select(docdb_family_id, region_code) |>
  dplyr::distinct()

write_parquet(patents_x_region, "inst/extdata/patents_x_region.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written patents_x_region.parquet:", nrow(patents_x_region), "rows\n\n")

# 3b. Build and write region_lookup
cat("Building region_lookup...\n")
region_lookup <- regionmap |>
  dplyr::select(region_code, region_name) |>
  dplyr::distinct()

write_parquet(region_lookup, "inst/extdata/region_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written region_lookup.parquet:", nrow(region_lookup), "rows\n\n")

# 4. Build and write slim bridge: patents_x_tech
# # ---- Technology group mapping ----
green_classes         <- c("Any Green technology", "Green Technology","Green Energy","Green Transport","Circular Economy","Green Manufacturing","Adaptation","Green Housing","Green ICT","Green Agriculture","GHG Capture")
battery_classes       <- c("Any battery technology", "Battery Technology","Lithium Extraction & Processing","Graphite & Carbon Materials","Cathode Materials","Anode Materials","Electrolytes & Additives","Separators","Battery Cell Design & Assembly","Battery Management Systems (BMS)","Electric Vehicles & Mobility","Battery Recycling & Recovery")
hard_to_abate_classes <- c("Any Hard to Abate technology", "Hard to Abate Sector Decarbonization","Aviation Decarbonisation","Cement & Concrete Decarbonisation","Chemicals & Plastics Decarbonisation","Shipping Decarbonisation","Steel & Iron Decarbonisation")
ai_classes            <- c("AI","Machine Learning","Deep Learning","Natural Language Processing (NLP)","Computer Vision","Speech Recognition & Synthesis","Robotics & Autonomous Systems","Knowledge Representation & Reasoning","Planning & Decision Making","Generative AI","Semiconductors","Cloud & Data Infrastructure","Data Rettrieval & Processing System","Platform & Frameworks","Deployment & Support")
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

# # ---- Country group boolean flags ----
all_iso2      <- unique(na.omit(countrycode::codelist$iso2c))
lmics         <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG","BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI","CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE","GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ","KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML","MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG","MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC","SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO","TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")
eu_countries  <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
hic           <- setdiff(all_iso2, lmics)

cat("Building patents_x_tech bridge...\n")
patents_x_tech <- techmap |>
  dplyr::select(docdb_family_id, technology) |>
  dplyr::distinct()

write_parquet(patents_x_tech, "inst/extdata/patents_x_tech.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written patents_x_tech.parquet:", nrow(patents_x_tech), "rows\n\n")

# 4b. Build and write tech_lookup
cat("Building tech_lookup...\n")
tech_lookup <- techmap |>
  dplyr::select(technology) |>
  dplyr::distinct() |>
  dplyr::mutate(tech_group = dplyr::coalesce(tech_group_map[technology], "Other"))

write_parquet(tech_lookup, "inst/extdata/tech_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written tech_lookup.parquet:", nrow(tech_lookup), "rows\n\n")

# 5. Build and write slim bridge: patents_x_firm
cat("Building patents_x_firm bridge...\n")
patents_x_firm <- firmmap_top100 |>
  dplyr::select(docdb_family_id, firm) |>
  dplyr::distinct()

write_parquet(patents_x_firm, "inst/extdata/patents_x_firm.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written patents_x_firm.parquet:", nrow(patents_x_firm), "rows\n\n")

# 5b. Build and write firm_lookup
cat("Building firm_lookup...\n")
firm_lookup <- firmsectormap |>
  filter(firm %in% top_companies)

write_parquet(firm_lookup, "inst/extdata/firm_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written firm_lookup.parquet:", nrow(firm_lookup), "rows\n\n")


# 6. Build country_lookup
cat("Building country_lookup...\n")
country_lookup <- countrymap |>
  select(ctry_code) |>
  distinct() |>
  dplyr::mutate(
    is_lmic            = ctry_code %in% lmics,
    is_lmic_excl_china = ctry_code %in% setdiff(lmics, "CN"),
    is_eu              = ctry_code %in% eu_countries,
    is_hic             = ctry_code %in% hic
  )

write_parquet(country_lookup, "inst/extdata/country_lookup.parquet",
              compression = "zstd", compression_level = 3)
cat("  ✓ Written country_lookup.parquet:", nrow(country_lookup), "rows\n\n")

# 7. Build toflow_database from source files (wide)
tictoc::tic()
cat("Building toflow_database from source files...\n")

patent_data <- countrymap |>
  dplyr::select(docdb_family_id, ctry_code, appln_id) |>
  distinct()

for (f in toflow_files) {
  cat("  Joining", basename(f), "...\n")
  measure_name <- basename(f) |>
    tools::file_path_sans_ext() |>
    tolower() |>
    stringr::str_replace_all("avstrax_", "av_") |>
    stringr::str_replace_all("istrax_", "is_") |>
    stringr::str_replace_all("ev_", "ev_")

  toflow <- read_fst(f) |>
    dplyr::select(docdb_family_id, ctry_code, 3) |>
    dplyr::rename(!!measure_name := 3) |>
    dplyr::mutate(dplyr::across(where(is.double), \(x) tidyr::replace_na(x, 0)))

  patent_data <- patent_data |>
    dplyr::left_join(toflow, by = c("docdb_family_id", "ctry_code"))
}

# Round to 4 decimal places and replace post-join NAs with 0
patent_data <- patent_data |>
  dplyr::mutate(dplyr::across(where(is.double), \(x) tidyr::replace_na(x, 0))) |>
  dplyr::mutate(dplyr::across(where(is.double), \(x) round(x, 4)))

cat("  ✓ toflow_data built:", nrow(patent_data), "rows,", ncol(patent_data), "columns\n\n")
tictoc::toc()

# 7b. Pivot to long format
# tictoc::tic()
# cat("Pivoting to long format...\n")

# patent_data_long <- patent_data |>
#   dplyr::select(-ctry_code, -appln_id) |>
#   tidyr::pivot_longer(
#     cols      = -docdb_family_id,
#     names_to  = "measure",
#     values_to = "value"
#   ) |>
#   dplyr::filter(!is.na(value))

# patent_data_long |>
#   dplyr::filter(value == 0) |>
#   nrow()

# patent_data_long |>
#   dplyr::filter(is.na(value)) |>
#   nrow()

# cat("  ✓ Long built:", nrow(patent_data_long), "rows\n\n")
# tictoc::toc()


# 8. Preview final structure
cat("=== FINAL DATABASE STRUCTURE ===\n")
cat("Total rows:", nrow(patent_data), "\n")
cat("Total columns:", ncol(patent_data), "\n")
cat("Memory size:", format(object.size(patent_data), units = "GB"), "\n\n")

cat("Columns:\n")
print(colnames(patent_data))

cat("\nSample data:\n")
print(head(patent_data, 10))

cat("\nUnique values:\n")
cat("  Patents:", nrow(patent_data), "\n")
cat("  Patents:", n_distinct(patent_data$docdb_family_id), "\n")
cat("  Countries:", n_distinct(patent_data$ctry_code), "\n")
cat("  ToFlows:", ncol(patent_data) - 3, "\n\n")

# 8. Save as parquet
cat("Saving to parquet...\n")
output_file <- "inst/extdata/patent_database.parquet"

# Create directory if needed
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# cat("Sorting data for optimal parquet predicate pushdown...\n")
patent_data <- patent_data |>
  dplyr::arrange(ctry_code)
cat("  ✓ Sorted by ctry_code\n")

float_schema <- arrow::schema(
  purrr::map(names(patent_data), \(col) {
    if (col == "docdb_family_id") {
      arrow::field(col, arrow::int32())
    } else if (is.double(patent_data[[col]])) {
      arrow::field(col, arrow::float32())
    } else {
      arrow::field(col, arrow::infer_type(patent_data[[col]]))
    }
  })
)

# Write parquet with compression
patent_data |>
  arrow::as_arrow_table(schema = float_schema) |>
  arrow::write_parquet(
    output_file,
    compression = "zstd",
    compression_level = 3
  )

cat("  ✓ Saved to", output_file, "\n")

# Check file size
file_size <- file.info(output_file)$size / 1024^3 # Convert to GB
cat("  File size:", round(file_size, 2), "GB\n\n")

cat("=== DONE ===\n")

# 9 Load and convert the InGlobe data
df_raw <- fst::read_fst("data-raw/big_files/long_final.fst")
# names(df_raw)
# glimpse(df_raw)
# df_raw2 <- arrow::read_parquet("data-raw/big_files/citation_links_long_NEW_expanded_sampled.parquet")
# names(df_raw2)
# glimpse(df_raw2)

# parquet_path <- normalizePath("inst/extdata/patent_database.parquet")

# Open via DuckDB — same pattern as the app
# con <- duckdb::dbConnect(duckdb::duckdb())
# DBI::dbExecute(con, sprintf(
#   "CREATE VIEW patent_database AS SELECT * FROM read_parquet('%s')",
#   parquet_path
# ))
# main_patentIDs <- DBI::dbGetQuery(con,
#   "SELECT DISTINCT docdb_family_id FROM patent_database"
# ) |>
#   dplyr::arrange() |>
#   dplyr::pull(docdb_family_id)

# inglobe_IDs_from <- df_raw2 |>
#   dplyr::select(from_id) |>
#   dplyr::distinct() |>
#   dplyr::arrange() |>
#   dplyr::pull(from_id)

# inglobe_IDs_to <- df_raw2 |>
#   dplyr::select(to_id) |>
#   dplyr::distinct() |>
#   dplyr::arrange() |>
#   dplyr::pull(to_id)

# cat("Count of distinct patent family IDs in main database")
# length(main_patentIDs)
# cat("Count of distinct IDs from_id column of new inglobe data")
# length(inglobe_IDs_from)
# cat("Count of distinct IDs to_id column of new inglobe data")
# length(inglobe_IDs_to)

# cat("IDs in new inglobe data from_id column NOT in main_patentIDs")
# missing_from_main_1 <- dplyr::setdiff(inglobe_IDs_from, main_patentIDs)
# length(missing_from_main_1)
# cat("IDs in new inglobe data to_id column NOT in main_patentIDs")
# missing_from_main_2 <- dplyr::setdiff(inglobe_IDs_to, main_patentIDs)
# length(missing_from_main_2)


df_processed <- df_raw |>
  dplyr::arrange(sce_country, tech_group, tech_subgroup, source_id, wave) |>
  dplyr::mutate(
    wave = as.integer(wave),
    chain_id = paste0(
      sce_country, "_", tech_group, "_",
      ifelse(is.na(tech_subgroup), "ALL", tech_subgroup), "_",
      sample_size, "_", source_id
    )
  )

df_processed <- df_processed |>
  dplyr::select(
    sce_country,
    sce_tech_display,
    tech_group,
    sample_size,
    wave,
    source_lon,
    source_lat,
    target_lon,
    target_lat,
    chain_id
  )

arrow::write_parquet(df_processed, "inst/extdata/inglobe_processed.parquet")