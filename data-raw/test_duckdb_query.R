# Test DuckDB Queries on Full Patent Database
# Run this to verify query speed before updating the app

library(duckdb)
library(dplyr)
library(tictoc)

cat("=== TESTING DUCKDB QUERIES ===\n\n")

# Connect to DuckDB
con <- dbConnect(duckdb())

# Test 1: Single country + single tech
cat("Test 1: Single country (US) + Single tech (AI)\n")
tic()
result1 <- dbGetQuery(con, "
  SELECT 
    technology,
    COUNT(*) as n_patents,
    AVG(avstrax_global) as mean_avstrax,
    MEDIAN(avstrax_global) as median_avstrax,
    QUANTILE_CONT(avstrax_global, 0.25) as q25,
    QUANTILE_CONT(avstrax_global, 0.75) as q75
  FROM read_parquet('inst/extdata/full_patent_database.parquet')
  WHERE ctry_code = 'US' 
    AND technology = 'AI'
    AND avstrax_global IS NOT NULL
  GROUP BY technology
")
toc()
print(result1)
cat("\n")

# Test 2: Country group (multiple countries) + tech category
cat("Test 2: Multiple countries (US, CN, DE) + Green Energy\n")
tic()
result2 <- dbGetQuery(con, "
  SELECT 
    ctry_code,
    COUNT(*) as n_patents,
    AVG(avstrax_global) as mean_avstrax
  FROM read_parquet('inst/extdata/full_patent_database.parquet')
  WHERE ctry_code IN ('US', 'CN', 'DE')
    AND technology = 'Green Energy'
    AND avstrax_global IS NOT NULL
  GROUP BY ctry_code
  ORDER BY mean_avstrax DESC
")
toc()
print(result2)
cat("\n")

# Test 3: Firm query
cat("Test 3: Single firm (Hitachi) + AI technology\n")
tic()
result3 <- dbGetQuery(con, "
  SELECT 
    firm,
    technology,
    COUNT(*) as n_patents,
    AVG(avstrax_global) as mean_avstrax
  FROM read_parquet('inst/extdata/full_patent_database.parquet')
  WHERE firm = 'Hitachi'
    AND technology = 'AI'
    AND avstrax_global IS NOT NULL
  GROUP BY firm, technology
")
toc()
print(result3)
cat("\n")

# Test 4: Complex aggregation (like module_country does)
cat("Test 4: All technologies for US (complex aggregation)\n")
tic()
result4 <- dbGetQuery(con, "
  SELECT 
    technology,
    COUNT(*) as innos,
    AVG(avstrax_global) as mean,
    STDDEV(avstrax_global) as sd,
    QUANTILE_CONT(avstrax_global, 0.25) as q1,
    QUANTILE_CONT(avstrax_global, 0.50) as q2,
    QUANTILE_CONT(avstrax_global, 0.75) as q3
  FROM read_parquet('inst/extdata/full_patent_database.parquet')
  WHERE ctry_code = 'US'
    AND avstrax_global IS NOT NULL
    AND technology IS NOT NULL
  GROUP BY technology
  ORDER BY innos DESC
  LIMIT 10
")
toc()
print(result4)
cat("\n")

# Test 5: Top patents (for top3_ids functionality)
cat("Test 5: Top 10 patents for US + AI\n")
tic()
result5 <- dbGetQuery(con, "
  SELECT 
    docdb_family_id,
    ctry_code,
    technology,
    avstrax_global
  FROM read_parquet('inst/extdata/full_patent_database.parquet')
  WHERE ctry_code = 'US'
    AND technology = 'AI'
    AND avstrax_global IS NOT NULL
  ORDER BY avstrax_global DESC
  LIMIT 10
")
toc()
print(result5)
cat("\n")

dbDisconnect(con, shutdown = TRUE)

cat("=== SUMMARY ===\n")
cat("If all queries completed in <500ms, the database approach is viable!\n")
cat("Compare these times to loading preprocessed FST files.\n")
