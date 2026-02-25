# duck_queries.R
# SQL query functions that replace the R-based compute functions.
# All heavy data work (joins, filtering, aggregation) happens in DuckDB.
# Functions return data frames in the same format the plotting functions expect.
#
# Performance notes:
# - Uses pre-joined tables (istrax_country, istrax_region) created by prep_duckdb.Rmd
#   to eliminate expensive runtime joins between istrax_values and countrymap/regionmap.
# - Uses pre-computed patent count tables (country_patent_counts, region_patent_counts)
#   to avoid repeated COUNT DISTINCT queries for RTA calculation.
# - Uses TEMP tables to avoid scanning+joining data twice (main aggregation + extras).

library(DBI)
library(duckdb)

# ============================================================================
# Connection Management
# ============================================================================

#' Open a read-only DuckDB connection
#' @param duck_db_path Path to the .duckdb file
#' @return DBI connection object
duck_connect <- function(duck_db_path) {
  if (!file.exists(duck_db_path)) {
    stop("DuckDB database not found at: ", duck_db_path,
         "\nPlease run prep_duckdb.Rmd first to build the database.")
  }
  con <- dbConnect(duckdb(), dbdir = duck_db_path, read_only = TRUE)
  message("Connected to DuckDB: ", duck_db_path)
  con
}


# ============================================================================
# Helper: Build SQL IN clause from a character vector
# ============================================================================
sql_in_list <- function(values) {
  paste0("('", paste(gsub("'", "''", values), collapse = "','"), "')")
}


# ============================================================================
# Helper: Determine scaler for flow type
# ============================================================================
get_scaler <- function(flow_type) {
  if (grepl("strax", flow_type)) 100 else 1
}


# ============================================================================
# Firm filter helper: resolve firm_selection to list of company_raw names
# ============================================================================

#' Resolve firm filter selections to company names
#' Handles both individual firms and "sector:XXX" selections
#' @param con DuckDB connection
#' @param firm_selection Character vector of selections (can include "sector:Name")
#' @return Character vector of company_raw names, or NULL if no filter
resolve_firm_filter <- function(con, firm_selection) {
  if (is.null(firm_selection) || "All" %in% firm_selection) {
    return(NULL)
  }

  sector_selections <- grep("^sector:", firm_selection, value = TRUE)
  sector_names <- sub("^sector:", "", sector_selections)
  firm_selections <- setdiff(firm_selection, sector_selections)

  firms_from_sectors <- character(0)
  if (length(sector_names) > 0) {
    q <- paste0("SELECT DISTINCT company_raw FROM firmsectormap WHERE sector IN ",
                sql_in_list(sector_names))
    firms_from_sectors <- dbGetQuery(con, q)$company_raw
  }

  all_firms <- unique(c(firms_from_sectors, firm_selections))
  if (length(all_firms) == 0) return(NULL)
  all_firms
}


# ============================================================================
# Build firm filter SQL clause
# ============================================================================

#' Build a SQL subquery clause to filter by firm
#' @param firm_names Character vector of company_raw names, or NULL
#' @param alias Table alias prefix for docdb_family_id (default "ic")
#' @return SQL fragment string (empty string if no filter)
firm_filter_sql <- function(firm_names, alias = "ic") {
  if (is.null(firm_names) || length(firm_names) == 0) return("")
  paste0(" AND ", alias, ".docdb_family_id IN (SELECT DISTINCT docdb_family_id FROM firmmap WHERE company_raw IN ",
         sql_in_list(firm_names), ")")
}


# ============================================================================
# duck_compute_avstrax: Group by TECHNOLOGY
# Replaces compute_avstrax() from istraxfunctions.R
# ============================================================================

#' Compute average istrax by technology category (DuckDB version)
#'
#' Uses pre-joined istrax_country/istrax_region tables for fast queries.
#' Materializes the shared CTE into a temp table to avoid scanning data twice
#' (once for main aggregation, once for top25/top50/top3 extras).
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string (e.g. "istrax_global")
#' @param tech_categories Character vector of technology categories to include.
#'   If NULL, uses all technologies from techmap.
#' @param country_codes Character vector of country codes (or region codes if use_regionmap=TRUE)
#' @param firm_names Character vector of company_raw names for firm filter, or NULL
#' @param other_label If not NULL, technologies NOT in tech_categories are relabeled to this
#'   (used for Plot 1's "Other" category)
#' @param colorings Named list for greenclass assignment (passed through, applied in R)
#' @param use_regionmap Logical; if TRUE, uses istrax_region instead of istrax_country
#' @return Data frame with columns: technology, mean, innos, sem, q1, q2, q3,
#'   top25, top50, top25_bin_mean, top50_bin_mean, top3_ids, top3_ids_url, greenclass
duck_compute_avstrax <- function(con, flow_type, tech_categories = NULL,
                                  country_codes, firm_names = NULL,
                                  other_label = NULL, colorings = NULL,
                                  use_regionmap = FALSE) {

  scaler <- get_scaler(flow_type)

  # Technology column expression (with optional "Other" relabeling)
  if (!is.null(other_label) && !is.null(tech_categories)) {
    tech_case <- paste0(
      "CASE WHEN t.technology IN ", sql_in_list(tech_categories),
      " THEN t.technology ELSE '", gsub("'", "''", other_label), "' END"
    )
  } else {
    tech_case <- "t.technology"
  }

  # Technology filter (independent of "Other" relabeling)
  if (!is.null(tech_categories) && length(tech_categories) > 0 && is.null(other_label)) {
    tech_filter_sql <- paste0("AND t.technology IN ", sql_in_list(tech_categories))
  } else {
    tech_filter_sql <- ""
  }

  # Choose pre-joined table and filter based on regionmap or countrymap
  if (use_regionmap) {
    src_table <- "istrax_region"
    src_alias <- "ir"
    country_filter <- paste0("AND ir.region_code IN ", sql_in_list(country_codes))
    appln_col <- "ir.appln_id"
  } else {
    src_table <- "istrax_country"
    src_alias <- "ic"
    country_filter <- paste0("AND ic.ctry_code IN ", sql_in_list(country_codes))
    appln_col <- "ic.appln_id"
  }

  ff_sql <- firm_filter_sql(firm_names, src_alias)

  # --- Create temp table with the shared CTE result ---
  # This avoids scanning+joining data twice (main + extras queries)
  temp_sql <- paste0("
    CREATE TEMP TABLE IF NOT EXISTS _avstrax_combined AS
    WITH base AS (
      SELECT DISTINCT
        ", tech_case, " AS technology,
        ", src_alias, ".docdb_family_id,
        ", appln_col, " AS appln_id,
        ", src_alias, ".value * ", scaler, " AS val
      FROM ", src_table, " ", src_alias, "
      JOIN techmap t
        ON ", src_alias, ".docdb_family_id = t.docdb_family_id
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        ", country_filter, "
        AND t.technology != 'All'
        ", tech_filter_sql, "
        ", ff_sql, "
    ),
    base_all AS (
      SELECT DISTINCT
        'All' AS technology,
        ", src_alias, ".docdb_family_id,
        ", appln_col, " AS appln_id,
        ", src_alias, ".value * ", scaler, " AS val
      FROM ", src_table, " ", src_alias, "
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        ", country_filter, "
        ", ff_sql, "
    )
    SELECT * FROM base
    UNION ALL
    SELECT * FROM base_all
  ")

  # Drop any leftover temp table, create new one
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _avstrax_combined"), error = function(e) NULL)
  dbExecute(con, temp_sql)

  # --- Main aggregation from temp table ---
  main_sql <- "
    SELECT
      technology,
      AVG(val) AS mean,
      COUNT(*) AS innos,
      CASE WHEN COUNT(*) > 1
        THEN STDDEV_SAMP(val) / SQRT(COUNT(*))
        ELSE 0
      END AS sem,
      PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY val) AS q1,
      PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY val) AS q2,
      PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY val) AS q3
    FROM _avstrax_combined
    GROUP BY technology
  "

  result <- dbGetQuery(con, main_sql)

  if (nrow(result) == 0) {
    tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _avstrax_combined"), error = function(e) NULL)
    return(data.frame(
      technology = character(0), mean = numeric(0), innos = integer(0),
      sem = numeric(0), q1 = numeric(0), q2 = numeric(0), q3 = numeric(0),
      top25 = numeric(0), top50 = numeric(0),
      top25_bin_mean = numeric(0), top50_bin_mean = numeric(0),
      top3_ids = character(0), top3_ids_url = character(0),
      greenclass = character(0)
    ))
  }

  # --- Top25/Top50 bin means + top3 appln_ids from same temp table ---
  extras_sql <- "
    WITH ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY technology ORDER BY val DESC) AS prank
      FROM _avstrax_combined
    )
    SELECT
      technology,
      AVG(CASE WHEN prank < 0.25 THEN val END) AS top25_bin_mean,
      AVG(CASE WHEN prank < 0.50 THEN val END) AS top50_bin_mean,
      STRING_AGG(CASE WHEN prank_inner <= 10 THEN appln_id END, ', ')
        AS top3_ids
    FROM (
      SELECT *,
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY val DESC) AS prank_inner
      FROM ranked
    ) sub
    GROUP BY technology
  "

  extras <- dbGetQuery(con, extras_sql)

  # Clean up temp table
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _avstrax_combined"), error = function(e) NULL)

  # Merge extras with main result
  result <- merge(result, extras, by = "technology", all.x = TRUE)

  # Add compatibility columns
  result$top25 <- 0.25
  result$top50 <- 0.5

  # Build Espacenet search URL (reuse function from istraxfunctions.R)
  result$top3_ids_url <- if (exists("build_espacenet_search")) {
    build_espacenet_search(result$top3_ids)
  } else {
    rep("", nrow(result))
  }

  # Add greenclass categorization (lightweight R-side lookup)
  if (!is.null(colorings)) {
    result$greenclass <- ifelse(result$technology %in% unlist(colorings["green"]), "green",
                         ifelse(result$technology %in% unlist(colorings["battery"]), "battery",
                         ifelse(result$technology %in% unlist(colorings["hard_to_abate"]), "hard to abate",
                         ifelse(result$technology %in% unlist(colorings["ai"]), "AI",
                         ifelse(result$technology %in% unlist(colorings["cpcsecs"]), "CPC Sections",
                         ifelse(result$technology %in% unlist(colorings["agrifood"]), "agrifood", "other"))))))
  } else {
    result$greenclass <- "other"
  }

  result
}


# ============================================================================
# duck_compute_avstrax_for_techs: Group by COUNTRY
# Replaces compute_avstrax_for_techs() from istraxfunctions.R
# ============================================================================

#' Compute average istrax by country/region (DuckDB version)
#'
#' Uses pre-joined istrax_country/istrax_region tables and pre-computed patent
#' count tables for fast queries. Materializes shared CTE into temp table.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param tech_filter Character vector of technology names to filter by.
#'   If empty/NULL, includes all patents (no technology filter).
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @param use_regionmap Logical; if TRUE, uses istrax_region instead of istrax_country
#' @return Data frame with columns: ctry_code, [country_name], mean, innos, sem,
#'   q1, q2, q3, top25, top50, top25_bin_mean, top50_bin_mean, top3_ids,
#'   top3_ids_url, Allinnos, SumAllinnos, share_c, share, RTA
duck_compute_avstrax_for_techs <- function(con, flow_type, tech_filter = NULL,
                                            country_codes, firm_names = NULL,
                                            use_regionmap = FALSE) {

  scaler <- get_scaler(flow_type)

  # Choose pre-joined table and grouping columns
  if (use_regionmap) {
    src_table <- "istrax_region"
    src_alias <- "ir"
    group_col <- "ir.region_code"
    name_col <- "ir.region_name"
    country_filter <- paste0("ir.region_code IN ", sql_in_list(country_codes))
    appln_col <- "ir.appln_id"
    count_table <- "region_patent_counts"
  } else {
    src_table <- "istrax_country"
    src_alias <- "ic"
    group_col <- "ic.ctry_code"
    name_col <- "NULL"
    country_filter <- paste0("ic.ctry_code IN ", sql_in_list(country_codes))
    appln_col <- "ic.appln_id"
    count_table <- "country_patent_counts"
  }

  ff_sql <- firm_filter_sql(firm_names, src_alias)

  # Technology filter
  tech_join_sql <- ""
  if (!is.null(tech_filter) && length(tech_filter) > 0) {
    tech_join_sql <- paste0(
      " JOIN techmap t ON ", src_alias, ".docdb_family_id = t.docdb_family_id AND t.technology IN ",
      sql_in_list(tech_filter)
    )
  }

  # --- Create temp table with the shared CTE result ---
  temp_sql <- paste0("
    CREATE TEMP TABLE IF NOT EXISTS _techs_combined AS
    WITH filtered AS (
      SELECT DISTINCT
        ", group_col, " AS ctry_code,
        ", name_col, " AS country_name,
        ", src_alias, ".docdb_family_id,
        ", appln_col, " AS appln_id,
        ", src_alias, ".value * ", scaler, " AS val
      FROM ", src_table, " ", src_alias, "
      ", tech_join_sql, "
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    ),
    filtered_all AS (
      SELECT DISTINCT
        'All' AS ctry_code,
        'All' AS country_name,
        ", src_alias, ".docdb_family_id,
        ", appln_col, " AS appln_id,
        ", src_alias, ".value * ", scaler, " AS val
      FROM ", src_table, " ", src_alias, "
      ", tech_join_sql, "
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    )
    SELECT * FROM filtered
    UNION ALL
    SELECT * FROM filtered_all
  ")

  # Drop any leftover temp table, create new one
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _techs_combined"), error = function(e) NULL)
  dbExecute(con, temp_sql)

  # --- Main aggregation from temp table ---
  main_sql <- "
    SELECT
      ctry_code,
      MAX(country_name) AS country_name,
      AVG(val) AS mean,
      COUNT(*) AS innos,
      CASE WHEN COUNT(*) > 1
        THEN STDDEV_SAMP(val) / SQRT(COUNT(*))
        ELSE 0
      END AS sem,
      PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY val) AS q1,
      PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY val) AS q2,
      PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY val) AS q3
    FROM _techs_combined
    GROUP BY ctry_code
  "

  result <- dbGetQuery(con, main_sql)

  if (nrow(result) == 0) {
    tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _techs_combined"), error = function(e) NULL)
    return(data.frame(
      ctry_code = character(0), mean = numeric(0), innos = integer(0),
      sem = numeric(0), q1 = numeric(0), q2 = numeric(0), q3 = numeric(0),
      top25 = numeric(0), top50 = numeric(0),
      top25_bin_mean = numeric(0), top50_bin_mean = numeric(0),
      top3_ids = character(0), top3_ids_url = character(0),
      Allinnos = integer(0), SumAllinnos = integer(0),
      share_c = numeric(0), share = numeric(0), RTA = numeric(0)
    ))
  }

  # Remove country_name column if it's all NULL (non-region case)
  if (!use_regionmap) {
    result$country_name <- NULL
  }

  # --- Top25/Top50 + top3_ids from same temp table ---
  extras_sql <- "
    WITH ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS prank,
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS rn
      FROM _techs_combined
    )
    SELECT
      ctry_code,
      AVG(CASE WHEN prank < 0.25 THEN val END) AS top25_bin_mean,
      AVG(CASE WHEN prank < 0.50 THEN val END) AS top50_bin_mean,
      STRING_AGG(CASE WHEN rn <= 10 THEN appln_id END, ', ') AS top3_ids
    FROM ranked
    GROUP BY ctry_code
  "

  extras <- dbGetQuery(con, extras_sql)

  # Clean up temp table
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _techs_combined"), error = function(e) NULL)

  result <- merge(result, extras, by = "ctry_code", all.x = TRUE)

  # --- Allinnos from pre-computed patent counts table ---
  counted_sql <- paste0("
    SELECT ctry_code, Allinnos
    FROM ", count_table, "
    WHERE ctry_code IN ", sql_in_list(country_codes)
  )
  counted <- dbGetQuery(con, counted_sql)

  sum_all_sql <- paste0("
    SELECT SUM(Allinnos) AS SumAllinnos
    FROM ", count_table, "
    WHERE ctry_code IN ", sql_in_list(country_codes)
  )
  sum_all <- dbGetQuery(con, sum_all_sql)$SumAllinnos

  # Total filtered innovations (distinct patents matching tech filter)
  if (!is.null(tech_filter) && length(tech_filter) > 0) {
    total_filtered_sql <- paste0("
      SELECT COUNT(DISTINCT ", src_alias, ".docdb_family_id) AS total
      FROM ", src_table, " ", src_alias, "
      ", tech_join_sql, "
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql
    )
  } else {
    total_filtered_sql <- paste0("
      SELECT COUNT(DISTINCT ", src_alias, ".docdb_family_id) AS total
      FROM ", src_table, " ", src_alias, "
      WHERE ", src_alias, ".flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql
    )
  }
  total_filtered <- dbGetQuery(con, total_filtered_sql)$total

  # Join counted and compute RTA
  result <- merge(result, counted, by = "ctry_code", all.x = TRUE)
  result$SumAllinnos <- sum_all

  # RTA = 2 * share_c / (share_c + share)
  # share_c = innos / Allinnos (country's share of filtered tech in total patents)
  # share = total_filtered / SumAllinnos (global share of filtered tech)
  result$share_c <- result$innos / result$Allinnos
  result$share <- total_filtered / sum_all
  result$RTA <- 2 * result$share_c / (result$share_c + result$share)

  # Compatibility columns
  result$top25 <- 0.25
  result$top50 <- 0.5

  # Espacenet URL
  result$top3_ids_url <- if (exists("build_espacenet_search")) {
    build_espacenet_search(result$top3_ids)
  } else {
    rep("", nrow(result))
  }

  result
}


# ============================================================================
# duck_get_istrax_joined: Raw joined data (fallback for complex cases)
# ============================================================================

#' Get raw joined data from DuckDB (for cases requiring custom R processing)
#'
#' Uses the pre-joined istrax_country table for fast retrieval.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @return Data frame with columns: docdb_family_id, ctry_code, appln_id, {flow_type}
duck_get_istrax_joined <- function(con, flow_type, country_codes,
                                    firm_names = NULL) {

  ff_sql <- firm_filter_sql(firm_names, "ic")

  sql <- paste0("
    SELECT
      ic.docdb_family_id,
      ic.ctry_code,
      ic.appln_id,
      ic.value
    FROM istrax_country ic
    WHERE ic.flow_type = '", gsub("'", "''", flow_type), "'
      AND ic.ctry_code IN ", sql_in_list(country_codes), "
    ", ff_sql
  )

  result <- dbGetQuery(con, sql)

  # Rename 'value' to the flow_type name for compatibility with existing code
  names(result)[names(result) == "value"] <- flow_type

  result
}


# ============================================================================
# duck_get_istrax_joined_regions: Raw joined data for regions
# ============================================================================

#' Get raw joined data for regions from DuckDB
#'
#' Uses the pre-joined istrax_region table for fast retrieval.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param region_codes Character vector of region codes (e.g. "UKC", "UKD")
#' @param firm_names Character vector of company_raw names, or NULL
#' @return Data frame with columns matching patchar_regionmap format
duck_get_istrax_joined_regions <- function(con, flow_type, region_codes,
                                            firm_names = NULL) {

  ff_sql <- firm_filter_sql(firm_names, "ir")

  sql <- paste0("
    SELECT
      ir.docdb_family_id,
      ir.region_code AS ctry_code,
      ir.region_name AS country_name,
      ir.appln_id,
      ir.value
    FROM istrax_region ir
    WHERE ir.flow_type = '", gsub("'", "''", flow_type), "'
      AND ir.region_code IN ", sql_in_list(region_codes), "
    ", ff_sql
  )

  result <- dbGetQuery(con, sql)

  # Rename 'value' to the flow_type name for compatibility
  names(result)[names(result) == "value"] <- flow_type

  result
}


# ============================================================================
# duck_get_country_groups: Expand country group to individual codes
# ============================================================================

#' Get expanded country codes for a group name
#' @param con DuckDB connection
#' @param group_name Name of the country group
#' @return Character vector of ISO2 country codes
duck_get_country_groups <- function(con, group_name) {
  sql <- paste0("SELECT ctry_code FROM country_groups WHERE group_name = '",
                gsub("'", "''", group_name), "' ORDER BY ctry_code")
  dbGetQuery(con, sql)$ctry_code
}


# ============================================================================
# duck_get_techmap_techs: Get distinct technology names
# ============================================================================

#' Get list of unique technology names from DuckDB
#' @param con DuckDB connection
#' @return Character vector of technology names
duck_get_techmap_techs <- function(con) {
  dbGetQuery(con, "SELECT DISTINCT technology FROM techmap ORDER BY technology")$technology
}


# ============================================================================
# duck_get_firmsectormap: Get firm-sector data for UI
# ============================================================================

#' Get firmsectormap data for building UI choices
#' @param con DuckDB connection
#' @return Data frame with company_raw, sector, RnD
duck_get_firmsectormap <- function(con) {
  dbGetQuery(con, "SELECT company_raw, sector, RnD FROM firmsectormap ORDER BY sector, RnD DESC")
}
