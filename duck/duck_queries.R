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

#' Open a DuckDB connection (MotherDuck cloud or local file)
#'
#' If MOTHERDUCK_TOKEN is set, tries MotherDuck first. Falls back to local file.
#' Note: The MotherDuck R extension is not available on Windows; on Windows the
#' fallback to local file will be used automatically.
#'
#' @param duck_db_path Path to the local .duckdb file (NULL to skip local fallback)
#' @return DBI connection object
duck_connect <- function(duck_db_path = NULL) {
  md_token <- Sys.getenv("MOTHERDUCK_TOKEN")

  # Try MotherDuck if token is available
  if (nzchar(md_token)) {
    con <- tryCatch({
      message("Connecting to MotherDuck...")
      md_con <- dbConnect(duckdb::duckdb())
      dbExecute(md_con, "INSTALL 'motherduck'")
      dbExecute(md_con, "LOAD 'motherduck'")
      dbExecute(md_con, paste0("SET motherduck_token = '", md_token, "'"))
      dbExecute(md_con, "ATTACH 'md:iseapp' AS iseapp")
      dbExecute(md_con, "USE iseapp")
      message("Connected to MotherDuck: iseapp")
      md_con
    }, error = function(e) {
      message("MotherDuck connection failed: ", conditionMessage(e))
      message("Falling back to local database...")
      NULL
    })
    if (!is.null(con)) return(con)
  }

  # Fallback: local file
  if (is.null(duck_db_path) || !file.exists(duck_db_path)) {
    stop("DuckDB database not found at: ", duck_db_path,
         "\nSet MOTHERDUCK_TOKEN for cloud access, or run prep_duckdb.Rmd for local.")
  }
  con <- dbConnect(duckdb(), dbdir = duck_db_path, read_only = TRUE)
  message("Connected to local DuckDB: ", duck_db_path)
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
#' Greenclass is resolved via SQL JOIN with tech_greenclass table.
#' Espacenet URLs are built in SQL using STRING_AGG + CONCAT.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string (e.g. "istrax_global")
#' @param tech_categories Character vector of technology categories to include.
#'   If NULL, uses all technologies from techmap.
#' @param country_codes Character vector of country codes (or region codes if use_regionmap=TRUE)
#' @param firm_names Character vector of company_raw names for firm filter, or NULL
#' @param other_label If not NULL, technologies NOT in tech_categories are relabeled to this
#'   (used for Plot 1's "Other" category)
#' @param use_regionmap Logical; if TRUE, uses istrax_region instead of istrax_country
#' @return Data frame with columns: technology, mean, innos, sem, q1, q2, q3,
#'   top25, top50, top25_bin_mean, top50_bin_mean, top3_ids, top3_ids_url, greenclass
duck_compute_avstrax <- function(con, flow_type, tech_categories = NULL,
                                  country_codes, firm_names = NULL,
                                  other_label = NULL,
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

  # --- Single query: main aggregation + extras + greenclass + URLs ---
  full_sql <- "
    WITH main_agg AS (
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
    ),
    ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY technology ORDER BY val DESC) AS prank,
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY val DESC) AS rn
      FROM _avstrax_combined
    ),
    extras AS (
      SELECT
        technology,
        AVG(CASE WHEN prank < 0.25 THEN val END) AS top25_bin_mean,
        AVG(CASE WHEN prank < 0.50 THEN val END) AS top50_bin_mean,
        STRING_AGG(CASE WHEN rn <= 10 THEN CAST(appln_id AS VARCHAR) END, ', ')
          AS top3_ids,
        'javascript:window.open(\"https://worldwide.espacenet.com/patent/search?q=' ||
          REPLACE(
            COALESCE(STRING_AGG(
              CASE WHEN rn <= 10 THEN 'ap%3D' || CAST(appln_id AS VARCHAR) END,
              '%20OR%20'
            ), ''),
            ' ', '%20'
          ) || '\")' AS top3_ids_url
      FROM ranked
      GROUP BY technology
    )
    SELECT
      m.technology,
      m.mean,
      m.innos,
      m.sem,
      m.q1,
      m.q2,
      m.q3,
      0.25 AS top25,
      0.5 AS top50,
      e.top25_bin_mean,
      e.top50_bin_mean,
      e.top3_ids,
      e.top3_ids_url,
      COALESCE(tg.greenclass, 'other') AS greenclass
    FROM main_agg m
    LEFT JOIN extras e ON m.technology = e.technology
    LEFT JOIN tech_greenclass tg ON m.technology = tg.technology
  "

  result <- dbGetQuery(con, full_sql)

  # Clean up temp table
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _avstrax_combined"), error = function(e) NULL)

  if (nrow(result) == 0) {
    return(data.frame(
      technology = character(0), mean = numeric(0), innos = integer(0),
      sem = numeric(0), q1 = numeric(0), q2 = numeric(0), q3 = numeric(0),
      top25 = numeric(0), top50 = numeric(0),
      top25_bin_mean = numeric(0), top50_bin_mean = numeric(0),
      top3_ids = character(0), top3_ids_url = character(0),
      greenclass = character(0)
    ))
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
#' All processing (aggregation, RTA calculation, country name resolution,
#' Espacenet URL building, mininno/minallinnos filtering) happens in SQL.
#' Returns a fully plot-ready dataframe.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param tech_filter Character vector of technology names to filter by.
#'   If empty/NULL, includes all patents (no technology filter).
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @param use_regionmap Logical; if TRUE, uses istrax_region instead of istrax_country
#' @param min_innos Minimum innovation count to include a country (default 0 = no filter)
#' @param min_allinnos Minimum all-innovation count threshold (default 0 = no filter)
#' @return Data frame with columns: ctry_code, country_name, mean, innos, sem,
#'   q1, q2, q3, top25, top50, top25_bin_mean, top50_bin_mean, top3_ids,
#'   top3_ids_url, Allinnos, SumAllinnos, share_c, share, RTA
duck_compute_avstrax_for_techs <- function(con, flow_type, tech_filter = NULL,
                                            country_codes, firm_names = NULL,
                                            use_regionmap = FALSE,
                                            min_innos = 0,
                                            min_allinnos = 0) {

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
    # For regions, country_name comes from the source table
    name_join_sql <- ""
    name_expr <- "MAX(tc.country_name)"
  } else {
    src_table <- "istrax_country"
    src_alias <- "ic"
    group_col <- "ic.ctry_code"
    name_col <- "NULL"
    country_filter <- paste0("ic.ctry_code IN ", sql_in_list(country_codes))
    appln_col <- "ic.appln_id"
    count_table <- "country_patent_counts"
    # For countries, country_name comes from the country_names lookup table
    name_join_sql <- ""
    name_expr <- "NULL"
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

  # --- Build filter clauses for min_innos and min_allinnos ---
  having_clause <- ""
  where_filters <- character(0)
  if (min_innos > 0) {
    where_filters <- c(where_filters, paste0("m.innos >= ", min_innos))
  }
  # min_allinnos filter applied after JOIN with patent counts
  allinnos_filter <- ""
  if (min_allinnos > 0) {
    allinnos_filter <- paste0(" AND pc.Allinnos >= ", min_allinnos)
  }

  innos_where <- if (length(where_filters) > 0) {
    paste0(" WHERE ", paste(where_filters, collapse = " AND "))
  } else {
    ""
  }

  # --- Single comprehensive query: agg + extras + RTA + country names + URLs ---
  full_sql <- paste0("
    WITH main_agg AS (
      SELECT
        ctry_code,
        MAX(country_name) AS region_name,
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
    ),
    ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS prank,
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS rn
      FROM _techs_combined
    ),
    extras AS (
      SELECT
        ctry_code,
        AVG(CASE WHEN prank < 0.25 THEN val END) AS top25_bin_mean,
        AVG(CASE WHEN prank < 0.50 THEN val END) AS top50_bin_mean,
        STRING_AGG(CASE WHEN rn <= 10 THEN CAST(appln_id AS VARCHAR) END, ', ')
          AS top3_ids,
        'javascript:window.open(\"https://worldwide.espacenet.com/patent/search?q=' ||
          REPLACE(
            COALESCE(STRING_AGG(
              CASE WHEN rn <= 10 THEN 'ap%3D' || CAST(appln_id AS VARCHAR) END,
              '%20OR%20'
            ), ''),
            ' ', '%20'
          ) || '\")' AS top3_ids_url
      FROM ranked
      GROUP BY ctry_code
    ),
    patent_counts AS (
      SELECT ctry_code, Allinnos
      FROM ", count_table, "
      WHERE ctry_code IN ", sql_in_list(country_codes), "
    ),
    total_patent_counts AS (
      SELECT SUM(Allinnos) AS SumAllinnos
      FROM ", count_table, "
      WHERE ctry_code IN ", sql_in_list(country_codes), "
    ),
    total_filtered AS (
      SELECT COUNT(DISTINCT docdb_family_id) AS total_innos
      FROM _techs_combined
      WHERE ctry_code != 'All'
    )
    SELECT
      m.ctry_code,
      CASE
        WHEN m.ctry_code = 'All' THEN 'All'
        WHEN ", if (use_regionmap) "TRUE" else "FALSE", " THEN m.region_name
        ELSE COALESCE(cn.country_name, m.ctry_code)
      END AS country_name,
      m.mean,
      m.innos,
      m.sem,
      m.q1,
      m.q2,
      m.q3,
      0.25 AS top25,
      0.5 AS top50,
      e.top25_bin_mean,
      e.top50_bin_mean,
      e.top3_ids,
      e.top3_ids_url,
      pc.Allinnos,
      tp.SumAllinnos,
      CASE WHEN pc.Allinnos > 0
        THEN m.innos * 1.0 / pc.Allinnos
        ELSE NULL
      END AS share_c,
      CASE WHEN tp.SumAllinnos > 0
        THEN tf.total_innos * 1.0 / tp.SumAllinnos
        ELSE NULL
      END AS share,
      CASE
        WHEN pc.Allinnos > 0 AND tp.SumAllinnos > 0
          AND (m.innos * 1.0 / pc.Allinnos + tf.total_innos * 1.0 / tp.SumAllinnos) > 0
        THEN 2.0 * (m.innos * 1.0 / pc.Allinnos) /
             (m.innos * 1.0 / pc.Allinnos + tf.total_innos * 1.0 / tp.SumAllinnos)
        ELSE NULL
      END AS RTA
    FROM main_agg m
    LEFT JOIN extras e ON m.ctry_code = e.ctry_code
    LEFT JOIN patent_counts pc ON m.ctry_code = pc.ctry_code
    CROSS JOIN total_patent_counts tp
    CROSS JOIN total_filtered tf",
    if (!use_regionmap) "
    LEFT JOIN country_names cn ON m.ctry_code = cn.ctry_code" else "", "
    WHERE (m.ctry_code = 'All' OR TRUE)
    ", if (min_innos > 0) paste0("AND (m.ctry_code = 'All' OR m.innos >= ", min_innos, ")") else "", "
    ", if (min_allinnos > 0) paste0("AND (m.ctry_code = 'All' OR pc.Allinnos IS NULL OR pc.Allinnos >= ", min_allinnos, ")") else ""
  )

  result <- dbGetQuery(con, full_sql)

  # Clean up temp table
  tryCatch(dbExecute(con, "DROP TABLE IF EXISTS _techs_combined"), error = function(e) NULL)

  if (nrow(result) == 0) {
    return(data.frame(
      ctry_code = character(0), country_name = character(0),
      mean = numeric(0), innos = integer(0),
      sem = numeric(0), q1 = numeric(0), q2 = numeric(0), q3 = numeric(0),
      top25 = numeric(0), top50 = numeric(0),
      top25_bin_mean = numeric(0), top50_bin_mean = numeric(0),
      top3_ids = character(0), top3_ids_url = character(0),
      Allinnos = integer(0), SumAllinnos = integer(0),
      share_c = numeric(0), share = numeric(0), RTA = numeric(0)
    ))
  }

  result
}


# ============================================================================
# duck_data_for_gdp_scatter: avstrax_for_techs + GDP join
# ============================================================================

#' Get avstrax-for-techs data with GDP per capita joined (for scatter plots)
#'
#' Wraps duck_compute_avstrax_for_techs() and JOINs the gdp_data table in SQL.
#' Filters out UK regions and rows without GDP data.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param tech_filter Character vector of technology names, or NULL
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @param min_innos Minimum innovation count (default 0)
#' @param min_allinnos Minimum all-innovation count (default 0)
#' @return Data frame with all avstrax_for_techs columns plus gdp_pc_2015, log_gdp_pc
duck_data_for_gdp_scatter <- function(con, flow_type, tech_filter = NULL,
                                       country_codes, firm_names = NULL,
                                       min_innos = 0, min_allinnos = 0) {

  # Get the base avstrax_for_techs data
  avstrax_data <- duck_compute_avstrax_for_techs(
    con, flow_type,
    tech_filter = tech_filter,
    country_codes = country_codes,
    firm_names = firm_names,
    min_innos = min_innos,
    min_allinnos = min_allinnos
  )

  if (nrow(avstrax_data) == 0) return(avstrax_data)

  # Filter out "All" and UK regions, then JOIN GDP via SQL
  # We have the data in R already; do the GDP join in SQL for consistency
  uk_regions <- c("UKC","UKD","UKE","UKF","UKG","UKH","UKI","UKJ","UKK","UKL","UKM","UKN")
  eligible <- avstrax_data[!avstrax_data$ctry_code %in% c("All", uk_regions), ]

  if (nrow(eligible) == 0) return(eligible)

  # Query GDP data from DuckDB for the specific country codes
  gdp_sql <- paste0("
    SELECT ctry_code, gdp_pc_2015, LN(gdp_pc_2015) AS log_gdp_pc
    FROM gdp_data
    WHERE gdp_pc_2015 IS NOT NULL AND gdp_pc_2015 > 0
      AND ctry_code IN ", sql_in_list(eligible$ctry_code)
  )
  gdp <- dbGetQuery(con, gdp_sql)

  # Merge
  result <- merge(eligible, gdp, by = "ctry_code", all.x = FALSE)
  result
}


# ============================================================================
# build_espacenet_search: URL builder (kept for backward compatibility)
# ============================================================================

#' Build Espacenet search URLs from comma-separated application IDs
#' @param id_strings Character vector of comma-separated appln_id strings
#' @return Character vector of javascript onclick URLs
build_espacenet_search <- function(id_strings) {
  sapply(id_strings, function(ids) {
    if (is.na(ids) || ids == "") return("")
    id_vec <- unlist(strsplit(ids, ",\\s*"))
    query <- paste(paste0("ap=", id_vec), collapse = " OR ")
    paste0('window.open("https://worldwide.espacenet.com/patent/search?q=',
           utils::URLencode(query, reserved = TRUE), '")')
  }, USE.NAMES = FALSE)
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
