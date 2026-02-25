# duck_queries.R
# SQL query functions that replace the R-based compute functions.
# All heavy data work (joins, filtering, aggregation) happens in DuckDB.
# Functions return data frames in the same format the plotting functions expect.

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
#' @return SQL fragment string (empty string if no filter)
firm_filter_sql <- function(firm_names) {
  if (is.null(firm_names) || length(firm_names) == 0) return("")
  paste0(" AND iv.docdb_family_id IN (SELECT DISTINCT docdb_family_id FROM firmmap WHERE company_raw IN ",
         sql_in_list(firm_names), ")")
}


# ============================================================================
# duck_compute_avstrax: Group by TECHNOLOGY
# Replaces compute_avstrax() from istraxfunctions.R
# ============================================================================

#' Compute average istrax by technology category (DuckDB version)
#'
#' This is the SQL rewrite of compute_avstrax(). It joins istrax_values with
#' countrymap and techmap inside DuckDB, groups by technology, and computes
#' mean/sem/quantiles/top25/top50/top3_ids.
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
#' @param use_regionmap Logical; if TRUE, joins with regionmap instead of countrymap
#' @return Data frame with columns: technology, mean, innos, sem, q1, q2, q3,
#'   top25, top50, top25_bin_mean, top50_bin_mean, top3_ids, top3_ids_url, greenclass
duck_compute_avstrax <- function(con, flow_type, tech_categories = NULL,
                                  country_codes, firm_names = NULL,
                                  other_label = NULL, colorings = NULL,
                                  use_regionmap = FALSE) {

  scaler <- get_scaler(flow_type)
  ff_sql <- firm_filter_sql(firm_names)

  # Build the technology CASE WHEN for "Other" relabeling
  if (!is.null(other_label) && !is.null(tech_categories)) {
    # Technologies in tech_categories keep their name; all others become other_label
    tech_case <- paste0(
      "CASE WHEN t.technology IN ", sql_in_list(tech_categories),
      " THEN t.technology ELSE '", gsub("'", "''", other_label), "' END"
    )
  } else {
    tech_case <- "t.technology"
  }

  # Choose map table and join/filter based on regionmap or countrymap
  if (use_regionmap) {
    map_join <- "JOIN regionmap rm ON iv.docdb_family_id = rm.docdb_family_id AND iv.ctry_code = rm.ctry_code"
    map_filter <- paste0("AND rm.region_code IN ", sql_in_list(country_codes))
    appln_col <- "rm.appln_id"
  } else {
    map_join <- "JOIN countrymap cm ON iv.docdb_family_id = cm.docdb_family_id AND iv.ctry_code = cm.ctry_code"
    map_filter <- paste0("AND iv.ctry_code IN ", sql_in_list(country_codes))
    appln_col <- "cm.appln_id"
  }

  # --- Main aggregation query ---
  # This mirrors compute_avstrax: join istrax_values with countrymap/regionmap on
  # (docdb_family_id, ctry_code), join with techmap on docdb_family_id,
  # select distinct (technology, docdb_family_id, value), group by technology
  main_sql <- paste0("
    WITH base AS (
      SELECT DISTINCT
        ", tech_case, " AS technology,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      ", map_join, "
      JOIN techmap t
        ON iv.docdb_family_id = t.docdb_family_id
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        ", map_filter, "
        AND t.technology != 'All'
        ", ff_sql, "
    ),
    base_all AS (
      SELECT DISTINCT
        'All' AS technology,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      ", map_join, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        ", map_filter, "
        ", ff_sql, "
    ),
    combined AS (
      SELECT * FROM base
      UNION ALL
      SELECT * FROM base_all
    )
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
    FROM combined
    GROUP BY technology
  ")

  result <- dbGetQuery(con, main_sql)

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

  # --- Top25/Top50 bin means + top3 appln_ids per technology ---
  extras_sql <- paste0("
    WITH base AS (
      SELECT DISTINCT
        ", tech_case, " AS technology,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      ", map_join, "
      JOIN techmap t
        ON iv.docdb_family_id = t.docdb_family_id
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        ", map_filter, "
        AND t.technology != 'All'
        ", ff_sql, "
    ),
    base_all AS (
      SELECT DISTINCT
        'All' AS technology,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      ", map_join, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        ", map_filter, "
        ", ff_sql, "
    ),
    combined AS (
      SELECT * FROM base
      UNION ALL
      SELECT * FROM base_all
    ),
    ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY technology ORDER BY val DESC) AS prank
      FROM combined
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
  ")

  extras <- dbGetQuery(con, extras_sql)

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
#' This is the SQL rewrite of compute_avstrax_for_techs(). It joins data,
#' filters by technology, groups by ctry_code, and computes mean/sem/quantiles/
#' top25/top50/top3_ids + RTA.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param tech_filter Character vector of technology names to filter by.
#'   If empty/NULL, includes all patents (no technology filter).
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @param use_regionmap Logical; if TRUE, uses regionmap instead of countrymap
#'   (for UK regions). Renames region_code -> ctry_code, region_name -> country_name.
#' @return Data frame with columns: ctry_code, [country_name], mean, innos, sem,
#'   q1, q2, q3, top25, top50, top25_bin_mean, top50_bin_mean, top3_ids,
#'   top3_ids_url, Allinnos, SumAllinnos, share_c, share, RTA
duck_compute_avstrax_for_techs <- function(con, flow_type, tech_filter = NULL,
                                            country_codes, firm_names = NULL,
                                            use_regionmap = FALSE) {

  scaler <- get_scaler(flow_type)
  ff_sql <- firm_filter_sql(firm_names)

  # Choose the map table and grouping columns
  if (use_regionmap) {
    map_table <- "regionmap"
    map_join <- "iv.docdb_family_id = rm.docdb_family_id AND iv.ctry_code = rm.ctry_code"
    group_col <- "rm.region_code"
    name_col <- "rm.region_name"
    country_filter <- paste0("rm.region_code IN ", sql_in_list(country_codes))
    appln_col <- "rm.appln_id"
    # For total counts (unfiltered by tech), use regionmap
    count_table <- "regionmap"
    count_group_col <- "region_code"
    count_filter <- paste0("region_code IN ", sql_in_list(country_codes))
  } else {
    map_table <- "countrymap"
    map_join <- "iv.docdb_family_id = cm.docdb_family_id AND iv.ctry_code = cm.ctry_code"
    group_col <- "cm.ctry_code"
    name_col <- "NULL"
    country_filter <- paste0("cm.ctry_code IN ", sql_in_list(country_codes))
    appln_col <- "cm.appln_id"
    count_table <- "countrymap"
    count_group_col <- "ctry_code"
    count_filter <- paste0("ctry_code IN ", sql_in_list(country_codes))
  }

  # Technology filter
  tech_join_sql <- ""
  if (!is.null(tech_filter) && length(tech_filter) > 0) {
    tech_join_sql <- paste0(
      " JOIN techmap t ON iv.docdb_family_id = t.docdb_family_id AND t.technology IN ",
      sql_in_list(tech_filter)
    )
  }

  # Alias for map table
  map_alias <- if (use_regionmap) "rm" else "cm"

  # --- Main aggregation: group by country ---
  main_sql <- paste0("
    WITH filtered AS (
      SELECT DISTINCT
        ", group_col, " AS ctry_code,
        ", name_col, " AS country_name,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      ", tech_join_sql, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    ),
    filtered_all AS (
      SELECT DISTINCT
        'All' AS ctry_code,
        'All' AS country_name,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      ", tech_join_sql, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    ),
    combined AS (
      SELECT * FROM filtered
      UNION ALL
      SELECT * FROM filtered_all
    )
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
    FROM combined
    GROUP BY ctry_code
  ")

  result <- dbGetQuery(con, main_sql)

  if (nrow(result) == 0) {
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

  # --- Top25/Top50 + top3_ids ---
  extras_sql <- paste0("
    WITH filtered AS (
      SELECT DISTINCT
        ", group_col, " AS ctry_code,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      ", tech_join_sql, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    ),
    filtered_all AS (
      SELECT DISTINCT
        'All' AS ctry_code,
        iv.docdb_family_id,
        ", appln_col, " AS appln_id,
        iv.value * ", scaler, " AS val
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      ", tech_join_sql, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql, "
    ),
    combined AS (
      SELECT * FROM filtered
      UNION ALL
      SELECT * FROM filtered_all
    ),
    ranked AS (
      SELECT *,
        PERCENT_RANK() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS prank,
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY val DESC) AS rn
      FROM combined
    )
    SELECT
      ctry_code,
      AVG(CASE WHEN prank < 0.25 THEN val END) AS top25_bin_mean,
      AVG(CASE WHEN prank < 0.50 THEN val END) AS top50_bin_mean,
      STRING_AGG(CASE WHEN rn <= 10 THEN appln_id END, ', ') AS top3_ids
    FROM ranked
    GROUP BY ctry_code
  ")

  extras <- dbGetQuery(con, extras_sql)
  result <- merge(result, extras, by = "ctry_code", all.x = TRUE)

  # --- Counted: total distinct patents per country (unfiltered by tech) ---
  counted_sql <- paste0("
    SELECT ", count_group_col, " AS ctry_code,
           COUNT(DISTINCT docdb_family_id) AS Allinnos
    FROM ", count_table, "
    WHERE ", count_filter, "
    GROUP BY ", count_group_col
  )
  counted <- dbGetQuery(con, counted_sql)

  # SumAllinnos: total distinct patents across all selected countries
  sum_all_sql <- paste0("
    SELECT COUNT(DISTINCT docdb_family_id) AS SumAllinnos
    FROM ", count_table, "
    WHERE ", count_filter
  )
  sum_all <- dbGetQuery(con, sum_all_sql)$SumAllinnos

  # Total filtered innovations (distinct patents matching tech filter)
  if (!is.null(tech_filter) && length(tech_filter) > 0) {
    total_filtered_sql <- paste0("
      SELECT COUNT(DISTINCT iv.docdb_family_id) AS total
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      ", tech_join_sql, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
        AND ", country_filter, "
        ", ff_sql
    )
  } else {
    total_filtered_sql <- paste0("
      SELECT COUNT(DISTINCT iv.docdb_family_id) AS total
      FROM istrax_values iv
      JOIN ", map_table, " ", map_alias, " ON ", map_join, "
      WHERE iv.flow_type = '", gsub("'", "''", flow_type), "'
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
#' Returns the joined istrax+countrymap data. Used as fallback when DuckDB
#' aggregation can't handle the specific filtering logic.
#'
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param country_codes Character vector of country codes
#' @param firm_names Character vector of company_raw names, or NULL
#' @return Data frame with columns: docdb_family_id, ctry_code, appln_id, {flow_type}
duck_get_istrax_joined <- function(con, flow_type, country_codes,
                                    firm_names = NULL) {

  ff_sql <- firm_filter_sql(firm_names)

  sql <- paste0("
    SELECT
      cm.docdb_family_id,
      cm.ctry_code,
      cm.appln_id,
      COALESCE(iv.value, 0) AS value
    FROM countrymap cm
    LEFT JOIN istrax_values iv
      ON cm.docdb_family_id = iv.docdb_family_id
      AND cm.ctry_code = iv.ctry_code
      AND iv.flow_type = '", gsub("'", "''", flow_type), "'
    WHERE cm.ctry_code IN ", sql_in_list(country_codes), "
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
#' @param con DuckDB connection
#' @param flow_type Flow type string
#' @param region_codes Character vector of region codes (e.g. "UKC", "UKD")
#' @param firm_names Character vector of company_raw names, or NULL
#' @return Data frame with columns matching patchar_regionmap format
duck_get_istrax_joined_regions <- function(con, flow_type, region_codes,
                                            firm_names = NULL) {

  ff_sql <- firm_filter_sql(firm_names)

  sql <- paste0("
    SELECT
      rm.docdb_family_id,
      rm.region_code AS ctry_code,
      rm.region_name AS country_name,
      rm.appln_id,
      COALESCE(iv.value, 0) AS value
    FROM regionmap rm
    LEFT JOIN istrax_values iv
      ON rm.docdb_family_id = iv.docdb_family_id
      AND rm.ctry_code = iv.ctry_code
      AND iv.flow_type = '", gsub("'", "''", flow_type), "'
    WHERE rm.region_code IN ", sql_in_list(region_codes), "
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
