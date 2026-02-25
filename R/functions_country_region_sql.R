# SQL Query Functions for Country and Region Modules
# 
# These functions generate SQL queries for aggregating patent return flows
# by technology, country, and region. Extracted from module_country.R.

#' Generate SQL for Technology Aggregation
#'
#' Creates a SQL query that aggregates return flow metrics by technology category.
#' Calculates mean, count, standard error, and quartiles for each technology.
#'
#' @param toflow Character. Column name for the return flow measure (e.g., "istrax_global")
#' @param country_sql Character. Comma-separated quoted country codes (e.g., "'US', 'GB'")
#' @param firm_clause Character. AND clause for firm filtering (e.g., "AND firm = 'Toyota'" or "")
#'
#' @return Character. SQL query string
sql_country_tech_aggregation <- function(toflow, country_sql, firm_clause) {
  glue::glue("
    SELECT 
      technology,
      AVG({toflow}) as mean,
      COUNT(*) as innos,
      CASE 
        WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*))
        ELSE NULL 
      END as sem,
      QUANTILE_CONT({toflow}, 0.25) as q1,
      QUANTILE_CONT({toflow}, 0.50) as q2,
      QUANTILE_CONT({toflow}, 0.75) as q3
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      AND technology IS NOT NULL
      AND {toflow} IS NOT NULL
      {firm_clause}
    GROUP BY technology
  ")
}

#' Generate SQL for Technology Percentile Bins
#'
#' Calculates mean returns for top 25% and top 50% patents within each technology.
#' Uses ranked partitions to identify top performers.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_tech_percentile_bins <- function(toflow, country_sql, firm_clause) {
  glue::glue("
    SELECT 
      technology,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN {toflow} END) as top25_bin_mean,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN {toflow} END) as top50_bin_mean
    FROM (
      SELECT 
        technology,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) as rnk,
        COUNT(*) OVER (PARTITION BY technology) as cnt
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND technology IS NOT NULL
        AND {toflow} IS NOT NULL
        {firm_clause}
    ) sub
    GROUP BY technology
  ")
}

#' Generate SQL for Top 3 Patents by Technology
#'
#' Finds the top 3 highest-performing patent IDs for each technology category.
#' Results are typically post-processed in R to concatenate patent IDs.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_tech_top_patents <- function(toflow, country_sql, firm_clause) {
  glue::glue("
    SELECT 
      technology,
      docdb_family_id,
      {toflow} as value
    FROM (
      SELECT 
        technology,
        docdb_family_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) as rnk
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND technology IS NOT NULL
        AND {toflow} IS NOT NULL
        {firm_clause}
    ) sub
    WHERE rnk <= 3
  ")
}

#' Generate SQL for Technology Filter Clause
#'
#' Creates an optional WHERE clause for filtering by technology categories.
#' Used when "All Innovations" is NOT selected.
#'
#' @param tech_sql Character. Comma-separated quoted technology names
#'
#' @return Character. SQL WHERE clause fragment
sql_country_tech_filter_clause <- function(tech_sql) {
  glue::glue("AND technology IN ({tech_sql})")
}

#' Generate SQL for Country Aggregation
#'
#' Creates a SQL query that aggregates return flow metrics by country.
#' Calculates mean, count, standard error, and quartiles for each country.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering (may be empty string)
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
sql_country_country_aggregation <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      ctry_code,
      AVG({toflow}) as mean,
      COUNT(*) as innos,
      CASE 
        WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*))
        ELSE NULL 
      END as sem,
      QUANTILE_CONT({toflow}, 0.25) as q1,
      QUANTILE_CONT({toflow}, 0.50) as q2,
      QUANTILE_CONT({toflow}, 0.75) as q3
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      AND {toflow} IS NOT NULL
      {tech_clause}
      {firm_clause}
    GROUP BY ctry_code
  ")
}

#' Generate SQL for All Countries Aggregation
#'
#' Calculates global aggregate metrics across all selected countries (not grouped by country).
#' Returns a single row with ctry_code = 'All' representing the overall average.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_all_countries_aggregation <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      'All' as ctry_code,
      AVG({toflow}) as mean,
      COUNT(*) as innos,
      CASE 
        WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*))
        ELSE NULL 
      END as sem,
      QUANTILE_CONT({toflow}, 0.25) as q1,
      QUANTILE_CONT({toflow}, 0.50) as q2,
      QUANTILE_CONT({toflow}, 0.75) as q3
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      AND {toflow} IS NOT NULL
      {tech_clause}
      {firm_clause}
  ")
}

#' Generate SQL for Country Percentile Bins
#'
#' Calculates mean returns for top 25% and top 50% patents within each country.
#' Identifies top-performing patents partitioned by country.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_country_percentile_bins <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      ctry_code,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN {toflow} END) as top25_bin_mean,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN {toflow} END) as top50_bin_mean
    FROM (
      SELECT 
        ctry_code,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY {toflow} DESC) as rnk,
        COUNT(*) OVER (PARTITION BY ctry_code) as cnt
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        {tech_clause}
        {firm_clause}
    ) sub
    GROUP BY ctry_code
  ")
}

#' Generate SQL for All Countries Percentile Bins
#'
#' Calculates top 25% and top 50% bin means across all selected countries (no partition).
#' Returns a single row with ctry_code = 'All' for global top percentile bins.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_all_countries_percentile_bins <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      'All' as ctry_code,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN {toflow} END) as top25_bin_mean,
      AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN {toflow} END) as top50_bin_mean
    FROM (
      SELECT 
        {toflow},
        ROW_NUMBER() OVER (ORDER BY {toflow} DESC) as rnk,
        COUNT(*) OVER () as cnt
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        {tech_clause}
        {firm_clause}
    ) sub
  ")
}

#' Generate SQL for Top 3 Patents by Country
#'
#' Finds the top 3 highest-performing patents for each country.
#' Results are typically post-processed in R to concatenate patent IDs.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_country_top_patents <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      ctry_code,
      docdb_family_id
    FROM (
      SELECT 
        ctry_code,
        docdb_family_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY {toflow} DESC) as rnk
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        {tech_clause}
        {firm_clause}
    ) sub
    WHERE rnk <= 3
  ")
}

#' Generate SQL for Top 3 Patents Across All Countries
#'
#' Finds the top 3 highest-performing patents globally across all selected countries.
#' Returns a single result set (no grouping by country).
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. Optional AND clause for technology filtering
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_all_countries_top_patents <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT 
      docdb_family_id
    FROM (
      SELECT 
        docdb_family_id,
        {toflow},
        ROW_NUMBER() OVER (ORDER BY {toflow} DESC) as rnk
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        {tech_clause}
        {firm_clause}
    ) sub
    WHERE rnk <= 3
  ")
}

#' Generate SQL for RTA All Innovations Count
#'
#' Counts total DISTINCT patents per country across ALL technologies (no tech filter).
#' Used for RTA (Revealed Technological Advantage) calculation denominator.
#'
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_rta_allinnos <- function(country_sql, firm_clause) {
  glue::glue("
    SELECT 
      ctry_code,
      COUNT(DISTINCT docdb_family_id) as allinnos
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      {firm_clause}
    GROUP BY ctry_code
  ")
}

#' Generate SQL for RTA Sum All Innovations Count
#'
#' Counts total DISTINCT patents globally across all selected countries and ALL technologies.
#' Returns a single scalar value used as the global denominator for RTA calculations.
#'
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
sql_country_rta_sum_allinnos <- function(country_sql, firm_clause) {
  glue::glue("
    SELECT 
      COUNT(DISTINCT docdb_family_id) as sum_allinnos
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      {firm_clause}
  ")
}