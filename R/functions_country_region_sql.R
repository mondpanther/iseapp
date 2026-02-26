# SQL Query Functions for Country and Region Modules
# 
# These functions generate SQL queries for aggregating patent return flows
# by technology, country, and region. Extracted from module_country.R.

#' Generate SQL for RTA All Innovations Count
#'
#' Counts total DISTINCT patents per country across ALL technologies (no tech filter).
#' Used for RTA (Revealed Technological Advantage) calculation denominator.
#'
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
# sql_country_rta_allinnos <- function(country_sql, firm_clause) {
#   glue::glue("
#     SELECT 
#       ctry_code,
#       COUNT(DISTINCT docdb_family_id) as allinnos
#     FROM full_patent_database
#     WHERE ctry_code IN ({country_sql})
#       {firm_clause}
#     GROUP BY ctry_code
#   ")
# }

#' Generate SQL for RTA Sum All Innovations Count
#'
#' Counts total DISTINCT patents globally across all selected countries and ALL technologies.
#' Returns a single scalar value used as the global denominator for RTA calculations.
#'
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering
#'
#' @return Character. SQL query string
# sql_country_rta_sum_allinnos <- function(country_sql, firm_clause) {
#   glue::glue("
#     SELECT 
#       COUNT(DISTINCT docdb_family_id) as sum_allinnos
#     FROM full_patent_database
#     WHERE ctry_code IN ({country_sql})
#       {firm_clause}
#   ")
# }

#' Generate SQL base query for country/tech filtered patents
#'
#' Simple filtered SELECT returning raw rows for R-side aggregation.
#' Uses parquet predicate pushdown on ctry_code and tech_group for speed.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_clause Character. AND clause for tech_group filtering (may be empty string)
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
sql_country_base <- function(toflow, country_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT ctry_code, docdb_family_id, {toflow}
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      AND {toflow} IS NOT NULL
      {tech_clause}
      {firm_clause}
  ")
}

#' Generate SQL base query for tech filtered patents (by tech_filters list)
#'
#' Same as sql_country_base but accepts tech_filters named list for
#' mixed tech_group/technology filtering used in fallback_by_tech.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_filters Named list. label -> SQL filter clause from build_tech_filter()
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
sql_tech_base <- function(toflow, country_sql, tech_filters, firm_clause) {
  filter_clauses <- unlist(tech_filters)
  filter_clauses <- filter_clauses[nchar(trimws(filter_clauses)) > 0]
  
  tech_filter_sql <- if (length(filter_clauses) == 0) {
    ""
  } else {
    # Strip leading AND and combine with OR
    clauses <- gsub("^\\s*AND\\s*", "", filter_clauses)
    paste0("AND (", paste(clauses, collapse = " OR "), ")")
  }
  
  glue::glue("
    SELECT tech_group, technology, docdb_family_id, {toflow}
    FROM full_patent_database
    WHERE ctry_code IN ({country_sql})
      AND {toflow} IS NOT NULL
      {tech_filter_sql}
      {firm_clause}
  ")
}

#' Generate SQL base query for region/tech filtered patents
#'
#' Mirrors sql_country_base but filters on region_code.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param region_sql Character. Comma-separated quoted region codes
#' @param tech_clause Character. AND clause for tech_group filtering (may be empty string)
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
#' @export
sql_region_base <- function(toflow, region_sql, tech_clause, firm_clause) {
  glue::glue("
    SELECT region_code, docdb_family_id, {toflow}
    FROM full_patent_database
    WHERE region_code IN ({region_sql})
      AND {toflow} IS NOT NULL
      {tech_clause}
      {firm_clause}
  ")
}

#' Generate SQL base query for region tech filtered patents (by tech_filters list)
#'
#' Mirrors sql_tech_base but filters on region_code.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param region_sql Character. Comma-separated quoted region codes
#' @param tech_filters Named list. label -> SQL filter clause from build_tech_filter()
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
#' @export
sql_region_tech_base <- function(toflow, region_sql, tech_filters, firm_clause) {
  filter_clauses <- unlist(tech_filters)
  filter_clauses <- filter_clauses[nchar(trimws(filter_clauses)) > 0]

  tech_filter_sql <- if (length(filter_clauses) == 0) {
    ""
  } else {
    clauses <- gsub("^\\s*AND\\s*", "", filter_clauses)
    paste0("AND (", paste(clauses, collapse = " OR "), ")")
  }

  glue::glue("
    SELECT region_code, tech_group, technology, docdb_family_id, {toflow}
    FROM full_patent_database
    WHERE region_code IN ({region_sql})
      AND {toflow} IS NOT NULL
      {tech_filter_sql}
      {firm_clause}
  ")
}