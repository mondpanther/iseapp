# R/functions_data.R
# UI lookup objects (firm_choices, grouped_techs, grouped_choices, toflow_choices,
# uk_regions, region_choices, grouped_region_choices, group_definitions,
# region_group_definitions, colorings, tech_umbrella_map, etc.) are pre-computed
# at build time by data-raw/build_ui_data.R and stored in R/sysdata.rda.
# They are automatically available to all package functions — no explicit load needed.

#' Expand a country selection to individual ISO2 codes
#'
#' Converts predefined group names (e.g. "LMICs") to their constituent
#' ISO2 country codes. Individual codes are passed through unchanged.
#' @param selected Character vector of group names or ISO2 codes.
#' @return Character vector of unique ISO2 codes.
#' @export
expand_country_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(group_definitions)) group_definitions[[x]] else x
  }))
  unique(expanded)
}

#' Expand a region selection to individual NUTS1 codes
#'
#' Converts predefined region group names (e.g. "All UK regions") to their
#' constituent NUTS1 codes. Individual codes are passed through unchanged.
#' @param selected Character vector of group names or NUTS1 codes.
#' @return Character vector of unique NUTS1 codes.
#' @export
expand_region_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(region_group_definitions)) region_group_definitions[[x]] else x
  }))
  unique(expanded)
}

#' Expand a firm selection to individual firm names
#'
#' Converts sector group names to their constituent firm names.
#' Individual firm names are passed through unchanged.
#' @param selected Character vector of sector names or firm names.
#' @return Character vector of unique firm names.
#' @export
expand_firm_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(firm_sector_groups)) firm_sector_groups[[x]] else x
  }))
  unique(expanded)
}

#' Map firm or sector-group labels to their ICB sector name
#'
#' Used by the "Value flow by firm" tab to colour bars by sector. When the
#' label is already a sector group (matches a name in
#' \code{firm_sector_groups}), it is its own sector. Individual firms are
#' looked up in the inverted mapping built from \code{firm_sector_groups}.
#' Unmatched / unsectored firms (sysdata occasionally has NA \code{firm_sector})
#' get \code{"Other"}.
#'
#' @param labels Character vector of bar labels (firm or sector names).
#' @return Character vector of the same length, holding sector names.
#' @export
firm_to_sector <- function(labels) {
  if (!exists("firm_sector_groups", inherits = TRUE))
    return(rep("Other", length(labels)))
  # Invert sector -> firm-list into firm -> sector. firm_sector_groups is
  # a list whose elements are themselves named lists (firm = firm).
  firm_to_sec <- unlist(lapply(names(firm_sector_groups), function(s) {
    setNames(rep(s, length(firm_sector_groups[[s]])),
             unlist(firm_sector_groups[[s]], use.names = FALSE))
  }))
  vapply(labels, function(x) {
    if (x %in% names(firm_sector_groups)) x
    else if (x %in% names(firm_to_sec))   unname(firm_to_sec[[x]])
    else                                  "Other"
  }, character(1), USE.NAMES = FALSE)
}

#' Get display name for a NUTS1 region code
#'
#' @param code A NUTS1 region code (e.g. "UKI").
#' @return The display name (e.g. "London"), or the code itself if not found.
#' @export
get_region_name <- function(code) {
  if (code %in% names(uk_regions)) uk_regions[[code]] else code
}

#' Build a SQL tech_group WHERE clause from a UI selection
#'
#' Returns an empty string if all technologies selected.
#' @param selected Character vector of technology names from the UI.
#' @return Character. SQL AND clause fragment, or empty string.
#' @export
build_tech_clause <- function(selected) {
  if ("All" %in% selected || length(selected) == 0) return("")
  tech_sql <- paste0("'", selected, "'", collapse = ", ")
  glue::glue("AND tech_group IN ({tech_sql})")
}

#' Build a named list of SQL technology filter clauses
#'
#' Returns one filter clause per selected technology. Sub-technologies
#' filter on the \code{technology} column directly; umbrella groups and
#' novel technologies filter on \code{tech_group}.
#' @param selected Character vector of technology names from the UI.
#' @return Named list of SQL AND clause fragments, one per selection.
#' @export
build_tech_filter <- function(selected) {
  if ("All" %in% selected || length(selected) == 0) return(list("All" = ""))
  
  lapply(setNames(selected, selected), function(t) {
    if (t %in% names(tech_umbrella_map)) {
      # sub-technology — filter on technology column directly
      glue::glue("AND technology = '{t}'")
    } else {
      # umbrella group or novel tech — filter on tech_group column
      glue::glue("AND tech_group = '{t}'")
    }
  })
}

#' Build a boolean SQL tech_group expression (no leading AND)
#' For use inside CASE WHEN expressions
#' @param selected Character vector of technology names from the UI.
#' @return Character. Boolean SQL expression string.
build_tech_bool <- function(selected) {
  if ("All" %in% selected || length(selected) == 0) return("TRUE")
  tech_sql <- paste0("'", selected, "'", collapse = ", ")
  glue::glue("tech_group IN ({tech_sql})")
}

# SQL Query Functions for Country and Region Modules
# 
# These functions generate SQL queries for aggregating patent return flows
# by technology, country, and region. Extracted from module_country.R.

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

sql_country_tech_combined <- function(
  toflow,
  country_sql,
  tech_filters,
  firm_clause = ""
) {

  firm_clause <- if (firm_clause != "") paste0(" ", firm_clause) else ""

  # Extract selected tech group names from filters
  tech_names <- names(tech_filters)

  # Build IN clause from names
  tech_sql <- paste0("'", tech_names, "'", collapse = ", ")

  glue::glue("
    WITH base AS (
      SELECT
        tech_group AS technology,
        docdb_family_id,
        {toflow}
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        AND tech_group IN ({tech_sql})
        {firm_clause}
    ),

    ranked AS (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY technology
          ORDER BY {toflow} DESC
        ) AS rnk,
        COUNT(*) OVER (
          PARTITION BY technology
        ) AS cnt
      FROM base
    )

    SELECT
      technology,
      AVG({toflow}) AS mean,
      COUNT(*)      AS innos,
      CASE WHEN COUNT(*) > 1
           THEN STDDEV({toflow}) / SQRT(COUNT(*))
      END AS sem,

      quantile_cont({toflow}, 0.25) AS q1,
      quantile_cont({toflow}, 0.50) AS q2,
      quantile_cont({toflow}, 0.75) AS q3,

      AVG(
        CASE WHEN rnk <= CEILING(cnt * 0.25)
             THEN {toflow}
        END
      ) AS top25_bin_mean,

      AVG(
        CASE WHEN rnk <= CEILING(cnt * 0.50)
             THEN {toflow}
        END
      ) AS top50_bin_mean,

      string_agg(
        CASE WHEN rnk <= 3
             THEN CAST(docdb_family_id AS VARCHAR)
        END,
        ', '
      ) AS top3_ids

    FROM ranked
    GROUP BY technology
  ")
}

sql_country_combined <- function(toflow, country_sql, techs, firm_clause) {

  tech_bool   <- build_tech_bool(techs)
  tech_clause <- build_tech_clause(techs)

  glue::glue("
    WITH ranked AS (
      SELECT
        ctry_code,
        docdb_family_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY {toflow} DESC) AS rnk_c,
        COUNT(*)     OVER (PARTITION BY ctry_code)                        AS cnt_c,
        ROW_NUMBER() OVER (ORDER BY {toflow} DESC)                        AS rnk_all,
        COUNT(*)     OVER ()                                              AS cnt_all
      FROM full_patent_database
      WHERE ctry_code IN ({country_sql})
        AND {toflow} IS NOT NULL
        {tech_clause}
        {firm_clause}
    ),

    summary AS (
      SELECT
        ctry_code,
        AVG({toflow})                                                          AS mean,
        COUNT(*)                                                               AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END     AS sem,
        QUANTILE_CONT({toflow}, 0.25)                                          AS q1,
        QUANTILE_CONT({toflow}, 0.50)                                          AS q2,
        QUANTILE_CONT({toflow}, 0.75)                                          AS q3,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.25) THEN {toflow} END)          AS top25_bin_mean,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.50) THEN {toflow} END)          AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk_c <= 3 THEN CAST(docdb_family_id AS VARCHAR) END,
          ', ' ORDER BY {toflow} DESC
        )                                                                      AS top3_ids
      FROM ranked
      GROUP BY ctry_code
    ),

    overall AS (
      SELECT
        'All'                                                                  AS ctry_code,
        AVG({toflow})                                                          AS mean,
        COUNT(*)                                                               AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END     AS sem,
        QUANTILE_CONT({toflow}, 0.25)                                          AS q1,
        QUANTILE_CONT({toflow}, 0.50)                                          AS q2,
        QUANTILE_CONT({toflow}, 0.75)                                          AS q3,
        AVG(CASE WHEN rnk_all <= CEIL(cnt_all * 0.25) THEN {toflow} END)      AS top25_bin_mean,
        AVG(CASE WHEN rnk_all <= CEIL(cnt_all * 0.50) THEN {toflow} END)      AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk_all <= 3 THEN CAST(docdb_family_id AS VARCHAR) END,
          ', ' ORDER BY {toflow} DESC
        )                                                                      AS top3_ids
      FROM ranked
      GROUP BY cnt_all
    )

    SELECT * FROM summary
    UNION ALL
    SELECT * FROM overall
  ")
}