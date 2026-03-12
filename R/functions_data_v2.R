#' Build a SQL tech_group WHERE clause with table alias (v2)
#'
#' References t.tech_group from the patents_x_tech bridge JOIN.
#' Returns an empty string if all technologies selected.
#' @param selected Character vector of technology names from the UI.
#' @return Character. SQL AND clause fragment, or empty string.
build_tech_clause_v2 <- function(selected) {
  if ("All" %in% selected || length(selected) == 0) return("")
  tech_sql <- paste0("'", selected, "'", collapse = ", ")
  glue::glue("AND tl.tech_group IN ({tech_sql})")
}

#' Build a named list of SQL technology filter clauses with table aliases (v2)
#'
#' Sub-technologies filter on t.technology; umbrella groups and novel
#' technologies filter on t.tech_group.
#' @param selected Character vector of technology names from the UI.
#' @return Named list of SQL AND clause fragments, one per selection.
build_tech_filter_v2 <- function(selected) {
  # Strip "All categories" — it's a UI-only shortcut expanded by the server
  selected <- setdiff(selected, "All categories")

  if (length(selected) == 0) return(list("All innovations" = ""))

  # "All innovations" is just another selection alongside others
  # It gets an empty filter clause (handled specially in the SQL builder)
  lapply(setNames(selected, selected), function(t) {
    if (t == "All innovations") return("")
    glue::glue(
      "AND (tl.tech_group = '{t}' OR t.technology = '{t}')"
    )
  })
}

#' Build a boolean SQL tech_group expression with table alias (v2)
#'
#' For use inside CASE WHEN expressions. References t.tech_group.
#' @param selected Character vector of technology names from the UI.
#' @return Character. Boolean SQL expression string.
build_tech_bool_v2 <- function(selected) {
  selected <- setdiff(selected, "All categories")
  if ("All innovations" %in% selected || length(selected) == 0) return("TRUE")
  tech_sql <- paste0("'", selected, "'", collapse = ", ")
  glue::glue("tl.tech_group IN ({tech_sql})")
}

#' Build a SQL firm WHERE clause with table alias (v2)
#'
#' Accepts a character vector of firm names and generates an IN clause.
#' When no_filter is TRUE or firms is empty, returns empty string (no filtering).
#' @param firms Character vector. Firm names from the treeInput.
#' @param no_filter Logical. If TRUE, skip firm filtering entirely.
#' @return Character. SQL AND clause fragment, or empty string.
build_firm_clause_v2 <- function(firms, no_filter = TRUE) {
  if (no_filter || is.null(firms) || length(firms) == 0) return("")
  firms_sql <- paste0("'", gsub("'", "''", firms), "'", collapse = ", ")
  glue::glue("AND f.firm IN ({firms_sql})")
}

#' Generate SQL combined query for country aggregation (v2)
#'
#' Joins slim bridge parquets for tech and firm at query time.
#' Aggregates per ctry_code plus an overall 'All' row.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param techs Character vector. Technology selection from UI
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
sql_country_combined_v2 <- function(toflow, country_sql, techs, firm_clause, top_n_ids = 10) {

  tech_bool <- build_tech_bool_v2(techs)

  # Build WHERE clause for tech filtering
  tech_filter_sql <- if (tech_bool == "TRUE") {
    ""
  } else {
    paste0("WHERE ", tech_bool)
  }
  
  # Build WHERE clause for firm filtering
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
    paste0("WHERE ", firm_condition)
  }

  glue::glue("
    {if (tech_bool != 'TRUE') '
    WITH filtered_tech AS (
      SELECT DISTINCT t.docdb_family_id
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
      ' else ''}
    {if (tech_bool != 'TRUE') tech_filter_sql else ''}
    {if (tech_bool != 'TRUE') '),' else 'WITH'}

    {if (nchar(trimws(firm_clause)) > 0) '
    filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      ' else ''}
    {if (nchar(trimws(firm_clause)) > 0) firm_filter_sql else ''}
    {if (nchar(trimws(firm_clause)) > 0) '),' else ''}

    ranked AS (
      SELECT
        p.ctry_code,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow},
        ROW_NUMBER() OVER (PARTITION BY p.ctry_code ORDER BY p.{toflow} DESC) AS rnk_c,
        COUNT(*)     OVER (PARTITION BY p.ctry_code)                          AS cnt_c
      FROM full_patent_database p
      {if (tech_bool != 'TRUE') 'INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id' else ''}
      {if (nchar(trimws(firm_clause)) > 0) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
      WHERE p.ctry_code IN ({country_sql})
        AND p.{toflow} IS NOT NULL
    ),

    -- Deduplicate across countries: one row per distinct patent
    deduped_all AS (
      SELECT DISTINCT ON (docdb_family_id) docdb_family_id, appln_id, {toflow}
      FROM ranked
    ),

    deduped_all_ranked AS (
      SELECT
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER ()                        AS cnt
      FROM deduped_all
    ),

    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS overall_allinnos
      FROM deduped_all
    ),

    summary AS (
      SELECT
        ctry_code,
        AVG({toflow}) AS mean,
        COUNT(*) AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END AS sem,
        QUANTILE_CONT({toflow}, 0.25) AS q1,
        QUANTILE_CONT({toflow}, 0.50) AS q2,
        QUANTILE_CONT({toflow}, 0.75) AS q3,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.25) THEN {toflow} END) AS top25_bin_mean,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.50) THEN {toflow} END) AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk_c <= {top_n_ids} THEN appln_id END,
          ', ' ORDER BY {toflow} DESC
        ) AS top3_ids,
        os.allmean,
        os.overall_allinnos
      FROM ranked
      CROSS JOIN overall_stats os
      GROUP BY ctry_code, os.allmean, os.overall_allinnos
    ),

    overall AS (
      SELECT
        'All' AS ctry_code,
        AVG({toflow}) AS mean,
        COUNT(*) AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END AS sem,
        QUANTILE_CONT({toflow}, 0.25) AS q1,
        QUANTILE_CONT({toflow}, 0.50) AS q2,
        QUANTILE_CONT({toflow}, 0.75) AS q3,
        AVG(CASE WHEN rnk <= CEIL(cnt * 0.25) THEN {toflow} END) AS top25_bin_mean,
        AVG(CASE WHEN rnk <= CEIL(cnt * 0.50) THEN {toflow} END) AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk <= {top_n_ids} THEN appln_id END,
          ', ' ORDER BY {toflow} DESC
        ) AS top3_ids,
        os.allmean,
        os.overall_allinnos
      FROM deduped_all_ranked
      CROSS JOIN overall_stats os
      GROUP BY cnt, os.allmean, os.overall_allinnos
    )

    SELECT * FROM summary
    UNION ALL
    SELECT * FROM overall
  ")
}

#' Generate SQL combined query for country tech aggregation (v2)
#'
#' Joins slim bridge parquets for tech and firm at query time.
#' Aggregates per ctry_code x tech_group x technology plus an overall 'All' row.
#'
#' @param toflow Character. Column name for the return flow measure
#' @param country_sql Character. Comma-separated quoted country codes
#' @param tech_filters Named list. label -> SQL filter clause from build_tech_filter()
#' @param firm_clause Character. AND clause for firm filtering (may be empty string)
#'
#' @return Character. SQL query string
sql_country_tech_combined_v2 <- function(toflow, country_sql, tech_filters, firm_clause, top_n_ids = 10) {

  filter_clauses <- unlist(tech_filters)
  filter_clauses <- filter_clauses[nchar(trimws(filter_clauses)) > 0]

  # Build WHERE clause for firm filtering within the CTE
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+f\\.firm", "firm", firm_clause)
    paste0("WHERE ", firm_condition)
  }

  firm_join <- if (nchar(trimws(firm_clause)) > 0) {
    "INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id"
  } else {
    ""
  }

  # Build filtered_tech CTE:
  # "All innovations" = all patents regardless of tech mapping (single bar)
  # Specific selections = UNION ALL so each gets its own bar
  has_all_innovations <- "All innovations" %in% names(tech_filters)

  # Collect parts for specific technology selections
  selected_names <- names(tech_filters)
  selected_names <- selected_names[!selected_names %in% c("All categories", "All innovations")]

  parts <- character(0)

  # "All innovations" part: all docdb_family_ids, no tech join

  if (has_all_innovations) {
    parts <- c(parts, "
      SELECT DISTINCT docdb_family_id, 'All innovations' AS technology
      FROM full_patent_database
    ")
  }

  # Individual technology parts
  if (length(selected_names) > 0) {
    tech_parts <- vapply(selected_names, function(s) {
      glue::glue("
        SELECT DISTINCT t.docdb_family_id, '{s}' AS technology
        FROM patents_x_tech t
        JOIN tech_lookup tl ON t.technology = tl.technology
        WHERE tl.tech_group = '{s}' OR t.technology = '{s}'
      ")
    }, character(1))
    parts <- c(parts, tech_parts)
  }

  # Fallback: no specific selections and no All innovations (shouldn't happen)
  if (length(parts) == 0) {
    parts <- "
      SELECT DISTINCT t.docdb_family_id, tl.tech_group AS technology
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
    "
  }

  filtered_tech_sql <- paste(parts, collapse = "\nUNION ALL\n")

  glue::glue("
    WITH filtered_tech AS (
      {filtered_tech_sql}
    ),

    filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      {firm_filter_sql}
    ),

    deduped AS (
      SELECT DISTINCT ON (ft.technology, p.docdb_family_id)
        ft.technology,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow}
      FROM full_patent_database p
      INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id
      {firm_join}
      WHERE p.ctry_code IN ({country_sql})
        AND p.{toflow} IS NOT NULL
    ),

    ranked AS (
      SELECT
        technology,
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER (PARTITION BY technology)                        AS cnt
      FROM deduped
    ),

    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS allinnos
      FROM (
        SELECT DISTINCT ON (p.docdb_family_id) p.docdb_family_id, p.{toflow}
        FROM full_patent_database p
        {firm_join}
        WHERE p.ctry_code IN ({country_sql})
          AND p.{toflow} IS NOT NULL
      ) t
    )

    SELECT
      technology,
      AVG({toflow})                                                          AS mean,
      COUNT(*)                                                               AS innos,
      CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END     AS sem,
      QUANTILE_CONT({toflow}, 0.25)                                          AS q1,
      QUANTILE_CONT({toflow}, 0.50)                                          AS q2,
      QUANTILE_CONT({toflow}, 0.75)                                          AS q3,
      AVG(CASE WHEN rnk <= CEIL(cnt * 0.25) THEN {toflow} END)              AS top25_bin_mean,
      AVG(CASE WHEN rnk <= CEIL(cnt * 0.50) THEN {toflow} END)              AS top50_bin_mean,
      STRING_AGG(
        CASE WHEN rnk <= {top_n_ids} THEN appln_id END,
        ', ' ORDER BY {toflow} DESC
      )                                                                      AS top3_ids,
      os.allmean,
      os.allinnos
    FROM ranked
    CROSS JOIN overall_stats os
    GROUP BY technology, os.allmean, os.allinnos
  ")
}

#' Generate SQL combined query for region aggregation (v2)
#'
#' Mirrors sql_country_combined_v2 but filters and partitions on
#' region_code via the patents_x_region bridge JOIN.
#' Returns per-region aggregates plus an overall 'All' row.
#' RTA fields are computed in R after this query via a left_join
#' to allinnos_region_baseline.
#'
#' @param toflow Character. Column name for the return flow measure.
#' @param region_sql Character. Comma-separated quoted region codes.
#' @param techs Character vector. Technology selection from the UI.
#' @param firm_clause Character. AND clause from build_firm_clause_v2().
#' @return Character. SQL query string.
sql_region_combined_v2 <- function(toflow, region_sql, techs, firm_clause, top_n_ids = 10) {

  tech_bool <- build_tech_bool_v2(techs)

  # Build WHERE clause for tech filtering
  tech_filter_sql <- if (tech_bool == "TRUE") {
    ""
  } else {
    paste0("WHERE ", tech_bool)
  }
  
  # Build WHERE clause for firm filtering
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
    paste0("WHERE ", firm_condition)
  }

  glue::glue("
    {if (tech_bool != 'TRUE') '
    WITH filtered_tech AS (
      SELECT DISTINCT t.docdb_family_id
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
      ' else ''}
    {if (tech_bool != 'TRUE') tech_filter_sql else ''}
    {if (tech_bool != 'TRUE') '),' else 'WITH'}

    {if (nchar(trimws(firm_clause)) > 0) '
    filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      ' else ''}
    {if (nchar(trimws(firm_clause)) > 0) firm_filter_sql else ''}
    {if (nchar(trimws(firm_clause)) > 0) '),' else ''}

    ranked AS (
      SELECT
        r.region_code,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow},
        ROW_NUMBER() OVER (PARTITION BY r.region_code ORDER BY p.{toflow} DESC) AS rnk_c,
        COUNT(*)     OVER (PARTITION BY r.region_code)                          AS cnt_c
      FROM full_patent_database p
      INNER JOIN patents_x_region r ON p.docdb_family_id = r.docdb_family_id
      {if (tech_bool != 'TRUE') 'INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id' else ''}
      {if (nchar(trimws(firm_clause)) > 0) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
      WHERE r.region_code IN ({region_sql})
        AND p.ctry_code = 'GB'
        AND p.{toflow} IS NOT NULL
    ),

    -- Deduplicate across regions: one row per distinct patent
    deduped_all AS (
      SELECT DISTINCT ON (docdb_family_id) docdb_family_id, appln_id, {toflow}
      FROM ranked
    ),

    deduped_all_ranked AS (
      SELECT
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER ()                        AS cnt
      FROM deduped_all
    ),

    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS overall_allinnos
      FROM deduped_all
    ),

    summary AS (
      SELECT
        region_code,
        AVG({toflow})                                                              AS mean,
        COUNT(*)                                                                   AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END         AS sem,
        QUANTILE_CONT({toflow}, 0.25)                                              AS q1,
        QUANTILE_CONT({toflow}, 0.50)                                              AS q2,
        QUANTILE_CONT({toflow}, 0.75)                                              AS q3,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.25) THEN {toflow} END)              AS top25_bin_mean,
        AVG(CASE WHEN rnk_c <= CEIL(cnt_c * 0.50) THEN {toflow} END)              AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk_c <= {top_n_ids} THEN appln_id END,
          ', ' ORDER BY {toflow} DESC
        )                                                                          AS top3_ids,
        os.allmean,
        os.overall_allinnos
      FROM ranked
      CROSS JOIN overall_stats os
      GROUP BY region_code, os.allmean, os.overall_allinnos
    ),

    overall AS (
      SELECT
        'All'                                                                      AS region_code,
        AVG({toflow})                                                              AS mean,
        COUNT(*)                                                                   AS innos,
        CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END         AS sem,
        QUANTILE_CONT({toflow}, 0.25)                                              AS q1,
        QUANTILE_CONT({toflow}, 0.50)                                              AS q2,
        QUANTILE_CONT({toflow}, 0.75)                                              AS q3,
        AVG(CASE WHEN rnk <= CEIL(cnt * 0.25) THEN {toflow} END)                  AS top25_bin_mean,
        AVG(CASE WHEN rnk <= CEIL(cnt * 0.50) THEN {toflow} END)                  AS top50_bin_mean,
        STRING_AGG(
          CASE WHEN rnk <= {top_n_ids} THEN appln_id END,
          ', ' ORDER BY {toflow} DESC
        )                                                                          AS top3_ids,
        os.allmean,
        os.overall_allinnos
      FROM deduped_all_ranked
      CROSS JOIN overall_stats os
      GROUP BY cnt, os.allmean, os.overall_allinnos
    )

    SELECT * FROM summary
    UNION ALL
    SELECT * FROM overall
  ")
}

#' Generate SQL combined query for region x technology aggregation (v2)
#'
#' Mirrors sql_country_tech_combined_v2 but filters and partitions on
#' region_code via the patents_x_region bridge JOIN.
#' Aggregates per region_code x tech_group x technology plus an overall 'All' row.
#'
#' @param toflow Character. Column name for the return flow measure.
#' @param region_sql Character. Comma-separated quoted region codes.
#' @param tech_filters Named list from build_tech_filter_v2().
#' @param firm_clause Character. AND clause from build_firm_clause_v2().
#' @return Character. SQL query string.
sql_region_tech_combined_v2 <- function(toflow, region_sql, tech_filters, firm_clause, top_n_ids = 10) {

  filter_clauses <- unlist(tech_filters)
  filter_clauses <- filter_clauses[nchar(trimws(filter_clauses)) > 0]

  # Build WHERE clause for firm filtering within the CTE
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
    paste0("WHERE ", firm_condition)
  }

  firm_join <- if (nchar(trimws(firm_clause)) > 0) {
    "INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id"
  } else {
    ""
  }

  # Build filtered_tech CTE:
  # "All innovations" = all patents regardless of tech mapping (single bar)
  # Specific selections = UNION ALL so each gets its own bar
  has_all_innovations <- "All innovations" %in% names(tech_filters)

  selected_names <- names(tech_filters)
  selected_names <- selected_names[!selected_names %in% c("All categories", "All innovations")]

  parts <- character(0)

  if (has_all_innovations) {
    parts <- c(parts, "
      SELECT DISTINCT docdb_family_id, 'All innovations' AS technology
      FROM full_patent_database
    ")
  }

  if (length(selected_names) > 0) {
    tech_parts <- vapply(selected_names, function(s) {
      glue::glue("
        SELECT DISTINCT t.docdb_family_id, '{s}' AS technology
        FROM patents_x_tech t
        JOIN tech_lookup tl ON t.technology = tl.technology
        WHERE tl.tech_group = '{s}' OR t.technology = '{s}'
      ")
    }, character(1))
    parts <- c(parts, tech_parts)
  }

  if (length(parts) == 0) {
    parts <- "
      SELECT DISTINCT t.docdb_family_id, tl.tech_group AS technology
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
    "
  }

  filtered_tech_sql <- paste(parts, collapse = "\nUNION ALL\n")

  glue::glue("
    WITH filtered_tech AS (
      {filtered_tech_sql}
    ),

    filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      {firm_filter_sql}
    ),

    deduped AS (
      SELECT DISTINCT ON (ft.technology, p.docdb_family_id)
        ft.technology,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow}
      FROM full_patent_database p
      INNER JOIN patents_x_region r ON p.docdb_family_id = r.docdb_family_id
      INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id
      {firm_join}
      WHERE r.region_code IN ({region_sql})
        AND p.ctry_code = 'GB'
        AND p.{toflow} IS NOT NULL
    ),

    ranked AS (
      SELECT
        technology,
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER (PARTITION BY technology)                        AS cnt
      FROM deduped
    ),

    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS allinnos
      FROM (
        SELECT DISTINCT ON (p.docdb_family_id) p.docdb_family_id, p.{toflow}
        FROM full_patent_database p
        INNER JOIN patents_x_region r ON p.docdb_family_id = r.docdb_family_id
        {firm_join}
        WHERE r.region_code IN ({region_sql})
          AND p.ctry_code = 'GB'
          AND p.{toflow} IS NOT NULL
      ) t
    )

    SELECT
      technology,
      AVG({toflow})                                                          AS mean,
      COUNT(*)                                                               AS innos,
      CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) END     AS sem,
      QUANTILE_CONT({toflow}, 0.25)                                          AS q1,
      QUANTILE_CONT({toflow}, 0.50)                                          AS q2,
      QUANTILE_CONT({toflow}, 0.75)                                          AS q3,
      AVG(CASE WHEN rnk <= CEIL(cnt * 0.25) THEN {toflow} END)              AS top25_bin_mean,
      AVG(CASE WHEN rnk <= CEIL(cnt * 0.50) THEN {toflow} END)              AS top50_bin_mean,
      STRING_AGG(
        CASE WHEN rnk <= {top_n_ids} THEN appln_id END,
        ', ' ORDER BY {toflow} DESC
      )                                                                      AS top3_ids,
      os.allmean,
      os.allinnos
    FROM ranked
    CROSS JOIN overall_stats os
    GROUP BY technology, os.allmean, os.allinnos
  ")
}