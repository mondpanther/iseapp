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
  glue::glue("(tl.tech_group IN ({tech_sql}) OR t.technology IN ({tech_sql}))")
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

#' Build a SQL clause restricting to granted families
#'
#' When \code{granted_only} is TRUE, returns the AND clause used in the main
#' WHERE to keep only docdbs with \code{granted = TRUE} (the per-family
#' boolean attached at build time — TRUE if any appln in the family was
#' granted per patbis.fromPATSTAT2021.tls201_appln).
#'
#' @param granted_only Logical. Default FALSE (no filter).
#' @return Character. Empty string, or an \code{AND p.granted = TRUE} clause.
build_granted_clause_v2 <- function(granted_only = FALSE) {
  if (isTRUE(granted_only)) "AND p.granted = TRUE" else ""
}

#' Build a SQL clause restricting to multi-application families
#'
#' When \code{multifam_only} is TRUE, returns the AND clause used in the main
#' WHERE to keep only docdbs with \code{fam_size_min2 = TRUE} — the per-family
#' boolean attached at build time, TRUE iff PATSTAT's
#' \code{docdb_family_size >= 2} for that family.
#'
#' @param multifam_only Logical. Default FALSE (no filter).
#' @return Character. Empty string, or an \code{AND p.fam_size_min2 = TRUE}
#'   clause.
build_multifam_clause_v2 <- function(multifam_only = FALSE) {
  if (isTRUE(multifam_only)) "AND p.fam_size_min2 = TRUE" else ""
}

#' Build a SQL clause excluding utility-model-only families
#'
#' When \code{exclude_um} is TRUE, returns the AND clause that drops every
#' docdb with \code{is_um = TRUE} (the per-family boolean attached at build
#' time — TRUE iff every application in the family has appln_kind in
#' UM_KINDS = ('U', 'W'), set in patbis2025 nb1 cell 9). About 30% of all
#' PATSTAT families are flagged UM and most are post-2013 CN filings; the
#' rationale is the Kogan PV extrapolation shouldn't propagate listed-firm
#' market values to weaker / shorter-life utility models.
#'
#' @param exclude_um Logical. Default FALSE (no filter, UM families kept).
#' @return Character. Empty string, or an \code{AND p.is_um = FALSE} clause.
build_exclude_um_clause_v2 <- function(exclude_um = FALSE) {
  if (isTRUE(exclude_um)) "AND p.is_um = FALSE" else ""
}

#' Build a SQL clause restricting to selected cities + capital-fallback toggle
#'
#' Used by query builders that join \code{countrymap} (alias \code{c} below).
#' Mirrors the HiGGlo helper \code{build_city_filter_sql()}: a vector of
#' city names is whitelisted (AND clause), and (when a city filter IS
#' active) unless \code{include_fallback} is TRUE rows where
#' \code{c.geocode_missing = TRUE} are dropped. The "No city filter"
#' sentinel from the dropdown disables both the city whitelist and the
#' geocode-missing filter — i.e. when no city is chosen we don't apply
#' any geocoding constraint, so the headline counts reflect the full
#' patent_database universe rather than the geocoded subset (~17% of it).
#' (HiGGlobe has its own geocode handling and is unaffected.)
#'
#' @param selected_cities  Character vector from input$city. Empty / NULL
#'                          / containing "No city filter" disables both
#'                          the city whitelist AND the geocode constraint.
#' @param include_fallback Logical. Only consulted when a real city is
#'                          selected. TRUE keeps capital-fallback rows;
#'                          FALSE (default) drops them.
#' @param alias            SQL alias of the countrymap row in the
#'                          surrounding query (default "c").
#' @return Character. Empty string, or an \code{AND ...} fragment safe
#'   to append to a WHERE clause. Empty (no INNER JOIN to countrymap
#'   needed) when no city is selected.
build_city_clause_v2 <- function(selected_cities,
                                 include_fallback = FALSE,
                                 alias            = "c") {
  city_filter_active <- length(selected_cities) > 0L &&
    !all(selected_cities %in% c("No city filter", ""))
  if (!city_filter_active) {
    # No city chosen → no geocode filter at all. The country/tech view
    # uses the full ~3.7M family universe instead of the geocoded ~656K
    # subset. The geocode-missing constraint only makes sense when the
    # user has actually scoped to specific cities.
    return("")
  }
  cs <- setdiff(selected_cities, "No city filter")
  quoted <- paste0("'", gsub("'", "''", cs, fixed = TRUE),
                   "'", collapse = ", ")
  clauses <- sprintf("%s.city IN (%s)", alias, quoted)
  if (!isTRUE(include_fallback)) {
    clauses <- c(clauses, sprintf("(NOT %s.geocode_missing)", alias))
  }
  paste("AND", paste(clauses, collapse = " AND "))
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
sql_country_combined_v2 <- function(toflow, country_sql, techs, firm_clause,
                                      top_n_ids = 10, granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE) {

  tech_bool      <- build_tech_bool_v2(techs)
  granted_clause <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

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
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
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

#' RTA denominator: distinct-family count per country, ignoring any tech filter
#'
#' Mirrors the row-selection logic used by \code{sql_country_combined_v2}
#' (same \code{full_patent_database} scan, same firm-filter CTE, same
#' \code{ctry_code IN (...)} and non-null-\code{toflow} restrictions) but
#' drops the tech-filter join. Using the same row base guarantees that
#' \code{allinnos} is an honest superset of \code{innos}: when tech_bool is
#' TRUE ("All Innovations") the two are equal row-for-row, so RTA collapses
#' to 1. Firm filtering is applied via the same filtered_firm CTE, so a
#' family matched by two selected firms still contributes exactly once —
#' no inflation from firm mapping.
#'
#' @param toflow Character. Flow column (used for NOT NULL restriction only)
#' @param country_sql Character. Comma-separated quoted country codes
#' @param firm_clause Character. AND clause for firm filtering (may be empty)
#' @return Character. SQL returning ctry_code, allinnos
sql_ctry_allinnos_v2 <- function(toflow, country_sql, firm_clause,
                                   granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE) {
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
    paste0("WHERE ", firm_condition)
  }
  granted_clause <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

  glue::glue("
    {if (nchar(trimws(firm_clause)) > 0) '
    WITH filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
    ' else ''}
    {if (nchar(trimws(firm_clause)) > 0) firm_filter_sql else ''}
    {if (nchar(trimws(firm_clause)) > 0) ')' else ''}
    SELECT
      p.ctry_code,
      COUNT(DISTINCT p.docdb_family_id) AS allinnos
    FROM full_patent_database p
    {if (nchar(trimws(firm_clause)) > 0) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
    WHERE p.ctry_code IN ({country_sql})
      AND p.{toflow} IS NOT NULL
      {granted_clause}
      {exclude_um_clause}
        {multifam_clause}
    GROUP BY p.ctry_code
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
sql_country_tech_combined_v2 <- function(toflow, country_sql, tech_filters, firm_clause,
                                           top_n_ids = 10, granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE,
                                           selected_cities = character(0),
                                           include_fallback = FALSE) {

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

  granted_clause  <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

  # City filter (Value flows by Technology only, for now). When the
  # filter is active we INNER JOIN countrymap to expose the city +
  # geocode_missing columns the WHERE clause needs. The clause / join
  # collapse to empty strings when the user keeps the default ("No
  # city filter" + "Include capital city fallback" off implies a join
  # is still needed to evaluate `NOT c.geocode_missing`).
  city_clause <- build_city_clause_v2(
    selected_cities  = selected_cities,
    include_fallback = include_fallback,
    alias            = "c"
  )
  city_join_active <- nchar(trimws(city_clause)) > 0
  city_join <- if (city_join_active) {
    "INNER JOIN countrymap c
       ON c.docdb_family_id = p.docdb_family_id
      AND c.ctry_code       = p.ctry_code"
  } else ""

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
      {city_join}
      WHERE p.ctry_code IN ({country_sql})
        AND p.{toflow} IS NOT NULL
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
        {city_clause}
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
        {city_join}
        WHERE p.ctry_code IN ({country_sql})
          AND p.{toflow} IS NOT NULL
          {granted_clause}
          {exclude_um_clause}
          {multifam_clause}
          {city_clause}
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

#' Build per-firm-category WHERE clauses for the firm-bar tab
#'
#' Takes the raw selection from the "Sector and firm categories" selectize
#' (a mix of sector-group names and individual firm names from
#' \code{firm_grouped_choices}) and returns one WHERE clause per selection,
#' keyed by the display label that should appear on the corresponding bar.
#' Sector groups are expanded to their constituent firms via
#' \code{firm_sector_groups} (from \code{R/sysdata.rda}).
#'
#' Sentinel values ("No firm filter", empty strings, "All categories") are
#' dropped so they never become bars.
#'
#' @param selected Character vector from the selectizeInput.
#' @return Named list label -> SQL WHERE clause for \code{patents_x_firm f}.
#' @keywords internal
build_firm_categories_filter_v2 <- function(selected) {
  selected <- setdiff(selected, c("No firm filter", "", "All categories"))
  if (length(selected) == 0) return(list())
  lapply(setNames(selected, selected), function(s) {
    if (s %in% names(firm_sector_groups)) {
      firms <- firm_sector_groups[[s]]
      firms_sql <- paste0("'", gsub("'", "''", firms, fixed = TRUE),
                          "'", collapse = ", ")
      glue::glue("WHERE f.firm IN ({firms_sql})")
    } else {
      glue::glue("WHERE f.firm = '{gsub(\"'\", \"''\", s, fixed = TRUE)}'")
    }
  })
}

#' Build per-bar country-category filters (v2)
#'
#' Country analogue of \code{build_firm_categories_filter_v2()}: turns the
#' "Country Categories" tree selection (a mix of group names and ISO2
#' codes) into a named list of \code{ctry_code} conditions, one per bar.
#' A group becomes an aggregate bar over its member codes; an individual
#' country becomes a single-code bar labelled by country name.
#' @param selected Character vector from \code{country_tree_selected()}.
#' @return Named list label -> SQL condition on \code{p.ctry_code}.
#' @keywords internal
build_country_categories_filter_v2 <- function(selected) {
  selected  <- setdiff(selected, c("", "All categories"))
  if (length(selected) == 0) return(list())
  grp_names <- names(group_definitions)
  c2n       <- country_code_to_name()
  out <- list()
  for (s in selected) {
    if (s %in% grp_names) {
      codes     <- group_definitions[[s]]
      codes_sql <- paste0("'", gsub("'", "''", codes, fixed = TRUE),
                          "'", collapse = ", ")
      out[[s]]  <- as.character(glue::glue("p.ctry_code IN ({codes_sql})"))
    } else {
      label <- if (s %in% names(c2n)) unname(c2n[[s]]) else s
      out[[label]] <- as.character(
        glue::glue("p.ctry_code = '{gsub(\"'\", \"''\", s, fixed = TRUE)}'"))
    }
  }
  out
}

#' Combined per-country-category query (v2)
#'
#' Country analogue of \code{sql_country_firm_combined_v2()}. Each selected
#' category (group or individual country) becomes one bar. Membership is
#' assigned in \code{filtered_country_cats} (a family belongs to a bar when
#' it has a patent in one of the bar's member countries *and* in the
#' universe \code{country_sql}); the per-bar
#' \code{DISTINCT ON (country_category, docdb_family_id)} then guarantees a
#' family in several member countries is counted ONCE for the group bar.
#' @keywords internal
sql_country_countrycat_combined_v2 <- function(toflow, country_sql, cat_filters,
                                               firm_clause = "", techs = "All",
                                               top_n_ids = 10,
                                               granted_only = FALSE,
                                               multifam_only = FALSE,
                                               exclude_um = FALSE) {
  if (length(cat_filters) == 0) return(NULL)

  granted_clause    <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause   <- build_multifam_clause_v2(multifam_only)

  tech_bool <- build_tech_bool_v2(techs)
  tech_join <- if (tech_bool == "TRUE") "" else
    "INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id"
  tech_cte_sql <- if (tech_bool == "TRUE") "" else glue::glue("
    , filtered_tech AS (
      SELECT DISTINCT t.docdb_family_id
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
      WHERE {tech_bool}
    )")

  global_firm_active <- nchar(trimws(firm_clause)) > 0
  global_firm_filter_sql <- if (!global_firm_active) "" else
    paste0("WHERE ", gsub("^\\s*AND\\s+f\\.firm", "firm", firm_clause))
  global_firm_join <- if (global_firm_active)
    "INNER JOIN filtered_firm_global ffg ON p.docdb_family_id = ffg.docdb_family_id" else ""

  # Per-bar membership: family -> country_category if it has a patent in one
  # of the bar's member countries that is also inside the universe.
  parts <- vapply(names(cat_filters), function(label) {
    safe_label <- gsub("'", "''", label, fixed = TRUE)
    glue::glue("
      SELECT DISTINCT p.docdb_family_id, '{safe_label}' AS country_category
      FROM full_patent_database p
      WHERE ({cat_filters[[label]]})
        AND p.ctry_code IN ({country_sql})
    ")
  }, character(1))
  filtered_country_cats_sql <- paste(parts, collapse = "\nUNION ALL\n")

  glue::glue("
    WITH filtered_country_cats AS (
      {filtered_country_cats_sql}
    )
    {tech_cte_sql}
    {if (global_firm_active) ',
    filtered_firm_global AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      ' else ''}
    {if (global_firm_active) global_firm_filter_sql else ''}
    {if (global_firm_active) ')' else ''}
    ,
    deduped AS (
      SELECT DISTINCT ON (cc.country_category, p.docdb_family_id)
        cc.country_category,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow}
      FROM full_patent_database p
      INNER JOIN filtered_country_cats cc ON p.docdb_family_id = cc.docdb_family_id
      {tech_join}
      {global_firm_join}
      WHERE p.ctry_code IN ({country_sql})
        AND p.{toflow} IS NOT NULL
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
    ),
    ranked AS (
      SELECT
        country_category,
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY country_category ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER (PARTITION BY country_category)                        AS cnt
      FROM deduped
    ),
    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS allinnos
      FROM (
        SELECT DISTINCT ON (p.docdb_family_id) p.docdb_family_id, p.{toflow}
        FROM full_patent_database p
        {tech_join}
        {global_firm_join}
        WHERE p.ctry_code IN ({country_sql})
          AND p.{toflow} IS NOT NULL
          {granted_clause}
          {exclude_um_clause}
          {multifam_clause}
      ) t
    )
    SELECT
      country_category AS technology,
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
    GROUP BY country_category, os.allmean, os.allinnos
  ")
}

#' Universe-wide total-innovations denominator (v2)
#'
#' A single COUNT(DISTINCT family) across the whole universe after the
#' country / firm / granted / multifam / um filters but WITHOUT the
#' technology filter — the RTA denominator reported in the by-country /
#' RTA innovation-count tooltip.
#' @keywords internal
sql_universe_allinnos_v2 <- function(toflow, country_sql, firm_clause = "",
                                     granted_only = FALSE,
                                     multifam_only = FALSE,
                                     exclude_um = FALSE) {
  granted_clause    <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause   <- build_multifam_clause_v2(multifam_only)

  global_firm_active <- nchar(trimws(firm_clause)) > 0
  global_firm_filter_sql <- if (!global_firm_active) "" else
    paste0("WHERE ", gsub("^\\s*AND\\s+f\\.firm", "firm", firm_clause))
  global_firm_join <- if (global_firm_active)
    "INNER JOIN filtered_firm_global ffg ON p.docdb_family_id = ffg.docdb_family_id" else ""

  glue::glue("
    {if (global_firm_active) 'WITH filtered_firm_global AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
    ' else ''}
    {if (global_firm_active) global_firm_filter_sql else ''}
    {if (global_firm_active) ')' else ''}
    SELECT COUNT(DISTINCT p.docdb_family_id) AS allinnos
    FROM full_patent_database p
    {global_firm_join}
    WHERE p.ctry_code IN ({country_sql})
      AND p.{toflow} IS NOT NULL
      {granted_clause}
      {exclude_um_clause}
      {multifam_clause}
  ")
}

#' Per-country-category total-innovations denominator (v2)
#'
#' RTA denominator for the country-category bars: COUNT(DISTINCT family) per
#' category WITHOUT the technology filter, with the same per-bar membership
#' (and dedup) as \code{sql_country_countrycat_combined_v2()}.
#' @keywords internal
sql_countrycat_allinnos_v2 <- function(toflow, country_sql, cat_filters,
                                       firm_clause = "",
                                       granted_only = FALSE,
                                       multifam_only = FALSE,
                                       exclude_um = FALSE) {
  if (length(cat_filters) == 0) return(NULL)

  granted_clause    <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause   <- build_multifam_clause_v2(multifam_only)

  global_firm_active <- nchar(trimws(firm_clause)) > 0
  global_firm_filter_sql <- if (!global_firm_active) "" else
    paste0("WHERE ", gsub("^\\s*AND\\s+f\\.firm", "firm", firm_clause))
  global_firm_join <- if (global_firm_active)
    "INNER JOIN filtered_firm_global ffg ON p.docdb_family_id = ffg.docdb_family_id" else ""

  parts <- vapply(names(cat_filters), function(label) {
    safe_label <- gsub("'", "''", label, fixed = TRUE)
    glue::glue("
      SELECT DISTINCT p.docdb_family_id, '{safe_label}' AS country_category
      FROM full_patent_database p
      WHERE ({cat_filters[[label]]})
        AND p.ctry_code IN ({country_sql})
    ")
  }, character(1))
  filtered_country_cats_sql <- paste(parts, collapse = "\nUNION ALL\n")

  glue::glue("
    WITH filtered_country_cats AS (
      {filtered_country_cats_sql}
    )
    {if (global_firm_active) ',
    filtered_firm_global AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      ' else ''}
    {if (global_firm_active) global_firm_filter_sql else ''}
    {if (global_firm_active) ')' else ''}
    SELECT
      cc.country_category,
      COUNT(DISTINCT p.docdb_family_id) AS allinnos
    FROM full_patent_database p
    INNER JOIN filtered_country_cats cc ON p.docdb_family_id = cc.docdb_family_id
    {global_firm_join}
    WHERE p.ctry_code IN ({country_sql})
      AND p.{toflow} IS NOT NULL
      {granted_clause}
      {exclude_um_clause}
      {multifam_clause}
    GROUP BY cc.country_category
  ")
}

#' Generate SQL combined query for country x firm-category aggregation (v2)
#'
#' Sibling of \code{sql_country_tech_combined_v2}: same shape, same stats,
#' but each bar corresponds to a selected firm or sector group rather than
#' a technology. The per-bar selection comes from
#' \code{build_firm_categories_filter_v2()}; the global firm filter
#' (\code{firm_clause}) and the global tech selection (\code{techs}) still
#' scope the universe consistently across bars and the overall average.
#'
#' The returned column \code{technology} carries the firm-category label so
#' \code{plot_avstrax_by_country()} can be reused without modification.
#'
#' @param toflow Character. Column name for the return flow measure.
#' @param country_sql Character. Comma-separated quoted country codes.
#' @param firm_filters Named list from \code{build_firm_categories_filter_v2()}.
#' @param firm_clause Character. AND clause for the global firm filter
#'   (typically empty on this tab since the global firm filter is hidden,
#'   but kept for symmetry / future use).
#' @param techs Character vector. Global technology selection from the UI.
#' @return Character. SQL query string, or NULL when no firm categories
#'   were selected.
#' @keywords internal
sql_country_firm_combined_v2 <- function(toflow, country_sql, firm_filters,
                                         firm_clause = "", techs = "All",
                                         top_n_ids = 10,
                                         granted_only = FALSE,
                                         multifam_only = FALSE,
                                         exclude_um = FALSE) {
  if (length(firm_filters) == 0) return(NULL)

  granted_clause    <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause   <- build_multifam_clause_v2(multifam_only)

  tech_bool <- build_tech_bool_v2(techs)
  tech_join <- if (tech_bool == "TRUE") {
    ""
  } else {
    "INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id"
  }
  tech_cte_sql <- if (tech_bool == "TRUE") {
    ""
  } else {
    glue::glue("
    , filtered_tech AS (
      SELECT DISTINCT t.docdb_family_id
      FROM patents_x_tech t
      JOIN tech_lookup tl ON t.technology = tl.technology
      WHERE {tech_bool}
    )")
  }

  # Per-bar union of selected firms / sector groups
  parts <- vapply(names(firm_filters), function(label) {
    safe_label <- gsub("'", "''", label, fixed = TRUE)
    glue::glue("
      SELECT DISTINCT f.docdb_family_id, '{safe_label}' AS firm_category
      FROM patents_x_firm f
      {firm_filters[[label]]}
    ")
  }, character(1))
  filtered_firm_cats_sql <- paste(parts, collapse = "\nUNION ALL\n")

  # Optional global firm filter (sidebar's existing "Firm or Sector Group"
  # control). When the new tab hides this control the clause is empty and
  # the CTE collapses away.
  global_firm_active <- nchar(trimws(firm_clause)) > 0
  global_firm_filter_sql <- if (!global_firm_active) {
    ""
  } else {
    cond <- gsub("^\\s*AND\\s+f\\.firm", "firm", firm_clause)
    paste0("WHERE ", cond)
  }
  global_firm_join <- if (global_firm_active) {
    "INNER JOIN filtered_firm_global ffg ON p.docdb_family_id = ffg.docdb_family_id"
  } else ""

  glue::glue("
    WITH filtered_firm_cats AS (
      {filtered_firm_cats_sql}
    )
    {tech_cte_sql}
    {if (global_firm_active) ',
    filtered_firm_global AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
      ' else ''}
    {if (global_firm_active) global_firm_filter_sql else ''}
    {if (global_firm_active) ')' else ''}
    ,
    deduped AS (
      SELECT DISTINCT ON (fc.firm_category, p.docdb_family_id)
        fc.firm_category,
        p.docdb_family_id,
        p.appln_id,
        p.{toflow}
      FROM full_patent_database p
      INNER JOIN filtered_firm_cats fc ON p.docdb_family_id = fc.docdb_family_id
      {tech_join}
      {global_firm_join}
      WHERE p.ctry_code IN ({country_sql})
        AND p.{toflow} IS NOT NULL
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
    ),
    ranked AS (
      SELECT
        firm_category,
        docdb_family_id,
        appln_id,
        {toflow},
        ROW_NUMBER() OVER (PARTITION BY firm_category ORDER BY {toflow} DESC) AS rnk,
        COUNT(*)     OVER (PARTITION BY firm_category)                        AS cnt
      FROM deduped
    ),
    overall_stats AS (
      SELECT
        AVG({toflow}) AS allmean,
        COUNT(*)      AS allinnos
      FROM (
        SELECT DISTINCT ON (p.docdb_family_id) p.docdb_family_id, p.{toflow}
        FROM full_patent_database p
        {tech_join}
        {global_firm_join}
        WHERE p.ctry_code IN ({country_sql})
          AND p.{toflow} IS NOT NULL
          {granted_clause}
          {exclude_um_clause}
          {multifam_clause}
      ) t
    )
    SELECT
      firm_category AS technology,
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
    GROUP BY firm_category, os.allmean, os.allinnos
  ")
}

#' Generate SQL combined query for region aggregation (v2)
#'
#' Mirrors sql_country_combined_v2 but filters and partitions on
#' region_code via the patents_x_region bridge JOIN.
#' Returns per-region aggregates plus an overall 'All' row.
#' RTA fields are computed in R after this query via a left_join to
#' the per-call denominator returned by sql_region_allinnos_v2() —
#' the previous static `allinnos_region_baseline` from sysdata.rda
#' was retired alongside the v2 SQL refactor.
#'
#' @param toflow Character. Column name for the return flow measure.
#' @param region_sql Character. Comma-separated quoted region codes.
#' @param techs Character vector. Technology selection from the UI.
#' @param firm_clause Character. AND clause from build_firm_clause_v2().
#' @return Character. SQL query string.
sql_region_combined_v2 <- function(toflow, region_sql, techs, firm_clause,
                                     top_n_ids = 10, granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE) {

  tech_bool      <- build_tech_bool_v2(techs)
  granted_clause <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

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
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
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

#' RTA denominator for regions: distinct-family count per region, no tech filter
#'
#' Region analogue of \code{sql_ctry_allinnos_v2}. Mirrors the row-selection
#' logic of \code{sql_region_combined_v2} — same \code{patents_x_region}
#' bridge join, same \code{ctry_code = 'GB'} restriction, same firm-filter
#' CTE — but omits the tech-filter join and uses
#' \code{COUNT(DISTINCT docdb_family_id)} so firm mapping cannot inflate
#' the count.
#'
#' @param toflow Character. Flow column (used for NOT NULL restriction only)
#' @param region_sql Character. Comma-separated quoted region codes
#' @param firm_clause Character. AND clause for firm filtering (may be empty)
#' @return Character. SQL returning region_code, allinnos
sql_region_allinnos_v2 <- function(toflow, region_sql, firm_clause,
                                     granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE) {
  firm_filter_sql <- if (nchar(trimws(firm_clause)) == 0) {
    ""
  } else {
    firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
    paste0("WHERE ", firm_condition)
  }
  granted_clause <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

  glue::glue("
    {if (nchar(trimws(firm_clause)) > 0) '
    WITH filtered_firm AS (
      SELECT DISTINCT docdb_family_id
      FROM patents_x_firm f
    ' else ''}
    {if (nchar(trimws(firm_clause)) > 0) firm_filter_sql else ''}
    {if (nchar(trimws(firm_clause)) > 0) ')' else ''}
    SELECT
      r.region_code,
      COUNT(DISTINCT p.docdb_family_id) AS allinnos
    FROM full_patent_database p
    INNER JOIN patents_x_region r ON p.docdb_family_id = r.docdb_family_id
    {if (nchar(trimws(firm_clause)) > 0) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
    WHERE r.region_code IN ({region_sql})
      AND p.ctry_code = 'GB'
      AND p.{toflow} IS NOT NULL
      {granted_clause}
      {exclude_um_clause}
        {multifam_clause}
    GROUP BY r.region_code
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
sql_region_tech_combined_v2 <- function(toflow, region_sql, tech_filters, firm_clause,
                                         top_n_ids = 10, granted_only = FALSE, multifam_only = FALSE, exclude_um = FALSE) {

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

  granted_clause <- build_granted_clause_v2(granted_only)
  exclude_um_clause <- build_exclude_um_clause_v2(exclude_um)
  multifam_clause <- build_multifam_clause_v2(multifam_only)

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
        {granted_clause}
        {exclude_um_clause}
        {multifam_clause}
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
          {granted_clause}
          {exclude_um_clause}
        {multifam_clause}
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