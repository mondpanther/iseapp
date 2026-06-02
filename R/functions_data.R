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

#' Build a shinyTree data structure for a firm FILTER selector
#'
#' One folder per ICB sector, each holding its firm leaves (the
#' "Individual firms" branch only — no aggregate-sector leaves, since this
#' is a filter rather than a set of bar categories). Firms in `picks` are
#' pre-checked and their sector folders opened; everything else is
#' collapsed and unchecked.
#' @param picks Character vector of firm names to pre-check.
#' @return Nested named list suitable for `shinyTree::renderTree()`.
#' @export
build_firm_filter_tree <- function(picks = character(0)) {
  lapply(stats::setNames(nm = names(firm_sector_groups)), function(sec) {
    firm_names <- unlist(firm_sector_groups[[sec]], use.names = FALSE)
    children <- stats::setNames(as.list(rep("", length(firm_names))),
                                firm_names)
    for (f in intersect(firm_names, picks))
      attr(children[[f]], "stselected") <- TRUE
    if (any(firm_names %in% picks))
      attr(children, "stopened") <- TRUE
    children
  })
}

#' Read the checked firm leaves out of a firm-filter shinyTree input
#'
#' Returns the individual firm names that are ticked, dropping the sector
#' folder names (ticking a folder cascade-checks its firm leaves, which
#' already carry the real selection).
#' @param tree_state The `input$<id>` value of a `shinyTree` filter control.
#' @return Character vector of selected firm names (empty = no filter).
#' @export
firm_tree_selected_firms <- function(tree_state) {
  if (is.null(tree_state)) return(character(0))
  sel <- shinyTree::get_selected(tree_state, format = "names")
  if (length(sel) == 0) return(character(0))
  nm <- unique(unlist(lapply(sel, as.character), use.names = FALSE))
  setdiff(nm, names(firm_sector_groups))
}

#' Short leaf label for a Value-Flow tree node
#'
#' Strips the repeated measure words from a full `toflow_choices` label,
#' leaving just the qualifier ("Global", "National", "LMICs", "US", ...).
#' @param full Full label, e.g. "Average Returns to US".
#' @return Short qualifier label, e.g. "US".
#' @keywords internal
toflow_short_label <- function(full) {
  if (grepl(" to ", full, fixed = TRUE)) sub("^.* to ", "", full)
  else strsplit(full, " ", fixed = TRUE)[[1]][[2]]
}

#' Derive the Value-Flow tree spec from `toflow_choices`
#'
#' Builds the single-select tree template plus the bidirectional maps
#' between a selected node and its underlying flow value. Top-level
#' "Citation Count" is a selectable leaf; "Average Spillovers", "Average
#' Returns" and "Marginal Returns" are branches whose leaves carry short
#' qualifier labels (the branch is recovered from the node's ancestry).
#' @return list(data, key_value, value_key, sep).
#' @keywords internal
toflow_tree_spec <- function() {
  tc  <- toflow_choices
  sep <- "@@"   # separator that never appears in a label
  branch_order  <- c("Citation Count", "Average Spillovers",
                     "Average Returns", "Marginal Returns")
  branch_groups <- c("Average Spillovers" = "Average Spillovers",
                     "Average Returns"    = "Average Returns",
                     "Marginal Returns"   = "Marginal Returns")

  data      <- list()
  key_value <- character(0)
  value_key <- character(0)

  cit_val <- unlist(tc[["Citation Counts"]], use.names = FALSE)
  if (length(cit_val)) {
    data[["Citation Count"]]      <- ""
    key_value[["Citation Count"]] <- cit_val[[1]]
    value_key[[cit_val[[1]]]]     <- "Citation Count"
  }

  for (bname in names(branch_groups)) {
    grp    <- tc[[branch_groups[[bname]]]]
    leaves <- list()
    for (full in names(grp)) {
      val  <- grp[[full]]
      slab <- toflow_short_label(full)
      leaves[[slab]]   <- ""
      key              <- paste(bname, slab, sep = sep)
      key_value[[key]] <- val
      value_key[[val]] <- key
    }
    data[[bname]] <- leaves
  }

  list(data      = data[intersect(branch_order, names(data))],
       key_value = key_value,
       value_key = value_key,
       sep       = sep)
}

#' Build Value-Flow shinyTree data with one leaf pre-selected
#' @param selected A flow value (e.g. "ev_global") to pre-select, or NULL.
#' @return Nested list for `shinyTree::renderTree()`.
#' @export
build_toflow_tree_data <- function(selected = NULL) {
  spec <- toflow_tree_spec()
  data <- spec$data
  if (!is.null(selected) && length(selected) == 1L &&
      selected %in% names(spec$value_key)) {
    key <- spec$value_key[[selected]]
    if (identical(key, "Citation Count")) {
      attr(data[["Citation Count"]], "stselected") <- TRUE
    } else {
      parts <- strsplit(key, spec$sep, fixed = TRUE)[[1]]
      attr(data[[parts[1]]][[parts[2]]], "stselected") <- TRUE
      attr(data[[parts[1]]], "stopened") <- TRUE
    }
  }
  data
}

#' Read the selected flow value out of a Value-Flow shinyTree input
#' @param tree_state The `input$<id>` value of the tree.
#' @return The selected flow value (character) or NULL if none / a branch.
#' @export
toflow_tree_value <- function(tree_state) {
  if (is.null(tree_state)) return(NULL)
  sel <- shinyTree::get_selected(tree_state, format = "names")
  if (length(sel) == 0) return(NULL)
  spec <- toflow_tree_spec()
  for (node in sel) {
    nm  <- as.character(node)
    anc <- attr(node, "ancestry")
    key <- if (length(anc)) paste(anc[[1]], nm, sep = spec$sep) else nm
    if (key %in% names(spec$key_value)) return(unname(spec$key_value[[key]]))
  }
  NULL
}

#' Resolve the initial Value-Flow tree selection from the page URL
#'
#' Reads the native-bookmark value of the hidden selectize (keyed by its
#' namespaced id) or the short `flow` deep-link param, falling back to the
#' default. Used to pre-select the correct leaf at tree-render time so the
#' tree view matches a restored / deep-linked flow without needing a
#' selectize-to-tree observer.
#' @param url_search The `session$clientData$url_search` string.
#' @param ns_id Namespaced id of the hidden selectize (e.g. "country-toflow").
#' @param default Default flow value when nothing valid is found.
#' @return A valid flow value (character).
#' @keywords internal
toflow_init_value <- function(url_search, ns_id, default = "ev_global") {
  q   <- shiny::parseQueryString(if (is.null(url_search)) "" else url_search)
  raw <- q[[ns_id]]
  v   <- NULL
  if (!is.null(raw) && nzchar(raw))
    v <- tryCatch(jsonlite::fromJSON(raw),
                  error = function(e) gsub('^"|"$', "", raw))
  if (is.null(v) || !length(v) || !nzchar(v)) v <- q[["flow"]]
  spec <- toflow_tree_spec()
  if (!is.null(v) && length(v) == 1L && v %in% names(spec$value_key)) v
  else default
}

# ── Technology trees (mirror `grouped_techs`) ───────────────────────────
# `grouped_techs` is a named list of groups; group 1 ("Broad Technology
# Categories") holds the broad categories, groups 2..n-1 are the narrow
# "detail" groups (Detailed Green technologies, AI subcategories, …, CPC
# Sections & Other Categories), and the final "─── Actions" group is a
# UI-only shortcut that is dropped from the trees.

#' The tech groups used by the trees (grouped_techs minus the Actions group)
#' @keywords internal
tech_tree_groups <- function() {
  gt <- grouped_techs
  gt[!grepl("Actions", names(gt), fixed = TRUE)]
}

#' Name of the broad-categories group inside grouped_techs
#' @keywords internal
tech_broad_group_name <- function() "Broad Technology Categories"

#' All selectable tech values across broad + narrow groups
#' @keywords internal
tech_all_values <- function() {
  gt <- tech_tree_groups()
  unique(unlist(lapply(gt, names), use.names = FALSE))
}

#' Narrow-only tech values (detail groups, excluding the broad group)
#' @keywords internal
tech_narrow_values <- function() {
  gt <- tech_tree_groups()
  gt <- gt[setdiff(names(gt), tech_broad_group_name())]
  unique(unlist(lapply(gt, names), use.names = FALSE))
}

#' Build the two-branch technology shinyTree (categories or filter)
#'
#' Two top branches: "Broad Categories" (leaves = the broad group's items)
#' and "Detailed Categories" (the detail groups as sub-folders, each holding
#' its narrow-technology leaves). Used for both the by-technology bar
#' categories and the "Technologies included" filter. Items in `selected`
#' are pre-checked.
#' @param selected Character vector of tech values to pre-check.
#' @return Nested list for `shinyTree::renderTree()`.
#' @export
build_tech_category_tree <- function(selected = character(0)) {
  gt        <- tech_tree_groups()
  broad_grp <- tech_broad_group_name()

  mk_leaves <- function(items) {
    leaves <- stats::setNames(as.list(rep("", length(items))), items)
    for (it in intersect(items, selected))
      attr(leaves[[it]], "stselected") <- TRUE
    leaves
  }

  detail_names  <- setdiff(names(gt), broad_grp)

  # The broad group accidentally duplicates the narrow sub-categories of
  # themed broads (e.g. the AI subcategories). Drop those from the Broad
  # branch so they only appear under their parent in Detailed Categories.
  # The "CPC Sections & Other Categories" group is itself broad-level, so
  # its items are kept in the broad branch.
  themed_detail <- setdiff(detail_names, "CPC Sections & Other Categories")
  narrow_sub    <- unique(unlist(lapply(gt[themed_detail], names),
                                 use.names = FALSE))
  broad_items   <- setdiff(names(gt[[broad_grp]]), narrow_sub)
  broad_leaves  <- mk_leaves(broad_items)
  if (any(broad_items %in% selected)) attr(broad_leaves, "stopened") <- TRUE

  narrow_branch <- lapply(stats::setNames(nm = detail_names), function(g) {
    items  <- names(gt[[g]])
    leaves <- mk_leaves(items)
    if (any(items %in% selected)) attr(leaves, "stopened") <- TRUE
    leaves
  })
  if (any(tech_narrow_values() %in% selected))
    attr(narrow_branch, "stopened") <- TRUE

  list("Broad Categories" = broad_leaves,
       "Detailed Categories" = narrow_branch)
}

#' Read the selected values out of a Technology Categories tree
#' @param tree_state The `input$<id>` value of the tree.
#' @return Character vector of selected tech values (broad and/or narrow).
#' @export
tech_category_tree_selected <- function(tree_state) {
  if (is.null(tree_state)) return(character(0))
  sel <- shinyTree::get_selected(tree_state, format = "names")
  if (length(sel) == 0) return(character(0))
  nm <- unique(unlist(lapply(sel, as.character), use.names = FALSE))
  intersect(nm, tech_all_values())
}

#' Resolve the initial tech-tree selection from the page URL
#'
#' Reads the native-bookmark value of the hidden selectize (keyed by its
#' namespaced id) and, optionally, a short deep-link param. Returns NULL
#' when neither is present (so callers can apply their own default), or a
#' (possibly empty) character vector when a value was found.
#' @param url_search The `session$clientData$url_search` string.
#' @param ns_id Namespaced id of the hidden selectize.
#' @param deep_link_param Optional query-param name (e.g. "tech") to fall
#'   back to, parsed as a comma-separated list.
#' @return Character vector of selected values, or NULL if nothing in URL.
#' @keywords internal
tech_url_selected <- function(url_search, ns_id, deep_link_param = NULL) {
  q <- shiny::parseQueryString(if (is.null(url_search)) "" else url_search)
  present <- FALSE
  v <- character(0)
  raw <- q[[ns_id]]
  if (!is.null(raw)) {
    present <- TRUE
    if (nzchar(raw))
      v <- tryCatch(as.character(jsonlite::fromJSON(raw)),
                    error = function(e) gsub('^"|"$', "", raw))
  }
  if (!length(v) && !is.null(deep_link_param)) {
    t <- q[[deep_link_param]]
    if (!is.null(t)) {
      present <- TRUE
      if (nzchar(t)) v <- trimws(strsplit(t, ",", fixed = TRUE)[[1]])
    }
  }
  if (!present) return(NULL)
  v
}

# ── Country tree (Country Groupings + Individual Countries) ──────────────
# Mirrors the firm-categories tree: a top-level fork between predefined
# groups (selected as a whole) and individual countries organised under
# the same groups. `grouped_choices[["Individual Countries"]]` maps country
# NAME -> ISO2 code; `group_definitions` maps group name -> ISO2 codes.

#' Country NAME -> ISO2 code map (named character vector)
#' @keywords internal
country_name_to_code <- function() unlist(grouped_choices[["Individual Countries"]])

#' Country ISO2 code -> NAME map (named character vector)
#' @keywords internal
country_code_to_name <- function() {
  nc <- unlist(grouped_choices[["Individual Countries"]])
  stats::setNames(names(nc), unname(nc))
}

#' Build the Country/Group shinyTree
#'
#' Two top branches: "Country Groupings" (leaves = the predefined group
#' names) and "Individual Countries" (the same groups as sub-folders, each
#' holding its member countries as leaves, labelled by country name). A
#' country may appear under several folders; selections are deduplicated
#' downstream by `expand_country_selection()` and the SQL DISTINCT counts.
#' @param selected Character vector of group names and/or ISO2 codes to
#'   pre-check.
#' @return Nested list for `shinyTree::renderTree()`.
#' @export
build_country_tree <- function(selected = character(0)) {
  gd        <- group_definitions
  c2n       <- country_code_to_name()
  grp_names <- names(gd)
  all_codes <- unname(country_name_to_code())

  sel_groups <- intersect(selected, grp_names)
  sel_codes  <- intersect(selected, all_codes)

  grp_leaves <- stats::setNames(as.list(rep("", length(grp_names))), grp_names)
  for (g in sel_groups) attr(grp_leaves[[g]], "stselected") <- TRUE
  if (length(sel_groups)) attr(grp_leaves, "stopened") <- TRUE

  indiv <- lapply(stats::setNames(nm = grp_names), function(g) {
    codes  <- gd[[g]]
    labels <- unname(ifelse(codes %in% names(c2n), c2n[codes], codes))
    leaves <- stats::setNames(as.list(rep("", length(codes))), labels)
    hit    <- which(codes %in% sel_codes)
    for (i in hit) attr(leaves[[i]], "stselected") <- TRUE
    if (length(hit)) attr(leaves, "stopened") <- TRUE
    leaves
  })
  if (length(sel_codes)) attr(indiv, "stopened") <- TRUE

  list("Country Groupings" = grp_leaves,
       "Individual Countries" = indiv)
}

#' Read selected group names + ISO2 codes out of a Country tree
#'
#' Uses each node's ancestry: a node under "Country Groupings" whose name is
#' a group -> emit the group name; a country leaf under "Individual
#' Countries" -> emit its ISO2 code. Group-folder names and branch headers
#' are ignored. Result is deduplicated.
#' @param tree_state The `input$<id>` value of the tree.
#' @return Character vector of group names and/or ISO2 codes.
#' @export
country_tree_selected <- function(tree_state) {
  if (is.null(tree_state)) return(character(0))
  sel <- shinyTree::get_selected(tree_state, format = "names")
  if (length(sel) == 0) return(character(0))
  grp_names <- names(group_definitions)
  n2c       <- country_name_to_code()
  groups <- character(0); codes <- character(0)
  for (node in sel) {
    nm  <- as.character(node)
    anc <- attr(node, "ancestry")
    top <- if (length(anc)) anc[[1]] else NA_character_
    if (identical(top, "Country Groupings") && nm %in% grp_names) {
      groups <- c(groups, nm)
    } else if (identical(top, "Individual Countries") &&
               !(nm %in% grp_names) && nm %in% names(n2c)) {
      codes <- c(codes, unname(n2c[[nm]]))
    }
  }
  unique(c(groups, codes))
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