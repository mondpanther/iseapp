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