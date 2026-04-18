# functions_istraxfunctions_processing.R
#
# Optimized versions of the three functions from functions_istraxfunctions.R
# used by data-raw/build_aggregated_database_v2.R:
#
#   - build_espacenet_search()
#   - compute_avstrax()
#   - compute_avstrax_for_techs()
#
# These produce IDENTICAL output to the originals but are faster.
# The original file is left untouched.
#
# Optimizations applied:
#   1. data.table for joins and distinct() instead of dplyr
#   2. Single setorder() + vectorised indexing for extras (top25/top50/top3)
#      instead of arrange() + mutate(ppp) + re-sorting inside summarise()
#   3. "All" category computed on deduplicated base, then rbind'd as summary
#      row — avoids doubling the working dataset with bind_rows + distinct
#   4. Dead country_name branch removed from compute_avstrax_for_techs
#      (build script's countrymap never has that column)
#   5. library() calls moved to file top-level

library(collapse)
library(dplyr)
library(data.table)
library(rlang)


# ============================================================================
# build_espacenet_search
# ============================================================================
# Unchanged — already fast.

build_espacenet_search <- function(id_strings) {
  sapply(id_strings, function(ids) {
    id_vec <- unlist(strsplit(ids, ",\\s*"))
    query <- paste(paste0("pn=", id_vec), collapse = " OR ")
    paste0('window.open("https://worldwide.espacenet.com/patent/search?q=',
           utils::URLencode(query, reserved = TRUE), '")')
  })
}


# ============================================================================
# compute_avstrax  (by-technology aggregation)
# ============================================================================

compute_avstrax <- function(data, istrax_var, classes, colorings = NULL) {

  istrax_sym <- rlang::sym(istrax_var)

  if (!istrax_var %in% names(data)) {
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler <- ifelse(grepl("strax", istrax_var), 100, 1)

  # --- Convert to data.table for fast joins/distinct ---

  # Base: one row per (docdb_family_id, appln_id, istrax)
  base_dt <- data.table::as.data.table(data)[,
    .(docdb_family_id, appln_id, istrax = get(istrax_var))
  ]
  base_dt <- unique(base_dt)

  # "All" category: original selects (docdb_family_id, istrax) without appln_id,
  # so "All" rows have appln_id = NA. The final distinct() in the original only
  # deduplicates within each set (tech rows have appln_id; All rows don't), so
  # we can handle them separately.
  all_dt_dedup <- unique(base_dt[, .(docdb_family_id, istrax)])
  all_dt_dedup[, `:=`(appln_id = NA, technology = "All")]

  # Tech categories (excluding "All")
  classes_dt <- data.table::as.data.table(classes)
  classes_dt <- classes_dt[technology != "All"]

  tech_dt <- base_dt[classes_dt, on = "docdb_family_id", nomatch = 0L, allow.cartesian = TRUE]
  # tech_dt now has: docdb_family_id, appln_id, istrax, technology (+ any extra cols from classes)
  # Keep only needed columns and deduplicate
  tech_dt <- unique(tech_dt[, .(docdb_family_id, appln_id, istrax, technology)])

  # Combine tech rows + All rows
  avstrax <- data.table::rbindlist(list(tech_dt, all_dt_dedup), use.names = TRUE, fill = TRUE)

  # Scale
  avstrax[, istrax_scaled := istrax * scaler]

  # --- Grouped aggregation (collapse — already fast) ---
  result <- avstrax |>
    fgroup_by(technology) |>
    fsummarize(
      mean = fmean(istrax_scaled, na.rm = TRUE),
      innos = fnobs(istrax_scaled),
      sd_val = fsd(istrax_scaled, na.rm = TRUE),
      q1 = fquantile(istrax_scaled, 0.25, na.rm = TRUE),
      q2 = fquantile(istrax_scaled, 0.5, na.rm = TRUE),
      q3 = fquantile(istrax_scaled, 0.75, na.rm = TRUE)
    ) |>
    fungroup() |>
    as.data.frame()

  result$sem <- result$sd_val / sqrt(result$innos)
  result$sd_val <- NULL

  # --- Extras: top25/top50 bin means and top3_ids ---
  # Single sort, then vectorised computation per group.
  data.table::setorder(avstrax, technology, -istrax_scaled)

  extras <- avstrax[, {
    n <- .N
    ppp <- seq_len(n) / n
    list(
      top25_bin_mean = mean(istrax_scaled[ppp < 0.25], na.rm = TRUE),
      top50_bin_mean = mean(istrax_scaled[ppp < 0.5], na.rm = TRUE),
      top3_ids = paste(head(appln_id[order(-istrax_scaled)], 10), collapse = ", ")
    )
  }, by = technology]

  extras <- as.data.frame(extras)

  # Merge
  result <- dplyr::left_join(result, extras, by = "technology")

  # Compatibility columns
  result$top25 <- 0.25
  result$top50 <- 0.5
  result$top3_ids_url <- build_espacenet_search(result$top3_ids)

  # Greenclass categorization
  result$greenclass <- ifelse(result$technology %in% unlist(colorings["green"]), "green",
                       ifelse(result$technology %in% unlist(colorings["battery"]), "battery",
                       ifelse(result$technology %in% unlist(colorings["hard_to_abate"]), "hard to abate",
                       ifelse(result$technology %in% unlist(colorings["ai"]), "AI",
                       ifelse(result$technology %in% unlist(colorings["cpcsecs"]), "CPC Sections",
                       ifelse(result$technology %in% unlist(colorings["agrifood"]), "agrifood", "other"))))))

  return(result)
}


# ============================================================================
# compute_avstrax_for_techs  (by-country aggregation)
# ============================================================================

compute_avstrax_for_techs <- function(data, istrax_var, classes) {

  istrax_sym <- rlang::sym(istrax_var)

  if (!istrax_var %in% names(data)) {
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler <- ifelse(grepl("strax", istrax_var), 100, 1)

  # --- Filter by classes ---
  data_dt <- data.table::as.data.table(data)

  if (nrow(classes) == 0) {
    filtered_dt <- data_dt
  } else {
    classes_ids <- unique(data.table::as.data.table(classes)[, .(docdb_family_id)])
    filtered_dt <- data_dt[classes_ids, on = "docdb_family_id", nomatch = 0L]
  }

  # --- RTA denominator: distinct innovations per country on UNFILTERED data ---
  counted <- data_dt[, .(Allinnos = data.table::uniqueN(docdb_family_id)), by = ctry_code]
  counted[, SumAllinnos := data.table::uniqueN(data_dt$docdb_family_id)]
  counted <- as.data.frame(counted)

  # --- Build avstrax with "All" country ---
  # Per-country rows
  per_country <- unique(filtered_dt[,
    .(docdb_family_id, appln_id, istrax = get(istrax_var), ctry_code)
  ])

  # "All" country rows: distinct on (docdb_family_id, appln_id, istrax) only
  all_country <- unique(filtered_dt[,
    .(docdb_family_id, appln_id, istrax = get(istrax_var))
  ])
  all_country[, ctry_code := "All"]

  avstrax <- data.table::rbindlist(list(per_country, all_country), use.names = TRUE, fill = TRUE)

  # Scale
  avstrax[, istrax_scaled := istrax * scaler]

  # --- Grouped aggregation (collapse) ---
  result <- avstrax |>
    fgroup_by(ctry_code) |>
    fsummarize(
      mean = fmean(istrax_scaled, na.rm = TRUE),
      innos = fnobs(istrax_scaled),
      sd_val = fsd(istrax_scaled, na.rm = TRUE),
      q1 = fquantile(istrax_scaled, 0.25, na.rm = TRUE),
      q2 = fquantile(istrax_scaled, 0.5, na.rm = TRUE),
      q3 = fquantile(istrax_scaled, 0.75, na.rm = TRUE)
    ) |>
    fungroup() |>
    as.data.frame()

  result$sem <- result$sd_val / sqrt(result$innos)
  result$sd_val <- NULL

  # --- Extras: single sort, vectorised ---
  data.table::setorder(avstrax, ctry_code, -istrax_scaled)

  extras <- avstrax[, {
    n <- .N
    ppp <- seq_len(n) / n
    list(
      top25_bin_mean = mean(istrax_scaled[ppp < 0.25], na.rm = TRUE),
      top50_bin_mean = mean(istrax_scaled[ppp < 0.5], na.rm = TRUE),
      top3_ids = paste(head(appln_id[order(-istrax_scaled)], 10), collapse = ", ")
    )
  }, by = ctry_code]

  extras <- as.data.frame(extras)

  result <- dplyr::left_join(result, extras, by = "ctry_code")

  # Compatibility columns
  result$top25 <- 0.25
  result$top50 <- 0.5
  result$top3_ids_url <- build_espacenet_search(result$top3_ids)

  # --- RTA ---
  result <- dplyr::left_join(result, counted, by = "ctry_code")

  total_innos <- data.table::uniqueN(filtered_dt$docdb_family_id)
  sum_all_innos <- counted$SumAllinnos[1]
  result$share_c <- result$innos / result$Allinnos
  result$share <- total_innos / sum_all_innos
  result$RTA <- 2 * result$share_c / (result$share_c + result$share)

  return(result)
}
