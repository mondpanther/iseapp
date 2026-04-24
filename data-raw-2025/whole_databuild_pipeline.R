# ============================================================================
# whole_databuild_pipeline.R
# ----------------------------------------------------------------------------
# Run the full ISE app data pipeline, start to finish, in the correct order.
#
# Usage (from the project root):
#   source("data-raw/whole_databuild_pipeline.R")
#
# What this produces (in order):
#   <dropbox>/patbis2021/data/fromWATSON/innos_ev_<CC>_2009_2018.parquet
#       – compact parquet siblings of the Watson DSVs (one-time conversion)
#   .bigdata/inventor_countries_harm.parquet
#   .bigdata/holder_countries_harm.parquet
#       – harmonized per-person country assignments (one country per psn_name
#         in PATSTAT; tied names within a family are resolved to the
#         intersection with the family-mode country/ies — computed across
#         all persons in the family, both inventors and holders, with
#         multiple modes retained when tied; no intersection => all tied
#         candidates are kept)
#   .bigdata/countrymap.fst
#   .bigdata/nationalkey.fst
#       – family × country map restricted to the 185 countries with Watson
#         EV data; nationalkey attaches the EV value per (family, country)
#   inst/extdata/patent_database.parquet   (and other per-dimension parquets)
#       – the app's main database (istrax / avstrax / EV measures, joined
#         against countrymap + firm/tech/region lookups)
#   R/sysdata.rda
#       – UI lookup objects loaded automatically by the Shiny package
#
# Expected runtime on a warm cache (all intermediates present):
#   ~5-10 minutes end-to-end.
# Cold cache (first run, or after deleting .bigdata/):
#   ~45-60 minutes, dominated by BigQuery downloads (CPC + persons) and
#   the initial DSV -> parquet conversion.
#
# Cache controls:
#   * Set   RUN_WATSON_CONVERT = FALSE   to skip the DSV -> parquet conversion
#     (it is idempotent and fast when already done, so usually leave TRUE).
#   * Delete a specific .fst/.parquet under .bigdata/ to force that step to
#     re-run.
# ============================================================================

# -------- Options (override by assigning before source()) --------
if (!exists("RUN_WATSON_CONVERT")) RUN_WATSON_CONVERT <- TRUE
if (!exists("STOP_ON_ERROR"))      STOP_ON_ERROR      <- TRUE
# Use the BigQuery-native harmonization rebuild (post-1999 scope, address-
# inferred country codes, built from tls20* PATSTAT tables) rather than the
# legacy local DuckDB version that consumes the inglobe bridges.
if (!exists("USE_BQ_HARMONIZATION")) USE_BQ_HARMONIZATION <- TRUE

# -------- Sanity --------
if (!file.exists("DESCRIPTION"))
  stop("Run this from the iseapp project root (cwd should contain DESCRIPTION).")
if (!dir.exists("data-raw"))
  stop("Expected data-raw/ folder in the current working directory.")

run_step <- function(label, path) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("STEP: ", label, "\n", sep = "")
  cat("      ", path,  "\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
  t0 <- Sys.time()
  tryCatch(
    source(path, echo = FALSE),
    error = function(e) {
      cat("  >>> ERROR in step:", label, "\n")
      cat("  >>>", conditionMessage(e), "\n")
      if (STOP_ON_ERROR) stop(e)
    }
  )
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
  cat(sprintf("STEP done (%.2f min): %s\n", mins, label))
}

pipeline_start <- Sys.time()

# ============================================================================
# 0. One-time Watson DSV -> parquet conversion
# ----------------------------------------------------------------------------
# Converts the 185 per-country innos_ev_<CC>_2009_2018.dsv files in Dropbox
# to parquet siblings. Parquets are ~10x smaller and let build_nationalkey
# run the per-country join as one parallel DuckDB query instead of a 185-
# iteration R loop. Existing parquet files are skipped, so re-runs are
# effectively free once done.
# ============================================================================
if (RUN_WATSON_CONVERT) {
  run_step("Convert Watson DSVs to parquet (one-time)",
           "data-raw/convert_watson_dsvs.R")
} else {
  cat("\nSkipping Watson DSV conversion (RUN_WATSON_CONVERT = FALSE).\n")
}

# ============================================================================
# 1. Harmonize per-person country codes (inventors + holders)
# ----------------------------------------------------------------------------
# For each psn_name in PATSTAT, pick one "best" country based on majority
# vote across (inventor + holder) × family rows. Ties within a family are
# resolved by intersecting the tied candidates with the family-mode
# country/ies (computed across ALL persons in the family — both inventors
# and holders; multiple modes are retained when countries tie for the
# family max). If the intersection is empty, all tied candidates are kept.
# Invalid ISO2 codes (blanks, "SU", "DD", etc.) are filtered out up front.
#
# USE_BQ_HARMONIZATION = TRUE (default): BigQuery-native rebuild from
#   tls201_appln, tls206_person, tls207_pers_appln; restricted to persons
#   linked to applications with earliest_publn_year >= 1999; Tier A
#   address-based country inference for missing ctry codes.
# USE_BQ_HARMONIZATION = FALSE: legacy local DuckDB version that joins
#   persons with the inglobe bridges from Dropbox.
# Both variants write the same output parquets:
#   .bigdata/inventor_countries_harm.parquet
#   .bigdata/holder_countries_harm.parquet
# ============================================================================
if (USE_BQ_HARMONIZATION) {
  run_step("Harmonize inventor/holder country codes (BigQuery)",
           "data-raw/build_inventor_countries_harm_bq.R")
} else {
  run_step("Harmonize inventor/holder country codes (local DuckDB)",
           "data-raw/build_inventor_countries_harm.R")
}

# ============================================================================
# 2. Build countrymap + nationalkey
# ----------------------------------------------------------------------------
# countrymap:
#   - Primary: inventor_countries_harm (optionally + holder_countries_harm
#     if INCLUDE_HOLDERS is set).
#   - Augmented (default, AUGMENT_WITH_WATSON = TRUE): for any docdb_family_id
#     present in Watson's innos_ev_nationalkey_2009_2018 but absent from the
#     primary set, carry over Watson's (family, ctry) pairs. This ensures we
#     don't drop Watson-scored families that happen to not have a harmonized
#     inventor attribution (e.g. pre-1999 families when USE_BQ_HARMONIZATION
#     restricts to post-1999 persons).
#   - Restricted to the 185 countries with Watson per-country EV files.
# nationalkey = countrymap joined with per-country Watson EV values
# (fast path: one DuckDB query over parquet siblings; fallback: DSV loop).
# Outputs: .bigdata/countrymap.fst
#          .bigdata/nationalkey.fst
# ============================================================================
run_step("Build countrymap + nationalkey",
         "data-raw/build_nationalkey.R")

# ============================================================================
# 3. Build the main app parquet database
# ----------------------------------------------------------------------------
# Produces inst/extdata/patent_database.parquet — the single file the Shiny
# app queries via DuckDB. Along the way it also builds the smaller per-
# dimension parquets: patents_x_firm, patents_x_tech, patents_x_region,
# firm_lookup, country_lookup, tech_lookup, region_lookup, inglobe_processed.
# Reads from: Watson DSVs, CPC table (BigQuery, cached), ifcreport.xlsx,
#             firmmap/firmsectormap/regionmap/inglobe from Dropbox, and
#             countrymap.fst produced by step 2. The Espacenet-searchable
#             appln_id per family is sourced from innos_pub.dsv as
#             publn_auth || publn_nr (preferring EP > WO > US).
# ============================================================================
run_step("Build app parquet database",
         "data-raw/01-build-app-parquets.R")

# ============================================================================
# 4. Build filtered citation network parquet
# ----------------------------------------------------------------------------
# Filters <dropbox>/patbis2025/data/fromPATSTAT/citenet_noself.parquet down
# to citations whose BOTH endpoints (citing and cited docdb_family_id) are
# present in inst/extdata/patent_database.parquet. Writes
# inst/extdata/citenet.parquet so the Shiny app can query it via DuckDB.
# ============================================================================
run_step("Build filtered citenet parquet",
         "data-raw-2025/03-build-citenet.R")

# ============================================================================
# 5. Build the Shiny UI sysdata
# ----------------------------------------------------------------------------
# Reads the parquets from step 3 and builds R/sysdata.rda — the internal
# package data loaded automatically whenever the app starts. Contains the
# selectizeInput choices, grouped hierarchies, and boundary geometries.
# ============================================================================
run_step("Build R/sysdata.rda for the app",
         "data-raw/02-build-app-sysdata.R")

# ============================================================================
# Summary
# ============================================================================
total_min <- round(as.numeric(difftime(Sys.time(), pipeline_start,
                                       units = "mins")), 2)
cat("\n", strrep("=", 78), "\n", sep = "")
cat(sprintf("PIPELINE COMPLETE in %.2f minutes.\n", total_min))
cat(strrep("=", 78), "\n", sep = "")
cat("Outputs:\n")
cat("  .bigdata/inventor_countries_harm.parquet\n")
cat("  .bigdata/holder_countries_harm.parquet\n")
cat("  .bigdata/countrymap.fst\n")
cat("  .bigdata/nationalkey.fst\n")
cat("  inst/extdata/patent_database.parquet (+ lookup parquets)\n")
cat("  R/sysdata.rda\n\n")
cat("Next: run/reload the Shiny app to pick up the new data.\n")
