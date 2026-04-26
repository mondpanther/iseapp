#' Run Shiny app as package
#'
#' @importFrom shiny shinyApp
#'
#' @export
runAppPackage <- function() {

  con <- DBI::dbConnect(duckdb::duckdb(
    # dbdir = file.path(getwd(), "inst/cache/ise.duckdb") # use this once during development
    # dbdir = system.file("cache", "ise.duckdb", package = "innovationStrategyExplorer")
  ))
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, "SET enable_http_metadata_cache = true;")
  DBI::dbExecute(con, "SET enable_object_cache = true;")
  # DBI::dbExecute(con, glue::glue("SET s3_region='{Sys.getenv('AWS_DEFAULT_REGION')}';"))
  # DBI::dbExecute(con, glue::glue("SET s3_access_key_id='{Sys.getenv('AWS_ACCESS_KEY_ID')}';"))
  # DBI::dbExecute(con, glue::glue("SET s3_secret_access_key='{Sys.getenv('AWS_SECRET_ACCESS_KEY')}';"))
  # DBI::dbExecute(con, "
  #   CREATE OR REPLACE VIEW full_patent_database AS 
  #   SELECT * FROM read_parquet('s3://iseapp-database/patent_database.parquet')
  # ")

  full_db_path <- system.file("extdata", "patent_database.parquet", package = "innovationStrategyExplorer")

  # The on-disk parquet now stores only ev_* (per-country / per-group flow
  # values) plus the three Watson scalars (cost, alpha, pv) per family.
  # For every ev_<dest> column we derive is_<dest> and av_<dest> on the
  # fly via this DuckDB view. Formulas mirror the original build script:
  #   is_<dest> = ((alpha + 1) / cost) * ev_<dest> * 1[pv <= 2*cost]
  #   av_<dest> = (pv + ev_<dest>) / cost
  # The CASE guard reproduces the previous behaviour of treating rows with
  # missing / zero cost as 0 (after the build-time NA->0 conversion) so
  # downstream `WHERE p.{toflow} IS NOT NULL` filters still pass them.
  ev_targets <- c("nationalkey_2013_2022",
                  "in", "at", "fr", "de", "gb", "cn", "us",
                  "wesbal", "southcau", "eca", "easteur", "ceneur",
                  "cenasia", "oecd", "g7", "euplusuk", "eu", "hic",
                  "emdenocn", "emde", "global")
  derived_sql <- paste(vapply(ev_targets, function(t) {
    sprintf(paste0(
      "  CASE WHEN cost IS NULL OR cost = 0 THEN 0\n",
      "       ELSE ((alpha + 1) / cost) * ev_%s * CAST(pv <= 2 * cost AS DOUBLE)\n",
      "  END AS is_%s,\n",
      "  CASE WHEN cost IS NULL OR cost = 0 THEN 0\n",
      "       ELSE (pv + ev_%s) / cost\n",
      "  END AS av_%s"
    ), t, t, t, t)
  }, character(1)), collapse = ",\n")

  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW full_patent_database AS\nSELECT *,\n%s\nFROM read_parquet('%s')",
    derived_sql, full_db_path
  ))
  
  patents_x_tech_path <- system.file("extdata", "patents_x_tech.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE patents_x_tech AS SELECT * FROM read_parquet('%s')", patents_x_tech_path))

  patents_x_firm_path <- system.file("extdata", "patents_x_firm.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE patents_x_firm AS SELECT * FROM read_parquet('%s')", patents_x_firm_path))

  patents_x_region_path <- system.file("extdata", "patents_x_region.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE patents_x_region AS SELECT * FROM read_parquet('%s')", patents_x_region_path))

  tech_lookup_path <- system.file("extdata", "tech_lookup.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE tech_lookup AS SELECT * FROM read_parquet('%s')", tech_lookup_path))

  firm_lookup_path <- system.file("extdata", "firm_lookup.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE firm_lookup AS SELECT * FROM read_parquet('%s')", firm_lookup_path))

  region_lookup_path <- system.file("extdata", "region_lookup.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE region_lookup AS SELECT * FROM read_parquet('%s')", region_lookup_path))

  country_lookup_path <- system.file("extdata", "country_lookup.parquet", package = "innovationStrategyExplorer")
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE country_lookup AS SELECT * FROM read_parquet('%s')", country_lookup_path))

  citenet_path <- system.file("extdata", "citenet.parquet", package = "innovationStrategyExplorer")
  if (nzchar(citenet_path)) {
    DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW citenet AS SELECT * FROM read_parquet('%s')", citenet_path))
  }

  countrymap_path <- system.file("extdata", "countrymap.parquet", package = "innovationStrategyExplorer")
  if (nzchar(countrymap_path)) {
    DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW countrymap AS SELECT * FROM read_parquet('%s')", countrymap_path))
  }

  shiny::onStop(function() {
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  # Run the Shiny app
  shiny::shinyApp(
    ui = ui,
    server = function(input, output, session) {
      server(input, output, session, con = con)
    },
    enableBookmarking = "url"
  )
}
