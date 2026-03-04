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
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW full_patent_database AS SELECT * FROM read_parquet('%s')", full_db_path))
  
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
