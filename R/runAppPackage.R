#' Run Shiny app as package
#'
#' @importFrom shiny shinyApp
#'
#' @export
runAppPackage <- function() {

  con <- DBI::dbConnect(duckdb::duckdb(
    # dbdir = file.path(getwd(), "inst/cache/ise.duckdb") # use this once during development
    dbdir = system.file("cache", "ise.duckdb", package = "innovationStrategyExplorer")
  ))
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, "SET enable_http_metadata_cache = true;")
  DBI::dbExecute(con, "SET enable_object_cache = true;")
  DBI::dbExecute(con, glue::glue("SET s3_region='{Sys.getenv('AWS_DEFAULT_REGION')}';"))
  DBI::dbExecute(con, glue::glue("SET s3_access_key_id='{Sys.getenv('AWS_ACCESS_KEY_ID')}';"))
  DBI::dbExecute(con, glue::glue("SET s3_secret_access_key='{Sys.getenv('AWS_SECRET_ACCESS_KEY')}';"))
  DBI::dbExecute(con, "
    CREATE OR REPLACE VIEW full_patent_database AS 
    SELECT * FROM read_parquet('s3://iseapp-database/full_patent_database.parquet')
  ")

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
