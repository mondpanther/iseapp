#' Shiny app Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @keywords internal
server <- function(input, output, session) {

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, glue::glue("SET s3_region='{Sys.getenv('AWS_DEFAULT_REGION')}';"))
  DBI::dbExecute(con, glue::glue("SET s3_access_key_id='{Sys.getenv('AWS_ACCESS_KEY_ID')}';"))
  DBI::dbExecute(con, glue::glue("SET s3_secret_access_key='{Sys.getenv('AWS_SECRET_ACCESS_KEY')}';"))
  DBI::dbExecute(con, "
    CREATE VIEW full_patent_database AS 
    SELECT * FROM read_parquet('s3://iseapp-database/full_patent_database.parquet')
  ")

  # useful for debugging; can comment off if not using
  session_id <- session$token

  # Show sever message & reload button
  sever::sever()

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    
    # Restore main navbar tab
    if (!is.null(query$navbar_page)) {
      tab_name <- gsub('^"|"$', '', query$navbar_page)
      bslib::nav_select(id = "navbar_page", selected = tab_name, session = session)
    }
    
    # Store all params for modules to access
    session$userData$restore_params <- query
  })

  # Call Modules
  country_module_server("country", session, con = con)
  shiny::observeEvent(c(req(input$navbar_page == "Region Explorer")), once = TRUE, {
    region_module_server("region", session, con = con)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Globe")), once = TRUE, {
    globe_module_server("globe", session)
  })
  
}
