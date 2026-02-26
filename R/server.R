#' Shiny app Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @keywords internal
server <- function(input, output, session, con) {

  startup_waiter <- waiter::Waiter$new(html = landing_ui_content())
  startup_waiter$show()

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
  landing_ready <- landing_module_server("landing", waiter = startup_waiter, con = con)
  shiny::observeEvent(landing_ready(), once = TRUE, {
    country_module_server("country", session, con = con)
  })
  shiny::observeEvent(landing_ready(), once = TRUE, {
    country2_module_server("country2", session, con = con)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Region Explorer")), once = TRUE, {
    region_module_server("region", session, con = con)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Globe")), once = TRUE, {
    globe_module_server("globe", session)
  })
  
}
