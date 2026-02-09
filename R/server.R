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

  # useful for debugging; can comment off if not using
  session_id <- session$token

  # Show sever message & reload button
  sever::sever()

  # # Parse URL on app load
  # shiny::observe({
  #   query <- shiny::parseQueryString(session$clientData$url_search)
    
  #   if (!is.null(query$tab)) {
  #     bslib::nav_select(id = "navbar_page", selected = query$tab, session = session)
  #   }
  # })

  # # Handle URL parameter restoration
  # shiny::observeEvent(input$url_params_restore, {
  #   browser()
  #   params <- input$url_params_restore
    
  #   # Restore main tab first
  #   if (!is.null(params$navbar_page)) {
  #     tab_name <- gsub('^"|"$', '', params$navbar_page)
  #     bslib::nav_select(id = "navbar_page", selected = tab_name, session = session)
  #   }
    
  #   # Send restoration signal to modules with all params
  #   session$userData$restore_params <- params
  # }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Call Modules
  shiny::observeEvent(c(req(input$navbar_page == "Country Explorer")), once = TRUE, {
    country_module_server("country", session)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Region Explorer")), once = TRUE, {
    region_module_server("region", session)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Globe")), once = TRUE, {
    globe_module_server("globe", session)
  })
  
}
