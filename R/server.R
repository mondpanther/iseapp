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

  # App Server logic here

  # Call Modules
  observeEvent(c(req(input$navbar_page == "Country Explorer")), once = TRUE, {
    country_module_server("country")
  })
  observeEvent(c(req(input$navbar_page == "Region Explorer")), once = TRUE, {
    region_module_server("region")
  })
  observeEvent(c(req(input$navbar_page == "Globe")), once = TRUE, {
    globe_module_server("globe")
  })
  
}
