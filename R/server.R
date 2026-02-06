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
  country_module_server("country")
  region_module_server("region")
  globe_module_server("globe")
}
