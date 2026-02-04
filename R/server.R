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
  home_module_server("home")
  other_module_server("other")
}
