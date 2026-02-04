#' Other module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h1 NS tagList
#'
#' @keywords internal
other_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Other module")
  )
}

#' Other module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer
#'
#' @keywords internal
other_module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    }
  )
}
