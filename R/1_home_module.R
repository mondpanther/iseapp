#' Home module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h1 NS tagList
#'
#' @keywords internal
home_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Home Module")
  )
}

#' Home module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer
#'
#' @keywords internal
home_module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    }
  )
}
