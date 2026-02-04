#' Run Shiny app as package
#'
#' @importFrom shiny shinyApp
#'
#' @export
runAppPackage <- function() {

  # Run the Shiny app
  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
