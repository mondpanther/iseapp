#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @importFrom bslib bs_theme page_navbar
#' @importFrom pkgload pkg_name
#' @importFrom shiny h1 tabPanel tags
#'
#' @keywords internal
ui <- function(req) {
  bslib::page_navbar(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cerulean"
    ),
    header = list(
      
      shinyjs::useShinyjs(),
      prompter::use_prompt(),
      sever::useSever(),
      waiter::autoWaiter(color = "#ffffff", fadeout = 10),

      shiny::tags$head(
        shiny::tags$link(rel = "icon", href = file.path(pkgload::pkg_name(), "img/main_logo.svg")),
        shiny::tags$link(href = file.path(pkgload::pkg_name(), "css/custom-styles.min.css"), rel = "stylesheet", type = "text/css"),
        shiny::tags$script(src = file.path(pkgload::pkg_name(), "js/custom-js.js"))
      )
    ),
    # footer = shiny::tags$footer(
    #   id = "zrsa-footer",
    #   "Developed by ZevRoss Spatial Analysis, LLC"
    # ),
    collapsible = TRUE,
    window_title = "Shiny Template",
    title = shiny::tags$img(
      src = file.path(pkgload::pkg_name(), "img/main_logo.svg"),
      style = "height: inherit"
    ),
    id = "navbar_page",

    bslib::nav_spacer(),

    shiny::tabPanel(
      "Country Explorer",
      country_module_ui("country")
    ),
    shiny::tabPanel(
      "Region Explorer",
      region_module_ui("region")
    ),
    shiny::tabPanel(
      "Globe",
      globe_module_ui("globe")
    ),
    shiny::tabPanel(
      "About",
      shiny::h1("About Page")
    )
  )
}
