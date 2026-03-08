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
      waiter::use_waiter(),
      waiter::waiter_show_on_load(html = landing_ui_content()),
      # waiter::autoWaiter(color = "#ffffff", fadeout = 10),

      shiny::tags$head(
        shiny::tags$link(rel = "icon", type = "image/svg+xml", href = file.path(pkgload::pkg_name(), "img/favicon.svg")),
        shiny::tags$link(href = file.path(pkgload::pkg_name(), "css/custom-styles.min.css"), rel = "stylesheet", type = "text/css"),
        shiny::tags$script(src = file.path(pkgload::pkg_name(), "js/custom-js.js"))
      )#,

    ),
    # footer = shiny::tags$footer(
    #   id = "zrsa-footer",
    #   "Developed by ZevRoss Spatial Analysis, LLC"
    # ),
    collapsible = TRUE,
    window_title = "Innovation Strategy Explorer",
    title = shiny::tags$div(
      class = "navbar-title-container",
      shiny::tags$div(
        style = "
          width: 40px;
          height: 40px;
          background-color: #2780e3;
          border-radius: 4px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-right: 10px;
        ",
        shiny::tags$span(
          style = "color: white; font-size: 20px; font-weight: bold;",
          "ISE"  # Innovation Strategy
        )
      ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/prinz_logo.png"),
      #   class = "navbar-logo"
      # ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/zrsa_logo.svg"),
      #   class = "navbar-logo"
      # ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/ifc_logo.svg"),
      #   class = "navbar-logo"
      # ),
      shiny::tags$div(
        class = "navbar-separator"
      ),
      shiny::tags$h1(
        "Innovation Strategy Explorer",
        class = "navbar-title-text"
      )
    ),
    id = "navbar_page",

    bslib::nav_spacer(),

    shiny::tabPanel(
      "Country Explorer",
      country_module_ui("country")
    ),
    # shiny::tabPanel(
    #   "Country Explorer (R)",
    #   country2_module_ui("country2")
    # ),
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
