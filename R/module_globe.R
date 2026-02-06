#' Globe module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
globe_module_sidebar <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",
    
    # GLOBAL FILTERS section
    shiny::div(
      shiny::h5("GLOBAL FILTERS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectInput(
          ns("country_group"),
          "Country/Group:",
          choices = c("All Countries", "Group A", "Group B", "Group C"),
          selected = "All Countries"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectInput(
          ns("return_flow"),
          "Return Flow:",
          choices = c("Inbound", "Outbound", "Both"),
          selected = "Both"
        )
      )
    ),
    
    # CHART OPTIONS section
    shiny::div(
      shiny::h5("CHART OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("bar_width_scale"),
          "Bar Width Scale:",
          choices = c("Linear", "Logarithmic"),
          selected = "Linear"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("display_mode"),
          "Display Mode:",
          choices = c("Count", "Percentage"),
          selected = "Count"
        )
      )
    )
  )
}

#' Globe module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h1 NS tagList
#'
#' @keywords internal
globe_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      open = TRUE,
      width = 330,
      globe_module_sidebar(id)
    ),
    
    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),
      
      bslib::nav_panel(
        "CHART 1",
        shiny::div(
          style = "padding: 20px;",
          
          # Controls above chart
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                ns("comparison_categories"),
                "COMPARISON CATEGORIES:",
                choices = c("Category A", "Category B", "Category C"),
                selected = "Category A"
              )
            ),
            shiny::column(
              4,
              shiny::sliderInput(
                ns("show_top_n"),
                "SHOW TOP N COUNTRIES:",
                min = 5,
                max = 50,
                value = 10,
                step = 5
              )
            ),
            shiny::column(
              4,
              shiny::sliderInput(
                ns("innovation_threshold"),
                "INNOVATION COUNT THRESHOLD:",
                min = 0,
                max = 100,
                value = 10,
                step = 5
              )
            )
          ),
          
          # Placeholder for chart
          shiny::plotOutput(ns("chart1"), height = "500px")
        )
      ),
      
      bslib::nav_panel(
        "CHART 2",
        shiny::div(
          style = "padding: 20px;",
          shiny::h3("Chart 2 Content"),
          shiny::plotOutput(ns("chart2"), height = "500px")
        )
      ),
      
      bslib::nav_panel(
        "MAP",
        shiny::div(
          style = "padding: 20px;",
          shiny::h3("Map Content"),
          shiny::plotOutput(ns("map"), height = "500px")
        )
      )
    )
  )
}

#' Globe module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer
#'
#' @keywords internal
globe_module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Placeholder outputs
      output$chart1 <- shiny::renderPlot({
        plot(1:10, main = "Chart 1 Placeholder")
      })
      
      output$chart2 <- shiny::renderPlot({
        plot(10:1, main = "Chart 2 Placeholder")
      })
      
      output$map <- shiny::renderPlot({
        plot(rnorm(100), main = "Map Placeholder")
      })
    }
  )
}