#' Country module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
country_module_sidebar <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",
    
    shiny::div(
      shiny::h5("GLOBAL FILTERS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("country"),
          label = "Country or Group",
          choices = grouped_choices,
          selected = "All countries",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more countries or groups...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("firm"),
          "Firm Filter:",
          choices = firm_choices,
          selected = "All Firms",
          multiple = FALSE,
          options = list(
            placeholder = 'Choose a firm...',
            server = TRUE
          )
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("toflow"),
          label = "Return flow",
          choices = toflow_choices,
          selected = "is_global",
          multiple = FALSE,
          width = "400px",
          options = list(placeholder = 'Choose a return flow...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("tech_categories_plot1"),
          label = "Technology categories",
          choices = grouped_techs,
          selected = c("AI","Green Technology"),
          multiple = TRUE,
          width = "200%",
          options = list(placeholder = 'Choose one or more technology categories...')
        )
      )
    ),
    
    shiny::div(
      shiny::h5("CHART OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          inputId = ns("widthscale"),
          label = "Bar width scale",
          choices = c("log", "proportional"),
          selected = "log"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          inputId = ns("display_mode"),
          label = "Display mode",
          choices = c("Confidence bands" = "confidence", "Returns for the top 25 and top 50 percent" = "quartiles"),
          selected = "confidence"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::checkboxInput(
          ns("show_top3_ids"),
          "Show Top Patent IDs",
          value = TRUE
        )
      )
    ),
    shiny::div(
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("techs"),
          label = "Technologies for country/map view",
          choices = grouped_techs,
          selected = "Green Technology",
          multiple = TRUE,
          options = list(placeholder = 'Choose technologies...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("topn"),
          "Top N countries",
          value = 20,
          min = 1,
          max = 50
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("mininno"),
          "Minimum innovations",
          value = 5,
          min = 1
        )
      )
    )
  )
}

#' Country module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h1 NS tagList
#'
#' @keywords internal
country_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      open = TRUE,
      width = 330,
      country_module_sidebar(id)
    ),
    
    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),
      
      bslib::nav_panel(
        "Returns by Technology",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot1"), width = "100%", height = "auto")
        )
      ),

      bslib::nav_panel(
        "Returns by Country",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot2"), width = "100%", height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "World Map",
        shiny::div(
          plotly::plotlyOutput(ns("world_map"), width = "100%", height = "auto")
        )
      )
    )
  )
}

#' Country module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer observeEvent observe req reactive reactiveValues bindEvent invalidateLater parseQueryString updateQueryString
#'
#' @keywords internal
country_module_server <- function(id, parent_session, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # DuckDB query for Plot 1 (by-technology)
      fallback_by_tech <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1, input$firm)

        toflow             <- input$toflow
        firm               <- input$firm
        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause  <- build_firm_clause_v2(firm)
        tech_filters <- build_tech_filter_v2(input$tech_categories_plot1)

        sql <- sql_country_tech_combined_v2(toflow, country_sql, tech_filters, firm_clause)

        out <- DBI::dbGetQuery(con, sql)

        if (nrow(out) == 0) {
          return(NULL)
        }

        out <- out |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            greenclass = dplyr::case_when(
              technology == "Green Technology"                      ~ "green",
              technology == "Battery Technology"                    ~ "battery",
              technology == "Hard to Abate Sector Decarbonization" ~ "hard to abate",
              technology == "AI"                                    ~ "AI",
              technology == "Any Agriculture & Food technology"     ~ "agrifood",
              technology %in% colorings$green                      ~ "green",
              technology %in% colorings$battery                    ~ "battery",
              technology %in% colorings$hard_to_abate              ~ "hard to abate",
              technology %in% colorings$ai                         ~ "AI",
              technology %in% colorings$agrifood                   ~ "agrifood",
              technology %in% colorings$cpcsecs                    ~ "cpcsecs",
              TRUE                                                  ~ "other"
            )
          )

        out

      }) |> shiny::bindCache(input$toflow, input$country, input$tech_categories_plot1, input$firm)

      # DuckDB query for Plot 2 / World Map (by-country)
      fallback_by_country <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$techs, input$firm)

        selected_countries <- expand_country_selection(input$country)
        toflow             <- input$toflow
        firm               <- input$firm
        techs              <- input$techs
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- build_firm_clause_v2(firm)

        tech_clause <- build_tech_clause_v2(techs)

        out <- DBI::dbGetQuery(con, sql_country_combined_v2(toflow, country_sql, techs, firm_clause))

        if (nrow(out) == 0) return(NULL)
        
        firm_input    <- firm
        allinnos_data <- allinnos_baseline |>
          dplyr::filter(ctry_code %in% selected_countries) |>
          dplyr::filter(
            if (firm_input %in% c("All", "All Firms")) TRUE
            else if (firm_input == "None") is.na(firm)
            else firm == firm_input
          ) |>
          dplyr::group_by(ctry_code) |>
          dplyr::summarise(allinnos = sum(allinnos), .groups = "drop")

        sum_allinnos_val <- sum(allinnos_data$allinnos)

        out <- out |>
          dplyr::left_join(allinnos_data, by = "ctry_code") |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            top25        = 0.25,
            top50        = 0.5,
            allinnos     = dplyr::if_else(ctry_code == "All", innos, allinnos),
            share_c      = dplyr::if_else(ctry_code == "All", 1, innos / allinnos),
            share        = dplyr::if_else(ctry_code == "All", 1, sum(innos[ctry_code != "All"]) / sum_allinnos_val),
            RTA          = dplyr::if_else(ctry_code == "All", 1, 2 * share_c / (share_c + share))
          )

        out

      }) |> shiny::bindCache(input$toflow, input$country, input$techs, input$firm)
      
      # Chart 1: Main avstrax plot
      output$avstrax_plot1 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$tech_categories_plot1, 
            input$widthscale, input$display_mode, !is.null(input$show_top3_ids))

        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        pdata      <- fallback_by_tech()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        plot_avstrax_by_country(
          pdata            = pdata,
          toflow           = input$toflow,
          custom_colors    = custom_colors,
          colorings        = colorings,
          widthscale       = input$widthscale,
          display_mode     = input$display_mode,
          show_top3_ids    = input$show_top3_ids,
          plot_title       = sub("^[^.]*\\.", "", flow_label),
          precomputed_data = pdata
        )
      })
      
      # Chart 2: Returns by Country for Selected Technologies
      output$avstrax_plot2 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs, input$topn,
            input$mininno, input$widthscale, input$display_mode,
            !is.null(input$show_top3_ids))

        flow_label     <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        precomputed_data <- fallback_by_country()

        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        plot_avstrax_by_technology(
          pdata                   = data.frame(),
          classes                 = NULL,
          technologies            = input$techs,
          toflow                  = input$toflow,
          custom_colors           = custom_colors,
          topn                    = input$topn,
          mininno                 = input$mininno,
          widthscale              = input$widthscale,
          display_mode            = input$display_mode,
          show_top3_ids           = input$show_top3_ids,
          plot_title              = sub("^[^.]*\\.", "", flow_label),
          comparison_technologies = input$techs_comparison,
          precomputed_avstrax     = precomputed_data
        )
      })
      
      # World Map
      output$world_map <- plotly::renderPlotly({
        req(input$country, input$toflow, input$techs, input$mininno)

        flow_label   <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        avstrax_data <- fallback_by_country()

        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        avstrax_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno)

        if (nrow(avstrax_data) == 0) return(NULL)
        
        is_return <- grepl("^is", input$toflow)

        plot_world_map(
          avstrax_data = avstrax_data,
          value_col    = "mean",
          color_scale  = "Viridis",
          plot_title   = sub("^[^.]*\\.", "", flow_label),
          is_return    = is_return
        )
      })

    }
  )
}