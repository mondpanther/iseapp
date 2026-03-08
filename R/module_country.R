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
    ),

    shiny::div(
      shiny::h5("RTA OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("topn_rta"), "RTA: Show top n countries:", value = 20, min = 1, max = 200)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("bottomn_rta"), "RTA: Show bottom n countries:", value = 0, min = 0, max = 200)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("mininno_rta"), "RTA: Innovation count threshold:", value = 0, min = 0, max = 500)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("minallinnos_rta"), "RTA: All innovation threshold:", value = 100, min = 0, max = 5000)
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

    download_buttons_css(),

    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),

      bslib::nav_panel(
        "Returns by Technology",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot1"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot1")
        )
      ),

      bslib::nav_panel(
        "Returns by Country",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot2"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot2")
        )
      ),

      bslib::nav_panel(
        "World Map",
        shiny::div(
          plotly::plotlyOutput(ns("world_map"), width = "100%", height = "auto"),
          map_download_buttons(ns, "world_map")
        )
      ),

      bslib::nav_panel(
        "RTA",
        shiny::div(
          shiny::h3("RTA by Country"),
          ggiraph::girafeOutput(ns("avstrax_plot2_rta"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot2_rta"),
          shiny::tags$br(),
          shiny::h3("RTA vs Returns"),
          ggiraph::girafeOutput(ns("rta_returns_scatter"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "rta_returns_scatter"),
          shiny::tags$br(),
          shiny::h3("RTA vs GDP per Capita"),
          ggiraph::girafeOutput(ns("rta_gdp_scatter"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "rta_gdp_scatter"),
          shiny::tags$br(),
          shiny::h3("World Map: RTA"),
          plotly::plotlyOutput(ns("world_map_rta"), width = "100%", height = "auto"),
          map_download_buttons(ns, "world_map_rta")
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

      # Reactive store for ggplot objects and data (for download handlers)
      plot_store <- shiny::reactiveValues()

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

        # Scale percentage flows (is_*/av_*) from decimal to percent
        if (grepl("^(is_|av_)", toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"), names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

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
          ) |>
          dplyr::rename(Allinnos = allinnos)

        # Scale percentage flows (is_*/av_*) from decimal to percent
        if (grepl("^(is_|av_)", toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"), names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

        out

      }) |> shiny::bindCache(input$toflow, input$country, input$techs, input$firm)

      # Chart 1: Main avstrax plot
      output$avstrax_plot1 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$tech_categories_plot1,
            input$widthscale, input$display_mode, !is.null(input$show_top3_ids))

        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        pdata      <- fallback_by_tech()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        result <- plot_avstrax_by_country(
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

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot1_gg   <- result$ggplot
          plot_store$avstrax_plot1_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })
      
      # Chart 2: Returns by Country for Selected Technologies
      output$avstrax_plot2 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs, input$topn,
            input$mininno, input$widthscale, input$display_mode,
            !is.null(input$show_top3_ids))

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        precomputed_data <- fallback_by_country()

        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        result <- plot_avstrax_by_technology(
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

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_gg   <- result$ggplot
          plot_store$avstrax_plot2_data <- result$plot_data
          result$girafe
        } else {
          result
        }
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

        is_return <- grepl("^(is|av)", input$toflow)
        map_title <- sub("^[^.]*\\.", "", flow_label)

        # Store ggplot version and data for PDF/CSV downloads
        plot_store$world_map_gg <- plot_world_map_gg(
          data       = avstrax_data,
          value_col  = "mean",
          plot_title = map_title,
          is_return  = is_return
        )
        plot_store$world_map_data <- avstrax_data

        plot_world_map(
          avstrax_data = avstrax_data,
          value_col    = "mean",
          color_scale  = "Viridis",
          plot_title   = map_title,
          is_return    = is_return
        )
      })

      # ── RTA Plots ─────────────────────────────────────────────────────────

      # RTA Bar Chart
      output$avstrax_plot2_rta <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs,
            input$topn_rta, input$bottomn_rta,
            input$mininno_rta, input$minallinnos_rta,
            input$widthscale)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        precomputed_data <- fallback_by_country()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        tech_label <- paste(input$techs, collapse = ", ")

        result <- plot_avstrax_rta(
          pdata               = NULL,
          classes             = NULL,
          technologies        = input$techs,
          toflow              = input$toflow,
          custom_colors       = custom_colors,
          topn                = input$topn_rta,
          bottomn             = input$bottomn_rta,
          mininno             = input$mininno_rta,
          minallinnos         = input$minallinnos_rta,
          widthscale          = input$widthscale,
          x_label             = "Country",
          plot_title          = paste0("RTA: ", tech_label, " - ", sub("^[^.]*\\.", "", flow_label)),
          precomputed_avstrax = precomputed_data
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_rta_gg   <- result$ggplot
          plot_store$avstrax_plot2_rta_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })

      # RTA vs Returns Scatter
      output$rta_returns_scatter <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs,
            input$mininno_rta, input$minallinnos_rta,
            input$widthscale)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        precomputed_data <- fallback_by_country()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        tech_label <- paste(input$techs, collapse = ", ")

        result <- plot_rta_returns_scatter(
          avstrax_data = precomputed_data,
          mininno      = input$mininno_rta,
          minallinnos  = input$minallinnos_rta,
          widthscale   = input$widthscale,
          plot_title   = paste0("RTA vs Returns: ", tech_label, " - ", sub("^[^.]*\\.", "", flow_label)),
          x_label      = "RTA",
          y_label      = "Return (%)"
        )

        if (!is.null(result$ggplot)) {
          plot_store$rta_returns_scatter_gg   <- result$ggplot
          plot_store$rta_returns_scatter_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })

      # RTA vs GDP per Capita Scatter
      output$rta_gdp_scatter <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs,
            input$mininno_rta, input$minallinnos_rta,
            input$widthscale)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        precomputed_data <- fallback_by_country()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        tech_label <- paste(input$techs, collapse = ", ")

        result <- plot_rta_gdp_scatter(
          avstrax_data = precomputed_data,
          mininno      = input$mininno_rta,
          minallinnos  = input$minallinnos_rta,
          widthscale   = input$widthscale,
          plot_title   = paste0("RTA vs GDP per Capita: ", tech_label)
        )

        if (!is.null(result$ggplot)) {
          plot_store$rta_gdp_scatter_gg   <- result$ggplot
          plot_store$rta_gdp_scatter_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })

      # World Map: RTA
      output$world_map_rta <- plotly::renderPlotly({
        req(input$country, input$toflow, input$techs, input$mininno_rta)

        avstrax_data <- fallback_by_country()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        avstrax_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_rta)

        if (nrow(avstrax_data) == 0) return(NULL)

        rta_title <- paste0("World Map: RTA - ", paste(input$techs, collapse = ", "))

        # Store ggplot version and data for PDF/CSV downloads
        plot_store$world_map_rta_gg <- plot_world_map_gg(
          data       = avstrax_data,
          value_col  = "RTA",
          plot_title = rta_title,
          is_return  = FALSE
        )
        plot_store$world_map_rta_data <- avstrax_data

        plot_world_map(
          avstrax_data = avstrax_data,
          value_col    = "RTA",
          color_scale  = "RdYlGn",
          plot_title   = rta_title,
          is_return    = FALSE
        )
      })

      # ── Download handlers ──────────────────────────────────────────────────
      # SVG + CSV for girafe plots
      output$dl_svg_avstrax_plot1 <- make_svg_handler(
        reactive(plot_store$avstrax_plot1_gg), "returns_by_technology")
      output$dl_csv_avstrax_plot1 <- make_csv_handler(
        reactive(plot_store$avstrax_plot1_data), "returns_by_technology")

      output$dl_svg_avstrax_plot2 <- make_svg_handler(
        reactive(plot_store$avstrax_plot2_gg), "returns_by_country")
      output$dl_csv_avstrax_plot2 <- make_csv_handler(
        reactive(plot_store$avstrax_plot2_data), "returns_by_country")

      # PDF + CSV for world map
      output$dl_pdf_world_map <- make_pdf_handler(
        reactive(plot_store$world_map_gg), "world_map")
      output$dl_csv_world_map <- make_csv_handler(
        reactive(plot_store$world_map_data), "world_map")

      # RTA plots: SVG + CSV
      output$dl_svg_avstrax_plot2_rta <- make_svg_handler(
        reactive(plot_store$avstrax_plot2_rta_gg), "rta_by_country")
      output$dl_csv_avstrax_plot2_rta <- make_csv_handler(
        reactive(plot_store$avstrax_plot2_rta_data), "rta_by_country")

      output$dl_svg_rta_returns_scatter <- make_svg_handler(
        reactive(plot_store$rta_returns_scatter_gg), "rta_vs_returns")
      output$dl_csv_rta_returns_scatter <- make_csv_handler(
        reactive(plot_store$rta_returns_scatter_data), "rta_vs_returns")

      output$dl_svg_rta_gdp_scatter <- make_svg_handler(
        reactive(plot_store$rta_gdp_scatter_gg), "rta_vs_gdp")
      output$dl_csv_rta_gdp_scatter <- make_csv_handler(
        reactive(plot_store$rta_gdp_scatter_data), "rta_vs_gdp")

      # RTA world map: PDF + CSV
      output$dl_pdf_world_map_rta <- make_pdf_handler(
        reactive(plot_store$world_map_rta_gg), "world_map_rta")
      output$dl_csv_world_map_rta <- make_csv_handler(
        reactive(plot_store$world_map_rta_data), "world_map_rta")

    }
  )
}