#' Country module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
country_module_sidebar <- function(id) {
  ns <- shiny::NS(id)

  # JS conditions for conditional panels based on active inner tab
  tab_id <- ns("inner_tabs")
  is_tech    <- sprintf("input['%s'] == 'Value flows by Technology'", tab_id)
  is_country <- sprintf("input['%s'] == 'Value flows by Country'", tab_id)
  is_rta     <- sprintf("input['%s'] == 'Revealed Technological Advantage'", tab_id)
  not_rta    <- sprintf("input['%s'] != 'Revealed Technological Advantage'", tab_id)
  not_tech   <- sprintf("input['%s'] != 'Value flows by Technology'", tab_id)

  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    # --- Always visible: Country, Firm ---
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
          inputId = ns("firm"),
          label = "Firm or Sector Group",
          choices = firm_grouped_choices,
          selected = "No firm filter",
          multiple = TRUE,
          options = list(placeholder = 'Choose firms or sector groups...')
        )
      ),

    ),

    # --- Value Flow: Tech + Country only ---
    shiny::conditionalPanel(
      condition = not_rta,
      shiny::div(
        shiny::h5("VALUE FLOW", style = "font-weight: 600; margin-bottom: 10px;"),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId = ns("toflow"),
            label = NULL,
            choices = toflow_choices,
            selected = "is_global",
            multiple = FALSE,
            width = "400px",
            options = list(placeholder = 'Choose a value flow...')
          )
        )
      )
    ),

    # --- Technology Categories: Tech only ---
    shiny::conditionalPanel(
      condition = is_tech,
      shiny::div(
        shiny::h5("TECHNOLOGY CATEGORIES", style = "font-weight: 600; margin-bottom: 10px;"),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId = ns("tech_categories_plot1"),
            label = NULL,
            choices = grouped_techs,
            selected = c("AI","Green Technology"),
            multiple = TRUE,
            width = "200%",
            options = list(placeholder = 'Choose one or more technology categories...')
          )
        )
      )
    ),

    # --- Chart options ---
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

      # Display mode: Tech + Country only
      shiny::conditionalPanel(
        condition = not_rta,
        shiny::div(
          class = "side_input",
          shiny::radioButtons(
            inputId = ns("display_mode"),
            label = "Display mode",
            choices = c("Confidence bands" = "confidence", "Returns for the top 25 and top 50 percent" = "quartiles"),
            selected = "confidence"
          )
        )
      ),

      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("top_n_ids"),
          "Number of Top Patent IDs shown",
          value = 10, min = 0, max = 50
        )
      )
    ),

    # --- Country/RTA inputs (not Tech) ---
    shiny::conditionalPanel(
      condition = not_tech,
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
      )
    ),

    # Top N countries + Minimum innovations: Country only
    shiny::conditionalPanel(
      condition = is_country,
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

    # --- RTA OPTIONS: RTA only ---
    shiny::conditionalPanel(
      condition = is_rta,
      shiny::div(
        shiny::h5("RTA OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("topn_rta"), "Show top n countries:", value = 20, min = 1, max = 200)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("bottomn_rta"), "Show bottom n countries:", value = 0, min = 0, max = 200)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("mininno_rta"), "Innovation count threshold:", value = 50, min = 0, max = 500)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("minallinnos_rta"), "All innovation threshold:", value = 100, min = 0, max = 5000)
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

    download_buttons_css(),

    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),

      bslib::nav_panel(
        "Value flows by Technology",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot1"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot1")
        )
      ),

      bslib::nav_panel(
        "Value flows by Country",
        bslib::navset_pill_list(
          widths = c(2, 10),
          bslib::nav_panel(
            "Bar Chart",
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
          )
        )
      ),

      bslib::nav_panel(
        "Revealed Technological Advantage",
        bslib::navset_pill_list(
          widths = c(2, 10),
          bslib::nav_panel(
            "Bar Chart",
            shiny::div(
              ggiraph::girafeOutput(ns("avstrax_plot2_rta"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "avstrax_plot2_rta")
            )
          ),
          bslib::nav_panel(
            "RTA vs Returns",
            shiny::div(
              ggiraph::girafeOutput(ns("rta_returns_scatter"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "rta_returns_scatter")
            )
          ),
          bslib::nav_panel(
            "RTA vs GDP",
            shiny::div(
              ggiraph::girafeOutput(ns("rta_gdp_scatter"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "rta_gdp_scatter")
            )
          ),
          bslib::nav_panel(
            "World Map",
            shiny::div(
              plotly::plotlyOutput(ns("world_map_rta"), width = "100%", height = "auto"),
              map_download_buttons(ns, "world_map_rta")
            )
          )
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

      # Expand "All categories" into all individual broad techs in the selectizeInput
      shiny::observeEvent(input$tech_categories_plot1, {
        if ("All categories" %in% input$tech_categories_plot1) {
          new_sel <- unique(c(setdiff(input$tech_categories_plot1, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "tech_categories_plot1",
                                     selected = new_sel)
        }
      })

      shiny::observeEvent(input$techs, {
        if ("All categories" %in% input$techs) {
          new_sel <- unique(c(setdiff(input$techs, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "techs",
                                     selected = new_sel)
        }
      })

      # When "No firm filter" is selected alongside other firms, keep only "No firm filter"
      # When a firm/sector is selected alongside "No firm filter", drop "No firm filter"
      shiny::observeEvent(input$firm, {
        sel <- input$firm
        if ("No firm filter" %in% sel && length(sel) > 1) {
          # User just added something else — drop "No firm filter"
          new_sel <- setdiff(sel, "No firm filter")
          shiny::updateSelectizeInput(session, "firm", selected = new_sel)
        }
      })

      # DuckDB query for Plot 1 (by-technology)
      fallback_by_tech <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1)

        toflow             <- input$toflow
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))
        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause  <- build_firm_clause_v2(selected_firms, no_filter = no_firm_filter)
        tech_filters <- build_tech_filter_v2(input$tech_categories_plot1)

        sql <- sql_country_tech_combined_v2(toflow, country_sql, tech_filters, firm_clause, top_n_ids = input$top_n_ids)

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

      }) |> shiny::bindCache(input$toflow, input$country, input$tech_categories_plot1,
                             sort(input$firm), input$top_n_ids)

      # DuckDB query for Plot 2 / World Map (by-country)
      fallback_by_country <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$techs)

        selected_countries <- expand_country_selection(input$country)
        toflow             <- input$toflow
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))
        techs              <- input$techs
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- build_firm_clause_v2(selected_firms, no_filter = no_firm_filter)

        out <- DBI::dbGetQuery(con, sql_country_combined_v2(toflow, country_sql, techs, firm_clause, top_n_ids = input$top_n_ids))

        if (nrow(out) == 0) return(NULL)

        allinnos_data <- allinnos_baseline |>
          dplyr::filter(ctry_code %in% selected_countries) |>
          dplyr::filter(
            if (no_firm_filter) TRUE
            else firm %in% selected_firms
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

      }) |> shiny::bindCache(input$toflow, input$country, input$techs,
                             sort(input$firm), input$top_n_ids)

      # Chart 1: Main avstrax plot
      output$avstrax_plot1 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$tech_categories_plot1,
            input$widthscale, input$display_mode, !is.null(input$top_n_ids))

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
          top_n_ids        = input$top_n_ids,
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
            !is.null(input$top_n_ids))

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
          top_n_ids               = input$top_n_ids,
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
          plot_title          = paste0("RTA - ", tech_label),
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
          plot_title   = paste0("RTA vs Returns - ", tech_label),
          x_label      = "Revealed Technological Advantage",
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
        req(input$country, input$toflow, input$techs,
            input$mininno_rta, input$minallinnos_rta)

        avstrax_data <- fallback_by_country()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        avstrax_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_rta)

        if ("Allinnos" %in% names(avstrax_data) && input$minallinnos_rta > 0) {
          avstrax_data <- avstrax_data |>
            dplyr::filter(Allinnos >= input$minallinnos_rta)
        }

        if (nrow(avstrax_data) == 0) return(NULL)

        rta_title <- paste0("RTA - ", paste(input$techs, collapse = ", "))

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