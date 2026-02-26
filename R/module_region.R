#' Region module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
region_module_sidebar <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    shiny::div(
      shiny::h5("GLOBAL FILTERS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("region"),
          "Region or Group:",
          choices  = grouped_region_choices,
          selected = "All UK regions",
          multiple = TRUE,
          options  = list(placeholder = 'Choose one or more regions...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("firm"),
          "Firm Filter:",
          choices  = firm_choices,
          selected = "All Firms",
          multiple = FALSE,
          options  = list(
            placeholder = 'Choose a firm...',
            server      = TRUE
          )
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("toflow_region"),
          "Return Flow:",
          choices  = toflow_choices,
          selected = "istrax_global",
          multiple = FALSE,
          options  = list(placeholder = 'Choose a return flow...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("tech_categories_plot1_region"),
          "Technology Categories:",
          choices  = grouped_techs,
          selected = c("AI", "Green Technology"),
          multiple = TRUE,
          options  = list(placeholder = 'Choose one or more technology categories...')
        )
      )
    ),

    shiny::div(
      shiny::h5("CHART OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("widthscale_region"),
          "Bar Width Scale:",
          choices  = c("log", "proportional"),
          selected = "log"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("display_mode_region"),
          "Display Mode:",
          choices  = c("Confidence bands" = "confidence",
                       "Returns for the top 25 and top 50 percent" = "quartiles"),
          selected = "confidence"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::checkboxInput(
          ns("show_top3_ids_region"),
          "Show top patent IDs",
          value = TRUE
        )
      )
    ),

    shiny::div(
      shiny::h5("BY REGION / RTA", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("techs_region"),
          "Technology Categories:",
          choices  = grouped_techs,
          selected = "Green Technology",
          multiple = TRUE,
          options  = list(placeholder = 'Choose one or more technology categories...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("techs_comparison_region"),
          "Comparison Categories:",
          choices  = grouped_techs,
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = 'Choose categories to compare...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("topn_region"),        "Show top n regions:",            value = 12,  min = 1,  max = 50)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("mininno_region"),     "Innovation count threshold:",     value = 100, min = 1,  max = 500)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("topn_rta_region"),    "RTA: Show top n regions:",        value = 12,  min = 1,  max = 50)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("bottomn_rta_region"), "RTA: Show bottom n regions:",     value = 0,   min = 0,  max = 50)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("mininno_rta_region"),    "RTA: Innovation count threshold:", value = 100, min = 1, max = 500)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("minallinnos_rta_region"), "RTA: All innovation threshold:",  value = 500, min = 1, max = 5000)
      )
    )
  )
}

#' Region module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h3 NS tagList
#' @importFrom ggiraph girafeOutput
#' @importFrom leaflet leafletOutput
#'
#' @keywords internal
region_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      open = TRUE,
      width = 330,
      region_module_sidebar(id)
    ),
    
    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),
      
      bslib::nav_panel(
        "Returns by Technology",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot1_region"), width = "100%", height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "Returns by Region",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot2_region"), width = "100%", height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "UK Map",
        shiny::div(
          # shiny::h3("UK Regions Map: Returns"),
          leaflet::leafletOutput(ns("uk_regions_map"), width = "100%", height = "500px")
        )
      ),
      
      bslib::nav_panel(
        "RTA",
        shiny::div(
          shiny::h3("RTA by Region"),
          ggiraph::girafeOutput(ns("avstrax_plot2_region_rta"), width = "100%", height = "auto"),
          shiny::tags$br(),
          shiny::h3("RTA vs Returns"),
          ggiraph::girafeOutput(ns("rta_returns_scatter_region"), width = "100%", height = "auto"),
          shiny::tags$br(),
          shiny::h3("RTA by Region Map"),
          leaflet::leafletOutput(ns("uk_regions_map_rta"), width = "100%", height = "500px")
        )
      )
    )
  )
}

#' Region module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer observeEvent req reactive bindCache bindEvent
#' @importFrom ggiraph renderGirafe
#' @importFrom leaflet renderLeaflet
#'
#' @keywords internal
region_module_server <- function(id, parent_session) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Update URL when subtab changes
      shiny::observeEvent(input$inner_tabs, {
        query <- shiny::parseQueryString(parent_session$clientData$url_search)
        query$subtab <- input$inner_tabs
        
        query_string <- paste(names(query), query, sep = "=", collapse = "&")
        shiny::updateQueryString(paste0("?", query_string), 
                        mode = "push", 
                        session = parent_session)
      }, ignoreInit = TRUE)
      
      # ===== DATA SETUP =====
      # Uses the shared con connection from runAppPackage()
      # All UI choices set directly in sidebar — no updateSelectizeInput needed

      full_db_path <- system.file("extdata", "full_patent_database.parquet", package = "innovationStrategyExplorer")
      con <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbExecute(con, sprintf("CREATE VIEW full_patent_database AS SELECT * FROM read_parquet('%s')", full_db_path))

      shiny::onSessionEnded(function() {
        DBI::dbDisconnect(con, shutdown = TRUE)
      })
      
      fallback_by_tech_region <- shiny::reactive({
        shiny::req(input$toflow_region, input$region, input$tech_categories_plot1_region, input$firm)

        toflow             <- input$toflow_region
        firm               <- input$firm
        selected_regions   <- expand_region_selection(input$region)
        region_sql         <- paste0("'", selected_regions, "'", collapse = ", ")

        firm_clause <- dplyr::case_when(
          firm == "All Firms" ~ "",
          firm == "No Firm"   ~ "AND firm IS NULL",
          TRUE                ~ glue::glue("AND firm = '{firm}'")
        )

        tech_filters <- build_tech_filter(input$tech_categories_plot1_region)
        use_tech_group_labels <- length(tech_filters) == 1 && names(tech_filters) == "All"

        base_data <- DBI::dbGetQuery(con, sql_region_tech_base(toflow, region_sql, tech_filters, firm_clause))

        if (nrow(base_data) == 0) return(NULL)

        if (use_tech_group_labels) {
          base_data <- base_data |> dplyr::mutate(label = tech_group)
        } else {
          base_data <- base_data |>
            dplyr::mutate(
              label = dplyr::case_when(
                !!!purrr::imap(tech_filters, function(filter, lbl) {
                  if (filter == "") rlang::expr(TRUE ~ !!lbl)
                  else if (grepl("tech_group", filter)) {
                    grp <- gsub(".*'(.*)'.*", "\\1", filter)
                    rlang::expr(tech_group == !!grp ~ !!lbl)
                  } else {
                    tech <- gsub(".*'(.*)'.*", "\\1", filter)
                    rlang::expr(technology == !!tech ~ !!lbl)
                  }
                }),
                TRUE ~ technology
              )
            )
        }

        out <- base_data |>
          dplyr::group_by(label) |>
          dplyr::arrange(dplyr::desc(.data[[toflow]]), .by_group = TRUE) |>
          dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
          dplyr::summarise(
            mean           = mean(.data[[toflow]], na.rm = TRUE),
            innos          = dplyr::n(),
            sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
            q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
            q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
            q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
            top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
            top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
            top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", "),
            .groups        = "drop"
          ) |>
          dplyr::rename(technology = label) |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            greenclass   = dplyr::case_when(
              technology %in% colorings$green           ~ "green",
              technology %in% colorings$battery         ~ "battery",
              technology %in% colorings$hard_to_abate   ~ "hard to abate",
              technology %in% colorings$ai              ~ "AI",
              technology %in% colorings$agrifood        ~ "agrifood",
              technology %in% colorings$cpcsecs         ~ "cpcsecs",
              TRUE                                      ~ "other"
            )
          )

        out
      }) |> shiny::bindCache(input$toflow_region, input$region, input$tech_categories_plot1_region, input$firm)

      fallback_by_region <- shiny::reactive({
        shiny::req(input$toflow_region, input$region, input$techs_region, input$firm)

        toflow           <- input$toflow_region
        firm             <- input$firm
        selected_regions <- expand_region_selection(input$region)
        region_sql       <- paste0("'", selected_regions, "'", collapse = ", ")

        firm_clause <- dplyr::case_when(
          firm == "All Firms" ~ "",
          firm == "No Firm"   ~ "AND firm IS NULL",
          TRUE                ~ glue::glue("AND firm = '{firm}'")
        )

        tech_clause <- build_tech_clause(input$techs_region)

        base_data <- DBI::dbGetQuery(con, sql_region_base(toflow, region_sql, tech_clause, firm_clause))

        if (nrow(base_data) == 0) return(NULL)

        firm_input        <- firm
        allinnos_data     <- allinnos_region_baseline |>
          dplyr::filter(region_code %in% selected_regions) |>
          dplyr::filter(
            if (firm_input == "All Firms") is.na(firm) | !is.na(firm)
            else if (firm_input == "No Firm") is.na(firm)
            else firm == firm_input
          ) |>
          dplyr::group_by(region_code) |>
          dplyr::summarise(allinnos = sum(allinnos), .groups = "drop")

        sum_allinnos_val  <- sum_allinnos_region_baseline |>
          dplyr::filter(
            if (firm_input == "All Firms") is.na(firm) | !is.na(firm)
            else if (firm_input == "No Firm") is.na(firm)
            else firm == firm_input
          ) |>
          dplyr::pull(sum_allinnos) |>
          sum()

        by_region <- base_data |>
          dplyr::group_by(region_code) |>
          dplyr::arrange(dplyr::desc(.data[[toflow]]), .by_group = TRUE) |>
          dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
          dplyr::summarise(
            mean           = mean(.data[[toflow]], na.rm = TRUE),
            innos          = dplyr::n(),
            sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
            q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
            q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
            q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
            top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
            top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
            top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", "),
            .groups        = "drop"
          )

        all_row <- base_data |>
          dplyr::arrange(dplyr::desc(.data[[toflow]])) |>
          dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
          dplyr::summarise(
            region_code    = "All",
            mean           = mean(.data[[toflow]], na.rm = TRUE),
            innos          = dplyr::n(),
            sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
            q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
            q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
            q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
            top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
            top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
            top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", ")
          )

        dplyr::bind_rows(by_region, all_row) |>
          dplyr::left_join(allinnos_data, by = "region_code") |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            top25        = 0.25,
            top50        = 0.5,
            allinnos     = dplyr::if_else(region_code == "All", innos, allinnos),
            sum_allinnos = sum_allinnos_val,
            share_c      = dplyr::if_else(region_code == "All", 1, innos / allinnos),
            share        = dplyr::if_else(region_code == "All", 1, sum(innos[region_code != "All"]) / sum_allinnos_val),
            RTA          = dplyr::if_else(region_code == "All", 1, 2 * share_c / (share_c + share)),
            # Rename for plotting functions
            ctry_code    = region_code,
            country_name = uk_regions[region_code],
            Allinnos     = allinnos
          )

      }) |> shiny::bindCache(input$toflow_region, input$region, input$techs_region, input$firm)
      # ===== RENDER OUTPUTS =====
      
      # Plot 1: Returns by Technology
      output$avstrax_plot1_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$tech_categories_plot1_region,
                  input$widthscale_region, input$display_mode_region, !is.null(input$show_top3_ids_region))

        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        pdata      <- fallback_by_tech_region()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        plot_avstrax_by_country(
          pdata            = pdata,
          toflow           = input$toflow_region,
          custom_colors    = custom_colors,
          colorings        = colorings,
          widthscale       = input$widthscale_region,
          display_mode     = input$display_mode_region,
          show_top3_ids    = input$show_top3_ids_region,
          plot_title       = sub("^[^.]*\\.", "", flow_label),
          precomputed_data = pdata
        )
      })
      
      # Plot 2: Returns by Region
      output$avstrax_plot2_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                  input$topn_region, input$mininno_region,
                  input$widthscale_region, input$display_mode_region,
                  !is.null(input$show_top3_ids_region))

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        precomputed_data <- fallback_by_region()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        plot_avstrax_by_technology(
          pdata                   = data.frame(),
          classes                 = NULL,
          technologies            = input$techs_region,
          toflow                  = input$toflow_region,
          custom_colors           = custom_colors,
          topn                    = input$topn_region,
          mininno                 = input$mininno_region,
          widthscale              = input$widthscale_region,
          display_mode            = input$display_mode_region,
          show_top3_ids           = input$show_top3_ids_region,
          x_label                 = "Region",
          plot_title              = sub("^[^.]*\\.", "", flow_label),
          comparison_technologies = input$techs_comparison_region,
          precomputed_avstrax     = precomputed_data
        )
      })
      
      # UK Map: Returns
      output$uk_regions_map <- leaflet::renderLeaflet({
        shiny::req(input$region, input$toflow_region, input$techs_region, input$mininno_region)

        avstrax_data <- fallback_by_region()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        map_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_region)

        if (nrow(map_data) == 0) return(NULL)

        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col    = "mean",
          plot_title   = paste0("Returns: ", paste(input$techs_region, collapse = ", ")),
          is_return    = grepl("strax", input$toflow_region)
        )
      })
      
      # RTA Plot: Returns by Region (RTA version)
      output$avstrax_plot2_region_rta <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                  input$topn_rta_region, input$bottomn_rta_region,
                  input$mininno_rta_region, input$minallinnos_rta_region,
                  input$widthscale_region)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        precomputed_data <- fallback_by_region()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        plot_avstrax_rta(
          pdata               = NULL,
          classes             = NULL,
          technologies        = input$techs_region,
          toflow              = input$toflow_region,
          custom_colors       = custom_colors,
          topn                = input$topn_rta_region,
          bottomn             = input$bottomn_rta_region,
          mininno             = input$mininno_rta_region,
          minallinnos         = input$minallinnos_rta_region,
          widthscale          = input$widthscale_region,
          x_label             = "Region",
          plot_title          = paste0("RTA: ", paste(input$techs_region, collapse = ", "), " - ", sub("^[^.]*\\.", "", flow_label)),
          precomputed_avstrax = precomputed_data
        )
      })
      
      # RTA Scatter: RTA vs Returns
      output$rta_returns_scatter_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                  input$mininno_rta_region, input$minallinnos_rta_region,
                  input$widthscale_region)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        precomputed_data <- fallback_by_region()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        plot_rta_returns_scatter(
          avstrax_data = precomputed_data,
          mininno      = input$mininno_rta_region,
          minallinnos  = input$minallinnos_rta_region,
          widthscale   = input$widthscale_region,
          plot_title   = paste0("RTA vs Returns: ", paste(input$techs_region, collapse = ", "), " - ", sub("^[^.]*\\.", "", flow_label)),
          x_label      = "RTA",
          y_label      = "Return (%)"
        )
      })
      
      # UK Map: RTA
      output$uk_regions_map_rta <- leaflet::renderLeaflet({
        shiny::req(input$region, input$toflow_region, input$techs_region, input$mininno_rta_region)

        avstrax_data <- fallback_by_region()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        map_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_rta_region)

        if (nrow(map_data) == 0) return(NULL)

        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col    = "RTA",
          plot_title   = paste0("RTA: ", paste(input$techs_region, collapse = ", ")),
          is_return    = FALSE
        )
      })

    }
  )
}