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
          selected = "istrax_global",
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

      # DUCKDB CONNECTION TO FULL PARQUET DATABASE
      # full_db_path <- system.file("extdata", "full_patent_database.parquet", package = "innovationStrategyExplorer")
      # con <- DBI::dbConnect(duckdb::duckdb())
      # DBI::dbExecute(con, sprintf("CREATE VIEW full_patent_database AS SELECT * FROM read_parquet('%s')", full_db_path))
      # con <- DBI::dbConnect(duckdb::duckdb())
      # DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
      # DBI::dbExecute(con, "
      #   CREATE VIEW full_patent_database AS 
      #   SELECT * FROM read_parquet('https://iseapp-database.s3.us-east-2.amazonaws.com/full_patent_database.parquet')
      # ")

      # Clean up connection on session end
      # shiny::onSessionEnded(function() {
      #   DBI::dbDisconnect(con, shutdown = TRUE)
      # })

      # FALLBACK: DuckDB query for Plot 1 (by-technology)
      fallback_by_tech <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1, input$firm)

        tictoc::tic("SQL fallback_by_tech total")

        toflow             <- input$toflow
        firm               <- input$firm
        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- dplyr::case_when(
          firm == "All Firms" ~ "",
          firm == "No Firm"   ~ "AND firm IS NULL",
          TRUE                ~ glue::glue("AND firm = '{firm}'")
        )

        tech_filters <- build_tech_filter(input$tech_categories_plot1)

        # Handle "All" case — use tech_group as label directly
        use_tech_group_labels <- length(tech_filters) == 1 && names(tech_filters) == "All"

        browser()

        tictoc::tic("sql_country_tech_combined")
        out <- DBI::dbGetQuery(con, sql_country_tech_combined(toflow, country_sql, tech_filters, firm_clause))
        # out <- DBI::dbGetQuery(con, sql_country_tech_combined_v2(toflow, country_sql, tech_filters, firm_clause))
        # base_data <- DBI::dbGetQuery(con, sql_tech_base(toflow, country_sql, tech_filters, firm_clause))
        tictoc::toc()

        if (nrow(out) == 0) return(NULL)

        out <- out |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            greenclass = dplyr::case_when(
              technology %in% colorings$green         ~ "green",
              technology %in% colorings$battery       ~ "battery",
              technology %in% colorings$hard_to_abate ~ "hard to abate",
              technology %in% colorings$ai            ~ "AI",
              technology %in% colorings$agrifood      ~ "agrifood",
              technology %in% colorings$cpcsecs       ~ "cpcsecs",
              TRUE                                    ~ "other"
            )
          )

        # tictoc::tic("R-side aggregation")

        # # Assign display label per row from tech_filters
        # if (use_tech_group_labels) {
        #   base_data <- base_data |> dplyr::mutate(label = tech_group)
        # } else {
        #   base_data <- base_data |>
        #     dplyr::mutate(
        #       label = dplyr::case_when(
        #         !!!purrr::imap(tech_filters, function(filter, lbl) {
        #           if (filter == "") rlang::expr(TRUE ~ !!lbl)
        #           else if (grepl("tech_group", filter)) {
        #             grp <- gsub(".*'(.*)'.*", "\\1", filter)
        #             rlang::expr(tech_group == !!grp ~ !!lbl)
        #           } else {
        #             tech <- gsub(".*'(.*)'.*", "\\1", filter)
        #             rlang::expr(technology == !!tech ~ !!lbl)
        #           }
        #         }),
        #         TRUE ~ technology
        #       )
        #     )
        # }

        # out <- base_data |>
        #   dplyr::group_by(label) |>
        #   dplyr::arrange(dplyr::desc(.data[[toflow]]), .by_group = TRUE) |>
        #   dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
        #   dplyr::summarise(
        #     mean           = mean(.data[[toflow]], na.rm = TRUE),
        #     innos          = dplyr::n(),
        #     sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
        #     q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
        #     q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
        #     q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
        #     top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
        #     top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
        #     top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", "),
        #     .groups        = "drop"
        #   ) |>
        #   dplyr::rename(technology = label) |>
        #   dplyr::mutate(
        #     top3_ids_url = build_espacenet_search(top3_ids),
        #     greenclass   = dplyr::case_when(
        #       technology %in% colorings$green           ~ "green",
        #       technology %in% colorings$battery         ~ "battery",
        #       technology %in% colorings$hard_to_abate   ~ "hard to abate",
        #       technology %in% colorings$ai              ~ "AI",
        #       technology %in% colorings$agrifood        ~ "agrifood",
        #       technology %in% colorings$cpcsecs         ~ "cpcsecs",
        #       TRUE                                      ~ "other"
        #     )
        #   )

        # tictoc::toc()
        tictoc::toc()
        out
      }) |> shiny::bindCache(input$toflow, input$country, input$tech_categories_plot1, input$firm)

      # ============================================================================
      # FALLBACK: DuckDB query for Plot 2 / World Map (by-country)
      # ============================================================================
      fallback_by_country <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$techs, input$firm)

        tictoc::tic("fallback_by_country total")

        selected_countries <- expand_country_selection(input$country)
        toflow             <- input$toflow
        firm               <- input$firm
        techs              <- input$techs
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- dplyr::case_when(
          firm == "All Firms" ~ "",
          firm == "No Firm"   ~ "AND firm IS NULL",
          TRUE                ~ glue::glue("AND firm = '{firm}'")
        )

        tech_clause <- build_tech_clause(techs)

        # browser()

        tictoc::tic("sql_country_base")
        out <- DBI::dbGetQuery(con, sql_country_combined(toflow, country_sql, techs, firm_clause))
        # out <- DBI::dbGetQuery(con, sql_country_combined_v2(toflow, country_sql, techs, firm_clause))
        # base_data <- DBI::dbGetQuery(con, sql_country_base(toflow, country_sql, tech_clause, firm_clause))
        tictoc::toc()

        if (nrow(out) == 0) return(NULL)
        
        firm_input    <- firm
        allinnos_data <- allinnos_baseline |>
          dplyr::filter(ctry_code %in% selected_countries) |>
          dplyr::filter(
            if (firm_input == "All Firms") TRUE
            else if (firm_input == "No Firm") is.na(firm)
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

        # if (nrow(base_data) == 0) return(NULL)
        
        # tictoc::tic("allinnos lookup")
        # firm_input <- firm  # scalar from input, avoids collision with data column name

        # allinnos_data <- allinnos_baseline |>
        #   dplyr::filter(ctry_code %in% selected_countries) |>
        #   dplyr::filter(
        #     if (firm_input == "All Firms") is.na(firm) | !is.na(firm)
        #     else if (firm_input == "No Firm") is.na(firm)
        #     else firm == firm_input
        #   ) |>
        #   dplyr::group_by(ctry_code) |>
        #   dplyr::summarise(allinnos = sum(allinnos), .groups = "drop")

        # sum_allinnos_val <- sum_allinnos_baseline |>
        #   dplyr::filter(
        #     if (firm_input == "All Firms") is.na(firm) | !is.na(firm)
        #     else if (firm_input == "No Firm") is.na(firm)
        #     else firm == firm_input
        #   ) |>
        #   dplyr::pull(sum_allinnos) |>
        #   sum()
        # tictoc::toc()

        # tictoc::tic("R-side aggregation")

        # by_country <- base_data |>
        #   dplyr::group_by(ctry_code) |>
        #   dplyr::arrange(dplyr::desc(.data[[toflow]]), .by_group = TRUE) |>
        #   dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
        #   dplyr::summarise(
        #     mean           = mean(.data[[toflow]], na.rm = TRUE),
        #     innos          = dplyr::n(),
        #     sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
        #     q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
        #     q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
        #     q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
        #     top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
        #     top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
        #     top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", "),
        #     .groups        = "drop"
        #   )

        # # Global "All" row
        # all_row <- base_data |>
        #   dplyr::arrange(dplyr::desc(.data[[toflow]])) |>
        #   dplyr::mutate(rnk = dplyr::row_number(), cnt = dplyr::n()) |>
        #   dplyr::summarise(
        #     ctry_code      = "All",
        #     mean           = mean(.data[[toflow]], na.rm = TRUE),
        #     innos          = dplyr::n(),
        #     sem            = ifelse(dplyr::n() > 1, sd(.data[[toflow]], na.rm = TRUE) / sqrt(dplyr::n()), NA_real_),
        #     q1             = quantile(.data[[toflow]], 0.25, na.rm = TRUE),
        #     q2             = quantile(.data[[toflow]], 0.50, na.rm = TRUE),
        #     q3             = quantile(.data[[toflow]], 0.75, na.rm = TRUE),
        #     top25_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.25), 1)], na.rm = TRUE),
        #     top50_bin_mean = mean(.data[[toflow]][rnk <= max(floor(cnt * 0.50), 1)], na.rm = TRUE),
        #     top3_ids       = paste(docdb_family_id[seq_len(min(3, dplyr::n()))], collapse = ", ")
        #   )

        # out <- dplyr::bind_rows(by_country, all_row) |>
        #   dplyr::left_join(allinnos_data, by = "ctry_code") |>
        #   dplyr::mutate(
        #     top3_ids_url = build_espacenet_search(top3_ids),
        #     top25        = 0.25,
        #     top50        = 0.5,
        #     allinnos     = dplyr::if_else(ctry_code == "All", innos, allinnos),
        #     sum_allinnos = sum_allinnos_val,
        #     share_c      = dplyr::if_else(ctry_code == "All", 1, innos / allinnos),
        #     share        = dplyr::if_else(ctry_code == "All", 1, sum(innos[ctry_code != "All"]) / sum_allinnos_val),
        #     RTA          = dplyr::if_else(ctry_code == "All", 1, 2 * share_c / (share_c + share))
        #   )

        # tictoc::toc()
        tictoc::toc()
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

        is_return <- grepl("strax", input$toflow)

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