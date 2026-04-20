#' Region module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
region_module_sidebar <- function(id) {
  ns <- shiny::NS(id)

  # JS conditions for conditional panels based on active inner tab
  tab_id <- ns("inner_tabs")
  is_tech   <- sprintf("input['%s'] == 'Value flows by Technology'", tab_id)
  is_region <- sprintf("input['%s'] == 'Value flows by Region'", tab_id)
  is_rta    <- sprintf("input['%s'] == 'Revealed Technological Advantage'", tab_id)
  not_rta   <- sprintf("input['%s'] != 'Revealed Technological Advantage'", tab_id)
  not_tech  <- sprintf("input['%s'] != 'Value flows by Technology'", tab_id)

  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    # --- Always visible: Region, Firm ---
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
          inputId = ns("firm"),
          label = "Firm or Sector Group",
          choices = firm_grouped_choices,
          selected = "No firm filter",
          multiple = TRUE,
          options = list(placeholder = 'Choose firms or sector groups...')
        )
      ),
      shiny::conditionalPanel(
        condition = not_tech,
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            ns("techs_region"),
            "Technologies included",
            choices  = grouped_techs,
            selected = "Green Technology",
            multiple = TRUE,
            options  = list(placeholder = 'Choose one or more technology categories...')
          )
        )
      ),

    ),

    # --- Value Flow: Tech + Region only ---
    shiny::conditionalPanel(
      condition = not_rta,
      shiny::div(
        shiny::h5("VALUE FLOW", style = "font-weight: 600; margin-bottom: 10px;"),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            ns("toflow_region"),
            label = NULL,
            choices  = toflow_choices,
            selected = "is_global",
            multiple = FALSE,
            options  = list(placeholder = 'Choose a value flow...')
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
            ns("tech_categories_plot1_region"),
            label = NULL,
            choices  = grouped_techs,
            selected = c("AI", "Green Technology"),
            multiple = TRUE,
            options  = list(placeholder = 'Choose one or more technology categories...')
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
          ns("widthscale_region"),
          "Bar Width Scale:",
          choices  = c("log", "proportional"),
          selected = "log"
        )
      ),

      # Display mode: Tech + Region only
      shiny::conditionalPanel(
        condition = not_rta,
        shiny::div(
          class = "side_input",
          shiny::radioButtons(
            ns("display_mode_region"),
            "Display Mode:",
            choices  = c("Confidence bands" = "confidence",
                         "Returns for the top 25 and top 50 percent" = "quartiles"),
            selected = "confidence"
          )
        )
      ),

      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("top_n_ids_region"),
          "Number of Top Patent IDs shown",
          value = 10, min = 0, max = 50
        )
      )
    ),

    # --- Region/RTA inputs (not Tech) ---
    shiny::conditionalPanel(
      condition = not_tech,
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
      )
    ),

    # Top N regions + Innovation threshold: Region only
    shiny::conditionalPanel(
      condition = is_region,
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("topn_region"), "Top N regions:", value = 12, min = 1, max = 50)
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(ns("mininno_region"), "Minimum innovations:", value = 10, min = 1, max = 500)
      )
    ),

    # --- RTA OPTIONS: RTA only ---
    shiny::conditionalPanel(
      condition = is_rta,
      shiny::div(
        shiny::h5("RTA OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("topn_rta_region"), "Show top n regions:", value = 12, min = 1, max = 50)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("bottomn_rta_region"), "Show bottom n regions:", value = 0, min = 0, max = 50)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("mininno_rta_region"), "Innovation count threshold:", value = 0, min = 0, max = 500)
        ),
        shiny::div(
          class = "side_input",
          shiny::numericInput(ns("minallinnos_rta_region"), "All innovation threshold:", value = 100, min = 0, max = 5000)
        )
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

    download_buttons_css(),

    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),

      bslib::nav_panel(
        "Value flows by Technology",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot1_region"), width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot1_region")
        )
      ),

      bslib::nav_panel(
        "Value flows by Region",
        bslib::navset_pill_list(
          widths = c(2, 10),
          bslib::nav_panel(
            "Bar Chart",
            shiny::div(
              ggiraph::girafeOutput(ns("avstrax_plot2_region"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "avstrax_plot2_region")
            )
          ),
          bslib::nav_panel(
            "UK Map",
            shiny::div(
              leaflet::leafletOutput(ns("uk_regions_map"), width = "100%", height = "500px"),
              map_download_buttons(ns, "uk_regions_map")
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
              ggiraph::girafeOutput(ns("avstrax_plot2_region_rta"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "avstrax_plot2_region_rta")
            )
          ),
          bslib::nav_panel(
            "RTA vs Returns",
            shiny::div(
              ggiraph::girafeOutput(ns("rta_returns_scatter_region"), width = "100%", height = "auto"),
              plot_download_buttons(ns, "rta_returns_scatter_region")
            )
          ),
          bslib::nav_panel(
            "UK Map",
            shiny::div(
              leaflet::leafletOutput(ns("uk_regions_map_rta"), width = "100%", height = "500px"),
              map_download_buttons(ns, "uk_regions_map_rta")
            )
          )
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
region_module_server <- function(id, parent_session, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Reactive store for ggplot objects and data (for download handlers)
      plot_store <- shiny::reactiveValues()

      # Expand "All categories" into all individual broad techs in the selectizeInput
      shiny::observeEvent(input$tech_categories_plot1_region, {
        if ("All categories" %in% input$tech_categories_plot1_region) {
          new_sel <- unique(c(setdiff(input$tech_categories_plot1_region, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "tech_categories_plot1_region",
                                     selected = new_sel)
        }
      })

      shiny::observeEvent(input$techs_region, {
        if ("All categories" %in% input$techs_region) {
          new_sel <- unique(c(setdiff(input$techs_region, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "techs_region",
                                     selected = new_sel)
        }
      })

      # When "No firm filter" is selected alongside other firms, drop "No firm filter"
      shiny::observeEvent(input$firm, {
        sel <- input$firm
        if ("No firm filter" %in% sel && length(sel) > 1) {
          new_sel <- setdiff(sel, "No firm filter")
          shiny::updateSelectizeInput(session, "firm", selected = new_sel)
        }
      })

      # Update URL when subtab changes
      shiny::observeEvent(input$inner_tabs, {
        query <- shiny::parseQueryString(parent_session$clientData$url_search)
        query$subtab <- input$inner_tabs
        
        query_string <- paste(names(query), query, sep = "=", collapse = "&")
        shiny::updateQueryString(paste0("?", query_string), 
                        mode = "push", 
                        session = parent_session)
      }, ignoreInit = TRUE)

      fallback_by_region <- shiny::reactive({
        shiny::req(input$toflow_region, input$region, input$techs_region)

        toflow           <- input$toflow_region
        no_firm_filter   <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms   <- expand_firm_selection(setdiff(input$firm, "No firm filter"))
        selected_regions <- expand_region_selection(input$region)
        region_sql       <- paste0("'", selected_regions, "'", collapse = ", ")

        firm_clause <- build_firm_clause_v2(selected_firms, no_filter = no_firm_filter)

        out <- DBI::dbGetQuery(con, sql_region_combined_v2(toflow, region_sql, input$techs_region, firm_clause, top_n_ids = input$top_n_ids_region))

        if (nrow(out) == 0) return(NULL)

        allinnos_data <- allinnos_region_baseline |>
          dplyr::filter(region_code %in% selected_regions) |>
          dplyr::filter(
            if (no_firm_filter) TRUE
            else firm %in% selected_firms
          ) |>
          dplyr::group_by(region_code) |>
          dplyr::summarise(allinnos = sum(allinnos), .groups = "drop")

        sum_allinnos_val <- sum_allinnos_region_firm_baseline |>
          dplyr::filter(
            if (no_firm_filter) TRUE
            else firm %in% selected_firms
          ) |>
          dplyr::pull(sum_allinnos) |>
          sum()

        out <- out |>
          dplyr::rename(ctry_code = region_code) |>
          dplyr::left_join(allinnos_data |> dplyr::rename(ctry_code = region_code), by = "ctry_code") |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            top25        = 0.25,
            top50        = 0.5,
            allinnos     = dplyr::if_else(ctry_code == "All", innos, allinnos),
            share_c      = dplyr::if_else(ctry_code == "All", 1, innos / allinnos),
            share        = dplyr::if_else(ctry_code == "All", 1, sum(innos[ctry_code != "All"]) / sum_allinnos_val),
            RTA          = dplyr::if_else(ctry_code == "All", 1, 2 * share_c / (share_c + share)),
            country_name = dplyr::if_else(ctry_code == "All", "All", uk_regions[ctry_code])
          ) |>
          dplyr::rename(Allinnos = allinnos)

        # Scale percentage flows (is_*/av_*) from decimal to percent
        if (grepl("^(is_|av_)", toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"), names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

        out

      }) |> shiny::bindCache(input$toflow_region, input$region, input$techs_region,
                             sort(input$firm), input$top_n_ids_region)

      fallback_by_tech_region <- shiny::reactive({
        shiny::req(input$toflow_region, input$region, input$tech_categories_plot1_region)

        toflow             <- input$toflow_region
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))
        selected_regions   <- expand_region_selection(input$region)
        region_sql         <- paste0("'", selected_regions, "'", collapse = ", ")

        firm_clause <- build_firm_clause_v2(selected_firms, no_filter = no_firm_filter)

        tech_filters <- build_tech_filter_v2(input$tech_categories_plot1_region)

        use_tech_group_labels <- length(tech_filters) == 1 && names(tech_filters) == "All"

        out <- DBI::dbGetQuery(con, sql_region_tech_combined_v2(toflow, region_sql, tech_filters, firm_clause, top_n_ids = input$top_n_ids_region))

        if (nrow(out) == 0) return(NULL)

        out <- out |>
          dplyr::mutate(
            top3_ids_url = build_espacenet_search(top3_ids),
            greenclass = dplyr::case_when(
              technology == "Green Technology"                          ~ "green",
              technology == "Battery Technology"                        ~ "battery",
              technology == "Hard to Abate Sector Decarbonization"      ~ "hard to abate",
              technology == "AI"                                        ~ "AI",
              technology == "Any Agriculture & Food technology"         ~ "agrifood",
              technology %in% colorings$green                          ~ "green",
              technology %in% colorings$battery                        ~ "battery",
              technology %in% colorings$hard_to_abate                  ~ "hard to abate",
              technology %in% colorings$ai                             ~ "AI",
              technology %in% colorings$agrifood                       ~ "agrifood",
              technology %in% colorings$cpcsecs                        ~ "cpcsecs",
              TRUE                                                      ~ "other"
            )
          )

        # Scale percentage flows (is_*/av_*) from decimal to percent
        if (grepl("^(is_|av_)", toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"), names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

        out

      }) |> shiny::bindCache(input$toflow_region, input$region, input$tech_categories_plot1_region,
                             sort(input$firm), input$top_n_ids_region)
      # ===== RENDER OUTPUTS =====
      
      # Plot 1: Returns by Technology
      output$avstrax_plot1_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$tech_categories_plot1_region,
                  input$widthscale_region, input$display_mode_region, !is.null(input$top_n_ids_region))

        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        pdata      <- fallback_by_tech_region()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        result <- plot_avstrax_by_country(
          pdata            = pdata,
          toflow           = input$toflow_region,
          custom_colors    = custom_colors,
          colorings        = colorings,
          widthscale       = input$widthscale_region,
          display_mode     = input$display_mode_region,
          top_n_ids        = input$top_n_ids_region,
          plot_title       = sub("^[^.]*\\.", "", flow_label),
          precomputed_data = pdata
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot1_region_gg   <- result$ggplot
          plot_store$avstrax_plot1_region_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })
      
      # Plot 2: Returns by Region
      output$avstrax_plot2_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                  input$topn_region, input$mininno_region,
                  input$widthscale_region, input$display_mode_region,
                  !is.null(input$top_n_ids_region))

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        precomputed_data <- fallback_by_region()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        result <- plot_avstrax_by_technology(
          pdata                   = data.frame(),
          classes                 = NULL,
          technologies            = input$techs_region,
          toflow                  = input$toflow_region,
          custom_colors           = custom_colors,
          topn                    = input$topn_region,
          mininno                 = input$mininno_region,
          widthscale              = input$widthscale_region,
          display_mode            = input$display_mode_region,
          top_n_ids               = input$top_n_ids_region,
          x_label                 = "Region",
          plot_title              = sub("^[^.]*\\.", "", flow_label),
          comparison_technologies = input$techs_comparison_region,
          precomputed_avstrax     = precomputed_data
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_region_gg   <- result$ggplot
          plot_store$avstrax_plot2_region_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })
      
      # UK Map: Returns
      output$uk_regions_map <- leaflet::renderLeaflet({
        shiny::req(input$region, input$toflow_region, input$techs_region, input$mininno_region)

        avstrax_data <- fallback_by_region()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        map_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_region)

        if (nrow(map_data) == 0) return(NULL)

        is_return  <- grepl("^(is|av)", input$toflow_region)
        map_title  <- paste0("Returns: ", paste(input$techs_region, collapse = ", "))

        # Store ggplot version and data for PDF/CSV downloads
        plot_store$uk_regions_map_gg <- plot_uk_regions_map_gg(
          data       = map_data,
          value_col  = "mean",
          plot_title = map_title,
          is_return  = is_return
        )
        plot_store$uk_regions_map_data <- map_data

        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col    = "mean",
          plot_title   = map_title,
          is_return    = is_return
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

        result <- plot_avstrax_rta(
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
          plot_title          = paste0("RTA - ", paste(input$techs_region, collapse = ", ")),
          precomputed_avstrax = precomputed_data
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_region_rta_gg   <- result$ggplot
          plot_store$avstrax_plot2_region_rta_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })
      
      # RTA Scatter: RTA vs Returns
      output$rta_returns_scatter_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                  input$mininno_rta_region, input$minallinnos_rta_region,
                  input$widthscale_region)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        precomputed_data <- fallback_by_region()
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) return(NULL)

        result <- plot_rta_returns_scatter(
          avstrax_data = precomputed_data,
          mininno      = input$mininno_rta_region,
          minallinnos  = input$minallinnos_rta_region,
          widthscale   = input$widthscale_region,
          plot_title   = paste0("RTA vs Returns - ", paste(input$techs_region, collapse = ", ")),
          x_label      = "Revealed Technological Advantage",
          y_label      = "Return (%)"
        )

        if (!is.null(result$ggplot)) {
          plot_store$rta_returns_scatter_region_gg   <- result$ggplot
          plot_store$rta_returns_scatter_region_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      })
      
      # UK Map: RTA
      output$uk_regions_map_rta <- leaflet::renderLeaflet({
        shiny::req(input$region, input$toflow_region, input$techs_region,
                   input$mininno_rta_region, input$minallinnos_rta_region)

        avstrax_data <- fallback_by_region()
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) return(NULL)

        map_data <- avstrax_data |>
          dplyr::filter(ctry_code != "All", innos >= input$mininno_rta_region)

        if ("Allinnos" %in% names(map_data) && input$minallinnos_rta_region > 0) {
          map_data <- map_data |>
            dplyr::filter(Allinnos >= input$minallinnos_rta_region)
        }

        if (nrow(map_data) == 0) return(NULL)

        rta_title <- paste0("RTA - ", paste(input$techs_region, collapse = ", "))

        # Store ggplot version and data for PDF/CSV downloads
        plot_store$uk_regions_map_rta_gg <- plot_uk_regions_map_gg(
          data       = map_data,
          value_col  = "RTA",
          plot_title = rta_title,
          is_return  = FALSE
        )
        plot_store$uk_regions_map_rta_data <- map_data

        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col    = "RTA",
          plot_title   = rta_title,
          is_return    = FALSE
        )
      })

      # ── Download handlers ──────────────────────────────────────────────────
      # SVG + CSV for girafe plots
      output$dl_svg_avstrax_plot1_region <- make_svg_handler(
        reactive(plot_store$avstrax_plot1_region_gg), "region_returns_by_technology")
      output$dl_csv_avstrax_plot1_region <- make_csv_handler(
        reactive(plot_store$avstrax_plot1_region_data), "region_returns_by_technology")

      output$dl_svg_avstrax_plot2_region <- make_svg_handler(
        reactive(plot_store$avstrax_plot2_region_gg), "region_returns_by_region")
      output$dl_csv_avstrax_plot2_region <- make_csv_handler(
        reactive(plot_store$avstrax_plot2_region_data), "region_returns_by_region")

      output$dl_svg_avstrax_plot2_region_rta <- make_svg_handler(
        reactive(plot_store$avstrax_plot2_region_rta_gg), "region_rta_by_region")
      output$dl_csv_avstrax_plot2_region_rta <- make_csv_handler(
        reactive(plot_store$avstrax_plot2_region_rta_data), "region_rta_by_region")

      output$dl_svg_rta_returns_scatter_region <- make_svg_handler(
        reactive(plot_store$rta_returns_scatter_region_gg), "region_rta_vs_returns")
      output$dl_csv_rta_returns_scatter_region <- make_csv_handler(
        reactive(plot_store$rta_returns_scatter_region_data), "region_rta_vs_returns")

      # PDF + CSV for UK region maps
      output$dl_pdf_uk_regions_map <- make_pdf_handler(
        reactive(plot_store$uk_regions_map_gg), "uk_regions_map")
      output$dl_csv_uk_regions_map <- make_csv_handler(
        reactive(plot_store$uk_regions_map_data), "uk_regions_map")

      output$dl_pdf_uk_regions_map_rta <- make_pdf_handler(
        reactive(plot_store$uk_regions_map_rta_gg), "uk_regions_map_rta")
      output$dl_csv_uk_regions_map_rta <- make_csv_handler(
        reactive(plot_store$uk_regions_map_rta_data), "uk_regions_map_rta")

    }
  )
}