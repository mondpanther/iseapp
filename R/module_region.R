#' Region module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
region_module_sidebar <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",
    
    # GLOBAL FILTERS section
    shiny::div(
      shiny::h5("GLOBAL FILTERS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("region"),
          "Region or Group:",
          choices = NULL,  # Will be set in server
          selected = "All UK regions",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more regions...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("toflow_region"),
          "Return Flow:",
          choices = NULL,  # Will be set in server
          selected = "istrax_global",
          multiple = FALSE,
          options = list(placeholder = 'Choose a return flow...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("tech_categories_plot1_region"),
          "Technology Categories:",
          choices = NULL,  # Will be set in server
          selected = c("All", "AI", "Green Technology"),
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more technology categories...')
        )
      )
    ),
    
    # CHART OPTIONS section
    shiny::div(
      shiny::h5("CHART OPTIONS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("widthscale_region"),
          "Bar Width Scale:",
          choices = c("log", "proportional"),
          selected = "log"
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          ns("display_mode_region"),
          "Display Mode:",
          choices = c("Confidence bands" = "confidence", 
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
    
    # BY REGION / RTA section
    shiny::div(
      shiny::h5("BY REGION / RTA", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("techs_region"),
          "Technology Categories:",
          choices = NULL,  # Will be set in server
          selected = "Green Technology",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more technology categories...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          ns("techs_comparison_region"),
          "Comparison Categories:",
          choices = NULL,  # Will be set in server
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Choose categories to compare...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("topn_region"),
          "Show top n regions:",
          value = 12,
          min = 1,
          max = 50
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("mininno_region"),
          "Innovation count threshold:",
          value = 100,
          min = 1,
          max = 500
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("topn_rta_region"),
          "RTA: Show top n regions:",
          value = 12,
          min = 1,
          max = 50
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("bottomn_rta_region"),
          "RTA: Show bottom n regions:",
          value = 0,
          min = 0,
          max = 50
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("mininno_rta_region"),
          "RTA: Innovation count threshold:",
          value = 100,
          min = 1,
          max = 500
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          ns("minallinnos_rta_region"),
          "RTA: All innovation threshold:",
          value = 500,
          min = 1,
          max = 5000
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
    
    # Main content with inner tabs
    bslib::navset_card_tab(
      id = ns("inner_tabs"),
      
      bslib::nav_panel(
        "Returns by Technology",
        shiny::div(
          style = "padding: 20px;",
          ggiraph::girafeOutput(ns("avstrax_plot1_region"), width = "100%", height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "Returns by Region",
        shiny::div(
          style = "padding: 20px;",
          ggiraph::girafeOutput(ns("avstrax_plot2_region"), width = "100%", height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "UK Map",
        shiny::div(
          style = "padding: 20px;",
          shiny::h3("UK Regions Map: Returns"),
          leaflet::leafletOutput(ns("uk_regions_map"), width = "100%", height = "500px")
        )
      ),
      
      bslib::nav_panel(
        "RTA",
        shiny::div(
          style = "padding: 20px;",
          shiny::h3("RTA by Region"),
          ggiraph::girafeOutput(ns("avstrax_plot2_region_rta"), width = "100%", height = "auto"),
          shiny::tags$br(),
          shiny::h3("RTA vs Returns"),
          ggiraph::girafeOutput(ns("rta_returns_scatter_region"), width = "100%", height = "auto"),
          shiny::tags$br(),
          shiny::h3("UK Regions Map: RTA"),
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

      # Set up DuckDB connections to both aggregated parquet files
      aggregated_tech_path <- system.file("extdata", "aggregated_by_tech_region.parquet", package = "innovationStrategyExplorer")
      aggregated_region_path <- system.file("extdata", "aggregated_by_region.parquet", package = "innovationStrategyExplorer")

      con_region <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbExecute(con_region, sprintf("CREATE VIEW aggregated_by_tech_region AS SELECT * FROM read_parquet('%s')", aggregated_tech_path))
      DBI::dbExecute(con_region, sprintf("CREATE VIEW aggregated_by_region AS SELECT * FROM read_parquet('%s')", aggregated_region_path))

      # Clean up connection on session end
      shiny::onSessionEnded(function() {
        DBI::dbDisconnect(con_region, shutdown = TRUE)
      })
      
      # Update selectize choices
      shiny::updateSelectizeInput(session, "region", 
                                  choices = grouped_region_choices, 
                                  selected = "All UK regions", 
                                  server = TRUE)
      shiny::updateSelectizeInput(session, "toflow_region", 
                                  choices = toflow_choices, 
                                  selected = "istrax_global", 
                                  server = TRUE)
      shiny::updateSelectizeInput(session, "tech_categories_plot1_region", 
                                  choices = grouped_techs, 
                                  selected = c("All", "AI", "Green Technology"), 
                                  server = TRUE)
      shiny::updateSelectizeInput(session, "techs_region", 
                                  choices = grouped_techs, 
                                  selected = "Green Technology", 
                                  server = TRUE)
      shiny::updateSelectizeInput(session, "techs_comparison_region", 
                                  choices = grouped_techs, 
                                  selected = NULL, 
                                  server = TRUE)
      
      # Window dimensions for ggiraph
      window_dims <- shiny::reactive({
        list(
          width = session$clientData$output_avstrax_plot1_region_width,
          height = session$clientData$output_avstrax_plot1_region_height
        )
      })
      
      # ===== RENDER OUTPUTS WILL GO BELOW =====

      # ===== REACTIVE DATA LOADERS =====
      
      # Store loaded data (follows country module pattern)
      loaded_data <- shiny::reactiveValues(
        techmap = NULL,
        regionmap = NULL
      )
      
      # Helper to get current techmap (prefers loaded data, falls back to global)
      get_techmap <- shiny::reactive({
        if (!is.null(loaded_data$techmap) && nrow(loaded_data$techmap) > 0) {
          loaded_data$techmap
        } else {
          techmap
        }
      })
      
      # Helper to get current regionmap
      get_regionmap <- shiny::reactive({
        if (!is.null(loaded_data$regionmap) && nrow(loaded_data$regionmap) > 0) {
          loaded_data$regionmap
        } else {
          regionmap
        }
      })
      
            # Precomputed data for Plot 1 (returns by technology)
            # Queries aggregated_by_tech_region parquet via DuckDB (already aggregated across regions)
            precomputed_avstrax_region <- shiny::reactive({
              shiny::req(input$region, input$toflow_region, input$tech_categories_plot1_region)
              
              toflow_name <- input$toflow_region
              tech_cats <- input$tech_categories_plot1_region
              
              # Query tech parquet by technology display names
              query <- glue::glue_sql(
                "SELECT technology, mean, innos, sem, top25_bin_mean, top50_bin_mean, 
                        top3_ids, top3_ids_url, greenclass
                FROM aggregated_by_tech_region
                WHERE toflow = {toflow_name}
                  AND technology IN ({tech_cats*})
                  AND firm = 'No firm'",
                .con = con_region
              )
              
              result <- DBI::dbGetQuery(con_region, query)
              
              if (nrow(result) == 0) return(NULL)
              
              result
            }) |>
              shiny::bindCache(input$region, input$toflow_region, input$tech_categories_plot1_region)

      # ===== RENDER OUTPUTS =====
      
      # Plot 1: Returns by Technology
      output$avstrax_plot1_region <- ggiraph::renderGirafe({
        shiny::req(input$region, input$toflow_region, input$tech_categories_plot1_region, input$widthscale_region, input$display_mode_region, !is.null(input$show_top3_ids_region))
        
        regions_selected <- expand_region_selection(input$region)
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        
        # Get current techmap
        current_techmap <- get_techmap()
        
        # Filter techmap based on selected technology categories
        selected_categories <- input$tech_categories_plot1_region
        include_other <- "Other" %in% selected_categories
        explicit_categories <- setdiff(selected_categories, "Other")
        
        if(include_other && length(explicit_categories) > 0) {
          filtered_techmap <- current_techmap |>
            dplyr::mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
        } else if(include_other && length(explicit_categories) == 0) {
          filtered_techmap <- current_techmap |>
            dplyr::mutate(technology = "Other")
        } else {
          filtered_techmap <- current_techmap |>
            dplyr::filter(technology %in% explicit_categories)
        }
        
        # Calculate responsive dimensions
        plot_width <- max(window_dims()$width, 400)
        width_inches <- plot_width / 96
        aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
        height_inches <- width_inches * aspect_ratio
        
        plot_avstrax_by_country(
          pdata = NULL,  # Using precomputed data
          classes = filtered_techmap,
          country_code = regions_selected,
          toflow = input$toflow_region,
          custom_colors = custom_colors,
          colorings = colorings,
          widthscale = input$widthscale_region,
          display_mode = input$display_mode_region,
          show_top3_ids = input$show_top3_ids_region,
          width_svg = width_inches,
          height_svg = height_inches,
          plot_title = sub("^[^.]*\\.", "", flow_label),
          precomputed_data = precomputed_avstrax_region()
        )
      }) |>
        shiny::bindCache(
          input$toflow_region,
          input$region,
          input$tech_categories_plot1_region,
          input$widthscale_region,
          input$display_mode_region,
          input$show_top3_ids_region,
          window_dims()$width,
          window_dims()$height
        )
      
      # Plot 2: Returns by Region
      output$avstrax_plot2_region <- ggiraph::renderGirafe({
        shiny::req(input$toflow_region, input$techs_region, input$region)
        
        regions_selected <- expand_region_selection(input$region)
        toflow_name <- input$toflow_region
        
        tech_key <- gsub("[^a-zA-Z0-9]", "_", input$techs_region[1]) |>
          gsub("_+", "_", x = _) |>
          gsub("^_|_$", "", x = _)
        
        query <- glue::glue_sql(
          "SELECT region_code, region_name, mean, innos, sem, top25_bin_mean, top50_bin_mean,
                  top3_ids, top3_ids_url, RTA, Allinnos, SumAllinnos
           FROM aggregated_by_region
           WHERE toflow = {toflow_name}
             AND tech_selection = {tech_key}
             AND firm = 'No firm'
             AND region_code IN ({regions_selected*})",
          .con = con_region
        )
        
        precomputed_data <- DBI::dbGetQuery(con_region, query)
        if (nrow(precomputed_data) == 0) return(NULL)
        
        # Rename to match what the plotting function expects
        precomputed_data <- precomputed_data |>
          dplyr::rename(ctry_code = region_code, country_name = region_name)
        
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        
        plot_avstrax_by_technology(
          pdata = NULL,
          classes = get_techmap(),
          technologies = input$techs_region[1],
          toflow = input$toflow_region,
          custom_colors = custom_colors,
          topn = input$topn_region,
          mininno = input$mininno_region,
          widthscale = input$widthscale_region,
          display_mode = input$display_mode_region,
          show_top3_ids = input$show_top3_ids_region,
          x_label = "Region",
          plot_title = paste0(input$techs_region[1], " - ", sub("^[^.]*\\.", "", flow_label)),
          precomputed_avstrax = precomputed_data
        )
      }) |>
        shiny::bindCache(
          input$toflow_region, input$region, input$techs_region,
          input$techs_comparison_region, input$widthscale_region,
          input$display_mode_region, input$show_top3_ids_region,
          input$topn_region, input$mininno_region
        )
      
      # UK Map: Returns
      output$uk_regions_map <- leaflet::renderLeaflet({
        shiny::req(input$toflow_region, input$techs_region, input$region)
        
        regions_selected <- expand_region_selection(input$region)
        toflow_name <- input$toflow_region
        
        tech_key <- gsub("[^a-zA-Z0-9]", "_", input$techs_region[1]) |>
          gsub("_+", "_", x = _) |>
          gsub("^_|_$", "", x = _)
        
        query <- glue::glue_sql(
          "SELECT region_code, region_name, mean, innos, RTA
           FROM aggregated_by_region
           WHERE toflow = {toflow_name}
             AND tech_selection = {tech_key}
             AND firm = 'No firm'
             AND region_code IN ({regions_selected*})",
          .con = con_region
        )
        
        map_data <- DBI::dbGetQuery(con_region, query)
        if (nrow(map_data) == 0) return(NULL)
        
        # Rename to ctry_code — plot_uk_regions_map expects this column name
        map_data <- map_data |>
          dplyr::rename(ctry_code = region_code)
        
        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col = "mean",
          plot_title = paste0("Returns: ", input$techs_region[1])
        )
      }) |>
        shiny::bindCache(
          input$toflow_region,
          input$region,
          input$techs_region
        )
      
      # RTA Plot: Returns by Region (RTA version)
      output$avstrax_plot2_region_rta <- ggiraph::renderGirafe({
        shiny::req(input$toflow_region, input$techs_region, input$region)
        
        regions_selected <- expand_region_selection(input$region)
        toflow_name <- input$toflow_region
        
        tech_key <- gsub("[^a-zA-Z0-9]", "_", input$techs_region[1]) |>
          gsub("_+", "_", x = _) |>
          gsub("^_|_$", "", x = _)
        
        query <- glue::glue_sql(
          "SELECT region_code, region_name, mean, innos, sem, top25_bin_mean, top50_bin_mean,
                  top3_ids, top3_ids_url, RTA, Allinnos, SumAllinnos
           FROM aggregated_by_region
           WHERE toflow = {toflow_name}
             AND tech_selection = {tech_key}
             AND firm = 'No firm'
             AND region_code IN ({regions_selected*})",
          .con = con_region
        )
        
        precomputed_data <- DBI::dbGetQuery(con_region, query)
        if (nrow(precomputed_data) == 0) return(NULL)
        
        # Rename to match what the plotting function expects
        precomputed_data <- precomputed_data |>
          dplyr::rename(ctry_code = region_code, country_name = region_name)
        
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        
        plot_avstrax_rta(
          pdata = NULL,
          classes = get_techmap(),
          technologies = input$techs_region[1],
          toflow = input$toflow_region,
          custom_colors = custom_colors,
          topn = input$topn_rta_region,
          bottomn = input$bottomn_rta_region,
          mininno = input$mininno_rta_region,
          minallinnos = input$minallinnos_rta_region,
          widthscale = input$widthscale_region,
          x_label = "Region",
          plot_title = paste0("RTA: ", input$techs_region[1], " - ", sub("^[^.]*\\.", "", flow_label)),
          precomputed_avstrax = precomputed_data
        )
      }) |>
        shiny::bindCache(
          input$toflow_region, input$region, input$techs_region,
          input$widthscale_region, input$topn_rta_region,
          input$bottomn_rta_region, input$mininno_rta_region,
          input$minallinnos_rta_region
        )
      
      # RTA Scatter: RTA vs Returns
      output$rta_returns_scatter_region <- ggiraph::renderGirafe({
        shiny::req(input$toflow_region, input$techs_region, input$region)
        
        regions_selected <- expand_region_selection(input$region)
        toflow_name <- input$toflow_region
        
        tech_key <- gsub("[^a-zA-Z0-9]", "_", input$techs_region[1]) |>
          gsub("_+", "_", x = _) |>
          gsub("^_|_$", "", x = _)
        
        query <- glue::glue_sql(
          "SELECT region_code, region_name, mean, innos, RTA, Allinnos, SumAllinnos
           FROM aggregated_by_region
           WHERE toflow = {toflow_name}
             AND tech_selection = {tech_key}
             AND firm = 'No firm'
             AND region_code IN ({regions_selected*})",
          .con = con_region
        )
        
        precomputed_data <- DBI::dbGetQuery(con_region, query)
        if (nrow(precomputed_data) == 0) return(NULL)
        
        # Rename to match expected column names
        precomputed_data <- precomputed_data |>
          dplyr::rename(ctry_code = region_code, country_name = region_name)
        
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
        
        plot_rta_returns_scatter(
          avstrax_data = precomputed_data,
          mininno = input$mininno_rta_region,
          minallinnos = input$minallinnos_rta_region,
          widthscale = input$widthscale_region,
          plot_title = paste0("RTA vs Returns: ", input$techs_region[1], " - ", sub("^[^.]*\\.", "", flow_label)),
          x_label = "RTA",
          y_label = "Return (%)"
        )
      }) |>
        shiny::bindCache(
          input$toflow_region, input$region, input$techs_region,
          input$widthscale_region, input$mininno_rta_region,
          input$minallinnos_rta_region
        )
      
      # UK Map: RTA
      output$uk_regions_map_rta <- leaflet::renderLeaflet({
        shiny::req(input$toflow_region, input$techs_region, input$region)
        
        regions_selected <- expand_region_selection(input$region)
        toflow_name <- input$toflow_region
        
        tech_key <- gsub("[^a-zA-Z0-9]", "_", input$techs_region[1]) |>
          gsub("_+", "_", x = _) |>
          gsub("^_|_$", "", x = _)
        
        query <- glue::glue_sql(
          "SELECT region_code, region_name, mean, innos, RTA
           FROM aggregated_by_region
           WHERE toflow = {toflow_name}
             AND tech_selection = {tech_key}
             AND firm = 'No firm'
             AND region_code IN ({regions_selected*})",
          .con = con_region
        )
        
        map_data <- DBI::dbGetQuery(con_region, query)
        if (nrow(map_data) == 0) return(NULL)
        
        # Rename to ctry_code — plot_uk_regions_map expects this column name
        map_data <- map_data |>
          dplyr::rename(ctry_code = region_code)
        
        plot_uk_regions_map(
          avstrax_data = map_data,
          value_col = "RTA",
          plot_title = paste0("RTA: ", input$techs_region[1]),
          is_return = FALSE
        )
      }) |>
        shiny::bindCache(
          input$toflow_region,
          input$region,
          input$techs_region
        )
    }
  )
}