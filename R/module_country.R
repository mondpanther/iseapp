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
          inputId = ns("bwidthscale"),
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
          ggiraph::girafeOutput(ns("avstrax_plot1"), height = "100%")
        )
      ),

      bslib::nav_panel(
        "Returns by Country",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot2"), height = "auto")
        )
      ),
      
      bslib::nav_panel(
        "World Map",
        shiny::div(
          shiny::h3("World Map: Returns"),
          plotly::plotlyOutput(ns("world_map"), height = "500px")
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
country_module_server <- function(id, parent_session) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Restore subtab from URL on load
      # shiny::observe({
      #   query <- parseQueryString(parent_session$clientData$url_search)
        
      #   # Only restore if we're on the correct main tab
      #   if (!is.null(query$tab) &&!is.null(query$subtab) && query$tab == "Country Explorer") {
      #     bslib::nav_select(id = "inner_tabs", selected = query$subtab, session = session)
      #   }
      # })

      # Get prepdata path
      prepdata_path <- system.file("extdata", "prepdata", package = "shinyTemplate")
      if (prepdata_path == "" || !dir.exists(prepdata_path)) {
        prepdata_path <- "inst/extdata/prepdata"
      }

      # Reactive: Load precomputed aggregations
      precomputed_avstrax <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1)
        
        expanded_countries <- expand_country_selection(input$country)
        group_name <- match_country_group(expanded_countries, group_definitions)
        
        if (is.null(group_name)) {
          return(NULL)
        }
        
        data <- load_precomputed_by_tech(input$toflow, group_name)
        
        if (is.null(data)) {
          return(NULL)
        }
        
        # Filter to selected technologies
        selected_categories <- input$tech_categories_plot1
        include_other <- "Other" %in% selected_categories
        explicit_categories <- setdiff(selected_categories, "Other")
        
        if (include_other && length(explicit_categories) > 0) {
          data <- data %>%
            mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
        } else if (!include_other) {
          data <- data %>%
            filter(technology %in% explicit_categories)
        }
        
        data
      }) %>% shiny::bindCache(input$toflow, input$country, input$tech_categories_plot1)

      # Reactive values for window dimensions
      window_dims <- reactiveValues(width = 800, height = 600, initialized = TRUE)

      # ============================================================================
      # DEFERRED DATA LOADING
      # ============================================================================
      # Track loading state for big datasets
      data_state <- reactiveValues(
        techmap_loaded = !has_precomputed_data,  # Already loaded if no precomputed
        countrymap_loaded = !has_precomputed_data,
        regionmap_loaded = !has_precomputed_data,
        loading_started = FALSE,
        loading_complete = !has_precomputed_data
      )

      # Store loaded data in reactive values (will be populated by deferred loading)
      loaded_data <- reactiveValues(
        techmap = if (!has_precomputed_data) techmap else NULL,
        countrymap = if (!has_precomputed_data) countrymap else NULL,
        regionmap = if (!has_precomputed_data) regionmap else NULL
      )

      # Start deferred loading after a short delay (allows UI to render first)
      observe({
        if (has_precomputed_data && !data_state$loading_started) {
          data_state$loading_started <- TRUE

          # Use invalidateLater to defer loading to next tick, allowing UI to render
          invalidateLater(100)
        }
      }) |> bindEvent(TRUE, once = TRUE)

      # Deferred loading observer - loads big datasets in background
      # observe({
      #   req(has_precomputed_data)
      #   req(data_state$loading_started)
      #   req(!data_state$loading_complete)

      #   # Show waiter
      #   waiter::waiter_show(
      #     html = shiny::tagList(
      #       waiter::spin_fading_circles(),
      #       shiny::h1("Loading datasets", style = "margin-top: 20px;"),
      #       shiny::p("Please be patient...", style = "")
      #     ),
      #     color = waiter::transparent(0.5)
      #   )

      #   # Load datasets
      #   tryCatch({
      #     datasets <- load_big_datasets()

      #     # Process techmap (add "All" category and normalize names)
      #     # processed_techmap <- process_techmap(datasets$techmap, datasets$countrymap)

      #     # Update reactive values
      #     loaded_data$techmap <- datasets$techmap
      #     loaded_data$countrymap <- datasets$countrymap
      #     loaded_data$regionmap <- datasets$regionmap

      #     # Update global variables for backward compatibility with plot functions
      #     techmap <<- processed_techmap
      #     countrymap <<- datasets$countrymap
      #     regionmap <<- datasets$regionmap
      #     regionmap_available <<- !is.null(datasets$regionmap) && nrow(datasets$regionmap) > 0

      #     # Mark as loaded
      #     data_state$loading_complete
      #     data_state$techmap_loaded <- TRUE
      #     data_state$countrymap_loaded <- TRUE
      #     data_state$regionmap_loaded <- TRUE
      #     data_state$loading_complete <- TRUE

      #     message("Deferred loading complete. Big datasets are now available.")
      #     waiter::waiter_hide()
      #   }, error = function(e) {
      #     message("Error during deferred loading: ", e$message)
      #     waiter::waiter_hide()
      #   })
      # }) |> bindEvent(data_state$loading_started, once = TRUE)

      # Helper to get current techmap (prefers loaded data, falls back to global)
      get_techmap <- reactive({
        if (!is.null(loaded_data$techmap) && nrow(loaded_data$techmap) > 0) {
          loaded_data$techmap
        } else {
          techmap
        }
      })

      # Helper to get current countrymap
      get_countrymap <- reactive({
        if (!is.null(loaded_data$countrymap) && nrow(loaded_data$countrymap) > 0) {
          loaded_data$countrymap
        } else {
          countrymap
        }
      })

      # Helper to get current regionmap
      get_regionmap <- reactive({
        if (!is.null(loaded_data$regionmap) && nrow(loaded_data$regionmap) > 0) {
          loaded_data$regionmap
        } else {
          regionmap
        }
      })

      # Reactive for regionmap availability
      is_regionmap_available <- reactive({
        rm <- get_regionmap()
        !is.null(rm) && nrow(rm) > 0
      })

      patchar_countrymap <- reactive({
        req(input$toflow)
        
        # Don't load full countrymap - just join the specific flow file
        path <- paste0("/istraxes/", input$toflow, ".fst")
        read_fst(localpath_fname(path))
      })

      # patchar_countrymap <- reactive({
      #   req(input$toflow)
      #   # Require countrymap to be loaded for on-the-fly computation
      #   req(data_state$countrymap_loaded)

      #   # Get the current countrymap (from reactive helper)
      #   current_countrymap <- get_countrymap()
      #   req(nrow(current_countrymap) > 0)

      #   path <- paste0("/istraxes/", input$toflow,".fst")

      #   pp=localpath_fname(path)
      #   if(file.exists(pp)){
      #     ddd <- read_fst(pp)
      #   }

      #   # Replace missing values with 0 in the value column
      #   # Some files (avstrax, ev) contain NAs that need to be treated as 0
      #   value_col <- input$toflow
      #   if (value_col %in% names(ddd)) {
      #     ddd[[value_col]][is.na(ddd[[value_col]])] <- 0
      #   }

      #   patchar_countrymap <- current_countrymap %>% left_join(ddd, by = c("docdb_family_id", "ctry_code"))
      # })

      observe({
        width <- session$clientData[[paste0("output_", ns("avstrax_plot1"), "_width")]]
        if (!is.null(width) && width > 0) {
          window_dims$width <- width
        }
      })

      observe({
        width <- session$clientData[[paste0("output_", ns("avstrax_plot2"), "_width")]]
        if (!is.null(width) && width > 0) {
          window_dims$width <- width
        }
      })
      
      # Chart 1: Main avstrax plot
      output$avstrax_plot1 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$tech_categories_plot1, input$bwidthscale, input$display_mode, !is.null(input$show_top3_ids))
        # req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

        selected_countries <- expand_country_selection(input$country)
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

        # Get current techmap
        current_techmap <- get_techmap()

        # Filter techmap based on selected technology categories
        # Handle "Other" category to include all non-selected technologies
        selected_categories <- input$tech_categories_plot1
        include_other <- "Other" %in% selected_categories
        explicit_categories <- setdiff(selected_categories, "Other")

        if(include_other && length(explicit_categories) > 0) {
          # Include explicitly selected categories AND other categories relabeled as "Other"
          filtered_techmap <- current_techmap %>%
            mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
        } else if(include_other && length(explicit_categories) == 0) {
          # Only "Other" selected - show all categories as "Other"
          filtered_techmap <- current_techmap %>%
            mutate(technology = "Other")
        } else {
          # No "Other" - just filter to explicitly selected categories
          filtered_techmap <- current_techmap %>%
            filter(technology %in% explicit_categories)
        }
        
        # Calculate responsive dimensions - wider browser = wider plot
        plot_width <- max(window_dims$width, 400)
        # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
        width_inches <- plot_width / 96
        # Wider windows get wider aspect ratio (less height per width)
        aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
        height_inches <- width_inches * aspect_ratio

        p <- plot_avstrax_by_country(
          pdata = patchar_countrymap(),
          classes = filtered_techmap,
          country_code = selected_countries,
          toflow = input$toflow,
          custom_colors = custom_colors,
          colorings=colorings,
          bwidthscale=input$bwidthscale,
          display_mode=input$display_mode,
          show_top3_ids=input$show_top3_ids,
          width_svg = width_inches,
          height_svg = height_inches,
          plot_title =  sub("^[^.]*\\.", "", flow_label),
          precomputed_data = precomputed_avstrax()
        )

        p
      })
      
      # Chart 2: Returns by Country for Selected Technologies
      output$avstrax_plot2 <- ggiraph::renderGirafe({
        req(input$country,
            input$toflow,
            input$techs,
            input$topn,
            input$mininno,
            input$bwidthscale,
            input$display_mode,
            !is.null(input$show_top3_ids))
        # req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

        selected_countries <- expand_country_selection(input$country)
        # Get the label from the nested toflow_choices list
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

        # Try to load pre-computed data if available
        precomputed_data <- NULL

        # Check if we can use pre-computed data:
        # 1. No comparison technologies (pre-computation doesn't cover comparisons)
        # 2. Technology selection matches a known category
        # Note: by_country files aggregate BY country for a specific technology category
        if (is.null(input$techs_comparison) || length(input$techs_comparison) == 0) {
          # Try to match tech selection to a pre-computed category
          tech_category <- match_tech_category(input$techs)

          if (!is.null(tech_category)) {
            # Try to load pre-computed data - by_country files are keyed by tech_category only
            precomputed_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
            if (!is.null(precomputed_data)) {
              message("Using pre-computed data for tech category: ", tech_category)
              # Filter precomputed data to selected countries if needed
              # BUT keep the "All" row which is needed for computing the average line
              selected_countries <- expand_country_selection(input$country)
              if (!is.null(precomputed_data$ctry_code)) {
                precomputed_data <- precomputed_data %>%
                  filter(ctry_code %in% selected_countries | ctry_code == "All")
              }
            }
          }
        }

        # When using pre-computed data, we still need a minimal filtered for any fallback
        filtered <- NULL

        # Calculate responsive dimensions - wider browser = wider plot
        plot_width <- max(window_dims$width, 400)
        # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
        width_inches <- plot_width / 96
        # Calculate height based on number of countries to display (topn)
        # Base height per country bar, with minimum height
        n_countries <- input$topn
        height_per_bar <- 0.35  # inches per bar
        min_height <- 4  # minimum height in inches
        height_inches <- max(min_height, n_countries * height_per_bar)

        # Get current techmap (may be placeholder if still loading)
        current_techmap <- get_techmap()

        p <- plot_avstrax_by_technology(
          pdata = if(is.null(precomputed_data)) filtered else data.frame(),
          classes = current_techmap,
          technologies = input$techs,
          toflow = input$toflow,
          custom_colors = custom_colors,
          topn = input$topn,
          mininno = input$mininno,
          bwidthscale = input$bwidthscale,
          display_mode = input$display_mode,
          show_top3_ids = input$show_top3_ids,
          width_svg = width_inches,
          height_svg = height_inches,
          plot_title = sub("^[^.]*\\.", "", flow_label),
          comparison_technologies = input$techs_comparison,
          precomputed_avstrax = precomputed_data
        )

        p
      })
      
      # World Map
      output$world_map <- plotly::renderPlotly({
        req(input$country,
            input$toflow,
            input$techs,
            input$mininno)

        selected_countries <- expand_country_selection(input$country)
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

        # Try to load pre-computed data if available
        avstrax_data <- NULL

        # Try to match tech selection to pre-computed data
        # World map shows data BY country for a specific technology, so uses by_country files
        tech_category <- match_tech_category(input$techs)

        if (!is.null(tech_category)) {
          avstrax_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
          if (!is.null(avstrax_data)) {
            message("World map using pre-computed data for tech: ", tech_category)
            # Filter to selected countries
            if (!is.null(avstrax_data$ctry_code)) {
              avstrax_data <- avstrax_data %>%
                filter(ctry_code %in% selected_countries)
            }
          }
        }

        # If no pre-computed data, compute on the fly
        if (is.null(avstrax_data)) {
          # Require big datasets for on-the-fly computation
          req(data_state$countrymap_loaded)
          req(data_state$techmap_loaded)

          # Get current techmap
          current_techmap <- get_techmap()

          # Filter by technology class
          filtered_classes <- current_techmap %>%
            filter(technology %in% input$techs) %>%
            distinct()

          if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

          # Filter data by selected countries
          filtered <- patchar_countrymap() %>%
            filter(ctry_code %in% selected_countries)

          # Compute aggregated data for all countries
          avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)
        }

        # Filter by minimum innovations
        avstrax_data <- avstrax_data %>%
          filter(innos >= input$mininno)

        # Determine if this is a return (%) or spillover ($) variable
        is_return <- grepl("strax", input$toflow)

        plot_world_map(
          avstrax_data = avstrax_data,
          value_col = "mean",
          color_scale = "Viridis",
          plot_title = paste0("World Map: ", sub("^[^.]*\\.", "", flow_label)),
          is_return = is_return
        )
      })

      # Restore inputs from URL parameters
      # shiny::observe({
      #   restore_module_inputs(
      #     params = parent_session$userData$restore_params,
      #     module_prefix = "country-",
      #     tab_name = "Country Explorer",
      #     input_configs = list(
      #       inner_tabs = "nav",
      #       country = "selectize",
      #       toflow = "selectize",
      #       tech_categories_plot1 = "selectize",
      #       techs = "selectize",
      #       bwidthscale = "radio",
      #       display_mode = "radio",
      #       show_top3_ids = "checkbox",
      #       topn = "numeric",
      #       mininno = "numeric"
      #     ),
      #     session = session
      #   )
      # })
    }
  )
}