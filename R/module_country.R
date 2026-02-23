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
          choices = c("All Firms", "Hitachi", "No Firm"),
          selected = "All Firms",
          multiple = FALSE
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
          ggiraph::girafeOutput(ns("avstrax_plot1"), height = "100%")
          # highcharter::highchartOutput(ns("avstrax_plot1"), height = "600px")
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

      # Get prepdata path
      prepdata_path <- system.file("extdata", "prepdata", package = "innovationStrategyExplorer")
      if (prepdata_path == "" || !dir.exists(prepdata_path)) {
        prepdata_path <- "inst/extdata/prepdata"
      }

      # ============================================================================
      # DUCKDB CONNECTION TO FULL PARQUET DATABASE
      # ============================================================================
      full_db_path <- system.file("extdata", "full_patent_database.parquet", package = "innovationStrategyExplorer")
      if (full_db_path == "" || !file.exists(full_db_path)) {
        full_db_path <- "inst/extdata/full_patent_database.parquet"
      }

      con <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbExecute(con, paste0(
        "CREATE VIEW full_db AS SELECT * FROM read_parquet('",
        gsub("\\\\", "/", full_db_path),
        "')"
      ))

      session$onSessionEnded(function() {
        tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
      })

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

      # ============================================================================
      # DETERMINE WHETHER TO USE PRECOMPUTED DATA
      # ============================================================================
      use_precomputed <- shiny::reactive({
        shiny::req(input$firm, input$toflow, input$country)
        
        # If firm filter is not "All Firms", always use fallback
        if (input$firm != "All Firms") {
          return(FALSE)
        }
        
        # Check if precomputed data exists for this selection
        expanded_countries <- expand_country_selection(input$country)
        group_name <- match_country_group(expanded_countries, group_definitions)
        
        if (is.null(group_name)) {
          return(FALSE)
        }
        
        # Check if the by_tech file exists
        by_tech_file <- file.path(prepdata_path, paste0("by_tech_", input$toflow, "_", group_name, ".fst"))
        file.exists(by_tech_file)
      })

      # ============================================================================
      # FALLBACK: DuckDB query for Plot 1 (by-technology)
      # ============================================================================
      fallback_by_tech <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1, input$firm)

        selected_countries <- expand_country_selection(input$country)
        toflow <- input$toflow
        firm <- input$firm

        # Build country filter
        country_sql <- paste0("'", selected_countries, "'", collapse = ", ")

        # Build firm filter
        firm_clause <- ""
        if (firm == "Hitachi") {
          firm_clause <- "AND firm = 'Hitachi'"
        } else if (firm == "No Firm") {
          firm_clause <- "AND firm IS NULL"
        }
        # "All Firms" => no firm filter

        # Query: aggregate by technology
        sql <- glue::glue("
          SELECT
            technology,
            AVG({toflow}) AS mean,
            COUNT(*) AS innos,
            CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) ELSE 0 END AS sem,
            QUANTILE_CONT({toflow}, 0.25) AS q1,
            QUANTILE_CONT({toflow}, 0.50) AS q2,
            QUANTILE_CONT({toflow}, 0.75) AS q3
          FROM full_db
          WHERE ctry_code IN ({country_sql})
            AND technology IS NOT NULL
            AND {toflow} IS NOT NULL
            {firm_clause}
          GROUP BY technology
        ")

        result <- DBI::dbGetQuery(con, sql)

        if (nrow(result) == 0) return(NULL)

        # Compute top25/top50 bin means per technology
        bin_sql <- glue::glue("
          SELECT
            technology,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN val END) AS top25_bin_mean,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN val END) AS top50_bin_mean
          FROM (
            SELECT
              technology,
              {toflow} AS val,
              ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) AS rnk,
              COUNT(*) OVER (PARTITION BY technology) AS cnt
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND technology IS NOT NULL
              AND {toflow} IS NOT NULL
              {firm_clause}
          ) sub
          GROUP BY technology
        ")

        bin_result <- DBI::dbGetQuery(con, bin_sql)

        # Compute top 3 patent IDs per technology
        top3_sql <- glue::glue("
          SELECT technology, docdb_family_id, val
          FROM (
            SELECT
              technology,
              docdb_family_id,
              {toflow} AS val,
              ROW_NUMBER() OVER (PARTITION BY technology ORDER BY {toflow} DESC) AS rnk
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND technology IS NOT NULL
              AND {toflow} IS NOT NULL
              {firm_clause}
          ) sub
          WHERE rnk <= 3
        ")

        top3_raw <- DBI::dbGetQuery(con, top3_sql)

        top3_agg <- top3_raw |>
          dplyr::group_by(technology) |>
          dplyr::summarise(
            top3_ids = paste(docdb_family_id, collapse = ", "),
            top3_ids_url = build_espacenet_search(paste(docdb_family_id, collapse = ", ")),
            .groups = "drop"
          )

        # Join all pieces
        out <- result |>
          dplyr::left_join(bin_result, by = "technology") |>
          dplyr::left_join(top3_agg, by = "technology")

        # Expand umbrella categories for filtering
        tech_expansion <- list(
          "Green Technology" = green_classes,
          "Battery Technology" = battery_classes,
          "Hard to Abate Sector Decarbonization" = hard_to_abate_classes,
          "AI" = ai_classes,
          "Any Agriculture & Food technology" = agrifood_classes
        )

        # Build reverse mapping: sub-technology -> umbrella name
        tech_reverse_map <- new.env(hash = TRUE, parent = emptyenv())
        for (umbrella in names(tech_expansion)) {
          for (sub_tech in tech_expansion[[umbrella]]) {
            tech_reverse_map[[sub_tech]] <- umbrella
          }
        }

        # Remap technology names to umbrella categories where applicable
        out <- out |>
          dplyr::mutate(
            technology = sapply(technology, function(t) {
              if (!is.null(tech_reverse_map[[t]])) tech_reverse_map[[t]] else t
            })
          )

        # Add greenclass AFTER remapping (so umbrella names get consistent colors)
        out <- out |>
          dplyr::mutate(
            greenclass = dplyr::case_when(
              technology %in% c("Green Technology", colorings$green) ~ "green",
              technology %in% c("Battery Technology", colorings$battery) ~ "battery",
              technology %in% c("Hard to Abate Sector Decarbonization", colorings$hard_to_abate) ~ "hard to abate",
              technology %in% c("AI", colorings$ai) ~ "AI",
              technology %in% c("Any Agriculture & Food technology", colorings$agrifood) ~ "agrifood",
              technology %in% colorings$cpcsecs ~ "cpcsecs",
              TRUE ~ "other"
            )
          )

        # Re-aggregate after remapping to umbrella names
        out <- out |>
          dplyr::group_by(technology, greenclass) |>
          dplyr::summarise(
            mean = sum(mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            sem = sqrt(sum((sem * sqrt(innos))^2, na.rm = TRUE)) / sqrt(sum(innos, na.rm = TRUE)),
            q1 = sum(q1 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            q2 = sum(q2 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            q3 = sum(q3 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            top25_bin_mean = sum(top25_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            top50_bin_mean = sum(top50_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
            top3_ids = dplyr::first(top3_ids),
            top3_ids_url = dplyr::first(top3_ids_url),
            innos = sum(innos, na.rm = TRUE),
            .groups = "drop"
          )
        
        out <- out |>
          dplyr::filter(innos > 0)

        # Handle tech category filtering (same logic as precomputed path)
        selected_categories <- input$tech_categories_plot1
        include_other <- "Other" %in% selected_categories
        explicit_categories <- setdiff(selected_categories, "Other")

        if (include_other && length(explicit_categories) > 0) {
          out <- out |>
            dplyr::mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
          # Re-aggregate again after collapsing to "Other"
          out <- out |>
            dplyr::group_by(technology, greenclass) |>
            dplyr::summarise(
              mean = sum(mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              sem = sqrt(sum((sem * sqrt(innos))^2, na.rm = TRUE)) / sqrt(sum(innos, na.rm = TRUE)),
              q1 = sum(q1 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              q2 = sum(q2 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              q3 = sum(q3 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top25_bin_mean = sum(top25_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top50_bin_mean = sum(top50_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top3_ids = dplyr::first(top3_ids),
              top3_ids_url = dplyr::first(top3_ids_url),
              innos = sum(innos, na.rm = TRUE),
              .groups = "drop"
            )
        } else if (include_other && length(explicit_categories) == 0) {
          out <- out |>
            dplyr::mutate(technology = "Other")
          out <- out |>
            dplyr::group_by(technology, greenclass) |>
            dplyr::summarise(
              mean = sum(mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              sem = sqrt(sum((sem * sqrt(innos))^2, na.rm = TRUE)) / sqrt(sum(innos, na.rm = TRUE)),
              q1 = sum(q1 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              q2 = sum(q2 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              q3 = sum(q3 * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top25_bin_mean = sum(top25_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top50_bin_mean = sum(top50_bin_mean * innos, na.rm = TRUE) / sum(innos, na.rm = TRUE),
              top3_ids = dplyr::first(top3_ids),
              top3_ids_url = dplyr::first(top3_ids_url),
              innos = sum(innos, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          out <- out |>
            dplyr::filter(technology %in% explicit_categories)
        }

        out
      }) |> shiny::bindCache(input$toflow, input$country, input$tech_categories_plot1, input$firm)

      # ============================================================================
      # FALLBACK: DuckDB query for Plot 2 / World Map (by-country)
      # ============================================================================
      fallback_by_country <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$techs, input$firm)

        selected_countries <- expand_country_selection(input$country)
        toflow <- input$toflow
        firm <- input$firm
        techs <- input$techs

        # Build SQL clauses
        country_sql <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- ""
        if (firm == "Hitachi") {
          firm_clause <- "AND firm = 'Hitachi'"
        } else if (firm == "No Firm") {
          firm_clause <- "AND firm IS NULL"
        }

        # Expand umbrella technology categories into sub-technologies
        tech_expansion <- list(
          "Green Technology" = green_classes,
          "Battery Technology" = battery_classes,
          "Hard to Abate Sector Decarbonization" = hard_to_abate_classes,
          "AI" = ai_classes,
          "Any Agriculture & Food technology" = agrifood_classes
        )

        expanded_techs <- unique(unlist(lapply(techs, function(t) {
          if (t %in% names(tech_expansion)) {
            return(tech_expansion[[t]])
          } else {
            return(t)
          }
        })))

        tech_sql <- paste0("'", expanded_techs, "'", collapse = ", ")

        # Tech filter — "All Innovations" means no tech filter
        tech_clause <- ""
        if (!("All Innovations" %in% techs)) {
          tech_clause <- glue::glue("AND technology IN ({tech_sql})")
        }

        # ---- Per-country aggregation ----
        country_agg_sql <- glue::glue("
          SELECT
            ctry_code,
            AVG({toflow}) AS mean,
            COUNT(*) AS innos,
            CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) ELSE 0 END AS sem,
            QUANTILE_CONT({toflow}, 0.25) AS q1,
            QUANTILE_CONT({toflow}, 0.50) AS q2,
            QUANTILE_CONT({toflow}, 0.75) AS q3
          FROM full_db
          WHERE ctry_code IN ({country_sql})
            AND {toflow} IS NOT NULL
            {tech_clause}
            {firm_clause}
          GROUP BY ctry_code
        ")

        by_country <- DBI::dbGetQuery(con, country_agg_sql)
        if (nrow(by_country) == 0) return(NULL)

        # ---- "All" row (global average across selected countries) ----
        all_agg_sql <- glue::glue("
          SELECT
            'All' AS ctry_code,
            AVG({toflow}) AS mean,
            COUNT(*) AS innos,
            CASE WHEN COUNT(*) > 1 THEN STDDEV({toflow}) / SQRT(COUNT(*)) ELSE 0 END AS sem,
            QUANTILE_CONT({toflow}, 0.25) AS q1,
            QUANTILE_CONT({toflow}, 0.50) AS q2,
            QUANTILE_CONT({toflow}, 0.75) AS q3
          FROM full_db
          WHERE ctry_code IN ({country_sql})
            AND {toflow} IS NOT NULL
            {tech_clause}
            {firm_clause}
        ")

        all_row <- DBI::dbGetQuery(con, all_agg_sql)

        # ---- Top25/Top50 bin means per country ----
        bin_sql <- glue::glue("
          SELECT
            ctry_code,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN val END) AS top25_bin_mean,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN val END) AS top50_bin_mean
          FROM (
            SELECT
              ctry_code,
              {toflow} AS val,
              ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY {toflow} DESC) AS rnk,
              COUNT(*) OVER (PARTITION BY ctry_code) AS cnt
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND {toflow} IS NOT NULL
              {tech_clause}
              {firm_clause}
          ) sub
          GROUP BY ctry_code
        ")

        bin_result <- DBI::dbGetQuery(con, bin_sql)

        # ---- Top25/Top50 for "All" row ----
        bin_all_sql <- glue::glue("
          SELECT
            'All' AS ctry_code,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.25), 1) THEN val END) AS top25_bin_mean,
            AVG(CASE WHEN rnk <= GREATEST(CEIL(cnt * 0.50), 1) THEN val END) AS top50_bin_mean
          FROM (
            SELECT
              {toflow} AS val,
              ROW_NUMBER() OVER (ORDER BY {toflow} DESC) AS rnk,
              COUNT(*) OVER () AS cnt
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND {toflow} IS NOT NULL
              {tech_clause}
              {firm_clause}
          ) sub
        ")

        bin_all <- DBI::dbGetQuery(con, bin_all_sql)

        # ---- Top 3 patent IDs per country ----
        top3_sql <- glue::glue("
          SELECT ctry_code, docdb_family_id
          FROM (
            SELECT
              ctry_code,
              docdb_family_id,
              {toflow} AS val,
              ROW_NUMBER() OVER (PARTITION BY ctry_code ORDER BY {toflow} DESC) AS rnk
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND {toflow} IS NOT NULL
              {tech_clause}
              {firm_clause}
          ) sub
          WHERE rnk <= 3
        ")

        top3_raw <- DBI::dbGetQuery(con, top3_sql)

        top3_agg <- top3_raw |>
          dplyr::group_by(ctry_code) |>
          dplyr::summarise(
            top3_ids = paste(docdb_family_id, collapse = ", "),
            top3_ids_url = build_espacenet_search(paste(docdb_family_id, collapse = ", ")),
            .groups = "drop"
          )

        # ---- Top 3 for "All" row ----
        top3_all_sql <- glue::glue("
          SELECT docdb_family_id
          FROM (
            SELECT
              docdb_family_id,
              {toflow} AS val,
              ROW_NUMBER() OVER (ORDER BY {toflow} DESC) AS rnk
            FROM full_db
            WHERE ctry_code IN ({country_sql})
              AND {toflow} IS NOT NULL
              {tech_clause}
              {firm_clause}
          ) sub
          WHERE rnk <= 3
        ")

        top3_all_raw <- DBI::dbGetQuery(con, top3_all_sql)
        top3_all_ids <- paste(top3_all_raw$docdb_family_id, collapse = ", ")

        # ---- RTA: Allinnos and SumAllinnos ----
        # Allinnos = total distinct patents per country (all techs, no tech filter)
        allinnos_sql <- glue::glue("
          SELECT ctry_code, COUNT(DISTINCT docdb_family_id) AS Allinnos
          FROM full_db
          WHERE ctry_code IN ({country_sql})
            {firm_clause}
          GROUP BY ctry_code
        ")

        allinnos <- DBI::dbGetQuery(con, allinnos_sql)

        # SumAllinnos = total distinct patents globally (all techs)
        sum_allinnos_sql <- glue::glue("
          SELECT COUNT(DISTINCT docdb_family_id) AS SumAllinnos
          FROM full_db
          WHERE ctry_code IN ({country_sql})
            {firm_clause}
        ")

        sum_allinnos <- DBI::dbGetQuery(con, sum_allinnos_sql)$SumAllinnos

        # ---- Assemble per-country rows ----
        out <- by_country |>
          dplyr::left_join(bin_result, by = "ctry_code") |>
          dplyr::left_join(top3_agg, by = "ctry_code") |>
          dplyr::left_join(allinnos, by = "ctry_code") |>
          dplyr::mutate(
            top25 = 0.25,
            top50 = 0.5,
            SumAllinnos = sum_allinnos,
            share_c = innos / Allinnos,
            share = sum(innos) / SumAllinnos,
            RTA = 2 * share_c / (share_c + share)
          )

        # ---- Assemble "All" row ----
        all_assembled <- all_row |>
          dplyr::bind_cols(bin_all |> dplyr::select(-ctry_code)) |>
          dplyr::mutate(
            top3_ids = top3_all_ids,
            top3_ids_url = build_espacenet_search(top3_all_ids),
            top25 = 0.25,
            top50 = 0.5,
            Allinnos = sum_allinnos,
            SumAllinnos = sum_allinnos,
            share_c = 1,
            share = 1,
            RTA = 1
          )

        # Combine
        dplyr::bind_rows(out, all_assembled)
      }) |> shiny::bindCache(input$toflow, input$country, input$techs, input$firm)

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
      # output$avstrax_plot1 <- highcharter::renderHighchart({
        req(input$country, input$toflow, input$tech_categories_plot1, input$widthscale, input$display_mode, !is.null(input$show_top3_ids))
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

        # p <- plot_avstrax_by_country_hc(
        p <- plot_avstrax_by_country(
          pdata = NULL, # patchar_countrymap(),
          classes = filtered_techmap,
          country_code = selected_countries,
          toflow = input$toflow,
          custom_colors = custom_colors,
          colorings=colorings,
          widthscale=input$widthscale,
          display_mode=input$display_mode,
          show_top3_ids=input$show_top3_ids,
          # width_svg = width_inches,
          # height_svg = height_inches,
          plot_title =  sub("^[^.]*\\.", "", flow_label),
          precomputed_data = if (use_precomputed()) precomputed_avstrax() else fallback_by_tech()
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
            input$widthscale,
            input$display_mode,
            !is.null(input$show_top3_ids))
        # req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

        selected_countries <- expand_country_selection(input$country)
        # Get the label from the nested toflow_choices list
        flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

        # Try to load pre-computed data if available
        precomputed_data <- NULL

        if (use_precomputed()) {
          # Try to match tech selection to a pre-computed category
          if (is.null(input$techs_comparison) || length(input$techs_comparison) == 0) {
            tech_category <- match_tech_category(input$techs)
            if (!is.null(tech_category)) {
              precomputed_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
              if (!is.null(precomputed_data)) {
                selected_countries <- expand_country_selection(input$country)
                precomputed_data <- precomputed_data %>%
                  filter(ctry_code %in% selected_countries | ctry_code == "All")
              }
            }
          }
        }

        # If no precomputed data available, use DuckDB fallback
        if (is.null(precomputed_data)) {
          precomputed_data <- fallback_by_country()
        }

        # Guard: if still no data, return NULL early
        if (is.null(precomputed_data) || nrow(precomputed_data) == 0) {
          return(NULL)
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
          widthscale = input$widthscale,
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

        if (use_precomputed()) {
          tech_category <- match_tech_category(input$techs)
          if (!is.null(tech_category)) {
            avstrax_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
            if (!is.null(avstrax_data)) {
              avstrax_data <- avstrax_data %>%
                filter(ctry_code %in% selected_countries)
            }
          }
        }

        # If no precomputed data available, use DuckDB fallback
        if (is.null(avstrax_data)) {
          avstrax_data <- fallback_by_country()
          if (!is.null(avstrax_data)) {
            avstrax_data <- avstrax_data %>%
              filter(ctry_code != "All")
          }
        }

        # Guard: if still no data, return NULL early
        if (is.null(avstrax_data) || nrow(avstrax_data) == 0) {
          return(NULL)
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

    }
  )
}