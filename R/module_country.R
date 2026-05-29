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
  is_firm    <- sprintf("input['%s'] == 'Value flow by firm'", tab_id)
  is_country <- sprintf("input['%s'] == 'Value flows by Country'", tab_id)
  is_rta     <- sprintf("input['%s'] == 'Revealed Technological Advantage'", tab_id)
  not_rta    <- sprintf("input['%s'] != 'Revealed Technological Advantage'", tab_id)
  not_tech   <- sprintf("input['%s'] != 'Value flows by Technology'", tab_id)
  not_firm   <- sprintf("input['%s'] != 'Value flow by firm'", tab_id)

  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    # --- Always visible: Country, Firm --- (foldable, open by default)
    shiny::tags$details(
      open = NA,
      shiny::tags$summary(
        "GLOBAL FILTERS",
        style = paste0(
          "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
          "user-select: none;"
        )
      ),
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
      shiny::conditionalPanel(
        condition = not_firm,
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
        )
      ),
      shiny::conditionalPanel(
        condition = not_tech,
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId = ns("techs"),
            label = "Technologies included",
            choices = grouped_techs,
            selected = "All innovations",
            multiple = TRUE,
            options = list(placeholder = 'Choose technologies...')
          )
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::div(
          style = "display:flex; gap:18px; flex-wrap:wrap;",
          shiny::checkboxInput(
            inputId = ns("granted_only"),
            label   = "Granted families only",
            value   = FALSE
          ),
          shiny::checkboxInput(
            inputId = ns("multifam_only"),
            label   = "Multi-application families only",
            value   = FALSE
          ),
          shiny::checkboxInput(
            inputId = ns("exclude_um"),
            label   = "Exclude utility model patents",
            value   = FALSE
          )
        )
      ),

      # City filter — currently wired into the "Value flows by
      # Technology" pipeline only; the panel is hidden on other inner
      # tabs to avoid suggesting it has effect there. Mirrors the
      # HiGGlo controls so users get a consistent vocabulary.
      # `choices = NULL` here; the full list is pushed via
      # updateSelectizeInput(..., server = TRUE) in the server body
      # below to avoid Shiny's large-options performance warning.
      shiny::conditionalPanel(
        condition = is_tech,
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId  = ns("city"),
            label    = "City",
            choices  = NULL,
            multiple = TRUE,
            options  = list(placeholder = 'Choose cities...')
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::checkboxInput(
            inputId = ns("include_fallback"),
            label   = "Include capital city fallback",
            value   = FALSE
          )
        )
      )

    ),

    # --- Value Flow: Tech + Country only --- (foldable, open by default)
    shiny::conditionalPanel(
      condition = not_rta,
      shiny::tags$details(
        open = NA,
        shiny::tags$summary(
          "VALUE FLOW",
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId = ns("toflow"),
            label = NULL,
            choices = toflow_choices,
            selected = "ev_global",
            multiple = FALSE,
            width = "400px",
            options = list(placeholder = 'Choose a value flow...')
          )
        )
      )
    ),

    # --- Technology Categories: Tech only --- (foldable, open by default)
    shiny::conditionalPanel(
      condition = is_tech,
      shiny::tags$details(
        open = NA,
        shiny::tags$summary(
          "TECHNOLOGY CATEGORIES",
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId = ns("tech_categories_plot1"),
            label = NULL,
            choices = grouped_techs,
            selected = c("AI", "Green Technology",
                         "Any Agriculture & Food technology",
                         "Defence Technology"),
            multiple = TRUE,
            width = "200%",
            options = list(placeholder = 'Choose one or more technology categories...')
          )
        )
      )
    ),

    # --- Sector and firm categories: Firm tab only ---
    # Wrapped in a native <details> element so the whole section folds
    # away by default — clears clutter from the sidebar until the user
    # actively wants to change which categories are plotted. Two controls
    # inside:
    #   1. "Aggregate sector bars" — a flat multi-select. Each chosen
    #      sector becomes ONE aggregate bar (all its firms pooled).
    #   2. "Individual firm bars" — a foldable jstree (via shinyTree).
    #      Sectors are folder nodes; firms are leaves. Default jstree
    #      cascade means ticking a sector header auto-checks every firm
    #      beneath it. Only the leaves become bars; sector folder nodes
    #      are filtered out server-side so they don't double-count with
    #      the aggregate-sector multi-select above.
    shiny::conditionalPanel(
      condition = is_firm,
      shiny::tags$details(
        # `open` attribute absent = collapsed by default. The cursor +
        # subtle hover hint make the summary look interactive; the small
        # caret comes for free via the browser's default `<summary>`
        # marker.
        shiny::tags$summary(
          "SECTOR AND FIRM CATEGORIES",
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::selectizeInput(
            inputId  = ns("firm_categories_sectors"),
            label    = "Aggregate sector bars",
            choices  = names(firm_sector_groups),
            # Default: the 10 sectors with the largest distinct-docdb
            # count, precomputed at build time and saved into sysdata.rda
            # as `firm_sector_top10`. Falls back to the first 10 sectors
            # if the object isn't available (e.g. before the next
            # data-raw-2025/02-build-app-sysdata.R run).
            # Banks are excluded from the default selection (still
            # available in the dropdown for users to add manually).
            selected = setdiff(
              tryCatch(
                get("firm_sector_top10", envir = asNamespace(
                      "innovationStrategyExplorer"), inherits = FALSE),
                error = function(e) utils::head(names(firm_sector_groups),
                                                10L)
              ),
              "Banks"
            ),
            multiple = TRUE,
            width    = "100%",
            options  = list(placeholder = 'Choose sectors as single aggregate bars...')
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::tags$label(
            "Individual firm bars",
            style = "display:block; margin-bottom:4px; font-weight:500;"
          ),
          shiny::div(
            class = "firm-categories-tree",
            style = "max-height: 380px; overflow-y: auto;",
            shinyTree::shinyTree(
              outputId    = ns("firm_categories_tree"),
              checkbox    = TRUE,
              themeIcons  = FALSE,
              themeDots   = FALSE,
              search      = TRUE,
              contextmenu = FALSE
            )
          ),
          # Hidden mirror of the checked firm leaves, persisted as a
          # delimited string. The full tree state is too bulky to bookmark
          # (every firm name with state attrs), but this compact mirror
          # IS bookmarked (auto-included by the country-* keep rule in
          # server.R), and on session restore the tree initialisation
          # reads it back from the URL to re-check the right leaves.
          shiny::tags$div(
            style = "display: none;",
            shiny::textInput(
              ns("firm_categories_picked_persist"),
              label = NULL,
              value = ""
            )
          )
        )
      )
    ),

    # --- Chart options --- (foldable, open by default)
    shiny::tags$details(
      open = NA,
      shiny::tags$summary(
        "CHART OPTIONS",
        style = paste0(
          "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
          "user-select: none;"
        )
      ),
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
          value = 10,
          min = 1
        )
      )
    ),

    # --- RTA OPTIONS: RTA only --- (foldable, open by default)
    shiny::conditionalPanel(
      condition = is_rta,
      shiny::tags$details(
        open = NA,
        shiny::tags$summary(
          "RTA OPTIONS",
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
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
          shiny::numericInput(ns("mininno_rta"), "Innovation count threshold:", value = 0, min = 0, max = 500)
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
        "Value flow by firm",
        shiny::div(
          ggiraph::girafeOutput(ns("avstrax_plot_by_firm"),
                                width = "100%", height = "auto"),
          plot_download_buttons(ns, "avstrax_plot_by_firm")
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
      # Local null-coalesce. R 4.4+ ships `%||%` natively, but defining
      # it here keeps the module compatible with earlier installs and
      # mirrors the same idiom used in module_hglobe.R.
      `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

      # Reactive store for ggplot objects and data (for download handlers)
      plot_store <- shiny::reactiveValues()

      # Server-side selectize for the City filter (Value flows by
      # Technology tab). Same reasoning as the HiGGlo module: we
      # leave `choices = NULL` in the UI and push the full list via
      # updateSelectizeInput(server = TRUE) so type-ahead is handled
      # incrementally on the server rather than shipping the entire
      # city list to the browser at page-load.
      #
      # Bookmark restore: read the original URL query string directly
      # rather than relying on `isolate(input$city)`. Lazy module
      # initialisation + server-side selectize together drop the
      # restored value off the input registry — the URL is the only
      # stable source of truth at this point in the lifecycle. See
      # module_hglobe.R for the same fix.
      url_q     <- shiny::parseQueryString(
        session$clientData$url_search %||% "")
      city_key  <- session$ns("city")
      raw_city  <- url_q[[city_key]]
      restored_city <- if (!is.null(raw_city) && nzchar(raw_city)) {
        tryCatch(jsonlite::fromJSON(raw_city),
                 error = function(e) gsub('^"|"$', '', raw_city))
      } else NULL
      city_default <- if (length(restored_city) > 0L &&
                          any(nzchar(as.character(restored_city)))) {
        restored_city
      } else {
        "No city filter"
      }
      shiny::updateSelectizeInput(
        session  = session,
        inputId  = "city",
        choices  = city_grouped_choices,
        selected = city_default,
        server   = TRUE
      )

      # Expand "All categories" into all individual broad techs in the selectizeInput
      shiny::observeEvent(input$tech_categories_plot1, {
        if ("__CLEAR_TECHS__" %in% input$tech_categories_plot1) {
          shiny::updateSelectizeInput(session, "tech_categories_plot1",
                                      selected = character(0))
          return()
        }
        if ("All categories" %in% input$tech_categories_plot1) {
          new_sel <- unique(c(setdiff(input$tech_categories_plot1, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "tech_categories_plot1",
                                     selected = new_sel)
        }
      })

      shiny::observeEvent(input$techs, {
        # "Clear all categories" (value "__CLEAR_TECHS__"): wipe the field.
        if ("__CLEAR_TECHS__" %in% input$techs) {
          shiny::updateSelectizeInput(session, "techs",
                                      selected = character(0))
          return()
        }
        # "Include all categories" (value "All categories"): expand.
        if ("All categories" %in% input$techs) {
          new_sel <- unique(c(setdiff(input$techs, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "techs",
                                     selected = new_sel)
        }
      })

      # ── Foldable tree for "Sector and firm categories" (firm tab) ─────
      # The tree mirrors `firm_sector_groups` from R/sysdata.rda — one
      # branch per ICB sector, one leaf per firm. On a fresh load nothing
      # is checked; on bookmark restore the checked-leaf list comes from
      # the URL (via the hidden `firm_categories_picked_persist` input)
      # and we pre-check those leaves + open their parent sectors.
      firm_tree_data <- local({
        # `session$clientData$url_search` already carries the restored
        # URL at module-load time. The persist field is name-spaced by
        # the module (country-firm_categories_picked_persist) and Shiny
        # bookmark-quotes its scalar value with surrounding "..." which
        # `jsonlite::fromJSON` strips cleanly.
        url_q <- shiny::parseQueryString(
          session$clientData$url_search %||% "")
        raw   <- url_q[[session$ns("firm_categories_picked_persist")]]
        picks <- if (!is.null(raw) && nzchar(raw)) {
          val <- tryCatch(jsonlite::fromJSON(raw),
                          error = function(e) gsub('^"|"$', "", raw))
          if (is.null(val) || !length(val) || identical(val, "")) {
            character(0)
          } else {
            strsplit(as.character(val), "||", fixed = TRUE)[[1]]
          }
        } else character(0)

        tree <- lapply(firm_sector_groups, function(firms_list) {
          firm_names <- unlist(firms_list, use.names = FALSE)
          setNames(as.list(rep("", length(firm_names))), firm_names)
        })
        if (length(picks) > 0) {
          for (sec in names(tree)) {
            for (f in names(tree[[sec]])) {
              if (f %in% picks) {
                attr(tree[[sec]][[f]], "stselected") <- TRUE
                attr(tree[[sec]],      "stopened")  <- TRUE
              }
            }
          }
        }
        tree
      })

      output$firm_categories_tree <- shinyTree::renderTree(firm_tree_data)

      # Live mirror: whenever the user checks/unchecks a leaf, write the
      # current selection into the hidden persist input. The bookmark
      # observer in server.R picks up its value automatically because
      # the input is prefixed `country-`.
      shiny::observe({
        sel <- firm_categories_selected()
        txt <- paste(sel, collapse = "||")
        shiny::updateTextInput(session, "firm_categories_picked_persist",
                                value = txt)
      })

      # Pulls the checked leaves (firm names) out of the tree input. With
      # default jstree cascade, ticking a sector header auto-checks every
      # firm beneath it AND the sector itself. `get_selected(format =
      # "names")` returns a list whose elements are length-1 character
      # vectors holding the node *name* (the path is in attr "ancestry").
      # We coerce to plain strings, dedupe, and filter out sector names
      # so they don't double-count alongside the aggregate-sector
      # selectize.
      firm_categories_selected <- shiny::reactive({
        tree_state <- input$firm_categories_tree
        if (is.null(tree_state)) return(character(0))
        sel <- shinyTree::get_selected(tree_state, format = "names")
        if (length(sel) == 0) return(character(0))
        names_sel <- unique(unlist(lapply(sel, as.character),
                                    use.names = FALSE))
        setdiff(names_sel, names(firm_sector_groups))
      })

      # Union of both controls — aggregate sectors + individual firms —
      # passed to `build_firm_categories_filter_v2()` to produce one bar
      # per selection. Aggregate-sector entries are kept on top so the
      # bar order is predictable.
      firm_categories_combined <- shiny::reactive({
        unique(c(input$firm_categories_sectors,
                 firm_categories_selected()))
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

        sql <- sql_country_tech_combined_v2(
          toflow, country_sql, tech_filters, firm_clause,
          top_n_ids        = input$top_n_ids,
          granted_only     = isTRUE(input$granted_only),
          multifam_only    = isTRUE(input$multifam_only), exclude_um = isTRUE(input$exclude_um),
          selected_cities  = input$city,
          include_fallback = isTRUE(input$include_fallback)
        )

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
              technology == "Defence Technology"                    ~ "defence",
              technology %in% colorings$green                      ~ "green",
              technology %in% colorings$battery                    ~ "battery",
              technology %in% colorings$hard_to_abate              ~ "hard to abate",
              technology %in% colorings$ai                         ~ "AI",
              technology %in% colorings$agrifood                   ~ "agrifood",
              technology %in% colorings$defence                    ~ "defence",
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
                             sort(input$firm), input$top_n_ids,
                             isTRUE(input$granted_only), isTRUE(input$multifam_only), isTRUE(input$exclude_um),
                             sort(input$city), isTRUE(input$include_fallback))

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

        out <- DBI::dbGetQuery(con, sql_country_combined_v2(
          toflow, country_sql, techs, firm_clause,
          top_n_ids   = input$top_n_ids,
          granted_only = isTRUE(input$granted_only), multifam_only = isTRUE(input$multifam_only), exclude_um = isTRUE(input$exclude_um)
        ))

        if (nrow(out) == 0) return(NULL)

        # Compute the RTA denominator with the SAME SQL path as `innos` but
        # without the tech filter. This guarantees that for "All Innovations"
        # innos == allinnos and therefore RTA == 1 for every country — firm
        # mapping cannot inflate the denominator because the COUNT(DISTINCT
        # docdb_family_id) dedupes across matched firms.
        allinnos_data <- DBI::dbGetQuery(
          con,
          sql_ctry_allinnos_v2(toflow, country_sql, firm_clause,
                               granted_only = isTRUE(input$granted_only), multifam_only = isTRUE(input$multifam_only), exclude_um = isTRUE(input$exclude_um))
        ) |>
          dplyr::filter(ctry_code %in% selected_countries)

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
                             sort(input$firm), input$top_n_ids,
                             isTRUE(input$granted_only), isTRUE(input$multifam_only), isTRUE(input$exclude_um))

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

      # ── Value flow by firm ──────────────────────────────────────────────
      # Mirror of `fallback_by_tech()` / `avstrax_plot1` but with the bar
      # dimension swapped to firm/sector categories. The global firm filter
      # is hidden on this tab (mirrors how the tech tab hides the global
      # tech filter), so we pass an empty firm_clause and rely on
      # `input$firm_categories_plot1` for the per-bar partitioning.
      fallback_by_firm <- shiny::reactive({
        shiny::req(input$toflow, input$country)

        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_filters <- build_firm_categories_filter_v2(
          firm_categories_combined())
        if (length(firm_filters) == 0) return(NULL)

        sql <- sql_country_firm_combined_v2(
          toflow        = input$toflow,
          country_sql   = country_sql,
          firm_filters  = firm_filters,
          firm_clause   = "",              # global firm filter is hidden on this tab
          techs         = input$techs %||% "All",
          top_n_ids     = input$top_n_ids,
          granted_only  = isTRUE(input$granted_only),
          multifam_only = isTRUE(input$multifam_only),
          exclude_um    = isTRUE(input$exclude_um)
        )

        out <- DBI::dbGetQuery(con, sql)
        if (nrow(out) == 0) return(NULL)

        # Bars are coloured by ICB sector via `plot_avstrax_by_firm()`:
        # sector-group selections collapse to their own sector; individual
        # firms get their sector via the inverted `firm_sector_groups`
        # lookup. Unmatched firms (sysdata occasionally has NA sector)
        # fall through to "Other".
        out <- out |>
          dplyr::mutate(
            sector       = firm_to_sector(technology),
            top3_ids_url = build_espacenet_search(top3_ids)
          )

        # Scale percentage flows (is_*/av_*) from decimal to percent
        if (grepl("^(is_|av_)", input$toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"),
                                names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

        out
      }) |> shiny::bindCache(input$toflow, input$country,
                             sort(firm_categories_combined()),
                             sort(input$techs), input$top_n_ids,
                             isTRUE(input$granted_only),
                             isTRUE(input$multifam_only),
                             isTRUE(input$exclude_um))

      output$avstrax_plot_by_firm <- ggiraph::renderGirafe({
        req(input$country, input$toflow,
            length(firm_categories_combined()) > 0,
            input$widthscale, input$display_mode, !is.null(input$top_n_ids))

        flow_label <- names(unlist(toflow_choices))[
          unlist(toflow_choices) == input$toflow]
        pdata <- fallback_by_firm()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        result <- plot_avstrax_by_firm(
          pdata        = pdata,
          toflow       = input$toflow,
          widthscale   = input$widthscale,
          display_mode = input$display_mode,
          top_n_ids    = input$top_n_ids,
          plot_title   = sub("^[^.]*\\.", "", flow_label)
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot_by_firm_gg   <- result$ggplot
          plot_store$avstrax_plot_by_firm_data <- result$plot_data
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
          plot_title              = paste0(sub("^[^.]*\\.", "", flow_label), " - ", paste(input$techs, collapse = ", ")),
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
        map_title <- paste0(sub("^[^.]*\\.", "", flow_label), " - ", paste(input$techs, collapse = ", "))

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

      output$dl_svg_avstrax_plot_by_firm <- make_svg_handler(
        reactive(plot_store$avstrax_plot_by_firm_gg), "returns_by_firm")
      output$dl_csv_avstrax_plot_by_firm <- make_csv_handler(
        reactive(plot_store$avstrax_plot_by_firm_data), "returns_by_firm")

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