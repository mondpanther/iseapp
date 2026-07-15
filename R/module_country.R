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

    # --- Apply settings ---
    # Charts are deliberately NOT live-reactive to sidebar edits: each
    # change used to fire the expensive DuckDB queries while the user
    # was still mid-edit. Every figure render in the server is gated on
    # this button (see `apply_trigger` there), so users batch their
    # changes and apply them in one go. HiGGlo has its own "Render Map"
    # button and needs no equivalent.
    shiny::div(
      shiny::actionButton(
        inputId = ns("apply_settings"),
        label   = "Apply settings",
        icon    = shiny::icon("rotate"),
        class   = "btn-primary",
        width   = "100%"
      ),
      shiny::div(
        "Charts update only when you press Apply.",
        style = paste0(
          "font-size: 0.75rem; color: #777; margin-top: 4px; ",
          "text-align: center;"
        )
      )
    ),

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
      # Foldable, collapsed by default (no `open` attribute).
      shiny::tags$details(
        shiny::tags$summary(
          "Country or Group",
          style = paste0(
            "font-weight: 500; font-size: 0.8rem; color: #555; ",
            "margin-bottom: 8px; cursor: pointer; user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          # Hidden value-holder read by every query / cache / deep-link; the
          # visible tree writes the chosen countries/groups into it.
          shiny::div(
            style = "display:none;",
            shiny::selectizeInput(
              inputId = ns("country"),
              label = NULL,
              choices = grouped_choices,
              selected = "All countries",
              multiple = TRUE
            )
          ),
          shiny::div(
            class = "firm-categories-tree",
            style = "max-height: 360px; overflow-y: auto;",
            shinyTree::shinyTree(
              outputId    = ns("country_tree"),
              checkbox    = TRUE,
              themeIcons  = FALSE,
              themeDots   = FALSE,
              search      = TRUE,
              contextmenu = FALSE
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = not_firm,
        # Foldable, collapsed by default (no `open` attribute).
        shiny::tags$details(
          shiny::tags$summary(
            "Firm or Sector Group",
            title = paste0(
              "Tick firms to restrict the chart to them; ticking a sector ",
              "folder selects every firm in it. Leave empty for no firm ",
              "filter."
            ),
            style = paste0(
              "font-weight: 500; font-size: 0.8rem; color: #555; ",
              "margin-bottom: 8px; cursor: pointer; user-select: none;"
            )
          ),
          shiny::div(
            class = "side_input",
            shiny::div(
              class = "firm-categories-tree",
              style = "max-height: 320px; overflow-y: auto;",
              shinyTree::shinyTree(
                outputId    = ns("firm_tree"),
                checkbox    = TRUE,
                themeIcons  = FALSE,
                themeDots   = FALSE,
                search      = TRUE,
                contextmenu = FALSE
              )
            ),
            # Compact bookmarkable mirror of the checked firm leaves (the raw
            # tree input is force-excluded from bookmarks in server.R).
            shiny::tags$div(
              style = "display: none;",
              shiny::textInput(ns("firm_tree_persist"), label = NULL, value = "")
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = not_tech,
        # Foldable, collapsed by default (no `open` attribute).
        shiny::tags$details(
          shiny::tags$summary(
            "Technologies Included",
            style = paste0(
              "font-weight: 500; font-size: 0.8rem; color: #555; ",
              "margin-bottom: 8px; cursor: pointer; user-select: none;"
            )
          ),
          shiny::div(
            class = "side_input",
            # Hidden value-holder read by every query / cache; the visible
            # tree writes the chosen filter technologies into it.
            shiny::div(
              style = "display:none;",
              shiny::selectizeInput(
                inputId = ns("techs"),
                label = NULL,
                choices = grouped_techs,
                selected = "All innovations",
                multiple = TRUE
              )
            ),
            shiny::div(
              class = "firm-categories-tree",
              style = "max-height: 360px; overflow-y: auto;",
              shinyTree::shinyTree(
                outputId    = ns("tech_filter_tree"),
                checkbox    = TRUE,
                themeIcons  = FALSE,
                themeDots   = FALSE,
                search      = TRUE,
                contextmenu = FALSE
              )
            )
          )
        )
      ),
      # Innovation types — foldable, collapsed by default.
      shiny::tags$details(
        shiny::tags$summary(
          "Innovation Types",
          style = paste0(
            "font-weight: 500; font-size: 0.8rem; color: #555; ",
            "margin-bottom: 8px; cursor: pointer; user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input innovation-types",
          shiny::checkboxInput(
            ns("granted_only"), "Granted families only", FALSE),
          shiny::checkboxInput(
            ns("multifam_only"), "Multi-application families only", FALSE),
          shiny::checkboxInput(
            ns("exclude_um"), "Exclude utility model patents", FALSE)
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
          # Hidden value-holder: stays the single source of truth read by
          # every query / cache / deep-link. The visible tree below writes
          # the chosen flow into it.
          shiny::div(
            style = "display:none;",
            shiny::selectizeInput(
              inputId = ns("toflow"),
              label = NULL,
              choices = toflow_choices,
              selected = "ev_global",
              multiple = FALSE
            )
          ),
          shiny::div(
            class = "firm-categories-tree",
            style = "max-height: 360px; overflow-y: auto;",
            shinyTree::shinyTree(
              outputId    = ns("toflow_tree"),
              checkbox    = FALSE,
              multiple    = FALSE,
              themeIcons  = FALSE,
              themeDots   = FALSE,
              search      = TRUE,
              contextmenu = FALSE
            )
          )
        )
      )
    ),

    # --- Technology Categories: Tech only --- (foldable, open by default)
    shiny::conditionalPanel(
      condition = is_tech,
      # Foldable, collapsed by default (no `open` attribute).
      shiny::tags$details(
        shiny::tags$summary(
          "TECHNOLOGY CATEGORIES",
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          # Hidden value-holder read by the by-technology query / cache /
          # deep-links; the visible tree writes the chosen categories in.
          shiny::div(
            style = "display:none;",
            shiny::selectizeInput(
              inputId = ns("tech_categories_plot1"),
              label = NULL,
              choices = grouped_techs,
              selected = c("AI", "Green Technology",
                           "Any Agriculture & Food technology",
                           "Defence Technology"),
              multiple = TRUE
            )
          ),
          shiny::div(
            class = "firm-categories-tree",
            style = "max-height: 360px; overflow-y: auto;",
            shinyTree::shinyTree(
              outputId    = ns("tech_categories_tree"),
              checkbox    = TRUE,
              themeIcons  = FALSE,
              themeDots   = FALSE,
              search      = TRUE,
              contextmenu = FALSE
            )
          )
        )
      )
    ),

    # --- Country categories: Country + RTA tabs --- (foldable, collapsed)
    # Defines the bars on the by-country / RTA charts. Ticking a group makes
    # ONE aggregate bar over its member countries (families deduped across
    # members in SQL); ticking individual countries makes one bar each.
    shiny::conditionalPanel(
      condition = sprintf("(%s) || (%s)", is_country, is_rta),
      shiny::tags$details(
        shiny::tags$summary(
          "COUNTRY CATEGORIES",
          title = paste0(
            "Under Country Groupings, tick a group for one aggregate bar ",
            "over its members. Under Individual Countries, tick countries ",
            "for their own bars. Group bars dedupe innovations across ",
            "member countries."
          ),
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
          shiny::div(
            class = "firm-categories-tree",
            style = "max-height: 360px; overflow-y: auto;",
            shinyTree::shinyTree(
              outputId    = ns("country_categories_tree"),
              checkbox    = TRUE,
              themeIcons  = FALSE,
              themeDots   = FALSE,
              search      = TRUE,
              contextmenu = FALSE
            )
          ),
          # Compact bookmarkable mirror of the picked categories.
          shiny::tags$div(
            style = "display: none;",
            shiny::textInput(ns("country_categories_persist"),
                             label = NULL, value = "")
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
          title = paste0(
            "Under Broad sectors, tick a sector for one aggregate bar ",
            "covering the whole sector. Under Individual firms, tick firms ",
            "for their own bars (ticking a sector there selects every firm ",
            "in it). A sector's aggregate and its individual firms can be ",
            "shown together."
          ),
          style = paste0(
            "font-weight: 600; margin-bottom: 10px; cursor: pointer; ",
            "user-select: none;"
          )
        ),
        shiny::div(
          class = "side_input",
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

      # Value-flow-by-firm display controls (firm tab only)
      shiny::conditionalPanel(
        condition = is_firm,
        shiny::div(
          class = "side_input",
          shiny::checkboxInput(
            ns("sort_firm_bars"),
            "Sort bars by value (tallest first)",
            value = TRUE
          ),
          shiny::numericInput(
            ns("limit_firms"),
            "Limit to top N firms/sectors",
            value = 20, min = 1, max = 500
          ),
          shiny::numericInput(
            ns("min_innos"),
            "Min number of innovations",
            value = 10, min = 0
          )
        )
      ),

      # Value-flow-by-technology display controls (tech tab only)
      shiny::conditionalPanel(
        condition = is_tech,
        shiny::div(
          class = "side_input",
          shiny::checkboxInput(
            ns("sort_tech_bars"),
            "Sort bars by value (tallest first)",
            value = TRUE
          ),
          shiny::numericInput(
            ns("limit_techs"),
            "Limit to top N tech categories",
            value = 20, min = 1, max = 500
          ),
          shiny::numericInput(
            ns("min_innos_tech"),
            "Min number of innovations",
            value = 10, min = 0
          )
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

      # ── Deferred settings: the "Apply settings" button ────────────────
      # All figure renders below (and the query reactives feeding them)
      # are gated with `shiny::bindEvent(apply_trigger())`, so editing a
      # sidebar control no longer recomputes anything — the expensive
      # DuckDB queries run only when the user clicks "Apply settings".
      # The shinyTree widgets initialise asynchronously (only once their
      # sidebar panel becomes visible), so a one-shot observer per tree
      # bumps the trigger when the tree's input first arrives; that gives
      # every tab its initial render from the default selections without
      # requiring a click.
      apply_trigger <- shiny::reactiveVal(0L)
      bump_apply    <- function() {
        apply_trigger(shiny::isolate(apply_trigger()) + 1L)
      }
      shiny::observeEvent(input$apply_settings, bump_apply())
      auto_apply_when_ready <- function(get_value) {
        obs <- shiny::observe({
          if (is.null(get_value())) return()
          bump_apply()
          obs$destroy()
        })
      }
      auto_apply_when_ready(function() input$country_categories_tree)
      auto_apply_when_ready(function() input$firm_categories_tree)

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
      # Two top-level branches, both built from `firm_sector_groups`:
      #   • "Broad sectors"    — one leaf per ICB sector; ticking a leaf
      #                          adds a single aggregate bar for that whole
      #                          sector.
      #   • "Individual firms" — the same sectors as sub-folders, each
      #                          holding its firm leaves; ticking a firm
      #                          adds that firm's own bar (ticking a
      #                          sub-folder selects every firm in it).
      # A sector's aggregate and its individual firms are independent, so
      # they can be shown together. The same sector name appears in both
      # branches; `get_selected()` attaches an `ancestry` attribute (the
      # chain of parent names) to each checked node, and the selection
      # reader uses the top-level ancestor to tell the aggregate leaf apart
      # from the firm sub-folder. Selections are persisted to the URL via
      # the hidden `firm_categories_picked_persist` input: stored tokens
      # are sector names (aggregates) and firm names (individuals), and on
      # restore we pre-check the matching leaves.
      broad_branch <- "Broad sectors"
      firms_branch <- "Individual firms"

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

        # No bookmark state → default to the 10 sectors with the largest
        # distinct-docdb count as aggregate bars (Banks excluded),
        # matching the previous selectize default. Precomputed at build
        # time as `firm_sector_top10`; falls back to the first 10 sectors.
        if (length(picks) == 0) {
          picks <- setdiff(
            tryCatch(
              get("firm_sector_top10", envir = asNamespace(
                    "innovationStrategyExplorer"), inherits = FALSE),
              error = function(e) utils::head(names(firm_sector_groups),
                                              10L)
            ),
            "Banks"
          )
        }

        agg_sel  <- intersect(picks, names(firm_sector_groups))  # aggregates
        firm_sel <- setdiff(picks, names(firm_sector_groups))    # individuals

        # Branch 1 — aggregate-sector leaves (open by default; it's short).
        broad <- lapply(stats::setNames(nm = names(firm_sector_groups)),
                        function(sec) {
          node <- ""
          if (sec %in% agg_sel) attr(node, "stselected") <- TRUE
          node
        })
        attr(broad, "stopened") <- TRUE

        # Branch 2 — per-sector folders of individual firms (open a folder
        # only when one of its firms is pre-selected; the branch itself is
        # opened only if any firm is pre-selected, since it is large).
        indiv <- lapply(stats::setNames(nm = names(firm_sector_groups)),
                        function(sec) {
          firm_names <- unlist(firm_sector_groups[[sec]], use.names = FALSE)
          children <- stats::setNames(as.list(rep("", length(firm_names))),
                                      firm_names)
          for (f in intersect(firm_names, firm_sel))
            attr(children[[f]], "stselected") <- TRUE
          if (any(firm_names %in% firm_sel))
            attr(children, "stopened") <- TRUE
          children
        })
        if (length(firm_sel) > 0) attr(indiv, "stopened") <- TRUE

        stats::setNames(list(broad, indiv), c(broad_branch, firms_branch))
      })

      output$firm_categories_tree <- shinyTree::renderTree(firm_tree_data)

      # Bars actually drawn in the "Value flow by firm" chart: the selection
      # narrowed by the "Limit to top N" / "Min number of innovations"
      # controls. Mirrors the filtering inside `plot_avstrax_by_firm()`
      # (filter innos >= min_innos, rank by `mean`, keep the top N) so this
      # set is exactly what the user sees — keep the two in sync if that
      # ranking ever changes. Empty when nothing matches yet.
      visible_firm_bars <- shiny::reactive({
        pdata <- fallback_by_firm()
        if (is.null(pdata) || nrow(pdata) == 0) return(character(0))

        mi <- input$min_innos %||% 10
        if (is.null(mi) || is.na(mi) || mi < 0) mi <- 10
        d <- pdata |>
          dplyr::filter(innos >= mi) |>
          dplyr::arrange(dplyr::desc(mean))

        lf <- input$limit_firms
        if (!is.null(lf) && !is.na(lf) && lf > 0) d <- utils::head(d, lf)
        as.character(d$technology)
      })

      # Live mirror: write the selection into the hidden persist input, which
      # the bookmark observer in server.R serialises into the URL (the input
      # is auto-included by the `country-` keep rule). We persist the bars
      # that are actually *visible* rather than the full tree selection, so
      # picking many firms but showing only a few keeps the shareable URL
      # short. On restore those bars re-check the matching leaves and, with
      # the (also bookmarked) filter values, reproduce the identical chart.
      # Fall back to the full selection only when nothing is drawn yet, so
      # the bookmark is never blanked while the chart is still computing.
      shiny::observe({
        visible <- visible_firm_bars()
        tokens  <- if (length(visible)) visible else firm_categories_selected()
        txt <- paste(tokens, collapse = "||")
        shiny::updateTextInput(session, "firm_categories_picked_persist",
                                value = txt)
      })

      # Reads the checked nodes out of the tree and splits them into the two
      # bar kinds using each node's `ancestry` (the chain of parent names
      # that `get_selected()` attaches). A node whose top-level ancestor is
      # "Broad sectors" and whose name is a sector → an aggregate bar; a
      # node under "Individual firms" whose name is a firm → that firm's
      # bar. Sector *folder* names under "Individual firms" and the two
      # branch headers are ignored (ticking a folder cascade-checks its firm
      # leaves, which already carry the real selection). Aggregates are kept
      # on top so bar order is predictable. No mutual exclusion: a sector's
      # aggregate and its individual firms can both be selected at once.
      firm_categories_selected <- shiny::reactive({
        tree_state <- input$firm_categories_tree
        if (is.null(tree_state)) return(character(0))
        sel <- shinyTree::get_selected(tree_state, format = "names")
        if (length(sel) == 0) return(character(0))

        agg_secs <- character(0)
        firms    <- character(0)
        for (node in sel) {
          nm  <- as.character(node)
          anc <- attr(node, "ancestry")
          top <- if (length(anc)) anc[[1]] else NA_character_
          if (identical(top, broad_branch) &&
              nm %in% names(firm_sector_groups)) {
            agg_secs <- c(agg_secs, nm)
          } else if (identical(top, firms_branch) &&
                     !(nm %in% names(firm_sector_groups))) {
            firms <- c(firms, nm)
          }
        }
        unique(c(agg_secs, firms))
      })

      # The single tree is now the only control, so the combined selection
      # passed to `build_firm_categories_filter_v2()` is just its output.
      firm_categories_combined <- firm_categories_selected

      # ── Firm FILTER tree (replaces the old `firm` selectize) ──────────
      # Only the individual-firms branch is needed: this restricts the
      # query rather than defining bar categories. Ticking firms (or a
      # whole sector folder) filters; nothing ticked = no firm filter.
      # Collapsed by default. The selection is mirrored into a compact,
      # bookmarkable hidden input and restored from the URL on load.
      firm_filter_tree_data <- local({
        url_q <- shiny::parseQueryString(session$clientData$url_search %||% "")
        raw   <- url_q[[session$ns("firm_tree_persist")]]
        picks <- if (!is.null(raw) && nzchar(raw)) {
          val <- tryCatch(jsonlite::fromJSON(raw),
                          error = function(e) gsub('^"|"$', "", raw))
          if (is.null(val) || !length(val) || identical(val, "")) character(0)
          else strsplit(as.character(val), "||", fixed = TRUE)[[1]]
        } else character(0)
        build_firm_filter_tree(picks)
      })
      output$firm_tree <- shinyTree::renderTree(firm_filter_tree_data)

      firm_filter_firms <- shiny::reactive({
        firm_tree_selected_firms(input$firm_tree)
      })

      shiny::observe({
        shiny::updateTextInput(session, "firm_tree_persist",
                                value = paste(firm_filter_firms(),
                                              collapse = "||"))
      })

      # ── Value Flow tree (single-select; drives the hidden `toflow`) ────
      # Clicking a leaf writes its flow value into the hidden selectize so
      # all the existing query/cache/deep-link code keeps reading
      # `input$toflow` unchanged. Branch clicks map to no value and are
      # ignored, leaving the current flow intact.
      output$toflow_tree <- shinyTree::renderTree(
        build_toflow_tree_data(
          toflow_init_value(session$clientData$url_search,
                            session$ns("toflow"))))
      shiny::observeEvent(input$toflow_tree, {
        v <- toflow_tree_value(input$toflow_tree)
        if (!is.null(v) && !identical(v, input$toflow))
          shiny::updateSelectizeInput(session, "toflow", selected = v)
      }, ignoreInit = TRUE)

      # ── Technology Categories tree (multi-select; drives hidden selectize)
      output$tech_categories_tree <- shinyTree::renderTree({
        iv <- tech_url_selected(session$clientData$url_search,
                                session$ns("tech_categories_plot1"))
        if (is.null(iv)) iv <- c("AI", "Green Technology",
                                 "Any Agriculture & Food technology",
                                 "Defence Technology")
        build_tech_category_tree(iv)
      })
      shiny::observeEvent(input$tech_categories_tree, {
        v   <- tech_category_tree_selected(input$tech_categories_tree)
        cur <- input$tech_categories_plot1; if (is.null(cur)) cur <- character(0)
        if (!identical(sort(v), sort(cur)))
          shiny::updateSelectizeInput(session, "tech_categories_plot1",
                                      selected = v)
      }, ignoreInit = TRUE)

      # ── Technologies-included filter tree (drives hidden `techs`) ──────
      output$tech_filter_tree <- shinyTree::renderTree({
        iv <- tech_url_selected(session$clientData$url_search,
                                session$ns("techs"), deep_link_param = "tech")
        build_tech_category_tree(if (is.null(iv)) character(0) else iv)
      })
      shiny::observeEvent(input$tech_filter_tree, {
        v   <- tech_category_tree_selected(input$tech_filter_tree)
        if (length(v) == 0) v <- "All innovations"   # canonical no-filter
        cur <- input$techs; if (is.null(cur)) cur <- character(0)
        if (!identical(sort(v), sort(cur)))
          shiny::updateSelectizeInput(session, "techs", selected = v)
      }, ignoreInit = TRUE)

      # ── Country/Group tree (drives the hidden `country` selectize) ────
      output$country_tree <- shinyTree::renderTree({
        iv <- tech_url_selected(session$clientData$url_search,
                                session$ns("country"), deep_link_param = "ctry")
        build_country_tree(if (is.null(iv)) "All countries" else iv)
      })
      shiny::observeEvent(input$country_tree, {
        v   <- country_tree_selected(input$country_tree)
        if (length(v) == 0) v <- "All countries"   # canonical "everything"
        cur <- input$country; if (is.null(cur)) cur <- character(0)
        if (!identical(sort(v), sort(cur)))
          shiny::updateSelectizeInput(session, "country", selected = v)
      }, ignoreInit = TRUE)

      # ── Country CATEGORIES tree (defines the bars on by-country / RTA) ──
      # Like the firm-categories tree but for countries: group leaves are
      # aggregate bars, individual-country leaves are single-country bars.
      output$country_categories_tree <- shinyTree::renderTree(local({
        url_q <- shiny::parseQueryString(session$clientData$url_search %||% "")
        raw   <- url_q[[session$ns("country_categories_persist")]]
        picks <- if (!is.null(raw) && nzchar(raw)) {
          val <- tryCatch(jsonlite::fromJSON(raw),
                          error = function(e) gsub('^"|"$', "", raw))
          if (is.null(val) || !length(val) || identical(val, "")) character(0)
          else strsplit(as.character(val), "||", fixed = TRUE)[[1]]
        } else character(0)
        if (length(picks) == 0)
          picks <- c("EU countries", "US", "CN", "GB", "JP", "IN")
        build_country_tree(picks)
      }))

      country_categories_selected <- shiny::reactive({
        country_tree_selected(input$country_categories_tree)
      })
      shiny::observe({
        shiny::updateTextInput(
          session, "country_categories_persist",
          value = paste(country_categories_selected(), collapse = "||"))
      })

      # Per-country-category data: one row per bar (group aggregate or single
      # country), with group-level docdb dedup handled in SQL, plus per-bar
      # RTA computed in R from the no-tech-filter denominator.
      fallback_by_country_cat <- shiny::reactive({
        shiny::req(input$toflow, input$country)
        cat_filters <- build_country_categories_filter_v2(
          country_categories_selected())
        if (length(cat_filters) == 0) return(NULL)

        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")
        selected_firms     <- firm_filter_firms()
        firm_clause <- build_firm_clause_v2(
          selected_firms, no_filter = length(selected_firms) == 0)

        out <- DBI::dbGetQuery(con, sql_country_countrycat_combined_v2(
          toflow = input$toflow, country_sql = country_sql,
          cat_filters = cat_filters, firm_clause = firm_clause,
          techs = input$techs %||% "All", top_n_ids = input$top_n_ids,
          granted_only = isTRUE(input$granted_only),
          multifam_only = isTRUE(input$multifam_only),
          exclude_um = isTRUE(input$exclude_um)))
        if (nrow(out) == 0) return(NULL)

        allinnos <- DBI::dbGetQuery(con, sql_countrycat_allinnos_v2(
          toflow = input$toflow, country_sql = country_sql,
          cat_filters = cat_filters, firm_clause = firm_clause,
          granted_only = isTRUE(input$granted_only),
          multifam_only = isTRUE(input$multifam_only),
          exclude_um = isTRUE(input$exclude_um)))

        out <- out |>
          dplyr::left_join(
            dplyr::rename(allinnos, technology = country_category,
                          Allinnos = allinnos),
            by = "technology") |>
          dplyr::mutate(
            ctry_code = technology,   # bar label for the RTA plotter
            sector = dplyr::if_else(technology %in% names(group_definitions),
                                    "Country group", "Individual country"),
            top3_ids_url = build_espacenet_search(top3_ids))

        if (grepl("^(is_|av_)", input$toflow)) {
          pct_cols <- intersect(c("mean", "allmean", "sem", "q1", "q2", "q3",
                                  "top25_bin_mean", "top50_bin_mean"),
                                names(out))
          out[pct_cols] <- out[pct_cols] * 100
        }

        # Universe denominator: distinct families in the full dataset after
        # country/firm/etc filters but WITHOUT the technology filter. Used
        # for the benchmark share below and for the RTA innovation-count
        # tooltip (the numerator total is already the `allinnos` column).
        denom <- DBI::dbGetQuery(con, sql_universe_allinnos_v2(
          toflow = input$toflow, country_sql = country_sql,
          firm_clause = firm_clause,
          granted_only = isTRUE(input$granted_only),
          multifam_only = isTRUE(input$multifam_only),
          exclude_um = isTRUE(input$exclude_um)))$allinnos[1]

        # Per-bar Balassa RTA. The benchmark share must be independent of
        # which categories happen to be selected: universe innovations in
        # the selected techs (`allinnos`, from the combined query's
        # overall_stats CTE) over universe innovations without the tech
        # filter (`denom`). Summing innos/Allinnos over the selected bars
        # instead moves every bar's RTA whenever a category is added —
        # overlapping groups (e.g. HIC alongside US) get double-counted
        # and the selection rarely spans the universe.
        out <- out |>
          dplyr::mutate(
            share_c = innos / Allinnos,
            share   = allinnos / denom,
            RTA     = 2 * share_c / (share_c + share))

        attr(out, "denom") <- denom
        out
      }) |> shiny::bindCache(
        input$toflow, input$country, sort(country_categories_selected()),
        sort(input$techs), sort(firm_filter_firms()), input$top_n_ids,
        isTRUE(input$granted_only), isTRUE(input$multifam_only),
        isTRUE(input$exclude_um)) |>
        shiny::bindEvent(apply_trigger())

      # DuckDB query for Plot 1 (by-technology)
      fallback_by_tech <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$tech_categories_plot1)

        toflow             <- input$toflow
        selected_firms     <- firm_filter_firms()
        selected_countries <- expand_country_selection(input$country)
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause  <- build_firm_clause_v2(selected_firms,
                                             no_filter = length(selected_firms) == 0)
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
                             sort(firm_filter_firms()), input$top_n_ids,
                             isTRUE(input$granted_only), isTRUE(input$multifam_only), isTRUE(input$exclude_um),
                             sort(input$city), isTRUE(input$include_fallback)) |>
        shiny::bindEvent(apply_trigger())

      # DuckDB query for Plot 2 / World Map (by-country)
      fallback_by_country <- shiny::reactive({
        shiny::req(input$toflow, input$country, input$techs)

        selected_countries <- expand_country_selection(input$country)
        toflow             <- input$toflow
        selected_firms     <- firm_filter_firms()
        techs              <- input$techs
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")

        firm_clause <- build_firm_clause_v2(selected_firms,
                                            no_filter = length(selected_firms) == 0)

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
                             sort(firm_filter_firms()), input$top_n_ids,
                             isTRUE(input$granted_only), isTRUE(input$multifam_only), isTRUE(input$exclude_um)) |>
        shiny::bindEvent(apply_trigger())

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
          sort_bars        = isTRUE(input$sort_tech_bars),
          limit_n          = input$limit_techs,
          min_innos        = input$min_innos_tech %||% 10,
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
      }) |> shiny::bindEvent(apply_trigger())

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
                             isTRUE(input$exclude_um)) |>
        shiny::bindEvent(apply_trigger())

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
          sort_bars    = isTRUE(input$sort_firm_bars),
          limit_firms  = input$limit_firms,
          min_innos    = input$min_innos %||% 10,
          plot_title   = sub("^[^.]*\\.", "", flow_label)
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot_by_firm_gg   <- result$ggplot
          plot_store$avstrax_plot_by_firm_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      }) |> shiny::bindEvent(apply_trigger())

      # Chart 2: Returns by Country for Selected Technologies
      output$avstrax_plot2 <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$topn, input$mininno,
            input$widthscale, input$display_mode, !is.null(input$top_n_ids),
            length(country_categories_selected()) > 0)

        flow_label <- names(unlist(toflow_choices))[
          unlist(toflow_choices) == input$toflow]
        pdata <- fallback_by_country_cat()
        if (is.null(pdata) || nrow(pdata) == 0) return(NULL)

        # One bar per selected country category (group aggregates + single
        # countries), rendered with the same per-category bar plotter used
        # by the by-firm tab. "Top N countries" / "Minimum innovations"
        # reused as the limit / min-innovation controls.
        result <- plot_avstrax_by_firm(
          pdata        = pdata,
          toflow       = input$toflow,
          widthscale   = input$widthscale,
          display_mode = input$display_mode,
          top_n_ids    = input$top_n_ids,
          sort_bars    = TRUE,
          limit_firms  = input$topn,
          min_innos    = input$mininno %||% 1,
          plot_title   = paste0(sub("^[^.]*\\.", "", flow_label), " - ",
                                paste(input$techs %||% "All innovations",
                                      collapse = ", "))
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_gg   <- result$ggplot
          plot_store$avstrax_plot2_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      }) |> shiny::bindEvent(apply_trigger())
      
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
      }) |> shiny::bindEvent(apply_trigger())

      # ── RTA Plots ─────────────────────────────────────────────────────────

      # RTA Bar Chart
      output$avstrax_plot2_rta <- ggiraph::renderGirafe({
        req(input$country, input$toflow, input$techs,
            input$topn_rta, input$bottomn_rta,
            input$mininno_rta, input$minallinnos_rta,
            input$widthscale,
            length(country_categories_selected()) > 0)

        flow_label       <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
        # One bar per selected country category (group aggregates + single
        # countries), with per-bar RTA computed in fallback_by_country_cat().
        precomputed_data <- fallback_by_country_cat()
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
          precomputed_avstrax = precomputed_data,
          denom               = attr(precomputed_data, "denom")
        )

        if (!is.null(result$ggplot)) {
          plot_store$avstrax_plot2_rta_gg   <- result$ggplot
          plot_store$avstrax_plot2_rta_data <- result$plot_data
          result$girafe
        } else {
          result
        }
      }) |> shiny::bindEvent(apply_trigger())

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
      }) |> shiny::bindEvent(apply_trigger())

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
      }) |> shiny::bindEvent(apply_trigger())

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
      }) |> shiny::bindEvent(apply_trigger())

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