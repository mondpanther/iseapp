#' HiGGlobe module Sidebar
#'
#' Same global filters as the "Value flows by Country" view in the Country
#' Explorer (country / firm / techs / granted_only / toflow), plus a sampling
#' rate. The "Render Map" button runs the filter, samples the given share of
#' surviving (family, country) rows, inner-joins to the countrymap table
#' (docdb_family_id x ctry_code -> lat/lon/city), and plots the locations on
#' a world map.
#'
#' @param id the ID of the module
#' @keywords internal
hglobe_module_sidebar <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    shiny::div(
      shiny::h5("GLOBAL FILTERS", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("country"),
          label   = "Country or Group",
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
          label   = "Firm or Sector Group",
          choices = firm_grouped_choices,
          selected = "No firm filter",
          multiple = TRUE,
          options = list(placeholder = 'Choose firms or sector groups...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("techs"),
          label   = "Technologies included",
          choices = grouped_techs,
          selected = "All innovations",
          multiple = TRUE,
          options = list(placeholder = 'Choose technologies...')
        )
      ),
      # City filter — restricts the patent universe to docdbs whose
      # countrymap row points at one of the chosen cities. The "No city
      # filter" sentinel is the default and means "ignore", same idiom
      # we use for firms above. Server-side option enabled because the
      # full city list can run into the thousands.
      shiny::div(
        class = "side_input",
        # `choices = NULL` here. The full city list is pushed into the
        # widget by `updateSelectizeInput(..., server = TRUE)` in the
        # module server below — that's the only way to enable Shiny's
        # genuine server-side selectize. Setting `server = TRUE` inside
        # `options =` does NOT do the same thing and triggers the
        # large-options performance warning.
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
          ),
          # When unticked (default), rows whose countrymap.city is the
          # country's capital used as a *fallback* (geocode_missing =
          # TRUE) are dropped from both the seed and any follow-on
          # generation. Tick to include them back — useful when you
          # want the "country-level" view rather than the
          # geographically-precise one.
          shiny::checkboxInput(
            inputId = ns("include_fallback"),
            label   = "Include capital city fallback",
            value   = FALSE
          )
        )
      )
    ),

    shiny::div(
      shiny::h5("VALUE FLOW", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::selectizeInput(
          inputId = ns("toflow"),
          label    = NULL,
          choices  = toflow_choices,
          selected = "ev_global",
          multiple = FALSE,
          width    = "400px"
        )
      )
    ),

    # Compact CSS shared by both sampling sections: sit the two
    # radioGroupButtons rows next to each other on one line and shrink
    # their bottom margin so the integer input below docks tight.
    # Padding / font-size overrides go beyond what bslib's `size = "xs"`
    # offers — they let both toggle pairs fit on a single 330 px sidebar
    # line without wrapping.
    shiny::tags$style(shiny::HTML(
      ".hglobe-toggle-row { display:flex; gap:6px; flex-wrap:nowrap;
                            align-items:center; margin-bottom: 4px; }
       .hglobe-toggle-row .shiny-input-radiogroup { margin-bottom: 0; }
       .hglobe-toggle-row .btn-group { flex-wrap: nowrap; }
       .hglobe-toggle-row .btn { padding: 1px 7px; font-size: 11px;
                                  line-height: 1.4; }
       .hglobe-sample-section { margin-bottom: 4px; }
       .hglobe-sample-section .form-group { margin-bottom: 6px; }"
    )),

    # ----- SECTION 1: Generation 0 sampling --------------------------------
    # Two segmented toggles (Top|Random + Percent|Number) decide HOW the
    # filtered universe is reduced to the seed set. The single integer
    # input below carries either a percentage (0-100) or a row count,
    # depending on the right-hand toggle. The label flips dynamically in
    # an observer below.
    shiny::div(
      class = "hglobe-sample-section",
      shiny::h5("GENERATION 0 SAMPLING",
                style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "hglobe-toggle-row",
        shinyWidgets::radioGroupButtons(
          inputId  = ns("gen0_select_mode"),
          label    = NULL,
          choices  = c("Random", "Top"),
          selected = "Random",
          size     = "xs"
        ),
        shinyWidgets::radioGroupButtons(
          inputId  = ns("gen0_unit_mode"),
          label    = NULL,
          choices  = c("Percent", "Number"),
          selected = "Percent",
          size     = "xs"
        )
      ),
      # Sample-value input + Initiate button on the same row. The input
      # is narrowed to ~95 px so the button can grow to fill the
      # remaining sidebar width without wrapping. `.hglobe-gen-row`
      # already zeroes the form-group margin so the button bottom-aligns
      # with the input baseline.
      shiny::tags$div(
        class = "hglobe-gen-row",
        style = "display: flex; gap: 8px; align-items: flex-end;",
        shiny::div(
          style = "flex: 0 0 95px;",
          shiny::numericInput(
            inputId = ns("gen0_sample_val"),
            label   = "Sampling rate [%]",
            value   = 1,
            min     = 0,
            max     = 100,
            step    = 0.1
          )
        ),
        shiny::div(
          style = "flex: 1; min-width: 0;",
          bslib::input_task_button(
            ns("render_map"),
            "Initiate Innovation",
            label_busy = "Initiating...",
            class = "btn-primary",
            width = "100%"
          )
        )
      )
    ),

    # ----- SECTION 2: Edge sampling (citation linkages) --------------------
    # Same shape as section 1 but applied to the (next-generation docdb x
    # ctry) candidates citing the previous frontier. Top mode picks the
    # citing units with the highest toflow; Random falls back to the
    # bernoulli/reservoir sample DuckDB has shipped from the start.
    shiny::div(
      class = "hglobe-sample-section",
      shiny::h5("EDGE SAMPLING",
                style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "hglobe-toggle-row",
        shinyWidgets::radioGroupButtons(
          inputId  = ns("edge_select_mode"),
          label    = NULL,
          choices  = c("Random", "Top"),
          selected = "Random",
          size     = "xs"
        ),
        shinyWidgets::radioGroupButtons(
          inputId  = ns("edge_unit_mode"),
          label    = NULL,
          choices  = c("Percent", "Number"),
          selected = "Percent",
          size     = "xs"
        )
      ),
      # Three controls on one row: edge sample value, Add Generations,
      # Generate button. Both numeric inputs are narrowed (95 px / 80 px)
      # so the button still has room to render its label.
      # `.hglobe-gen-row` zeroes the form-group margin so the button
      # bottom-aligns with the input baselines.
      shiny::tags$div(
        class = "hglobe-gen-row",
        style = paste0(
          "display: flex; gap: 8px; align-items: flex-end; margin-top: 6px;"
        ),
        shiny::tags$style(shiny::HTML(
          ".hglobe-gen-row .form-group { margin-bottom: 0; }
           .hglobe-gen-row .shiny-input-container { width: 100%; }"
        )),
        shiny::div(
          style = "flex: 0 0 95px;",
          shiny::numericInput(
            inputId = ns("edge_sample_val"),
            label   = "Sampling rate [%]",
            value   = 1,
            min     = 0,
            max     = 100,
            step    = 0.1
          )
        ),
        shiny::div(
          style = "flex: 0 0 80px;",
          shiny::numericInput(
            inputId = ns("add_generations"),
            label   = "Add Gens",
            value   = 1,
            min     = 1,
            step    = 1
          )
        ),
        shiny::div(
          style = "flex: 1; min-width: 0; position: relative;",
          bslib::input_task_button(
            ns("next_step"),
            "Generate",
            label_busy = "Generating...",
            class = "btn-secondary",
            width = "100%"
          ),
          # Persistent spinner overlay shown for the *whole* multi-
          # generation batch, not just one iteration. The bslib task
          # button reverts to its idle look between iterations because
          # each iteration is a separate observer firing; this overlay
          # keeps the user aware that the server is still working until
          # pending_gens hits 0.
          shiny::tags$div(
            id    = ns("next_step_spinner"),
            style = paste0(
              "display:none;position:absolute;top:50%;right:10px;",
              "transform:translateY(-50%);pointer-events:none;"),
            shiny::tags$span(
              class = "spinner-border spinner-border-sm",
              role  = "status",
              `aria-hidden` = "true"
            )
          )
        )
      ),
      # Pre-flight safety cap. Before each generation's heavy candidate
      # join runs, a cheap COUNT over `citenet` projects how many citing
      # docdbs the frontier would expand to. If it exceeds this limit
      # the iteration is aborted with a clear status message — saves
      # the user from a 100K-node browser freeze. Raise it if your
      # machine is comfortable; lower for conservative exploration.
      shiny::div(
        style = "margin-top: 8px;",
        shiny::numericInput(
          inputId = ns("max_iter_nodes"),
          label   = "Max citing nodes per iteration",
          value   = 50000,
          min     = 100,
          step    = 1000,
          width   = "100%"
        )
      ),
      # Emergency stop — only visible while a batch is in flight (i.e.
      # pending_gens > 0). Aborts the deferred-iteration chain so the
      # user regains control after the *currently* drawing iteration
      # finishes; the chain built so far is preserved (Render Map is the
      # button that resets everything).
      shiny::tags$div(
        id    = ns("stop_generation_wrapper"),
        style = "display:none; margin-top: 8px;",
        shiny::actionButton(
          inputId = ns("stop_generation"),
          label   = "Stop generating",
          class   = "btn-danger btn-sm",
          width   = "100%",
          icon    = shiny::icon("stop")
        )
      )
    )
  )
}

#' HiGGlobe module UI
#'
#' @param id the ID of the module
#' @keywords internal
hglobe_module_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id    = ns("sidebar"),
      open  = TRUE,
      width = 330,
      hglobe_module_sidebar(id)
    ),

    shiny::div(
      style = "padding: 20px;",
      shiny::div(
        style = "margin-bottom: 16px;",
        shiny::h2("HiGGlo - The Hidden Giants Globe",
                  style = "margin: 0 0 4px 0; font-weight: 600;"),
        shiny::div(
          "Visualize direct and indirect knowledge spillovers",
          style = "color: #666; font-size: 1.05em;"
        )
      ),
      # html2canvas drives both the "Save PNG" download and the "Copy to
      # clipboard" path. Loaded once from CDN; no R-package dependency.
      shiny::tags$head(
        # CSS hook: when an external capture (chromote, Selenium, etc.)
        # adds the `higglobe-capture-mode` class to <body> before
        # screenshot, the leaflet zoom +/- controls disappear from the
        # rasterised image. The interactive UI keeps them as long as
        # nobody flips the class. The html2canvas Save PNG path already
        # excludes them via its `ignoreElements` callback below, so
        # this CSS rule is the canonical "make the map clean for
        # screenshots" hook for any non-html2canvas capture mechanism.
        shiny::tags$style(shiny::HTML(
          "body.higglobe-capture-mode .leaflet-control-zoom {
             display: none !important;
           }"
        )),
        shiny::tags$script(
          src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"
        ),
        shiny::tags$script(shiny::HTML(
          "window.higglobe_capture = async function(elId, action) {
             const node = document.getElementById(elId);
             if (!node) { console.warn('higglobe_capture: no element', elId); return; }
             try {
               const canvas = await html2canvas(node, {
                 useCORS: true, allowTaint: true, scale: 2,
                 backgroundColor: '#ffffff',
                 // Skip the +/- zoom control from the rasterised image.
                 // Add other selectors here if you want to drop more
                 // overlay controls (e.g. layer picker) from the export.
                 ignoreElements: function(el) {
                   return !!(el && el.classList &&
                             el.classList.contains('leaflet-control-zoom'));
                 }
               });
               if (action === 'save') {
                 const a = document.createElement('a');
                 a.href = canvas.toDataURL('image/png');
                 a.download = 'higglobe-map.png';
                 document.body.appendChild(a);
                 a.click();
                 document.body.removeChild(a);
               } else if (action === 'copy') {
                 canvas.toBlob(async function(blob) {
                   try {
                     await navigator.clipboard.write([
                       new ClipboardItem({ 'image/png': blob })
                     ]);
                     console.log('Copied map to clipboard.');
                   } catch (e) {
                     alert('Clipboard copy failed: ' + e.message +
                       '\\nNote: requires HTTPS (or localhost) and a recent browser.');
                   }
                 }, 'image/png');
               }
             } catch (e) {
               alert('Map capture failed: ' + e.message);
             }
           };"
        ))
      ),

      shiny::div(
        style = "margin-bottom: 10px; color: #666; font-size: 0.9em;",
        shiny::textOutput(ns("status"))
      ),
      leaflet::leafletOutput(ns("map"), height = "550px"),
      shiny::div(
        style = "display:flex; gap:8px; justify-content:flex-end; margin-top:10px;",
        shiny::actionButton(ns("save_png"),
                            shiny::tagList(shiny::icon("download"), "Save PNG"),
                            class = "btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("copy_clip"),
                            shiny::tagList(shiny::icon("copy"),
                                           "Copy to clipboard"),
                            class = "btn-sm btn-outline-secondary")
      ),
      shiny::div(
        style = "margin-top: 15px;",
        shiny::uiOutput(ns("stats_ui"))
      ),
      shiny::div(
        style = paste0(
          "margin-top: 14px; color: #555; font-size: 1.05em; ",
          "line-height: 1.5; max-width: 60em;"),
        shiny::tags$strong("Notes:"),
        " The HiGGlo tool allows you to trace the direct and indirect ",
        "knowledge spillovers from a set of initial innovations. To make ",
        "handling a potentially large number of innovations and associated ",
        "citations tractable, the tool allows to draw a sample from a larger ",
        "population as well as only displaying a random sample of the ",
        "associated citation edges. With this you can uncover ",
        shiny::tags$em("giants"),
        " - innovations that are heavily cited - and ",
        shiny::tags$em("hidden giants"),
        " - innovations that are not cited very much directly but have a ",
        "strong impact via indirect citation links."
      )
    )
  )
}

#' HiGGlobe module Server
#'
#' @param id the ID of the module
#' @param con DuckDB connection shared with other modules; must expose the
#'   `full_patent_database`, `patents_x_tech`, `tech_lookup`, `patents_x_firm`
#'   and `countrymap` views/tables.
#' @keywords internal
hglobe_module_server <- function(id, con) {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Server-side selectize for the City filter. The full
      # `city_grouped_choices` list runs to thousands of entries;
      # pushing them all to the browser at UI-init time triggers
      # Shiny's large-options performance warning. Doing it via
      # updateSelectizeInput(server = TRUE) defers the heavy lifting
      # to incremental, type-as-you-go server queries instead.
      #
      # Bookmark restore: reading `isolate(input$city)` was unreliable
      # because the HiGGlo module is initialised LAZILY (only after
      # the user lands on the HiGGlobe tab). By that point Shiny has
      # already fired the bookmark-restore pass, but the selectize
      # widget client-side never had any options registered, so the
      # restored value can be silently dropped. The fix is to bypass
      # Shiny's bookmark machinery entirely and read the original URL
      # query string directly. `session$clientData$url_search` is the
      # URL the page was loaded with — stable across the lazy-init
      # gap.
      url_q     <- shiny::parseQueryString(
        session$clientData$url_search %||% "")
      city_key  <- session$ns("city")
      raw_city  <- url_q[[city_key]]
      restored_city <- if (!is.null(raw_city) && nzchar(raw_city)) {
        # Shiny URL-encodes scalar strings as `"X"` and arrays as JSON
        # `["X","Y"]`. fromJSON handles both; if anything goes wrong
        # we fall back to the literal value with surrounding quotes
        # stripped.
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

      # "Include all categories" expansion — mirrors country-tab behaviour.
      shiny::observeEvent(input$techs, {
        if ("__CLEAR_TECHS__" %in% input$techs) {
          shiny::updateSelectizeInput(session, "techs", selected = character(0))
          return()
        }
        if ("All categories" %in% input$techs) {
          new_sel <- unique(c(setdiff(input$techs, "All categories"),
                              all_broad_techs))
          shiny::updateSelectizeInput(session, "techs", selected = new_sel)
        }
      })

      # "No firm filter" vs. specific firms — mirrors country-tab behaviour.
      shiny::observeEvent(input$firm, {
        sel <- input$firm
        if ("No firm filter" %in% sel && length(sel) > 1) {
          shiny::updateSelectizeInput(session, "firm",
                                      selected = setdiff(sel, "No firm filter"))
        }
      })

      # Save PNG / Copy to clipboard — both invoke the same client-side
      # html2canvas capture; the JS branch decides how to dispatch the canvas
      # (download vs Clipboard API). Using shinyjs::runjs avoids the need to
      # register a custom message handler.
      # `ignoreInit = TRUE` is essential here: without it the observer fires
      # once on module init (button value transitions NULL -> 0) and would
      # spuriously trigger a download / clipboard write on every page load.
      shiny::observeEvent(input$save_png, {
        shinyjs::runjs(sprintf(
          "higglobe_capture('%s', 'save');", session$ns("map")))
      }, ignoreInit = TRUE)
      shiny::observeEvent(input$copy_clip, {
        shinyjs::runjs(sprintf(
          "higglobe_capture('%s', 'copy');", session$ns("map")))
      }, ignoreInit = TRUE)

      # Re-label / re-bound the integer input under each sampling section
      # when the Percent|Number toggle flips. Percent stays clamped to
      # 0-100 with a 0.1 step (so 1.5 % is allowed); Number uses unbounded
      # integers with step 1.
      relabel_sample_input <- function(input_id, unit_mode) {
        if (identical(unit_mode, "Number")) {
          shiny::updateNumericInput(session, input_id,
            label = "Sample size [#]",
            min = 0, max = NA, step = 1)
        } else {
          shiny::updateNumericInput(session, input_id,
            label = "Sampling rate [%]",
            min = 0, max = 100, step = 0.1)
        }
      }
      shiny::observeEvent(input$gen0_unit_mode, {
        relabel_sample_input("gen0_sample_val", input$gen0_unit_mode)
      }, ignoreInit = TRUE)
      shiny::observeEvent(input$edge_unit_mode, {
        relabel_sample_input("edge_sample_val", input$edge_unit_mode)
      }, ignoreInit = TRUE)

      # Bootstrap an empty leaflet base layer.
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
          leaflet::addTiles() |>
          leaflet::setView(10, 20, zoom = 2)
      })

      status_msg <- shiny::reactiveVal("")
      output$status <- shiny::renderText(status_msg())

      # Accumulates one row per completed step (gen 0 seeded by Render Map,
      # gen N added by each Next step click). Each row carries its integer
      # `gen` so the per-row "Show" checkbox can address the matching
      # leaflet layer group. Reset on every Render Map.
      stats_rv <- shiny::reactiveVal(NULL)

      # Render a Bootstrap-styled HTML table where every row has a Shiny
      # checkbox. We use isolate() when reading prior checkbox state so
      # this renderUI does NOT take a reactive dependency on the checkbox
      # values themselves — otherwise ticking a box would re-render the
      # whole table and reset every other state.
      output$stats_ui <- shiny::renderUI({
        df <- stats_rv()
        if (is.null(df) || nrow(df) == 0) return(NULL)

        rows <- lapply(seq_len(nrow(df)), function(i) {
          g     <- df$gen[i]
          cb_id <- sprintf("show_gen_%d", g)
          prev  <- shiny::isolate(input[[cb_id]])
          val   <- if (is.null(prev)) TRUE else isTRUE(prev)
          fmt_num <- function(x) {
            if (is.null(x) || length(x) == 0 || is.na(x)) "—"
            else format(x, big.mark = ",", digits = 4, scientific = FALSE)
          }
          shiny::tags$tr(
            shiny::tags$td(
              shiny::checkboxInput(ns(cb_id), label = NULL, value = val,
                                   width = "30px")
            ),
            shiny::tags$td(shiny::HTML(df$color[i])),
            shiny::tags$td(df$step[i]),
            shiny::tags$td(fmt_num(df$avg_flow_pop[i])),
            shiny::tags$td(fmt_num(df$avg_flow_sample[i])),
            shiny::tags$td(fmt_num(df$avg_pv[i])),
            shiny::tags$td(format(df$nodes_found[i], big.mark = ",")),
            shiny::tags$td(format(df$nodes_kept[i],  big.mark = ","))
          )
        })

        shiny::tags$table(
          class = "table table-sm table-striped table-hover",
          style = "width: 100%;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Show"),
            shiny::tags$th("Color"),
            shiny::tags$th("Step"),
            shiny::tags$th("Avg flow (pop)"),
            shiny::tags$th("Avg flow (sample)"),
            shiny::tags$th("Avg pv"),
            shiny::tags$th("Nodes found"),
            shiny::tags$th("Nodes kept")
          )),
          shiny::tags$tbody(rows)
        )
      })

      # When any "Show" checkbox toggles, push showGroup/hideGroup to the
      # map for that generation. This is a CSS-class flip on the existing
      # leaflet layer — no re-query, no re-render, cheap regardless of how
      # many edges or markers are in the group.
      shiny::observe({
        df <- stats_rv()
        if (is.null(df) || nrow(df) == 0) return()
        proxy <- leaflet::leafletProxy("map")
        for (g in df$gen) {
          val   <- input[[sprintf("show_gen_%d", g)]]
          show  <- if (is.null(val)) TRUE else isTRUE(val)
          group <- sprintf("gen_%d", g)
          if (show) leaflet::showGroup(proxy, group)
          else      leaflet::hideGroup(proxy, group)
        }
      })

      # Color legend overlay. Lives inside the leaflet container so that
      # html2canvas (used by the Save PNG / Copy buttons) rasterises it
      # together with the map. Re-painted whenever the set of generations
      # or the per-gen visibility checkboxes change. Only currently-
      # visible generations appear, so a saved snapshot's legend always
      # matches what the user actually sees on the map.
      shiny::observe({
        df <- stats_rv()
        proxy <- leaflet::leafletProxy("map")
        # Always tear down the old control first, even if no rows — that
        # way the legend disappears between sessions instead of going
        # stale after a fresh "Initiate Innovation".
        leaflet::removeControl(proxy, layerId = "gen_legend")
        if (is.null(df) || nrow(df) == 0) return()

        # Filter to visible generations using the same default-TRUE rule
        # as the show/hide handler above.
        keep <- vapply(df$gen, function(g) {
          val <- input[[sprintf("show_gen_%d", g)]]
          if (is.null(val)) TRUE else isTRUE(val)
        }, logical(1))
        vis <- df[keep, , drop = FALSE]
        if (nrow(vis) == 0) return()

        # Per-gen swatch + short label. gen 0 is the seed, gen N is just
        # "gen N". Circles instead of squares to match the marker shape.
        items <- vapply(seq_len(nrow(vis)), function(i) {
          g     <- vis$gen[i]
          color <- pick_color(g)
          label <- if (g == 0L) "gen 0" else sprintf("gen %d", g)
          sprintf(paste0(
            '<span style="display:inline-flex;align-items:center;',
            'gap:4px;margin-right:8px;">',
            '<span style="display:inline-block;width:10px;height:10px;',
            'border-radius:50%%;background:%s;border:1px solid #555;">',
            '</span>%s</span>'),
            color, label)
        }, character(1))

        legend_html <- paste0(
          '<div style="background:rgba(255,255,255,0.92);',
          'border:1px solid #aaa;border-radius:4px;',
          'padding:4px 8px;font-size:11px;line-height:1.4;',
          'font-family:sans-serif;color:#222;',
          'box-shadow:0 1px 2px rgba(0,0,0,0.15);">',
          '<b>Generations:</b> ',
          paste(items, collapse = ""),
          '</div>'
        )

        leaflet::addControl(
          proxy,
          html     = legend_html,
          position = "bottomleft",
          layerId  = "gen_legend"
        )
      })

      # Map a vector of flow values to marker radii on a log(1 + v) scale,
      # normalised so the largest value gets the largest radius within this
      # draw. NA or non-positive values fall back to the minimum radius.
      # Defaults sized for clickability at max zoom — at radius 5 px the
      # circle is a comfortable touch/click target on most displays.
      # ---------------------------------------------------------------------
      # Deterministic per-(docdb, ctry) jitter applied at the countrymap
      # join so every generation — including arc endpoints — uses the
      # same offset for the same pair. DuckDB's hash() is stable, so
      # repeat sessions land identical points; the offset is band-
      # limited to ±`jitter_amp` degrees on each axis.
      # Returns a single SQL scalar expression evaluating to the jittered
      # value, e.g. `(CAST(hash(c.docdb_family_id, c.ctry_code, 'lat') AS
      # DOUBLE) / 18446744073709551616.0 - 0.5) * 0.5 + c.lat`.
      # ---------------------------------------------------------------------
      jitter_amp <- 0.25  # degrees, per-axis half-range
      jitter_expr <- function(alias, axis) {
        sprintf(paste0(
          "(CAST(hash(%s.docdb_family_id, %s.ctry_code, '%s') AS DOUBLE) ",
          "/ 18446744073709551616.0 - 0.5) * %s + %s.%s"),
          alias, alias, axis,
          format(2 * jitter_amp, nsmall = 4),
          alias, axis)
      }

      # ---------------------------------------------------------------------
      # SQL fragment for the HiGGlo city filter + capital-fallback toggle.
      # Both controls live in the GLOBAL FILTERS section of the sidebar
      # and are applied at the countrymap join (so they affect Gen 0 and
      # every subsequent generation identically).
      #
      # Args
      #   alias              SQL alias of the countrymap row to filter
      #                       (e.g. "c" inside the gen-0 join, "cm" inside
      #                       the gen-N candidate join).
      #   selected_cities    character vector from input$city. The
      #                       "No city filter" sentinel disables the
      #                       city restriction entirely.
      #   include_fallback   logical from input$include_fallback. TRUE
      #                       keeps capital-fallback rows; FALSE drops
      #                       any row with geocode_missing = TRUE.
      #
      # Returns a string starting with "AND ..." (or "" if no filtering),
      # safe to concatenate after an existing WHERE in any countrymap
      # join.
      # ---------------------------------------------------------------------
      build_city_filter_sql <- function(alias,
                                        selected_cities,
                                        include_fallback) {
        clauses <- character(0)

        # City whitelist
        if (length(selected_cities) > 0L &&
            !"No city filter" %in% selected_cities) {
          # Drop the sentinel if the user picked it alongside real
          # cities, keep only the actual city names. SQL-quote each.
          cs <- setdiff(selected_cities, "No city filter")
          if (length(cs) > 0L) {
            quoted <- paste0("'", gsub("'", "''", cs, fixed = TRUE),
                             "'", collapse = ", ")
            clauses <- c(clauses,
                         sprintf("%s.city IN (%s)", alias, quoted))
          }
        }

        # Capital-fallback exclusion (default)
        if (!isTRUE(include_fallback)) {
          clauses <- c(clauses,
                       sprintf("(NOT %s.geocode_missing)", alias))
        }

        if (length(clauses) == 0L) "" else paste("AND",
                                                 paste(clauses,
                                                       collapse = " AND "))
      }

      # Build the SAMPLE / ORDER BY clause appended to the row source
      # being sampled. The same logic is shared between the gen-0
      # selection (filtered docdb x ctry universe) and the per-edge
      # selection (next-generation docdb x ctry candidates citing the
      # frontier). The candidate row source is expected to expose a
      # `toflow_val` column for Top mode; both call sites already do.
      #
      # Args
      #   select_mode "Random" | "Top"
      #   unit_mode   "Percent" | "Number"
      #   sample_val  numeric. Percent: 0-100. Number: non-negative int.
      #   total_n     integer (or NA). Required for Top + Percent so the
      #               percentage can be translated into an integer LIMIT;
      #               ignored otherwise.
      #
      # Returns a SQL fragment, e.g.:
      #   "USING SAMPLE 1.5 PERCENT (bernoulli)"
      #   "USING SAMPLE 100 ROWS (reservoir)"
      #   "ORDER BY toflow_val DESC NULLS LAST LIMIT 50"
      build_sample_clause_v2 <- function(select_mode, unit_mode, sample_val,
                                         total_n = NA_integer_) {
        if (identical(select_mode, "Top")) {
          if (identical(unit_mode, "Percent")) {
            if (is.na(total_n))
              stop("Top + Percent requires total_n to size the LIMIT.")
            n <- max(0L, as.integer(ceiling(total_n * sample_val / 100)))
            return(sprintf(
              "ORDER BY toflow_val DESC NULLS LAST LIMIT %d", n))
          }
          # Top + Number
          return(sprintf(
            "ORDER BY toflow_val DESC NULLS LAST LIMIT %d",
            as.integer(sample_val)))
        }
        # Random
        if (identical(unit_mode, "Percent")) {
          return(sprintf("USING SAMPLE %s PERCENT (bernoulli)",
                         format(sample_val, nsmall = 4)))
        }
        # Random + Number
        sprintf("USING SAMPLE %d ROWS (reservoir)", as.integer(sample_val))
      }

      radius_from_flow <- function(v, r_min = 5, r_max = 16) {
        v  <- suppressWarnings(as.numeric(v))
        lv <- log1p(pmax(v, 0, na.rm = FALSE))
        lv[!is.finite(lv)] <- 0
        rng <- diff(range(lv, na.rm = TRUE))
        if (is.na(rng) || rng <= 0) return(rep((r_min + r_max) / 3, length(v)))
        r_min + (r_max - r_min) * (lv - min(lv, na.rm = TRUE)) / rng
      }

      # Session-scoped DuckDB staging tables. The app's DuckDB connection is
      # shared across sessions, so we must namespace on the session token to
      # avoid collisions. Generation N is stored at `hglobe_gen<N>_<token>`,
      # with N=0 being the Step 1 source set. Each "Next step" click reads
      # generation N-1 as its frontier and materialises generation N.
      tok <- gsub("[^A-Za-z0-9]", "", session$token %||% "")
      frontier_tbl <- function(g) sprintf("hglobe_gen%d_%s", g, tok)

      # NULL until "Render Map" seeds gen 0; otherwise = next generation to
      # produce on the next "Next step" click.
      gen_next <- shiny::reactiveVal(NULL)

      # Per-generation edge / marker color cycle so successive generations
      # are visually distinct on the map. Gen 0 (the seed) gets the first
      # entry; each subsequent generation steps to the next color.
      gen_colors <- c("#2780e3", "#d9534f", "#5cb85c", "#f0ad4e",
                      "#9467bd", "#8c564b", "#17becf", "#e377c2")
      pick_color <- function(g) gen_colors[(g %% length(gen_colors)) + 1]
      color_swatch_html <- function(hex)
        sprintf(paste0('<span style="display:inline-block;width:24px;',
                       'height:14px;background:%s;border-radius:2px;',
                       'border:1px solid #999;"></span>'), hex)

      # Build a clickable Espacenet search link from an appln_id (the EP/WO/US
      # publication number stored in patent_database.appln_id). Mirrors the
      # behaviour of build_espacenet_search() used in the bar-chart tooltips,
      # but emits a plain <a target="_blank"> for leaflet popups instead of a
      # ggiraph window.open() snippet.
      espacenet_link <- function(appln_id) {
        # utils::URLencode is scalar-only, so loop with vapply.
        vapply(as.character(appln_id), function(id) {
          if (is.na(id) || !nzchar(id)) return(NA_character_)
          sprintf(
            '<a href="https://worldwide.espacenet.com/patent/search?q=%s" target="_blank" rel="noopener">%s</a>',
            utils::URLencode(paste0("pn=", id), reserved = TRUE),
            id
          )
        }, character(1), USE.NAMES = FALSE)
      }

      drop_session_tables <- function() {
        tabs <- tryCatch(DBI::dbListTables(con),
                         error = function(e) character())
        pat  <- sprintf("^hglobe_(gen[0-9]+|edges_tmp|cand_tmp|passing_tmp)_%s$", tok)
        for (t in tabs[grepl(pat, tabs)]) {
          try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", t)),
              silent = TRUE)
        }
      }

      shiny::onSessionEnded(function() drop_session_tables())

      shiny::observeEvent(input$render_map, ignoreInit = TRUE, {
        shiny::req(input$toflow, input$country, input$techs)
        gen0_select <- input$gen0_select_mode %||% "Random"
        gen0_unit   <- input$gen0_unit_mode   %||% "Percent"
        gen0_val    <- suppressWarnings(as.numeric(input$gen0_sample_val))
        # Validate the integer / percentage up front; the actual SAMPLE /
        # ORDER BY clause is built once n_found is known (Top+Percent
        # needs the candidate count to translate the percentage into an
        # integer LIMIT).
        if (identical(gen0_unit, "Percent")) {
          if (!is.finite(gen0_val) || gen0_val < 0 || gen0_val > 100) {
            status_msg("Sampling rate must be between 0 and 100.")
            return()
          }
        } else {
          if (!is.finite(gen0_val) || gen0_val < 0) {
            status_msg("Sample size must be a non-negative integer.")
            return()
          }
        }

        selected_countries <- expand_country_selection(input$country)
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))

        country_sql    <- paste0("'", gsub("'", "''", selected_countries),
                                 "'", collapse = ", ")
        firm_clause    <- build_firm_clause_v2(selected_firms,
                                               no_filter = no_firm_filter)
        tech_bool      <- build_tech_bool_v2(input$techs)
        granted_clause     <- build_granted_clause_v2(isTRUE(input$granted_only))
        exclude_um_clause  <- build_exclude_um_clause_v2(isTRUE(input$exclude_um))
        multifam_clause <- build_multifam_clause_v2(isTRUE(input$multifam_only))

        has_tech <- tech_bool != "TRUE"
        has_firm <- nchar(trimws(firm_clause)) > 0

        # Assemble the CTE list. Each piece is a full CTE terminated with a
        # trailing comma; the last CTE (sampled) closes the chain.
        ctes <- c()
        if (has_tech) {
          ctes <- c(ctes, glue::glue(
            "filtered_tech AS (
               SELECT DISTINCT t.docdb_family_id
               FROM patents_x_tech t
               JOIN tech_lookup tl ON t.technology = tl.technology
               WHERE {tech_bool}
             )"))
        }
        if (has_firm) {
          firm_condition <- gsub("^\\s*AND\\s+", "", firm_clause)
          ctes <- c(ctes, glue::glue(
            "filtered_firm AS (
               SELECT DISTINCT f.docdb_family_id
               FROM patents_x_firm f
               WHERE {firm_condition}
             )"))
        }

        # Materialise the `passing` set so we can (a) count found-before-sample
        # and (b) pull the chosen flow value for each (docdb, ctry) alongside
        # the row for marker sizing. MAX() aggregates across duplicate
        # appln_ids per (docdb, ctry), giving one row per pair.
        #
        # The countrymap join is folded INTO this CTE so the city /
        # capital-fallback filters apply before sampling and the stats
        # rollups below — without that, "Avg flow (sample)" would still
        # be computed against the unfiltered universe.
        passing_tbl  <- sprintf("hglobe_passing_tmp_%s", tok)
        toflow_col   <- input$toflow
        city_filter  <- build_city_filter_sql(
          "c",
          selected_cities  = input$city,
          include_fallback = isTRUE(input$include_fallback)
        )
        jit_lat_pass <- jitter_expr("c", "lat")
        jit_lon_pass <- jitter_expr("c", "lon")
        passing_sql <- glue::glue("
          passing AS (
            SELECT p.docdb_family_id,
                   p.ctry_code,
                   MAX(p.{toflow_col}) AS toflow_val,
                   MAX(p.pv)             AS pv_val,
                   ANY_VALUE(p.appln_id) AS appln_id,
                   ANY_VALUE(c.city)    AS city,
                   ANY_VALUE({jit_lat_pass}) AS lat,
                   ANY_VALUE({jit_lon_pass}) AS lon,
                   BOOL_OR(c.geocode_missing) AS geocode_missing
            FROM full_patent_database p
            {if (has_tech) 'INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id' else ''}
            {if (has_firm) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
            INNER JOIN countrymap c
              ON c.docdb_family_id = p.docdb_family_id
             AND c.ctry_code       = p.ctry_code
            WHERE p.ctry_code IN ({country_sql})
              AND p.{toflow_col} IS NOT NULL
              AND c.lat IS NOT NULL AND c.lon IS NOT NULL
              {granted_clause}
              {exclude_um_clause}
              {multifam_clause}
              {city_filter}
            GROUP BY p.docdb_family_id, p.ctry_code
          )")
        ctes <- c(ctes, passing_sql)

        create_passing <- paste0(
          "CREATE OR REPLACE TABLE ", passing_tbl, " AS\n",
          "WITH ", paste(ctes, collapse = ",\n"), "\n",
          "SELECT * FROM passing"
        )

        ok <- tryCatch({ DBI::dbExecute(con, create_passing); TRUE },
                      error = function(e) {
                        status_msg(paste("Filter query failed:",
                                         conditionMessage(e))); FALSE })
        if (!ok) return()

        n_found <- DBI::dbGetQuery(con,
          sprintf("SELECT COUNT(*) AS n FROM %s", passing_tbl))$n

        # Build the gen-0 sampling clause now that we know the candidate
        # count. The clause is appended to the (passing_tbl) row source —
        # see the SQL fragment further down. Top + Percent translates the
        # percentage into an integer LIMIT against the now-known total.
        sample_clause <- build_sample_clause_v2(
          select_mode = gen0_select,
          unit_mode   = gen0_unit,
          sample_val  = gen0_val,
          total_n     = n_found
        )

        # Two means each, in one round-trip:
        #   - population mean (over the full filtered universe, before
        #     any sampling). For Percent / Number this is what users
        #     intuitively expect; for Top it lets you compare against
        #     the universe-wide average.
        #   - sample mean (over the rows sample_clause actually keeps,
        #     i.e. the rows ending up on the map). For Percent / Number
        #     this is approximately equal to the population mean; for
        #     Top mode it's the only number that tracks "what the user
        #     is looking at".
        agg_pop <- DBI::dbGetQuery(con, sprintf(
          "SELECT AVG(toflow_val) AS avg_flow, AVG(pv_val) AS avg_pv FROM %s",
          passing_tbl))
        agg_smp <- DBI::dbGetQuery(con, sprintf(
          "SELECT AVG(toflow_val) AS avg_flow, AVG(pv_val) AS avg_pv
           FROM (SELECT * FROM %s %s) sampled",
          passing_tbl, sample_clause))
        avg_flow_pop    <- agg_pop$avg_flow
        avg_flow_sample <- agg_smp$avg_flow
        avg_pv_pop      <- agg_pop$avg_pv
        avg_pv_sample   <- agg_smp$avg_pv

        # passing_tbl already carries city / lat / lon / geocode_missing
        # because the countrymap join + city + capital-fallback filters
        # were folded into the `passing` CTE above. The final read is
        # therefore just a sample applied to the prepared table.
        # Jitter on lat/lon was likewise computed inside `passing` via
        # jitter_expr("c", ...), so frontier_tbl(0) inherits the exact
        # same per-(docdb, ctry) offset used by every later generation.
        # sample_clause is one of:
        #     "USING SAMPLE <pct> PERCENT (bernoulli)"           (Percent)
        #     "USING SAMPLE <n> ROWS (reservoir)"                (Number)
        #     "ORDER BY toflow_val DESC NULLS LAST LIMIT <n>"    (Top)
        sql <- sprintf("
          SELECT docdb_family_id, ctry_code, toflow_val, appln_id,
                 city, lat, lon, geocode_missing
          FROM %s
          %s
        ", passing_tbl, sample_clause)

        dat <- tryCatch(
          DBI::dbGetQuery(con, sql),
          error = function(e) {
            status_msg(paste("Sample query failed:", conditionMessage(e)))
            NULL
          }
        )
        try(DBI::dbExecute(con,
              sprintf("DROP TABLE IF EXISTS %s", passing_tbl)),
            silent = TRUE)
        if (is.null(dat)) return()

        if (nrow(dat) == 0) {
          status_msg("No rows matched the filter + sample.")
          leaflet::leafletProxy("map") |>
            leaflet::clearMarkerClusters() |>
            leaflet::clearMarkers()
          return()
        }

        # Reset any previous generation chain — a fresh "Render Map" click
        # starts a new seed, discarding whatever `Next step` expanded before.
        drop_session_tables()
        # Bump the generation epoch and zero pending_gens so any in-flight
        # later::later() callbacks from the previous batch see a stale
        # epoch and bail out instead of continuing to enqueue Generate
        # iterations on top of the new seed.
        gen_epoch(shiny::isolate(gen_epoch()) + 1L)
        pending_gens(0L)
        seed <- data.frame(
          docdb_family_id = dat$docdb_family_id,
          ctry_code       = dat$ctry_code,
          city            = dat$city,
          lat             = dat$lat,
          lon             = dat$lon,
          toflow_val      = dat$toflow_val,
          appln_id        = dat$appln_id,
          stringsAsFactors = FALSE
        )
        DBI::dbWriteTable(con, frontier_tbl(0), seed, overwrite = TRUE)
        gen_next(1)

        # Visual cue: once gen 0 is seeded, promote the "Generate" button
        # from a muted secondary style to the primary blue so the user sees
        # the next-step affordance light up.
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('btn-secondary').addClass('btn-primary');",
          session$ns("next_step")))

        # Reset stats with the gen 0 row.
        stats_rv(data.frame(
          gen             = 0L,
          color           = color_swatch_html(pick_color(0)),
          step            = "gen 0 (seed)",
          avg_flow_pop    = avg_flow_pop,
          avg_flow_sample = avg_flow_sample,
          avg_pv          = avg_pv_sample,
          nodes_found     = n_found,
          nodes_kept      = nrow(dat),
          stringsAsFactors = FALSE,
          check.names     = FALSE
        ))

        status_msg(sprintf(
          "Seeded generation 0: %s of %s rows kept after sampling. Click 'Generate' to expand citations.",
          format(nrow(dat), big.mark = ","),
          format(n_found,   big.mark = ",")))

        # NOTE: jitter is no longer applied here — `dat$lat` / `dat$lon`
        # already carry the per-(docdb, ctry) deterministic offset from
        # the SQL query above. Doing it upstream means the seed table
        # and every downstream gen-N frontier carry the same jittered
        # coordinates, so citation arcs land exactly on the marker the
        # user clicks.

        appln_link <- espacenet_link(dat$appln_id)
        appln_html <- ifelse(
          is.na(appln_link),
          "(no appln id)",
          appln_link
        )
        popup_txt <- sprintf(
          "<b>docdb</b>: %s<br/><b>appln</b>: %s<br/><b>ctry</b>: %s<br/><b>city</b>: %s%s<br/><b>%s</b>: %s",
          dat$docdb_family_id, appln_html, dat$ctry_code,
          ifelse(is.na(dat$city), "(capital fallback)", dat$city),
          ifelse(isTRUE(dat$geocode_missing), " <i>(geocode missing)</i>", ""),
          toflow_col,
          format(dat$toflow_val, big.mark = ",", digits = 4, scientific = FALSE)
        )

        leaflet::leafletProxy("map") |>
          leaflet::clearMarkerClusters() |>
          leaflet::clearMarkers() |>
          leaflet::clearShapes() |>
          leaflet::addCircleMarkers(
            lng          = dat$lon,
            lat          = dat$lat,
            radius       = radius_from_flow(dat$toflow_val),
            stroke       = FALSE,
            fillOpacity  = 0.6,
            fillColor    = ifelse(isTRUE(dat$geocode_missing),
                                  "#bbbbbb", pick_color(0)),
            popup        = popup_txt,
            group        = "gen_0"
          )
      })

      # Quadratic-bezier curve between two points used to draw citation arcs.
      # `bend` controls the peak height relative to the source->target distance.
      make_curve <- function(sx, sy, tx, ty, n = 40, bend = 0.3) {
        if (sx == tx && sy == ty)
          return(data.frame(lon = c(sx, tx), lat = c(sy, ty)))
        mx <- (sx + tx) / 2; my <- (sy + ty) / 2
        dx <- tx - sx;       dy <- ty - sy
        d  <- sqrt(dx^2 + dy^2); if (d == 0) d <- 1
        px <- -dy / d;       py <-  dx / d
        h  <- bend * d
        cx <- mx + px * h;   cy <- my + py * h
        tt <- seq(0, 1, length.out = n)
        data.frame(
          lon = (1 - tt)^2 * sx + 2 * (1 - tt) * tt * cx + tt^2 * tx,
          lat = (1 - tt)^2 * sy + 2 * (1 - tt) * tt * cy + tt^2 * ty
        )
      }

      # ---------------- Step 2+: recursive citation expansion -----------------
      #
      # Each press of "Next step" advances one generation:
      #   gen 0 = source set (seeded by "Render Map")
      #   gen 1 = docdbs that cite gen 0
      #   gen 2 = docdbs that cite gen 1
      #   ...
      # For generation N, we LEFT JOIN citenet on
      #   cited_docdb_family_id = frontier_gen(N-1).docdb_family_id
      # which means citenet's other column (`docdb_family_id`, the citing
      # side) becomes the new frontier. We rename:
      #   cited_docdb_family_id -> prev_docdb_family_id
      #   docdb_family_id       -> next_docdb_family_id
      # to keep the semantics explicit at each level. The candidate set
      # of next-generation (docdb x ctry) targets is sampled per the
      # EDGE SAMPLING toggles (Random/Top + Percent/Number) before
      # rebuilding the (prev, next) edge rows; the resulting distinct
      # next-generation docdbs are persisted as the new frontier table
      # for the next click.
      # Per-iteration worker: advances exactly ONE generation. Returns TRUE
      # on success, FALSE if the SQL failed or there were zero citations
      # (the latter still appends a stats row before returning). Defined
      # here so both the immediate first-iteration path and the deferred
      # later::later() path can reuse it.
      do_one_generation <- function(edge_select, edge_unit, edge_val,
                                    toflow_col, max_nodes = Inf) {
        g        <- gen_next()
        prev_tbl <- frontier_tbl(g - 1)
        new_tbl  <- frontier_tbl(g)
        tmp_tbl  <- sprintf("hglobe_edges_tmp_%s", tok)
        cand_tbl <- sprintf("hglobe_cand_tmp_%s",  tok)

        # Pre-flight cap: before the heavy candidate join runs, cheaply
        # project how many citing docdbs the previous frontier would
        # expand to (citenet-only query, no countrymap join — uses the
        # cited-side index, sub-second even for million-row frontiers).
        # If it blows past the user's limit, abort *before* doing any
        # expensive work; otherwise the user is stuck waiting for the
        # candidate table to materialise just to see a giant chart.
        if (is.finite(max_nodes) && max_nodes > 0) {
          projected <- tryCatch(
            DBI::dbGetQuery(con, sprintf("
              SELECT COUNT(DISTINCT c.docdb_family_id) AS n
              FROM citenet c
              WHERE c.cited_docdb_family_id IN (
                SELECT docdb_family_id FROM %s
              )
            ", prev_tbl))$n,
            error = function(e) NA_real_
          )
          if (isTRUE(projected > max_nodes)) {
            status_msg(sprintf(
              paste0("Generation %d aborted: would expand to %s citing ",
                     "patents (limit %s). Lower the sampling rate, ",
                     "tighten the seed, or raise 'Max citing nodes per ",
                     "iteration'."),
              g,
              format(projected, big.mark = ","),
              format(max_nodes, big.mark = ",")
            ))
            return(FALSE)
          }
        }

        # Step A: candidate (next-docdb x chosen ctry) targets — one row
        # per citing docdb_family_id, with the geocoded ctry that
        # countrymap supplies. Targets are sampled in step C; we
        # materialise them first so we can both (a) count rows for
        # Top+Percent and (b) reuse the table when stitching edges.
        # Same per-(docdb, ctry) jitter formula used for the gen-0 seed.
        # Embedding it here means the candidate table — and therefore
        # the new frontier we persist below — already carries jittered
        # coordinates; arc endpoints in the next iteration land exactly
        # on the markers we draw.
        #
        # IMPORTANT: NONE of the GLOBAL FILTERS (country / firm / techs /
        # granted_only / multifam_only / city / include_fallback) are
        # applied here. They constrain the *seed* set only — citations
        # from gen 0 are free to lead anywhere. The only conditions on
        # this query are data-validity (NOT NULL coordinates and a
        # sizeable flow value), which are required for any docdb to be
        # drawable at all.
        jit_lat_cm <- jitter_expr("cm", "lat")
        jit_lon_cm <- jitter_expr("cm", "lon")
        sql_candidates <- glue::glue("
          CREATE OR REPLACE TABLE {cand_tbl} AS
          WITH citing AS (
            SELECT DISTINCT c.docdb_family_id AS docdb_family_id
            FROM citenet c
            WHERE c.cited_docdb_family_id IN (
              SELECT docdb_family_id FROM {prev_tbl}
            )
          ),
          flow_per_fam AS (
            SELECT docdb_family_id, ctry_code,
                   MAX({toflow_col})    AS toflow_val,
                   ANY_VALUE(appln_id)  AS appln_id
            FROM full_patent_database
            WHERE {toflow_col} IS NOT NULL
            GROUP BY docdb_family_id, ctry_code
          )
          SELECT DISTINCT ON (cm.docdb_family_id)
                 cm.docdb_family_id,
                 cm.ctry_code,
                 cm.city,
                 {jit_lat_cm} AS lat,
                 {jit_lon_cm} AS lon,
                 fpf.toflow_val,
                 fpf.appln_id
          FROM countrymap cm
          JOIN citing ci      ON ci.docdb_family_id  = cm.docdb_family_id
          JOIN flow_per_fam fpf
            ON fpf.docdb_family_id = cm.docdb_family_id
           AND fpf.ctry_code       = cm.ctry_code
          WHERE cm.lat IS NOT NULL AND cm.lon IS NOT NULL
          ORDER BY cm.docdb_family_id, cm.ctry_code
        ")

        ok <- tryCatch({
          DBI::dbExecute(con, sql_candidates); TRUE
        }, error = function(e) {
          status_msg(paste("Edge candidate query failed:",
                           conditionMessage(e))); FALSE
        })
        if (!ok) return(FALSE)

        # Step B: candidate count, needed by Top + Percent to translate
        # the percentage into an integer LIMIT. Cheap — DuckDB tables
        # carry row counts in metadata.
        n_candidates <- DBI::dbGetQuery(con,
          sprintf("SELECT COUNT(*) AS n FROM %s", cand_tbl))$n
        edge_sample_clause <- build_sample_clause_v2(
          select_mode = edge_select,
          unit_mode   = edge_unit,
          sample_val  = edge_val,
          total_n     = n_candidates
        )

        # Step C: sample the candidate targets, then expand back to
        # (prev, next) edge rows by joining citenet to the prev frontier.
        # The unit being sampled here is (docdb x ctry), i.e. one row per
        # citing docdb — not per edge — so a hot citing docdb consumes a
        # single Top "slot" rather than one per cited prev.
        sql_edges <- glue::glue("
          CREATE OR REPLACE TABLE {tmp_tbl} AS
          WITH sampled_targets AS (
            SELECT * FROM {cand_tbl}
            {edge_sample_clause}
          )
          SELECT
            c.cited_docdb_family_id AS prev_docdb_family_id,
            c.docdb_family_id       AS next_docdb_family_id,
            pf.ctry_code AS src_ctry, pf.lat AS src_lat, pf.lon AS src_lon,
            st.ctry_code AS tgt_ctry, st.lat AS tgt_lat, st.lon AS tgt_lon,
            st.city      AS tgt_city,
            st.toflow_val,
            st.appln_id  AS tgt_appln_id
          FROM citenet c
          JOIN sampled_targets st ON st.docdb_family_id = c.docdb_family_id
          JOIN {prev_tbl} pf      ON pf.docdb_family_id = c.cited_docdb_family_id
          WHERE c.cited_docdb_family_id IN (
            SELECT docdb_family_id FROM {prev_tbl}
          )
        ")

        ok <- tryCatch({
          DBI::dbExecute(con, sql_edges); TRUE
        }, error = function(e) {
          status_msg(paste("Edge query failed:", conditionMessage(e))); FALSE
        })
        try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", cand_tbl)),
            silent = TRUE)
        if (!ok) return(FALSE)

        n_edges <- DBI::dbGetQuery(con,
          sprintf("SELECT COUNT(*) AS n FROM %s", tmp_tbl))$n
        found_stats <- DBI::dbGetQuery(con, sprintf("
          WITH found AS (
            SELECT DISTINCT c.docdb_family_id
            FROM citenet c
            WHERE c.cited_docdb_family_id IN (
              SELECT docdb_family_id FROM %s
            )
          ),
          per_fam AS (
            SELECT p.docdb_family_id,
                   MAX(p.%s) AS toflow_val,
                   MAX(p.pv) AS pv_val
            FROM full_patent_database p
            INNER JOIN found USING (docdb_family_id)
            WHERE p.%s IS NOT NULL
            GROUP BY p.docdb_family_id
          )
          SELECT
            (SELECT COUNT(*)         FROM found)   AS n,
            (SELECT AVG(toflow_val)  FROM per_fam) AS avg_flow,
            (SELECT AVG(pv_val)      FROM per_fam) AS avg_pv
        ", prev_tbl, toflow_col, toflow_col))
        n_found_nodes <- found_stats$n
        # Population (pre-sample) means over the citing docdb universe.
        avg_flow_pop  <- found_stats$avg_flow
        avg_pv_found  <- found_stats$avg_pv

        if (n_edges == 0) {
          status_msg(sprintf(
            "No citations found for generation %d frontier.", g - 1))
          stats_rv(rbind(stats_rv(), data.frame(
            gen             = as.integer(g),
            color           = color_swatch_html(pick_color(g)),
            step            = sprintf("gen %d", g),
            avg_flow_pop    = avg_flow_pop,
            avg_flow_sample = NA_real_,
            avg_pv          = avg_pv_found,
            nodes_found     = n_found_nodes,
            nodes_kept      = 0L,
            stringsAsFactors = FALSE,
            check.names     = FALSE)))
          try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tmp_tbl)),
              silent = TRUE)
          return(FALSE)
        }

        DBI::dbExecute(con, sprintf("
          CREATE OR REPLACE TABLE %s AS
          SELECT DISTINCT
                 next_docdb_family_id AS docdb_family_id,
                 tgt_ctry             AS ctry_code,
                 tgt_lat              AS lat,
                 tgt_lon              AS lon,
                 tgt_city             AS city,
                 toflow_val,
                 tgt_appln_id         AS appln_id
          FROM %s
        ", new_tbl, tmp_tbl))

        edges <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tmp_tbl))
        try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tmp_tbl)),
            silent = TRUE)

        n_kept_nodes <- length(unique(edges$next_docdb_family_id))
        # Sample mean: average flow over only the docdbs that survived the
        # sampling step (this is what the user actually sees rendered on the
        # map). Differs from the population mean above whenever the chosen
        # sampling mode is biased — in particular `Top` deliberately picks
        # the highest-flow rows.
        unique_kept <- edges[!duplicated(edges$next_docdb_family_id), ]
        avg_flow_sample <- if (nrow(unique_kept) == 0) NA_real_ else
          mean(unique_kept$toflow_val, na.rm = TRUE)
        stats_rv(rbind(stats_rv(), data.frame(
          gen             = as.integer(g),
          color           = color_swatch_html(pick_color(g)),
          step            = sprintf("gen %d", g),
          avg_flow_pop    = avg_flow_pop,
          avg_flow_sample = avg_flow_sample,
          avg_pv          = avg_pv_found,
          nodes_found     = n_found_nodes,
          nodes_kept      = n_kept_nodes,
          stringsAsFactors = FALSE,
          check.names     = FALSE)))

        gen_next(g + 1)
        col <- pick_color(g)

        status_msg(sprintf(
          "Generation %d: %s edges; %s citing docdbs kept of %s candidates.",
          g,
          format(n_edges,       big.mark = ","),
          format(n_kept_nodes,  big.mark = ","),
          format(n_found_nodes, big.mark = ",")))

        tgt_pts <- unique(edges[, c("next_docdb_family_id",
                                    "tgt_ctry", "tgt_lat", "tgt_lon",
                                    "tgt_city", "toflow_val", "tgt_appln_id")])
        tgt_link <- espacenet_link(tgt_pts$tgt_appln_id)
        tgt_link_html <- ifelse(is.na(tgt_link), "(no appln id)", tgt_link)

        gen_group <- sprintf("gen_%d", g)
        m <- leaflet::leafletProxy("map") |>
          leaflet::addCircleMarkers(
            lng         = tgt_pts$tgt_lon,
            lat         = tgt_pts$tgt_lat,
            radius      = radius_from_flow(tgt_pts$toflow_val),
            stroke      = FALSE,
            fillOpacity = 0.7,
            fillColor   = col,
            popup       = sprintf(
              "<b>citing%d docdb</b>: %s<br/><b>appln</b>: %s<br/><b>ctry</b>: %s<br/><b>city</b>: %s<br/><b>%s</b>: %s",
              g,
              tgt_pts$next_docdb_family_id,
              tgt_link_html,
              tgt_pts$tgt_ctry,
              ifelse(is.na(tgt_pts$tgt_city), "(capital fallback)", tgt_pts$tgt_city),
              toflow_col,
              format(tgt_pts$toflow_val, big.mark = ",",
                     digits = 4, scientific = FALSE)
            ),
            group       = gen_group
          )
        for (i in seq_len(nrow(edges))) {
          cv <- make_curve(edges$src_lon[i], edges$src_lat[i],
                           edges$tgt_lon[i], edges$tgt_lat[i])
          m <- m |>
            leaflet::addPolylines(
              lng     = cv$lon,
              lat     = cv$lat,
              color   = col,
              weight  = 1,
              opacity = 0.35,
              group   = gen_group
            )
        }
        TRUE
      }

      # Queue of generations still to run for the current "Generate" click.
      # Driving iterations through this reactiveVal + later::later() lets each
      # generation paint to the browser before the next one starts, so the
      # user sees the chain build up incrementally instead of waiting for the
      # whole batch to finish in one frozen blob.
      pending_gens <- shiny::reactiveVal(0L)
      # gen_epoch identifies a single multi-generation batch. Render Map
      # bumps it; the deferred later::later() callback that schedules the
      # next iteration captures the epoch as it was when scheduling and
      # bails out if it no longer matches — so a click on Initiate
      # Innovation cancels any pending iterations from the previous batch.
      # (The currently running do_one_generation cannot be aborted because
      # R is single-threaded; the in-flight iteration finishes, but no
      # further iterations are kicked off.)
      gen_epoch <- shiny::reactiveVal(0L)
      # Inputs needed by each iteration are captured when "Generate" is
      # pressed, so changes to the sliders mid-batch don't surprise the
      # user (the Render Map seed locked the toflow already).
      # Inputs needed by each iteration are captured when "Generate" is
      # pressed, so changes to the toggles mid-batch don't surprise the
      # user. The Render Map seed already locks the toflow.
      pending_select    <- shiny::reactiveVal(NA_character_)
      pending_unit      <- shiny::reactiveVal(NA_character_)
      pending_val       <- shiny::reactiveVal(NA_real_)
      pending_flow      <- shiny::reactiveVal(NA_character_)
      pending_max_nodes <- shiny::reactiveVal(Inf)

      shiny::observeEvent(input$next_step, ignoreInit = TRUE, {
        if (is.null(gen_next())) {
          status_msg("Run 'Render Map' first to seed generation 0.")
          return()
        }
        edge_select <- input$edge_select_mode %||% "Random"
        edge_unit   <- input$edge_unit_mode   %||% "Percent"
        edge_val    <- suppressWarnings(as.numeric(input$edge_sample_val))
        if (identical(edge_unit, "Percent")) {
          if (!is.finite(edge_val) || edge_val < 0 || edge_val > 100) {
            status_msg("Edge sampling rate must be between 0 and 100.")
            return()
          }
        } else {
          if (!is.finite(edge_val) || edge_val < 0) {
            status_msg("Edge sample size must be a non-negative integer.")
            return()
          }
        }
        n_steps <- suppressWarnings(as.integer(input$add_generations))
        if (is.na(n_steps) || n_steps < 1) n_steps <- 1L

        # Snapshot the safety cap once; later edits don't disturb the
        # in-flight batch. NA / non-positive disables the check.
        mn_raw <- suppressWarnings(as.numeric(input$max_iter_nodes))
        mn     <- if (is.finite(mn_raw) && mn_raw > 0) mn_raw else Inf

        pending_select(edge_select)
        pending_unit(edge_unit)
        pending_val(edge_val)
        pending_flow(input$toflow)
        pending_max_nodes(mn)
        pending_gens(n_steps)
      })

      shiny::observeEvent(pending_gens(), ignoreInit = TRUE, {
        rem <- pending_gens()
        if (rem <= 0) return()
        # Snapshot the epoch we're working under; if Render Map fires
        # mid-batch it will bump gen_epoch and our scheduled callback will
        # see the mismatch and exit.
        my_epoch <- shiny::isolate(gen_epoch())
        ok <- do_one_generation(pending_select(), pending_unit(),
                                pending_val(),    pending_flow(),
                                max_nodes = pending_max_nodes())
        if (!ok) {
          pending_gens(0L)
          return()
        }
        if (rem > 1) {
          # Defer the decrement so the current reactive flush completes —
          # the leaflet markers / polylines / stats row added above paint
          # to the browser before the next iteration starts.
          later::later(function() {
            if (!isTRUE(shiny::isolate(gen_epoch()) == my_epoch)) return()
            shiny::isolate(pending_gens(rem - 1L))
          }, delay = 0.15)
        } else {
          pending_gens(0L)
        }
      })

      # URL-driven auto-init + auto-Generate: when ?higglobe_run=1 (or
      # ?higglobe_gen=N or ?step=N) is on the URL, server.R has stashed
      # the flag in session$userData$restore_params. The module fires the
      # actual button clicks itself, AFTER all its observers are
      # registered, so the clicks never race lazy-init.
      auto_init_armed <- shiny::reactiveVal(FALSE)
      auto_gen_armed  <- shiny::reactiveVal(FALSE)
      shiny::observe({
        rp <- session$userData$restore_params %||% list()
        # auto-init if any of run / gen / step indicates we should
        run_flag <- isTRUE(tolower(as.character(rp$higglobe_run %||% "")) %in%
                           c("1", "true", "yes", "on"))
        raw_gen  <- rp$higglobe_gen
        n_gen <- if (!is.null(raw_gen) && nzchar(raw_gen))
                   suppressWarnings(as.integer(raw_gen)) else NA_integer_
        gen_flag <- length(n_gen) == 1L && !is.na(n_gen) && n_gen > 0
        if (run_flag || gen_flag) auto_init_armed(TRUE)
        if (gen_flag)             auto_gen_armed(TRUE)
        # Schedule the Initiate Innovation click DIRECTLY here, not via
        # an intervening observeEvent. The observeEvent route was
        # racing the same reactive flush in which auto_init_armed first
        # transitioned to TRUE — with the eventExpr already truthy on
        # initial evaluation, the change-detection logic skipped it and
        # the body never ran. Doing the click inline avoids that
        # entirely. The flag is still set for the status-message
        # display below.
        if (run_flag || gen_flag) {
          auto_click("render_map", delay_ms = 1200L,
                     label = "Initiate Innovation")
        }
      })
      # Helper: trigger the Shiny input value bump for a button so the
      # observeEvent(input$<id>, ...) listener fires — same wire signal a
      # real user click would produce. Goes through Shiny.setInputValue
      # with priority:'event', which works whether the button is a plain
      # actionButton or a bslib::input_task_button (whose ExtendedTask
      # wrapper sometimes ignores synthetic DOM clicks). The status_msg
      # update gives the user a visible breadcrumb so we can see whether
      # the auto-trigger actually fired even if the click somehow misses.
      auto_click <- function(short_id, delay_ms = 0L,
                             label = short_id) {
        full_id  <- session$ns(short_id)
        cur_raw  <- shiny::isolate(input[[short_id]])
        cur_val  <- if (is.null(cur_raw)) 0L else as.integer(cur_raw)
        next_val <- cur_val + 1L
        status_msg(sprintf(
          "Auto-%s scheduled (in %d ms)...", label, as.integer(delay_ms)))
        shinyjs::runjs(sprintf(
          paste0(
            "(function(){",
            "var id='%s';",
            "function fire(){",
            "  if(window.Shiny && Shiny.setInputValue){",
            "    Shiny.setInputValue(id, %d, {priority:'event'});",
            "    var b=document.getElementById(id);",
            "    if(b){try{b.click();}catch(e){}}",
            "  } else { setTimeout(fire, 100); }",
            "}",
            "setTimeout(fire, %d);",
            "})();"
          ),
          full_id, next_val, as.integer(delay_ms)
        ))
      }

      # (auto-init click is now scheduled inline in the observe() above
      # — no observeEvent indirection.)

      # Fire the Generate click once gen 0 is actually seeded (gen_next
      # flips from NULL to 1L). `auto_gen_armed` is set above, so a user
      # who later re-runs Initiate Innovation manually doesn't get a
      # surprise auto-Generate.
      shiny::observe({
        if (!isTRUE(auto_gen_armed())) return()
        g <- gen_next()
        if (is.null(g) || g != 1L) return()
        auto_gen_armed(FALSE)
        auto_click("next_step", delay_ms = 400L, label = "Generate")
      })

      # Spinner + Stop button: both follow pending_gens > 0. While a batch
      # is in flight, show the spinner overlay AND surface the emergency
      # stop button. Toggling with shinyjs::show / hide is cheap and works
      # regardless of the bslib task-button's per-iteration busy reset.
      shiny::observe({
        rem <- pending_gens()
        if (is.null(rem) || rem <= 0) {
          shinyjs::hide(id = "next_step_spinner")
          shinyjs::hide(id = "stop_generation_wrapper")
        } else {
          shinyjs::show(id = "next_step_spinner")
          shinyjs::show(id = "stop_generation_wrapper")
        }
      })

      # Emergency stop: zero pending_gens so no further iterations get
      # scheduled, and bump gen_epoch so any later() callback already
      # queued bails out at the epoch check. The *currently executing*
      # do_one_generation cannot be aborted mid-call (R is single-threaded
      # and the leaflet draws happen after the R-side call returns), so
      # the iteration drawing right now will finish — but the chain stops
      # there and the user can change inputs without another wave landing.
      shiny::observeEvent(input$stop_generation, ignoreInit = TRUE, {
        if (shiny::isolate(pending_gens()) <= 0) return()
        rem_now <- shiny::isolate(pending_gens())
        gen_epoch(shiny::isolate(gen_epoch()) + 1L)
        pending_gens(0L)
        status_msg(sprintf(
          "Generation stopped (%d iteration%s skipped). Existing chain preserved — adjust inputs and Generate again to continue.",
          rem_now, if (rem_now == 1L) "" else "s"
        ))
      })

      # Reflect the current generation count in the URL as `step=N`. We
      # use history.replaceState() (via raw JS) so we can patch a single
      # query param without disturbing whatever else Shiny's native
      # bookmarking has put into the URL. Bookmarking the URL with
      # step=N > 0 makes the page replay the init + N generations on
      # reopen via the URL handler in server.R.
      #
      # Crucial guard: if the user opened the page WITH a non-zero
      # `step` in the URL, we must NOT immediately overwrite it with
      # step=0 just because gen_next is still NULL during the auto-init
      # boot phase — that would erase the replay-target before the
      # auto-Generate has a chance to read it on a refresh.
      shiny::observe({
        g <- gen_next()
        if (is.null(g)) {
          rp <- session$userData$restore_params %||% list()
          incoming <- suppressWarnings(as.integer(rp$step %||% rp$higglobe_gen))
          if (length(incoming) == 1L && !is.na(incoming) && incoming > 0L)
            return()  # honour the URL's step until the module actually seeds
        }
        step_val <- if (is.null(g)) 0L else max(0L, as.integer(g) - 1L)
        shinyjs::runjs(sprintf(
          "(function(){var u=new URL(window.location);u.searchParams.set('step','%d');window.history.replaceState({},'',u);})()",
          step_val
        ))
      })

    }
  )
}
