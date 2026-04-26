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
          selected = "Green Technology",
          multiple = TRUE,
          options = list(placeholder = 'Choose technologies...')
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::div(
          style = "display:flex; gap:18px; flex-wrap:wrap;",
          shiny::checkboxInput(
            inputId = ns("granted_only"),
            label   = "Granted families only",
            value   = TRUE
          ),
          shiny::checkboxInput(
            inputId = ns("multifam_only"),
            label   = "Multi-application families only",
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

    shiny::div(
      shiny::h5("SAMPLING", style = "font-weight: 600; margin-bottom: 10px;"),
      shiny::div(
        class = "side_input",
        shiny::radioButtons(
          inputId = ns("sampling_mode"),
          label   = "Sampling mode",
          choices = c("Percent", "Number", "Top"),
          selected = "Percent",
          inline   = TRUE
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          inputId = ns("sampling_rate"),
          label   = "Sampling rate [%]",
          value   = 1,
          min     = 0,
          max     = 100,
          step    = 0.1
        )
      ),
      shiny::div(
        class = "side_input",
        shiny::numericInput(
          inputId = ns("edge_sampling_rate"),
          label   = "Edge sampling rate [%]",
          value   = 1,
          min     = 0,
          max     = 100,
          step    = 0.1
        )
      )
    ),

    bslib::input_task_button(
      ns("render_map"),
      "Initiate Innovation",
      label_busy = "Initiating...",
      class = "btn-primary",
      width = "100%"
    ),

    # The numericInput is wrapped in a <div class="form-group"> with a
    # default 1rem bottom margin; a flex row with align-items:flex-end then
    # leaves the button hovering above the input's bottom edge. Inline CSS
    # zeroes that margin only inside this container.
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
        style = "flex: 0 0 120px;",
        shiny::numericInput(
          inputId = ns("add_generations"),
          label   = "Add Generations",
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
        # Persistent spinner overlay shown for the *whole* multi-generation
        # batch, not just one iteration. The bslib task button reverts to
        # its idle look between iterations because each iteration is a
        # separate observer firing; this overlay keeps the user aware that
        # the server is still working until pending_gens hits 0.
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
          "margin-top: 14px; color: #555; font-size: 0.9em; ",
          "line-height: 1.45; max-width: 60em;"),
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

      # Re-label the Step 1 sample-size input when the sampling mode toggles.
      # Bounds and step also change so percent stays clamped to 0-100 with a
      # 0.1 step, while integer counts get a step of 1 with no upper bound.
      shiny::observeEvent(input$sampling_mode, {
        if (identical(input$sampling_mode, "Number") ||
            identical(input$sampling_mode, "Top")) {
          shiny::updateNumericInput(session, "sampling_rate",
            label = "Sample size [#]",
            min = 0, max = NA, step = 1)
        } else {
          shiny::updateNumericInput(session, "sampling_rate",
            label = "Sampling rate [%]",
            min = 0, max = 100, step = 0.1)
        }
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
          shiny::tags$tr(
            shiny::tags$td(
              shiny::checkboxInput(ns(cb_id), label = NULL, value = val,
                                   width = "30px")
            ),
            shiny::tags$td(shiny::HTML(df$color[i])),
            shiny::tags$td(df$step[i]),
            shiny::tags$td(if (is.na(df$avg_flow[i])) "—"
                           else format(df$avg_flow[i],
                                       big.mark = ",", digits = 4,
                                       scientific = FALSE)),
            shiny::tags$td(if (is.null(df$avg_pv[i]) || is.na(df$avg_pv[i])) "—"
                           else format(df$avg_pv[i],
                                       big.mark = ",", digits = 4,
                                       scientific = FALSE)),
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
            shiny::tags$th("Avg flow"),
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

      # Map a vector of flow values to marker radii on a log(1 + v) scale,
      # normalised so the largest value gets the largest radius within this
      # draw. NA or non-positive values fall back to the minimum radius.
      # Defaults sized for clickability at max zoom — at radius 5 px the
      # circle is a comfortable touch/click target on most displays.
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
        pat  <- sprintf("^hglobe_(gen[0-9]+|edges_tmp|passing_tmp)_%s$", tok)
        for (t in tabs[grepl(pat, tabs)]) {
          try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", t)),
              silent = TRUE)
        }
      }

      shiny::onSessionEnded(function() drop_session_tables())

      shiny::observeEvent(input$render_map, ignoreInit = TRUE, {
        shiny::req(input$toflow, input$country, input$techs)
        sample_mode <- input$sampling_mode %||% "Percent"
        sample_val  <- suppressWarnings(as.numeric(input$sampling_rate))
        if (identical(sample_mode, "Number")) {
          if (!is.finite(sample_val) || sample_val < 0) {
            status_msg("Sample size must be a non-negative integer.")
            return()
          }
          sample_clause <- sprintf("USING SAMPLE %d ROWS (reservoir)",
                                   as.integer(sample_val))
        } else if (identical(sample_mode, "Top")) {
          # Deterministic top-N by the chosen flow value — same logic the
          # Country Explorer's Value-flows-by-Technology bar chart uses to
          # build "Top N patent IDs" lists. The `toflow_val` column on
          # `passing_tbl` is the per-(docdb, ctry) MAX of input$toflow,
          # so an ORDER BY ... DESC LIMIT N gives the highest-flow
          # families. NULLS LAST keeps any unscored rows out of the top.
          if (!is.finite(sample_val) || sample_val < 0) {
            status_msg("Sample size must be a non-negative integer.")
            return()
          }
          sample_clause <- sprintf("ORDER BY toflow_val DESC NULLS LAST LIMIT %d",
                                   as.integer(sample_val))
        } else {
          if (!is.finite(sample_val) || sample_val < 0 || sample_val > 100) {
            status_msg("Sampling rate must be between 0 and 100.")
            return()
          }
          sample_clause <- sprintf("USING SAMPLE %s PERCENT (bernoulli)",
                                   format(sample_val, nsmall = 4))
        }

        selected_countries <- expand_country_selection(input$country)
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))

        country_sql    <- paste0("'", gsub("'", "''", selected_countries),
                                 "'", collapse = ", ")
        firm_clause    <- build_firm_clause_v2(selected_firms,
                                               no_filter = no_firm_filter)
        tech_bool      <- build_tech_bool_v2(input$techs)
        granted_clause  <- build_granted_clause_v2(isTRUE(input$granted_only))
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
        passing_tbl <- sprintf("hglobe_passing_tmp_%s", tok)
        toflow_col  <- input$toflow
        passing_sql <- glue::glue("
          passing AS (
            SELECT p.docdb_family_id,
                   p.ctry_code,
                   MAX(p.{toflow_col}) AS toflow_val,
                   MAX(p.pv)             AS pv_val,
                   ANY_VALUE(p.appln_id) AS appln_id
            FROM full_patent_database p
            {if (has_tech) 'INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id' else ''}
            {if (has_firm) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
            WHERE p.ctry_code IN ({country_sql})
              AND p.{toflow_col} IS NOT NULL
              {granted_clause}
              {multifam_clause}
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
        # Mean of the chosen flow column AND the patent value (pv) over the
        # nodes FOUND (pre-sample). One round-trip carries both averages.
        agg_found <- DBI::dbGetQuery(con, sprintf(
          "SELECT AVG(toflow_val) AS avg_flow, AVG(pv_val) AS avg_pv FROM %s",
          passing_tbl))
        avg_flow_found <- agg_found$avg_flow
        avg_pv_found   <- agg_found$avg_pv

        # Sample over the filtered set, then join countrymap for geo. The
        # SAMPLE clause goes inside a subquery because DuckDB's parser won't
        # accept USING SAMPLE between FROM and an explicit JOIN, and sampling
        # BEFORE the join ensures each (docdb, ctry) gets an independent
        # inclusion decision regardless of how many rows the countrymap join
        # produces downstream.
        #   sample_clause is one of:
        #     "USING SAMPLE <pct> PERCENT (bernoulli)"           (Percent)
        #     "USING SAMPLE <n> ROWS (reservoir)"                (Number)
        #     "ORDER BY toflow_val DESC NULLS LAST LIMIT <n>"    (Top)
        sql <- sprintf("
          SELECT p.docdb_family_id, p.ctry_code, p.toflow_val, p.appln_id,
                 c.city, c.lat, c.lon, c.geocode_missing
          FROM (
            SELECT * FROM %s %s
          ) p
          INNER JOIN countrymap c
            ON c.docdb_family_id = p.docdb_family_id
           AND c.ctry_code       = p.ctry_code
          WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
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
          avg_flow        = avg_flow_found,
          avg_pv          = avg_pv_found,
          nodes_found     = n_found,
          nodes_kept      = nrow(dat),
          stringsAsFactors = FALSE,
          check.names     = FALSE
        ))

        status_msg(sprintf(
          "Seeded generation 0: %s of %s rows kept after sampling. Click 'Generate' to expand citations.",
          format(nrow(dat), big.mark = ","),
          format(n_found,   big.mark = ",")))

        # Tiny jitter so overlapping points are individually visible; the
        # deterministic lat/lon coming from countrymap would otherwise stack
        # every city onto one pixel.
        set.seed(42)
        jit <- 0.25
        dat$lon_j <- dat$lon + stats::runif(nrow(dat), -jit, jit)
        dat$lat_j <- dat$lat + stats::runif(nrow(dat), -jit, jit)

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
            lng          = dat$lon_j,
            lat          = dat$lat_j,
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
      # to keep the semantics explicit at each level. Within every
      # prev-generation docdb we keep `edge_sampling_rate` percent of its
      # citing rows (at least one), resolve coordinates via countrymap, and
      # persist the resulting distinct next-generation docdbs as the new
      # frontier table for the next click.
      # Per-iteration worker: advances exactly ONE generation. Returns TRUE
      # on success, FALSE if the SQL failed or there were zero citations
      # (the latter still appends a stats row before returning). Defined
      # here so both the immediate first-iteration path and the deferred
      # later::later() path can reuse it.
      do_one_generation <- function(pct_edge, toflow_col) {
        g        <- gen_next()
        prev_tbl <- frontier_tbl(g - 1)
        new_tbl  <- frontier_tbl(g)
        tmp_tbl  <- sprintf("hglobe_edges_tmp_%s", tok)

        sql_edges <- glue::glue("
          CREATE OR REPLACE TABLE {tmp_tbl} AS
          WITH citations AS (
            SELECT c.cited_docdb_family_id AS prev_docdb_family_id,
                   c.docdb_family_id       AS next_docdb_family_id
            FROM citenet c
            WHERE c.cited_docdb_family_id IN (
              SELECT docdb_family_id FROM {prev_tbl}
            )
          ),
          sampled_edges AS (
            SELECT prev_docdb_family_id, next_docdb_family_id
            FROM citations
            USING SAMPLE {format(pct_edge, nsmall = 4)} PERCENT (bernoulli)
          ),
          flow_per_fam AS (
            SELECT docdb_family_id, ctry_code,
                   MAX({toflow_col})    AS toflow_val,
                   ANY_VALUE(appln_id)  AS appln_id
            FROM full_patent_database
            WHERE {toflow_col} IS NOT NULL
            GROUP BY docdb_family_id, ctry_code
          ),
          target_geo AS (
            SELECT DISTINCT ON (c.docdb_family_id)
                   c.docdb_family_id, c.ctry_code, c.city, c.lat, c.lon,
                   fpf.toflow_val, fpf.appln_id
            FROM countrymap c
            JOIN flow_per_fam fpf
              ON fpf.docdb_family_id = c.docdb_family_id
             AND fpf.ctry_code       = c.ctry_code
            WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
            ORDER BY c.docdb_family_id, c.ctry_code
          )
          SELECT
            se.prev_docdb_family_id,
            se.next_docdb_family_id,
            pf.ctry_code AS src_ctry, pf.lat AS src_lat, pf.lon AS src_lon,
            tg.ctry_code AS tgt_ctry, tg.lat AS tgt_lat, tg.lon AS tgt_lon,
            tg.city      AS tgt_city,
            tg.toflow_val,
            tg.appln_id  AS tgt_appln_id
          FROM sampled_edges se
          JOIN {prev_tbl} pf   ON pf.docdb_family_id = se.prev_docdb_family_id
          JOIN target_geo tg   ON tg.docdb_family_id = se.next_docdb_family_id
        ")

        ok <- tryCatch({
          DBI::dbExecute(con, sql_edges); TRUE
        }, error = function(e) {
          status_msg(paste("Edge query failed:", conditionMessage(e))); FALSE
        })
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
        n_found_nodes  <- found_stats$n
        avg_flow_found <- found_stats$avg_flow
        avg_pv_found   <- found_stats$avg_pv

        if (n_edges == 0) {
          status_msg(sprintf(
            "No citations found for generation %d frontier.", g - 1))
          stats_rv(rbind(stats_rv(), data.frame(
            gen             = as.integer(g),
            color           = color_swatch_html(pick_color(g)),
            step            = sprintf("gen %d", g),
            avg_flow        = avg_flow_found,
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
        stats_rv(rbind(stats_rv(), data.frame(
          gen             = as.integer(g),
          color           = color_swatch_html(pick_color(g)),
          step            = sprintf("gen %d", g),
          avg_flow        = avg_flow_found,
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
      pending_pct  <- shiny::reactiveVal(NA_real_)
      pending_flow <- shiny::reactiveVal(NA_character_)

      shiny::observeEvent(input$next_step, ignoreInit = TRUE, {
        if (is.null(gen_next())) {
          status_msg("Run 'Render Map' first to seed generation 0.")
          return()
        }
        pct_edge <- suppressWarnings(as.numeric(input$edge_sampling_rate))
        if (!is.finite(pct_edge) || pct_edge < 0 || pct_edge > 100) {
          status_msg("Edge sampling rate must be between 0 and 100.")
          return()
        }
        n_steps <- suppressWarnings(as.integer(input$add_generations))
        if (is.na(n_steps) || n_steps < 1) n_steps <- 1L

        pending_pct(pct_edge)
        pending_flow(input$toflow)
        pending_gens(n_steps)
      })

      shiny::observeEvent(pending_gens(), ignoreInit = TRUE, {
        rem <- pending_gens()
        if (rem <= 0) return()
        # Snapshot the epoch we're working under; if Render Map fires
        # mid-batch it will bump gen_epoch and our scheduled callback will
        # see the mismatch and exit.
        my_epoch <- shiny::isolate(gen_epoch())
        ok <- do_one_generation(pending_pct(), pending_flow())
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

      # Spinner: visible whenever a multi-generation batch is mid-flight,
      # i.e. pending_gens > 0. Toggling this with shinyjs::show / hide is
      # cheap and works regardless of the bslib task-button's per-iteration
      # busy reset.
      shiny::observe({
        rem <- pending_gens()
        if (is.null(rem) || rem <= 0) {
          shinyjs::hide(id = "next_step_spinner")
        } else {
          shinyjs::show(id = "next_step_spinner")
        }
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
