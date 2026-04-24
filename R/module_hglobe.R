#' HGlobe module Sidebar
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
        shiny::checkboxInput(
          inputId = ns("granted_only"),
          label   = "Granted families only",
          value   = TRUE
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
          selected = "is_global",
          multiple = FALSE,
          width    = "400px"
        )
      )
    ),

    shiny::div(
      shiny::h5("SAMPLING", style = "font-weight: 600; margin-bottom: 10px;"),
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
      "Render Map",
      label_busy = "Rendering...",
      class = "btn-primary",
      width = "100%"
    ),

    bslib::input_task_button(
      ns("next_step"),
      "Next step",
      label_busy = "Rendering edges...",
      class = "btn-secondary",
      width = "100%"
    )
  )
}

#' HGlobe module UI
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
        style = "margin-bottom: 10px; color: #666; font-size: 0.9em;",
        shiny::textOutput(ns("status"))
      ),
      leaflet::leafletOutput(ns("map"), height = "650px")
    )
  )
}

#' HGlobe module Server
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

      # Bootstrap an empty leaflet base layer.
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
          leaflet::addTiles() |>
          leaflet::setView(10, 20, zoom = 2)
      })

      status_msg <- shiny::reactiveVal("")
      output$status <- shiny::renderText(status_msg())

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
      # are visually distinct on the map.
      gen_colors <- c("#2780e3", "#d9534f", "#5cb85c", "#f0ad4e",
                      "#9467bd", "#8c564b", "#17becf", "#e377c2")
      pick_color <- function(g) gen_colors[((g - 1) %% length(gen_colors)) + 1]

      drop_session_tables <- function() {
        tabs <- tryCatch(DBI::dbListTables(con),
                         error = function(e) character())
        pat  <- sprintf("^hglobe_(gen[0-9]+|edges_tmp)_%s$", tok)
        for (t in tabs[grepl(pat, tabs)]) {
          try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", t)),
              silent = TRUE)
        }
      }

      shiny::onSessionEnded(function() drop_session_tables())

      shiny::observeEvent(input$render_map, {
        shiny::req(input$toflow, input$country, input$techs)
        pct <- suppressWarnings(as.numeric(input$sampling_rate))
        if (!is.finite(pct) || pct < 0 || pct > 100) {
          status_msg("Sampling rate must be between 0 and 100.")
          return()
        }

        selected_countries <- expand_country_selection(input$country)
        no_firm_filter     <- "No firm filter" %in% input$firm || length(input$firm) == 0
        selected_firms     <- expand_firm_selection(setdiff(input$firm, "No firm filter"))

        country_sql    <- paste0("'", gsub("'", "''", selected_countries),
                                 "'", collapse = ", ")
        firm_clause    <- build_firm_clause_v2(selected_firms,
                                               no_filter = no_firm_filter)
        tech_bool      <- build_tech_bool_v2(input$techs)
        granted_clause <- build_granted_clause_v2(isTRUE(input$granted_only))

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

        passing_sql <- glue::glue("
          passing AS (
            SELECT DISTINCT p.docdb_family_id, p.ctry_code
            FROM full_patent_database p
            {if (has_tech) 'INNER JOIN filtered_tech ft ON p.docdb_family_id = ft.docdb_family_id' else ''}
            {if (has_firm) 'INNER JOIN filtered_firm ff ON p.docdb_family_id = ff.docdb_family_id' else ''}
            WHERE p.ctry_code IN ({country_sql})
              AND p.{input$toflow} IS NOT NULL
              {granted_clause}
          )")
        ctes <- c(ctes, passing_sql)

        # Bernoulli sample over the filtered set — fast and unbiased.
        sampled_sql <- glue::glue("
          sampled AS (
            SELECT * FROM passing
            USING SAMPLE {pct} PERCENT (bernoulli)
          )")
        ctes <- c(ctes, sampled_sql)

        sql <- paste0(
          "WITH ", paste(ctes, collapse = ",\n"), "\n",
          "SELECT
             s.docdb_family_id,
             s.ctry_code,
             c.city,
             c.lat,
             c.lon,
             c.geocode_missing
           FROM sampled s
           INNER JOIN countrymap c
             ON c.docdb_family_id = s.docdb_family_id
            AND c.ctry_code       = s.ctry_code
           WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL"
        )

        dat <- tryCatch(
          DBI::dbGetQuery(con, sql),
          error = function(e) {
            status_msg(paste("Query failed:", conditionMessage(e)))
            NULL
          }
        )
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
        seed <- data.frame(
          docdb_family_id = dat$docdb_family_id,
          ctry_code       = dat$ctry_code,
          city            = dat$city,
          lat             = dat$lat,
          lon             = dat$lon,
          stringsAsFactors = FALSE
        )
        DBI::dbWriteTable(con, frontier_tbl(0), seed, overwrite = TRUE)
        gen_next(1)

        status_msg(sprintf(
          "Seeded generation 0: %s sampled (family x country) points. Click 'Next step' to expand citations.",
          format(nrow(dat), big.mark = ",")))

        # Tiny jitter so overlapping points are individually visible; the
        # deterministic lat/lon coming from countrymap would otherwise stack
        # every city onto one pixel.
        set.seed(42)
        jit <- 0.25
        dat$lon_j <- dat$lon + stats::runif(nrow(dat), -jit, jit)
        dat$lat_j <- dat$lat + stats::runif(nrow(dat), -jit, jit)

        popup_txt <- sprintf(
          "<b>docdb</b>: %s<br/><b>ctry</b>: %s<br/><b>city</b>: %s%s",
          dat$docdb_family_id, dat$ctry_code,
          ifelse(is.na(dat$city), "(capital fallback)", dat$city),
          ifelse(isTRUE(dat$geocode_missing), " <i>(geocode missing)</i>", "")
        )

        leaflet::leafletProxy("map") |>
          leaflet::clearMarkerClusters() |>
          leaflet::clearMarkers() |>
          leaflet::clearShapes() |>
          leaflet::addCircleMarkers(
            lng          = dat$lon_j,
            lat          = dat$lat_j,
            radius       = 3,
            stroke       = FALSE,
            fillOpacity  = 0.6,
            fillColor    = ifelse(isTRUE(dat$geocode_missing), "#bbbbbb", "#2780e3"),
            popup        = popup_txt,
            clusterOptions = leaflet::markerClusterOptions(
              showCoverageOnHover = FALSE,
              spiderfyOnMaxZoom   = TRUE,
              maxClusterRadius    = 40
            )
          )
      })

      # Quadratic-bezier curve between two points — same primitive Globe uses
      # to draw value-flow arcs. `bend` controls the peak height relative to
      # the source->target distance.
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
      shiny::observeEvent(input$next_step, {
        g <- gen_next()
        if (is.null(g)) {
          status_msg("Run 'Render Map' first to seed generation 0.")
          return()
        }
        pct_edge <- suppressWarnings(as.numeric(input$edge_sampling_rate))
        if (!is.finite(pct_edge) || pct_edge < 0 || pct_edge > 100) {
          status_msg("Edge sampling rate must be between 0 and 100.")
          return()
        }
        prev_tbl <- frontier_tbl(g - 1)
        new_tbl  <- frontier_tbl(g)
        tmp_tbl  <- sprintf("hglobe_edges_tmp_%s", tok)

        # Materialise the edge set (with src + tgt coords) in a temp table,
        # then derive the new frontier (distinct citing docdbs) from it.
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
          ranked AS (
            SELECT *,
                   ROW_NUMBER() OVER (
                     PARTITION BY prev_docdb_family_id
                     ORDER BY HASH(next_docdb_family_id)
                   ) AS rn,
                   COUNT(*) OVER (PARTITION BY prev_docdb_family_id) AS cnt
            FROM citations
          ),
          sampled_edges AS (
            SELECT prev_docdb_family_id, next_docdb_family_id
            FROM ranked
            WHERE rn <= GREATEST(1, CEIL(cnt * {pct_edge} / 100.0))
          ),
          target_geo AS (
            SELECT DISTINCT ON (docdb_family_id)
                   docdb_family_id, ctry_code, city, lat, lon
            FROM countrymap
            WHERE lat IS NOT NULL AND lon IS NOT NULL
            ORDER BY docdb_family_id, ctry_code
          )
          SELECT
            se.prev_docdb_family_id,
            se.next_docdb_family_id,
            pf.ctry_code AS src_ctry, pf.lat AS src_lat, pf.lon AS src_lon,
            tg.ctry_code AS tgt_ctry, tg.lat AS tgt_lat, tg.lon AS tgt_lon,
            tg.city      AS tgt_city
          FROM sampled_edges se
          JOIN {prev_tbl} pf  ON pf.docdb_family_id = se.prev_docdb_family_id
          JOIN target_geo tg  ON tg.docdb_family_id = se.next_docdb_family_id
        ")
        ok <- tryCatch({
          DBI::dbExecute(con, sql_edges); TRUE
        }, error = function(e) {
          status_msg(paste("Edge query failed:", conditionMessage(e))); FALSE
        })
        if (!ok) return()

        n_edges <- DBI::dbGetQuery(con,
          sprintf("SELECT COUNT(*) AS n FROM %s", tmp_tbl))$n
        if (n_edges == 0) {
          status_msg(sprintf(
            "No citations found for generation %d frontier.", g - 1))
          try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tmp_tbl)),
              silent = TRUE)
          return()
        }

        # Persist the distinct next-generation docdbs as the new frontier.
        DBI::dbExecute(con, sprintf("
          CREATE OR REPLACE TABLE %s AS
          SELECT DISTINCT
                 next_docdb_family_id AS docdb_family_id,
                 tgt_ctry             AS ctry_code,
                 tgt_lat              AS lat,
                 tgt_lon              AS lon,
                 tgt_city             AS city
          FROM %s
        ", new_tbl, tmp_tbl))

        edges <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tmp_tbl))
        try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tmp_tbl)),
            silent = TRUE)

        gen_next(g + 1)
        col <- pick_color(g)

        status_msg(sprintf(
          "Generation %d: %s edges from %s gen-%d docdbs to %s gen-%d docdbs.",
          g,
          format(n_edges,                                            big.mark = ","),
          format(length(unique(edges$prev_docdb_family_id)),         big.mark = ","),
          g - 1,
          format(length(unique(edges$next_docdb_family_id)),         big.mark = ","),
          g))

        # Target-side markers (one per new-frontier docdb) — plot on top of
        # prior generations' markers; don't clear them.
        tgt_pts <- unique(edges[, c("next_docdb_family_id",
                                    "tgt_ctry", "tgt_lat", "tgt_lon",
                                    "tgt_city")])

        m <- leaflet::leafletProxy("map") |>
          leaflet::addCircleMarkers(
            lng         = tgt_pts$tgt_lon,
            lat         = tgt_pts$tgt_lat,
            radius      = 2,
            stroke      = FALSE,
            fillOpacity = 0.7,
            fillColor   = col,
            popup       = sprintf(
              "<b>citing%d docdb</b>: %s<br/><b>ctry</b>: %s<br/><b>city</b>: %s",
              g,
              tgt_pts$next_docdb_family_id,
              tgt_pts$tgt_ctry,
              ifelse(is.na(tgt_pts$tgt_city), "(capital fallback)", tgt_pts$tgt_city)
            )
          )

        # Curved polyline per edge — same bezier primitive Globe uses.
        for (i in seq_len(nrow(edges))) {
          cv <- make_curve(edges$src_lon[i], edges$src_lat[i],
                           edges$tgt_lon[i], edges$tgt_lat[i])
          m <- m |>
            leaflet::addPolylines(
              lng     = cv$lon,
              lat     = cv$lat,
              color   = col,
              weight  = 1,
              opacity = 0.35
            )
        }
      })

    }
  )
}
