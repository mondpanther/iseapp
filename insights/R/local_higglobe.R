# insights/R/local_higglobe.R
#
# Build a HiGGlobe-style citation-network sample (gen 0 + gen 1..N edges)
# directly from the on-disk parquets in inst/extdata/, and render it as an
# interactive {leaflet} widget. Pure local R / DuckDB — no chromote, no
# Shiny, no HTTP, no waiting for a heavy deployed app to render.
#
# Layout of the parquets this depends on (all in inst/extdata/):
#   country_lookup.parquet : ctry_code + boolean group columns
#                            (is_lmic, is_lmic_excl_china, is_hic, is_eu, ...)
#   patents_x_tech.parquet : (docdb_family_id, technology)
#   tech_lookup.parquet    : (technology, tech_group)
#   countrymap.parquet     : (docdb_family_id, ctry_code, city, lat, lon)
#   citenet.parquet        : (docdb_family_id, cited_docdb_family_id) edges
#                            — citing  -> cited
#   patent_database.parquet: per-(docdb, ctry) spillover values + cost/alpha/pv

#' Translate a HiGGlobe country selection (the multi-select selectize value
#' the app uses) into a set of ISO2 codes by joining country_lookup.
#'
#' @param countries character: any of the predefined group labels
#'   ("LMICs", "LMICs (excl. China)", "High income countries", ...) and/or
#'   individual ISO2 codes ("CN", "US", ...).
#' @param data_dir directory holding the parquets.
.expand_countries <- function(countries, data_dir = "inst/extdata") {
  cl <- arrow::read_parquet(file.path(data_dir, "country_lookup.parquet"))
  iso_set <- cl$ctry_code
  out <- character(0)
  for (c in countries) {
    if (c == "All countries")               out <- c(out, cl$ctry_code)
    else if (c == "LMICs")                  out <- c(out, cl$ctry_code[cl$is_lmic])
    else if (c == "LMICs (excl. China)")    out <- c(out, cl$ctry_code[cl$is_lmic_excl_china])
    else if (c == "High income countries")  out <- c(out, cl$ctry_code[cl$is_hic])
    else if (c == "EU")                     out <- c(out, cl$ctry_code[cl$is_eu])
    else if (c %in% iso_set)                out <- c(out, c)  # already ISO2
    else if (requireNamespace("countrycode", quietly = TRUE)) {
      iso <- suppressWarnings(countrycode::countrycode(
        c, origin = "country.name", destination = "iso2c"
      ))
      if (!is.na(iso)) out <- c(out, iso) else
        warning("Country '", c, "' did not match any ISO2 code or group.",
                call. = FALSE)
    } else {
      warning("Country '", c, "' did not match any ISO2 code or group ",
              "(install the countrycode package for name-based lookup).",
              call. = FALSE)
    }
  }
  unique(out)
}

#' Translate a HiGGlobe `techs` selection into the set of technology labels
#' that appear in patents_x_tech. An umbrella label ("Green Technology",
#' "Defence Technology", ...) expands to every tech_lookup row whose
#' `tech_group` matches it.
.expand_techs <- function(techs, data_dir = "inst/extdata") {
  tl <- arrow::read_parquet(file.path(data_dir, "tech_lookup.parquet"))
  out <- character(0)
  for (t in techs) {
    grouped <- tl$technology[tl$tech_group == t]
    if (length(grouped) > 0) out <- c(out, grouped)
    out <- c(out, t)  # always include the literal label too
  }
  unique(out)
}

#' Build the HiGGlobe citation-network sample as a list of data frames.
#'
#' @param country chr: country selection (group label or ISO2). Multi-select
#'   allowed via a character vector.
#' @param techs   chr: technology selection (umbrella or specific). Multi-
#'   select allowed.
#' @param toflow  chr: spillover-flow column for marker sizing AND for
#'   ranking gen 0 candidates when `gen0_mode = "Top"`. Must exist in
#'   patent_database (`ev_global`, `ev_us`, ...).
#' @param gen0_n  int: number of gen 0 docdbs to take.
#' @param gen0_mode chr: `"Random"` (reservoir sample of the universe,
#'   matches the live app's "Random" gen 0 select mode) or `"Top"` (the
#'   `gen0_n` rows with highest `toflow`). Default `"Random"`.
#' @param n_generations int: how many follow-on generations to walk forward
#'   in the citation graph (gen 1..N citations of citations).
#' @param edge_sample_pct numeric in (0, 100]: at each generation step
#'   randomly subsample this percentage of the candidate edges before
#'   building the next frontier. Mirrors the live app's "Edge sampling"
#'   knob; lower values produce sparser, more legible maps. Default 100
#'   = no subsampling.
#' @param include_fallback Logical. When `FALSE` (default, matches the
#'   live HiGGlobe default) drop rows where countrymap recorded
#'   `geocode_missing = TRUE` — those are docdbs whose city wasn't
#'   geocoded so the country's capital coords were used as a fallback.
#'   Set `TRUE` to keep them on the map (popup will say
#'   "(capital fallback)").
#' @param seed    int: optional RNG seed for reproducibility.
#' @param data_dir directory holding the parquets.
#' @return list with components:
#'   * `nodes` data frame  : (docdb_family_id, ctry_code, city, lat, lon,
#'                           gen, toflow_val)
#'   * `edges` data frame  : (from_docdb, to_docdb, gen) — gen N edges
#'                           connect a gen-(N-1) source to a gen-N target.
#'   * `params`            : the call args, for caching / titles.
higglobe_local_sample <- function(country, techs,
                                  toflow            = "ev_global",
                                  gen0_n            = 100L,
                                  gen0_mode         = c("Random", "Top"),
                                  n_generations     = 4L,
                                  edge_sample_pct   = 100,
                                  include_fallback  = FALSE,
                                  seed              = NULL,
                                  data_dir          = "inst/extdata") {
  gen0_mode <- match.arg(gen0_mode)
  edge_sample_pct <- max(0.001, min(100, as.numeric(edge_sample_pct)))
  fallback_clause <- if (isTRUE(include_fallback)) ""
                     else "AND NOT c.geocode_missing"
  if (!requireNamespace("DBI",     quietly = TRUE) ||
      !requireNamespace("duckdb",  quietly = TRUE) ||
      !requireNamespace("arrow",   quietly = TRUE)) {
    stop("higglobe_local_sample needs DBI, duckdb, and arrow.")
  }
  ctries <- .expand_countries(country, data_dir = data_dir)
  techls <- .expand_techs(techs,      data_dir = data_dir)
  if (length(ctries) == 0L) stop("Country expansion produced 0 codes.")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d",
                              max(1L, parallel::detectCores() - 1L)))

  # Register parquets as views.
  for (nm in c("patents_x_tech", "countrymap", "citenet", "patent_database")) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s')",
      nm, file.path(data_dir, paste0(nm, ".parquet"))))
  }

  # Stage country / tech filters.
  DBI::dbWriteTable(con, "ctry_filter",
                    data.frame(ctry_code = ctries), overwrite = TRUE)
  DBI::dbWriteTable(con, "tech_filter",
                    data.frame(technology = techls), overwrite = TRUE)

  # ----------------------------------------------------------------------
  # Two universes:
  #  * hg_seed_universe  — (docdb, ctry) rows that pass the user's country
  #                        + tech filter. ONLY used to draw gen 0.
  #  * hg_global_coords  — (docdb -> best ctry) lookup for ANY docdb in the
  #                        database. Citing patents (gen 1..N) get located
  #                        from this. Filtering gen 1+ to the user's
  #                        country group would be wrong — the whole point
  #                        is that knowledge propagates globally.
  # ----------------------------------------------------------------------
  sql_seed <- sprintf("
    CREATE OR REPLACE TABLE hg_seed_universe AS
    SELECT p.docdb_family_id,
           p.ctry_code,
           ANY_VALUE(p.appln_id)     AS appln_id,
           MAX(p.%1$s)               AS toflow_val,
           ANY_VALUE(c.city)         AS city,
           ANY_VALUE(c.lat)          AS lat,
           ANY_VALUE(c.lon)          AS lon,
           BOOL_OR(c.geocode_missing) AS geocode_missing
    FROM patent_database p
    INNER JOIN countrymap c
      ON c.docdb_family_id = p.docdb_family_id
     AND c.ctry_code       = p.ctry_code
    INNER JOIN ctry_filter cf ON cf.ctry_code = p.ctry_code
    INNER JOIN (
      SELECT DISTINCT t.docdb_family_id
      FROM patents_x_tech t
      INNER JOIN tech_filter tf ON tf.technology = t.technology
    ) tt ON tt.docdb_family_id = p.docdb_family_id
    WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
      AND p.%1$s IS NOT NULL
      %2$s
    GROUP BY p.docdb_family_id, p.ctry_code", toflow, fallback_clause)
  DBI::dbExecute(con, sql_seed)

  # One row per docdb, choosing the (ctry) with the highest toflow_val.
  # ROW_NUMBER over partition of docdb_family_id gives a single best row.
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TABLE hg_global_coords AS
    WITH cand AS (
      SELECT p.docdb_family_id, p.ctry_code,
             ANY_VALUE(p.appln_id)      AS appln_id,
             COALESCE(MAX(p.%1$s), 0)   AS toflow_val,
             ANY_VALUE(c.city)          AS city,
             ANY_VALUE(c.lat)           AS lat,
             ANY_VALUE(c.lon)           AS lon,
             BOOL_OR(c.geocode_missing) AS geocode_missing
      FROM patent_database p
      INNER JOIN countrymap c
        ON c.docdb_family_id = p.docdb_family_id
       AND c.ctry_code       = p.ctry_code
      WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
        %2$s
      GROUP BY p.docdb_family_id, p.ctry_code
    ),
    ranked AS (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY docdb_family_id
                                   ORDER BY toflow_val DESC NULLS LAST,
                                            ctry_code) AS rn
      FROM cand
    )
    SELECT docdb_family_id, ctry_code, appln_id,
           city, lat, lon, geocode_missing, toflow_val
    FROM ranked WHERE rn = 1", toflow, fallback_clause))

  if (!is.null(seed)) {
    DBI::dbExecute(con, sprintf("SELECT setseed(%.6f)",
                                ((seed %% 1000L) / 1000L)))
  }

  # Gen 0: either a reservoir sample of the seed universe ("Random") or
  # the top-N rows by toflow_val ("Top"). Latter matches the live app's
  # "Top + Number" selectize combo.
  if (gen0_mode == "Top") {
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE hg_gen_0 AS
      SELECT * FROM hg_seed_universe
      ORDER BY toflow_val DESC NULLS LAST
      LIMIT %d", as.integer(gen0_n)))
  } else {
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE hg_gen_0 AS
      SELECT * FROM hg_seed_universe
      USING SAMPLE %d ROWS (reservoir)", as.integer(gen0_n)))
  }

  gens <- list(`0` = DBI::dbGetQuery(con,
    "SELECT docdb_family_id, ctry_code, appln_id, city, lat, lon,
            geocode_missing, toflow_val,
            CAST(0 AS INTEGER) AS gen FROM hg_gen_0"))

  edges <- list()

  for (g in seq_len(as.integer(n_generations))) {
    prev_tbl <- sprintf("hg_gen_%d", g - 1L)
    new_tbl  <- sprintf("hg_gen_%d", g)
    edge_tbl <- sprintf("hg_edge_%d", g)

    # Forward citations: rows of citenet whose `cited_docdb_family_id`
    # is in the previous frontier. Citing patents come from the *global*
    # docdb -> coords lookup (NOT filtered by country) — knowledge
    # propagates wherever it propagates. We dedupe to one row per citing
    # docdb (the global-coords table already picked one ctry per docdb).
    #
    # Edge subsampling mirrors the live app's "Edge sampling" knob: we
    # build the full set of candidate citing docdbs first, then take a
    # bernoulli sample of THAT (not of the global citenet, which would
    # discard ~99% of relevant rows even at 1% sample rate). At
    # edge_sample_pct = 100 every candidate survives; at 1%, ~1 in 100.
    cand_tbl <- sprintf("hg_cand_%d", g)
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE %s AS
      SELECT DISTINCT cn.docdb_family_id AS to_docdb,
             cn.cited_docdb_family_id    AS from_docdb
      FROM citenet cn
      INNER JOIN %s prev ON prev.docdb_family_id = cn.cited_docdb_family_id",
      cand_tbl, prev_tbl))

    sampled_tbl <- sprintf("hg_sampled_%d", g)
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE %s AS
      SELECT * FROM %s USING SAMPLE %f PERCENT (bernoulli)",
      sampled_tbl, cand_tbl, edge_sample_pct))

    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE %s AS
      SELECT u.docdb_family_id, u.ctry_code, u.appln_id,
             u.city, u.lat, u.lon, u.geocode_missing, u.toflow_val,
             CAST(%d AS INTEGER) AS gen
      FROM (
        SELECT DISTINCT to_docdb FROM %s
      ) cit
      INNER JOIN hg_global_coords u ON u.docdb_family_id = cit.to_docdb
      WHERE cit.to_docdb NOT IN (SELECT docdb_family_id FROM %s)
      ORDER BY u.toflow_val DESC NULLS LAST
      LIMIT 4000",
      new_tbl, g, sampled_tbl, prev_tbl))

    # Edges from prev gen to current gen, on the same sampled candidates.
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE %s AS
      SELECT s.from_docdb, s.to_docdb,
             CAST(%d AS INTEGER) AS gen
      FROM %s s
      INNER JOIN %s cur ON cur.docdb_family_id = s.to_docdb",
      edge_tbl, g, sampled_tbl, new_tbl))

    gens[[as.character(g)]] <- DBI::dbGetQuery(con,
      sprintf("SELECT docdb_family_id, ctry_code, appln_id,
                      city, lat, lon, geocode_missing, toflow_val, gen
               FROM %s", new_tbl))
    edges[[as.character(g)]] <- DBI::dbGetQuery(con,
      sprintf("SELECT from_docdb, to_docdb, gen FROM %s", edge_tbl))

    if (nrow(gens[[as.character(g)]]) == 0L) break
  }

  list(
    nodes  = do.call(rbind, gens),
    edges  = do.call(rbind, edges),
    params = list(country = country, techs = techs, toflow = toflow,
                  gen0_n = gen0_n, n_generations = n_generations,
                  seed = seed)
  )
}

#' Render a HiGGlobe-style leaflet map from `higglobe_local_sample()` output.
#'
#' Matches the deployed app's colour scheme:
#'   gen 0 = blue, 1 = red, 2 = green, 3 = orange, 4 = purple.
#'
#' @param data list returned by `higglobe_local_sample()`.
#' @param title chr: optional caption shown in a Leaflet control.
#' @param height chr: CSS height for the widget (default "420px").
#' @return a {leaflet} htmlwidget. Embeds inline in self-contained HTML.
higglobe_local_leaflet <- function(data, title = NULL, height = "420px") {
  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("higglobe_local_leaflet needs the leaflet package.")
  if (is.null(data$nodes) || nrow(data$nodes) == 0L) return(NULL)

  # HTML <a> wrapper around an Espacenet `pn=` search URL — same format
  # as R/module_hglobe.R::espacenet_link, vectorised over `appln_id`.
  espacenet_link_html <- function(appln_id) {
    vapply(as.character(appln_id), function(id) {
      if (is.na(id) || !nzchar(id)) return("(no appln id)")
      sprintf(
        '<a href="https://worldwide.espacenet.com/patent/search?q=%s" target="_blank" rel="noopener">%s</a>',
        utils::URLencode(paste0("pn=", id), reserved = TRUE),
        id
      )
    }, character(1))
  }

  gen_colours <- c(`0` = "#1f77b4",   # blue
                   `1` = "#d62728",   # red
                   `2` = "#2ca02c",   # green
                   `3` = "#ff7f0e",   # orange
                   `4` = "#9467bd")   # purple

  # Quadratic Bezier between (sx,sy) and (tx,ty) sampled at `n` points,
  # offset perpendicular to the chord by `bend * d`. Direct lift from
  # R/module_hglobe.R::make_curve so the local note matches the live
  # app's arc style.
  make_curve <- function(sx, sy, tx, ty, n = 40, bend = 0.3) {
    if (sx == tx && sy == ty)
      return(list(lon = c(sx, tx), lat = c(sy, ty)))
    mx <- (sx + tx) / 2; my <- (sy + ty) / 2
    dx <- tx - sx;       dy <- ty - sy
    d  <- sqrt(dx^2 + dy^2); if (d == 0) d <- 1
    px <- -dy / d;       py <-  dx / d
    h  <- bend * d
    cx <- mx + px * h;   cy <- my + py * h
    tt <- seq(0, 1, length.out = n)
    list(
      lon = (1 - tt)^2 * sx + 2 * (1 - tt) * tt * cx + tt^2 * tx,
      lat = (1 - tt)^2 * sy + 2 * (1 - tt) * tt * cy + tt^2 * ty
    )
  }

  # Tiny per-country jitter so coincident docdbs don't overlap perfectly.
  jitter_amt <- 0.6
  set.seed(42L)
  nodes <- data$nodes
  nodes$lat_j <- nodes$lat + stats::runif(nrow(nodes), -jitter_amt, jitter_amt)
  nodes$lon_j <- nodes$lon + stats::runif(nrow(nodes), -jitter_amt, jitter_amt)

  # Build coord lookup keyed on docdb_family_id so we can resolve edges.
  by_docdb <- split(nodes[, c("lat_j", "lon_j", "gen")],
                    nodes$docdb_family_id)

  # Marker radius scales with toflow_val on a log1p basis — direct lift
  # of radius_from_flow() from R/module_hglobe.R so the local note's
  # marker sizing matches the live app.
  radius_from_flow <- function(v, r_min = 5, r_max = 16) {
    v  <- suppressWarnings(as.numeric(v))
    lv <- log1p(pmax(v, 0, na.rm = FALSE))
    lv[!is.finite(lv)] <- 0
    rng <- diff(range(lv, na.rm = TRUE))
    if (is.na(rng) || rng <= 0) return(rep((r_min + r_max) / 3, length(v)))
    r_min + (r_max - r_min) * (lv - min(lv, na.rm = TRUE)) / rng
  }
  nodes$radius <- radius_from_flow(nodes$toflow_val)

  m <- leaflet::leaflet(
    data    = nodes,
    width   = "100%",
    height  = height,
    options = leaflet::leafletOptions(
      zoomControl        = TRUE,
      attributionControl = TRUE
      # NB: preferCanvas tried — broke per-polyline colours (arcs all
      # rendered black). SVG renderer with one addPolylines call per
      # edge applies the per-call colour reliably.
    )
  ) |>
    leaflet::addProviderTiles("CartoDB.Positron")

  # One layer per generation, so the legend reads "gen 0 / 1 / 2 / ..."
  # and each generation's markers carry their own fill colour. We use
  # `fillColor` (NOT `color`, which is the stroke colour and was the
  # source of the "all markers grey" appearance in the previous
  # version), with `stroke = FALSE` to match the live app's look.
  # Pre-compute popup HTML so we can use the Espacenet link helper (which
  # is vectorised but isn't a leaflet formula expression). Each marker's
  # popup leads with the searchable publication number (clickable to
  # Espacenet), then the city or the "(capital fallback)" label, then
  # country / flow / generation / docdb_family_id at the end.
  nodes$popup_html <- sprintf(
    paste0("<b>%s</b><br>",
           "%s, %s<br>",
           "flow: %s &middot; gen %d<br>",
           "<span style='color:#888;font-size:0.85em;'>docdb %s</span>"),
    espacenet_link_html(nodes$appln_id),
    ifelse(is.na(nodes$city) | !nzchar(nodes$city),
           "<i>(capital fallback)</i>", nodes$city),
    nodes$ctry_code,
    formatC(nodes$toflow_val, format = "g", digits = 3),
    nodes$gen,
    nodes$docdb_family_id
  )

  for (g in sort(unique(nodes$gen))) {
    col <- unname(gen_colours[as.character(g)])
    sub <- nodes[nodes$gen == g, , drop = FALSE]
    m <- leaflet::addCircleMarkers(
      m,
      data        = sub,
      lng         = ~lon_j, lat = ~lat_j,
      radius      = ~radius,
      stroke      = FALSE,
      fillColor   = col,
      fillOpacity = 0.65,
      group       = sprintf("gen %d", g),
      popup       = sub$popup_html
    )
  }

  # Edges as faint Bezier-curved polylines, coloured by destination gen.
  # One addPolylines call PER edge — leaflet's list-of-vectors form goes
  # through derivePolygons() and complains, so the per-edge loop is the
  # path of least resistance. SVG renderer applies the per-call colour
  # correctly; the previous "all arcs black" bug was caused by canvas
  # mode (now turned off in the leafletOptions above).
  if (!is.null(data$edges) && nrow(data$edges) > 0L) {
    edge_col <- function(g) unname(gen_colours[as.character(g)])
    for (i in seq_len(nrow(data$edges))) {
      g <- data$edges$gen[i]
      f <- by_docdb[[as.character(data$edges$from_docdb[i])]]
      t <- by_docdb[[as.character(data$edges$to_docdb[i])]]
      if (is.null(f) || is.null(t)) next
      cv <- make_curve(f$lon_j[1], f$lat_j[1], t$lon_j[1], t$lat_j[1])
      m <- leaflet::addPolylines(
        m,
        lng          = cv$lon, lat = cv$lat,
        color        = edge_col(g),
        weight       = 0.8,
        opacity      = 0.32,
        group        = sprintf("gen %d", g),
        smoothFactor = 1.5
      )
    }
  }

  # Layer toggle + legend.
  m <- leaflet::addLayersControl(
    m,
    overlayGroups = sprintf("gen %d", sort(unique(nodes$gen))),
    options       = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
    leaflet::addLegend(
      position = "bottomleft",
      colors   = unname(gen_colours[as.character(sort(unique(nodes$gen)))]),
      labels   = sprintf("gen %d", sort(unique(nodes$gen))),
      title    = "Generations",
      opacity  = 0.9
    )

  if (!is.null(title)) {
    m <- leaflet::addControl(
      m,
      html     = sprintf("<div style='background:#fff;padding:4px 8px;
                          border-radius:4px;border:1px solid #ddd;
                          font-weight:600;font-size:0.9rem;'>%s</div>", title),
      position = "topright"
    )
  }
  m
}


#' Local equivalent of the deployed app's "Country Explorer · Value flows
#' by Technology" CSV download. Computes per-(technology) mean / sem /
#' innovation-count of the chosen `toflow` column, restricted to a
#' country group, and optionally to granted families only.
#'
#' Returned columns mirror the subset of the live-app CSV we actually
#' use in tanks2turbines.rmd: `technology`, `mean`, `sem`, `innos`. No
#' chromote, no live-app dependency.
#'
#' @param country chr: country selection — group label
#'   ("LMICs (excl. China)", "All countries", ...) or ISO2 codes.
#' @param techs   chr vector: each entry will be one row in the result.
#'   * `"All innovations"` → no tech filter (universe-wide aggregate).
#'   * Any value present in `tech_lookup$tech_group` (e.g. "Green
#'     Technology", "Defence Technology") expands to every subtechnology
#'     mapped to that group.
#'   * Anything else is treated as a literal `patents_x_tech.technology`
#'     label.
#' @param toflow chr: column in patent_database to aggregate.
#' @param granted_only Logical. `FALSE` (default) = full database, both
#'   granted and non-granted. `TRUE` = restrict to families flagged
#'   `granted = TRUE` in patent_database.
#' @param multifam_only Logical. `FALSE` (default) = no restriction.
#'   `TRUE` = restrict to families flagged `fam_size_min2 = TRUE`
#'   (PATSTAT `docdb_family_size >= 2`), filtering out one-off
#'   single-application families. Combine with `granted_only` for the
#'   strictest sample.
#' @param data_dir directory holding the parquet files.
local_value_flows <- function(country, techs,
                              toflow        = "ev_global",
                              granted_only  = FALSE,
                              multifam_only = FALSE,
                              data_dir      = "inst/extdata") {
  if (!requireNamespace("DBI",    quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("arrow",  quietly = TRUE)) {
    stop("local_value_flows needs DBI, duckdb, and arrow.")
  }
  ctries <- .expand_countries(country, data_dir = data_dir)
  if (length(ctries) == 0L) stop("Country expansion produced 0 codes.")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d",
                              max(1L, parallel::detectCores() - 1L)))

  for (nm in c("patent_database", "patents_x_tech", "tech_lookup")) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s')",
      nm, file.path(data_dir, paste0(nm, ".parquet"))))
  }
  DBI::dbWriteTable(con, "ctry_filter",
                    data.frame(ctry_code = ctries), overwrite = TRUE)

  filter_clause <- paste(
    if (isTRUE(granted_only))  "AND p.granted = TRUE"        else "",
    if (isTRUE(multifam_only)) "AND p.fam_size_min2 = TRUE"  else ""
  )

  # Aggregate one row per (technology). The CSV-side `mean` is the
  # per-(docdb, ctry) average of toflow over rows passing the filter;
  # `innos` is the count of distinct docdbs; `sem` is the standard error
  # of the mean = stddev / sqrt(N rows).
  rows <- vector("list", length(techs))
  for (i in seq_along(techs)) {
    t <- techs[[i]]
    if (identical(t, "All innovations")) {
      sql <- sprintf("
        SELECT '%s' AS technology,
               AVG(p.%s)                          AS mean,
               STDDEV_SAMP(p.%s) / SQRT(COUNT(*)) AS sem,
               COUNT(DISTINCT p.docdb_family_id)  AS innos
        FROM patent_database p
        INNER JOIN ctry_filter cf ON cf.ctry_code = p.ctry_code
        WHERE p.%s IS NOT NULL %s",
        gsub("'", "''", t),
        toflow, toflow, toflow, filter_clause)
    } else {
      expanded <- .expand_techs(t, data_dir = data_dir)
      tech_list <- paste0("'", paste(gsub("'", "''", expanded),
                                     collapse = "','"), "'")
      sql <- sprintf("
        SELECT '%s' AS technology,
               AVG(p.%s)                          AS mean,
               STDDEV_SAMP(p.%s) / SQRT(COUNT(*)) AS sem,
               COUNT(DISTINCT p.docdb_family_id)  AS innos
        FROM patent_database p
        INNER JOIN ctry_filter cf ON cf.ctry_code = p.ctry_code
        INNER JOIN (
          SELECT DISTINCT docdb_family_id
          FROM patents_x_tech
          WHERE technology IN (%s)
        ) tt ON tt.docdb_family_id = p.docdb_family_id
        WHERE p.%s IS NOT NULL %s",
        gsub("'", "''", t),
        toflow, toflow, tech_list, toflow, filter_clause)
    }
    rows[[i]] <- DBI::dbGetQuery(con, sql)
  }
  do.call(rbind, rows)
}


# ============================================================================
# Helpers used by tanks2turbines.rmd to render four sample-variant tabs.
# Each one takes a `flows` data frame (from local_value_flows() across all
# origins) and an `origins` list, and returns a single ggplot or patchwork.
# ============================================================================

#' @keywords internal
.t2t_umbrella_labels <- c("All innovations",
                          "Green Technology", "AI",
                          "Any Agriculture & Food technology",
                          "Defence Technology")

#' @keywords internal
.t2t_green_subs <- c("Green Energy", "Green Transport", "Green Manufacturing",
                     "GHG Capture", "Adaptation", "Green Housing",
                     "Circular Economy", "Green ICT", "Green Agriculture")

#' @keywords internal
.t2t_defence_subs <- c("Small Arms & Ordnance", "Non-firearm Weapons",
                       "Projectile & Missile Launching",
                       "Weapon Sights & Aiming", "Missile Propulsion",
                       "Armour & Armoured Vehicles", "Targets & Training",
                       "Ammunition", "Fuzes & Ammunition Safety",
                       "Blasting & Demolition", "Naval Warfare",
                       "Military Aviation Equipment", "Defence Radar & Sonar")

#' Headline 4-umbrella bar chart, faceted by origin.
#' @keywords internal
t2t_make_umbrella_bar <- function(flows_var, sample_label = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  bar_df <- flows_var[flows_var$technology %in% .t2t_umbrella_labels, ]
  if (nrow(bar_df) == 0L) return(NULL)
  bar_df$label <- dplyr::recode(bar_df$technology,
    "Any Agriculture & Food technology" = "Agri & Food",
    "Green Technology"   = "Green",
    "Defence Technology" = "Defence")
  bar_df$fill_grp <- dplyr::case_when(
    bar_df$technology == "All innovations"                   ~ "all",
    bar_df$technology == "Green Technology"                  ~ "green",
    bar_df$technology == "AI"                                ~ "AI",
    bar_df$technology == "Any Agriculture & Food technology" ~ "agrifood",
    bar_df$technology == "Defence Technology"                ~ "defence"
  )
  ord <- bar_df |>
    dplyr::group_by(label) |>
    dplyr::summarise(s = mean(mean, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(s) |>
    dplyr::pull(label)
  bar_df$label <- factor(bar_df$label, levels = ord)

  ggplot2::ggplot(bar_df, ggplot2::aes(y = label, x = mean)) +
    ggplot2::geom_col(ggplot2::aes(fill = fill_grp), width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = pmax(mean - sem, 0), xmax = mean + sem),
      width = 0.18, colour = "grey30", linewidth = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = innos, fill = fill_grp),
      shape = 21, colour = "grey15", stroke = 0.6, alpha = 0.95
    ) +
    ggplot2::facet_wrap(~ origin, nrow = 1) +
    ggplot2::scale_fill_manual(
      values = c(all = "gray70", green = "forestgreen", AI = "orange",
                 agrifood = "burlywood", defence = "gray35"),
      guide  = "none"
    ) +
    ggplot2::scale_size_area(
      name     = "Innovations",
      max_size = 11,
      labels   = scales::comma,
      breaks   = scales::pretty_breaks(4)
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(
                                  mult = c(0, 0.18))) +
    ggplot2::labs(
      title    = "Average global spillovers per innovation, by origin",
      subtitle = paste0(
        "Common x-axis · ",
        if (nzchar(sample_label)) paste0(sample_label, " · "),
        "toflow = ev_global · 'All innovations' baseline included · ",
        "bars: mean +/- 1 SEM · circle area = N innovations"),
      y = NULL,
      x = "Spillover to global per innovation ($)",
      caption = "Source: Innovation Strategy Explorer, computed locally from patent_database.parquet"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position    = "right",
                   strip.text         = ggplot2::element_text(face = "bold",
                                                              size = 11))
}

#' Subcategory drilldown bar chart, faceted by origin.
#' @keywords internal
t2t_make_drilldown_bar <- function(flows_var, sample_label = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  plot_df <- flows_var[flows_var$technology != "All" &
                        flows_var$technology != "All innovations", ]
  plot_df$family <- dplyr::case_when(
    plot_df$technology %in% .t2t_green_subs   ~ "Green Technology",
    plot_df$technology %in% .t2t_defence_subs ~ "Defence Technology",
    TRUE                                       ~ NA_character_
  )
  plot_df <- plot_df[!is.na(plot_df$family), ]
  if (nrow(plot_df) == 0L) return(NULL)

  ord <- plot_df |>
    dplyr::group_by(technology) |>
    dplyr::summarise(s = mean(mean, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(s) |>
    dplyr::pull(technology)
  plot_df$technology <- factor(plot_df$technology, levels = ord)

  family_means <- plot_df |>
    dplyr::group_by(origin, family) |>
    dplyr::summarise(mean_of_means = mean(mean, na.rm = TRUE),
                     .groups = "drop")

  ggplot2::ggplot(plot_df, ggplot2::aes(y = technology, x = mean)) +
    ggplot2::geom_col(ggplot2::aes(fill = family), width = 0.75) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = pmax(mean - sem, 0), xmax = mean + sem),
      width = 0.25, colour = "grey30", linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      data        = family_means,
      mapping     = ggplot2::aes(xintercept = mean_of_means, colour = family),
      linetype    = "dashed", linewidth = 0.7, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = innos, fill = family),
      shape = 21, colour = "grey15", stroke = 0.5, alpha = 0.95
    ) +
    ggplot2::facet_wrap(~ origin, nrow = 1) +
    ggplot2::scale_fill_manual(
      values = c("Green Technology" = "forestgreen",
                 "Defence Technology" = "gray35"),
      name = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c("Green Technology" = "forestgreen",
                 "Defence Technology" = "gray35"),
      guide = "none"
    ) +
    ggplot2::scale_size_area(
      name     = "Innovations",
      max_size = 10,
      labels   = scales::comma,
      breaks   = scales::pretty_breaks(4)
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(
                                  mult = c(0, 0.08))) +
    ggplot2::labs(
      title    = "Average global spillovers per innovation: subcategory drilldown, by origin",
      subtitle = paste0(
        "Common x-axis · ",
        if (nzchar(sample_label)) paste0(sample_label, " · "),
        "bars: spillover +/- SEM · dashed: family means · ",
        "circle area = N innovations"),
      y = NULL,
      x = "Spillover to global per innovation ($, +/- 1 SEM)",
      caption = "Source: Innovation Strategy Explorer, computed locally from patent_database.parquet"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position    = "right",
                   panel.grid.major.y = ggplot2::element_blank(),
                   strip.text         = ggplot2::element_text(face = "bold",
                                                              size = 11))
}

#' Two-row donut grid (green subcats top, defence subcats bottom), one
#' donut per origin. Uses {patchwork} to assemble.
#' @keywords internal
t2t_make_donut_grid <- function(flows_var, origins) {
  if (!requireNamespace("patchwork", quietly = TRUE)) return(NULL)
  green_pal <- setNames(grDevices::colorRampPalette(
                          c("#1a5c1a", "#a3d8a3"))(length(.t2t_green_subs)),
                        .t2t_green_subs)
  defence_pal <- setNames(grDevices::colorRampPalette(
                            c("#2a2a2a", "#bbbbbb"))(length(.t2t_defence_subs)),
                          .t2t_defence_subs)

  pick <- function(o, tech) {
    v <- flows_var$innos[flows_var$origin == o$label &
                          flows_var$technology == tech]
    if (!length(v)) NA_real_ else v[1]
  }
  share_pct <- function(o, tech, digits) {
    total <- pick(o, "All innovations")
    v     <- pick(o, tech)
    if (is.na(v) || is.na(total) || total == 0) return("--")
    format(round(100 * v / total, digits), nsmall = digits)
  }

  build_donut <- function(o, subs, pal, umbrella_label, umbrella_pct,
                          legend_name) {
    sub_df <- flows_var[flows_var$origin == o$label &
                         flows_var$technology %in% subs, ]
    sub_df <- sub_df[!is.na(sub_df$innos) & sub_df$innos > 0, ]
    if (nrow(sub_df) == 0L) return(
      ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::ggtitle(o$label) +
        ggplot2::theme(plot.title = ggplot2::element_text(
          hjust = 0.5, face = "bold")))
    sub_df$technology <- factor(sub_df$technology, levels = subs)
    sub_df <- sub_df[order(sub_df$technology), ]
    ggplot2::ggplot(sub_df, ggplot2::aes(x = 2, y = innos,
                                          fill = technology)) +
      ggplot2::geom_col(colour = "white", width = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(0.4, 2.7) +
      ggplot2::scale_fill_manual(values = pal, drop = FALSE,
                                  breaks = subs, name = legend_name) +
      ggplot2::annotate("text", x = 0.4, y = 0, vjust = 0.5,
                        label = sprintf("%s\n%s%%\nof total",
                                        umbrella_label, umbrella_pct),
                        size = 3.6, lineheight = 1.0, fontface = "bold") +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(
        hjust = 0.5, face = "bold")) +
      ggplot2::labs(title = o$label)
  }

  green_donuts <- lapply(origins, function(o)
    build_donut(o, .t2t_green_subs, green_pal,
                "Green",   share_pct(o, "Green Technology", 2),
                "Green subcategories"))
  defence_donuts <- lapply(origins, function(o)
    build_donut(o, .t2t_defence_subs, defence_pal,
                "Defence", share_pct(o, "Any Defence technology", 3),
                "Defence subcategories"))

  green_row <- patchwork::wrap_plots(green_donuts, nrow = 1) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = "Green-tech subcategories") &
    ggplot2::theme(legend.position = "right",
                   legend.text     = ggplot2::element_text(size = 8),
                   legend.key.size = ggplot2::unit(11, "pt"))

  defence_row <- patchwork::wrap_plots(defence_donuts, nrow = 1) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = "Defence-tech subcategories") &
    ggplot2::theme(legend.position = "right",
                   legend.text     = ggplot2::element_text(size = 8),
                   legend.key.size = ggplot2::unit(11, "pt"))

  patchwork::wrap_plots(list(green_row, defence_row), ncol = 1,
                        heights = c(1, 1)) +
    patchwork::plot_annotation(
      caption = paste0(
        "Slice area = patent families tagged with each subcategory ",
        "(subcategories overlap; one family can carry several tags). ",
        "Centre text: umbrella share of the origin's total patent universe."
      ),
      theme = ggplot2::theme(
        plot.caption = ggplot2::element_text(size = 9, colour = "grey40")
      )
    )
}
