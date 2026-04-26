#' Shiny app Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @keywords internal
server <- function(input, output, session, con) {

  startup_waiter <- waiter::Waiter$new(html = landing_ui_content())
  startup_waiter$show()

  # useful for debugging; can comment off if not using
  session_id <- session$token

  # Show sever message & reload button
  sever::sever()

  # --------------------------------------------------------------------------
  # Scope bookmarked URLs to the active tab only.
  #
  # Without this, Shiny serialises every registered input (country-*,
  # region-*, globe-*, welcome-*, ...) into the URL. Only the inputs of the
  # tab the user is looking at matter for the shareable view — the rest is
  # noise that bloats the URL and may blow past proxy length limits.
  #
  # `setBookmarkExclude()` takes a character vector of input IDs to omit.
  # We recompute it reactively from input$navbar_page, so switching tabs
  # shrinks the URL to just that tab's inputs.
  # --------------------------------------------------------------------------
  shiny::observe({
    tab         <- input$navbar_page
    all_inputs  <- names(shiny::reactiveValuesToList(input))

    # Always keep the tab selector itself. Inputs are namespaced by their
    # module id ("country-...", "region-...", "globe-...").
    keep_prefixes <- switch(tab %||% "",
      "Country Explorer" = c("country-"),
      "Region Explorer"  = c("region-"),
      "HiGGlobe"         = c("hglobe-"),
      "About"            = character(),
      character()        # unknown / fallback — keep only navbar_page
    )

    keep_flag <- vapply(all_inputs, function(x) {
      x == "navbar_page" ||
        (length(keep_prefixes) > 0 &&
         any(startsWith(x, keep_prefixes)))
    }, logical(1))

    session$setBookmarkExclude(all_inputs[!keep_flag])
  })

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)

    # Restore main navbar tab
    if (!is.null(query$navbar_page)) {
      tab_name <- gsub('^"|"$', '', query$navbar_page)
      bslib::nav_select(id = "navbar_page", selected = tab_name, session = session)
    }

    # Store all params for modules to access
    session$userData$restore_params <- query
  })

  # --------------------------------------------------------------------------
  # Custom URL-parameter API (orthogonal to Shiny's native URL bookmarking).
  #
  # Lets external callers open the app pre-configured with a short, hand-
  # written query string, e.g.
  #   https://<app>/?tab=Country+Explorer
  #                 &ctry=US,CN,DE
  #                 &tech=Green+Technology
  #                 &flow=is_global
  #                 &granted=1
  #
  # Recognised params (all optional, all case-sensitive for values):
  #   tab        Navbar tab: "Country Explorer" | "Region Explorer" |
  #              "HiGGlobe" | "About"
  #   ctry       Comma-separated country codes or predefined group names
  #              (uses country-tab input).
  #   region     Comma-separated UK region names (region-tab input).
  #   tech       Comma-separated technology labels; applied to BOTH the
  #              country-tab and region-tab tech inputs.
  #   firm       Comma-separated firm names or "No firm filter"; applied
  #              to both country and region firm inputs.
  #   flow       Flow column name (e.g. "is_global", "ev_emde").
  #   granted    "1" / "true" / "yes" to tick the "Granted families only"
  #              checkbox; anything else unticks it.
  #   top_n_ids  Integer — number of top patents to list.
  #
  # Fires once per session. Module inputs updated via the global session
  # with full namespaced IDs ("country-country", "region-toflow_region",
  # ...). Shiny queues the updates if the input isn't yet realised, so the
  # order of arrival (params vs lazy module init) doesn't matter.
  # --------------------------------------------------------------------------
  shiny::observeEvent(session$clientData$url_search, once = TRUE,
                      ignoreNULL = FALSE, {
    q <- shiny::parseQueryString(session$clientData$url_search %||% "")
    if (!length(q)) return()

    csv <- function(x) if (is.null(x) || !nzchar(x)) character() else
                         trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    as_bool <- function(x) !is.null(x) &&
                           tolower(x) %in% c("1", "true", "yes", "on")

    # --- Country tab inputs (namespaced as "country-<id>") ------------------
    if (!is.null(q$ctry))
      shiny::updateSelectizeInput(session, "country-country",
                                  selected = csv(q$ctry))
    if (!is.null(q$tech))
      shiny::updateSelectizeInput(session, "country-techs",
                                  selected = csv(q$tech))
    if (!is.null(q$firm))
      shiny::updateSelectizeInput(session, "country-firm",
                                  selected = csv(q$firm))
    if (!is.null(q$flow))
      shiny::updateSelectizeInput(session, "country-toflow", selected = q$flow)
    if (!is.null(q$granted))
      shiny::updateCheckboxInput(session, "country-granted_only",
                                 value = as_bool(q$granted))
    if (!is.null(q$multifam))
      shiny::updateCheckboxInput(session, "country-multifam_only",
                                 value = as_bool(q$multifam))
    if (!is.null(q$top_n_ids))
      shiny::updateNumericInput(session, "country-top_n_ids",
                                value = suppressWarnings(as.integer(q$top_n_ids)))

    # --- Region tab inputs (namespaced as "region-<id>") --------------------
    if (!is.null(q$region))
      shiny::updateSelectizeInput(session, "region-region",
                                  selected = csv(q$region))
    if (!is.null(q$tech))
      shiny::updateSelectizeInput(session, "region-techs_region",
                                  selected = csv(q$tech))
    if (!is.null(q$firm))
      shiny::updateSelectizeInput(session, "region-firm",
                                  selected = csv(q$firm))
    if (!is.null(q$flow))
      shiny::updateSelectizeInput(session, "region-toflow_region",
                                  selected = q$flow)
    if (!is.null(q$granted))
      shiny::updateCheckboxInput(session, "region-granted_only",
                                 value = as_bool(q$granted))
    if (!is.null(q$multifam))
      shiny::updateCheckboxInput(session, "region-multifam_only",
                                 value = as_bool(q$multifam))
    if (!is.null(q$top_n_ids))
      shiny::updateNumericInput(session, "region-top_n_ids_region",
                                value = suppressWarnings(as.integer(q$top_n_ids)))

    # --- HiGGlobe URL params (also accept ctry / firm / tech / flow /
    # granted / multifam — those are forwarded by the country-tab handlers
    # above only when ctry / firm / tech are present, but HiGGlobe uses
    # the same input ids prefixed `hglobe-`, so we mirror them here).
    if (!is.null(q$ctry))
      shiny::updateSelectizeInput(session, "hglobe-country",
                                  selected = csv(q$ctry))
    if (!is.null(q$tech))
      shiny::updateSelectizeInput(session, "hglobe-techs",
                                  selected = csv(q$tech))
    if (!is.null(q$firm))
      shiny::updateSelectizeInput(session, "hglobe-firm",
                                  selected = csv(q$firm))
    if (!is.null(q$flow))
      shiny::updateSelectizeInput(session, "hglobe-toflow", selected = q$flow)
    if (!is.null(q$granted))
      shiny::updateCheckboxInput(session, "hglobe-granted_only",
                                 value = as_bool(q$granted))
    if (!is.null(q$multifam))
      shiny::updateCheckboxInput(session, "hglobe-multifam_only",
                                 value = as_bool(q$multifam))

    # higglobe_gen=N preloads the "Add Generations" field. The actual click
    # is wired up inside the module so it can wait for gen 0 to be seeded
    # before firing Generate.
    if (!is.null(q$higglobe_gen)) {
      n_gen <- suppressWarnings(as.integer(q$higglobe_gen))
      if (!is.na(n_gen) && n_gen > 0)
        shiny::updateNumericInput(session, "hglobe-add_generations",
                                  value = n_gen)
    }

    # `step=N` (legacy custom param) — preserved as a shorthand.
    step_val <- suppressWarnings(as.integer(q$step))
    if (length(step_val) == 1L && !is.na(step_val) && step_val > 0L) {
      shiny::updateNumericInput(session, "hglobe-add_generations",
                                value = step_val)
      if (is.null(session$userData$restore_params))
        session$userData$restore_params <- list()
      session$userData$restore_params$higglobe_gen <-
        as.character(step_val)
      session$userData$restore_params$higglobe_run <- "1"
      bslib::nav_select(id = "navbar_page", selected = "HiGGlobe",
                        session = session)
    }

    # Auto-replay from a Shiny native bookmark: when the bookmark URL
    # contains hglobe-render_map > 0 (meaning the button was clicked at
    # least once in the previous session), treat that as the auto-init
    # signal. If hglobe-next_step is also > 0, auto-fire Generate after
    # the seed is ready. The actual button clicks are dispatched by the
    # module itself, after its observers are registered (clicking from
    # server.R raced the lazy module init and was unreliable).
    rm_clicks <- suppressWarnings(as.integer(q[["hglobe-render_map"]]))
    if (length(rm_clicks) == 1L && !is.na(rm_clicks) && rm_clicks > 0L) {
      if (is.null(session$userData$restore_params))
        session$userData$restore_params <- list()
      session$userData$restore_params$higglobe_run <- "1"
      ns_clicks <- suppressWarnings(as.integer(q[["hglobe-next_step"]]))
      if (length(ns_clicks) == 1L && !is.na(ns_clicks) && ns_clicks > 0L) {
        # Mark the auto-Generate as armed; the module reads
        # higglobe_gen and the user's bookmarked add_generations field
        # already carries the per-click step count Shiny restored.
        session$userData$restore_params$higglobe_gen <- "1"
      }
      bslib::nav_select(id = "navbar_page", selected = "HiGGlobe",
                        session = session)
    }

    # higglobe_run=1 just stashes the flag and switches to HiGGlobe; the
    # module fires the click itself once it has booted.
    if (as_bool(q$higglobe_run)) {
      if (is.null(session$userData$restore_params))
        session$userData$restore_params <- list()
      session$userData$restore_params$higglobe_run <- "1"
      bslib::nav_select(id = "navbar_page", selected = "HiGGlobe",
                        session = session)
    }
  })

  # Call Modules
  landing_ready <- landing_module_server("landing", waiter = startup_waiter, con = con)
  welcome_module_server("welcome", parent_session = session)
  about_module_server("about")
  shiny::observeEvent(landing_ready(), once = TRUE, {
    country_module_server("country", session, con = con)
  })
  shiny::observeEvent(c(req(input$navbar_page == "Region Explorer")), once = TRUE, {
    region_module_server("region", session, con = con)
  })
  shiny::observeEvent(c(req(input$navbar_page == "HiGGlobe")), once = TRUE, {
    hglobe_module_server("hglobe", con = con)
  })

}
