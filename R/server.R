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
      "Globe"            = c("globe-"),
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
  #              "Globe" | "About"
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
    if (!is.null(q$top_n_ids))
      shiny::updateNumericInput(session, "region-top_n_ids_region",
                                value = suppressWarnings(as.integer(q$top_n_ids)))
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
  shiny::observeEvent(c(req(input$navbar_page == "Globe")), once = TRUE, {
    globe_module_server("globe", session)
  })
  
}
