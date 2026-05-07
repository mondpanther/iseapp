#' Welcome / landing-page Shiny module
#'
#' Shown as the default tab when the app opens without URL parameters.
#' Heading in the middle, four large metric buttons below that jump to the
#' Country Explorer tab and pre-select the matching value flow.
#'
#' @keywords internal
welcome_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Outer dynamic-bg layer covers the whole welcome panel. Background URL
    # is set reactively in welcome_module_server via output$bg_css — that
    # output re-rolls on every navbar tab change, so each visit to the
    # welcome page lands on a different randomly-picked HiGGlobe image
    # from inst/insights_html/figures/.
    shiny::uiOutput(ns("bg_css"), inline = TRUE),
    shiny::tags$style(shiny::HTML("
      /* Outer bg layer: image fills the welcome panel; visible behind and
         between the buttons. Solid white fallback while the image loads. */
      .ise-welcome-bg {
        background-color: #ffffff;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        min-height: calc(100vh - 80px);
        transition: background-image 0.6s ease-in-out;
      }
      /* Heading + intro: a soft translucent card so they stay legible on
         top of a busy citation-globe background, without obscuring the
         image too much. */
      .ise-welcome-msg {
        display: inline-block;
        max-width: 760px;
        padding: 22px 30px 14px 30px;
        margin-bottom: 36px;
        background: rgba(255, 255, 255, 0.78);
        backdrop-filter: blur(2px);
        -webkit-backdrop-filter: blur(2px);
        border-radius: 14px;
        box-shadow: 0 2px 14px rgba(0, 0, 0, 0.08);
      }
      .ise-welcome-msg h1 {
        font-size: 2.1rem;
        font-weight: 600;
        margin: 0 0 10px 0;
        line-height: 1.25;
        color: #1a1a1a;
      }
      .ise-welcome-msg p {
        margin: 0;
        color: #444;
      }
    ")),
  shiny::div(
    class = "ise-welcome-bg",
    shiny::div(
    style = "
      max-width: 900px;
      margin: 0 auto;
      padding: 80px 20px 60px 20px;
      text-align: center;
    ",
    shiny::div(
      class = "ise-welcome-msg",
      shiny::h1("Welcome to the Innovation Strategy Explorer"),
      shiny::p("Choose a metric to start exploring:")
    ),
    shiny::div(
      style = "
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 16px;
        max-width: 720px;
        margin: 0 auto;
      ",
      shiny::actionButton(
        ns("go_rta"), "Revealed Technological Advantage",
        class = "btn-primary btn-lg",
        style = "white-space: normal; min-height: 84px; font-weight: 500;"
      ),
      shiny::actionButton(
        ns("go_spillovers"), "Average Spillovers",
        class = "btn-primary btn-lg",
        style = "white-space: normal; min-height: 84px; font-weight: 500;"
      ),
      shiny::actionButton(
        ns("go_returns"), "Internal and External Returns to R&D",
        class = "btn-primary btn-lg",
        style = "white-space: normal; min-height: 84px; font-weight: 500;"
      ),
      shiny::actionButton(
        ns("go_marginal"), "Marginal Returns to R&D",
        class = "btn-primary btn-lg",
        style = "white-space: normal; min-height: 84px; font-weight: 500;"
      )
    ),

    # Scrolling message ticker (marquee) beneath the buttons. Each message
    # lives in welcome_ticker_messages (declared below); horizontally
    # scrolls right-to-left via pure CSS so no JS timer is needed.
    shiny::tags$style(shiny::HTML("
      .ise-ticker {
        margin-top: 42px;
        max-width: 720px;
        margin-left: auto;
        margin-right: auto;
        overflow: hidden;
        white-space: nowrap;
        border-top: 1px solid #e4e4e4;
        border-bottom: 1px solid #e4e4e4;
        padding: 10px 0;
        color: #555;
        font-size: 0.95rem;
        font-style: italic;
      }
      .ise-ticker-track {
        display: inline-block;
        padding-left: 100%;
        animation: ise-ticker-scroll 28s linear infinite;
      }
      .ise-ticker-track:hover { animation-play-state: paused; }
      .ise-ticker-msg { padding: 0 60px; }
      @keyframes ise-ticker-scroll {
        0%   { transform: translateX(0); }
        100% { transform: translateX(-100%); }
      }
    ")),
    shiny::div(
      class = "ise-ticker",
      shiny::div(
        class = "ise-ticker-track",
        lapply(welcome_ticker_messages(), function(msg) {
          shiny::span(class = "ise-ticker-msg", msg)
        })
      )
    )
  )
  )
  )
}

# Messages shown in the welcome-page ticker. Prepend newer entries so they
# appear first in the scroll. Keep entries short (~1 line).
# Uses `n_docdbs_total` precomputed in data-raw/02-build-app-sysdata.R —
# falls back to a silent drop if sysdata hasn't been rebuilt yet.
welcome_ticker_messages <- function() {
  msgs <- "Now based on innovation data from 2013 to 2022"
  if (exists("n_docdbs_total") && is.finite(n_docdbs_total) &&
      n_docdbs_total > 0) {
    n_m <- n_docdbs_total / 1e6
    fmt <- if (n_m >= 10) sprintf("%.0f", n_m) else sprintf("%.1f", n_m)
    msgs <- c(
      sprintf("Drawing on data from %s million innovations", fmt),
      msgs
    )
  }
  msgs
}

#' Preset query strings for the welcome-page metric buttons.
#'
#' Each string is the query portion of a Shiny bookmark URL (everything
#' after the '?' — including the leading `_inputs_` marker). Navigating
#' `window.location.search` to one of these triggers Shiny's bookmark-
#' restore logic (enabled in runAppPackage.R via `enableBookmarking = "url"`)
#' which sets every listed input to the given value. The URL is relative,
#' so it works on any host without changes.
#'
#' @keywords internal
welcome_presets <- list(
  # Average Spillovers: Country Explorer / Value flows by Country,
  # ev_global with LMIC + All-countries comparators.
  spillovers = paste0(
    "?_inputs_",
    "&navbar_page=%22Country%20Explorer%22",
    "&country-inner_tabs=%22Value%20flows%20by%20Country%22",
    "&country-country=%5B%22LMICs%20(excl.%20China)%22%2C%22All%20countries%22%5D",
    "&country-firm=%22No%20firm%20filter%22",
    "&country-techs=%22All%20innovations%22",
    "&country-toflow=%22ev_global%22",
    "&country-tech_categories_plot1=%5B%22Green%20Technology%22%2C%22AI%22%5D",
    "&country-widthscale=%22log%22",
    "&country-display_mode=%22confidence%22",
    "&country-top_n_ids=10",
    "&country-topn=20",
    "&country-mininno=50"
  ),
  # Internal and External Returns to R&D: same layout, av_global instead.
  returns = paste0(
    "?_inputs_",
    "&navbar_page=%22Country%20Explorer%22",
    "&country-inner_tabs=%22Value%20flows%20by%20Country%22",
    "&country-country=%5B%22LMICs%20(excl.%20China)%22%2C%22All%20countries%22%5D",
    "&country-firm=%22No%20firm%20filter%22",
    "&country-techs=%22All%20innovations%22",
    "&country-toflow=%22av_global%22",
    "&country-tech_categories_plot1=%5B%22Green%20Technology%22%2C%22AI%22%5D",
    "&country-widthscale=%22log%22",
    "&country-display_mode=%22confidence%22",
    "&country-top_n_ids=10",
    "&country-topn=20",
    "&country-mininno=50"
  ),
  # Marginal Returns to R&D: same layout, is_global instead.
  marginal = paste0(
    "?_inputs_",
    "&navbar_page=%22Country%20Explorer%22",
    "&country-inner_tabs=%22Value%20flows%20by%20Country%22",
    "&country-country=%5B%22LMICs%20(excl.%20China)%22%2C%22All%20countries%22%5D",
    "&country-firm=%22No%20firm%20filter%22",
    "&country-techs=%22All%20innovations%22",
    "&country-toflow=%22is_global%22",
    "&country-tech_categories_plot1=%5B%22Green%20Technology%22%2C%22AI%22%5D",
    "&country-widthscale=%22log%22",
    "&country-display_mode=%22confidence%22",
    "&country-top_n_ids=10",
    "&country-topn=20",
    "&country-mininno=50"
  ),
  # Revealed Technological Advantage: RTA inner tab, focused on agrifood,
  # is_global toflow, lower all-innovation threshold (25) for RTA scatter.
  rta = paste0(
    "?_inputs_",
    "&navbar_page=%22Country%20Explorer%22",
    "&country-inner_tabs=%22Revealed%20Technological%20Advantage%22",
    "&country-country=%5B%22LMICs%20(excl.%20China)%22%2C%22All%20countries%22%5D",
    "&country-firm=%22No%20firm%20filter%22",
    "&country-techs=%22Any%20Agriculture%20%26%20Food%20technology%22",
    "&country-toflow=%22is_global%22",
    "&country-tech_categories_plot1=%5B%22Green%20Technology%22%2C%22AI%22%5D",
    "&country-widthscale=%22log%22",
    "&country-display_mode=%22confidence%22",
    "&country-top_n_ids=10",
    "&country-topn=20",
    "&country-mininno=50",
    "&country-topn_rta=20",
    "&country-bottomn_rta=0",
    "&country-mininno_rta=0",
    "&country-minallinnos_rta=25"
  )
)

#' Parse a Shiny bookmark-style query string into a named list of inputs.
#'
#' Handles '?_inputs_&key1=val1&key2=val2' format. Values are URL-decoded
#' then JSON-decoded (Shiny bookmarks wrap scalars in quotes and arrays
#' in JSON), so `%22foo%22` becomes `"foo"` and
#' `%5B%22a%22%2C%22b%22%5D` becomes `c("a", "b")`.
#'
#' @keywords internal
parse_preset_query <- function(query) {
  q <- sub("^\\?", "", query)
  q <- sub("^_inputs_&?", "", q)
  pairs <- strsplit(q, "&", fixed = TRUE)[[1]]
  out <- list()
  for (p in pairs) {
    if (!nzchar(p)) next
    idx <- regexpr("=", p, fixed = TRUE)
    if (idx <= 0) next
    key <- utils::URLdecode(substr(p, 1, idx - 1))
    raw <- utils::URLdecode(substr(p, idx + 1, nchar(p)))
    val <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = TRUE),
      error = function(e) raw
    )
    out[[key]] <- val
  }
  out
}

#' Apply a parsed preset to the current Shiny session without reloading.
#'
#' - Tab-panel inputs (`navbar_page`, `*-inner_tabs`) are switched via
#'   `bslib::nav_select`.
#' - Every other input is updated by sending a generic `{value: ...}`
#'   message through `session$sendInputMessage`, which every standard Shiny
#'   input binding (selectize, numeric, radio, checkbox, text, date, ...)
#'   understands — same mechanism Shiny's own `update*Input` functions use.
#'
#' @keywords internal
apply_preset <- function(preset, session) {
  # Drop ephemeral bookmark keys that we don't want to replay
  noise <- c("waiter_shown", "waiter_hidden",
             grep("_hovered$", names(preset), value = TRUE),
             grep("^plotly_",   names(preset), value = TRUE),
             grep("^\\.clientValue-", names(preset), value = TRUE))
  preset <- preset[setdiff(names(preset), noise)]

  # Tab/navbar switches first
  tab_keys <- intersect(
    names(preset),
    c("navbar_page", grep("-inner_tabs$", names(preset), value = TRUE))
  )
  for (k in tab_keys) {
    bslib::nav_select(id = k, selected = preset[[k]], session = session)
  }

  # Remaining inputs — generic message
  for (k in setdiff(names(preset), tab_keys)) {
    session$sendInputMessage(k, list(value = preset[[k]]))
  }
  invisible()
}

#' Server-side wiring for the welcome page.
#'
#' @param id Module namespace id (string).
#' @param parent_session The top-level Shiny session — used to switch tabs
#'   and update inputs that live in other (namespaced) modules.
#' @keywords internal
welcome_module_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {

    # Apply a preset in place: no reload, no flash — just updated inputs.
    go_preset <- function(query) {
      apply_preset(parse_preset_query(query), parent_session)
    }

    shiny::observeEvent(input$go_rta,        go_preset(welcome_presets$rta))
    shiny::observeEvent(input$go_spillovers, go_preset(welcome_presets$spillovers))
    shiny::observeEvent(input$go_returns,    go_preset(welcome_presets$returns))
    shiny::observeEvent(input$go_marginal,   go_preset(welcome_presets$marginal))

    # Random-rotating HiGGlobe background. Re-rolls every time the parent
    # navbar tab input changes, so each visit to the welcome page lands on
    # a different image from inst/insights_html/figures/.
    output$bg_css <- shiny::renderUI({
      parent_session$input$navbar_page  # take dependency to re-roll on tabs
      url <- welcome_random_bg_url()
      if (is.null(url)) return(NULL)
      shiny::tags$style(shiny::HTML(sprintf(
        ".ise-welcome-bg { background-image: url('%s'); }", url
      )))
    })
  })
}

#' Master on/off switch for the random rotating welcome background.
#'
#' Flip to `TRUE` once a curated set of HiGGlobe pictures is in
#' `insights/figures/`. While `FALSE`, `welcome_random_bg_url()` short-
#' circuits to NULL — the welcome page falls back to a plain white
#' background and the heading-card / button layout are unchanged. Leaving
#' all the rotation code in place so flipping this back on is a one-line
#' change.
#' @keywords internal
WELCOME_RANDOM_BG_ENABLED <- FALSE

#' List the rotating welcome backgrounds available to this app build, and
#' pick one URL at random. Returns `NULL` if disabled (see
#' `WELCOME_RANDOM_BG_ENABLED`) or if no images are bundled.
#'
#' Looks under `inst/insights_html/figures/` (mirrored there by
#' `insights/render_all.R`). Anything `*.png/jpg/jpeg/webp/svg` qualifies.
#' @keywords internal
welcome_random_bg_url <- function() {
  if (!isTRUE(WELCOME_RANDOM_BG_ENABLED)) return(NULL)
  fig_dir <- system.file("insights_html", "figures",
                         package = "innovationStrategyExplorer")
  if (!nzchar(fig_dir) || !dir.exists(fig_dir)) return(NULL)
  files <- list.files(fig_dir,
                      pattern = "\\.(png|jpe?g|webp|svg)$",
                      ignore.case = TRUE)
  if (!length(files)) return(NULL)
  paste0("insights/figures/", sample(files, 1))
}
