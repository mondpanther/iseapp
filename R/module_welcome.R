#' Welcome / landing-page Shiny module
#'
#' Shown as the default tab when the app opens without URL parameters.
#' Heading in the middle, four large metric buttons below that jump to the
#' Country Explorer tab and pre-select the matching value flow.
#'
#' @keywords internal
welcome_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "
      max-width: 900px;
      margin: 0 auto;
      padding: 80px 20px 60px 20px;
      text-align: center;
    ",
    shiny::h1(
      "Welcome to the Innovation Strategy Explorer",
      style = "
        font-size: 2.1rem;
        font-weight: 600;
        margin-bottom: 40px;
        line-height: 1.25;
      "
    ),
    shiny::p(
      "Choose a metric to start exploring:",
      style = "color: #555; margin-bottom: 30px;"
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
        lapply(welcome_ticker_messages, function(msg) {
          shiny::span(class = "ise-ticker-msg", msg)
        })
      )
    )
  )
}

# Messages shown in the welcome-page ticker. Prepend newer entries so they
# appear first in the scroll. Keep entries short (~1 line).
welcome_ticker_messages <- c(
  "Now based on innovation data from 2013 to 2022"
)

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
  })
}
