#' Landing module UI
#'
#' Returns the HTML content used as the waiter overlay during app startup.
#' This is passed as the `html` argument to `waiter::Waiter$new()` in the server.
#'
#' @keywords internal
landing_ui_content <- function() {
  shiny::tagList(
    shiny::tags$div(
      style = paste(
        "display: flex;",
        "flex-direction: column;",
        "align-items: center;",
        "justify-content: center;",
        "height: 100vh;",
        "gap: 24px;"
      ),
      shiny::tags$h1(
        "Innovation Strategy Explorer",
        style = "color: #ffffff; font-weight: 600; font-size: 2rem; margin: 0;"
      ),
      waiter::spin_fading_circles()
    )
  )
}

#' Landing module Server
#'
#' Fires a one-time prefetch of the default `fallback_by_tech` query to warm
#' the DuckDB/S3 connection. Dismisses the waiter when complete.
#'
#' @param id the ID of the module
#' @param waiter a `waiter::Waiter` R6 object to dismiss on completion
#' @param con a DuckDB connection
#'
#' @keywords internal
landing_module_server <- function(id, waiter, con) {
  shiny::moduleServer(id, function(input, output, session) {

      ready <- shiny::reactiveVal(FALSE)

      shiny::observe({
        # Hardcoded defaults matching country module sidebar
        toflow             <- "is_global"
        firm               <- "All"
        selected_countries <- expand_country_selection("All countries")
        country_sql        <- paste0("'", selected_countries, "'", collapse = ", ")
        firm_clause        <- ""
        tech_filters       <- build_tech_filter_v2(c("AI", "Green Technology"))

        DBI::dbGetQuery(con, sql_country_tech_combined_v2(toflow, country_sql, tech_filters, firm_clause))

        ready(TRUE)
        waiter$hide()
      }) |> shiny::bindEvent(session$clientData$url_protocol, once = TRUE)

      shiny::reactive(ready())

    }
  )
}