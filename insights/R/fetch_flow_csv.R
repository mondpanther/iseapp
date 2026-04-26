# insights/R/fetch_flow_csv.R
#
# Download Country-Explorer "Value flows by Technology" CSVs from the live
# Innovation Strategy Explorer Shiny app using headless Chrome via the
# {chromote} package. No database access required — only what any public
# user of the app can obtain from the Download CSV button.

#' Fetch one or more Value-flows-by-Technology CSVs
#'
#' @param specs A list of named lists. Each element must contain:
#'   * `name`         — file tag; output will be `flow_<name>.csv`
#'   * `country`      — value for the `country-country` selectize input
#'   * `toflow`       — value for the `country-toflow` select input
#'   * `granted_only` — optional logical (default `TRUE`) for the
#'                       "Granted families only" checkbox
#'   * `multifam_only`— optional logical (default `FALSE`) for the
#'                       "Multi-application families only" checkbox
#' @param out_dir Directory where CSVs will be written
#' @param app_url Base URL of the deployed app
#' @param tech_categories Character vector for `country-tech_categories_plot1`
#' @param cold_start_wait Seconds to wait after initial navigation (cold boot)
#' @param reflow_wait Seconds to wait after changing filters before fetching
#' @param overwrite If FALSE, skip specs whose CSV already exists
fetch_flow_csvs <- function(specs,
                            out_dir,
                            app_url  = "https://mondpanther-iseapp2.share.connect.posit.cloud/",
                            tech_categories = c(
                              "All innovations",
                              "Green Technology",
                              "Battery Technology",
                              "AI", "Green Transport",
                              "Green Manufacturing", "GHG Capture",
                              "Green Energy", "Adaptation", "Green Housing",
                              "Circular Economy", "Green ICT",
                              "Green Agriculture",
                              "Any Agriculture & Food technology"
                            ),
                            cold_start_wait = 30,
                            reflow_wait     = 20,
                            fetch_timeout_s = 120,
                            overwrite       = FALSE) {

  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("The 'chromote' package is required. Install with install.packages('chromote').")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  needed <- if (overwrite) specs else
    Filter(function(s) !file.exists(file.path(out_dir, paste0("flow_", s$name, ".csv"))),
           specs)
  if (length(needed) == 0) {
    message("All CSVs already present in ", out_dir, ". Nothing to fetch.")
    return(invisible(character(0)))
  }

  q <- function(x) utils::URLencode(paste0('"', x, '"'), reserved = TRUE)
  tech_arr <- paste0(
    "%5B",
    paste0(vapply(tech_categories,
                  function(t) paste0("%22", utils::URLencode(t, reserved = TRUE), "%22"),
                  character(1)),
           collapse = "%2C"),
    "%5D"
  )

  `%||%` <- function(a, b) if (is.null(a)) b else a
  spec_granted  <- function(s) isTRUE(s$granted_only  %||% TRUE)
  spec_multifam <- function(s) isTRUE(s$multifam_only %||% FALSE)

  initial_url <- paste0(
    app_url,
    "?_inputs_",
    "&navbar_page=", q("Country Explorer"),
    "&country-inner_tabs=", q("Value flows by Technology"),
    "&country-country=", q(needed[[1]]$country),
    "&country-firm=", q("No firm filter"),
    "&country-toflow=", q(needed[[1]]$toflow),
    "&country-tech_categories_plot1=", tech_arr,
    "&country-granted_only=",   tolower(as.character(spec_granted(needed[[1]]))),
    "&country-multifam_only=",  tolower(as.character(spec_multifam(needed[[1]]))),
    "&country-widthscale=", q("log"),
    "&country-display_mode=", q("confidence"),
    "&country-top_n_ids=10"
  )

  message("Launching headless Chrome ...")
  # Bump chromote's default wait timeout for CDP responses BEFORE we
  # create the session, so Runtime.evaluate calls that take a while to
  # come back (e.g. the CSV download fetch when all 14 tech categories
  # are aggregated) don't trip the default 10s wait.
  prev_timeout <- tryCatch(chromote::default_timeout(),
                           error = function(e) NULL)
  try(chromote::default_timeout(fetch_timeout_s), silent = TRUE)
  on.exit({
    if (!is.null(prev_timeout))
      try(chromote::default_timeout(prev_timeout), silent = TRUE)
  }, add = TRUE)
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  message("Navigating to app (cold-start wait ", cold_start_wait, "s) ...")
  b$Page$navigate(initial_url)
  b$Page$loadEventFired()
  Sys.sleep(cold_start_wait)

  wait_for_download_href <- function(timeout = 30) {
    deadline <- Sys.time() + timeout
    repeat {
      v <- b$Runtime$evaluate(
        "(function(){var a=document.getElementById('country-dl_csv_avstrax_plot1');return a?a.href:'';})()",
        returnByValue = TRUE
      )$result$value
      if (isTRUE(grepl("/download/", v, fixed = TRUE))) return(invisible(TRUE))
      if (Sys.time() > deadline) stop("Timed out waiting for Download CSV link to populate.")
      Sys.sleep(1)
    }
  }

  set_input <- function(id, value_json) {
    b$Runtime$evaluate(sprintf(
      "Shiny.setInputValue('%s', %s, {priority:'event'})", id, value_json
    ))
  }

  fetch_current_csv <- function() {
    # The download fetch can take well over the default chromote 10 s
    # timeout when the SQL behind the CSV touches all 14 tech categories
    # at once. Pass an explicit timeout (in milliseconds) sized for the
    # slowest realistic case.
    res <- b$Runtime$evaluate(
      "fetch(document.getElementById('country-dl_csv_avstrax_plot1').href).then(r => r.text())",
      awaitPromise  = TRUE,
      returnByValue = TRUE,
      timeout       = fetch_timeout_s * 1000
    )
    if (!is.null(res$exceptionDetails)) {
      stop("Chromote fetch error: ",
           res$exceptionDetails$exception$description)
    }
    res$result$value
  }

  written <- character(0)
  for (i in seq_along(needed)) {
    spec <- needed[[i]]
    out_path <- file.path(out_dir, paste0("flow_", spec$name, ".csv"))

    if (i > 1) {
      message("Setting country='", spec$country, "', toflow='", spec$toflow,
              "', granted_only=", spec_granted(spec),
              ", multifam_only=", spec_multifam(spec), " ...")
      # country-country is a multi-select (selectizeInput multiple=TRUE) — pass an array
      set_input("country-country",
                jsonlite::toJSON(spec$country, auto_unbox = FALSE))
      set_input("country-toflow",
                jsonlite::toJSON(spec$toflow, auto_unbox = TRUE))
      set_input("country-granted_only",
                if (spec_granted(spec)) "true" else "false")
      set_input("country-multifam_only",
                if (spec_multifam(spec)) "true" else "false")
      Sys.sleep(reflow_wait)
    }
    wait_for_download_href()
    message("Fetching CSV for '", spec$name, "' ...")
    csv_text <- fetch_current_csv()
    writeLines(csv_text, out_path, useBytes = TRUE)
    message("  -> wrote ", out_path, " (", nchar(csv_text), " bytes)")
    written <- c(written, out_path)
  }

  invisible(written)
}
