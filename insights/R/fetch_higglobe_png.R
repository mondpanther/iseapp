# insights/R/fetch_higglobe_png.R
#
# Capture HiGGlobe panel screenshots from the deployed Innovation Strategy
# Explorer Shiny app via headless Chrome (the {chromote} package).
#
# Strategy: lean on the *module's own* URL-driven auto-init + auto-Generate
# mechanism. Appending `&step=N` to a HiGGlobe bookmark URL causes
# server.R + module_hglobe.R to:
#   1. set the "Add Generations" input to N
#   2. stash higglobe_run=1 + higglobe_gen=N in session$userData$restore_params
#   3. (delay 1200 ms) auto-click "Initiate Innovation"  -> seeds gen 0
#   4. once gen 0 lands, (delay 400 ms) auto-click "Generate"  -> gens 1..N
# This is the same code path the app uses for its own deep-links and is
# resilient to bslib::input_task_button quirks that previously caused
# synthetic chromote clicks to silently no-op.
#
# We then poll the next_step spinner and the leaflet marker count to
# detect completion before capturing the leaflet container as PNG.

#' Fetch one or more HiGGlobe panel screenshots
#'
#' @param specs A list of named lists. Each element must contain:
#'   * `name`         — base filename (no extension); output is `<name>.png`
#'   * `country`      — value for `hglobe-country`
#'   * `techs`        — value for `hglobe-techs`
#'   * `toflow`       — value for `hglobe-toflow` (default `ev_global`)
#'   * `granted_only` — optional logical (default `FALSE`)
#'   * `multifam_only`— optional logical (default `FALSE`)
#'   * `gen0_n`       — optional gen-0 sample size (default 100)
#'   * `n_generations`— optional follow-on generation count (default 4)
#' @param out_dir Directory where PNGs will be written.
#' @param app_url Base URL of the deployed app.
#' @param viewport `c(width_px, height_px)` for headless Chrome.
#' @param cold_start_wait Seconds after the FIRST navigation while the
#'   Shiny session loads sysdata. Subsequent panels reuse the warm session.
#' @param min_after_load_wait Minimum seconds to wait after each panel's
#'   navigate (gen 0 seed time before the spinner-poll loop kicks in).
#' @param after_generate_wait_max Hard ceiling on spinner-poll wait, secs.
#' @param post_render_settle Extra settle time after spinner clears, secs.
#' @param fetch_timeout_s CDP-call timeout passed to chromote.
#' @param overwrite If `FALSE` (default) skip specs whose PNG already exists.
fetch_higglobe_pngs <- function(specs,
                                out_dir                 = "insights/figures",
                                app_url                 = "https://mondpanther-iseapp2.share.connect.posit.cloud/",
                                viewport                = c(1600, 1000),
                                cold_start_wait         = 60,
                                min_after_load_wait     = 14,
                                after_generate_wait_max = 300,
                                post_render_settle      = 6,
                                fetch_timeout_s         = 240,
                                overwrite               = FALSE) {

  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("The 'chromote' package is required. Install with install.packages('chromote').")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required.")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  needed <- if (overwrite) specs else
    Filter(function(s) !file.exists(file.path(out_dir, paste0(s$name, ".png"))),
           specs)
  if (length(needed) == 0) {
    message("All HiGGlobe PNGs already present in ", out_dir,
            ". Nothing to fetch.")
    return(invisible(character(0)))
  }

  # Single-string selectize values (e.g. firm = "No firm filter") — quoted.
  q <- function(x) utils::URLencode(paste0('"', x, '"'), reserved = TRUE)
  # Multi-select selectize values (e.g. country, techs are `multiple=TRUE`)
  # — must be encoded as a JSON array so Shiny's bookmark restore actually
  # populates the input. A scalar quoted string is silently ignored, which
  # leaves the dropdown at its UI default and the auto-init loads the
  # wrong (or no) data.
  qarr <- function(x) utils::URLencode(
    jsonlite::toJSON(as.character(x), auto_unbox = FALSE),
    reserved = TRUE
  )
  `%||%` <- function(a, b) if (is.null(a)) b else a
  spec_granted   <- function(s) isTRUE(s$granted_only  %||% FALSE)
  spec_multifam  <- function(s) isTRUE(s$multifam_only %||% FALSE)
  spec_toflow    <- function(s) s$toflow %||% "ev_global"
  spec_gen0_n    <- function(s) as.integer(s$gen0_n %||% 100L)
  spec_n_gens    <- function(s) as.integer(s$n_generations %||% 4L)

  # Bookmark URL + `&step=N` to trigger the module's URL-driven auto-init
  # and auto-Generate code path.
  build_url <- function(s) {
    paste0(
      app_url, "?_inputs_",
      "&navbar_page=", q("HiGGlobe"),
      "&hglobe-country=",        qarr(s$country),
      "&hglobe-firm=",           q("No firm filter"),
      "&hglobe-techs=",          qarr(s$techs),
      "&hglobe-city=",           q("No city filter"),
      "&hglobe-toflow=",         q(spec_toflow(s)),
      "&hglobe-granted_only=",   tolower(as.character(spec_granted(s))),
      "&hglobe-multifam_only=",  tolower(as.character(spec_multifam(s))),
      "&hglobe-include_fallback=false",
      "&hglobe-gen0_select_mode=", q("Random"),
      "&hglobe-gen0_unit_mode=",   q("Number"),
      "&hglobe-gen0_sample_val=", spec_gen0_n(s),
      "&hglobe-edge_select_mode=", q("Random"),
      "&hglobe-edge_unit_mode=",   q("Percent"),
      "&hglobe-edge_sample_val=10",
      "&hglobe-add_generations=", spec_n_gens(s),
      "&hglobe-show_gen_0=true&hglobe-show_gen_1=true",
      "&hglobe-show_gen_2=true&hglobe-show_gen_3=true",
      "&hglobe-show_gen_4=true",
      "&hglobe-sidebar=false",
      "&step=", spec_n_gens(s)        # <-- the trigger
    )
  }

  message("HiGGlobe screenshotter: ", length(needed), " panel(s) to fetch.")
  prev_timeout <- tryCatch(chromote::default_timeout(),
                           error = function(e) NULL)
  try(chromote::default_timeout(fetch_timeout_s), silent = TRUE)
  on.exit({
    if (!is.null(prev_timeout))
      try(chromote::default_timeout(prev_timeout), silent = TRUE)
  }, add = TRUE)

  # Per-panel chromote session. Long-lived single sessions tend to die
  # mid-run on a slow Shiny app ("websocketpp End of File"), and one dead
  # session would otherwise kill the whole loop. A fresh Chrome per panel
  # costs ~2 s of startup but is dramatically more reliable.
  capture_one <- function(spec) {
    out_path <- file.path(out_dir, paste0(spec$name, ".png"))
    url      <- build_url(spec)

    b <- chromote::ChromoteSession$new(width  = viewport[1],
                                       height = viewport[2])
    on.exit(try(b$close(), silent = TRUE), add = TRUE)

    eval_js <- function(expr) {
      res <- b$Runtime$evaluate(expr, returnByValue = TRUE)
      if (!is.null(res$exceptionDetails)) {
        stop("Chromote eval error: ",
             res$exceptionDetails$exception$description)
      }
      res$result$value
    }

    count_markers <- function() {
      suppressWarnings(as.integer(eval_js(
        "(function(){
           var c = document.querySelectorAll(
             '.leaflet-marker-icon, path.leaflet-interactive'
           );
           return c ? c.length : 0;
         })()"
      )))
    }

    spinner_state <- function(spinner_id) {
      eval_js(sprintf(
        "(function(){
           var el = document.getElementById('%s');
           if (!el) return 'absent';
           var s = window.getComputedStyle(el);
           return s.display === 'none' ? 'hidden' : 'visible';
         })()", spinner_id))
    }

    # Wait until the spinner is hidden AND the marker count has been
    # stable for `stable_polls` consecutive 1-Hz polls. Times out at
    # `timeout` seconds.
    wait_until_idle <- function(spinner_id, timeout, stable_polls = 4L) {
      deadline <- Sys.time() + timeout
      last_n   <- -1L
      stable   <- 0L
      seen_visible <- FALSE
      repeat {
        vis <- spinner_state(spinner_id)
        n   <- count_markers()
        if (identical(vis, "visible")) seen_visible <- TRUE
        idle_now <- identical(vis, "hidden") || identical(vis, "absent")
        if (idle_now) {
          if (n == last_n && n > 0L) stable <- stable + 1L else stable <- 0L
          if (stable >= stable_polls && seen_visible) return(invisible(TRUE))
          if (stable >= (stable_polls + 4L) && !seen_visible)
            return(invisible(TRUE))
        } else {
          stable <- 0L
        }
        if (Sys.time() > deadline) {
          warning(sprintf(
            "Timed out waiting for spinner '%s' to clear (markers: %d).",
            spinner_id, n), call. = FALSE)
          return(invisible(FALSE))
        }
        last_n <- n
        Sys.sleep(1)
      }
    }

    b$Page$navigate(url)
    b$Page$loadEventFired()
    Sys.sleep(cold_start_wait)
    # Extra grace: the module schedules render_map +1.2s after URL parse,
    # then next_step ~+0.4s after gen 0 is seeded.
    Sys.sleep(min_after_load_wait)

    wait_until_idle("hglobe-next_step_spinner",
                    timeout = after_generate_wait_max)
    Sys.sleep(post_render_settle)

    n_after <- count_markers()
    message(sprintf("  markers on map at capture time: %d", n_after))

    if (is.na(n_after) || n_after < 10L) {
      warning(sprintf(paste0(
        "Panel '%s' came back with only %s markers; not saving (the ",
        "next render will retry). Try raising `min_after_load_wait` or ",
        "`after_generate_wait_max` in the autofetch call."),
        spec$name, ifelse(is.na(n_after), "NA", as.character(n_after))),
        call. = FALSE)
      return(NULL)
    }

    rect <- tryCatch({
      eval_js(
        "(function(){
           var el = document.querySelector('.leaflet-container')
                  || document.querySelector('#hglobe-map');
           if (!el) return null;
           var r = el.getBoundingClientRect();
           return {x: Math.round(r.left), y: Math.round(r.top),
                   width: Math.round(r.width),
                   height: Math.round(r.height), scale: 1};
         })()"
      )
    }, error = function(e) NULL)

    args <- list(format = "png")
    if (!is.null(rect) && is.list(rect) &&
        all(c("x", "y", "width", "height") %in% names(rect)) &&
        rect$width > 100 && rect$height > 100) {
      args$clip <- rect
    }
    shot <- do.call(b$Page$captureScreenshot, args)
    writeBin(jsonlite::base64_dec(shot$data), out_path)
    message("  -> wrote ", out_path,
            sprintf(" (%d KB)", as.integer(file.info(out_path)$size / 1024)))
    out_path
  }

  written <- character(0)
  for (i in seq_along(needed)) {
    spec <- needed[[i]]
    message(sprintf(
      "[%d/%d] HiGGlobe '%s'  country='%s'  techs='%s'  gen0_n=%d  +%dgens",
      i, length(needed), spec$name, spec$country, spec$techs,
      spec_gen0_n(spec), spec_n_gens(spec)))
    res <- tryCatch(capture_one(spec),
                    error = function(e) {
                      warning(sprintf(
                        "Panel '%s' failed: %s. Continuing with the rest.",
                        spec$name, conditionMessage(e)),
                        call. = FALSE)
                      NULL
                    })
    if (!is.null(res)) written <- c(written, res)
  }

  invisible(written)
}
