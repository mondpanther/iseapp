# insights/render_all.R
#
# Render every .Rmd in insights/ to self-contained HTML and copy the output
# into inst/insights_html/ so the files get bundled with the Shiny app on
# the next Posit deploy and become reachable at:
#
#   https://<app-url>/insights/<note>.html
#
# Run from the project root:
#   source("insights/render_all.R")
#
# Prereqs:
#   install.packages(c("rmarkdown", "chromote"))
#   (chromote is only needed if a note has CSV caches missing from insights/data/)

render_insights <- function(
  src_dir   = "insights",
  out_dir   = file.path("inst", "insights_html"),
  rmd_files = NULL,
  force     = FALSE
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install rmarkdown first: install.packages('rmarkdown')")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(rmd_files)) {
    # Match both `.Rmd` and `.rmd` — innocity.rmd uses lowercase.
    rmd_files <- list.files(src_dir, pattern = "\\.[Rr]md$",
                            full.names = TRUE, recursive = FALSE)
  }
  if (length(rmd_files) == 0) {
    message("No .Rmd files found in ", src_dir)
    return(invisible(character(0)))
  }

  rendered <- 0L
  copied   <- 0L
  skipped  <- 0L
  written  <- character(0)
  for (rmd in rmd_files) {
    sibling_html <- sub("\\.[Rr]md$", ".html", rmd)
    bundle_html  <- file.path(
      out_dir, sub("\\.[Rr]md$", ".html", basename(rmd))
    )
    have_sibling <- file.exists(sibling_html)
    have_bundle  <- file.exists(bundle_html)

    if (have_sibling && !isTRUE(force)) {
      # Author has an authoritative sibling HTML next to the .Rmd —
      # copy it into the bundle (overwriting any older copy there).
      # Some notes (innocity, patent_cluster_compare, ...) take tens
      # of minutes to re-knit because they hit live APIs and bake
      # chromote screenshots, so we never re-render when a sibling
      # HTML is already present.
      message("\n=== Reusing sibling HTML for ", basename(rmd), " ===")
      message("  source: ", sibling_html)
      html_out <- sibling_html
      copied   <- copied + 1L
      dest <- file.path(out_dir, basename(html_out))
      file.copy(html_out, dest, overwrite = TRUE)
      message("  -> copied to ", dest)
      written <- c(written, dest)
    } else if (have_bundle && !isTRUE(force)) {
      # No sibling, but the bundle already has an HTML for this note.
      # Leave it in place — same "don't re-render slow notes on every
      # deploy" reasoning.
      message("\n=== Skipping ", basename(rmd),
              " (bundle already has HTML) ===")
      message("  bundle: ", bundle_html)
      skipped <- skipped + 1L
      written <- c(written, bundle_html)
    } else {
      message("\n=== Rendering ", rmd, " ===")
      if ((have_sibling || have_bundle) && isTRUE(force)) {
        message("  (force = TRUE: re-rendering despite existing HTML)")
      }
      html_out <- rmarkdown::render(
        rmd,
        output_format = rmarkdown::html_document(self_contained = TRUE),
        quiet         = FALSE,
        envir         = new.env()
      )
      rendered <- rendered + 1L
      dest <- file.path(out_dir, basename(html_out))
      file.copy(html_out, dest, overwrite = TRUE)
      message("  -> copied to ", dest)
      written <- c(written, dest)
    }
  }

  # ----- Figures -----
  # Mirror insights/figures/ into inst/insights_html/figures/ so the bundled
  # app can serve them at /insights/figures/<file>. Used by the welcome
  # page's randomly-rotating HiGGlobe background and by any rmd that
  # references images via the `figures/` relative path.
  fig_src <- file.path(src_dir, "figures")
  fig_dst <- file.path(out_dir, "figures")
  if (dir.exists(fig_src)) {
    dir.create(fig_dst, recursive = TRUE, showWarnings = FALSE)
    fig_files <- list.files(fig_src, full.names = TRUE,
                            pattern = "\\.(png|jpg|jpeg|webp|svg)$",
                            ignore.case = TRUE)
    if (length(fig_files)) {
      file.copy(fig_files, fig_dst, overwrite = TRUE)
      message(sprintf("Copied %d figure(s) -> %s",
                      length(fig_files), fig_dst))
    }
  }

  message(sprintf(
    "\nDone: %d rendered, %d copied from sibling, %d kept from bundle. Output dir: %s",
    rendered, copied, skipped, out_dir))
  message("Deploy the app (Posit Publisher) to make them reachable at ",
          "/insights/<file>.html")
  invisible(written)
}

# Run the renderer whenever this file is sourced or executed. If you want
# to pass custom arguments, call render_insights(...) explicitly after
# sourcing — the auto-call below is just the no-args default.
#
# To force a re-knit of every .Rmd (even those with sibling HTMLs), call:
#   render_insights(force = TRUE)
# To force one specific file:
#   render_insights(rmd_files = "insights/innocity.rmd", force = TRUE)
render_insights()
