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
  written  <- character(0)
  for (rmd in rmd_files) {
    expected_html <- sub("\\.[Rr]md$", ".html", rmd)
    have_html <- file.exists(expected_html)

    if (have_html && !isTRUE(force)) {
      # Some of our notes (innocity, patent_cluster_compare, ...) take
      # tens of minutes to (re-)knit because they hit live external
      # APIs (EPO OPS, OpenAI) and bake screenshots from chromote.
      # Re-rendering them on every deploy is wasteful when the
      # author has already produced a fresh HTML alongside the .Rmd.
      # If a sibling .html exists, just copy it.
      message("\n=== Reusing existing HTML for ", basename(rmd), " ===")
      message("  source: ", expected_html)
      html_out <- expected_html
      copied   <- copied + 1L
    } else {
      message("\n=== Rendering ", rmd, " ===")
      if (have_html && isTRUE(force)) {
        message("  (force = TRUE: re-rendering despite sibling HTML)")
      }
      html_out <- rmarkdown::render(
        rmd,
        output_format = rmarkdown::html_document(self_contained = TRUE),
        quiet         = FALSE,
        envir         = new.env()
      )
      rendered <- rendered + 1L
    }

    dest <- file.path(out_dir, basename(html_out))
    file.copy(html_out, dest, overwrite = TRUE)
    message("  -> copied to ", dest)
    written <- c(written, dest)
  }

  message(sprintf(
    "\nDone: %d rendered, %d copied from existing HTML. Output dir: %s",
    rendered, copied, out_dir))
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
