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
  rmd_files = NULL
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install rmarkdown first: install.packages('rmarkdown')")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(rmd_files)) {
    rmd_files <- list.files(src_dir, pattern = "\\.Rmd$",
                            full.names = TRUE, recursive = FALSE)
  }
  if (length(rmd_files) == 0) {
    message("No .Rmd files found in ", src_dir)
    return(invisible(character(0)))
  }

  written <- character(0)
  for (rmd in rmd_files) {
    message("\n=== Rendering ", rmd, " ===")
    html_out <- rmarkdown::render(
      rmd,
      output_format = rmarkdown::html_document(self_contained = TRUE),
      quiet         = FALSE,
      envir         = new.env()
    )
    dest <- file.path(out_dir, basename(html_out))
    file.copy(html_out, dest, overwrite = TRUE)
    message("  -> copied to ", dest)
    written <- c(written, dest)
  }

  message("\nRendered ", length(written), " note(s) into ", out_dir, ".\n",
          "Deploy the app (Posit Publisher) to make them reachable at /insights/<file>.html")
  invisible(written)
}

if (sys.nframe() == 0L) {
  render_insights()
}
