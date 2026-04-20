# Writes inst/VERSION — a single-line file containing the current git branch,
# short commit SHA, dirty flag, and build date. The Shiny UI reads this at
# startup via app_version() so the "Version" line shows a reliable identifier
# even when the deployed app has no git available (shinyapps.io, Posit
# Connect, Docker image, etc.).
#
# Run before every deploy:
#   source("data-raw/write_version.R")
# Then bundle/deploy the app as usual; inst/VERSION is picked up automatically.

branch <- tryCatch(
  trimws(system("git rev-parse --abbrev-ref HEAD",
                intern = TRUE, ignore.stderr = TRUE)),
  error = function(e) "unknown"
)
sha <- tryCatch(
  trimws(system("git rev-parse --short HEAD",
                intern = TRUE, ignore.stderr = TRUE)),
  error = function(e) "unknown"
)
dirty <- tryCatch(
  length(system("git status --porcelain",
                intern = TRUE, ignore.stderr = TRUE)) > 0,
  error = function(e) FALSE
)

tag <- paste0(
  branch, "@", sha,
  if (isTRUE(dirty)) " *dirty",
  " \u00b7 built ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)

dir.create("inst", showWarnings = FALSE)
writeLines(tag, "inst/VERSION")
cat("Wrote inst/VERSION:\n  ", tag, "\n", sep = "")
