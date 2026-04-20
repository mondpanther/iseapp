#' Build a short human-readable version identifier for the app.
#'
#' Resolution order (first non-empty wins):
#'   1. `inst/VERSION` — a one-line file written by `data-raw/write_version.R`
#'      before deployment. This is the reliable source on shinyapps.io /
#'      Posit Connect where git isn't available.
#'   2. Live `git` in a development checkout (branch + short SHA + dirty flag).
#'   3. The package DESCRIPTION version as a last-resort fallback.
#'
#' The returned string is safe to drop directly into the UI (no HTML).
#'
#' @noRd
app_version <- function() {
  # 1. VERSION file shipped in inst/
  vfile <- tryCatch(
    system.file("VERSION", package = "innovationStrategyExplorer"),
    error = function(e) ""
  )
  if (nzchar(vfile) && file.exists(vfile)) {
    v <- readLines(vfile, warn = FALSE)
    v <- v[nzchar(v)]
    if (length(v)) return(v[1])
  }

  # 2. Live git (development path)
  git_out <- tryCatch({
    branch <- suppressWarnings(system(
      "git rev-parse --abbrev-ref HEAD",
      intern = TRUE, ignore.stderr = TRUE
    ))
    sha <- suppressWarnings(system(
      "git rev-parse --short HEAD",
      intern = TRUE, ignore.stderr = TRUE
    ))
    dirty <- length(suppressWarnings(system(
      "git status --porcelain",
      intern = TRUE, ignore.stderr = TRUE
    ))) > 0
    if (length(branch) == 1L && length(sha) == 1L &&
        nzchar(branch) && nzchar(sha)) {
      paste0(branch, "@", sha, if (dirty) " *dirty",
             " · built ", format(Sys.Date(), "%Y-%m-%d"))
    } else NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(git_out)) return(git_out)

  # 3. DESCRIPTION
  pv <- tryCatch(
    as.character(utils::packageVersion("innovationStrategyExplorer")),
    error = function(e) "unknown"
  )
  paste0("v", pv)
}
