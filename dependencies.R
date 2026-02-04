#' Dependencies for ZRSA_Template1 UI Template
#'
#' @return Character vector of required packages
#' @noRd

get_zrsa_template1_dependencies <- function() {
  c(
    # Core Shiny stack
    "shiny",
    "bslib",           # Modern Bootstrap theming
    "htmltools",       # Better HTML generation
    
    # Styling
    "sass",            # CSS preprocessing
    
    # Development tools (always included)
    "pkgload",         # Load R packages without installing
    "devtools",        # Package development tools
    "usethis"          # Automate package and project setup tasks
  )
}