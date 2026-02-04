#' FIRST TIME SETUP: Run these commands in order
#' After that, just run the pkgload::load_all() and runAppPackage()
#' lines below to launch the app
#'
#' To use renv for dependency management in this project:
#' 1. Run install.packages("renv") to install renv if not already installed
#' 2. Run renv::init() to initialize renv (this also activates the project)
#' 3. Run renv::install() to install all packages listed in the DESCRIPTION file
#' 4. Run renv::snapshot() to create/update the lockfile
#'
#' For dependency updates:
#' Run renv::update()  # Update packages and snapshot
#'
#' For deployment or sharing:
#' Include renv.lock in version control
#' Recipients should run renv::restore() to recreate the exact package environment

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
runAppPackage()

#' Package development/testing workflow:
#' devtools::document()          # Generate/update documentation from roxygen comments
#' devtools::check()             # Comprehensive package validation
#' devtools::test()              # Run unit tests (if testthat is configured)
#'
#' Optional development helpers:
#' devtools::build()             # Build source package for distribution
#' devtools::load_all()          # Alternative to pkgload::load_all() above
#' devtools::install()           # Install the package locally for testing
