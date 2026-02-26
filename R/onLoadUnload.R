#' On Package Load
#'
#' Function to run on package load, mainly adding resource paths
#' for static assets
#'
#' @param libname the library name
#' @param pkgname the package name
#'
#' @importFrom sass output_template sass sass_file sass_options
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(libname, pkgname) {

  cfg <- config::get("aws", file = system.file("config.yml", package = pkgname))
  
  Sys.setenv(
    AWS_ACCESS_KEY_ID = cfg$key,
    AWS_SECRET_ACCESS_KEY = cfg$secret,
    AWS_DEFAULT_REGION = "us-east-2",
    S3_BUCKET = "iseapp-database"
  )

  # Images
  shiny::addResourcePath(
    prefix = pkgname,
    directoryPath = system.file(
      "www",
      package = pkgname#,
      # lib.loc = libname
    )
  )

  # CSS
  shiny::addResourcePath(
    prefix = pkgname,
    directoryPath = system.file(
      "www",
      package = pkgname#,
      # lib.loc = libname
    )
  )

  # Javascript
  shiny::addResourcePath(
    prefix = pkgname,
    directoryPath = system.file(
      "www",
      package = pkgname
    )
  )

  # Bundle SASS into CSS
  sass_output_template <- sass::output_template(
    basename = "custom-styles",
    dirname = "css",
    fileext = ".min.css",
    path = system.file("www", package = pkgname)
  )

  sass::sass(
    sass::sass_file(system.file("www", "scss", "custom-styles.scss", package = pkgname)),
    output = sass_output_template(suffix = ""),
    options = sass::sass_options(
      output_style = "compressed",
      include_path = list.files(system.file("www", "scss", package = pkgname), full.names = TRUE),
      # source_map_embed = TRUE
      source_map_embed = FALSE
    ),
    cache = FALSE
    # cache = system.file("app_cache", "sass", package = pkgname),
    # cache_key_extra = file.mtime(system.file("www", "css", "custom-styles.min.css", package = pkgname))
  )

  invisible()
}


#' On Package Unload
#'
#' Function to remove resource paths on package unload (e.g. static assets)
#'
#' @param libname the library name
#' @param pkgname the package name
#'
#' @noRd
.onUnload <- function(libname, pkgname) {
  # On package unload
}
