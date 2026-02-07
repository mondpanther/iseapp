#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @importFrom bslib bs_theme page_navbar
#' @importFrom pkgload pkg_name
#' @importFrom shiny h1 tabPanel tags
#'
#' @keywords internal
ui <- function(req) {
  bslib::page_navbar(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cerulean"
    ),
    header = list(
      
      shinyjs::useShinyjs(),
      prompter::use_prompt(),
      sever::useSever(),
      waiter::use_waiter(),
      waiter::autoWaiter(color = "#ffffff", fadeout = 10),

      shiny::tags$head(
        # shiny::tags$link(rel = "icon", href = file.path(pkgload::pkg_name(), "img/ifc_logo.svg")),
        shiny::tags$link(href = file.path(pkgload::pkg_name(), "css/custom-styles.min.css"), rel = "stylesheet", type = "text/css"),
        shiny::tags$script(src = file.path(pkgload::pkg_name(), "js/custom-js.js"))
      ),

      tags$script(HTML("
        var urlUpdateTimeout;
        var isInitialLoad = true;
        
        // Build query string from ALL current inputs
        function buildQueryString() {
          if (!Shiny || !Shiny.shinyapp || !Shiny.shinyapp.$inputValues) {
            return '';
          }
          
          var inputs = Shiny.shinyapp.$inputValues;
          var params = [];
          
          for (var key in inputs) {
            if (inputs.hasOwnProperty(key)) {
              var value = inputs[key];
              if (value !== null && value !== undefined) {
                if (Array.isArray(value)) {
                  params.push(key + '=' + encodeURIComponent(JSON.stringify(value)));
                } else if (typeof value === 'string') {
                  params.push(key + '=' + encodeURIComponent('\"' + value + '\"'));
                } else if (typeof value === 'boolean' || typeof value === 'number') {
                  params.push(key + '=' + encodeURIComponent(value));
                }
              }
            }
          }
          
          return params.length > 0 ? '?' + params.join('&') : '';
        }
        
        // Update browser URL
        function updateBrowserUrl() {
          // NEVER update on initial load - only after user interactions
          if (isInitialLoad) {
            return;
          }
          
          var queryString = buildQueryString();
          if (queryString) {
            var newUrl = window.location.pathname + queryString;
            history.replaceState(null, '', newUrl);
          }
        }
        
        // Restore inputs from URL on page load
        function restoreFromUrl() {
          var urlParams = new URLSearchParams(window.location.search);
          var paramsObj = {};
          
          urlParams.forEach(function(value, key) {
            paramsObj[key] = value;
          });
          
          // Send to Shiny for processing
          if (Object.keys(paramsObj).length > 0) {
            Shiny.setInputValue('url_params_restore', paramsObj, {priority: 'event'});
          }
          
          // After restoration signal is sent, allow URL updates from user interactions
          setTimeout(function() {
            isInitialLoad = false;
          }, 3000);
        }
        
        // Listen for input changes (but skip during initial load)
        $(document).on('shiny:inputchanged', function(event) {
          if (!isInitialLoad) {
            clearTimeout(urlUpdateTimeout);
            urlUpdateTimeout = setTimeout(updateBrowserUrl, 500);
          }
        });
        
        // Restore on initial connection
        $(document).on('shiny:connected', function() {
          setTimeout(restoreFromUrl, 500);
        });
      "))
    ),
    # footer = shiny::tags$footer(
    #   id = "zrsa-footer",
    #   "Developed by ZevRoss Spatial Analysis, LLC"
    # ),
    collapsible = TRUE,
    window_title = "Innovation Strategy Explorer",
    title = shiny::tags$div(
      class = "navbar-title-container",
      shiny::tags$img(
        src = file.path(pkgload::pkg_name(), "img/prinz_logo.png"),
        class = "navbar-logo"
      ),
      shiny::tags$img(
        src = file.path(pkgload::pkg_name(), "img/zrsa_logo.svg"),
        class = "navbar-logo"
      ),
      shiny::tags$img(
        src = file.path(pkgload::pkg_name(), "img/ifc_logo.svg"),
        class = "navbar-logo"
      ),
      shiny::tags$div(
        class = "navbar-separator"
      ),
      shiny::tags$h1(
        "Innovation Strategy Explorer",
        class = "navbar-title-text"
      )
    ),
    id = "navbar_page",

    bslib::nav_spacer(),

    shiny::tabPanel(
      "Country Explorer",
      country_module_ui("country")
    ),
    shiny::tabPanel(
      "Region Explorer",
      region_module_ui("region")
    ),
    shiny::tabPanel(
      "Globe",
      globe_module_ui("globe")
    ),
    shiny::tabPanel(
      "About",
      shiny::h1("About Page")
    )
  )
}
