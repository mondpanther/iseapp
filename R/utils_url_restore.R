#' Generic URL parameter restoration for modules
#' 
#' @param params List of URL parameters
#' @param module_prefix Module namespace prefix (e.g., "country-", "region-")
#' @param tab_name Expected tab name (e.g., "Country Explorer")
#' @param input_configs Named list mapping input IDs to their types
#' @param session Module session object
#' 
#' @keywords internal
restore_module_inputs <- function(params, module_prefix, tab_name, input_configs, session) {

  if (is.null(params)) return()
  
  # Check if we're on the correct tab
  current_tab <- gsub('^"|"$', '', params$navbar_page %||% "")
  if (current_tab != tab_name) return()
  
  # Helper to parse values
  parse_value <- function(val) {
    if (is.null(val)) return(NULL)
    tryCatch({
      jsonlite::fromJSON(val)
    }, error = function(e) {
      gsub('^"|"$', '', val)
    })
  }
  
  # Restore all inputs for this module
  for (param_name in names(params)) {
    if (startsWith(param_name, module_prefix)) {
      input_id <- sub(paste0("^", module_prefix), "", param_name)
      
      if (input_id %in% names(input_configs)) {
        value <- parse_value(params[[param_name]])
        input_type <- input_configs[[input_id]]
        
        tryCatch({
          switch(input_type,
            "selectize" = shiny::updateSelectizeInput(session, input_id, selected = value),
            "radio" = shiny::updateRadioButtons(session, input_id, selected = value),
            "checkbox" = shiny::updateCheckboxInput(session, input_id, value = as.logical(value)),
            "numeric" = shiny::updateNumericInput(session, input_id, value = as.numeric(value)),
            "nav" = bslib::nav_select(id = input_id, selected = value, session = session)
          )
        }, error = function(e) {
          # Silently skip if input doesn't exist yet
        })
      }
    }
  }
}