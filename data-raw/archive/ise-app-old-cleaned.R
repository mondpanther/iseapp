# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(ggplot2)
library(countrycode)
library(plotly)
library(arrow)
library(dplyr)
library(data.table)
library(fst)
library(shinycssloaders)
library(httr2)
library(ggiraph)
library(gdtools)
library(gfonts)
library(leaflet)
library(sf)
source("R/functions_istraxfunctions.R")

# Get prepdata path for pre-computed data
prepdata_path <- "inst/extdata/prepdata"

localpath_fname <- function(fname) {
  if (!is.null(iseapp_local_path)) {
    fname <- sub("^/+", "", fname)
    return(file.path(iseapp_local_path, fname))
  }
  return("")
}

# Initialize placeholders for deferred data
# These will be populated by the server on session start
techmap <- NULL
countrymap <- NULL
regionmap <- NULL
regionmap_available <- FALSE

# Function to load big datasets (called asynchronously in server)
load_big_datasets <- function() {
  result <- list()

  # Load techmap
  pp <- localpath_fname("inst/extdata/techmap.fst")
  result$techmap <- read_fst(pp)

  # Load countrymap
  pp <- localpath_fname("inst/extdata/countrymap.fst")
  result$countrymap <- read_fst(pp)

  # Load regionmap
  pp_region <- localpath_fname("inst/extdata/regionmap.fst")
  result$regionmap <- read_fst(pp_region)

  result
}

# Check if we have precomputed data - if so, we can defer big data loading
has_precomputed_data <- !is.null(prepdata_path) && dir.exists(prepdata_path)

if (has_precomputed_data) {
  techmap_placeholder <- data.frame(
    docdb_family_id = integer(0),
    technology = character(0)
  )
  techmap <- techmap_placeholder
  countrymap <- data.frame(
    docdb_family_id = integer(0),
    ctry_code = character(0)
  )
  regionmap <- NULL
  regionmap_available <- FALSE
}

#rsconnect::writeManifest()
enableBookmarking(store = "url")

# Function to process techmap after loading (adds "All" category and normalizes names)
process_techmap <- function(techmap_raw, countrymap_data) {
  # Add "All" technology from countrymap
  techmap_processed <- countrymap_data %>%
    select(docdb_family_id) %>%
    distinct() %>%
    mutate(technology = "All") %>%
    bind_rows(techmap_raw)

  # Normalize technology names
  setDT(techmap_processed)
  techmap_processed[, technology := fcase(
    technology == "Any Green technology", "Green Technology",
    technology == "Any battery technology", "Battery Technology",
    technology == "Any Hard to Abate technology", "Hard to Abate Sector Decarbonization",
    default = technology
  )]
  techmap_processed
}

green_classes <- c("Green Technology","Green Energy", "Green Transport", "Circular Economy", "Green Manufacturing",
                   "Adaptation", "Green Housing", "Green ICT", "Green Agriculture",
                   "GHG Capture")

battery_classes=c("Battery Technology", "Lithium Extraction & Processing", "Graphite & Carbon Materials", "Cathode Materials", "Anode Materials",
                  "Electrolytes & Additives", "Separators", "Battery Cell Design & Assembly", "Battery Management Systems (BMS)", 
                  "Electric Vehicles & Mobility", "Battery Recycling & Recovery")


hard_to_abate_classes=c("Hard to Abate Sector Decarbonization", "Aviation Decarbonisation", "Cement & Concrete Decarbonisation",
                        "Chemicals & Plastics Decarbonisation", "Shipping Decarbonisation",
                        "Steel & Iron Decarbonisation")

ai_classes=c("AI", "Machine Learning", "Deep Learning", "Natural Language Processing (NLP)", 
             "Computer Vision", "Speech Recognition & Synthesis", "Robotics & Autonomous Systems",
             "Knowledge Representation & Reasoning", "Planning & Decision Making", "Generative AI",
             "Semiconductors", "Cloud & Data Infrastructure", "Data Rettrieval & Processing System", "Platform & Frameworks", "Deployment & Support")


cpc_sections=c( "Human Necessities",
                "Performing Operations; Transporting ",
                "Chemistry; Metallurgy ",
                "Textiles; Paper",
                "Fixed Constructions",
                "Mechanical Engineering; Lighting; Heating; Weapons; Blasting",
                "Physics",
                "Electricity",
                 "General tagging of new or cross-sectional technology" )

agrifood_classes=c("Any Agriculture & Food technology", "Input supply", "Primary food and feed production",
                   "Post-harvest handling & aggregation", "Processing", "Distribution/wholesale",
                   "Retail/consumption", "Crosscutting")

# Get all unique technologies from techmap
# When using deferred loading with precomputed data, we use known technology categories
# instead of extracting from techmap (which may be a placeholder)
if (has_precomputed_data) {
  # Known technology categories - these match what's in the precomputed data
  all_techs <- c("All", green_classes, battery_classes, hard_to_abate_classes, ai_classes, cpc_sections, agrifood_classes)
  all_techs <- unique(all_techs)
}

# Create grouped technology choices
# Separate technologies into green, battery, and other categories
green_classes_d        =setdiff(green_classes,"Green Technology")
battery_classes_d      =setdiff(battery_classes,"Battery Technology")
hard_to_abate_classes_d=setdiff(hard_to_abate_classes,"Hard to Abate Sector Decarbonization")
ai_classes_d           =setdiff(ai_classes,"AI")
agrifood_classes_d     =setdiff(agrifood_classes,"Any Agriculture & Food technology")
colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes,cpcsecs=cpc_sections,agrifood=agrifood_classes)
other_techs <- c(setdiff(all_techs, c(green_classes, battery_classes,hard_to_abate_classes,cpc_sections,agrifood_classes)),"Green Technology","Battery Technology","Hard to Abate Sector Decarbonization","Any Agriculture & Food technology")

grouped_techs <- list(
  "Broad Technology Categories"                         = as.list(setNames(other_techs, other_techs)),
  "Detailed Green technologies"                         = as.list(setNames(green_classes_d, green_classes_d)),
  "AI subcategories"                                    = as.list(setNames(ai_classes_d, ai_classes_d)),
  "Detailed Battery technologies"                       = as.list(setNames(battery_classes_d, battery_classes_d)),
  "Detailed Hard to Abate Sector Decarbonization Technologies" = as.list(setNames(hard_to_abate_classes_d, hard_to_abate_classes_d)),
  "Agriculture & Food technology"                       = as.list(setNames(agrifood_classes_d, agrifood_classes_d)),
  "CPC Sections"                                        = as.list(setNames(cpc_sections, cpc_sections))
)

marginal  = list(
  "Marginal Global Returns"   = "istrax_global",
  "Marginal National Returns" = "istrax_nationalkey_2009_2018",
  "Marginal Returns to LMICs" = "istrax_EMDE",
  "Marginal Returns to LMICs (excl. China)" = "istrax_EMDENOCN",
  "Marginal Returns to LMICs (excl. China & India)" = "istrax_EMDENOCNIN",
  "Marginal Returns to HICs"    = "istrax_HIC",
  "Marginal Returns to the EU"  = "istrax_EU",
  "Marginal Returns to the EU"  = "istrax_EU",
  "Marginal Returns to US"      = "istrax_US",
  "Marginal Returns to China"   = "istrax_CN",
  "Marginal Returns to UK"      = "istrax_GB",
  "Marginal Returns to Austria" = "istrax_AT",
  "Marginal Returns to France"  = "istrax_FR"
)

average   = list(
  "Average Global Returns"   = "avstrax_global",
  "Average National Returns" = "avstrax_nationalkey_2009_2018",
  "Average Returns to LMICs" = "avstrax_EMDE",
  "Average Returns to LMICs (excl. China)" = "avstrax_EMDENOCN",
  "Average Returns to LMICs (excl. China & India)" = "avstrax_EMDENOCNIN",
  "Average Returns to HICs"    = "avstrax_HIC",
  "Average Returns to the EU"  = "avstrax_EU",
  "Average Returns to the EU"  = "avstrax_EU",
  "Average Returns to US"      = "avstrax_US",
  "Average Returns to China"   = "avstrax_CN",
  "Average Returns to UK"      = "avstrax_GB",
  "Average Returns to Austria" = "avstrax_AT",
  "Average Returns to France"  = "avstrax_FR"
)
  
spillovers = list(
  "Average Global Spillovers"   = "ev_global",
  "Average National Spillovers" = "ev_nationalkey_2009_2018",
  "Average Spillovers to LMICs" = "ev_EMDE",
  "Average Spillovers to LMICs (excl. China)"         = "ev_EMDENOCN",
  "Average Spillovers to LMICs (excl. China & India)" = "ev_EMDENOCNIN",
  "Average Spillovers to HICs"    = "ev_HIC",
  "Average Spillovers to the EU"  = "ev_EU",
  "Average Spillovers to the EU"  = "ev_EU",
  "Average Spillovers to US"      = "ev_US",
  "Average Spillovers to China"   = "ev_CN",
  "Average Spillovers to UK"      = "ev_GB",
  "Average Spillovers to Austria" = "ev_AT",
  "Average Spillovers to France"  = "ev_FR"
  
)

toflow_choices <- list("Marginal Returns"= marginal,"Average Returns"=average,
                       "Average Spillovers"=spillovers)

get_available_iso2 <- function() {
  candidates <- c("country_code", "iso2c", "iso2")
  if (exists("patchar_countrymap")) {
    for (cn in candidates) {
      if (!is.null(patchar_countrymap[[cn]])) {
        return(sort(unique(na.omit(patchar_countrymap[[cn]]))))
      }
    }
  }
  if (exists("techmap")) {
    for (cn in candidates) {
      if (!is.null(techmap[[cn]])) {
        return(sort(unique(na.omit(techmap[[cn]]))))
      }
    }
  }
  sort(unique(na.omit(countrycode::codelist$iso2c)))
}

available_iso2 <- get_available_iso2()
iso_ref <- unique(countrycode::codelist[, c("iso2c", "country.name.en", "region")])
match_idx <- match(available_iso2, iso_ref$iso2c)
valid <- !is.na(match_idx)
vals <- available_iso2[valid]
labs <- iso_ref$country.name.en[match_idx[valid]]
ord <- order(tolower(labs))
country_choices <- setNames(vals[ord], labs[ord])

all_countries <- sort(unique(na.omit(countrycode::codelist$iso2c)))
lmics <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG",
           "BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI",
           "CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE",
           "GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ",
           "KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML",
           "MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG",
           "MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC",
           "SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO",
           "TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")

lmics_excl_china <- setdiff(lmics, "CN")
eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
                  "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
                  "SI","ES","SE")
hic <- setdiff(all_countries, lmics)

group_definitions <- list(
  "All countries" = all_countries,
  "LMICs" = lmics,
  "LMICs (excl. China)" = lmics_excl_china,
  "EU countries" = eu_countries,
  "High income countries" = hic
)

grouped_choices <- list(
  "Predefined Groups" = lapply(names(group_definitions), function(name) setNames(name, name)),
  "Individual Countries" = as.list(country_choices)
)
names(grouped_choices[["Predefined Groups"]]) <- names(group_definitions)

default_country <- if ("VN" %in% vals) "VN" else if (length(vals) > 0) vals[1] else NA_character_


# Define a Bootstrap theme with a consistent base font
base_theme <- bs_theme(base_font = "Arial")



expand_country_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(group_definitions)) {
      return(group_definitions[[x]])
    } else {
      return(x)
    }
  }))
  unique(expanded)
}

# Region definitions for Region Explorer tab
# UK NUTS1 regions (can be expanded in the future)
uk_regions <- c(
  "UKC" = "North East England",
  "UKD" = "North West England",
  "UKE" = "Yorkshire and The Humber",
  "UKF" = "East Midlands",
  "UKG" = "West Midlands",
  "UKH" = "East of England",
  "UKI" = "London",
  "UKJ" = "South East England",
  "UKK" = "South West England",
  "UKL" = "Wales",
  "UKM" = "Scotland",
  "UKN" = "Northern Ireland"
)

# Create region choices (code -> name mapping)
region_choices <- setNames(names(uk_regions), uk_regions)

# Region group definitions
region_group_definitions <- list(
  "All UK regions" = names(uk_regions)
)

# Create grouped region choices
grouped_region_choices <- list(
  "Predefined Groups" = list("All UK regions" = "All UK regions"),
  "Individual Regions" = as.list(region_choices)
)

# Function to expand region selection (similar to country)
expand_region_selection <- function(selected) {
  expanded <- unlist(lapply(selected, function(x) {
    if (x %in% names(region_group_definitions)) {
      return(region_group_definitions[[x]])
    } else {
      return(x)
    }
  }))
  unique(expanded)
}

# Function to get region display name
get_region_name <- function(code) {
  if (code %in% names(uk_regions)) {
    return(uk_regions[code])
  }
  return(code)
}

# Define UI
ui <- function(request){fluidPage(

  bookmarkButton("Bookmark current data view...."),

  # Tabbed interface
  tabsetPanel(
    id = "main_tabs",

    # Tab 1: Country Explorer
    tabPanel(
      "Country Explorer",
      br(),
      inputPanel(
        selectizeInput(
          inputId = "country",
          label = "Country or Group",
          choices = grouped_choices,
          selected = "All countries",
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = 'Choose one or more countries or groups...')
        ),
        selectizeInput(
          inputId = "toflow",
          label = "Return flow",
          choices = toflow_choices,
          selected = "istrax_global",
          multiple = FALSE,
          width = "100%",
          options = list(placeholder = 'Choose a return flow...')
        ),
        selectizeInput(
          inputId = "tech_categories_plot1",
          label = "Technology categories",
          choices = grouped_techs,
          selected = c("AI","Green Technology"),
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = 'Choose one or more technology categories...')
        ),
        radioButtons(
          inputId = "bwidthscale",
          label = "Bar width scale",
          choices = c("log", "proportional"),
          selected = "log"
        ),
        radioButtons(
          inputId = "display_mode",
          label = "Display mode",
          choices = c("Confidence bands" = "confidence", "Returns for the top 25 and top 50 percent" = "quartiles"),
          selected = "confidence"
        ),
        checkboxInput(
          inputId = "show_top3_ids",
          label = "Show top patent IDs",
          value = TRUE
        )
      ),
      tags$button("▲ less", id = "togglePlot1", class = "plot-toggle"),
      tags$div(
        id = "plot1Container",
        withSpinner(girafeOutput("avstrax_plot1", width = "100%", height = "auto"), type = 4, color = "#3498db")
      ),
      inputPanel(
        selectizeInput(
          inputId = "techs",
          label = "Technology categories",
          choices = grouped_techs,
          selected = "Green Technology",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more technology categories...')
        ),
        selectizeInput(
          inputId = "techs_comparison",
          label = "Comparison categories",
          choices = grouped_techs,
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Choose categories to compare...')
        ),
        sliderInput(
          inputId = "topn",
          label = "Show top n countries",
          min = 1,
          max = 200,
          width = "350px",
          value = 20
        ),
        sliderInput(
          inputId = "mininno",
          label = "Innovation count threshold:",
          min = 1,
          max = 500,
          value = 100,
          width = "350px"
        )
      ),
      # Returns by Country plot and map
      tags$h4("Returns by Country"),
      tags$button("▲ less", id = "toggleReturnsBar", class = "plot-toggle"),
      tags$div(
        id = "returnsBarContainer",
        withSpinner(girafeOutput("avstrax_plot2", width = "100%", height = "auto"), type = 4, color = "#3498db"),
        tags$button("▲ less", id = "toggleReturns", class = "plot-toggle"),
        tags$div(
          id = "returnsContainer",
          tags$br(),
          tags$h4("World Map: Returns"),
          withSpinner(plotlyOutput("world_map", width = "100%", height = "500px"), type = 4, color = "#3498db")
        )
      ),
      tags$br(),
      tags$hr(),
      # RTA section with separate controls
      tags$h3("Revealed Technological Advantage (RTA)"),
      inputPanel(
        sliderInput(
          inputId = "topn_rta",
          label = "Show top n countries",
          min = 1,
          max = 200,
          width = "350px",
          value = 20
        ),
        sliderInput(
          inputId = "bottomn_rta",
          label = "Show bottom n countries",
          min = 0,
          max = 200,
          width = "350px",
          value = 0
        ),
        sliderInput(
          inputId = "mininno_rta",
          label = "Innovation count threshold:",
          min = 1,
          max = 500,
          value = 1,
          width = "350px"
        ),
        sliderInput(
          inputId = "minallinnos_rta",
          label = "All innovation threshold:",
          min = 1,
          max = 5000,
          value = 100,
          width = "350px"
        )
      ),
      fluidRow(
        column(6,
          tags$h4("RTA by Country"),
          withSpinner(girafeOutput("avstrax_plot2_rta", width = "100%", height = "auto"), type = 4, color = "#e74c3c")
        ),
        column(6,
          tags$h4("RTA vs Returns"),
          withSpinner(girafeOutput("rta_returns_scatter", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          tags$h4("RTA vs GDP per Capita"),
          withSpinner(girafeOutput("rta_gdp_scatter", width = "100%", height = "auto"), type = 4, color = "#e74c3c")
        )
      ),
      tags$br(),
      tags$h4("World Map: RTA"),
      withSpinner(plotlyOutput("world_map_rta", width = "100%", height = "500px"), type = 4, color = "#e74c3c")
    ),

    # Tab 2: Region Explorer
    tabPanel(
      "Region Explorer",
      br(),
      inputPanel(
        selectizeInput(
          inputId = "region",
          label = "Region or Group",
          choices = grouped_region_choices,
          selected = "All UK regions",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more regions...')
        ),
        selectizeInput(
          inputId = "toflow_region",
          label = "Return flow",
          choices = toflow_choices,
          selected = "istrax_global",
          multiple = FALSE,
          width = "400px",
          options = list(placeholder = 'Choose a return flow...')
        ),
        selectizeInput(
          inputId = "tech_categories_plot1_region",
          label = "Technology categories",
          choices = grouped_techs,
          selected = c("All", "AI", "Green Technology"),
          multiple = TRUE,
          width = "200%",
          options = list(placeholder = 'Choose one or more technology categories...')
        ),
        radioButtons(
          inputId = "bwidthscale_region",
          label = "Bar width scale",
          choices = c("log", "proportional"),
          selected = "log"
        ),
        radioButtons(
          inputId = "display_mode_region",
          label = "Display mode",
          choices = c("Confidence bands" = "confidence", "Returns for the top 25 and top 50 percent" = "quartiles"),
          selected = "confidence"
        ),
        checkboxInput(
          inputId = "show_top3_ids_region",
          label = "Show top patent IDs",
          value = TRUE
        )
      ),
      tags$button("▲ less", id = "togglePlot1_region", class = "plot-toggle"),
      tags$div(
        id = "plot1Container_region",
        withSpinner(girafeOutput("avstrax_plot1_region", width = "100%", height = "auto"), type = 4, color = "#3498db")
      ),
      inputPanel(
        selectizeInput(
          inputId = "techs_region",
          label = "Technology categories",
          choices = grouped_techs,
          selected = "Green Technology",
          multiple = TRUE,
          options = list(placeholder = 'Choose one or more technology categories...')
        ),
        selectizeInput(
          inputId = "techs_comparison_region",
          label = "Comparison categories",
          choices = grouped_techs,
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Choose categories to compare...')
        ),
        sliderInput(
          inputId = "topn_region",
          label = "Show top n regions",
          min = 1,
          max = 50,
          width = "350px",
          value = 12
        ),
        sliderInput(
          inputId = "mininno_region",
          label = "Innovation count threshold:",
          min = 1,
          max = 500,
          value = 100,
          width = "350px"
        )
      ),
      # Returns by Region plot and map
      tags$h4("Returns by Region"),
      tags$button("▲ less", id = "toggleReturnsBar_region", class = "plot-toggle"),
      tags$div(
        id = "returnsBarContainer_region",
        withSpinner(girafeOutput("avstrax_plot2_region", width = "100%", height = "auto"), type = 4, color = "#3498db"),
        tags$button("▲ less", id = "toggleReturns_region", class = "plot-toggle"),
        tags$div(
          id = "returnsContainer_region",
          tags$br(),
          tags$h4("UK Regions Map: Returns"),
          withSpinner(leafletOutput("uk_regions_map", width = "100%", height = "500px"), type = 4, color = "#3498db")
        )
      ),
      tags$br(),
      tags$hr(),
      # RTA section with separate controls
      tags$h3("Revealed Technological Advantage (RTA)"),
      inputPanel(
        sliderInput(
          inputId = "topn_rta_region",
          label = "Show top n regions",
          min = 1,
          max = 50,
          width = "350px",
          value = 12
        ),
        sliderInput(
          inputId = "bottomn_rta_region",
          label = "Show bottom n regions",
          min = 0,
          max = 50,
          width = "350px",
          value = 0
        ),
        sliderInput(
          inputId = "mininno_rta_region",
          label = "Innovation count threshold:",
          min = 1,
          max = 500,
          value = 100,
          width = "350px"
        ),
        sliderInput(
          inputId = "minallinnos_rta_region",
          label = "All innovation threshold:",
          min = 1,
          max = 5000,
          value = 500,
          width = "350px"
        )
      ),
      fluidRow(
        column(6,
          tags$h4("RTA by Region"),
          withSpinner(girafeOutput("avstrax_plot2_region_rta", width = "100%", height = "auto"), type = 4, color = "#e74c3c")
        ),
        column(6,
          tags$h4("RTA vs Returns"),
          withSpinner(girafeOutput("rta_returns_scatter_region", width = "100%", height = "auto"), type = 4, color = "#e74c3c")
        )
      ),
      tags$br(),
      tags$h4("UK Regions Map: RTA"),
      withSpinner(leafletOutput("uk_regions_map_rta", width = "100%", height = "500px"), type = 4, color = "#e74c3c")
    )
  )
)
}

  
  
# Define server
server <- function(input, output, session) {

  # Reactive values for window dimensions
  window_dims <- reactiveValues(width = 800, height = 600, initialized = FALSE)

  # ============================================================================
  # DEFERRED DATA LOADING
  # ============================================================================
  # Track loading state for big datasets
  data_state <- reactiveValues(
    techmap_loaded = !has_precomputed_data,  # Already loaded if no precomputed
    countrymap_loaded = !has_precomputed_data,
    regionmap_loaded = !has_precomputed_data,
    loading_started = FALSE,
    loading_complete = !has_precomputed_data
  )

  # Store loaded data in reactive values (will be populated by deferred loading)
  loaded_data <- reactiveValues(
    techmap = if (!has_precomputed_data) techmap else NULL,
    countrymap = if (!has_precomputed_data) countrymap else NULL,
    regionmap = if (!has_precomputed_data) regionmap else NULL
  )

  # Start deferred loading after a short delay (allows UI to render first)
  observe({
    if (has_precomputed_data && !data_state$loading_started) {
      data_state$loading_started <- TRUE

      # Use invalidateLater to defer loading to next tick, allowing UI to render
      invalidateLater(100)
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Deferred loading observer - loads big datasets in background
  observe({
    req(has_precomputed_data)
    req(data_state$loading_started)
    req(!data_state$loading_complete)

    message("Starting deferred loading of big datasets...")

    # Load datasets
    tryCatch({
      datasets <- load_big_datasets()

      # Process techmap (add "All" category and normalize names)
      processed_techmap <- process_techmap(datasets$techmap, datasets$countrymap)

      # Update reactive values
      loaded_data$techmap <- processed_techmap
      loaded_data$countrymap <- datasets$countrymap
      loaded_data$regionmap <- datasets$regionmap

      # Update global variables for backward compatibility with plot functions
      techmap <<- processed_techmap
      countrymap <<- datasets$countrymap
      regionmap <<- datasets$regionmap
      regionmap_available <<- !is.null(datasets$regionmap) && nrow(datasets$regionmap) > 0

      # Mark as loaded
      data_state$techmap_loaded <- TRUE
      data_state$countrymap_loaded <- TRUE
      data_state$regionmap_loaded <- TRUE
      data_state$loading_complete <- TRUE

      message("Deferred loading complete. Big datasets are now available.")
    }, error = function(e) {
      message("Error during deferred loading: ", e$message)
    })
  })

  # Helper to get current techmap (prefers loaded data, falls back to global)
  get_techmap <- reactive({
    if (!is.null(loaded_data$techmap) && nrow(loaded_data$techmap) > 0) {
      loaded_data$techmap
    } else {
      techmap
    }
  })

  # Helper to get current countrymap
  get_countrymap <- reactive({
    if (!is.null(loaded_data$countrymap) && nrow(loaded_data$countrymap) > 0) {
      loaded_data$countrymap
    } else {
      countrymap
    }
  })

  # Helper to get current regionmap
  get_regionmap <- reactive({
    if (!is.null(loaded_data$regionmap) && nrow(loaded_data$regionmap) > 0) {
      loaded_data$regionmap
    } else {
      regionmap
    }
  })

  # Reactive for regionmap availability
  is_regionmap_available <- reactive({
    rm <- get_regionmap()
    !is.null(rm) && nrow(rm) > 0
  })

  # Track window resize events
  observe({
    # Get the output container dimensions from session clientData
    w1 <- session$clientData$output_avstrax_plot1_width
    w2 <- session$clientData$output_avstrax_plot2_width
    w3 <- session$clientData$output_avstrax_plot1_region_width
    w4 <- session$clientData$output_avstrax_plot2_region_width

    # Use the larger of the widths, or default
    w <- max(c(w1, w2, w3, w4, 400), na.rm = TRUE)
    if (!is.null(w) && !is.na(w) && w > 400) {
      window_dims$width <- w
      window_dims$initialized <- TRUE
    } else {
      # Keep checking until we get valid dimensions (important for bookmark restoration)
      invalidateLater(100)
    }
  })


  patchar_countrymap <- reactive({
    req(input$toflow)
    # Require countrymap to be loaded for on-the-fly computation
    req(data_state$countrymap_loaded)

    # Get the current countrymap (from reactive helper)
    current_countrymap <- get_countrymap()
    req(nrow(current_countrymap) > 0)

    #input=list(toflow="avstrax_global")
    path <- paste0("/istraxes/", input$toflow,".fst")


    pp=localpath_fname(path)
    if(file.exists(pp)){
      ddd <- read_fst(pp)
    } else {
      ddd <- dropbox_read_fst(path)}

    # Replace missing values with 0 in the value column
    # Some files (avstrax, ev) contain NAs that need to be treated as 0
    value_col <- input$toflow
    if (value_col %in% names(ddd)) {
      ddd[[value_col]][is.na(ddd[[value_col]])] <- 0
    }

    #patchar_countrymap <- countrymap %>% left_join(read_fst(path))
    patchar_countrymap <- current_countrymap %>% left_join(ddd, by = c("docdb_family_id", "ctry_code"))

  })

  
  
  
  output$avstrax_plot1 <- renderGirafe({
    req(input$country, input$toflow, input$tech_categories_plot1, input$bwidthscale, input$display_mode, !is.null(input$show_top3_ids))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

    # Plot 1 always uses on-the-fly computation (precomputed data doesn't work well
    # with technology category filtering and color coding)
    req(data_state$techmap_loaded)
    req(data_state$countrymap_loaded)

    selected_countries <- expand_country_selection(input$country)

    # Get the label from the nested toflow_choices list
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Get current techmap
    current_techmap <- get_techmap()

    # Filter techmap based on selected technology categories
    # Handle "Other" category to include all non-selected technologies
    selected_categories <- input$tech_categories_plot1
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    if(include_other && length(explicit_categories) > 0) {
      # Include explicitly selected categories AND other categories relabeled as "Other"
      filtered_techmap <- current_techmap %>%
        mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
    } else if(include_other && length(explicit_categories) == 0) {
      # Only "Other" selected - show all categories as "Other"
      filtered_techmap <- current_techmap %>%
        mutate(technology = "Other")
    } else {
      # No "Other" - just filter to explicitly selected categories
      filtered_techmap <- current_techmap %>%
        filter(technology %in% explicit_categories)
    }

    #selected_countries="VN"  ;input=list(); input$toflow="istrax_global"
    #colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes)
    
    # Calculate responsive dimensions - wider browser = wider plot
    plot_width <- max(window_dims$width, 400)
    # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
    width_inches <- plot_width / 96
    # Wider windows get wider aspect ratio (less height per width)
    aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
    height_inches <- width_inches * aspect_ratio

    p <- plot_avstrax_by_country(
      pdata = patchar_countrymap(),
      classes = filtered_techmap,
      #green_classes = green_classes,
      country_code = selected_countries,
      toflow = input$toflow,
      custom_colors = custom_colors,
      colorings=colorings,
      bwidthscale=input$bwidthscale,
      display_mode=input$display_mode,
      show_top3_ids=input$show_top3_ids,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title =  sub("^[^.]*\\.", "", flow_label)
      #battery_classes = battery_classes,
      #hard_to_abate_classes = hard_to_abate_classes
    )

    p
  })


  output$avstrax_plot2 <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$topn,
        input$mininno,
        input$bwidthscale,
        input$display_mode,
        !is.null(input$show_top3_ids))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

    selected_countries <- expand_country_selection(input$country)
    # Get the label from the nested toflow_choices list
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Try to load pre-computed data if available
    precomputed_data <- NULL

    # Check if we can use pre-computed data:
    # 1. No comparison technologies (pre-computation doesn't cover comparisons)
    # 2. Technology selection matches a known category
    # Note: by_country files aggregate BY country for a specific technology category
    if (is.null(input$techs_comparison) || length(input$techs_comparison) == 0) {
      # Try to match tech selection to a pre-computed category
      tech_category <- match_tech_category(input$techs)

      if (!is.null(tech_category)) {
        # Try to load pre-computed data - by_country files are keyed by tech_category only
        precomputed_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
        if (!is.null(precomputed_data)) {
          message("Using pre-computed data for tech category: ", tech_category)
          # Filter precomputed data to selected countries if needed
          # BUT keep the "All" row which is needed for computing the average line
          selected_countries <- expand_country_selection(input$country)
          if (!is.null(precomputed_data$ctry_code)) {
            precomputed_data <- precomputed_data %>%
              filter(ctry_code %in% selected_countries | ctry_code == "All")
          }
        }
      }
    }

    # Only load and merge full data if pre-computed data is not available
    if (is.null(precomputed_data)) {
      # Require big datasets for on-the-fly computation
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)
      # We first implement the filter from the previous diagram; i.e. we restrict to the countries selected there...
      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries )
    } else {
      # When using pre-computed data, we still need a minimal filtered for any fallback
      filtered <- NULL
    }

    # Calculate responsive dimensions - wider browser = wider plot
    plot_width <- max(window_dims$width, 400)
    # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
    width_inches <- plot_width / 96
    # Calculate height based on number of countries to display (topn)
    # Base height per country bar, with minimum height
    n_countries <- input$topn
    height_per_bar <- 0.35  # inches per bar
    min_height <- 4  # minimum height in inches
    height_inches <- max(min_height, n_countries * height_per_bar)

    # Get current techmap (may be placeholder if still loading)
    current_techmap <- get_techmap()

    p <- plot_avstrax_by_technology(
      pdata = if(is.null(precomputed_data)) filtered else data.frame(),
      classes = current_techmap,
      technologies = input$techs,
      toflow = input$toflow,
      custom_colors = custom_colors,
      topn = input$topn,
      mininno = input$mininno,
      bwidthscale = input$bwidthscale,
      display_mode = input$display_mode,
      show_top3_ids = input$show_top3_ids,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = sub("^[^.]*\\.", "", flow_label),
      comparison_technologies = input$techs_comparison,
      precomputed_avstrax = precomputed_data
    )

    p
  })

  # World Map for Country Explorer
  output$world_map <- renderPlotly({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    # Try to match tech selection to pre-computed data
    # World map shows data BY country for a specific technology, so uses by_country files
    tech_category <- match_tech_category(input$techs)

    if (!is.null(tech_category)) {
      avstrax_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
      if (!is.null(avstrax_data)) {
        message("World map using pre-computed data for tech: ", tech_category)
        # Filter to selected countries
        if (!is.null(avstrax_data$ctry_code)) {
          avstrax_data <- avstrax_data %>%
            filter(ctry_code %in% selected_countries)
        }
      }
    }

    # If no pre-computed data, compute on the fly
    if (is.null(avstrax_data)) {
      # Require big datasets for on-the-fly computation
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)

      # Get current techmap
      current_techmap <- get_techmap()

      # Filter by technology class
      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs) %>%
        distinct()

      if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

      # Filter data by selected countries
      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries)

      # Compute aggregated data for all countries
      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)
    }

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno)

    # Determine if this is a return (%) or spillover ($) variable
    is_return <- grepl("strax", input$toflow)

    plot_world_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      color_scale = "Viridis",
      plot_title = paste0("World Map: ", sub("^[^.]*\\.", "", flow_label)),
      is_return = is_return
    )
  })

  # RTA Plot for Country Explorer (below returns plot)
  output$avstrax_plot2_rta <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$topn_rta,
        input$mininno_rta,
        input$minallinnos_rta,
        input$bwidthscale,
        !is.null(input$show_top3_ids))
    req(window_dims$initialized)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Try to load pre-computed data if available
    precomputed_data <- NULL

    if (is.null(input$techs_comparison) || length(input$techs_comparison) == 0) {
      tech_category <- match_tech_category(input$techs)

      if (!is.null(tech_category)) {
        precomputed_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
        if (!is.null(precomputed_data)) {
          # Check if precomputed data has RTA and Allinnos columns needed for RTA filtering
          if (!"RTA" %in% names(precomputed_data) || !"Allinnos" %in% names(precomputed_data)) {
            message("Precomputed data missing RTA or Allinnos columns, will compute on the fly")
            precomputed_data <- NULL
          } else {
            message("RTA plot using pre-computed data for tech category: ", tech_category)
            selected_countries <- expand_country_selection(input$country)
            if (!is.null(precomputed_data$ctry_code)) {
              precomputed_data <- precomputed_data %>%
                filter(ctry_code %in% selected_countries | ctry_code == "All")
            }
          }
        }
      }
    }

    if (is.null(precomputed_data)) {
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)
      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries)
    } else {
      filtered <- NULL
    }

    # Calculate responsive dimensions - use actual output width for proper scaling
    plot_width <- session$clientData$output_avstrax_plot2_rta_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600
    width_inches <- plot_width / 96
    # Calculate height based on number of countries to display (topn + bottomn)
    n_countries <- input$topn_rta + input$bottomn_rta
    height_per_bar <- 0.35  # inches per bar
    min_height <- 4  # minimum height in inches
    height_inches <- max(min_height, n_countries * height_per_bar)

    current_techmap <- get_techmap()

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    rta_title <- paste0("RTA in ", tech_label, " by Country")

    p <- plot_avstrax_rta(
      pdata = if(is.null(precomputed_data)) filtered else data.frame(),
      classes = current_techmap,
      technologies = input$techs,
      toflow = input$toflow,
      custom_colors = custom_colors,
      topn = input$topn_rta,
      bottomn = input$bottomn_rta,
      mininno = input$mininno_rta,
      minallinnos = input$minallinnos_rta,
      bwidthscale = input$bwidthscale,
      show_top3_ids = input$show_top3_ids,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = rta_title,
      precomputed_avstrax = precomputed_data
    )

    p
  })

  # RTA World Map for Country Explorer
  output$world_map_rta <- renderPlotly({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    tech_category <- match_tech_category(input$techs)

    if (!is.null(tech_category)) {
      avstrax_data <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
      if (!is.null(avstrax_data)) {
        message("RTA World map using pre-computed data for tech: ", tech_category)
        if (!is.null(avstrax_data$ctry_code)) {
          avstrax_data <- avstrax_data %>%
            filter(ctry_code %in% selected_countries)
        }
      }
    }

    # If no pre-computed data, compute on the fly
    if (is.null(avstrax_data)) {
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)

      current_techmap <- get_techmap()

      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs) %>%
        distinct()

      if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries)

      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)
    }

    # Filter by minimum innovations (using RTA-specific thresholds)
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_rta)

    # Apply Allinnos filter if the column exists
    if ("Allinnos" %in% names(avstrax_data) && input$minallinnos_rta > 0) {
      avstrax_data <- avstrax_data %>%
        filter(Allinnos >= input$minallinnos_rta)
    }

    # Plot RTA on world map (RTA is always an index, not a percentage or dollar value)
    plot_world_map(
      avstrax_data = avstrax_data,
      value_col = "RTA",
      color_scale = "RdYlGn",  # Red-Yellow-Green scale for RTA (red = low, green = high)
      plot_title = "World Map: RTA Index",
      is_return = FALSE  # RTA is an index, not a percentage
    )
  })

  # RTA vs Returns Scatter Plot for Country Explorer
  output$rta_returns_scatter <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    tech_category <- match_tech_category(input$techs)

    if (!is.null(tech_category)) {
      precomputed <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
      if (!is.null(precomputed) && "RTA" %in% names(precomputed) && "Allinnos" %in% names(precomputed)) {
        message("RTA scatter plot using pre-computed data for tech: ", tech_category)
        if (!is.null(precomputed$ctry_code)) {
          avstrax_data <- precomputed %>%
            filter(ctry_code %in% selected_countries)
        } else {
          avstrax_data <- precomputed
        }
      }
    }

    # If no pre-computed data with RTA and Allinnos, compute on the fly
    if (is.null(avstrax_data)) {
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)

      current_techmap <- get_techmap()

      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs) %>%
        distinct()

      if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries)

      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)
    }

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_returns_scatter_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    scatter_title <- paste0("RTA vs Returns: ", tech_label)

    p <- plot_rta_returns_scatter(
      avstrax_data = avstrax_data,
      mininno = input$mininno_rta,
      minallinnos = input$minallinnos_rta,
      bwidthscale = input$bwidthscale,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = scatter_title,
      x_label = "RTA",
      y_label = "Return (%)"
    )

    p
  })

  # RTA vs GDP per Capita Scatter Plot for Country Explorer
  output$rta_gdp_scatter <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    tech_category <- match_tech_category(input$techs)

    if (!is.null(tech_category)) {
      precomputed <- load_precomputed_by_country(prepdata_path, input$toflow, tech_category)
      if (!is.null(precomputed) && "RTA" %in% names(precomputed) && "Allinnos" %in% names(precomputed)) {
        message("RTA GDP scatter plot using pre-computed data for tech: ", tech_category)
        if (!is.null(precomputed$ctry_code)) {
          avstrax_data <- precomputed %>%
            filter(ctry_code %in% selected_countries)
        } else {
          avstrax_data <- precomputed
        }
      }
    }

    # If no pre-computed data with RTA and Allinnos, compute on the fly
    if (is.null(avstrax_data)) {
      req(data_state$countrymap_loaded)
      req(data_state$techmap_loaded)

      current_techmap <- get_techmap()

      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs) %>%
        distinct()

      if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

      filtered <- patchar_countrymap() %>%
        filter(ctry_code %in% selected_countries)

      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)
    }

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_gdp_scatter_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    scatter_title <- paste0("RTA vs GDP per Capita: ", tech_label)

    p <- plot_rta_gdp_scatter(
      avstrax_data = avstrax_data,
      mininno = input$mininno_rta,
      minallinnos = input$minallinnos_rta,
      bwidthscale = input$bwidthscale,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = scatter_title
    )

    p
  })

  # ============================================
  # Region Explorer Tab - Server Logic
  # ============================================

  # Reactive for region data (similar to patchar_countrymap but for regions)
  patchar_regionmap <- reactive({
    req(input$toflow_region)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    path <- paste0("/istraxes/", input$toflow_region,".fst")

    pp=localpath_fname(path)
    if(file.exists(pp)){
      ddd <- read_fst(pp)
    } else {
      ddd <- dropbox_read_fst(path)
    }

    # Replace missing values with 0 in the value column
    # Some files (avstrax, ev) contain NAs that need to be treated as 0
    value_col <- input$toflow_region
    if (value_col %in% names(ddd)) {
      ddd[[value_col]][is.na(ddd[[value_col]])] <- 0
    }

    # Get current regionmap
    current_regionmap <- get_regionmap()
    is_available <- is_regionmap_available()

    # Join regionmap with istrax data
    # Rename region_code to ctry_code and region_name to country_name for compatibility with plotting functions
    if (is_available) {
      # Remove ctry_name from ddd if it exists to avoid duplicate column names after join
      ddd_for_join <- ddd %>% select(-any_of(c("ctry_name")))

      patchar_regionmap <- current_regionmap %>%
        left_join(ddd_for_join, by = c("docdb_family_id", "ctry_code")) %>%
        select(-ctry_code) %>%
        rename(ctry_code = region_code, country_name = region_name)
      message("patchar_regionmap columns after rename: ", paste(names(patchar_regionmap), collapse = ", "))
      message("patchar_regionmap rows: ", nrow(patchar_regionmap))
    } else {
      patchar_regionmap <- data.frame()
      message("patchar_regionmap is empty - regionmap not available")
    }

    patchar_regionmap
  })

  # Region Plot 1 - Returns by technology for selected regions
  output$avstrax_plot1_region <- renderGirafe({
    req(input$region, input$toflow_region, input$tech_categories_plot1_region,
        input$bwidthscale_region, input$display_mode_region, !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

    # Require big datasets for region explorer (regions always need on-the-fly computation or regionmap)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    # Check if regionmap is available
    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available. Please run prep_UK_regions.Rmd first."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Get current techmap
    current_techmap <- get_techmap()
    req(nrow(current_techmap) > 0)

    # Filter techmap based on selected technology categories
    selected_categories <- input$tech_categories_plot1_region
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    if(include_other && length(explicit_categories) > 0) {
      filtered_techmap <- current_techmap %>%
        mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
    } else if(include_other && length(explicit_categories) == 0) {
      filtered_techmap <- current_techmap %>%
        mutate(technology = "Other")
    } else {
      filtered_techmap <- current_techmap %>%
        filter(technology %in% explicit_categories)
    }

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
    height_inches <- width_inches * aspect_ratio

    p <- plot_avstrax_by_country(
      pdata = patchar_regionmap(),
      classes = filtered_techmap,
      country_code = selected_regions,
      toflow = input$toflow_region,
      custom_colors = custom_colors,
      colorings = colorings,
      bwidthscale = input$bwidthscale_region,
      display_mode = input$display_mode_region,
      show_top3_ids = input$show_top3_ids_region,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = sub("^[^.]*\\.", "", flow_label)
    )

    p
  })

  # Region Plot 2 - Returns by region for selected technologies
  output$avstrax_plot2_region <- renderGirafe({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$topn_region,
        input$mininno_region,
        input$bwidthscale_region,
        input$display_mode_region,
        !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    # Check if regionmap is available
    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available. Please run prep_UK_regions.Rmd first."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Try to load pre-computed data if available
    precomputed_data <- NULL

    # Check if we can use pre-computed data (no comparison technologies)
    if (is.null(input$techs_comparison_region) || length(input$techs_comparison_region) == 0) {
      # Try to match region selection to a pre-computed group
      # Try to match tech selection to a pre-computed category
      tech_category <- match_tech_category(input$techs_region)

      if (!is.null(tech_category)) {
        # Try to load pre-computed data for regions - by_region files are keyed by tech_category
        precomputed_data <- load_precomputed_by_region(prepdata_path, input$toflow_region, tech_category)
        if (!is.null(precomputed_data)) {
          message("Using pre-computed region data for tech: ", tech_category)
          # Filter to selected regions, BUT keep "All" row for average line
          if (!is.null(precomputed_data$ctry_code)) {
            precomputed_data <- precomputed_data %>%
              filter(ctry_code %in% selected_regions | ctry_code == "All")
          }
        }
      }
    }

    # Only load and merge full data if pre-computed data is not available
    if (is.null(precomputed_data)) {
      # Get region data and validate it has required columns
      region_data <- patchar_regionmap()
      has_ctry_code <- "ctry_code" %in% names(region_data)
      shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns. Please regenerate regionmap.fst."))

      # Filter by selected regions
      filtered <- region_data %>%
        filter(ctry_code %in% selected_regions)
    } else {
      filtered <- NULL
    }

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    # Calculate height based on number of regions to display (topn)
    n_regions <- input$topn_region
    height_per_bar <- 0.35  # inches per bar
    min_height <- 4  # minimum height in inches
    height_inches <- max(min_height, n_regions * height_per_bar)

    # Get current techmap
    current_techmap <- get_techmap()

    p <- plot_avstrax_by_technology(
      pdata = if(is.null(precomputed_data)) filtered else data.frame(),
      classes = current_techmap,
      technologies = input$techs_region,
      toflow = input$toflow_region,
      custom_colors = custom_colors,
      topn = input$topn_region,
      mininno = input$mininno_region,
      bwidthscale = input$bwidthscale_region,
      display_mode = input$display_mode_region,
      show_top3_ids = input$show_top3_ids_region,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = sub("^[^.]*\\.", "", flow_label),
      x_label = "Region",
      comparison_technologies = input$techs_comparison_region,
      precomputed_avstrax = precomputed_data
    )

    p
  })

  # UK Regions Map for Region Explorer
  output$uk_regions_map <- renderLeaflet({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_region)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    # Check if regionmap is available
    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    # Try to match tech selection to pre-computed data
    # UK regions map shows data BY region for a specific technology, so uses by_region files
    tech_category <- match_tech_category(input$techs_region)

    if (!is.null(tech_category)) {
      avstrax_data <- load_precomputed_by_region(prepdata_path, input$toflow_region, tech_category)
      if (!is.null(avstrax_data)) {
        message("UK regions map using pre-computed data for tech: ", tech_category)
        # Filter to selected regions
        if (!is.null(avstrax_data$ctry_code)) {
          avstrax_data <- avstrax_data %>%
            filter(ctry_code %in% selected_regions)
        }
      }
    }

    # If no pre-computed data, compute on the fly
    if (is.null(avstrax_data)) {
      # Require big datasets for on-the-fly computation
      req(data_state$techmap_loaded)

      # Get current techmap
      current_techmap <- get_techmap()

      # Filter by technology class
      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs_region) %>%
        distinct()

      if("All Innovations" %in% input$techs_region) filtered_classes <- data.frame()

      # Get region data and filter by selected regions
      region_data <- patchar_regionmap()
      has_ctry_code <- "ctry_code" %in% names(region_data)
      shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns."))

      filtered <- region_data %>%
        filter(ctry_code %in% selected_regions)

      # Compute aggregated data for all regions
      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow_region, filtered_classes)
    }

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_region)

    # Determine if this is a return (%) or spillover ($) variable
    is_return <- grepl("strax", input$toflow_region)

    plot_uk_regions_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      plot_title = paste0("UK Regions: ", sub("^[^.]*\\.", "", flow_label)),
      is_return = is_return
    )
  })

  # RTA Plot for Region Explorer (below returns plot)
  output$avstrax_plot2_region_rta <- renderGirafe({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$topn_rta_region,
        input$mininno_rta_region,
        input$minallinnos_rta_region,
        input$bwidthscale_region,
        !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Try to load pre-computed data if available
    precomputed_data <- NULL

    if (is.null(input$techs_comparison_region) || length(input$techs_comparison_region) == 0) {
      tech_category <- match_tech_category(input$techs_region)

      if (!is.null(tech_category)) {
        precomputed_data <- load_precomputed_by_region(prepdata_path, input$toflow_region, tech_category)
        if (!is.null(precomputed_data)) {
          # Check if RTA and Allinnos columns exist in precomputed data
          if (!"RTA" %in% names(precomputed_data) || !"Allinnos" %in% names(precomputed_data)) {
            message("RTA or Allinnos column not found in precomputed data, will compute on the fly")
            precomputed_data <- NULL
          } else {
            message("RTA region plot using pre-computed data for tech: ", tech_category)
            if (!is.null(precomputed_data$ctry_code)) {
              precomputed_data <- precomputed_data %>%
                filter(ctry_code %in% selected_regions | ctry_code == "All")
            }
          }
        }
      }
    }

    # Only load region data if precomputed data is not available
    if (is.null(precomputed_data)) {
      req(data_state$techmap_loaded)

      region_data <- patchar_regionmap()
      has_ctry_code <- "ctry_code" %in% names(region_data)
      shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns."))

      filtered <- region_data %>%
        filter(ctry_code %in% selected_regions)
    } else {
      filtered <- NULL
    }

    # Calculate responsive dimensions - use actual output width for proper scaling
    plot_width <- session$clientData$output_avstrax_plot2_region_rta_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600
    width_inches <- plot_width / 96
    # Calculate height based on number of regions to display (topn + bottomn)
    n_regions <- input$topn_rta_region + input$bottomn_rta_region
    height_per_bar <- 0.35  # inches per bar
    min_height <- 4  # minimum height in inches
    height_inches <- max(min_height, n_regions * height_per_bar)

    current_techmap <- get_techmap()

    # Build title with selected technologies
    tech_label <- paste(input$techs_region, collapse = ", ")
    rta_title <- paste0("RTA in ", tech_label, " by Region")

    p <- plot_avstrax_rta(
      pdata = if(is.null(precomputed_data)) filtered else data.frame(),
      classes = current_techmap,
      technologies = input$techs_region,
      toflow = input$toflow_region,
      custom_colors = custom_colors,
      topn = input$topn_rta_region,
      bottomn = input$bottomn_rta_region,
      mininno = input$mininno_rta_region,
      minallinnos = input$minallinnos_rta_region,
      bwidthscale = input$bwidthscale_region,
      show_top3_ids = input$show_top3_ids_region,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = rta_title,
      x_label = "Region",
      precomputed_avstrax = precomputed_data
    )

    p
  })

  # RTA vs Returns Scatter Plot for Region Explorer
  output$rta_returns_scatter_region <- renderGirafe({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_rta_region)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available."))

    selected_regions <- expand_region_selection(input$region)

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    tech_category <- match_tech_category(input$techs_region)

    if (!is.null(tech_category)) {
      precomputed <- load_precomputed_by_region(prepdata_path, input$toflow_region, tech_category)
      if (!is.null(precomputed) && "RTA" %in% names(precomputed) && "Allinnos" %in% names(precomputed)) {
        message("RTA scatter plot (regions) using pre-computed data for tech: ", tech_category)
        if (!is.null(precomputed$ctry_code)) {
          avstrax_data <- precomputed %>%
            filter(ctry_code %in% selected_regions | ctry_code == "All")
        } else {
          avstrax_data <- precomputed
        }
      }
    }

    # If no pre-computed data with RTA and Allinnos, compute on the fly
    if (is.null(avstrax_data)) {
      req(data_state$techmap_loaded)

      current_techmap <- get_techmap()

      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs_region) %>%
        distinct()

      if("All Innovations" %in% input$techs_region) filtered_classes <- data.frame()

      region_data <- patchar_regionmap()
      has_ctry_code <- "ctry_code" %in% names(region_data)
      shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns."))

      filtered <- region_data %>%
        filter(ctry_code %in% selected_regions)

      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow_region, filtered_classes)
    }

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_returns_scatter_region_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs_region, collapse = ", ")
    scatter_title <- paste0("RTA vs Returns: ", tech_label)

    p <- plot_rta_returns_scatter(
      avstrax_data = avstrax_data,
      mininno = input$mininno_rta_region,
      minallinnos = input$minallinnos_rta_region,
      bwidthscale = input$bwidthscale_region,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = scatter_title,
      x_label = "RTA",
      y_label = "Return (%)"
    )

    p
  })

  # RTA UK Regions Map for Region Explorer
  output$uk_regions_map_rta <- renderLeaflet({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_rta_region)
    req(data_state$regionmap_loaded || data_state$loading_complete)

    shiny::validate(shiny::need(is_regionmap_available(), "Region data not available."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Try to load pre-computed data if available
    avstrax_data <- NULL

    tech_category <- match_tech_category(input$techs_region)

    if (!is.null(tech_category)) {
      precomputed <- load_precomputed_by_region(prepdata_path, input$toflow_region, tech_category)
      if (!is.null(precomputed) && "RTA" %in% names(precomputed) && "Allinnos" %in% names(precomputed)) {
        message("RTA UK regions map using pre-computed data for tech: ", tech_category)
        if (!is.null(precomputed$ctry_code)) {
          avstrax_data <- precomputed %>%
            filter(ctry_code %in% selected_regions)
        } else {
          avstrax_data <- precomputed
        }
      }
    }

    # If no pre-computed data with RTA and Allinnos, compute on the fly
    if (is.null(avstrax_data)) {
      req(data_state$techmap_loaded)

      current_techmap <- get_techmap()

      filtered_classes <- current_techmap %>%
        filter(technology %in% input$techs_region) %>%
        distinct()

      if("All Innovations" %in% input$techs_region) filtered_classes <- data.frame()

      region_data <- patchar_regionmap()
      has_ctry_code <- "ctry_code" %in% names(region_data)
      shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns."))

      filtered <- region_data %>%
        filter(ctry_code %in% selected_regions)

      avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow_region, filtered_classes)
    }

    # Filter by minimum innovations (using RTA-specific thresholds)
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_rta_region)

    # Apply Allinnos filter if the column exists
    if ("Allinnos" %in% names(avstrax_data) && input$minallinnos_rta_region > 0) {
      avstrax_data <- avstrax_data %>%
        filter(Allinnos >= input$minallinnos_rta_region)
    }

    # Plot RTA on UK regions map (RTA is always an index)
    plot_uk_regions_map(
      avstrax_data = avstrax_data,
      value_col = "RTA",
      plot_title = "UK Regions: RTA Index",
      is_return = FALSE  # RTA is an index, not a percentage
    )
  })

}

# Run the app
shinyApp(ui = ui, server = server)
