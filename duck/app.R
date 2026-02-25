# ============================================================================
# ISE App - DuckDB Backend Version
# ============================================================================
# This is an alternative version of the Shiny app that uses DuckDB as its
# backend instead of loading large FST files into memory. All heavy data
# work (joins, filtering, aggregation) happens in DuckDB.
# ============================================================================

# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(ggplot2)
library(countrycode)
library(plotly)
library(dplyr)
library(data.table)
library(shinycssloaders)
library(ggiraph)
library(gdtools)
library(gfonts)
library(leaflet)
library(sf)
library(svglite)
library(DBI)
library(duckdb)

# Source helper files with relative paths
source("../dropbox_auth.R")
source("../istraxfunctions.R")
source("duck_queries.R")

# Register Google Font
register_gfont("Open Sans")

# ============================================================================
# Determine DuckDB path from Dropbox config
# ============================================================================

# Function to detect local Dropbox path from Dropbox's config file
get_dropbox_path <- function() {
  if (.Platform$OS.type == "windows") {
    info_paths <- c(
      file.path(Sys.getenv("APPDATA"), "Dropbox", "info.json"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
    )
  } else {
    info_paths <- file.path(Sys.getenv("HOME"), ".dropbox", "info.json")
  }

  for (p in info_paths) {
    if (file.exists(p)) {
      info <- jsonlite::fromJSON(p)
      if (!is.null(info$business)) {
        return(info$business$path)
      } else if (!is.null(info$personal)) {
        return(info$personal$path)
      }
    }
  }
  return(NULL)
}

# Try to find local Dropbox Apps/iseapp folder
get_local_iseapp_path <- function() {
  # First check environment variable
  env_path <- Sys.getenv("ISEAPP_PATH_LOCAL")
  if (nzchar(env_path) && dir.exists(env_path)) {
    return(env_path)
  }
  # Try to detect Dropbox path automatically
  dropbox_root <- get_dropbox_path()
  if (!is.null(dropbox_root)) {
    iseapp_path <- file.path(dropbox_root, "Apps", "iseapp")
    if (dir.exists(iseapp_path)) {
      return(iseapp_path)
    }
  }
  return(NULL)
}

# Get the local path (cached)
iseapp_local_path <- get_local_iseapp_path()
if (!is.null(iseapp_local_path)) {
  message("Using local Dropbox path: ", iseapp_local_path)
} else {
  message("Local Dropbox not found - DuckDB database must exist locally")
}

# Determine the DuckDB path — stored locally in the repo's duck/ folder
# (not in Dropbox, to avoid sync/corruption issues with cloud storage)
duck_db_path <- file.path(getwd(), "iseapp.duckdb")
if (!file.exists(duck_db_path)) {
  # Try relative to this script's location
  duck_db_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iseapp.duckdb")
}
if (!file.exists(duck_db_path)) {
  # Last resort: check the repo duck/ folder explicitly
  duck_db_path <- file.path(dirname(getwd()), "duck", "iseapp.duckdb")
}

if (!file.exists(duck_db_path)) {
  stop("DuckDB database not found. Expected iseapp.duckdb in the duck/ folder.",
       "\nPlease run prep_duckdb.Rmd first to build the database.")
}
message("DuckDB database path: ", duck_db_path)

# ============================================================================
# Make www/ assets available via addResourcePath
# ============================================================================
addResourcePath("www", file.path("..", "www"))

# ============================================================================
# Global setup (same as original app.R)
# ============================================================================

#rsconnect::writeManifest()
enableBookmarking(store = "url")

# Harmonize UI and plot fonts across environments
thematic::thematic_shiny(font = "Arial")

# Ensure figures default to a sans-serif font as well
theme_set(theme_minimal(base_family = "Arial"))
update_geom_defaults("text", list(family = "Arial"))
update_geom_defaults("label", list(family = "Arial"))


# ============================================================================
# Technology class definitions (identical to original)
# ============================================================================

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

# Known technology categories
all_techs <- c("All", green_classes, battery_classes, hard_to_abate_classes, ai_classes, cpc_sections, agrifood_classes)
all_techs <- unique(all_techs)

# Create grouped technology choices
green_classes_d        =setdiff(green_classes,"Green Technology")
battery_classes_d      =setdiff(battery_classes,"Battery Technology")
hard_to_abate_classes_d=setdiff(hard_to_abate_classes,"Hard to Abate Sector Decarbonization")
ai_classes_d           =setdiff(ai_classes,"AI")
agrifood_classes_d     =setdiff(agrifood_classes,"Any Agriculture & Food technology")


colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes,cpcsecs=cpc_sections,agrifood=agrifood_classes)

# Define custom colors
custom_colors <- c("green" = "forestgreen",
                   "battery" = "gold",
                   "other" = "gray70",
                   "hard to abate"="blue",
                   "AI"="orange",
                   "cpcsecs"="purple",
                   "agrifood"="burlywood")

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


# ============================================================================
# Firm choices from DuckDB
# ============================================================================

build_grouped_firm_choices <- function(firmsectormap_data) {
  if (is.null(firmsectormap_data) || nrow(firmsectormap_data) == 0) {
    return(list("All" = list("All" = "All")))
  }
  sectors <- sort(unique(firmsectormap_data$sector))
  # First group: Sectors (selecting a sector filters to all firms in that sector)
  sector_choices <- as.list(setNames(paste0("sector:", sectors), sectors))
  grouped <- list("All" = list("All" = "All"), "Sectors" = sector_choices)
  # Then for each sector, a sub-group listing individual firms sorted by RnD (descending)
  for (s in sectors) {
    firms_in_sector <- firmsectormap_data %>%
      filter(sector == s) %>%
      arrange(desc(RnD)) %>%
      pull(company_raw)
    grouped[[s]] <- as.list(setNames(firms_in_sector, firms_in_sector))
  }
  grouped
}

# Get firm data from DuckDB for UI choices
duck_startup_con <- duck_connect(duck_db_path)
firmsectormap_data <- duck_get_firmsectormap(duck_startup_con)
grouped_firms <- build_grouped_firm_choices(firmsectormap_data)
dbDisconnect(duck_startup_con, shutdown = FALSE)


# ============================================================================
# Flow type choices (identical to original)
# ============================================================================

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


# ============================================================================
# Country and region definitions (identical to original)
# ============================================================================

available_iso2 <- sort(unique(na.omit(countrycode::codelist$iso2c)))
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

# Helper to create download button pair for a plot
plot_download_buttons <- function(plot_id) {
  tags$div(
    class = "plot-download-buttons",
    downloadButton(paste0("dl_svg_", plot_id), "SVG", class = "btn-sm btn-outline-secondary"),
    downloadButton(paste0("dl_csv_", plot_id), "Data (CSV)", class = "btn-sm btn-outline-secondary")
  )
}

# Helper to create download button pair for maps (PDF vector format + CSV)
map_download_buttons <- function(plot_id) {
  tags$div(
    class = "plot-download-buttons",
    downloadButton(paste0("dl_pdf_", plot_id), "PDF", class = "btn-sm btn-outline-secondary"),
    downloadButton(paste0("dl_csv_", plot_id), "Data (CSV)", class = "btn-sm btn-outline-secondary")
  )
}

# ============================================================================
# Define UI (IDENTICAL to the original app.R)
# ============================================================================
ui <- function(request){fluidPage(
  # Add Google Font
  addGFontHtmlDependency(family = "Open Sans"),

  # Add custom CSS
  tags$head(
    tags$script(async = NA,
                src = "https://www.googletagmanager.com/gtag/js?id=G-XXXXXXXXXX"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-YY70D2F685');
    ")),
    tags$style(HTML("
      h1 {
        font-family: 'Courier New', monospace;
        font-size: 50px;
        font-weight: bold;
        color: #2C3E50;
      }
      .intro-text {
        font-family: 'Arial', sans-serif;
        font-size: 20px;
        color: #34495E;
        margin-bottom: 20px;
      }
      body {
        font-family: 'Open Sans', sans-serif;
      }
      svg text {
        font-family: 'Open Sans', sans-serif !important;
      }
    "))
  ),

  tags$div(
    style = "display: flex; align-items: center; gap: 20px;",
    tags$img(src = "prinzlogo.svg", height = "70px"),
    tags$h1("Welcome to ISE - The Innovation Strategy Explorer")
  ),

  tags$style(HTML("
  details {
    margin-bottom: 20px;
    font-family: 'Arial', sans-serif;
    font-size: 15px;
    color: #34495E;
  }
  summary.toggle-summary {
    font-weight: bold;
    font-size: 15px;
    cursor: pointer;
    padding: 10px;
    background-color: #ecf0f1;
    border: 1px solid #bdc3c7;
    border-radius: 5px;
    transition: background-color 0.3s ease;
  }
  summary.toggle-summary:hover {
    background-color: #d0d7de;
  }
")),


  #titlePanel("Welcome to ISE - The Innovation Strategy Explorer"),

  tags$details(
    tags$summary("▼ About this tool", class = "toggle-summary"),
    tags$br(),
    tags$p("This tool supports the development of an innovation strategy at various scopes
     for either governments or (impact) investors.
     It examines where marginal spillover from innovation are highest and
     thus there is a case for further investments in R&D.
     The tool builds on the methodology proposed in Guillard et al. ",
      tags$a(href = "https://cep.lse.ac.uk/_NEW/publications/abstract.asp?index=8614",
             target = "_blank", "Efficient Industrial Policy - Standing on the Shoulders of Hidden Giants."),
      " The figures show the returns from further investment in R&D in different technology areas and specific
     countries via knowledge spillovers; that is a return of 100% means that further R&D investment of 1000 Euro
     will lead to extra profits worth 1000 Euro for innovators different from the investor undertaking the additional spending.",
      tags$br(),
      "The methodology is informed by data from patents. Spillovers are derived from citations between patents. Crucially, the approach takes
     into account direct as well as indirect citations where one innovation is connected to another via a citation chain of any degree of separation.
     The private economic value of an innovation to an inventor is derived using the approach proposed by Kogan et al ",
      tags$a(href = "https://academic.oup.com/qje/article-abstract/132/2/665/3076284?redirectedFrom=fulltext",
             target = "_blank", "Technological Innovation, Resource Allocation, and Growth"),
      tags$br(),
      "You can display the average returns for different countries or country groups broken down by technology areas. You can also examine this for different scopes of spillovers.",
      "Global Returns takes into account spillover benefits to inventors anywhere. Returns LMICs only take into account spillover benefits to innovators in Low and Medium Income countries.",
      class = "intro-text"
    )
  ),

  bookmarkButton("Bookmark current data view...."),
  # Add CSS and JavaScript for collapsible plot
  tags$style(HTML("
    .plot-toggle {
      font-weight: 400;
      font-size: 11px;
      cursor: pointer;
      padding: 2px 8px;
      background-color: transparent;
      color: #888;
      border: none;
      border-radius: 3px;
      margin: 4px 0;
      transition: all 0.2s ease;
      display: inline-block;
      width: auto;
    }
    .plot-toggle:hover {
      background-color: rgba(0, 0, 0, 0.05);
      color: #555;
    }
    .plot-container {
      overflow: hidden;
      transition: max-height 0.3s ease;
    }
    .plot-download-buttons {
      display: flex;
      gap: 6px;
      margin: 4px 0 8px 0;
      justify-content: flex-end;
    }
    .plot-download-buttons .btn {
      font-size: 12px;
      padding: 2px 10px;
    }
  ")),

  tags$script(HTML("
    $(document).ready(function() {
      $('#togglePlot1').click(function() {
        var plot = $('#plot1Container');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
      $('#togglePlot1_region').click(function() {
        var plot = $('#plot1Container_region');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
      $('#toggleReturns').click(function() {
        var plot = $('#returnsContainer');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
      $('#toggleReturns_region').click(function() {
        var plot = $('#returnsContainer_region');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
      $('#toggleReturnsBar').click(function() {
        var plot = $('#returnsBarContainer');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
      $('#toggleReturnsBar_region').click(function() {
        var plot = $('#returnsBarContainer_region');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ more');
        } else {
          plot.slideDown(300);
          button.text('▲ less');
        }
      });
    });
  ")),

  # JavaScript to auto-update browser address bar with input values
  tags$script(HTML("
    $(document).ready(function() {
      // Function to build query string with current input values
      function buildQueryString() {
        var params = [];

        // Get current tab
        var currentTab = $('#main_tabs li.active a').attr('data-value') ||
                         $('#main_tabs .nav-link.active').attr('data-value') ||
                         'Country Explorer';
        params.push('_inputs_&main_tabs=' + encodeURIComponent('\"' + currentTab + '\"'));

        // Country Explorer inputs
        var country = getShinyInputValue('country');
        if (country) params.push('country=' + encodeURIComponent(JSON.stringify(country)));

        var toflow = getShinyInputValue('toflow');
        if (toflow) params.push('toflow=' + encodeURIComponent('\"' + toflow + '\"'));

        var techCat1 = getShinyInputValue('tech_categories_plot1');
        if (techCat1) params.push('tech_categories_plot1=' + encodeURIComponent(JSON.stringify(techCat1)));

        var bwidthscale = getShinyInputValue('bwidthscale');
        if (bwidthscale) params.push('bwidthscale=' + encodeURIComponent('\"' + bwidthscale + '\"'));

        var displayMode = getShinyInputValue('display_mode');
        if (displayMode) params.push('display_mode=' + encodeURIComponent('\"' + displayMode + '\"'));

        var showTop3 = getShinyInputValue('show_top3_ids');
        if (showTop3 !== null) params.push('show_top3_ids=' + encodeURIComponent(showTop3));

        var techs = getShinyInputValue('techs');
        if (techs) params.push('techs=' + encodeURIComponent(JSON.stringify(techs)));

        var techsComp = getShinyInputValue('techs_comparison');
        if (techsComp && techsComp.length > 0) params.push('techs_comparison=' + encodeURIComponent(JSON.stringify(techsComp)));

        var topn = getShinyInputValue('topn');
        if (topn) params.push('topn=' + encodeURIComponent(topn));

        var mininno = getShinyInputValue('mininno');
        if (mininno) params.push('mininno=' + encodeURIComponent(mininno));

        var firmFilter = getShinyInputValue('firm_filter');
        if (firmFilter) params.push('firm_filter=' + encodeURIComponent(JSON.stringify(firmFilter)));

        // Region Explorer inputs
        var region = getShinyInputValue('region');
        if (region) params.push('region=' + encodeURIComponent(JSON.stringify(region)));

        var toflowRegion = getShinyInputValue('toflow_region');
        if (toflowRegion) params.push('toflow_region=' + encodeURIComponent('\"' + toflowRegion + '\"'));

        var techCat1Region = getShinyInputValue('tech_categories_plot1_region');
        if (techCat1Region) params.push('tech_categories_plot1_region=' + encodeURIComponent(JSON.stringify(techCat1Region)));

        var bwidthscaleRegion = getShinyInputValue('bwidthscale_region');
        if (bwidthscaleRegion) params.push('bwidthscale_region=' + encodeURIComponent('\"' + bwidthscaleRegion + '\"'));

        var displayModeRegion = getShinyInputValue('display_mode_region');
        if (displayModeRegion) params.push('display_mode_region=' + encodeURIComponent('\"' + displayModeRegion + '\"'));

        var showTop3Region = getShinyInputValue('show_top3_ids_region');
        if (showTop3Region !== null) params.push('show_top3_ids_region=' + encodeURIComponent(showTop3Region));

        var techsRegion = getShinyInputValue('techs_region');
        if (techsRegion) params.push('techs_region=' + encodeURIComponent(JSON.stringify(techsRegion)));

        var techsCompRegion = getShinyInputValue('techs_comparison_region');
        if (techsCompRegion && techsCompRegion.length > 0) params.push('techs_comparison_region=' + encodeURIComponent(JSON.stringify(techsCompRegion)));

        var topnRegion = getShinyInputValue('topn_region');
        if (topnRegion) params.push('topn_region=' + encodeURIComponent(topnRegion));

        var mininnoRegion = getShinyInputValue('mininno_region');
        if (mininnoRegion) params.push('mininno_region=' + encodeURIComponent(mininnoRegion));

        var firmFilterRegion = getShinyInputValue('firm_filter_region');
        if (firmFilterRegion) params.push('firm_filter_region=' + encodeURIComponent(JSON.stringify(firmFilterRegion)));

        return '?' + params.join('&');
      }

      // Helper to get Shiny input values
      function getShinyInputValue(inputId) {
        if (Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) {
          return Shiny.shinyapp.$inputValues[inputId];
        }
        return null;
      }

      // Update the browser address bar
      function updateBrowserUrl() {
        var queryString = buildQueryString();
        var newUrl = window.location.pathname + queryString;
        history.replaceState(null, '', newUrl);
      }

      // Listen for any Shiny input changes
      $(document).on('shiny:inputchanged', function(event) {
        // Debounce URL updates
        clearTimeout(window.urlUpdateTimeout);
        window.urlUpdateTimeout = setTimeout(updateBrowserUrl, 300);
      });

      // Initial update after Shiny is connected
      $(document).on('shiny:connected', function() {
        setTimeout(updateBrowserUrl, 500);
      });
    });
  ")),

  # Spacer between bookmark button and tabs
  tags$div(style = "margin-top: 20px;"),

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
          options = list(placeholder = 'Choose one or more countries or groups...')
        ),
        selectizeInput(
          inputId = "toflow",
          label = "Return flow",
          choices = toflow_choices,
          selected = "istrax_global",
          multiple = FALSE,
          width = "400px",
          options = list(placeholder = 'Choose a return flow...')
        ),
        selectizeInput(
          inputId = "tech_categories_plot1",
          label = "Technology categories",
          choices = grouped_techs,
          selected = c("AI","Green Technology"),
          multiple = TRUE,
          width = "200%",
          options = list(placeholder = 'Choose one or more technology categories...')
        ),
        selectizeInput(
          inputId = "firm_filter",
          label = "Filter by firms",
          choices = grouped_firms,
          selected = "All",
          multiple = TRUE,
          width = "200%",
          options = list(placeholder = 'Choose firms or sectors to filter...')
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
        withSpinner(girafeOutput("avstrax_plot1", width = "100%", height = "auto"), type = 4, color = "#3498db"),
        plot_download_buttons("avstrax_plot1")
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
        plot_download_buttons("avstrax_plot2"),
        tags$button("▲ less", id = "toggleReturns", class = "plot-toggle"),
        tags$div(
          id = "returnsContainer",
          tags$br(),
          tags$h4("World Map: Returns"),
          withSpinner(plotlyOutput("world_map", width = "100%", height = "500px"), type = 4, color = "#3498db"),
          map_download_buttons("world_map")
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
          withSpinner(girafeOutput("avstrax_plot2_rta", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          plot_download_buttons("avstrax_plot2_rta")
        ),
        column(6,
          tags$h4("RTA vs Returns"),
          withSpinner(girafeOutput("rta_returns_scatter", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          plot_download_buttons("rta_returns_scatter"),
          tags$h4("RTA vs GDP per Capita"),
          withSpinner(girafeOutput("rta_gdp_scatter", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          plot_download_buttons("rta_gdp_scatter")
        )
      ),
      tags$br(),
      tags$h4("World Map: RTA"),
      withSpinner(plotlyOutput("world_map_rta", width = "100%", height = "500px"), type = 4, color = "#e74c3c"),
      map_download_buttons("world_map_rta")
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
        selectizeInput(
          inputId = "firm_filter_region",
          label = "Filter by firms",
          choices = grouped_firms,
          selected = "All",
          multiple = TRUE,
          width = "200%",
          options = list(placeholder = 'Choose firms or sectors to filter...')
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
        withSpinner(girafeOutput("avstrax_plot1_region", width = "100%", height = "auto"), type = 4, color = "#3498db"),
        plot_download_buttons("avstrax_plot1_region")
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
        plot_download_buttons("avstrax_plot2_region"),
        tags$button("▲ less", id = "toggleReturns_region", class = "plot-toggle"),
        tags$div(
          id = "returnsContainer_region",
          tags$br(),
          tags$h4("UK Regions Map: Returns"),
          withSpinner(leafletOutput("uk_regions_map", width = "100%", height = "500px"), type = 4, color = "#3498db"),
          map_download_buttons("uk_regions_map")
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
          withSpinner(girafeOutput("avstrax_plot2_region_rta", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          plot_download_buttons("avstrax_plot2_region_rta")
        ),
        column(6,
          tags$h4("RTA vs Returns"),
          withSpinner(girafeOutput("rta_returns_scatter_region", width = "100%", height = "auto"), type = 4, color = "#e74c3c"),
          plot_download_buttons("rta_returns_scatter_region")
        )
      ),
      tags$br(),
      tags$h4("UK Regions Map: RTA"),
      withSpinner(leafletOutput("uk_regions_map_rta", width = "100%", height = "500px"), type = 4, color = "#e74c3c"),
      map_download_buttons("uk_regions_map_rta")
    )
  )
)
}



# ============================================================================
# Define server (DuckDB backend)
# ============================================================================
server <- function(input, output, session) {

  # ============================================================================
  # DuckDB connection per session
  # ============================================================================
  duck_con <- duck_connect(duck_db_path)
  onSessionEnded(function() {
    tryCatch(
      dbDisconnect(duck_con, shutdown = FALSE),
      error = function(e) message("Error disconnecting DuckDB: ", e$message)
    )
  })

  # Reactive values for window dimensions
  window_dims <- reactiveValues(width = 800, height = 600, initialized = FALSE)

  # Store ggplot objects and plot data for downloads
  plot_store <- reactiveValues()

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


  # ============================================================================
  # Plot 1: Returns by Technology (Country Explorer)
  # ============================================================================
  output$avstrax_plot1 <- renderGirafe({
    req(input$country, input$toflow, input$tech_categories_plot1, input$bwidthscale, input$display_mode, !is.null(input$show_top3_ids))
    req(window_dims$initialized)

    selected_countries <- expand_country_selection(input$country)

    # Get the label from the nested toflow_choices list
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Resolve firm filter
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter)

    # Handle "Other" relabeling
    selected_categories <- input$tech_categories_plot1
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    other_label <- if (include_other) "Other" else NULL

    # Use duck_compute_avstrax for DuckDB aggregation
    precomputed <- duck_compute_avstrax(
      duck_con, input$toflow,
      tech_categories = if (length(explicit_categories) > 0) explicit_categories else NULL,
      country_codes = selected_countries,
      firm_names = firm_names,
      other_label = other_label,
      colorings = colorings
    )

    # Calculate responsive dimensions
    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
    height_inches <- width_inches * aspect_ratio

    result <- plot_avstrax_by_country(
      pdata = NULL,
      classes = NULL,
      country_code = selected_countries,
      toflow = input$toflow,
      custom_colors = custom_colors,
      colorings = colorings,
      bwidthscale = input$bwidthscale,
      display_mode = input$display_mode,
      show_top3_ids = input$show_top3_ids,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = sub("^[^.]*\\.", "", flow_label),
      precomputed_data = precomputed
    )

    plot_store$avstrax_plot1_gg <- result$ggplot
    plot_store$avstrax_plot1_data <- result$plot_data
    result$girafe
  })


  # ============================================================================
  # Plot 2: Returns by Country (Country Explorer)
  # ============================================================================
  output$avstrax_plot2 <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$topn,
        input$mininno,
        input$bwidthscale,
        input$display_mode,
        !is.null(input$show_top3_ids))
    req(window_dims$initialized)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter)

    # Main technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    precomputed <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = firm_names
    )

    # Handle comparison technologies
    has_comparison <- !is.null(input$techs_comparison) && length(input$techs_comparison) > 0

    # Calculate responsive dimensions
    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    n_countries <- input$topn
    height_per_bar <- 0.35
    min_height <- 4
    height_inches <- max(min_height, n_countries * height_per_bar)

    # For comparison, compute separately if needed
    comparison_techs_arg <- NULL
    if (has_comparison && is.null(firm_names)) {
      # With DuckDB precomputed data, comparison is not directly supported in the precomputed path
      # We pass comparison_technologies = NULL since we use precomputed_avstrax
      comparison_techs_arg <- NULL
    } else if (has_comparison) {
      comparison_techs_arg <- NULL
    }

    result <- plot_avstrax_by_technology(
      pdata = NULL,
      classes = NULL,
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
      comparison_technologies = comparison_techs_arg,
      precomputed_avstrax = precomputed
    )

    plot_store$avstrax_plot2_gg <- result$ggplot
    plot_store$avstrax_plot2_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # World Map (Country Explorer)
  # ============================================================================
  output$world_map <- renderPlotly({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter)

    # Technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = firm_names
    )

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno)

    # Determine if this is a return (%) or spillover ($) variable
    is_return <- grepl("strax", input$toflow)
    map_title <- paste0("World Map: ", sub("^[^.]*\\.", "", flow_label))

    # Store ggplot version and data for PDF/CSV downloads
    plot_store$world_map_gg <- plot_world_map_gg(
      avstrax_data = avstrax_data, value_col = "mean",
      plot_title = map_title, is_return = is_return
    )
    plot_store$world_map_data <- avstrax_data

    plot_world_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      color_scale = "Viridis",
      plot_title = map_title,
      is_return = is_return
    )
  })

  # ============================================================================
  # RTA Plot (Country Explorer)
  # ============================================================================
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

    # Technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    precomputed_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = NULL  # RTA not affected by firm filter in original
    )

    # Calculate responsive dimensions
    plot_width <- session$clientData$output_avstrax_plot2_rta_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600
    width_inches <- plot_width / 96
    n_countries <- input$topn_rta + input$bottomn_rta
    height_per_bar <- 0.35
    min_height <- 4
    height_inches <- max(min_height, n_countries * height_per_bar)

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    rta_title <- paste0("RTA in ", tech_label, " by Country")

    result <- plot_avstrax_rta(
      pdata = NULL,
      classes = NULL,
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

    plot_store$avstrax_plot2_rta_gg <- result$ggplot
    plot_store$avstrax_plot2_rta_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # RTA World Map (Country Explorer)
  # ============================================================================
  output$world_map_rta <- renderPlotly({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = NULL
    )

    # Filter by minimum innovations (using RTA-specific thresholds)
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_rta)

    # Apply Allinnos filter if the column exists
    if ("Allinnos" %in% names(avstrax_data) && input$minallinnos_rta > 0) {
      avstrax_data <- avstrax_data %>%
        filter(Allinnos >= input$minallinnos_rta)
    }

    # Store ggplot version and data for PDF/CSV downloads
    plot_store$world_map_rta_gg <- plot_world_map_gg(
      avstrax_data = avstrax_data, value_col = "RTA",
      plot_title = "World Map: RTA Index", is_return = FALSE
    )
    plot_store$world_map_rta_data <- avstrax_data

    # Plot RTA on world map
    plot_world_map(
      avstrax_data = avstrax_data,
      value_col = "RTA",
      color_scale = "RdYlGn",
      plot_title = "World Map: RTA Index",
      is_return = FALSE
    )
  })

  # ============================================================================
  # RTA vs Returns Scatter Plot (Country Explorer)
  # ============================================================================
  output$rta_returns_scatter <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)

    # Technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = NULL
    )

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_returns_scatter_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    scatter_title <- paste0("RTA vs Returns: ", tech_label)

    result <- plot_rta_returns_scatter(
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

    plot_store$rta_returns_scatter_gg <- result$ggplot
    plot_store$rta_returns_scatter_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # RTA vs GDP per Capita Scatter Plot (Country Explorer)
  # ============================================================================
  output$rta_gdp_scatter <- renderGirafe({
    req(input$country,
        input$toflow,
        input$techs,
        input$mininno_rta)

    selected_countries <- expand_country_selection(input$country)

    # Technology filter
    tech_filter <- input$techs
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow,
      tech_filter = tech_filter,
      country_codes = selected_countries,
      firm_names = NULL
    )

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_gdp_scatter_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs, collapse = ", ")
    scatter_title <- paste0("RTA vs GDP per Capita: ", tech_label)

    result <- plot_rta_gdp_scatter(
      avstrax_data = avstrax_data,
      mininno = input$mininno_rta,
      minallinnos = input$minallinnos_rta,
      bwidthscale = input$bwidthscale,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = scatter_title
    )

    plot_store$rta_gdp_scatter_gg <- result$ggplot
    plot_store$rta_gdp_scatter_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # Region Explorer Tab - Server Logic
  # ============================================================================

  # ============================================================================
  # Region Plot 1 - Returns by technology for selected regions
  # ============================================================================
  output$avstrax_plot1_region <- renderGirafe({
    req(input$region, input$toflow_region, input$tech_categories_plot1_region,
        input$bwidthscale_region, input$display_mode_region, !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Resolve firm filter
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter_region)

    # Handle "Other" relabeling
    selected_categories <- input$tech_categories_plot1_region
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    other_label <- if (include_other) "Other" else NULL

    # Use duck_compute_avstrax for DuckDB aggregation with regionmap
    precomputed <- duck_compute_avstrax(
      duck_con, input$toflow_region,
      tech_categories = if (length(explicit_categories) > 0) explicit_categories else NULL,
      country_codes = selected_regions,
      firm_names = firm_names,
      other_label = other_label,
      colorings = colorings,
      use_regionmap = TRUE
    )

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    aspect_ratio <- ifelse(plot_width > 1200, 0.5, ifelse(plot_width > 800, 0.6, 0.7))
    height_inches <- width_inches * aspect_ratio

    result <- plot_avstrax_by_country(
      pdata = NULL,
      classes = NULL,
      country_code = selected_regions,
      toflow = input$toflow_region,
      custom_colors = custom_colors,
      colorings = colorings,
      bwidthscale = input$bwidthscale_region,
      display_mode = input$display_mode_region,
      show_top3_ids = input$show_top3_ids_region,
      width_svg = width_inches,
      height_svg = height_inches,
      plot_title = sub("^[^.]*\\.", "", flow_label),
      precomputed_data = precomputed
    )

    plot_store$avstrax_plot1_region_gg <- result$ggplot
    plot_store$avstrax_plot1_region_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # Region Plot 2 - Returns by region for selected technologies
  # ============================================================================
  output$avstrax_plot2_region <- renderGirafe({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$topn_region,
        input$mininno_region,
        input$bwidthscale_region,
        input$display_mode_region,
        !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter_region)

    # Technology filter
    tech_filter <- input$techs_region
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    precomputed <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow_region,
      tech_filter = tech_filter,
      country_codes = selected_regions,
      firm_names = firm_names,
      use_regionmap = TRUE
    )

    # Handle comparison technologies
    has_comparison <- !is.null(input$techs_comparison_region) && length(input$techs_comparison_region) > 0
    comparison_techs_arg <- NULL

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    n_regions <- input$topn_region
    height_per_bar <- 0.35
    min_height <- 4
    height_inches <- max(min_height, n_regions * height_per_bar)

    result <- plot_avstrax_by_technology(
      pdata = NULL,
      classes = NULL,
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
      comparison_technologies = comparison_techs_arg,
      precomputed_avstrax = precomputed
    )

    plot_store$avstrax_plot2_region_gg <- result$ggplot
    plot_store$avstrax_plot2_region_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # UK Regions Map (Region Explorer)
  # ============================================================================
  output$uk_regions_map <- renderLeaflet({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_region)

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]
    firm_names <- resolve_firm_filter(duck_con, input$firm_filter_region)

    # Technology filter
    tech_filter <- input$techs_region
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow_region,
      tech_filter = tech_filter,
      country_codes = selected_regions,
      firm_names = firm_names,
      use_regionmap = TRUE
    )

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_region)

    # Determine if this is a return (%) or spillover ($) variable
    is_return <- grepl("strax", input$toflow_region)
    map_title <- paste0("UK Regions: ", sub("^[^.]*\\.", "", flow_label))

    # Store ggplot version and data for PDF/CSV downloads
    plot_store$uk_regions_map_gg <- plot_uk_regions_map_gg(
      avstrax_data = avstrax_data, value_col = "mean",
      plot_title = map_title, is_return = is_return
    )
    plot_store$uk_regions_map_data <- avstrax_data

    plot_uk_regions_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      plot_title = map_title,
      is_return = is_return
    )
  })

  # ============================================================================
  # RTA Plot (Region Explorer)
  # ============================================================================
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

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Technology filter
    tech_filter <- input$techs_region
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    precomputed_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow_region,
      tech_filter = tech_filter,
      country_codes = selected_regions,
      firm_names = NULL,
      use_regionmap = TRUE
    )

    # Calculate responsive dimensions
    plot_width <- session$clientData$output_avstrax_plot2_region_rta_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600
    width_inches <- plot_width / 96
    n_regions <- input$topn_rta_region + input$bottomn_rta_region
    height_per_bar <- 0.35
    min_height <- 4
    height_inches <- max(min_height, n_regions * height_per_bar)

    # Build title with selected technologies
    tech_label <- paste(input$techs_region, collapse = ", ")
    rta_title <- paste0("RTA in ", tech_label, " by Region")

    result <- plot_avstrax_rta(
      pdata = NULL,
      classes = NULL,
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

    plot_store$avstrax_plot2_region_rta_gg <- result$ggplot
    plot_store$avstrax_plot2_region_rta_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # RTA vs Returns Scatter Plot (Region Explorer)
  # ============================================================================
  output$rta_returns_scatter_region <- renderGirafe({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_rta_region)

    selected_regions <- expand_region_selection(input$region)

    # Technology filter
    tech_filter <- input$techs_region
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow_region,
      tech_filter = tech_filter,
      country_codes = selected_regions,
      firm_names = NULL,
      use_regionmap = TRUE
    )

    # Get plot dimensions
    plot_width <- session$clientData$output_rta_returns_scatter_region_width
    if (is.null(plot_width) || plot_width < 100) plot_width <- 600

    width_inches <- plot_width / 96
    aspect_ratio <- 0.8
    height_inches <- width_inches * aspect_ratio

    # Build title with selected technologies
    tech_label <- paste(input$techs_region, collapse = ", ")
    scatter_title <- paste0("RTA vs Returns: ", tech_label)

    result <- plot_rta_returns_scatter(
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

    plot_store$rta_returns_scatter_region_gg <- result$ggplot
    plot_store$rta_returns_scatter_region_data <- result$plot_data
    result$girafe
  })

  # ============================================================================
  # RTA UK Regions Map (Region Explorer)
  # ============================================================================
  output$uk_regions_map_rta <- renderLeaflet({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_rta_region)

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Technology filter
    tech_filter <- input$techs_region
    if ("All Innovations" %in% tech_filter) tech_filter <- NULL

    avstrax_data <- duck_compute_avstrax_for_techs(
      duck_con, input$toflow_region,
      tech_filter = tech_filter,
      country_codes = selected_regions,
      firm_names = NULL,
      use_regionmap = TRUE
    )

    # Filter by minimum innovations (using RTA-specific thresholds)
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_rta_region)

    # Apply Allinnos filter if the column exists
    if ("Allinnos" %in% names(avstrax_data) && input$minallinnos_rta_region > 0) {
      avstrax_data <- avstrax_data %>%
        filter(Allinnos >= input$minallinnos_rta_region)
    }

    # Store ggplot version and data for PDF/CSV downloads
    plot_store$uk_regions_map_rta_gg <- plot_uk_regions_map_gg(
      avstrax_data = avstrax_data, value_col = "RTA",
      plot_title = "UK Regions: RTA Index", is_return = FALSE
    )
    plot_store$uk_regions_map_rta_data <- avstrax_data

    # Plot RTA on UK regions map
    plot_uk_regions_map(
      avstrax_data = avstrax_data,
      value_col = "RTA",
      plot_title = "UK Regions: RTA Index",
      is_return = FALSE
    )
  })

  # ============================================
  # Download Handlers (SVG + CSV) for all plots
  # ============================================

  make_svg_handler <- function(plot_reactive, filename_prefix) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".svg"),
      content = function(file) {
        p <- plot_reactive()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, device = "svg", width = 10, height = 6)
        }
      }
    )
  }

  make_pdf_handler <- function(plot_reactive, filename_prefix, width = 12, height = 7) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        p <- plot_reactive()
        if (!is.null(p)) {
          tryCatch(
            ggplot2::ggsave(file, plot = p, device = "pdf", width = width, height = height),
            error = function(e) {
              message("PDF export error for ", filename_prefix, ": ", e$message)
            }
          )
        } else {
          message("PDF export: plot object is NULL for ", filename_prefix)
        }
      }
    )
  }

  make_csv_handler <- function(data_reactive, filename_prefix) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- data_reactive()
        if (!is.null(d)) {
          write.csv(d, file, row.names = FALSE)
        }
      }
    )
  }

  # Country Explorer downloads
  output$dl_svg_avstrax_plot1 <- make_svg_handler(
    reactive(plot_store$avstrax_plot1_gg), "returns_by_technology")
  output$dl_csv_avstrax_plot1 <- make_csv_handler(
    reactive(plot_store$avstrax_plot1_data), "returns_by_technology_data")

  output$dl_svg_avstrax_plot2 <- make_svg_handler(
    reactive(plot_store$avstrax_plot2_gg), "returns_by_country")
  output$dl_csv_avstrax_plot2 <- make_csv_handler(
    reactive(plot_store$avstrax_plot2_data), "returns_by_country_data")

  output$dl_svg_avstrax_plot2_rta <- make_svg_handler(
    reactive(plot_store$avstrax_plot2_rta_gg), "rta_by_country")
  output$dl_csv_avstrax_plot2_rta <- make_csv_handler(
    reactive(plot_store$avstrax_plot2_rta_data), "rta_by_country_data")

  output$dl_svg_rta_returns_scatter <- make_svg_handler(
    reactive(plot_store$rta_returns_scatter_gg), "rta_vs_returns")
  output$dl_csv_rta_returns_scatter <- make_csv_handler(
    reactive(plot_store$rta_returns_scatter_data), "rta_vs_returns_data")

  output$dl_svg_rta_gdp_scatter <- make_svg_handler(
    reactive(plot_store$rta_gdp_scatter_gg), "rta_vs_gdp")
  output$dl_csv_rta_gdp_scatter <- make_csv_handler(
    reactive(plot_store$rta_gdp_scatter_data), "rta_vs_gdp_data")

  # Region Explorer downloads
  output$dl_svg_avstrax_plot1_region <- make_svg_handler(
    reactive(plot_store$avstrax_plot1_region_gg), "returns_by_technology_region")
  output$dl_csv_avstrax_plot1_region <- make_csv_handler(
    reactive(plot_store$avstrax_plot1_region_data), "returns_by_technology_region_data")

  output$dl_svg_avstrax_plot2_region <- make_svg_handler(
    reactive(plot_store$avstrax_plot2_region_gg), "returns_by_region")
  output$dl_csv_avstrax_plot2_region <- make_csv_handler(
    reactive(plot_store$avstrax_plot2_region_data), "returns_by_region_data")

  output$dl_svg_avstrax_plot2_region_rta <- make_svg_handler(
    reactive(plot_store$avstrax_plot2_region_rta_gg), "rta_by_region")
  output$dl_csv_avstrax_plot2_region_rta <- make_csv_handler(
    reactive(plot_store$avstrax_plot2_region_rta_data), "rta_by_region_data")

  output$dl_svg_rta_returns_scatter_region <- make_svg_handler(
    reactive(plot_store$rta_returns_scatter_region_gg), "rta_vs_returns_region")
  output$dl_csv_rta_returns_scatter_region <- make_csv_handler(
    reactive(plot_store$rta_returns_scatter_region_data), "rta_vs_returns_region_data")

  # Map downloads (PDF vector format + CSV)
  output$dl_pdf_world_map <- make_pdf_handler(
    reactive(plot_store$world_map_gg), "world_map_returns")
  output$dl_csv_world_map <- make_csv_handler(
    reactive(plot_store$world_map_data), "world_map_returns_data")

  output$dl_pdf_world_map_rta <- make_pdf_handler(
    reactive(plot_store$world_map_rta_gg), "world_map_rta")
  output$dl_csv_world_map_rta <- make_csv_handler(
    reactive(plot_store$world_map_rta_data), "world_map_rta_data")

  output$dl_pdf_uk_regions_map <- make_pdf_handler(
    reactive(plot_store$uk_regions_map_gg), "uk_regions_map_returns")
  output$dl_csv_uk_regions_map <- make_csv_handler(
    reactive(plot_store$uk_regions_map_data), "uk_regions_map_returns_data")

  output$dl_pdf_uk_regions_map_rta <- make_pdf_handler(
    reactive(plot_store$uk_regions_map_rta_gg), "uk_regions_map_rta")
  output$dl_csv_uk_regions_map_rta <- make_csv_handler(
    reactive(plot_store$uk_regions_map_rta_data), "uk_regions_map_rta_data")

}

# Run the app
shinyApp(ui = ui, server = server)
