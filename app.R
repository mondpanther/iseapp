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
source("dropbox_auth.R")


# Register Google Font
register_gfont("Open Sans")

# Function to detect local Dropbox path from Dropbox's config file
get_dropbox_path <- function() {
  # Dropbox stores its configuration in info.json
  # This works for both personal and team Dropbox accounts

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
  message("Local Dropbox not found, will use online Dropbox")
}

localpath_fname <- function(fname) {
  if (!is.null(iseapp_local_path)) {
    fname <- sub("^/+", "", fname)
    return(file.path(iseapp_local_path, fname))
  }
  return("")
}


pp=localpath_fname("/techmap.fst")
if(file.exists(pp)){ 
  techmap <- read_fst(pp)
} else {
  techmap <- dropbox_read_fst("/techmap.fst")}




#techmap <- db("/techmap.fst", token)


#rsconnect::writeManifest()
enableBookmarking(store = "url")

# Harmonize UI and plot fonts across environments
thematic::thematic_shiny(font = "Arial")

# Ensure figures default to a sans-serif font as well
theme_set(theme_minimal(base_family = "Arial"))
update_geom_defaults("text", list(family = "Arial"))
update_geom_defaults("label", list(family = "Arial"))

# Load data

#files <- list.files(path="istraxes", pattern = "parquet$", full.names = TRUE)
#countrymap <- read_fst("countrymap.fst")
#countrymap <- dropbox_read_fst("/countrymap.fst")
pp=localpath_fname("/countrymap.fst")
if(file.exists(pp)){
  countrymap <- read_fst(pp)
} else {
  countrymap <- dropbox_read_fst("/countrymap.fst")}

# Load regionmap for Region Explorer tab
pp_region=localpath_fname("/regionmap.fst")
if(file.exists(pp_region)){
  regionmap <- read_fst(pp_region)
  message("Regionmap loaded locally with ", nrow(regionmap), " rows")
} else {
  regionmap <- dropbox_read_fst("/regionmap.fst")
  message("Regionmap loaded from Dropbox with ", nrow(regionmap), " rows")
}
regionmap_available <- !is.null(regionmap) && nrow(regionmap) > 0
message("Regionmap columns: ", paste(names(regionmap), collapse = ", "))




#for (ff in files) {
#  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
#}

#techmap <- read_fst("techmap.fst")



#df <- reactive({
#  url <- "https://www.dropbox.com/scl/fi/j09lnxxd2wa2e1rlkywtd/techmap.fst?rlkey=rhq6w51bh9bzqz8rywmlwfuqj&st=f0napf4g&dl=1"
  
#  temp_file <- tempfile(fileext = ".fst")
#  download.file(url, temp_file, mode = "wb", quiet = TRUE)
#  techmap <- read_fst(temp_file)
#  unlink(temp_file)
#})


#temp_file <- tempfile(fileext = ".fst")
#drop_download("/techmap.fst", local_path = temp_file, overwrite = TRUE)
#techmap <- read_fst(temp_file)
#unlink(temp_file)


#techmap %>% distinct(technology) %>% pull(technology)

techmap=countrymap %>%
  select(docdb_family_id) %>% 
  distinct() %>% 
  mutate(technology = "All") %>% bind_rows(techmap)

# Correct
setDT(techmap)
techmap[, technology := fcase(
  technology == "Any Green technology", "Green Technology",
  technology == "Any battery technology", "Battery Technology",
  technology == "Any Hard to Abate technology", "Hard to Abate Sector Decarbonization",
  default = technology
)]

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

source("istraxfunctions.R")

# Get all unique technologies from techmap
all_techs <- c((techmap %>% distinct(technology))$technology, "All")

# Create grouped technology choices
# Separate technologies into green, battery, and other categories


green_classes_d        =setdiff(green_classes,"Green Technology")
battery_classes_d      =setdiff(battery_classes,"Battery Technology")
hard_to_abate_classes_d=setdiff(hard_to_abate_classes,"Hard to Abate Sector Decarbonization")
ai_classes_d           =setdiff(ai_classes,"AI")


colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes,cpcsecs=cpc_sections)


other_techs <- c(setdiff(all_techs, c(green_classes, battery_classes,hard_to_abate_classes,cpc_sections)),"Green Technology","Battery Technology","Hard to Abate Sector Decarbonization")

grouped_techs <- list(
  "Broad Technology Categories"                         = as.list(setNames(other_techs, other_techs)),
  "Detailed Green technologies"                         = as.list(setNames(green_classes_d, green_classes_d)),
  "AI subcategories"                                    = as.list(setNames(ai_classes_d, ai_classes_d)),
  "Detailed Battery technologies"                       = as.list(setNames(battery_classes_d, battery_classes_d)),
  "Detailed Hard to Abate Sector Decarbonization Technologies" = as.list(setNames(hard_to_abate_classes_d, hard_to_abate_classes_d)),
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
  
  tags$h1("Welcome to ISE - The Innovation Strategy Explorer"),
  
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
      font-weight: bold;
      font-size: 16px;
      cursor: pointer;
      padding: 10px;
      background-color: #3498db;
      color: white;
      border: none;
      border-radius: 5px;
      margin: 10px 0;
      transition: background-color 0.3s ease;
      display: inline-block;
      width: auto;
    }
    .plot-toggle:hover {
      background-color: #2980b9;
    }
    .plot-container {
      overflow: hidden;
      transition: max-height 0.3s ease;
    }
  ")),

  tags$script(HTML("
    $(document).ready(function() {
      $('#togglePlot1').click(function() {
        var plot = $('#plot1Container');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ More');
        } else {
          plot.slideDown(300);
          button.text('▲ Less');
        }
      });
      $('#togglePlot1_region').click(function() {
        var plot = $('#plot1Container_region');
        var button = $(this);
        if (plot.is(':visible')) {
          plot.slideUp(300);
          button.text('▼ More');
        } else {
          plot.slideDown(300);
          button.text('▲ Less');
        }
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
          selected = c("Other","AI","Green Technology"),
          multiple = TRUE,
          width = "200%",
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
      tags$button("▲ Less", id = "togglePlot1", class = "plot-toggle"),
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
      withSpinner(girafeOutput("avstrax_plot2", width = "100%", height = "auto"), type = 4, color = "#3498db"),
      tags$br(),
      tags$h4("World Map View"),
      withSpinner(plotlyOutput("world_map", width = "100%", height = "500px"), type = 4, color = "#3498db")
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
          selected = c("Other","AI","Green Technology"),
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
      tags$button("▲ Less", id = "togglePlot1_region", class = "plot-toggle"),
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
      withSpinner(girafeOutput("avstrax_plot2_region", width = "100%", height = "auto"), type = 4, color = "#3498db"),
      tags$br(),
      tags$h4("UK Regions Map View"),
      withSpinner(leafletOutput("uk_regions_map", width = "100%", height = "500px"), type = 4, color = "#3498db")
    )
  )
)
}

  
  
# Define server
server <- function(input, output, session) {

  # Reactive values for window dimensions
  window_dims <- reactiveValues(width = 800, height = 600, initialized = FALSE)

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
  
  #colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes)
  
  
  
  #for (ff in files) {
  #  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
  #}
  
  
  patchar_countrymap <- reactive({
    req(input$toflow)
    
    #input=list(toflow="avstrax_global")
    path <- paste0("/istraxes/", input$toflow,".fst")

    
    pp=localpath_fname(path)
    if(file.exists(pp)){ 
      ddd <- read_fst(pp)
    } else {
      ddd <- dropbox_read_fst(path)}
    
    
    
    
    #patchar_countrymap <- countrymap %>% left_join(read_fst(path))
    patchar_countrymap <- countrymap %>% left_join(ddd)
    
  })

  
  
  
  output$avstrax_plot1 <- renderGirafe({
    req(input$country, input$toflow, input$tech_categories_plot1, input$bwidthscale, input$display_mode, !is.null(input$show_top3_ids))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

    selected_countries <- expand_country_selection(input$country)
    # Get the label from the nested toflow_choices list
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow]

    # Filter techmap based on selected technology categories
    # Handle "Other" category to include all non-selected technologies
    selected_categories <- input$tech_categories_plot1
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    if(include_other && length(explicit_categories) > 0) {
      # Include explicitly selected categories AND other categories relabeled as "Other"
      filtered_techmap <- techmap %>%
        mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
    } else if(include_other && length(explicit_categories) == 0) {
      # Only "Other" selected - show all categories as "Other"
      filtered_techmap <- techmap %>%
        mutate(technology = "Other")
    } else {
      # No "Other" - just filter to explicitly selected categories
      filtered_techmap <- techmap %>%
        filter(technology %in% explicit_categories)
    }

    #selected_countries="VN"  ;input=list(); input$toflow="istrax_global"
    #colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes)
    
    # Calculate responsive dimensions - wider browser = wider plot
    plot_width <- max(window_dims$width, 400)
    # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
    width_inches <- plot_width / 96
    # Wider windows get wider aspect ratio (less height per width)
    aspect_ratio <- ifelse(plot_width > 1200, 0.4, ifelse(plot_width > 800, 0.5, 0.6))
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

    #plot_avstrax_by_technology <- function(pdata, classes, green_classes, technologies, toflow, custom_colors)
    #input$techs="Wireless" 
    
    # We first implement the filter from the previous diagram; i.e. we restrict to the countries selected there...
    #selected_countries="VN"
    filtered <- patchar_countrymap() %>%
      filter(ctry_code %in% selected_countries )  
    
    #filtered= patchar_countrymap %>%    filter(ctry_code %in% c("VN","GB") )  
    #input=list(techs="AI",toflow="istrax_global")
    # Calculate responsive dimensions - wider browser = wider plot
    plot_width <- max(window_dims$width, 400)
    # Convert pixels to inches (assuming 96 dpi), with aspect ratio that varies with width
    width_inches <- plot_width / 96
    # Wider windows get wider aspect ratio (less height per width)
    aspect_ratio <- ifelse(plot_width > 1200, 0.4, ifelse(plot_width > 800, 0.5, 0.6))
    height_inches <- width_inches * aspect_ratio

    p <- plot_avstrax_by_technology(
      pdata = filtered,
      classes = techmap,
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
      comparison_technologies = input$techs_comparison
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

    # Filter by technology class
    filtered_classes <- techmap %>%
      filter(technology %in% input$techs) %>%
      distinct()

    if("All Innovations" %in% input$techs) filtered_classes <- data.frame()

    # Filter data by selected countries
    filtered <- patchar_countrymap() %>%
      filter(ctry_code %in% selected_countries)

    # Compute aggregated data for all countries
    avstrax_data <- compute_avstrax_for_techs(filtered, input$toflow, filtered_classes)

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno)

    plot_world_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      color_scale = "Viridis",
      plot_title = paste0("World Map: ", sub("^[^.]*\\.", "", flow_label))
    )
  })

  # ============================================
  # Region Explorer Tab - Server Logic
  # ============================================

  # Reactive for region data (similar to patchar_countrymap but for regions)
  patchar_regionmap <- reactive({
    req(input$toflow_region)

    path <- paste0("/istraxes/", input$toflow_region,".fst")

    pp=localpath_fname(path)
    if(file.exists(pp)){
      ddd <- read_fst(pp)
    } else {
      ddd <- dropbox_read_fst(path)
    }

    # Join regionmap with istrax data
    # Rename region_code to ctry_code and region_name to country_name for compatibility with plotting functions
    if (regionmap_available && !is.null(regionmap) && nrow(regionmap) > 0) {
      # Remove ctry_code from ddd if it exists to avoid duplicate column names after join
      ddd_for_join <- ddd %>% select(-any_of(c("ctry_code", "country_name")))

      patchar_regionmap <- regionmap %>%
        rename(ctry_code = region_code, country_name = region_name) %>%
        left_join(ddd_for_join, by = "docdb_family_id")
      message("patchar_regionmap columns after rename: ", paste(names(patchar_regionmap), collapse = ", "))
      message("patchar_regionmap rows: ", nrow(patchar_regionmap))
    } else {
      patchar_regionmap <- data.frame()
      message("patchar_regionmap is empty - regionmap_available: ", regionmap_available)
    }

    patchar_regionmap
  })

  # Region Plot 1 - Returns by technology for selected regions
  output$avstrax_plot1_region <- renderGirafe({
    req(input$region, input$toflow_region, input$tech_categories_plot1_region,
        input$bwidthscale_region, input$display_mode_region, !is.null(input$show_top3_ids_region))
    req(window_dims$initialized)  # Wait for valid dimensions (important for bookmark restoration)

    # Check if regionmap is available
    shiny::validate(shiny::need(regionmap_available, "Region data not available. Please run prep_UK_regions.Rmd first."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Filter techmap based on selected technology categories
    selected_categories <- input$tech_categories_plot1_region
    include_other <- "Other" %in% selected_categories
    explicit_categories <- setdiff(selected_categories, "Other")

    if(include_other && length(explicit_categories) > 0) {
      filtered_techmap <- techmap %>%
        mutate(technology = ifelse(technology %in% explicit_categories, technology, "Other"))
    } else if(include_other && length(explicit_categories) == 0) {
      filtered_techmap <- techmap %>%
        mutate(technology = "Other")
    } else {
      filtered_techmap <- techmap %>%
        filter(technology %in% explicit_categories)
    }

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    aspect_ratio <- ifelse(plot_width > 1200, 0.4, ifelse(plot_width > 800, 0.5, 0.6))
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

    # Check if regionmap is available
    shiny::validate(shiny::need(regionmap_available, "Region data not available. Please run prep_UK_regions.Rmd first."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Get region data and validate it has required columns
    region_data <- patchar_regionmap()
    has_ctry_code <- "ctry_code" %in% names(region_data)
    shiny::validate(shiny::need(has_ctry_code, "Region data missing required columns. Please regenerate regionmap.fst."))

    # Filter by selected regions
    filtered <- region_data %>%
      filter(ctry_code %in% selected_regions)

    plot_width <- max(window_dims$width, 400)
    width_inches <- plot_width / 96
    aspect_ratio <- ifelse(plot_width > 1200, 0.4, ifelse(plot_width > 800, 0.5, 0.6))
    height_inches <- width_inches * aspect_ratio

    p <- plot_avstrax_by_technology(
      pdata = filtered,
      classes = techmap,
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
      comparison_technologies = input$techs_comparison_region
    )

    p
  })

  # UK Regions Map for Region Explorer
  output$uk_regions_map <- renderLeaflet({
    req(input$region,
        input$toflow_region,
        input$techs_region,
        input$mininno_region)

    # Check if regionmap is available
    shiny::validate(shiny::need(regionmap_available, "Region data not available."))

    selected_regions <- expand_region_selection(input$region)
    flow_label <- names(unlist(toflow_choices))[unlist(toflow_choices) == input$toflow_region]

    # Filter by technology class
    filtered_classes <- techmap %>%
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

    # Filter by minimum innovations
    avstrax_data <- avstrax_data %>%
      filter(innos >= input$mininno_region)

    plot_uk_regions_map(
      avstrax_data = avstrax_data,
      value_col = "mean",
      plot_title = paste0("UK Regions: ", sub("^[^.]*\\.", "", flow_label))
    )
  })

}

# Run the app
shinyApp(ui = ui, server = server)
