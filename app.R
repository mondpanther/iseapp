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
library(rdrop2)
source("dropbox_auth.R")

############## drop box drama
library(rdrop2)

load_dropbox_token <- function(local_token_path = "dropbox_token.rds") {
  
  # Detect if running on Posit Connect
  on_connect <- Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT" || 
    Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect"
  
  if (on_connect) {
    # Running on Posit Connect - use split token parts
    cat("Running on Posit Connect - loading split token...\n")
    
    parts <- character()
    i <- 1
    
    while(TRUE) {
      var_name <- paste0("DROPBOX_TOKEN_PART", i)
      part <- Sys.getenv(var_name)
      
      if(part == "") break  # No more parts found
      
      parts <- c(parts, part)
      i <- i + 1
    }
    
    if(length(parts) == 0) {
      stop("No DROPBOX_TOKEN_PART variables found on Posit Connect")
    }
    
    cat("Found", length(parts), "token parts\n")
    
    # Reassemble
    token_base64 <- paste0(parts, collapse = "")
    token_raw <- base64enc::base64decode(token_base64)
    token <- unserialize(token_raw)
    
  } else {
    # Running locally - load from RDS file
    cat("Running locally - loading token from RDS file...\n")
    
    if(!file.exists(local_token_path)) {
      stop("Token file not found at: ", local_token_path, 
           "\nRun drop_auth() and save with: saveRDS(drop_auth(), '", 
           local_token_path, "')")
    }
    
    token <- readRDS(local_token_path)
  }
  
  return(token)
}

# Usage in your app:
token <- load_dropbox_token()  # Uses default path "dropbox_token.rds"
# Or specify custom path:
# token <- load_dropbox_token("path/to/my_token.rds")

drop_acc(dtoken = token)
############################



# Function to load token with flexible number of parts
load_dropbox_token <- function(n_parts = 3) {
  # Get all parts dynamically
  parts <- character(n_parts)
  
  for(i in 1:n_parts) {
    var_name <- paste0("DROPBOX_TOKEN_PART", i)
    parts[i] <- Sys.getenv(var_name)
  }
  
  # Remove empty parts
  parts <- parts[parts != ""]
  
  if(length(parts) == 0) {
    stop("No DROPBOX_TOKEN_PART variables found")
  }
  
  # Reassemble
  token_base64 <- paste0(parts, collapse = "")
  token_raw <- base64enc::base64decode(token_base64)
  token <- unserialize(token_raw)
  
  return(token)
}





# Helper function to download from Dropbox
dropbox_download <- function(path, token) {
  request("https://content.dropboxapi.com/2/files/download") |>
    req_headers(
      Authorization = paste("Bearer", token),
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = path), auto_unbox = TRUE)
    ) |>
    req_perform() |>
    resp_body_raw()
  
  
  
}
db=function(path,token){
  raw_data <- dropbox_download(path, token)
  temp_file <- tempfile(fileext = ".fst")
  writeBin(raw_data, temp_file)
  df <- read_fst(temp_file)
  unlink(temp_file)
  return(df)
}




token <- Sys.getenv("DROPBOX_TOKEN")
techmap <- db("/techmap.fst", token)


#rsconnect::writeManifest()
enableBookmarking(store = "url")

# Load data

#files <- list.files(path="istraxes", pattern = "parquet$", full.names = TRUE)
#countrymap <- read_fst("countrymap.fst")
countrymap <- db("/countrymap.fst", token)



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



# Define UI
ui <- function(request){fluidPage(
  
  
  
  
  # Add custom CSS
  tags$head(
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
    tags$summary("▶ About this tool", class = "toggle-summary"),
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
    });
  ")),
  
  
  

  
   
  
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
      inputId = "bwidthscale:",
      label = "Bar width scale",
      choices = c("log", "proportional"),
      selected = "log"
    ),
    radioButtons(
      inputId = "display_mode",
      label = "Display mode",
      choices = c("Confidence bands" = "confidence", "Quartile bin means" = "quartiles"),
      selected = "quartiles"
    )
  ),

  # Toggle button for Figure 1
  tags$button("▲ Less", id = "togglePlot1", class = "plot-toggle"),

  # Wrap the first plot in a collapsible container
  tags$div(
    id = "plot1Container",
    withSpinner(plotOutput("avstrax_plot1", height = "600px"), type = 4, color = "#3498db")
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
    
    sliderInput(
      inputId = "topn",
      label = "Show top n countries",
      min = 1,
      max = 200,
      width = "350px",
      value = 20  # default starting value
    ),
    sliderInput(
      inputId = "mininno",
      label = "Innovation count threshold:",
      min = 1,
      max = 500,
      value = 100,  # default starting value
      width = "350px"
    )

  ),

  withSpinner(plotOutput("avstrax_plot2", height = "600px"), type = 4, color = "#3498db")
  
)
}

  
  
# Define server
server <- function(input, output) {
  
  #colorings=list(green=green_classes,battery=battery_classes,hard_to_abate=hard_to_abate_classes,ai=ai_classes)
  
  
  
  #for (ff in files) {
  #  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
  #}
  
  
  patchar_countrymap <- reactive({
    req(input$toflow)
    
    #input=list(toflow="avstrax_global")
    path <- paste0("/istraxes/", input$toflow,".fst")
    #path <- paste0("/istraxes/istrax_global.fst")
    
    ddd=db(path,token)
    #patchar_countrymap <- countrymap %>% left_join(read_fst(path))
    patchar_countrymap <- countrymap %>% left_join(ddd)
    
  })

  
  
  
  output$avstrax_plot1 <- renderPlot({
    req(input$country, input$toflow, input$tech_categories_plot1, input$bwidthscale, input$display_mode)

    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(toflow_choices)[toflow_choices == input$toflow]

    validate(
      need(exists("plot_avstrax_by_country"), "Function 'plot_avstrax_by_country' not found in the environment."),
      need(exists("patchar_countrymap"), "Object 'patchar_countrymap' not found."),
      need(exists("techmap"), "Object 'techmap' not found."),
      need(exists("green_classes"),   "Object 'green_classes' not found."),
      need(exists("battery_classes"), "Object 'battery_classes' not found."),
      need(exists("custom_colors"),   "Object 'custom_colors' not found.")
    )

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
    
    p <- plot_avstrax_by_country(
      pdata = patchar_countrymap(),
      classes = filtered_techmap,
      #green_classes = green_classes,
      country_code = selected_countries,
      toflow = input$toflow,
      custom_colors = custom_colors,
      colorings=colorings,
      bwidthscale=input$bwidthscale,
      display_mode=input$display_mode
      #battery_classes = battery_classes,
      #hard_to_abate_classes = hard_to_abate_classes
    ) + ggtitle("")

    p
  })
  
  
  output$avstrax_plot2 <- renderPlot({
    req(input$country,
        input$toflow,
        input$techs,
        input$topn,
        input$mininno,
        input$bwidthscale,
        input$display_mode)
    
    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(toflow_choices)[toflow_choices == input$toflow]
    
    validate(
      need(exists("plot_avstrax_by_country"), "Function 'plot_avstrax_by_country' not found in the environment."),
      need(exists("patchar_countrymap"), "Object 'patchar_countrymap' not found."),
      need(exists("techmap"), "Object 'techmap' not found."),
      need(exists("green_classes"), "Object 'green_classes' not found."),
      need(exists("battery_classes"), "Object 'battery_classes' not found."),
      need(exists("custom_colors"), "Object 'custom_colors' not found.")
    )
  
    
    
    #plot_avstrax_by_technology <- function(pdata, classes, green_classes, technologies, toflow, custom_colors)
    #input$techs="Wireless" 
    
    # We first implement the filter from the previous diagram; i.e. we restrict to the countries selected there...
    #selected_countries="VN"
    filtered <- patchar_countrymap() %>%
      filter(ctry_code %in% selected_countries )  
    
    #filtered= patchar_countrymap %>%    filter(ctry_code %in% c("VN","GB") )  
    #input=list(techs="AI",toflow="istrax_global")
    p <- plot_avstrax_by_technology(
      pdata = filtered,
      classes = techmap,
      #green_classes = green_classes,

      #country_code = selected_countries,
      technologies=input$techs,

      toflow = input$toflow,
      custom_colors = custom_colors,
      topn=input$topn,
      mininno=input$mininno,
      bwidthscale=input$bwidthscale,
      display_mode=input$display_mode
    ) + ggtitle("")
    
    p
  })
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
