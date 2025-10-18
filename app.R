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


#rsconnect::writeManifest()


# Load data
files <- list.files(path="istraxes", pattern = "parquet$", full.names = TRUE)
patchar_countrymap <- read_parquet("countrymap.parquet")
for (ff in files) {
  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
}

techmap <- read_parquet("techmap.parquet")

green_classes <- c("Green Energy", "Green Transport", "Circular Economy", "Green Manufacturing",
                   "Adaptation", "Green Housing", "Green ICT", "Green Agriculture",
                   "GHG Capture", "Any Green")

source("istraxfunctions.R")

toflow_choices <- c(
  "Global Returns" = "istrax_global",
  "National Returns" = "istrax_nationalkey_2009_2018",
  "Returns to LMICs (excl. China)" = "istrax_EMDEexCN",
  "Returns to LMICs (excl. China & India)" = "istrax_EMDEexCNIN",
  "Returns to HICs" = "istrax_HIC",
  "Returns to the EU" = "istrax_EU"
)

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
ui <- fluidPage(
  
  
  
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      h1 {
        font-family: 'Arial', serif;
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
  
  
  
  titlePanel("Welcome to ISE - The Industrial Strategy Explorer"),
  
  
  
  # Introductory paragraph
  tags$p("This is a tool to explore various version of the Industrial Strategy Index (ISTRAX) 
         developed in Guillard et al.     ",
         tags$a(href = "https://cep.lse.ac.uk/_NEW/publications/abstract.asp?index=8614", target = "_blank", "Efficient Industrial Policy - Standing on the Shoulders of Hidden Giants"),
         ". The figures show the returns from further investment in R&D in different technology areas and specific countries via knowledge spillovers; 
         i.e. a return of 100% means that further R&D investment of 1000 Euro will lead to extra profits worth 1000 Euro for innovators different from the investor undertaking the additional spending.",
         tags$br(),
         "You can desplay the average returns for different countries or countries groups broken down by technology areas. You can also examine this for different of scopes of spillovers.",
         "Global Returns means spillover benefits to inventors anywhere are taken into account. Returns LMICs only take into account spillover benefits to innovators in Low and Medium Income countries.",
         class = "intro-text"
       ),
  
  
  
  
  inputPanel(
    selectizeInput(
      inputId = "country",
      label = "Country or Group",
      choices = grouped_choices,
      selected = "All countries",
      multiple = TRUE,
      options = list(placeholder = 'Choose one or more countries or groups...')
    ),
    selectInput(
      inputId = "toflow",
      label = "Return flow",
      choices = toflow_choices,
      selected = "istrax_global"
    )
  ),
  plotOutput("avstrax_plot", height = "600px")
)

# Define server
server <- function(input, output) {
  output$avstrax_plot <- renderPlot({
    req(input$country, input$toflow)
    
    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(toflow_choices)[toflow_choices == input$toflow]
    
    validate(
      need(exists("plot_avstrax_by_country"), "Function 'plot_avstrax_by_country' not found in the environment."),
      need(exists("patchar_countrymap"), "Object 'patchar_countrymap' not found."),
      need(exists("techmap"), "Object 'techmap' not found."),
      need(exists("green_classes"), "Object 'green_classes' not found."),
      need(exists("custom_colors"), "Object 'custom_colors' not found.")
    )
    
    p <- plot_avstrax_by_country(
      pdata = patchar_countrymap,
      classes = techmap,
      green_classes = green_classes,
      country_code = selected_countries,
      toflow = input$toflow,
      custom_colors = custom_colors
    ) + ggtitle("")
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)
