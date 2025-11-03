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

#files <- list.files(path="istraxes", pattern = "parquet$", full.names = TRUE)
countrymap <- read_parquet("countrymap.parquet")
#for (ff in files) {
#  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
#}

techmap <- read_parquet("techmap.parquet")

green_classes <- c("Green Energy", "Green Transport", "Circular Economy", "Green Manufacturing",
                   "Adaptation", "Green Housing", "Green ICT", "Green Agriculture",
                   "GHG Capture", "Any Green")

source("istraxfunctions.R")

grouped_techs=c(as.list((techmap %>% distinct(technology))$technology),"All Innovations")

toflow_choices <- c(
  "Global Returns" = "istrax_global",
  "National Returns" = "istrax_nationalkey_2009_2018",
  "Returns to LMICs" = "istrax_EMDE",
  "Returns to LMICs (excl. China)" = "istrax_EMDENOCN",
  "Returns to LMICs (excl. China & India)" = "istrax_EMDENOCNIN",
  "Returns to HICs" = "istrax_HIC",
  "Returns to the EU" = "istrax_EU",
  "Returns to US" = "istrax_US",
  "Returns to UK" = "istrax_GB",
  "Returns to Austria" = "istrax_AT",
  "Returns to France" = "istrax_FR"
  
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
    tags$p(
      "This tool supports the development of an innovation strategy at various scopes 
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
  plotOutput("avstrax_plot1", height = "600px"),
  
  inputPanel(
    selectizeInput(
      inputId = "techs",
      label = "Technology categories",
      choices = grouped_techs,
      selected = "Any Green",
      multiple = TRUE,
      options = list(placeholder = 'Choose one or more technology categories...')
    ),
    
    sliderInput(
      inputId = "topn",
      label = "Show top n countries",
      min = 1,
      max = 200,
      width = "250px",
      value = 20  # default starting value
    ),
    sliderInput(
      inputId = "mininno",
      label = "Innovation count threshold:",
      min = 1,
      max = 500,
      value = 100,  # default starting value
      width = "250px"
    )
    
  ),
  
  plotOutput("avstrax_plot2", height = "600px")
  
)

# Define server
server <- function(input, output) {
  

  
  
  #for (ff in files) {
  #  patchar_countrymap <- patchar_countrymap %>% left_join(read_parquet(ff))
  #}
  
  
  patchar_countrymap <- reactive({
    req(input$toflow)
    
    
    path <- paste0("./istraxes/", input$toflow,".parquet")
    #path <- paste0("./istraxes/istrax_global.parquet")
    patchar_countrymap <- countrymap %>% left_join(read_parquet(path))

  })
  
  
  
  
  output$avstrax_plot1 <- renderPlot({
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

    #selected_countries="VN"  ;input=list(); input$toflow="istrax_global"  
    p <- plot_avstrax_by_country(
      pdata = patchar_countrymap(),
      classes = techmap,
      green_classes = green_classes,
      country_code = selected_countries,
      toflow = input$toflow,
      custom_colors = custom_colors
    ) + ggtitle("")
    
    p
  })
  
  
  output$avstrax_plot2 <- renderPlot({
    req(input$country, 
        input$toflow,
        input$techs,
        input$topn,
        input$mininno)
    
    selected_countries <- expand_country_selection(input$country)
    flow_label <- names(toflow_choices)[toflow_choices == input$toflow]
    
    validate(
      need(exists("plot_avstrax_by_country"), "Function 'plot_avstrax_by_country' not found in the environment."),
      need(exists("patchar_countrymap"), "Object 'patchar_countrymap' not found."),
      need(exists("techmap"), "Object 'techmap' not found."),
      need(exists("green_classes"), "Object 'green_classes' not found."),
      need(exists("custom_colors"), "Object 'custom_colors' not found.")
    )
  
    
    
    #plot_avstrax_by_technology <- function(pdata, classes, green_classes, technologies, toflow, custom_colors)
    #input$techs="Wireless" 
    
    # We first implement the filter from the previous diagram; i.e. we restrict to the countries selected there...
    
    filtered <- patchar_countrymap() %>%
      filter(ctry_code %in% selected_countries )  
    
    
    
    p <- plot_avstrax_by_technology(
      pdata = filtered,
      classes = techmap,
      green_classes = green_classes,
      
      #country_code = selected_countries,
      technologies=input$techs,
      
      toflow = input$toflow,
      custom_colors = custom_colors,
      topn=input$topn,
      mininno=input$mininno
    ) + ggtitle("")
    
    p
  })
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
