#' Globe module Sidebar
#'
#' @param id the ID of the module
#'
#' @keywords internal
globe_module_sidebar <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "display: flex; flex-direction: column; gap: 20px;",

    shiny::selectInput(
      ns("sce_country"),
      "Country or Group:",
      choices = NULL,  # Will be populated in server
      multiple = TRUE
    ),
    
    shiny::selectInput(
      ns("sce_tech_display"),
      "Technology or Group:",
      choices = NULL,  # Will be populated in server
      multiple = TRUE
    ),
    
    shiny::sliderInput(
      ns("max_wave"),
      "Max Wave:",
      min = 1,
      max = 10,  # Will be updated in server
      value = 1,
      step = 1
    ),
    
    shiny::selectInput(
      ns("sample_size"),
      "Sample Size:",
      choices = NULL,  # Will be populated in server
      selected = NULL
    ),

    bslib::input_task_button(
      ns("render_map"),
      "Render Map",
      label_busy = "Rendering...",
      class = "btn-primary",
      width = "100%"
    ),
  )
}

#' Globe module UI
#'
#' @param id the ID of the module
#'
#' @importFrom shiny column fluidRow h1 NS tagList
#'
#' @keywords internal
globe_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      open = TRUE,
      width = 330,
      globe_module_sidebar(id)
    ),
    
    # Main content - just the map, no tabs
    shiny::div(
      style = "padding: 20px;",
      leaflet::leafletOutput(ns("map"), height = "650px")
    )
  )
}

#' Country module Server
#'
#' @param id the ID of the module
#'
#' @importFrom shiny moduleServer observeEvent observe req reactive reactiveValues bindEvent invalidateLater parseQueryString updateQueryString
#'
#' @keywords internal
globe_module_server <- function(id, parent_session) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Track initialization state
      initialization_complete <- shiny::reactiveVal(FALSE)

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          shiny::h1("Loading map data", style = "margin-top: 20px;"),
          shiny::p("Please be patient...", style = "")
        ),
        color = waiter::transparent(0.5)
      )
      
      ###############################################################
      # LOAD DATA
      ###############################################################
      
      data_path <- system.file("extdata", "inglobe_processed.parquet", package = "innovationStrategyExplorer")
      con <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbExecute(con, sprintf("CREATE VIEW df AS SELECT * FROM read_parquet('%s')", data_path))

      # Clean up connection on session end
      shiny::onSessionEnded(function() {
        DBI::dbDisconnect(con, shutdown = TRUE)
      })
      
      ###############################################################
      # COUNTRY GROUP DEFINITIONS
      ###############################################################
      
      all_countries <- sort(unique(stats::na.omit(countrycode::codelist$iso2c)))
      
      north_america <- c("US","CA","MX")
      south_america <- c("BR","AR","CL","CO","PE","UY","EC","VE")
      western_europe <- c("FR","GB","DE","NL","BE","CH","AT","IE","LU")
      northern_europe <- c("SE","NO","FI","DK","IS")
      southern_europe <- c("IT","ES","PT","GR","MT","CY")
      eastern_europe <- c("PL","CZ","HU","SK","SI","HR","RO","BG",
                          "RS","BA","MK","AL","EE","LV","LT","UA","BY","MD")
      middle_east <- c("TR","IL","SA","AE","QA","KW","OM","BH","JO","IR","IQ","LB")
      africa <- c("ZA","EG","NG","KE","GH","SN","CI","MA","TN","DZ","ET","TZ","UG","ZM","MZ")
      central_asia <- c("KZ","UZ","KG","TJ","TM")
      south_asia <- c("IN","PK","BD","LK","NP")
      east_asia <- c("CN","JP","KR","MN")
      south_east_asia <- c("SG","MY","TH","VN","ID","PH","KH","LA","MM","BN")
      oceania <- c("AU","NZ","FJ")
      
      group_definitions <- list(
        "All countries"   = all_countries,
        "North America"   = north_america,
        "South America"   = south_america,
        "Western Europe"  = western_europe,
        "Northern Europe" = northern_europe,
        "Southern Europe" = southern_europe,
        "Eastern Europe"  = eastern_europe,
        "Middle East"     = middle_east,
        "Africa"          = africa,
        "Central Asia"    = central_asia,
        "South Asia"      = south_asia,
        "East Asia"       = east_asia,
        "South East Asia" = south_east_asia,
        "Oceania"         = oceania
      )
      
      expand_country_selection <- function(selected) {
        unique(unlist(lapply(selected, function(x) {
          if (x %in% names(group_definitions)) group_definitions[[x]] else x
        })))
      }
      
      ###############################################################
      # TECHNOLOGY GROUP DEFINITIONS
      ###############################################################
      
      tech_group_definitions <- list(
        "All"        = unique(metadata$sce_tech_display[metadata$tech_group == "All"]),
        "Green"      = unique(metadata$sce_tech_display[metadata$tech_group == "Green"]),
        "Non-Green"  = unique(metadata$sce_tech_display[metadata$tech_group == "Non-Green"])
      )
      
      tech_group_definitions <- lapply(
        tech_group_definitions,
        function(x) sort(unique(stats::na.omit(x)))
      )
      
      grouped_tech_choices <- list(
        "Technology Groups" = names(tech_group_definitions),
        "Individual Technologies" = sort(unique(metadata$sce_tech_display))
      )
      
      expand_tech_selection <- function(selected) {
        unique(unlist(lapply(selected, function(x) {
          if (x %in% names(tech_group_definitions)) {
            tech_group_definitions[[x]]
          } else {
            x
          }
        })))
      }
      
      ###############################################################
      # UPDATE UI INPUTS
      ###############################################################
      
      shiny::observe({
        shiny::updateSelectInput(
          session,
          "sce_country",
          choices = list(
            "Predefined Groups" = names(group_definitions),
            "Individual Countries" = sort(unique(metadata$sce_country))
          ),
          selected = "Africa"
        )
        
        shiny::updateSelectInput(
          session,
          "sce_tech_display",
          choices = grouped_tech_choices,
          selected = "Green"
        )
        
        shiny::updateSliderInput(
          session,
          "max_wave",
          min = wave_range$min_wave,
          max = wave_range$max_wave,
          value = 1
        )
        
        shiny::updateSelectInput(
          session,
          "sample_size",
          choices = sort(unique(metadata$sample_size)),
          selected = sort(unique(metadata$sample_size))[1]
        )
        
        # Mark initialization as complete and hide waiter
        initialization_complete(TRUE)
        waiter::waiter_hide()
      })
      
      ###############################################################
      # COLOR LOGIC
      ###############################################################
      
      get_scenario_color <- function(tech_group) {
        tech_group <- tolower(tech_group)
        if (tech_group == "all")        return("blue")
        if (tech_group == "non-green")  return("brown")
        if (tech_group == "green")      return("green")
        return("black")
      }
      
      get_random_shade <- function(base_color) {
        base_hue <- switch(base_color,
                           "blue"  = 210,
                           "green" = 120,
                           "brown" = 30,
                           0)
        
        hue <- base_hue + runif(1, -8, 8)
        sat <- runif(1, 0.55, 0.75)
        lig <- runif(1, 0.40, 0.65)
        
        h <- (hue %% 360) / 360
        q <- if (lig < 0.5) lig * (1 + sat) else lig + sat - lig * sat
        p <- 2 * lig - q
        
        hue_to_rgb <- function(t) {
          t <- ifelse(t < 0, t + 1, ifelse(t > 1, t - 1, t))
          if (t < 1/6)      return(p + (q - p) * 6 * t)
          else if (t < 1/2) return(q)
          else if (t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
          else              return(p)
        }
        
        r <- hue_to_rgb(h + 1/3)
        g <- hue_to_rgb(h)
        b <- hue_to_rgb(h - 1/3)
        
        sprintf("#%02X%02X%02X", round(r*255), round(g*255), round(b*255))
      }
      
      ###############################################################
      # CURVE FUNCTION
      ###############################################################
      
      make_curve <- function(sx, sy, tx, ty, n = 40, bend = 0.3) {
        if (sx == tx && sy == ty)
          return(data.frame(lon = c(sx, tx), lat = c(sy, ty)))
        
        mx <- (sx + tx) / 2
        my <- (sy + ty) / 2
        
        dx <- tx - sx
        dy <- ty - sy
        d  <- sqrt(dx^2 + dy^2)
        if (d == 0) d <- 1
        
        px <- -dy / d
        py <-  dx / d
        
        h <- bend * d
        
        cx <- mx + px * h
        cy <- my + py * h
        
        t <- seq(0, 1, length.out = n)
        
        lon <- (1 - t)^2 * sx + 2 * (1 - t) * t * cx + t^2 * tx
        lat <- (1 - t)^2 * sy + 2 * (1 - t) * t * cy + t^2 * ty
        
        data.frame(lon = lon, lat = lat)
      }
      
      ###############################################################
      # MAP OUTPUT
      ###############################################################
      
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
          leaflet::addTiles() |>
          leaflet::setView(10, 20, zoom = 3)
      })
      
      ###############################################################
      # RENDER TRIGGER & MAP RENDERING
      ###############################################################

      shiny::observeEvent(c(input$render_map), {
        # Don't run until initialization is complete
        shiny::req(initialization_complete())

        selected_countries <- expand_country_selection(input$sce_country)
        selected_techs <- expand_tech_selection(input$sce_tech_display)
        
        # Build query with glue_sql for proper vector expansion
        query <- glue::glue_sql(
          "SELECT *
          FROM df
          WHERE sce_country IN ({selected_countries*})
            AND sce_tech_display IN ({selected_techs*})
            AND wave <= {input$max_wave}
            AND sample_size = {input$sample_size}",
          .con = con
        )

        # edges <- edges_filtered()
        # nodes <- nodes_filtered()
        
        edges <- DBI::dbGetQuery(con, query)
        
        nodes <- dplyr::bind_rows(
          edges |> dplyr::select(lon = source_lon, lat = source_lat),
          edges |> dplyr::select(lon = target_lon, lat = target_lat)
        ) |> dplyr::distinct()
        
        if (nrow(edges) == 0) return()
        
        base_color <- get_scenario_color(unique(edges$tech_group)[1])
        
        m <- leaflet::leafletProxy("map") |>
          leaflet::clearShapes() |>
          leaflet::clearMarkers()
        
        m <- m |>
          leaflet::addCircleMarkers(
            data = nodes,
            lng = ~lon, lat = ~lat,
            radius = 5,
            fillColor = "white", fillOpacity = 1,
            color = "black", weight = 1
          )
        
        chain_colors <- edges |>
          dplyr::distinct(chain_id) |>
          dplyr::mutate(color = sapply(seq_len(dplyr::n()), function(i)
            get_random_shade(base_color)
          ))
        
        for (i in seq_len(nrow(edges))) {
          chain <- edges$chain_id[i]
          this_color <- chain_colors$color[chain_colors$chain_id == chain]
          
          curve_pts <- make_curve(
            edges$source_lon[i], edges$source_lat[i],
            edges$target_lon[i], edges$target_lat[i]
          )
          
          m <- m |>
            leaflet::addPolylines(
              lng = curve_pts$lon,
              lat = curve_pts$lat,
              color = this_color,
              weight = 1,
              opacity = 0.4
            )
        }
      })

    }
  )
}