
library(dplyr)
library(tidyr)
library(ggiraph)
library(scales)
library(fst)


# ============================================
# Pre-computed Data Helper Functions
# ============================================

#' Generate safe filename for pre-computed data
#' @param ... Parts to combine into filename
#' @return Safe filename string
safe_filename <- function(...) {
  parts <- c(...)
  name <- paste(parts, collapse = "_")
  name <- gsub("[^a-zA-Z0-9_]", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_|_$", "", name)
  paste0(name, ".fst")
}

#' Get the prepdata directory path
#' @param iseapp_local_path The base iseapp path
#' @return Path to prepdata directory or NULL
get_prepdata_path <- function(iseapp_local_path) {
  if (is.null(iseapp_local_path)) return(NULL)
  prepdata_path <- file.path(iseapp_local_path, "prepdata")
  if (dir.exists(prepdata_path)) return(prepdata_path)
  return(NULL)
}

#' Try to load pre-computed data for "by technology" aggregation
#' @param prepdata_path Path to prepdata directory
#' @param toflow The toflow variable name
#' @param country_group_name Name of the country group
#' @return Data frame or NULL if not found
load_precomputed_by_tech <- function(prepdata_path, toflow, country_group_name) {
  if (is.null(prepdata_path)) return(NULL)

  filename <- safe_filename("by_tech", toflow, country_group_name)
  filepath <- file.path(prepdata_path, filename)

  if (file.exists(filepath)) {
    tryCatch({
      data <- read_fst(filepath)
      message("Loaded pre-computed data: ", filename)
      return(data)
    }, error = function(e) {
      message("Error loading pre-computed data: ", e$message)
      return(NULL)
    })
  }
  return(NULL)
}

#' Try to load pre-computed data for "by country" aggregation
#' @param prepdata_path Path to prepdata directory
#' @param toflow The toflow variable name
#' @param tech_category_name Name of the technology category
#' @return Data frame or NULL if not found
load_precomputed_by_country <- function(prepdata_path, toflow, tech_category_name) {
  if (is.null(prepdata_path)) return(NULL)

  filename <- safe_filename("by_country", toflow, tech_category_name)
  filepath <- file.path(prepdata_path, filename)

  if (file.exists(filepath)) {
    tryCatch({
      data <- read_fst(filepath)
      message("Loaded pre-computed data: ", filename)
      return(data)
    }, error = function(e) {
      message("Error loading pre-computed data: ", e$message)
      return(NULL)
    })
  }
  return(NULL)
}

#' Try to load pre-computed data for "by region" aggregation
#' @param prepdata_path Path to prepdata directory
#' @param toflow The toflow variable name
#' @param tech_category_name Name of the technology category
#' @return Data frame or NULL if not found
load_precomputed_by_region <- function(prepdata_path, toflow, tech_category_name) {
  if (is.null(prepdata_path)) return(NULL)

  filename <- safe_filename("by_region", toflow, tech_category_name)
  filepath <- file.path(prepdata_path, filename)

  if (file.exists(filepath)) {
    tryCatch({
      data <- read_fst(filepath)
      message("Loaded pre-computed data: ", filename)
      return(data)
    }, error = function(e) {
      message("Error loading pre-computed data: ", e$message)
      return(NULL)
    })
  }
  return(NULL)
}

#' Map selected countries to a country group name for pre-computed lookup
#' @param selected_countries Vector of selected country codes
#' @param group_definitions Named list of country group definitions
#' @return Group name if exact match found, NULL otherwise
match_country_group <- function(selected_countries, group_definitions) {
  selected_set <- sort(unique(selected_countries))

  for (group_name in names(group_definitions)) {
    group_set <- sort(unique(group_definitions[[group_name]]))
    if (identical(selected_set, group_set)) {
      # Convert to safe name format
      return(gsub(" ", "_", group_name))
    }
  }
  return(NULL)
}

#' Map selected technologies to a category name for pre-computed lookup
#' @param selected_techs Vector of selected technology names
#' @param tech_definitions Named list mapping category names to tech filters
#' @return Category name if match found, NULL otherwise
match_tech_category <- function(selected_techs, tech_definitions = NULL) {
  # Default tech definitions
  if (is.null(tech_definitions)) {
    tech_definitions <- list(
      "All" = "All",
      "Green_Technology" = "Green Technology",
      "Battery_Technology" = "Battery Technology",
      "Hard_to_Abate" = "Hard to Abate Sector Decarbonization",
      "AI" = "AI",
      "Other" = "Other"
    )
  }

  if (length(selected_techs) == 1) {
    for (cat_name in names(tech_definitions)) {
      if (identical(selected_techs, tech_definitions[[cat_name]])) {
        return(cat_name)
      }
    }
  }
  return(NULL)
}


# Define custom colors
custom_colors <- c("green" = "forestgreen", 
                   "battery" = "gold", 
                   "other" = "gray70",
                   "hard to abate"="blue",
                   "AI"="orange",
                   cpcsecs="purple")



# threshold for bars
win_thres=0.01



### functions

#### agregate the data...
compute_avstrax <- function(data, istrax_var, classes,colorings=NULL#, green_classes, battery_classes = NULL,hard_to_abate_classes=NULL,
                            ) {
  library(dplyr)



  #data=filtered; istrax_var="istrax_global"
  istrax_sym <- rlang::sym(istrax_var)

  # Check if the required column exists in the data

  if (!istrax_var %in% names(data)) {
    # Try to find similar column names for better error message
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler=ifelse(grepl("strax", istrax_var ),100,1)

  # Filter out "All" from classes since we add it separately below
  # This prevents double-counting when techmap already has "All" rows
  classes_filtered <- classes %>% filter(technology != "All")

  avstrax <- data %>%
    select(docdb_family_id, appln_id, !!istrax_sym) %>%
    rename(istrax = !!istrax_sym) %>%
    distinct() %>%
    inner_join(classes_filtered, by = "docdb_family_id") %>%
    bind_rows(
      data %>%
        select(docdb_family_id, !!istrax_sym) %>%
        rename(istrax = !!istrax_sym) %>%
        distinct() %>%
        mutate(technology = "All")
    ) %>%
    distinct() %>%
    group_by(technology) %>% arrange(technology,-istrax*scaler) %>%
    mutate(ppp=(1:n())/n()) %>%
    mutate(top25=ppp<0.25,
           top50=ppp<0.5,
           q1=quantile(istrax*scaler, 0.25, na.rm = TRUE),
           q2=quantile(istrax*scaler, 0.5, na.rm = TRUE),
           q3=quantile(istrax*scaler, 0.75, na.rm = TRUE)
  ) %>%
  summarise(
    mean = mean(istrax*scaler, na.rm = TRUE),
    innos = n(),
    sem = sd(istrax*scaler, na.rm = TRUE) / sqrt(n()),
    # Quartile bin means: mean of observations within each quartile bin

    #q1_bin_mean = mean(scaler*istrax[scaler*istrax <= q1], na.rm = TRUE),
    #q2_bin_mean = mean(scaler*istrax[scaler*istrax <= q2 & scaler*istrax>=q1], na.rm = TRUE),
    #q3_bin_mean = mean(scaler*istrax[scaler*istrax <= q3 & scaler*istrax>=q2], na.rm = TRUE),
    #q4_bin_mean = mean(scaler*istrax[scaler*istrax > q3], na.rm = TRUE),

    #q0M_bin_mean= mean(scaler*istrax[(scaler*istrax) <= q2], na.rm = TRUE),
    #q1M_bin_mean= mean(scaler*istrax[(scaler*istrax) > q2], na.rm = TRUE),

    top25_bin_mean= mean(scaler*istrax[top25==T], na.rm = TRUE),
    top50_bin_mean= mean(scaler*istrax[top50==T], na.rm = TRUE),

    # Top appln_id values (highest istrax) as comma-separated string
    top3_ids = paste(head(appln_id[order(-istrax*scaler)], 10), collapse = ", "),

    across(c(q1,q2,q3,top25,top50),mean),
      .groups = "drop"
    ) %>%
    mutate(
      # Create Espacenet search URL for top 3 IDs (use double quotes for JS to avoid HTML attribute conflicts)
      top3_ids_url = build_espacenet_search(top3_ids),
      greenclass = ifelse(technology %in% unlist(colorings["green"]), "green",
                          ifelse( technology %in% unlist(colorings["battery"]), "battery", 
                                  ifelse( technology %in% unlist(colorings["hard_to_abate"]), "hard to abate",
                                          ifelse( technology %in% unlist(colorings["ai"]), "AI",
                                                  ifelse( technology %in% unlist(colorings["cpcsecs"]), "CPC Sections", "other")
                                                )
                                        )
                                )
                          )
      )
 
  return(avstrax)
}



#### Draw the plots
plot_avstrax_by_country <- function(pdata, classes, #green_classes,
                                    country_code, toflow,
                                    custom_colors,
                                    colorings=NULL,
                                    bwidthscale="log",
                                    display_mode="confidence",
                                    show_top3_ids=FALSE,
                                    width_svg=10,
                                    height_svg=6,
                                    plot_title="Spillover returns",
                                    precomputed_data=NULL  # Optional precomputed aggregated data
                                    #battery_classes = NULL,
                                    #hard_to_abate_classes=NULL
                                    ) {
  library(dplyr)
  library(ggplot2)

  library(patchwork)
  #path <- paste0("/istraxes/istrax_global.fst"); ddd=dropbox_read_fst(path);
  #patchar_countrymap <- countrymap %>% left_join(ddd)
  #classes=techmap %>% filter(technology=="Green Technology"); toflow="istrax_global"; pdata=patchar_countrymap; country_code="VN";bwidthscale=100;show_top3_ids=TRUE
  #display_mode="confidence"

  # Use precomputed data if available, otherwise compute
  if (!is.null(precomputed_data) && nrow(precomputed_data) > 0) {
    avstrax <- precomputed_data
    classlist <- unique(avstrax$technology)
    message("Using precomputed data with ", nrow(avstrax), " rows")
  } else {
    # Require pdata and classes for on-the-fly computation
    if (is.null(pdata) || is.null(classes)) {
      stop("Either precomputed_data or both pdata and classes must be provided")
    }

    classlist=(classes %>% distinct(technology))$technology

    # Filter by country and year
    filtered <- pdata %>%
      filter(ctry_code %in% country_code )  %>%
      distinct()

    # Compute avstrax
    avstrax <- compute_avstrax(filtered, toflow, classes,colorings#, green_classes, battery_classes,hard_to_abate_classes
                               )
  }

  #toflow="istrax_global"; pdata=countrymap
  #ylab=ifelse(grepl("Return", toflow ),"Return in %","Millions of $")
  ylab=ifelse(grepl("strax", toflow ),"Return in %","Millions of $")
  #scaler=ifelse(grepl("strax", toflow ),100,1)
  
  # Extract mean for "All"
  allmean <- avstrax %>%
    filter(technology == "All") %>%
    pull(mean)
  
  total_innos =  avstrax %>%
    filter(technology == "All") %>%
    pull(innos)

  # Prepare data for plotting
  #display_mode="quartiles";bwidthscale="log"
  if(!"All" %in% classlist) avstrax=avstrax %>% filter(technology != "All")


  # Determine if this is a return (%) or spillover ($) variable

  is_return <- grepl("strax", toflow)

  avstrax <- avstrax %>%
    #filter(technology != "All") %>%
    arrange(technology) %>%
    mutate(
      linnos1 = innos,  # Now correctly uses the innos column, not the scalar
      linnos2 = log(1+innos),
      bwidthscale = bwidthscale
    ) %>%
    filter(innos>1) %>%
    mutate(
      linnos=ifelse(bwidthscale=="log",linnos2,linnos1),

      width = linnos / max(linnos),
      #width =ifelse( innos / max(innos)>win_thres,innos / max(innos),win_thres),

      # Store x position consistently for bars and error bars
      x_pos = as.numeric(factor(technology)),
      xmin = x_pos - width / 2,
      xmax = x_pos + width / 2,
      ymin = 0,
      ymax = mean
    )

  # Format value label based on variable type - use if/else for scalar condition
  if (is_return) {
    avstrax$value_label <- paste0(round(avstrax$mean, 1), "%")
  } else {
    avstrax$value_label <- paste0("$", round(avstrax$mean, 1), " million")
  }

  # Create the plot

  # Use interactive bars if show_top3_ids is enabled
  if (show_top3_ids) {
    p <- ggplot(avstrax) +
      geom_rect_interactive(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = greenclass,
                                 data_id = technology,
                                 tooltip = paste0(technology, ": ", value_label,
                                                  "\nInnovations: ", scales::comma(innos),
                                                  "\nTop IDs: ", top3_ids),
                                 onclick = top3_ids_url))
  } else {
    p <- ggplot(avstrax) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = greenclass))
  }

  # Add either confidence bands or quartile means based on display_mode
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(aes(x = x_pos, ymin = ifelse(mean- 1.96 * sem>0,mean- 1.96 * sem,0) ,
                                                                   ymax = mean + 1.96 * sem),
                           width = 0.2, color = "black", linewidth = .4, alpha = .4)
  } else if (display_mode == "quartiles") {
    p <- p + # geom_errorbar(aes(x = x_pos,ymin = q1_bin_mean, ymax = q2_bin_mean, width = width),
      #color = "brown",
      #                      linewidth = .7, alpha = .5)+
            #geom_errorbar(aes(x = x_pos,ymin = q1, ymax = q2,width = width),
        #              color = "#3498db",
       #               linewidth = .7, alpha = .5)+

            #geom_errorbar(aes(x = x_pos,ymin = q2, ymax = q3,width = width),
         #           color = "#3498db",
          #          linewidth = .7, alpha = .5)+

            geom_errorbar(aes(color=greenclass,x = x_pos,ymin = top50_bin_mean, ymax = top25_bin_mean,width=width*1.05),
                     linewidth = 1, alpha = .5)
  }

  p <- p +
    scale_x_continuous(breaks = avstrax$x_pos, labels = avstrax$technology) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(
      title = plot_title,
      x = "Technology",
      y = ylab,
      fill = "Technology"
    ) +
    guides(color = "none")+
    theme_minimal(base_family = "Open Sans") +

    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      text = element_text(family = "Open Sans"),
      axis.text = element_text(family = "Open Sans"),
      axis.title = element_text(family = "Open Sans")
    )+

    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = allmean, x = max(avstrax$x_pos) + 0.4,
             label = "Average", angle = -90, vjust = 1.5, size = 4,
             family = "Open Sans", color = "black") +
    coord_flip()#+
  #paste0(as.character(innos)," Innovations")
  #annotate("text", 
  #       x = max(as.numeric(factor(avstrax$technology))), 
  #       y = max(avstrax$mean + 5 * avstrax$sem), 
  #       label = paste0(as.character(innos)," Innovations"), 
  #      hjust = 1, vjust = 1, size = 5)
  
  
  #innos=3
  annotation_plot <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = paste0(as.character(total_innos)," Innovations"), size=5 ) +
    theme(plot.margin = margin(0, 0, -10, 0))


  # Add subtitle and caption
  p <- p + labs(subtitle = paste0(as.character(total_innos), " Innovations"),
                caption = "© 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 14, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 10, color = "gray"))

  # Return girafe object for Shiny girafeOutput compatibility
  # Use responsive sizing with dynamic width/height based on browser window
  return(girafe(ggobj = p,
                width_svg = width_svg,
                height_svg = height_svg,
                options = list(
                  opts_sizing(rescale = TRUE, width = 1),
                  opts_hover(css = "cursor:pointer;fill:yellow;"),
                  opts_selection(type = "none"),
                  opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;")
                )))
}




compute_avstrax_for_techs <- function(data, istrax_var, classes#, green_classes
                                      ) {
  #data=patchar_countrymap;istrax_var="istrax_global"; classes=filtered; green_classes=green_classes;classes=data.frame()


  library(dplyr)


  istrax_sym <- rlang::sym(istrax_var)

  # Check if the required column exists in the data
  if (!istrax_var %in% names(data)) {
    # Try to find similar column names for better error message
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler=ifelse(grepl("strax", istrax_var ),100,1)
  
  # If not filter classes are selected we take all
  if(nrow(classes)==0){
     filtereddata=data  
     #print("aaaaa")
  }  else {
    #print("bbbbb")
    filtereddata=data %>% inner_join(classes %>% select(docdb_family_id)%>% distinct())
  }
  
  #scaler=ifelse()
  # Include country_name if it exists (for regions)
  has_country_name <- "country_name" %in% names(filtereddata)

  if (has_country_name) {
    avstrax <- filtereddata %>%
      select(docdb_family_id, appln_id, !!istrax_sym, ctry_code, country_name) %>%
      rename(istrax = !!istrax_sym) %>%
      distinct() %>%
      bind_rows(
        filtereddata %>%
          select(docdb_family_id, appln_id, !!istrax_sym) %>%
          rename(istrax = !!istrax_sym) %>%
          distinct() %>%
          mutate(ctry_code = "All", country_name = "All")
      )
  } else {
    avstrax <- filtereddata %>%
      select(docdb_family_id, appln_id, !!istrax_sym, ctry_code) %>%
      rename(istrax = !!istrax_sym) %>%
      distinct() %>%
      bind_rows(
        filtereddata %>%
          select(docdb_family_id, appln_id, !!istrax_sym) %>%
          rename(istrax = !!istrax_sym) %>%
          distinct() %>%
          mutate(ctry_code = "All")
      )
  }

  # Group by ctry_code (and country_name if it exists)
  if (has_country_name) {
    avstrax <- avstrax %>%
      distinct() %>%
      group_by(ctry_code, country_name) %>%
      arrange(ctry_code,-istrax*scaler)
  } else {
    avstrax <- avstrax %>%
      distinct() %>%
      group_by(ctry_code) %>%
      arrange(ctry_code,-istrax*scaler)
  }

  avstrax <- avstrax %>%
    mutate(ppp=(1:n())/n()) %>%
    mutate(q1=quantile(istrax*scaler, 0.25, na.rm = TRUE),
           q2=quantile(istrax*scaler, 0.5, na.rm = TRUE),
           q3=quantile(istrax*scaler, 0.75, na.rm = TRUE),
           top25=ppp<0.25,
           top50=ppp<0.5
    ) %>%
    summarise(
      mean = mean(istrax*scaler, na.rm = TRUE),
      innos = n(),
      sem = sd(istrax*scaler, na.rm = TRUE) / sqrt(n()),

      top25_bin_mean= mean(scaler*istrax[top25==T], na.rm = TRUE),
      top50_bin_mean= mean(scaler*istrax[top50==T], na.rm = TRUE),

      # Top appln_id values (highest istrax) as comma-separated string
      top3_ids = paste(head(appln_id[order(-istrax*scaler)],10), collapse = ", "),

      across(c(q1,q2,q3,top25,top50),mean),
      .groups = "drop"
    ) %>%
    mutate(
      # Create Espacenet search URL for top 3 IDs (use double quotes for JS to avoid HTML attribute conflicts)
      top3_ids_url = build_espacenet_search(top3_ids)
    )

  return(avstrax)
}




plot_avstrax_by_technology <- function(pdata, classes, #green_classes,
                                       technologies, toflow, custom_colors,topn=20,mininno=5,bwidthscale="log",
                                       display_mode="confidence",
                                       show_top3_ids=FALSE,
                                       width_svg=10,
                                       height_svg=6,
                                       plot_title="Spillover returns",
                                       x_label="Country",
                                       comparison_technologies=NULL,
                                       precomputed_avstrax=NULL) {
  #mininno=30;topn=20;  pdata=patchar_countrymap;toflow="istrax_global"; classes=techmap; green_classes=green_classes; technologies="Green Energy"

  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(countrycode)

  # Check if we have comparison technologies
  has_comparison <- !is.null(comparison_technologies) && length(comparison_technologies) > 0

  # Use pre-computed data if provided, otherwise compute
  if (!is.null(precomputed_avstrax) && nrow(precomputed_avstrax) > 0) {
    message("Using pre-computed avstrax data")
    avstrax <- precomputed_avstrax
    avstrax$group <- "Main"
    # For comparison, we still need to compute (pre-computation doesn't cover all combinations)
    has_comparison <- FALSE
  } else {
    # Filter by technology class for main selection
    filtered <- classes %>%
      filter(technology %in% technologies) %>%
      distinct()

    if("All Innovations" %in% technologies) filtered <- data.frame()

    # Compute avstrax for main technologies
    avstrax <- compute_avstrax_for_techs(pdata, toflow, filtered)
    avstrax$group <- "Main"

    # If comparison technologies are selected, compute for those too
    if (has_comparison) {
      filtered_comp <- classes %>%
        filter(technology %in% comparison_technologies) %>%
        distinct()

      if("All Innovations" %in% comparison_technologies) filtered_comp <- data.frame()

      avstrax_comp <- compute_avstrax_for_techs(pdata, toflow, filtered_comp)
      avstrax_comp$group <- "Comparison"
    }
  }

  # Extract mean for "All" from main selection
  allmean <- avstrax %>%
    filter(ctry_code == "All") %>%
    pull(mean)

  innos <- avstrax %>%
    filter(ctry_code == "All") %>%
    pull(innos)

  # Handle edge case where allmean or innos is empty
  if (length(allmean) == 0) allmean <- 0
  if (length(innos) == 0) innos <- 0

  # Use existing country_name if available (e.g., for regions), otherwise use countrycode
  if (!"country_name" %in% names(avstrax) || all(is.na(avstrax$country_name))) {
    avstrax$country_name <- countrycode(avstrax$ctry_code, origin = "iso2c", destination = "country.name.en")
  }

  if (has_comparison) {
    if (!"country_name" %in% names(avstrax_comp) || all(is.na(avstrax_comp$country_name))) {
      avstrax_comp$country_name <- countrycode(avstrax_comp$ctry_code, origin = "iso2c", destination = "country.name.en")
    }
  }

  # Filter and order main data
  avstrax <- avstrax %>%
    filter(ctry_code != "All", innos >= mininno) %>%
    arrange(-mean) %>%
    head(topn)

  # Check if we have data to plot
  if (nrow(avstrax) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(girafe(ggobj = p,
                  width_svg = width_svg,
                  height_svg = height_svg,
                  options = list(opts_sizing(rescale = TRUE, width = 1))))
  }

  # Set factor levels based on main data ordering
  country_order <- as.character(avstrax$country_name[order(avstrax$mean)])
  avstrax$country_name <- factor(as.character(avstrax$country_name), levels = country_order)
  avstrax$x_pos <- as.numeric(avstrax$country_name)

  # Compute bar widths for main data
  # Width is proportional to share of innovations (or log of share if log scale)
  if (bwidthscale == "log") {
    avstrax$width_raw <- log(1 + avstrax$innos)
  } else {
    avstrax$width_raw <- avstrax$innos
  }
  max_width_main <- max(avstrax$width_raw)
  avstrax$width <- avstrax$width_raw / max_width_main

  # Prepare comparison data if available
  has_comp_data <- FALSE
  max_width_comp <- 0
  if (has_comparison) {
    # Filter comparison data to same countries as main
    avstrax_comp <- avstrax_comp %>%
      filter(ctry_code %in% avstrax$ctry_code)

    if (nrow(avstrax_comp) > 0) {
      has_comp_data <- TRUE
      avstrax_comp$country_name <- factor(as.character(avstrax_comp$country_name), levels = country_order)
      avstrax_comp$x_pos <- as.numeric(avstrax_comp$country_name)

      # Compute bar widths for comparison data (separately)
      if (bwidthscale == "log") {
        avstrax_comp$width_raw <- log(1 + avstrax_comp$innos)
      } else {
        avstrax_comp$width_raw <- avstrax_comp$innos
      }
      max_width_comp <- max(avstrax_comp$width_raw)
      avstrax_comp$width <- avstrax_comp$width_raw / max_width_comp
    }
  }

  # Calculate bar positions
  # Allocate space per country: 2x max bar width if comparison, 1x if no comparison
  # This ensures bars never overlap
  # Leave larger gaps between countries/regions for clarity
  bar_gap <- 0.08  # Gap between grouped bars within a country
  country_gap <- 0.15  # Extra gap between countries (reduces available bar space)

  if (has_comp_data) {
    # Space allocation: enough for largest bar from each category plus gaps
    # Scale widths so max width = 0.35 (leaving room for gap, second bar, and country spacing)
    scale_factor <- 0.35
    avstrax$width_scaled <- avstrax$width * scale_factor
    avstrax_comp$width_scaled <- avstrax_comp$width * scale_factor

    # Position main bar on top (higher x position, since coord_flip makes it appear first)
    avstrax <- avstrax %>%
      mutate(
        xmin = x_pos + bar_gap/2,
        xmax = xmin + width_scaled,
        ymin = 0,
        ymax = mean
      )

    # Position comparison bar below main (lower x position)
    avstrax_comp <- avstrax_comp %>%
      mutate(
        xmin = x_pos - scale_factor - bar_gap/2,
        xmax = xmin + width_scaled,
        ymin = 0,
        ymax = mean
      )
  } else {
    # Single bar - scale so max width = 0.7 (centered, with space between countries)
    scale_factor <- 0.7
    avstrax$width_scaled <- avstrax$width * scale_factor

    avstrax <- avstrax %>%
      mutate(
        xmin = x_pos - width_scaled / 2,
        xmax = x_pos + width_scaled / 2,
        ymin = 0,
        ymax = mean
      )
  }

  # Create the plot
  ylab <- ifelse(grepl("strax", toflow), "Return in %", "Millions of $")
  is_return <- grepl("strax", toflow)

  # Add formatted value labels for tooltips
  if (is_return) {
    avstrax$value_label <- paste0(round(avstrax$mean, 1), "%")
  } else {
    avstrax$value_label <- paste0("$", round(avstrax$mean, 1), " million")
  }

  # Define colors for main and comparison
  main_color <- "#3498db"
  comp_color <- "#888888"  # Grey for comparison
  comp_alpha <- 0.6  # Fainter comparison bars

  # Use interactive bars if show_top3_ids is enabled
  # Add group column for legend
  avstrax$group <- "Main"
  if (has_comp_data) {
    avstrax_comp$group <- "Comparison"
    if (is_return) {
      avstrax_comp$value_label <- paste0(round(avstrax_comp$mean, 1), "%")
    } else {
      avstrax_comp$value_label <- paste0("$", round(avstrax_comp$mean, 1), " million")
    }
  }

  if (show_top3_ids) {
    p <- ggplot() +
      geom_rect_interactive(data = avstrax,
                            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                fill = group,
                                data_id = country_name,
                                tooltip = paste0(country_name, ": ", value_label,
                                                 "\nInnovations: ", scales::comma(innos),
                                                 "\nTop IDs: ", top3_ids),
                                onclick = top3_ids_url))
    if (has_comp_data) {
      p <- p + geom_rect_interactive(data = avstrax_comp,
                                     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                         fill = group,
                                         data_id = paste0(country_name, "_comp"),
                                         tooltip = paste0(country_name, " (Comparison): ", value_label,
                                                          "\nInnovations: ", scales::comma(innos),
                                                          "\nTop IDs: ", top3_ids),
                                         onclick = top3_ids_url),
                                     alpha = comp_alpha)
    }
  } else {
    p <- ggplot() +
      geom_rect(data = avstrax,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group))
    if (has_comp_data) {
      p <- p + geom_rect(data = avstrax_comp,
                         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
                         alpha = comp_alpha)
    }
  }

  # Set up color scale for legend
  if (has_comp_data) {
    p <- p + scale_fill_manual(
      values = c("Main" = main_color, "Comparison" = comp_color),
      name = NULL,  # No title for cleaner look
      breaks = c("Main", "Comparison"),
      labels = c("Main category", "Comparison category")
    )
  } else {
    p <- p + scale_fill_manual(
      values = c("Main" = main_color),
      guide = "none"  # No legend needed for single category
    )
  }

  # Add either confidence bands or quartile means based on display_mode
  # Position error bars at the center of each bar: (xmin + xmax) / 2
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(data = avstrax,
                           aes(x = (xmin + xmax) / 2,
                               ymin = ifelse(mean - 1.96 * sem > 0, mean - 1.96 * sem, 0),
                               ymax = mean + 1.96 * sem),
                           width = 0.15, color = "black", linewidth = .4, alpha = .4)
    if (has_comp_data) {
      p <- p + geom_errorbar(data = avstrax_comp,
                             aes(x = (xmin + xmax) / 2,
                                 ymin = ifelse(mean - 1.96 * sem > 0, mean - 1.96 * sem, 0),
                                 ymax = mean + 1.96 * sem),
                             width = 0.15, color = "black", linewidth = .4, alpha = .4)
    }
  } else if (display_mode == "quartiles") {
    p <- p + geom_errorbar(data = avstrax,
                           aes(x = (xmin + xmax) / 2,
                               ymin = top50_bin_mean, ymax = top25_bin_mean, width = width_scaled),
                           color = main_color, linewidth = .5, alpha = .5)
    if (has_comp_data) {
      p <- p + geom_errorbar(data = avstrax_comp,
                             aes(x = (xmin + xmax) / 2,
                                 ymin = top50_bin_mean, ymax = top25_bin_mean, width = width_scaled),
                             color = comp_color, linewidth = .5, alpha = .5)
    }
  }

  p <- p +
    scale_x_continuous(breaks = avstrax$x_pos, labels = avstrax$country_name) +
    labs(
      title = plot_title,
      x = x_label,
      y = ylab
    ) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      text = element_text(family = "Open Sans"),
      axis.text = element_text(family = "Open Sans"),
      axis.title = element_text(family = "Open Sans"),
      legend.position = if (has_comp_data) "bottom" else "none",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.box.background = element_rect(color = "grey80", linewidth = 0.5),
      legend.margin = margin(t = 5, r = 10, b = 5, l = 10)
    ) +
    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = allmean, x = max(avstrax$x_pos) - 0.6,
             label = "Average", angle = -90, vjust = 1.5, size = 4,
             family = "Open Sans", color = "black") +
    coord_flip()

  # Build subtitle with legend info
  subtitle_text <- paste0(as.character(innos), " Innovations")
  if (has_comp_data) {
    subtitle_text <- paste0(subtitle_text, "
Main: ", paste(technologies, collapse = ", "),
                            " | Comparison: ", paste(comparison_technologies, collapse = ", "))
  }

  # Add subtitle and caption
  p <- p + labs(subtitle = subtitle_text,
                caption = "© 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 11, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 10, color = "gray"))

  # Return girafe object for Shiny girafeOutput compatibility
  return(girafe(ggobj = p,
                width_svg = width_svg,
                height_svg = height_svg,
                options = list(
                  opts_sizing(rescale = TRUE, width = 1),
                  opts_hover(css = "cursor:pointer;fill:yellow;"),
                  opts_selection(type = "none"),
                  opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;")
                )))
}


# Build Espacenet search URLs for a comma-separated list of application IDs
build_espacenet_search <- function(id_strings) {
  sapply(id_strings, function(ids) {
    id_vec <- unlist(strsplit(ids, ",\\s*"))
    query <- paste(paste0("ap=", id_vec), collapse = " OR ")
    paste0('window.open("https://worldwide.espacenet.com/patent/search?q=',
           utils::URLencode(query, reserved = TRUE), '")')
  })
}


# ============================================
# Map Plotting Functions
# ============================================

#' Plot world choropleth map showing country-level data
#' @param avstrax_data Data frame with ctry_code and mean columns (output from compute_avstrax_for_techs)
#' @param value_col Column name for values to display (default: "mean")
#' @param color_scale Color scale for the choropleth
#' @param plot_title Title for the map
#' @param is_return Logical, TRUE if values are percentages (returns), FALSE if dollar values
#' @return A plotly object
plot_world_map <- function(avstrax_data,
                           value_col = "mean",
                           color_scale = "Viridis",
                           plot_title = "Returns by Country",
                           is_return = TRUE) {
  library(plotly)
  library(countrycode)

  # Filter out "All" and prepare data

  map_data <- avstrax_data %>%
    filter(ctry_code != "All") %>%
    mutate(
      # Convert ISO2 to ISO3 for plotly
      iso3 = countrycode(ctry_code, origin = "iso2c", destination = "iso3c"),
      country_name = countrycode(ctry_code, origin = "iso2c", destination = "country.name.en"),
      value = !!sym(value_col)
    ) %>%
    filter(!is.na(iso3))

  # Handle empty data
  if (nrow(map_data) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::layout(
        title = list(text = "No data available for map display", x = 0.5),
        annotations = list(
          list(text = "No countries with data to display",
               x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16))
        )
      )
    return(p)
  }

  # Use the explicit is_return parameter for determining format
  is_percentage <- is_return

  # Create hover text - use if/else for scalar condition to avoid ifelse recycling issue
  if (is_percentage) {
    map_data <- map_data %>%
      mutate(
        hover_text = paste0(
          "<b>", country_name, "</b><br>",
          "Value: ", round(value, 1), "%<br>",
          "Innovations: ", scales::comma(innos)
        )
      )
  } else {
    map_data <- map_data %>%
      mutate(
        hover_text = paste0(
          "<b>", country_name, "</b><br>",
          "Value: $", round(value, 2), "M<br>",
          "Innovations: ", scales::comma(innos)
        )
      )
  }

  # Create choropleth
  p <- plotly::plot_ly(
    data = map_data,
    type = "choropleth",
    locations = ~iso3,
    z = ~value,
    text = ~hover_text,
    hoverinfo = "text",
    colorscale = color_scale,
    reversescale = FALSE,
    marker = list(line = list(color = "white", width = 0.5)),
    colorbar = list(
      title = list(text = ifelse(is_percentage, "Return (%)", "Value ($M)")),
      ticksuffix = ifelse(is_percentage, "%", "")
    )
  ) %>%
    plotly::layout(
      title = list(text = plot_title, x = 0.5, font = list(size = 16)),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        coastlinecolor = "grey",
        projection = list(type = "natural earth"),
        showland = TRUE,
        landcolor = "lightgray",
        showocean = TRUE,
        oceancolor = "aliceblue",
        showcountries = TRUE,
        countrycolor = "white",
        countrywidth = 0.5
      ),
      margin = list(l = 0, r = 0, t = 50, b = 0)
    ) %>%
    plotly::config(
      scrollZoom = TRUE,
      displayModeBar = TRUE,
      modeBarButtonsToAdd = list("zoom2d", "pan2d", "resetScale2d")
    )

  return(p)
}


#' Plot UK regions choropleth map showing NUTS1 region-level data
#' Uses leaflet with GeoJSON for proper filled region polygons
#' @param avstrax_data Data frame with ctry_code (NUTS1 codes) and mean columns
#' @param value_col Column name for values to display (default: "mean")
#' @param plot_title Title for the map
#' @param is_return Logical, TRUE if values are percentages (returns), FALSE if dollar values
#' @return A leaflet object
plot_uk_regions_map <- function(avstrax_data,
                                 value_col = "mean",
                                 plot_title = "Returns by UK Region",
                                 is_return = TRUE) {
  library(leaflet)
  library(sf)
  library(htmltools)


  # UK NUTS1 region names mapping
  uk_regions_names <- c(
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

  # Load UK NUTS1 GeoJSON - try local file first, then GitHub
  # Try to load the GeoJSON, with caching
  if (!exists(".uk_nuts1_sf", envir = .GlobalEnv)) {
    uk_sf <- NULL

    # Option 1: Try local file first
    local_geojson <- "uk_nuts1_boundaries.geojson"
    if (file.exists(local_geojson)) {
      tryCatch({
        uk_sf <- sf::st_read(local_geojson, quiet = TRUE)
        message("Loaded UK NUTS1 boundaries from local file")
      }, error = function(e) {
        message("Could not load local GeoJSON: ", e$message)
      })
    }

    # Option 2: Try GitHub source (England & Wales)
    if (is.null(uk_sf)) {
      geojson_url <- "https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/eurostat/ew/nuts1.json"
      tryCatch({
        uk_sf <- sf::st_read(geojson_url, quiet = TRUE)
        message("Loaded UK NUTS1 boundaries from GitHub")
      }, error = function(e) {
        message("Could not load from GitHub: ", e$message)
      })
    }

    # Add Scotland and Northern Ireland from GADM file if missing
    if (!is.null(uk_sf)) {
      # Check which regions we have
      nuts_col <- names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]
      if (!is.null(nuts_col)) {
        existing_codes <- uk_sf[[nuts_col]]
        name_col <- names(uk_sf)[grepl("NUTS.*NM", names(uk_sf), ignore.case = TRUE)][1]

        # Try to load Scotland and NI from GADM file
        gadm_file <- "gadm41_GBR_1.json"
        if (file.exists(gadm_file) && (!"UKM" %in% existing_codes || !"UKN" %in% existing_codes)) {
          tryCatch({
            gadm_sf <- sf::st_read(gadm_file, quiet = TRUE)

            # Add Scotland (UKM) if missing
            if (!"UKM" %in% existing_codes) {
              scotland_sf <- gadm_sf %>% filter(NAME_1 == "Scotland")
              if (nrow(scotland_sf) > 0) {
                scotland_sf <- scotland_sf %>%
                  select(geometry) %>%
                  mutate(!!nuts_col := "UKM")
                if (!is.null(name_col)) scotland_sf[[name_col]] <- "Scotland"
                uk_sf <- rbind(uk_sf, scotland_sf)
                message("Added Scotland from GADM")
              }
            }

            # Add Northern Ireland (UKN) if missing
            if (!"UKN" %in% existing_codes) {
              ni_sf <- gadm_sf %>% filter(NAME_1 == "NorthernIreland")
              if (nrow(ni_sf) > 0) {
                ni_sf <- ni_sf %>%
                  select(geometry) %>%
                  mutate(!!nuts_col := "UKN")
                if (!is.null(name_col)) ni_sf[[name_col]] <- "Northern Ireland"
                uk_sf <- rbind(uk_sf, ni_sf)
                message("Added Northern Ireland from GADM")
              }
            }
          }, error = function(e) {
            message("Could not load GADM file: ", e$message)
          })
        }
      }

      # Standardize column names
      nuts_col <- names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]
      if (!is.null(nuts_col) && !is.na(nuts_col) && nuts_col != "NUTS1CD") {
        uk_sf <- uk_sf %>% rename(NUTS1CD = !!sym(nuts_col))
      }
    }

    assign(".uk_nuts1_sf", uk_sf, envir = .GlobalEnv)
  }

  uk_sf <- get(".uk_nuts1_sf", envir = .GlobalEnv)

  # Handle case where GeoJSON couldn't be loaded
  if (is.null(uk_sf)) {
    # Return empty leaflet with message
    return(
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
        addControl(
          html = "<div style='padding: 10px; background: white;'>Could not load UK region boundaries</div>",
          position = "topright"
        )
    )
  }

  # Filter out "All" and prepare data
  map_data <- avstrax_data %>%
    filter(ctry_code != "All", ctry_code %in% names(uk_regions_names)) %>%
    mutate(
      region_name = uk_regions_names[ctry_code],
      value = !!sym(value_col)
    )

  # Use the explicit is_return parameter for determining format
  is_percentage <- is_return

  # Join data to spatial features
  # First, identify the NUTS code column in the sf object
  nuts_col <- if ("NUTS121CD" %in% names(uk_sf)) "NUTS121CD" else
              if ("NUTS1CD" %in% names(uk_sf)) "NUTS1CD" else
              names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]

  name_col <- if ("NUTS121NM" %in% names(uk_sf)) "NUTS121NM" else
              if ("NUTS1NM" %in% names(uk_sf)) "NUTS1NM" else
              names(uk_sf)[grepl("NUTS.*NM", names(uk_sf), ignore.case = TRUE)][1]

  if (is.null(nuts_col)) {
    return(
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
        addControl(
          html = "<div style='padding: 10px; background: white;'>Could not identify NUTS code column</div>",
          position = "topright"
        )
    )
  }

  # Merge data with spatial features
  uk_sf$nuts_code <- uk_sf[[nuts_col]]
  uk_sf <- uk_sf %>%
    left_join(map_data, by = c("nuts_code" = "ctry_code"))

  # Create color palette
  if (nrow(map_data) > 0) {
    pal <- colorNumeric(
      palette = "viridis",
      domain = map_data$value,
      na.color = "#E0E0E0"  # Light gray for regions without data
    )
  } else {
    pal <- function(x) "#E0E0E0"
  }

  # Create hover labels - compute display_name first
  uk_sf <- uk_sf %>%
    mutate(
      display_name = ifelse(!is.na(region_name), region_name,
                            ifelse(!is.null(name_col) & name_col %in% names(uk_sf),
                                   .[[name_col]], nuts_code))
    )

  # Format value labels based on is_percentage - use if/else for scalar condition
  if (is_percentage) {
    uk_sf <- uk_sf %>%
      mutate(
        value_formatted = paste0(round(value, 1), "%"),
        label_text = ifelse(
          !is.na(value),
          paste0(
            "<strong>", display_name, "</strong><br/>",
            "Value: ", value_formatted, "<br/>",
            "Innovations: ", scales::comma(innos)
          ),
          paste0("<strong>", display_name, "</strong><br/>No data")
        )
      )
  } else {
    uk_sf <- uk_sf %>%
      mutate(
        value_formatted = paste0("$", round(value, 2), "M"),
        label_text = ifelse(
          !is.na(value),
          paste0(
            "<strong>", display_name, "</strong><br/>",
            "Value: ", value_formatted, "<br/>",
            "Innovations: ", scales::comma(innos)
          ),
          paste0("<strong>", display_name, "</strong><br/>No data")
        )
      )
  }

  # Create leaflet map
  map <- leaflet(uk_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(value),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~lapply(label_text, HTML),
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "12px",
        direction = "auto"
      )
    ) %>%
    setView(lng = -2.5, lat = 54.5, zoom = 5)

  # Add legend if we have data - reverse order so high values are on top
  if (nrow(map_data) > 0) {
    # Get the value range
    val_range <- range(map_data$value, na.rm = TRUE)

    # Create a reversed palette for the legend (high values on top)
    # We reverse the domain so colors map correctly when legend is drawn top-to-bottom
    pal_legend <- colorNumeric(
      palette = "viridis",
      domain = c(val_range[2], val_range[1]),  # Reversed domain
      na.color = "#E0E0E0"
    )

    map <- map %>%
      addLegend(
        position = "bottomright",
        colors = pal(seq(val_range[2], val_range[1], length.out = 5)),  # High to low colors
        labels = round(seq(val_range[2], val_range[1], length.out = 5), 1),  # High to low labels
        title = ifelse(is_percentage, "Return (%)", "Value ($M)"),
        opacity = 0.7,
        na.label = "No data"
      )
  }

  # Add title
  map <- map %>%
    addControl(
      html = paste0("<div style='padding: 6px 12px; background: white; ",
                    "border-radius: 4px; font-weight: bold; font-size: 14px;'>",
                    htmltools::htmlEscape(plot_title), "</div>"),
      position = "topright"
    )

  return(map)
}

