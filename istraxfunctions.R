
library(dplyr)
library(tidyr)
library(ggiraph)
library(scales)
library(fst)
library(collapse)  # For fast grouped operations (fgroup_by, fsummarize, etc.)


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
                   "cpcsecs"="purple",
                   "agrifood"="burlywood")



# threshold for bars
win_thres=0.01



### functions

#### agregate the data...
compute_avstrax <- function(data, istrax_var, classes, colorings=NULL) {
  # Use collapse for fast aggregation, but keep dplyr for data manipulation
  library(collapse)
  library(dplyr)

  istrax_sym <- rlang::sym(istrax_var)

  # Check if the required column exists in the data
  if (!istrax_var %in% names(data)) {
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler <- ifelse(grepl("strax", istrax_var), 100, 1)

  # Filter out "All" from classes since we add it separately below
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
    distinct()

  # Pre-compute scaled istrax
  avstrax$istrax_scaled <- avstrax$istrax * scaler

  # Use collapse for fast grouped aggregation
  result <- avstrax %>%
    fgroup_by(technology) %>%
    fsummarize(
      mean = fmean(istrax_scaled, na.rm = TRUE),
      innos = fnobs(istrax_scaled),
      sd_val = fsd(istrax_scaled, na.rm = TRUE),
      q1 = fquantile(istrax_scaled, 0.25, na.rm = TRUE),
      q2 = fquantile(istrax_scaled, 0.5, na.rm = TRUE),
      q3 = fquantile(istrax_scaled, 0.75, na.rm = TRUE)
    ) %>%
    fungroup() %>%
    as.data.frame()

  # Calculate sem
  result$sem <- result$sd_val / sqrt(result$innos)
  result$sd_val <- NULL

  # Compute top25/top50 bin means and top3_ids per group using dplyr
  extras <- avstrax %>%
    group_by(technology) %>%
    arrange(technology, -istrax_scaled) %>%
    mutate(ppp = (1:n()) / n()) %>%
    summarise(
      top25_bin_mean = mean(istrax_scaled[ppp < 0.25], na.rm = TRUE),
      top50_bin_mean = mean(istrax_scaled[ppp < 0.5], na.rm = TRUE),
      top3_ids = paste(head(appln_id[order(-istrax_scaled)], 10), collapse = ", "),
      .groups = "drop"
    ) %>%
    as.data.frame()

  # Merge extras with result
  result <- left_join(result, extras, by = "technology")

  # Add compatibility columns
  result$top25 <- 0.25
  result$top50 <- 0.5

  # Create Espacenet search URL
  result$top3_ids_url <- build_espacenet_search(result$top3_ids)

  # Add greenclass categorization
  result$greenclass <- ifelse(result$technology %in% unlist(colorings["green"]), "green",
                       ifelse(result$technology %in% unlist(colorings["battery"]), "battery",
                       ifelse(result$technology %in% unlist(colorings["hard_to_abate"]), "hard to abate",
                       ifelse(result$technology %in% unlist(colorings["ai"]), "AI",
                       ifelse(result$technology %in% unlist(colorings["cpcsecs"]), "CPC Sections",
                       ifelse(result$technology %in% unlist(colorings["agrifood"]), "agrifood", "other"))))))

  return(result)
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

  # Use collapse for fast aggregation, but keep dplyr for data manipulation
  library(collapse)
  library(dplyr)

  istrax_sym <- rlang::sym(istrax_var)

  # Check if the required column exists in the data
  if (!istrax_var %in% names(data)) {
    similar_cols <- grep("strax|^ev_", names(data), value = TRUE)
    stop(paste0("Column '", istrax_var, "' not found in data. ",
                "Available similar columns: ", paste(similar_cols, collapse = ", "),
                ". All columns: ", paste(head(names(data), 20), collapse = ", ")))
  }

  scaler <- ifelse(grepl("strax", istrax_var), 100, 1)

  # If no filter classes are selected we take all
 if(nrow(classes)==0){
     filtereddata=data
  }  else {
    filtereddata <- data %>%
      inner_join(classes %>% select(docdb_family_id) %>% distinct(), by = "docdb_family_id")
  }

  # Fast count of distinct innovations by country using collapse
  counted <- data %>%
    fgroup_by(ctry_code) %>%
    fsummarize(Allinnos = fndistinct(docdb_family_id)) %>%
    fungroup() %>%
    as.data.frame()
  counted$SumAllinnos <- fndistinct(data$docdb_family_id)  # Total distinct innovations across all countries

  # Include country_name if it exists (for regions)
  has_country_name <- "country_name" %in% names(filtereddata)

  # Build avstrax using dplyr for reliable data manipulation
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
      ) %>%
      distinct()
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
      ) %>%
      distinct()
  }

  # Pre-compute scaled istrax
  avstrax$istrax_scaled <- avstrax$istrax * scaler

  # Use collapse for fast grouped aggregation
  if (has_country_name) {
    result <- avstrax %>%
      fgroup_by(ctry_code, country_name) %>%
      fsummarize(
        mean = fmean(istrax_scaled, na.rm = TRUE),
        innos = fnobs(istrax_scaled),
        sd_val = fsd(istrax_scaled, na.rm = TRUE),
        q1 = fquantile(istrax_scaled, 0.25, na.rm = TRUE),
        q2 = fquantile(istrax_scaled, 0.5, na.rm = TRUE),
        q3 = fquantile(istrax_scaled, 0.75, na.rm = TRUE)
      ) %>%
      fungroup() %>%
      as.data.frame()
  } else {
    result <- avstrax %>%
      fgroup_by(ctry_code) %>%
      fsummarize(
        mean = fmean(istrax_scaled, na.rm = TRUE),
        innos = fnobs(istrax_scaled),
        sd_val = fsd(istrax_scaled, na.rm = TRUE),
        q1 = fquantile(istrax_scaled, 0.25, na.rm = TRUE),
        q2 = fquantile(istrax_scaled, 0.5, na.rm = TRUE),
        q3 = fquantile(istrax_scaled, 0.75, na.rm = TRUE)
      ) %>%
      fungroup() %>%
      as.data.frame()
  }

  # Calculate sem from sd and n
  result$sem <- result$sd_val / sqrt(result$innos)
  result$sd_val <- NULL

  # Compute top25/top50 bin means and top3_ids per group using dplyr
  if (has_country_name) {
    extras <- avstrax %>%
      group_by(ctry_code, country_name) %>%
      arrange(ctry_code, country_name, -istrax_scaled) %>%
      mutate(ppp = (1:n()) / n()) %>%
      summarise(
        top25_bin_mean = mean(istrax_scaled[ppp < 0.25], na.rm = TRUE),
        top50_bin_mean = mean(istrax_scaled[ppp < 0.5], na.rm = TRUE),
        top3_ids = paste(head(appln_id[order(-istrax_scaled)], 10), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame()

    result <- left_join(result, extras, by = c("ctry_code", "country_name"))
  } else {
    extras <- avstrax %>%
      group_by(ctry_code) %>%
      arrange(ctry_code, -istrax_scaled) %>%
      mutate(ppp = (1:n()) / n()) %>%
      summarise(
        top25_bin_mean = mean(istrax_scaled[ppp < 0.25], na.rm = TRUE),
        top50_bin_mean = mean(istrax_scaled[ppp < 0.5], na.rm = TRUE),
        top3_ids = paste(head(appln_id[order(-istrax_scaled)], 10), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame()

    result <- left_join(result, extras, by = "ctry_code")
  }

  # Add top25/top50 mean columns (kept for compatibility)
  result$top25 <- 0.25
  result$top50 <- 0.5

  # Create Espacenet search URL
  result$top3_ids_url <- build_espacenet_search(result$top3_ids)

  # Join with counted for RTA calculation
  result <- left_join(result, counted, by = "ctry_code")

  # Calculate RTA
  total_innos <- fndistinct(filtereddata$docdb_family_id)  # Distinct innovations in filtered data
  # Get SumAllinnos from counted directly (same value for all rows)
  # This avoids issues when "All" row is first alphabetically and has NA for SumAllinnos after join
  sum_all_innos <- counted$SumAllinnos[1]
  result$share_c <- result$innos / result$Allinnos
  result$share <- total_innos / sum_all_innos
  result$RTA <- 2 * result$share_c / (result$share_c + result$share)

  return(result)
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

  # UK NUTS1 region names mapping (for UK region explorer)
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

  # Use existing country_name if available (e.g., for regions), otherwise use countrycode
  if (!"country_name" %in% names(avstrax) || all(is.na(avstrax$country_name))) {
    # First check if codes are UK region codes
    is_uk_region <- avstrax$ctry_code %in% names(uk_regions_names)
    # Try to convert using countrycode (works for ISO2 country codes)
    converted_names <- countrycode(avstrax$ctry_code, origin = "iso2c", destination = "country.name.en", warn = FALSE)
    # Use UK region names for UK codes, countrycode for countries, fallback to code for unknown
    avstrax$country_name <- ifelse(
      is_uk_region,
      uk_regions_names[avstrax$ctry_code],
      ifelse(is.na(converted_names), avstrax$ctry_code, converted_names)
    )
  }

  if (has_comparison) {
    if (!"country_name" %in% names(avstrax_comp) || all(is.na(avstrax_comp$country_name))) {
      is_uk_region_comp <- avstrax_comp$ctry_code %in% names(uk_regions_names)
      converted_names <- countrycode(avstrax_comp$ctry_code, origin = "iso2c", destination = "country.name.en", warn = FALSE)
      avstrax_comp$country_name <- ifelse(
        is_uk_region_comp,
        uk_regions_names[avstrax_comp$ctry_code],
        ifelse(is.na(converted_names), avstrax_comp$ctry_code, converted_names)
      )
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


#' Plot RTA (Revealed Technological Advantage) by country/region
#' Similar to plot_avstrax_by_technology but plots the RTA variable instead of mean
#' @param pdata Patent data with country codes
#' @param classes Technology class mapping
#' @param technologies Selected technology categories
#' @param toflow The return flow variable name
#' @param custom_colors Custom colors for the plot
#' @param topn Number of top countries to show (highest RTA)
#' @param bottomn Number of bottom countries to show (lowest RTA)
#' @param mininno Minimum innovation count threshold (for innos)
#' @param minallinnos Minimum all innovation count threshold (for Allinnos)
#' @param bwidthscale "log" or "proportional" for bar width scaling
#' @param show_top3_ids Whether to show interactive top patent IDs
#' @param width_svg SVG width in inches
#' @param height_svg SVG height in inches
#' @param plot_title Title for the plot
#' @param x_label Label for x-axis (Country or Region)
#' @param precomputed_avstrax Pre-computed avstrax data (optional)
#' @return A girafe object
plot_avstrax_rta <- function(pdata, classes,
                             technologies, toflow, custom_colors, topn = 20, bottomn = 0, mininno = 5, minallinnos = 0, bwidthscale = "log",
                             show_top3_ids = FALSE,
                             width_svg = 10,
                             height_svg = 6,
                             plot_title = "Revealed Technological Advantage (RTA)",
                             x_label = "Country",
                             precomputed_avstrax = NULL) {

  library(dplyr)

  library(ggplot2)
  library(patchwork)
  library(countrycode)

  # Use pre-computed data if provided, otherwise compute
  if (!is.null(precomputed_avstrax) && nrow(precomputed_avstrax) > 0) {
    avstrax <- precomputed_avstrax
  } else {
    # Filter by technology class
    filtered <- classes %>%
      filter(technology %in% technologies) %>%
      distinct()

    if ("All Innovations" %in% technologies) filtered <- data.frame()

    # Compute avstrax for technologies
    avstrax <- compute_avstrax_for_techs(pdata, toflow, filtered)
  }

  # Check if RTA column exists
  if (!"RTA" %in% names(avstrax)) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "RTA data not available", size = 6) +
      theme_void()
    return(girafe(ggobj = p,
                  width_svg = width_svg,
                  height_svg = height_svg,
                  options = list(opts_sizing(rescale = TRUE, width = 1))))
  }

  # Extract global average RTA (should be around 1.0 for balanced data)
  # RTA = 1 means country has same specialization as global average
  global_rta <- 1.0  # Reference line at RTA = 1

  innos <- avstrax %>%
    filter(ctry_code == "All") %>%
    pull(innos)

  if (length(innos) == 0) innos <- 0

  # UK NUTS1 region names mapping (for UK region explorer)
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

  # Use existing country_name if available (e.g., for regions), otherwise use countrycode
  if (!"country_name" %in% names(avstrax) || all(is.na(avstrax$country_name))) {
    # First check if codes are UK region codes
    is_uk_region <- avstrax$ctry_code %in% names(uk_regions_names)
    # Try to convert using countrycode (works for ISO2 country codes)
    converted_names <- countrycode(avstrax$ctry_code, origin = "iso2c", destination = "country.name.en", warn = FALSE)
    # Use UK region names for UK codes, countrycode for countries, fallback to code for unknown
    avstrax$country_name <- ifelse(
      is_uk_region,
      uk_regions_names[avstrax$ctry_code],
      ifelse(is.na(converted_names), avstrax$ctry_code, converted_names)
    )
  }

  # Filter by minimum innovations (innos and Allinnos) and valid RTA
  message("RTA Debug: Before filter - rows = ", nrow(avstrax), ", mininno = ", mininno, ", minallinnos = ", minallinnos)
  avstrax_filtered <- avstrax %>%
    filter(ctry_code != "All", innos >= mininno, !is.na(RTA))
  message("RTA Debug: After innos/RTA filter - rows = ", nrow(avstrax_filtered))

  # Apply Allinnos filter if the column exists and threshold is set
  if ("Allinnos" %in% names(avstrax_filtered) && minallinnos > 0) {
    avstrax_filtered <- avstrax_filtered %>%
      filter(Allinnos >= minallinnos)
    message("RTA Debug: After Allinnos filter - rows = ", nrow(avstrax_filtered))
  }

  # Get top n and bottom n countries by RTA
  top_countries <- avstrax_filtered %>%
    arrange(-RTA) %>%
    head(topn)

  if (bottomn > 0) {
    bottom_countries <- avstrax_filtered %>%
      arrange(RTA) %>%
      head(bottomn)
    # Combine top and bottom, removing duplicates
    avstrax <- bind_rows(top_countries, bottom_countries) %>%
      distinct(ctry_code, .keep_all = TRUE) %>%
      arrange(-RTA)
  } else {
    avstrax <- top_countries
  }

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

  # Set factor levels based on RTA ordering
  country_order <- as.character(avstrax$country_name[order(avstrax$RTA)])
  avstrax$country_name <- factor(as.character(avstrax$country_name), levels = country_order)
  avstrax$x_pos <- as.numeric(avstrax$country_name)

  # Compute bar widths (proportional to innovations)
  if (bwidthscale == "log") {
    avstrax$width_raw <- log(1 + avstrax$innos)
  } else {
    avstrax$width_raw <- avstrax$innos
  }
  max_width <- max(avstrax$width_raw)
  avstrax$width <- avstrax$width_raw / max_width

  # Single bar scaling
  scale_factor <- 0.7
  avstrax$width_scaled <- avstrax$width * scale_factor

  avstrax <- avstrax %>%
    mutate(
      xmin = x_pos - width_scaled / 2,
      xmax = x_pos + width_scaled / 2,
      ymin = 0,
      ymax = RTA
    )

  # Create value labels for tooltips
  avstrax$value_label <- paste0("RTA: ", round(avstrax$RTA, 2))

  # Color based on RTA value (above/below 1)
  main_color <- "#e74c3c"  # Red for RTA plot to distinguish from mean plot

  if (show_top3_ids) {
    p <- ggplot() +
      geom_rect_interactive(data = avstrax,
                            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                data_id = country_name,
                                tooltip = paste0(country_name, "\n", value_label,
                                                 "\nInnovations: ", scales::comma(innos),
                                                 "\nTop IDs: ", top3_ids),
                                onclick = top3_ids_url),
                            fill = main_color)
  } else {
    p <- ggplot() +
      geom_rect(data = avstrax,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = main_color)
  }

  p <- p +
    scale_x_continuous(breaks = avstrax$x_pos, labels = avstrax$country_name) +
    labs(
      title = plot_title,
      x = x_label,
      y = "RTA Index"
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
      legend.position = "none"
    ) +
    geom_hline(yintercept = global_rta, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = global_rta, x = max(avstrax$x_pos) - 0.6,
             label = "RTA = 1", angle = -90, vjust = 1.5, size = 4,
             family = "Open Sans", color = "black") +
    coord_flip()

  # Add subtitle and caption
  subtitle_text <- paste0(as.character(innos), " Innovations | RTA > 1 = Specialization advantage")

  p <- p + labs(subtitle = subtitle_text,
                caption = "© 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 11, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 10, color = "gray"))

  # Return girafe object
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


#' Plot scatter plot of RTA vs Returns
#' @param avstrax_data Data frame with ctry_code, RTA, mean, and innos columns
#' @param mininno Minimum innovation count threshold (for innos)
#' @param minallinnos Minimum all innovation count threshold (for Allinnos)
#' @param bwidthscale "log" or "proportional" for dot size scaling
#' @param width_svg SVG width in inches
#' @param height_svg SVG height in inches
#' @param plot_title Title for the plot
#' @param x_label Label for x-axis
#' @param y_label Label for y-axis
#' @return A girafe object
plot_rta_returns_scatter <- function(avstrax_data,
                                      mininno = 5,
                                      minallinnos = 0,
                                      bwidthscale = "log",
                                      width_svg = 8,
                                      height_svg = 6,
                                      plot_title = "RTA vs Returns",
                                      x_label = "RTA",
                                      y_label = "Return (%)") {

  library(dplyr)
  library(ggplot2)
  library(ggiraph)
  library(countrycode)

  # Check if required columns exist
  if (!"RTA" %in% names(avstrax_data) || !"mean" %in% names(avstrax_data)) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "RTA or Returns data not available", size = 6) +
      theme_void()
    return(girafe(ggobj = p,
                  width_svg = width_svg,
                  height_svg = height_svg,
                  options = list(opts_sizing(rescale = TRUE, width = 1))))
  }

  # UK NUTS1 region names mapping (for UK region explorer)
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

  # Add country names if not present
  if (!"country_name" %in% names(avstrax_data) || all(is.na(avstrax_data$country_name))) {
    is_uk_region <- avstrax_data$ctry_code %in% names(uk_regions_names)
    converted_names <- countrycode(avstrax_data$ctry_code, origin = "iso2c", destination = "country.name.en", warn = FALSE)
    avstrax_data$country_name <- ifelse(
      is_uk_region,
      uk_regions_names[avstrax_data$ctry_code],
      ifelse(is.na(converted_names), avstrax_data$ctry_code, converted_names)
    )
  }


  # Get the average return value from the "All" row (same as displayed in bar chart)
  all_mean <- avstrax_data %>%
    filter(ctry_code == "All") %>%
    pull(mean)
  if (length(all_mean) == 0 || is.na(all_mean)) all_mean <- NULL

  # Filter data
  scatter_data <- avstrax_data %>%
    filter(ctry_code != "All", innos >= mininno, !is.na(RTA), !is.na(mean))

  # Apply Allinnos filter if the column exists and threshold is set
  if ("Allinnos" %in% names(scatter_data) && minallinnos > 0) {
    scatter_data <- scatter_data %>%
      filter(Allinnos >= minallinnos)
  }

  # Check if we have data to plot
  if (nrow(scatter_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(girafe(ggobj = p,
                  width_svg = width_svg,
                  height_svg = height_svg,
                  options = list(opts_sizing(rescale = TRUE, width = 1))))
  }

  # Calculate dot sizes based on innovation count
  if (bwidthscale == "log") {
    scatter_data$size_raw <- log(1 + scatter_data$innos)
  } else {
    scatter_data$size_raw <- scatter_data$innos
  }

  # Normalize sizes for plotting (scale to reasonable range)
  max_size <- max(scatter_data$size_raw, na.rm = TRUE)
  min_size <- min(scatter_data$size_raw, na.rm = TRUE)
  if (max_size > min_size) {
    scatter_data$size_scaled <- 3 + 12 * (scatter_data$size_raw - min_size) / (max_size - min_size)
  } else {
    scatter_data$size_scaled <- 7
  }

  # Create tooltip text
  scatter_data$tooltip_text <- paste0(
    "<b>", scatter_data$country_name, "</b><br>",
    "RTA: ", round(scatter_data$RTA, 2), "<br>",
    "Return: ", round(scatter_data$mean, 1), "%<br>",
    "Innovations: ", scales::comma(scatter_data$innos)
  )

  # Color based on RTA (blue for low, red for high)
  # Create the scatter plot
  p <- ggplot(scatter_data, aes(x = RTA, y = mean)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_point_interactive(
      aes(
        size = size_scaled,
        fill = RTA,
        tooltip = tooltip_text,
        data_id = ctry_code
      ),
      shape = 21,
      color = "white",
      stroke = 0.5,
      alpha = 0.8
    ) +
    scale_fill_gradient2(
      low = "#08306B",      # Dark blue for low RTA
      mid = "#FFFFFF",      # White for RTA = 1
      high = "#B2182B",     # Dark red for high RTA
      midpoint = 1,
      name = "RTA"
    ) +
    scale_size_identity() +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      text = element_text(family = "Open Sans"),
      legend.position = "right",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5)
    ) +
    annotate("text", x = 1, y = max(scatter_data$mean, na.rm = TRUE) * 0.95,
             label = "RTA = 1", hjust = -0.1, size = 3.5,
             family = "Open Sans", color = "gray40")

  # Add horizontal dashed line for average return (from "All" row)
  if (!is.null(all_mean)) {
    p <- p +
      geom_hline(yintercept = all_mean, linetype = "dashed", color = "#3498db", linewidth = 0.8) +
      annotate("text", x = max(scatter_data$RTA, na.rm = TRUE) * 0.95, y = all_mean,
               label = paste0("Avg: ", round(all_mean, 1), "%"), vjust = -0.5, size = 3.5,
               family = "Open Sans", color = "#3498db")
  }

  # Add caption
  p <- p + labs(caption = "© 2025 Innovation Strategy Explorer") +
    theme(plot.caption = element_text(hjust = 1, size = 9, color = "gray"))

  # Return girafe object
  return(girafe(ggobj = p,
                width_svg = width_svg,
                height_svg = height_svg,
                options = list(
                  opts_sizing(rescale = TRUE, width = 1),
                  opts_hover(css = "cursor:pointer;opacity:1;"),
                  opts_selection(type = "none"),
                  opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;font-size:12px;")
                )))
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
  is_rta <- value_col == "RTA"

  # Create hover text - use if/else for scalar condition to avoid ifelse recycling issue
  if (is_rta) {
    map_data <- map_data %>%
      mutate(
        hover_text = paste0(
          "<b>", country_name, "</b><br>",
          "RTA: ", round(value, 2), "<br>",
          "Innovations: ", scales::comma(innos)
        )
      )
  } else if (is_percentage) {
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

  # Determine colorbar title and suffix based on value type
  if (is_rta) {
    colorbar_title <- "RTA"
    colorbar_suffix <- ""
    # Diverging color scale for RTA: dark blue (low) -> light blue -> white (1.0) -> light red -> dark red (high)
    # Create custom colorscale centered at RTA = 1
    rta_min <- min(map_data$value, na.rm = TRUE)
    rta_max <- max(map_data$value, na.rm = TRUE)

    # Normalize so that 1.0 maps to the midpoint (0.5 in colorscale)
    # colorscale values must be between 0 and 1
    if (rta_max > rta_min) {
      # Calculate where 1.0 falls in the normalized scale
      mid_point <- (1.0 - rta_min) / (rta_max - rta_min)
      mid_point <- max(0.01, min(0.99, mid_point))  # Clamp to valid range
    } else {
      mid_point <- 0.5
    }

    # Custom diverging colorscale: blue -> white -> red
    rta_colorscale <- list(
      list(0, "rgb(8, 48, 107)"),       # Dark blue
      list(mid_point * 0.5, "rgb(66, 146, 198)"),  # Medium blue
      list(mid_point, "rgb(255, 255, 255)"),       # White at RTA = 1
      list(mid_point + (1 - mid_point) * 0.5, "rgb(239, 138, 98)"),  # Medium red
      list(1, "rgb(178, 24, 43)")        # Dark red
    )
    use_colorscale <- rta_colorscale
  } else if (is_percentage) {
    colorbar_title <- "Return (%)"
    colorbar_suffix <- "%"
    use_colorscale <- color_scale
  } else {
    colorbar_title <- "Value ($M)"
    colorbar_suffix <- ""
    use_colorscale <- color_scale
  }

  # Create choropleth
  p <- plotly::plot_ly(
    data = map_data,
    type = "choropleth",
    locations = ~iso3,
    z = ~value,
    text = ~hover_text,
    hoverinfo = "text",
    colorscale = use_colorscale,
    reversescale = FALSE,
    marker = list(line = list(color = "white", width = 0.5)),
    colorbar = list(
      title = list(text = colorbar_title),
      ticksuffix = colorbar_suffix
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

  # Check if value column exists
  if (!value_col %in% names(avstrax_data)) {
    return(
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
        addControl(
          html = paste0("<div style='padding: 10px; background: white;'>Column '", value_col, "' not found in data</div>"),
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
    ) %>%
    filter(!is.na(value))  # Remove rows with NA values

  # Handle empty data after filtering
  if (nrow(map_data) == 0) {
    return(
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
        addControl(
          html = "<div style='padding: 10px; background: white;'>No valid data available for UK regions</div>",
          position = "topright"
        )
    )
  }

  # Use the explicit is_return parameter for determining format
  is_percentage <- is_return
  is_rta <- value_col == "RTA"

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
    if (is_rta) {
      # Diverging palette for RTA: blue (low) -> white (1.0) -> red (high)
      rta_min <- min(map_data$value, na.rm = TRUE)
      rta_max <- max(map_data$value, na.rm = TRUE)

      # Create a custom diverging palette centered at 1.0
      # Use colorRamp to create a smooth gradient
      rta_colors <- c("#08306B", "#4292C6", "#FFFFFF", "#EF8A62", "#B2182B")

      # Calculate domain to center white at RTA = 1
      # Extend domain symmetrically around 1 if needed
      max_deviation <- max(abs(rta_max - 1), abs(1 - rta_min))
      sym_min <- 1 - max_deviation
      sym_max <- 1 + max_deviation

      pal <- colorNumeric(
        palette = rta_colors,
        domain = c(sym_min, sym_max),
        na.color = "#E0E0E0"
      )
    } else {
      pal <- colorNumeric(
        palette = "viridis",
        domain = map_data$value,
        na.color = "#E0E0E0"  # Light gray for regions without data
      )
    }
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
  if (is_rta) {
    uk_sf <- uk_sf %>%
      mutate(
        value_formatted = paste0("RTA: ", round(value, 2)),
        label_text = ifelse(
          !is.na(value),
          paste0(
            "<strong>", display_name, "</strong><br/>",
            value_formatted, "<br/>",
            "Innovations: ", scales::comma(innos)
          ),
          paste0("<strong>", display_name, "</strong><br/>No data")
        )
      )
  } else if (is_percentage) {
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

    if (is_rta) {
      # For RTA, use symmetric domain centered at 1.0
      max_deviation <- max(abs(val_range[2] - 1), abs(1 - val_range[1]))
      sym_min <- 1 - max_deviation
      sym_max <- 1 + max_deviation

      # Create legend with diverging colors (high to low)
      legend_values <- seq(sym_max, sym_min, length.out = 5)
      legend_colors <- pal(legend_values)

      map <- map %>%
        addLegend(
          position = "bottomright",
          colors = legend_colors,
          labels = round(legend_values, 2),
          title = "RTA",
          opacity = 0.7,
          na.label = "No data"
        )
    } else {
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
