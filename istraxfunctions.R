



library(dplyr)
library(tidyr)




# Define custom colors
custom_colors <- c("green" = "forestgreen", "other" = "gray70")







### functions

#### agregate the data...
compute_avstrax <- function(data, istrax_var, classes, green_classes) {
  library(dplyr)
  
  istrax_sym <- rlang::sym(istrax_var)
  
  avstrax <- data %>%
    select(docdb_family_id, !!istrax_sym) %>%
    rename(istrax = !!istrax_sym) %>%
    distinct() %>%
    inner_join(classes, by = "docdb_family_id") %>%
    bind_rows(
      data %>%
        select(docdb_family_id, starts_with("istrax")) %>%
        rename(istrax = !!istrax_sym) %>%
        distinct() %>%
        mutate(technology = "All")
    ) %>%
    distinct() %>%
    group_by(technology) %>%
    summarise(
      mean = mean(istrax*100, na.rm = TRUE),
      innos = n(),
      sem = sd(istrax*100, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      greenclass = ifelse(technology %in% green_classes, "green", "other")
    )
  
  return(avstrax)
}



#### Draw the plots
plot_avstrax_by_country <- function(pdata, classes, green_classes, country_code, toflow, custom_colors) {
  library(dplyr)
  library(ggplot2)
  
  library(patchwork)
  # Filter by country and year
  filtered <- pdata %>%
    filter(ctry_code %in% country_code )  %>%
    distinct()
  
  # Compute avstrax
  avstrax <- compute_avstrax(filtered, toflow, classes, green_classes)
  
  # Extract mean for "All"
  allmean <- avstrax %>%
    filter(technology == "All") %>%
    pull(mean)
  
  innos=  avstrax %>%
    filter(technology == "All") %>%
    pull(innos)
  
  # Prepare data for plotting
  avstrax <- avstrax %>%
    filter(technology != "All") %>%
    arrange(technology) %>%
    mutate(
      linnos = log(innos),
      width = linnos / max(linnos),
      xmin = as.numeric(factor(technology)) - width / 2,
      xmax = as.numeric(factor(technology)) + width / 2,
      ymin = 0,
      ymax = mean
    )
  
  # Create the plot
  p=ggplot(avstrax) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = greenclass)) +
    geom_errorbar(aes(x = as.numeric(factor(technology)), ymin = mean - 1.96 * sem, ymax = mean + 1.96 * sem),
                  width = 0.2, color = "black", linewidth = .4,alpha=.4) +
    scale_x_continuous(breaks = as.numeric(factor(avstrax$technology)), labels = avstrax$technology) +
    scale_fill_manual(values = custom_colors) +
    labs(
      title = "Spillover returns",
      x = "Technology",
      y = "Return in %",
      fill = "Technology"
    ) +
    theme_minimal() +
    
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )+
    
    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
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
    annotate("text", x = 0.5, y = 0.5, label = paste0(as.character(innos)," Innovations"), size=5 ) +
    theme(plot.margin = margin(0, 0, -10, 0))
  
  
  # Combine them
  annotation_plot / p + plot_layout(heights = c(0.1, 1))+
  
  
  
  labs(caption = "© 2025 Industrial Strategy Explorer") +
    theme(
      plot.caption = element_text(hjust = 1, size = 10, color = "gray")
    )
  
  #library(cowplot)
  
  
  
  #ggdraw(p) +
  #draw_text(paste0(as.character(innos)," Innovations"), x = 0.85, y = 0.95, size = 14)
  
}




compute_avstrax_for_techs <- function(data, istrax_var, classes, green_classes) {
  #data=patchar_countrymap;istrax_var="istrax_global"; classes=filtered; green_classes=green_classes
  
  
  library(dplyr)
  
  
  istrax_sym <- rlang::sym(istrax_var)
  
  avstrax <- data %>% inner_join(classes %>% select(docdb_family_id) %>% distinct()) %>% 
    select(docdb_family_id, !!istrax_sym, ctry_code) %>%
    rename(istrax = !!istrax_sym) %>%
    distinct() %>%

    bind_rows(
      #atest=
      data %>%  inner_join(classes %>% select(docdb_family_id) %>% distinct()) %>% 
        select(docdb_family_id, !!istrax_sym,) %>%
        rename(istrax = !!istrax_sym) %>%
        distinct() %>%
        mutate(ctry_code = "All")
    ) %>%
    
    distinct() %>%
    group_by(ctry_code) %>%
    summarise(
      mean = mean(istrax*100, na.rm = TRUE),
      innos = n(),
      sem = sd(istrax*100, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) #%>%
    #mutate(
    #  greenclass = ifelse(technology %in% green_classes, "green", "other")
    #)
  
  return(avstrax)
}




plot_avstrax_by_technology <- function(pdata, classes, green_classes, technologies, toflow, custom_colors,topn=20,mininno=5) {
  #mininno=30;topn=20;  pdata=patchar_countrymap;toflow="istrax_global"; classes=techmap; green_classes=green_classes; technologies="Green Energy"

  library(dplyr)
  library(ggplot2)
  
  library(patchwork)
  # Filter by technology class
  filtered <- classes %>%
    filter(technology %in% technologies )  %>%
    distinct()
  
  # Compute avstrax
  avstrax <- compute_avstrax_for_techs(pdata, toflow, filtered, green_classes)
  
  
  
  
  
  
  # Extract mean for "All"
  allmean <- avstrax %>%
    filter( ctry_code=="All") %>%
    pull(mean)
  
  innos=  avstrax %>%
    filter( ctry_code=="All") %>%
    pull(innos)
  
  # Prepare data for plotting
  
  library(countrycode)
  
  avstrax$country_name <- countrycode(avstrax$ctry_code, origin = "iso2c", destination = "country.name.en")
  
  
  
  
  avstrax$ctry_code <- factor(avstrax$ctry_code, levels = avstrax$ctry_code[order(avstrax$mean)])
  avstrax$country_name <- factor(avstrax$country_name, levels = avstrax$country_name[order(avstrax$mean)])
  
  avstrax <- avstrax %>%  
    filter( ctry_code!="All",innos>=mininno) %>% 
    
    arrange(-mean) %>% 
    head(topn)%>%
    mutate(
      linnos = log(innos),
      width = linnos / max(linnos),
      xmin = as.numeric(factor(country_name)) - width / 2,
      xmax = as.numeric(factor(country_name)) + width / 2,
      ymin = 0,
      ymax = mean
    ) 
  

  # Create the plot
  
  
  p <- ggplot(avstrax, aes(x = country_name)) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_errorbar(aes(ymin = mean - 1.96 * sem, ymax = mean + 1.96 * sem),
                  width = 0.2, color = "black", linewidth = .4, alpha = .4) +
    labs(
      title = "Spillover returns",
      x = "Country",
      y = "Return in %",
      fill = "Country"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    ) +
    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
    coord_flip()
  
  
  #innos=3
  annotation_plot <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = paste0(as.character(innos)," Innovations"), size=5 ) +
    theme(plot.margin = margin(0, 0, -10, 0))
  
  
  # Combine them
  annotation_plot / p + plot_layout(heights = c(0.1, 1))+
    
    labs(caption = "© 2025 Industrial Strategy Explorer") +
    theme(
      plot.caption = element_text(hjust = 1, size = 10, color = "gray")
    )
  
  
  #library(cowplot)
  
  
  
  #ggdraw(p) +
  #draw_text(paste0(as.character(innos)," Innovations"), x = 0.85, y = 0.95, size = 14)
  
}


