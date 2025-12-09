
library(dplyr)
library(tidyr)




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
  
  scaler=ifelse(grepl("strax", istrax_var ),100,1)
  
  avstrax <- data %>%
    select(docdb_family_id, !!istrax_sym) %>%
    rename(istrax = !!istrax_sym) %>%
    distinct() %>%
    inner_join(classes, by = "docdb_family_id") %>%
    bind_rows(
      data %>%
        #select(docdb_family_id, starts_with("istrax")) %>%
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
    
    q1_bin_mean = mean(scaler*istrax[scaler*istrax <= q1], na.rm = TRUE),
    q2_bin_mean = mean(scaler*istrax[scaler*istrax <= q2 & scaler*istrax>=q1], na.rm = TRUE),
    q3_bin_mean = mean(scaler*istrax[scaler*istrax <= q3 & scaler*istrax>=q2], na.rm = TRUE),
    q4_bin_mean = mean(scaler*istrax[scaler*istrax > q3], na.rm = TRUE),
    
    q0M_bin_mean= mean(scaler*istrax[(scaler*istrax) <= q2], na.rm = TRUE),
    q1M_bin_mean= mean(scaler*istrax[(scaler*istrax) > q2], na.rm = TRUE),
    
    top25_bin_mean= mean(scaler*istrax[top25==T], na.rm = TRUE),
    top50_bin_mean= mean(scaler*istrax[top50==T], na.rm = TRUE),
    
    
    across(c(q1,q2,q3,top25,top50),mean),
      .groups = "drop"
    ) %>%
    mutate(
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
                                    display_mode="confidence"
                                    #battery_classes = NULL,
                                    #hard_to_abate_classes=NULL
                                    ) {
  library(dplyr)
  library(ggplot2)

  library(patchwork)
  #classes=techmap %>% filter(technology=="All"); toflow="istrax_global"; pdata=patchar_countrymap; country_code="VN"
  classlist=(classes %>% distinct(technology))$technology

  #toflow="istrax_global"; pdata=countrymap
  #ylab=ifelse(grepl("Return", toflow ),"Return in %","Millions of $")
  ylab=ifelse(grepl("strax", toflow ),"Return in %","Millions of $")
  #scaler=ifelse(grepl("strax", toflow ),100,1)
  # Filter by country and year
  filtered <- pdata %>%
    filter(ctry_code %in% country_code )  %>%
    distinct()

  # Compute avstrax
  avstrax <- compute_avstrax(filtered, toflow, classes,colorings#, green_classes, battery_classes,hard_to_abate_classes
                             )
  
  # Extract mean for "All"
  allmean <- avstrax %>%
    filter(technology == "All") %>%
    pull(mean)
  
  innos=  avstrax %>%
    filter(technology == "All") %>%
    pull(innos)
  
  # Prepare data for plotting
  #display_mode="quartiles";bwidthscale="log"
  if(!"All" %in% classlist) avstrax=avstrax %>% filter(technology != "All") 
  
  
  avstrax <- avstrax %>% 
    #filter(technology != "All") %>%
    arrange(technology) %>%
    mutate(
      linnos1 = innos,
      linnos2 = log(1+innos),
      bwidthscale = bwidthscale,
    ) %>% 
    filter(innos>1) %>% 
    mutate(
      linnos=ifelse(bwidthscale=="log",linnos2,linnos1),

      width = linnos / max(linnos),
      #width =ifelse( innos / max(innos)>win_thres,innos / max(innos),win_thres),
      
      xmin = as.numeric(factor(technology)) - width / 2,
      xmax = as.numeric(factor(technology)) + width / 2,
      ymin = 0,
      ymax = mean
    )
  
  # Create the plot
  
  p=ggplot(avstrax) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = greenclass))

  # Add either confidence bands or quartile means based on display_mode
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(aes(x = as.numeric(factor(technology)), ymin = mean - 1.96 * sem, ymax = mean + 1.96 * sem),
                           width = 0.2, color = "black", linewidth = .4, alpha = .4)
  } else if (display_mode == "quartiles") {
    p <- p + # geom_errorbar(aes(x = as.numeric(factor(technology)),ymin = q1_bin_mean, ymax = q2_bin_mean, width = width),
      #color = "brown",
      #                      linewidth = .7, alpha = .5)+
            #geom_errorbar(aes(x = as.numeric(factor(technology)),ymin = q1, ymax = q2,width = width),
        #              color = "#3498db",
       #               linewidth = .7, alpha = .5)+
      
            #geom_errorbar(aes(x = as.numeric(factor(technology)),ymin = q2, ymax = q3,width = width),
         #           color = "#3498db",
          #          linewidth = .7, alpha = .5)+
      
            geom_errorbar(aes(color=greenclass,x = as.numeric(factor(technology)),ymin = top50_bin_mean, ymax = top25_bin_mean,width=width*1.05),
                     linewidth = 1, alpha = .5)
    
    
    
  }

  p <- p +
    scale_x_continuous(breaks = as.numeric(factor(avstrax$technology)), labels = avstrax$technology) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(
      title = "Spillover returns",
      x = "Technology",
      y = ylab,
      fill = "Technology"
    ) +
    guides(color = "none")+
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
  
  
  
  labs(caption = "© 2025 Innovation Strategy Explorer") +
    theme(
      plot.caption = element_text(hjust = 1, size = 10, color = "gray")
    )
  
  #library(cowplot)
  
  
  
  #ggdraw(p) +
  #draw_text(paste0(as.character(innos)," Innovations"), x = 0.85, y = 0.95, size = 14)
  
}




compute_avstrax_for_techs <- function(data, istrax_var, classes#, green_classes
                                      ) {
  #data=patchar_countrymap;istrax_var="istrax_global"; classes=filtered; green_classes=green_classes;classes=data.frame()
  
  
  library(dplyr)
  
  
  istrax_sym <- rlang::sym(istrax_var)
  
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
  avstrax <- filtereddata %>% 
    select(docdb_family_id, !!istrax_sym, ctry_code) %>%
    rename(istrax = !!istrax_sym) %>%
    distinct() %>%

    bind_rows(
      #atest=
      filtereddata %>% 
        select(docdb_family_id, !!istrax_sym,) %>%
        rename(istrax = !!istrax_sym) %>%
        distinct() %>%
        mutate(ctry_code = "All")
    ) %>%
    
    distinct() %>%
    group_by(ctry_code) %>%
    arrange(ctry_code,-istrax*scaler) %>% 
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
      # Quartile bin means: mean of observations within each quartile bin
      
      q1_bin_mean = mean(scaler*istrax[scaler*istrax <= q1], na.rm = TRUE),
      q2_bin_mean = mean(scaler*istrax[scaler*istrax <= q2 & scaler*istrax>=q1], na.rm = TRUE),
      q3_bin_mean = mean(scaler*istrax[scaler*istrax <= q3 & scaler*istrax>=q2], na.rm = TRUE),
      q4_bin_mean = mean(scaler*istrax[scaler*istrax >= q3], na.rm = TRUE),
      
      q0M_bin_mean= mean(scaler*istrax[scaler*istrax <= q2], na.rm = TRUE),
      q1M_bin_mean= mean(scaler*istrax[scaler*istrax >= q2], na.rm = TRUE),
      top25_bin_mean= mean(scaler*istrax[top25==T], na.rm = TRUE),
      top50_bin_mean= mean(scaler*istrax[top50==T], na.rm = TRUE),
      across(c(q1,q2,q3,top25,top50),mean),
      .groups = "drop"
    ) #%>%
    #mutate(
    #  greenclass = ifelse(technology %in% green_classes, "green", "other")
    #)
  
  return(avstrax)
}




plot_avstrax_by_technology <- function(pdata, classes, #green_classes,
                                       technologies, toflow, custom_colors,topn=20,mininno=5,bwidthscale="log",
                                       display_mode="confidence") {
  #mininno=30;topn=20;  pdata=patchar_countrymap;toflow="istrax_global"; classes=techmap; green_classes=green_classes; technologies="Green Energy"

  library(dplyr)
  library(ggplot2)
  
  library(patchwork)
  # Filter by technology class
  filtered <- classes %>%
    filter(technology %in% technologies )  %>%
    distinct()
  
  if("All Innovations" %in% technologies) filtered=data.frame()
  # Compute avstrax
  avstrax <- compute_avstrax_for_techs(pdata, toflow, filtered)#, green_classes)
  
  
  
  
  
  
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
  
  
  
  
  avstrax$ctry_code    <- factor(avstrax$ctry_code, levels = avstrax$ctry_code[order(avstrax$mean)])
  avstrax$country_name <- factor(avstrax$country_name, levels = avstrax$country_name[order(avstrax$mean)])
  
  avstrax <- avstrax %>%  
    filter( ctry_code!="All",innos>=mininno) %>% 
    
    arrange(-mean) %>% 
    head(topn)%>%
    mutate(
      #linnos = log(innos),
      linnos1 = innos,
      linnos2 = log(1+innos),
      bwidthscale=bwidthscale,
      linnos=ifelse(bwidthscale=="log",linnos2,linnos1),      
      width = linnos / max(linnos),
      
      #width =ifelse( innos / max(innos)>win_thres,innos / max(innos),win_thres),
      
      
      xmin = as.numeric(factor(country_name)) - width / 2,
      xmax = as.numeric(factor(country_name)) + width / 2,
      ymin = 0,
      ymax = mean
    ) 
  

  # Create the plot
  ylab=ifelse(grepl("strax", toflow ),"Return in %","Millions of $")

  p <- ggplot(avstrax, aes(x = country_name)) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))

  # Add either confidence bands or quartile means based on display_mode
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(aes(ymin = mean - 1.96 * sem, ymax = mean + 1.96 * sem),
                           width = 0.2, color = "black", linewidth = .4, alpha = .4)
  } else if (display_mode == "quartiles") {
#    p <- p + 
#         geom_errorbar(aes(ymin = q1_bin_mean, 
#                           ymax = q2,width=width),
#                           width = 0.2, color = "brown",
#                           linewidth = .4, alpha = .5)+
#        geom_errorbar(aes(ymin = q2, ymax = q4_bin_mean,width=width),
#                      color = "brown", 
#                      linewidth = .4, alpha = .5)
    
    
    p <- p + #geom_errorbar(aes(x = as.numeric(factor(country_name)),ymin = q1_bin_mean, ymax = q2_bin_mean, width = width),
             #               color = "brown",
             #               linewidth = .5, alpha = .5)+
      #geom_errorbar(aes(x = as.numeric(factor(country_name)),ymin = q2_bin_mean, ymax = q2,width = width),
      #              color = "#3498db",
      #              linewidth = .5, alpha = .5)+
      
      #geom_errorbar(aes(x = as.numeric(factor(country_name)),ymin = q2, ymax = q3_bin_mean,width = width),
      #              color = "#3498db",
      #              linewidth = .5, alpha = .5)+
      
      geom_errorbar(aes(x = as.numeric(factor(country_name)),ymin = top50_bin_mean, ymax = top25_bin_mean,width=width),
                    color = "#3498db",linewidth = .5, alpha = .5)
    
  }

  p <- p +
    labs(
      title = "Spillover returns",
      x = "Country",
      y = ylab,
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
    
    labs(caption = "© 2025 Innovation Strategy Explorer") +
    theme(
      plot.caption = element_text(hjust = 1, size = 10, color = "gray")
    )
  
  
  #library(cowplot)
  
  
  
  #ggdraw(p) +
  #draw_text(paste0(as.character(innos)," Innovations"), x = 0.85, y = 0.95, size = 14)
  
}


