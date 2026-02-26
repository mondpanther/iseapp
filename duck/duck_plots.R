# duck_plots.R
# Clean plotting functions for the DuckDB-backed ISE app.
#
# Each function accepts ONLY pre-computed data from SQL (no raw pdata/classes).
# Functions handle only visualization: bar width calc, ggplot/girafe construction,
# tooltip formatting, coord_flip. All data processing happens in duck_queries.R.
#
# Returns the same list(girafe, ggplot, plot_data) structure the app expects.

library(ggplot2)
library(ggiraph)
library(scales)

# ============================================================================
# Custom color palette (shared across all plots)
# ============================================================================

custom_colors <- c(
  "green"        = "forestgreen",
  "battery"      = "gold",
  "other"        = "gray70",

  "hard to abate" = "blue",
  "AI"           = "orange",
  "CPC Sections" = "purple",
  "agrifood"     = "burlywood"
)


# ============================================================================
# duck_plot_tech_bars: Bar chart of returns by technology
# Based on plot_avstrax_by_country() from istraxfunctions.R
# ============================================================================

#' Plot average returns by technology category (horizontal bar chart)
#'
#' @param data Pre-computed dataframe from duck_compute_avstrax().
#'   Must contain: technology, mean, innos, sem, q1-q3, top25/top50_bin_mean,
#'   top3_ids, top3_ids_url, greenclass
#' @param toflow Flow type string (for axis label detection)
#' @param custom_colors Named color vector for greenclass categories
#' @param bwidthscale "log" or "proportional" for bar width scaling
#' @param display_mode "confidence" or "quartiles"
#' @param show_top3_ids Logical; if TRUE, bars are interactive with tooltips/onclick
#' @param width_svg SVG width in inches
#' @param height_svg SVG height in inches
#' @param plot_title Title for the plot
#' @return list(girafe, ggplot, plot_data)
duck_plot_tech_bars <- function(data,
                                 toflow,
                                 custom_colors = custom_colors,
                                 bwidthscale = "log",
                                 display_mode = "confidence",
                                 show_top3_ids = FALSE,
                                 width_svg = 10,
                                 height_svg = 6,
                                 plot_title = "Spillover returns") {

  ylab <- ifelse(grepl("strax", toflow), "Return in %", "Millions of $")
  is_return <- grepl("strax", toflow)

  # Extract "All" stats
  allmean <- data$mean[data$technology == "All"]
  total_innos <- data$innos[data$technology == "All"]
  if (length(allmean) == 0) allmean <- 0
  if (length(total_innos) == 0) total_innos <- 0

  # Filter to technologies with > 1 innovation
  avstrax <- data[data$innos > 1, ]

  if (nrow(avstrax) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  avstrax <- avstrax[order(avstrax$technology), ]

  # Compute bar widths
  if (bwidthscale == "log") {
    avstrax$linnos <- log(1 + avstrax$innos)
  } else {
    avstrax$linnos <- avstrax$innos
  }
  avstrax$width <- avstrax$linnos / max(avstrax$linnos)
  avstrax$x_pos <- as.numeric(factor(avstrax$technology))
  avstrax$xmin <- avstrax$x_pos - avstrax$width / 2
  avstrax$xmax <- avstrax$x_pos + avstrax$width / 2
  avstrax$ymin <- 0
  avstrax$ymax <- avstrax$mean

  # Format value labels
  if (is_return) {
    avstrax$value_label <- paste0(round(avstrax$mean, 1), "%")
  } else {
    avstrax$value_label <- paste0("$", round(avstrax$mean, 1), " million")
  }

  # Build plot
  if (show_top3_ids) {
    p <- ggplot(avstrax) +
      geom_rect_interactive(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                 fill = greenclass, data_id = technology,
                                 tooltip = paste0(technology, ": ", value_label,
                                                  "\nInnovations: ", scales::comma(innos),
                                                  "\nTop IDs: ", top3_ids),
                                 onclick = top3_ids_url))
  } else {
    p <- ggplot(avstrax) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = greenclass))
  }

  # Error bars / quartile bands
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(aes(x = x_pos,
                               ymin = ifelse(mean - 1.96 * sem > 0, mean - 1.96 * sem, 0),
                               ymax = mean + 1.96 * sem),
                           width = 0.2, color = "black", linewidth = .4, alpha = .4)
  } else if (display_mode == "quartiles") {
    p <- p + geom_errorbar(aes(color = greenclass, x = x_pos,
                               ymin = top50_bin_mean, ymax = top25_bin_mean,
                               width = width * 1.05),
                           linewidth = 1, alpha = .5)
  }

  p <- p +
    scale_x_continuous(breaks = avstrax$x_pos, labels = avstrax$technology) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(title = plot_title, x = "Technology", y = ylab, fill = "Technology") +
    guides(color = "none") +
    theme_minimal(base_family = "Open Sans") +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      text = element_text(family = "Open Sans"),
      axis.text = element_text(family = "Open Sans"),
      axis.title = element_text(family = "Open Sans")
    ) +
    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = allmean, x = max(avstrax$x_pos) + 0.4,
             label = "Average", angle = -90, vjust = 1.5, size = 4,
             family = "Open Sans", color = "black") +
    coord_flip() +
    labs(subtitle = paste0(as.character(total_innos), " Innovations"),
         caption = "\u00a9 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 14, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 10, color = "gray"))

  # Export data
  export_cols <- c("technology", "mean", "innos", "sem", "greenclass", "value_label")
  if ("top50_bin_mean" %in% names(avstrax)) export_cols <- c(export_cols, "top50_bin_mean", "top25_bin_mean")
  plot_data <- avstrax[, intersect(export_cols, names(avstrax)), drop = FALSE]

  list(
    girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                    options = list(
                      opts_sizing(rescale = TRUE, width = 1),
                      opts_hover(css = "cursor:pointer;fill:yellow;"),
                      opts_selection(type = "none"),
                      opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;")
                    )),
    ggplot = p,
    plot_data = plot_data
  )
}


# ============================================================================
# duck_plot_country_bars: Bar chart of returns by country/region
# Based on plot_avstrax_by_technology() from istraxfunctions.R
# ============================================================================

#' Plot average returns by country/region (horizontal bar chart)
#'
#' @param data Pre-computed dataframe from duck_compute_avstrax_for_techs().
#'   Must contain: ctry_code, country_name, mean, innos, sem, top3_ids, top3_ids_url
#' @param toflow Flow type string
#' @param custom_colors Named color vector (not used for fill, but kept for consistency)
#' @param topn Number of top countries to show
#' @param mininno Minimum innovation count threshold
#' @param bwidthscale "log" or "proportional"
#' @param display_mode "confidence" or "quartiles"
#' @param show_top3_ids Logical; if TRUE, bars are interactive
#' @param width_svg SVG width in inches
#' @param height_svg SVG height in inches
#' @param plot_title Title for the plot
#' @param x_label Label for x-axis ("Country" or "Region")
#' @return list(girafe, ggplot, plot_data)
duck_plot_country_bars <- function(data,
                                    toflow,
                                    custom_colors = custom_colors,
                                    topn = 20,
                                    mininno = 5,
                                    bwidthscale = "log",
                                    display_mode = "confidence",
                                    show_top3_ids = FALSE,
                                    width_svg = 10,
                                    height_svg = 6,
                                    plot_title = "Spillover returns",
                                    x_label = "Country") {

  ylab <- ifelse(grepl("strax", toflow), "Return in %", "Millions of $")
  is_return <- grepl("strax", toflow)

  # Extract "All" stats
  allmean <- data$mean[data$ctry_code == "All"]
  innos <- data$innos[data$ctry_code == "All"]
  if (length(allmean) == 0) allmean <- 0
  if (length(innos) == 0) innos <- 0

  # Filter, sort, and limit
  avstrax <- data[data$ctry_code != "All" & data$innos >= mininno, ]
  avstrax <- avstrax[order(-avstrax$mean), ]
  avstrax <- head(avstrax, topn)

  if (nrow(avstrax) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  # Set factor levels for ordering
  country_order <- as.character(avstrax$country_name[order(avstrax$mean)])
  avstrax$country_name <- factor(as.character(avstrax$country_name), levels = country_order)
  avstrax$x_pos <- as.numeric(avstrax$country_name)

  # Bar widths
  if (bwidthscale == "log") {
    avstrax$width_raw <- log(1 + avstrax$innos)
  } else {
    avstrax$width_raw <- avstrax$innos
  }
  avstrax$width <- avstrax$width_raw / max(avstrax$width_raw)

  scale_factor <- 0.7
  avstrax$width_scaled <- avstrax$width * scale_factor
  avstrax$xmin <- avstrax$x_pos - avstrax$width_scaled / 2
  avstrax$xmax <- avstrax$x_pos + avstrax$width_scaled / 2
  avstrax$ymin <- 0
  avstrax$ymax <- avstrax$mean

  # Value labels
  if (is_return) {
    avstrax$value_label <- paste0(round(avstrax$mean, 1), "%")
  } else {
    avstrax$value_label <- paste0("$", round(avstrax$mean, 1), " million")
  }

  main_color <- "#3498db"
  avstrax$group <- "Main"

  if (show_top3_ids) {
    p <- ggplot() +
      geom_rect_interactive(data = avstrax,
                            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                fill = group, data_id = country_name,
                                tooltip = paste0(country_name, ": ", value_label,
                                                 "\nInnovations: ", scales::comma(innos),
                                                 "\nTop IDs: ", top3_ids),
                                onclick = top3_ids_url))
  } else {
    p <- ggplot() +
      geom_rect(data = avstrax,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group))
  }

  p <- p + scale_fill_manual(values = c("Main" = main_color), guide = "none")

  # Error bars
  if (display_mode == "confidence") {
    p <- p + geom_errorbar(data = avstrax,
                           aes(x = (xmin + xmax) / 2,
                               ymin = ifelse(mean - 1.96 * sem > 0, mean - 1.96 * sem, 0),
                               ymax = mean + 1.96 * sem),
                           width = 0.15, color = "black", linewidth = .4, alpha = .4)
  } else if (display_mode == "quartiles") {
    p <- p + geom_errorbar(data = avstrax,
                           aes(x = (xmin + xmax) / 2,
                               ymin = top50_bin_mean, ymax = top25_bin_mean,
                               width = width_scaled),
                           color = main_color, linewidth = .5, alpha = .5)
  }

  p <- p +
    scale_x_continuous(breaks = avstrax$x_pos, labels = avstrax$country_name) +
    labs(title = plot_title, x = x_label, y = ylab) +
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
    geom_hline(yintercept = allmean, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = allmean, x = max(avstrax$x_pos) - 0.6,
             label = "Average", angle = -90, vjust = 1.5, size = 4,
             family = "Open Sans", color = "black") +
    coord_flip() +
    labs(subtitle = paste0(as.character(innos), " Innovations"),
         caption = "\u00a9 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 11, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 10, color = "gray"))

  # Export data
  export_cols <- c("ctry_code", "country_name", "mean", "innos", "sem", "group", "value_label")
  if ("top50_bin_mean" %in% names(avstrax)) export_cols <- c(export_cols, "top50_bin_mean", "top25_bin_mean")
  plot_data <- avstrax[, intersect(export_cols, names(avstrax)), drop = FALSE]

  list(
    girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                    options = list(
                      opts_sizing(rescale = TRUE, width = 1),
                      opts_hover(css = "cursor:pointer;fill:yellow;"),
                      opts_selection(type = "none"),
                      opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;")
                    )),
    ggplot = p,
    plot_data = plot_data
  )
}


# ============================================================================
# duck_plot_rta_bars: RTA bar chart by country/region
# Based on plot_avstrax_rta() from istraxfunctions.R
# ============================================================================

#' Plot RTA (Revealed Technological Advantage) by country/region
#'
#' @param data Pre-computed dataframe from duck_compute_avstrax_for_techs().
#'   Must contain: ctry_code, country_name, RTA, innos, Allinnos, top3_ids, top3_ids_url
#' @param toflow Flow type string
#' @param topn Number of top countries (highest RTA)
#' @param bottomn Number of bottom countries (lowest RTA)
#' @param mininno Minimum innovation count
#' @param minallinnos Minimum all-innovation count
#' @param bwidthscale "log" or "proportional"
#' @param show_top3_ids Logical
#' @param width_svg, height_svg SVG dimensions
#' @param plot_title Title
#' @param x_label "Country" or "Region"
#' @return list(girafe, ggplot, plot_data)
duck_plot_rta_bars <- function(data,
                                toflow,
                                topn = 20,
                                bottomn = 0,
                                mininno = 5,
                                minallinnos = 0,
                                bwidthscale = "log",
                                show_top3_ids = FALSE,
                                width_svg = 10,
                                height_svg = 6,
                                plot_title = "Revealed Technological Advantage (RTA)",
                                x_label = "Country") {

  innos <- data$innos[data$ctry_code == "All"]
  if (length(innos) == 0) innos <- 0

  # Filter
  avstrax_filtered <- data[data$ctry_code != "All" & data$innos >= mininno & !is.na(data$RTA), ]
  if (minallinnos > 0 && "Allinnos" %in% names(avstrax_filtered)) {
    avstrax_filtered <- avstrax_filtered[!is.na(avstrax_filtered$Allinnos) & avstrax_filtered$Allinnos >= minallinnos, ]
  }

  # Top n and bottom n
  top_countries <- head(avstrax_filtered[order(-avstrax_filtered$RTA), ], topn)
  if (bottomn > 0) {
    bottom_countries <- head(avstrax_filtered[order(avstrax_filtered$RTA), ], bottomn)
    avstrax <- unique(rbind(top_countries, bottom_countries))
    avstrax <- avstrax[order(-avstrax$RTA), ]
  } else {
    avstrax <- top_countries
  }

  if (nrow(avstrax) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  # Factor levels for ordering
  country_order <- as.character(avstrax$country_name[order(avstrax$RTA)])
  avstrax$country_name <- factor(as.character(avstrax$country_name), levels = country_order)
  avstrax$x_pos <- as.numeric(avstrax$country_name)

  # Bar widths
  if (bwidthscale == "log") {
    avstrax$width_raw <- log(1 + avstrax$innos)
  } else {
    avstrax$width_raw <- avstrax$innos
  }
  avstrax$width <- avstrax$width_raw / max(avstrax$width_raw)
  scale_factor <- 0.7
  avstrax$width_scaled <- avstrax$width * scale_factor
  avstrax$xmin <- avstrax$x_pos - avstrax$width_scaled / 2
  avstrax$xmax <- avstrax$x_pos + avstrax$width_scaled / 2
  avstrax$ymin <- 0
  avstrax$ymax <- avstrax$RTA

  avstrax$value_label <- paste0("RTA: ", round(avstrax$RTA, 2))
  main_color <- "#e74c3c"

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
    labs(title = plot_title, x = x_label, y = "RTA Index") +
    theme_minimal(base_family = "Open Sans") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      text = element_text(family = "Open Sans"),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", y = 1.0, x = max(avstrax$x_pos) - 0.6,
             label = "RTA = 1", angle = -90, vjust = 1.5, size = 3.5,
             family = "Open Sans", color = "black") +
    coord_flip() +
    labs(subtitle = paste0(as.character(innos), " Innovations | RTA > 1 = Specialization advantage"),
         caption = "\u00a9 2025 Innovation Strategy Explorer") +
    theme(plot.subtitle = element_text(size = 11, hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 9, color = "gray"))

  export_cols <- c("ctry_code", "country_name", "RTA", "innos")
  if ("Allinnos" %in% names(avstrax)) export_cols <- c(export_cols, "Allinnos")
  plot_data <- avstrax[, intersect(export_cols, names(avstrax)), drop = FALSE]

  list(
    girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                    options = list(
                      opts_sizing(rescale = TRUE, width = 1),
                      opts_hover(css = "cursor:pointer;fill:yellow;"),
                      opts_selection(type = "none"),
                      opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;")
                    )),
    ggplot = p,
    plot_data = plot_data
  )
}


# ============================================================================
# duck_plot_rta_scatter: RTA vs Returns scatter plot
# Based on plot_rta_returns_scatter() from istraxfunctions.R
# ============================================================================

#' Plot RTA vs Returns scatter
#'
#' @param data Pre-computed dataframe from duck_compute_avstrax_for_techs().
#'   Must contain: ctry_code, country_name, RTA, mean, innos, Allinnos
#' @param mininno, minallinnos Minimum thresholds
#' @param bwidthscale "log" or "proportional" for dot size
#' @param width_svg, height_svg SVG dimensions
#' @param plot_title, x_label, y_label Plot labels
#' @return list(girafe, ggplot, plot_data)
duck_plot_rta_scatter <- function(data,
                                   mininno = 5,
                                   minallinnos = 0,
                                   bwidthscale = "log",
                                   width_svg = 8,
                                   height_svg = 6,
                                   plot_title = "RTA vs Returns",
                                   x_label = "RTA",
                                   y_label = "Return (%)") {

  if (!"RTA" %in% names(data) || !"mean" %in% names(data)) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "RTA or Returns data not available", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  # Average from "All" row
  all_mean <- data$mean[data$ctry_code == "All"]
  if (length(all_mean) == 0 || is.na(all_mean[1])) all_mean <- NULL else all_mean <- all_mean[1]

  # Filter
  scatter_data <- data[data$ctry_code != "All" & data$innos >= mininno &
                       !is.na(data$RTA) & !is.na(data$mean), ]
  if (minallinnos > 0 && "Allinnos" %in% names(scatter_data)) {
    scatter_data <- scatter_data[!is.na(scatter_data$Allinnos) & scatter_data$Allinnos >= minallinnos, ]
  }

  if (nrow(scatter_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  # Dot sizes
  if (bwidthscale == "log") {
    scatter_data$size_raw <- log(1 + scatter_data$innos)
  } else {
    scatter_data$size_raw <- scatter_data$innos
  }
  max_size <- max(scatter_data$size_raw, na.rm = TRUE)
  min_size <- min(scatter_data$size_raw, na.rm = TRUE)
  if (max_size > min_size) {
    scatter_data$size_scaled <- 3 + 12 * (scatter_data$size_raw - min_size) / (max_size - min_size)
  } else {
    scatter_data$size_scaled <- 7
  }

  scatter_data$tooltip_text <- paste0(
    "<b>", scatter_data$country_name, "</b><br>",
    "RTA: ", round(scatter_data$RTA, 2), "<br>",
    "Return: ", round(scatter_data$mean, 1), "%<br>",
    "Innovations: ", scales::comma(scatter_data$innos)
  )

  p <- ggplot(scatter_data, aes(x = RTA, y = mean)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#2ecc71", fill = "#2ecc71", alpha = 0.2, linewidth = 1) +
    geom_point_interactive(
      aes(size = size_scaled, fill = RTA, tooltip = tooltip_text, data_id = ctry_code),
      shape = 21, color = "white", stroke = 0.5, alpha = 0.8
    ) +
    scale_fill_gradient2(
      low = "#08306B", mid = "#FFFFFF", high = "#B2182B",
      midpoint = 1, name = "RTA"
    ) +
    scale_size_identity() +
    labs(title = plot_title, x = x_label, y = y_label) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      text = element_text(family = "Open Sans"),
      legend.position = "right",
      plot.title = element_text(size = 14, hjust = 0.5)
    ) +
    annotate("text", x = 1, y = max(scatter_data$mean, na.rm = TRUE) * 0.95,
             label = "RTA = 1", hjust = -0.1, size = 3.5,
             family = "Open Sans", color = "gray40")

  if (!is.null(all_mean)) {
    p <- p +
      geom_hline(yintercept = all_mean, linetype = "dashed", color = "#3498db", linewidth = 0.8) +
      annotate("text", x = max(scatter_data$RTA, na.rm = TRUE) * 0.95, y = all_mean,
               label = paste0("Avg: ", round(all_mean, 1), "%"), vjust = -0.5, size = 3.5,
               family = "Open Sans", color = "#3498db")
  }

  p <- p + labs(caption = "\u00a9 2025 Innovation Strategy Explorer") +
    theme(plot.caption = element_text(hjust = 1, size = 9, color = "gray"))

  export_cols <- c("ctry_code", "country_name", "RTA", "mean", "innos")
  if ("Allinnos" %in% names(scatter_data)) export_cols <- c(export_cols, "Allinnos")
  plot_data <- scatter_data[, intersect(export_cols, names(scatter_data)), drop = FALSE]

  list(
    girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                    options = list(
                      opts_sizing(rescale = TRUE, width = 1),
                      opts_hover(css = "cursor:pointer;opacity:1;"),
                      opts_selection(type = "none"),
                      opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;font-size:12px;")
                    )),
    ggplot = p,
    plot_data = plot_data
  )
}


# ============================================================================
# duck_plot_gdp_scatter: RTA vs GDP per Capita scatter plot
# Based on plot_rta_gdp_scatter() from istraxfunctions.R
# ============================================================================

#' Plot RTA vs GDP per Capita scatter
#'
#' @param data Pre-computed dataframe from duck_data_for_gdp_scatter().
#'   Must contain: ctry_code, country_name, RTA, innos, gdp_pc_2015, log_gdp_pc
#' @param bwidthscale "log" or "proportional"
#' @param width_svg, height_svg SVG dimensions
#' @param plot_title Title
#' @return list(girafe, ggplot, plot_data)
duck_plot_gdp_scatter <- function(data,
                                   bwidthscale = "log",
                                   width_svg = 8,
                                   height_svg = 6,
                                   plot_title = "RTA vs GDP per Capita") {

  if (!"RTA" %in% names(data) || !"gdp_pc_2015" %in% names(data)) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "RTA or GDP data not available", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  scatter_data <- data[!is.na(data$RTA) & !is.na(data$log_gdp_pc), ]

  if (nrow(scatter_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
      theme_void()
    return(list(
      girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                      options = list(opts_sizing(rescale = TRUE, width = 1))),
      ggplot = NULL, plot_data = NULL
    ))
  }

  # Dot sizes
  if (bwidthscale == "log") {
    scatter_data$size_raw <- log(1 + scatter_data$innos)
  } else {
    scatter_data$size_raw <- scatter_data$innos
  }
  max_size <- max(scatter_data$size_raw, na.rm = TRUE)
  min_size <- min(scatter_data$size_raw, na.rm = TRUE)
  if (max_size > min_size) {
    scatter_data$size_scaled <- 3 + 12 * (scatter_data$size_raw - min_size) / (max_size - min_size)
  } else {
    scatter_data$size_scaled <- 7
  }

  scatter_data$tooltip_text <- paste0(
    "<b>", scatter_data$country_name, "</b><br>",
    "RTA: ", round(scatter_data$RTA, 2), "<br>",
    "GDP/capita PPP (2015): $", scales::comma(round(scatter_data$gdp_pc_2015)), "<br>",
    "log(GDP/capita PPP): ", round(scatter_data$log_gdp_pc, 2), "<br>",
    "Innovations: ", scales::comma(scatter_data$innos)
  )

  p <- ggplot(scatter_data, aes(x = RTA, y = log_gdp_pc)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#2ecc71", fill = "#2ecc71", alpha = 0.2, linewidth = 1) +
    geom_point_interactive(
      aes(size = size_scaled, fill = RTA, tooltip = tooltip_text, data_id = ctry_code),
      shape = 21, color = "white", stroke = 0.5, alpha = 0.8
    ) +
    scale_fill_gradient2(
      low = "#08306B", mid = "#FFFFFF", high = "#B2182B",
      midpoint = 1, name = "RTA"
    ) +
    scale_size_identity() +
    labs(title = plot_title, x = "RTA", y = "GDP per capita (log)") +
    theme_minimal(base_family = "Open Sans") +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      text = element_text(family = "Open Sans"),
      legend.position = "right",
      plot.title = element_text(size = 14, hjust = 0.5)
    ) +
    annotate("text", x = 1, y = max(scatter_data$log_gdp_pc, na.rm = TRUE) * 0.98,
             label = "RTA = 1", hjust = -0.1, size = 3.5,
             family = "Open Sans", color = "gray40") +
    labs(caption = "GDP PPP data: World Bank (2015, constant 2017 intl $) | \u00a9 2025 Innovation Strategy Explorer") +
    theme(plot.caption = element_text(hjust = 1, size = 9, color = "gray"))

  export_cols <- c("ctry_code", "country_name", "RTA", "gdp_pc_2015", "log_gdp_pc", "innos")
  plot_data <- scatter_data[, intersect(export_cols, names(scatter_data)), drop = FALSE]

  list(
    girafe = girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg,
                    options = list(
                      opts_sizing(rescale = TRUE, width = 1),
                      opts_hover(css = "cursor:pointer;opacity:1;"),
                      opts_selection(type = "none"),
                      opts_tooltip(css = "background-color:white;padding:5px;border-radius:3px;border:1px solid #ccc;font-size:12px;")
                    )),
    ggplot = p,
    plot_data = plot_data
  )
}


# ============================================================================
# duck_plot_world_map: World choropleth (Plotly)
# Based on plot_world_map() from istraxfunctions.R
# ============================================================================

#' Plot world choropleth map
#'
#' @param data Pre-computed dataframe with ctry_code, country_name, innos, and value columns
#' @param value_col Column name for values (default "mean")
#' @param color_scale Color scale name
#' @param plot_title Title
#' @param is_return Logical, TRUE if values are percentages
#' @return A plotly object
duck_plot_world_map <- function(data,
                                 value_col = "mean",
                                 color_scale = "Viridis",
                                 plot_title = "Returns by Country",
                                 is_return = TRUE) {
  library(plotly)
  library(countrycode)

  # Filter out "All" and convert ISO2 to ISO3 (Plotly needs ISO3)
  map_data <- data[data$ctry_code != "All", ]
  map_data$iso3 <- countrycode(map_data$ctry_code, origin = "iso2c", destination = "iso3c", warn = FALSE)
  map_data$value <- map_data[[value_col]]
  map_data <- map_data[!is.na(map_data$iso3), ]

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

  is_rta <- value_col == "RTA"

  # Create hover text
  if (is_rta) {
    map_data$hover_text <- paste0(
      "<b>", map_data$country_name, "</b><br>",
      "RTA: ", round(map_data$value, 2), "<br>",
      "Innovations: ", scales::comma(map_data$innos)
    )
  } else if (is_return) {
    map_data$hover_text <- paste0(
      "<b>", map_data$country_name, "</b><br>",
      "Value: ", round(map_data$value, 1), "%<br>",
      "Innovations: ", scales::comma(map_data$innos)
    )
  } else {
    map_data$hover_text <- paste0(
      "<b>", map_data$country_name, "</b><br>",
      "Value: $", round(map_data$value, 2), "M<br>",
      "Innovations: ", scales::comma(map_data$innos)
    )
  }

  # Color settings
  if (is_rta) {
    colorbar_title <- "RTA"
    colorbar_suffix <- ""
    rta_min <- min(map_data$value, na.rm = TRUE)
    rta_max <- max(map_data$value, na.rm = TRUE)
    if (rta_max > rta_min) {
      mid_point <- (1.0 - rta_min) / (rta_max - rta_min)
      mid_point <- max(0.01, min(0.99, mid_point))
    } else {
      mid_point <- 0.5
    }
    use_colorscale <- list(
      list(0, "rgb(8, 48, 107)"),
      list(mid_point * 0.5, "rgb(66, 146, 198)"),
      list(mid_point, "rgb(255, 255, 255)"),
      list(mid_point + (1 - mid_point) * 0.5, "rgb(239, 138, 98)"),
      list(1, "rgb(178, 24, 43)")
    )
  } else if (is_return) {
    colorbar_title <- "Return (%)"
    colorbar_suffix <- "%"
    use_colorscale <- color_scale
  } else {
    colorbar_title <- "Value ($M)"
    colorbar_suffix <- ""
    use_colorscale <- color_scale
  }

  p <- plotly::plot_ly(
    data = map_data, type = "choropleth",
    locations = ~iso3, z = ~value, text = ~hover_text, hoverinfo = "text",
    colorscale = use_colorscale, reversescale = FALSE,
    marker = list(line = list(color = "white", width = 0.5)),
    colorbar = list(title = list(text = colorbar_title), ticksuffix = colorbar_suffix)
  ) %>%
    plotly::layout(
      title = list(text = plot_title, x = 0.5, font = list(size = 16)),
      geo = list(
        showframe = FALSE, showcoastlines = TRUE, coastlinecolor = "grey",
        projection = list(type = "natural earth"),
        showland = TRUE, landcolor = "lightgray",
        showocean = TRUE, oceancolor = "aliceblue",
        showcountries = TRUE, countrycolor = "white", countrywidth = 0.5
      ),
      margin = list(l = 0, r = 0, t = 50, b = 0)
    ) %>%
    plotly::config(
      scrollZoom = TRUE, displayModeBar = TRUE,
      modeBarButtonsToAdd = list("zoom2d", "pan2d", "resetScale2d")
    )

  p
}


# ============================================================================
# duck_plot_world_map_gg: Static ggplot world map (for PDF export)
# Based on plot_world_map_gg() from istraxfunctions.R
# ============================================================================

#' Create static ggplot world map for PDF/vector export
#'
#' @param data Pre-computed dataframe with ctry_code, country_name, value columns
#' @param value_col Column name for values (default "mean")
#' @param plot_title Title
#' @param is_return Logical
#' @return A ggplot2 object
duck_plot_world_map_gg <- function(data,
                                    value_col = "mean",
                                    plot_title = "Returns by Country",
                                    is_return = TRUE) {
  library(countrycode)

  tryCatch({
    map_data <- data[data$ctry_code != "All", ]
    map_data$map_value <- as.numeric(map_data[[value_col]])
    map_data <- map_data[!is.na(map_data$country_name) & !is.na(map_data$map_value), ]

    if (nrow(map_data) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available") + theme_void())
    }

    is_rta <- value_col == "RTA"

    world_poly <- ggplot2::map_data("world")

    map_data$region <- countrycode(map_data$ctry_code, origin = "iso2c", destination = "country.name",
                                    custom_match = c(
                                      "US" = "USA", "GB" = "UK", "RU" = "Russia",
                                      "KR" = "South Korea", "KP" = "North Korea",
                                      "CZ" = "Czech Republic", "SK" = "Slovakia",
                                      "TW" = "Taiwan", "CD" = "Democratic Republic of the Congo",
                                      "CG" = "Republic of Congo", "CI" = "Ivory Coast",
                                      "TT" = "Trinidad", "VN" = "Vietnam",
                                      "MM" = "Myanmar", "LA" = "Laos",
                                      "SY" = "Syria", "IR" = "Iran",
                                      "BO" = "Bolivia", "VE" = "Venezuela",
                                      "TZ" = "Tanzania", "MK" = "North Macedonia",
                                      "PS" = "Palestine", "BN" = "Brunei",
                                      "SZ" = "Eswatini", "VA" = "Vatican"
                                    ))

    world_merged <- merge(world_poly, map_data[, c("region", "map_value")],
                          by = "region", all.x = TRUE)

    if (is_rta) {
      max_dev <- max(abs(max(map_data$map_value, na.rm = TRUE) - 1),
                     abs(1 - min(map_data$map_value, na.rm = TRUE)))
      p <- ggplot(world_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = map_value), color = "white", linewidth = 0.15) +
        scale_fill_gradient2(low = "#08306B", mid = "#FFFFFF", high = "#B2182B",
                             midpoint = 1, na.value = "lightgray", name = "RTA",
                             limits = c(1 - max_dev, 1 + max_dev))
    } else {
      p <- ggplot(world_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = map_value), color = "white", linewidth = 0.15) +
        scale_fill_viridis_c(na.value = "lightgray",
                             name = if (is_return) "Return (%)" else "Value ($M)")
    }

    p <- p + coord_fixed(1.3) + labs(title = plot_title) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "right",
            panel.grid.major = element_line(color = "grey90"),
            axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())

    return(p)
  }, error = function(e) {
    message("duck_plot_world_map_gg error: ", e$message)
    return(NULL)
  })
}


# ============================================================================
# duck_plot_uk_regions_map: UK regions choropleth (Leaflet)
# Based on plot_uk_regions_map() from istraxfunctions.R
# ============================================================================

#' Plot UK regions choropleth map using leaflet
#'
#' @param data Pre-computed dataframe with ctry_code (NUTS1), country_name, innos, value columns
#' @param value_col Column name for values (default "mean")
#' @param plot_title Title
#' @param is_return Logical
#' @return A leaflet object
duck_plot_uk_regions_map <- function(data,
                                      value_col = "mean",
                                      plot_title = "Returns by UK Region",
                                      is_return = TRUE) {
  library(leaflet)
  library(sf)
  library(htmltools)

  uk_regions_names <- c(
    "UKC" = "North East England", "UKD" = "North West England",
    "UKE" = "Yorkshire and The Humber", "UKF" = "East Midlands",
    "UKG" = "West Midlands", "UKH" = "East of England",
    "UKI" = "London", "UKJ" = "South East England",
    "UKK" = "South West England", "UKL" = "Wales",
    "UKM" = "Scotland", "UKN" = "Northern Ireland"
  )

  # Load GeoJSON (cached in .GlobalEnv)
  if (!exists(".uk_nuts1_sf", envir = .GlobalEnv)) {
    uk_sf <- NULL
    local_geojson <- "uk_nuts1_boundaries.geojson"
    if (file.exists(local_geojson)) {
      tryCatch({
        uk_sf <- sf::st_read(local_geojson, quiet = TRUE)
        message("Loaded UK NUTS1 boundaries from local file")
      }, error = function(e) message("Could not load local GeoJSON: ", e$message))
    }
    if (is.null(uk_sf)) {
      geojson_url <- "https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/eurostat/ew/nuts1.json"
      tryCatch({
        uk_sf <- sf::st_read(geojson_url, quiet = TRUE)
        message("Loaded UK NUTS1 boundaries from GitHub")
      }, error = function(e) message("Could not load from GitHub: ", e$message))
    }
    if (!is.null(uk_sf)) {
      nuts_col <- names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]
      if (!is.null(nuts_col)) {
        existing_codes <- uk_sf[[nuts_col]]
        name_col <- names(uk_sf)[grepl("NUTS.*NM", names(uk_sf), ignore.case = TRUE)][1]
        gadm_file <- "gadm41_GBR_1.json"
        if (file.exists(gadm_file) && (!"UKM" %in% existing_codes || !"UKN" %in% existing_codes)) {
          tryCatch({
            gadm_sf <- sf::st_read(gadm_file, quiet = TRUE)
            if (!"UKM" %in% existing_codes) {
              scotland_sf <- gadm_sf[gadm_sf$NAME_1 == "Scotland", ]
              if (nrow(scotland_sf) > 0) {
                scotland_sf <- scotland_sf[, "geometry"]
                scotland_sf[[nuts_col]] <- "UKM"
                if (!is.null(name_col)) scotland_sf[[name_col]] <- "Scotland"
                uk_sf <- rbind(uk_sf, scotland_sf)
              }
            }
            if (!"UKN" %in% existing_codes) {
              ni_sf <- gadm_sf[gadm_sf$NAME_1 == "NorthernIreland", ]
              if (nrow(ni_sf) > 0) {
                ni_sf <- ni_sf[, "geometry"]
                ni_sf[[nuts_col]] <- "UKN"
                if (!is.null(name_col)) ni_sf[[name_col]] <- "Northern Ireland"
                uk_sf <- rbind(uk_sf, ni_sf)
              }
            }
          }, error = function(e) message("Could not load GADM file: ", e$message))
        }
        if (!is.null(nuts_col) && !is.na(nuts_col) && nuts_col != "NUTS1CD") {
          names(uk_sf)[names(uk_sf) == nuts_col] <- "NUTS1CD"
        }
      }
    }
    assign(".uk_nuts1_sf", uk_sf, envir = .GlobalEnv)
  }

  uk_sf <- get(".uk_nuts1_sf", envir = .GlobalEnv)

  if (is.null(uk_sf)) {
    return(leaflet() %>% addTiles() %>% setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
             addControl(html = "<div style='padding: 10px; background: white;'>Could not load UK region boundaries</div>",
                        position = "topright"))
  }
  if (!value_col %in% names(data)) {
    return(leaflet() %>% addTiles() %>% setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
             addControl(html = paste0("<div style='padding: 10px; background: white;'>Column '", value_col, "' not found</div>"),
                        position = "topright"))
  }

  map_data <- data[data$ctry_code != "All" & data$ctry_code %in% names(uk_regions_names), ]
  map_data$region_name <- uk_regions_names[map_data$ctry_code]
  map_data$value <- map_data[[value_col]]
  map_data <- map_data[!is.na(map_data$value), ]

  if (nrow(map_data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
             addControl(html = "<div style='padding: 10px; background: white;'>No valid data available for UK regions</div>",
                        position = "topright"))
  }

  is_rta <- value_col == "RTA"
  nuts_col <- if ("NUTS121CD" %in% names(uk_sf)) "NUTS121CD" else
              if ("NUTS1CD" %in% names(uk_sf)) "NUTS1CD" else
              names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]
  name_col <- if ("NUTS121NM" %in% names(uk_sf)) "NUTS121NM" else
              if ("NUTS1NM" %in% names(uk_sf)) "NUTS1NM" else
              names(uk_sf)[grepl("NUTS.*NM", names(uk_sf), ignore.case = TRUE)][1]

  if (is.null(nuts_col)) {
    return(leaflet() %>% addTiles() %>% setView(lng = -2.5, lat = 54.5, zoom = 5) %>%
             addControl(html = "<div style='padding: 10px; background: white;'>Could not identify NUTS code column</div>",
                        position = "topright"))
  }

  uk_sf$nuts_code <- uk_sf[[nuts_col]]
  uk_sf <- merge(uk_sf, map_data[, c("ctry_code", "region_name", "value", "innos")],
                 by.x = "nuts_code", by.y = "ctry_code", all.x = TRUE)

  # Color palette
  if (is_rta) {
    rta_colors <- c("#08306B", "#4292C6", "#FFFFFF", "#EF8A62", "#B2182B")
    max_deviation <- max(abs(max(map_data$value, na.rm = TRUE) - 1),
                         abs(1 - min(map_data$value, na.rm = TRUE)))
    pal <- colorNumeric(palette = rta_colors,
                        domain = c(1 - max_deviation, 1 + max_deviation),
                        na.color = "#E0E0E0")
  } else {
    pal <- colorNumeric(palette = "viridis", domain = map_data$value, na.color = "#E0E0E0")
  }

  # Labels
  uk_sf$display_name <- ifelse(!is.na(uk_sf$region_name), uk_sf$region_name,
                               ifelse(!is.null(name_col) & name_col %in% names(uk_sf),
                                      uk_sf[[name_col]], uk_sf$nuts_code))

  if (is_rta) {
    uk_sf$label_text <- ifelse(!is.na(uk_sf$value),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>RTA: ", round(uk_sf$value, 2),
                                      "<br/>Innovations: ", scales::comma(uk_sf$innos)),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>No data"))
  } else if (is_return) {
    uk_sf$label_text <- ifelse(!is.na(uk_sf$value),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>Value: ", round(uk_sf$value, 1), "%",
                                      "<br/>Innovations: ", scales::comma(uk_sf$innos)),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>No data"))
  } else {
    uk_sf$label_text <- ifelse(!is.na(uk_sf$value),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>Value: $", round(uk_sf$value, 2), "M",
                                      "<br/>Innovations: ", scales::comma(uk_sf$innos)),
                               paste0("<strong>", uk_sf$display_name, "</strong><br/>No data"))
  }

  map <- leaflet(uk_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(value), weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
      highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
      label = ~lapply(label_text, HTML),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "12px", direction = "auto")
    ) %>%
    setView(lng = -2.5, lat = 54.5, zoom = 5)

  # Legend
  if (nrow(map_data) > 0) {
    val_range <- range(map_data$value, na.rm = TRUE)
    if (is_rta) {
      max_deviation <- max(abs(val_range[2] - 1), abs(1 - val_range[1]))
      legend_values <- seq(1 + max_deviation, 1 - max_deviation, length.out = 5)
      map <- map %>% addLegend(position = "bottomright", colors = pal(legend_values),
                                labels = round(legend_values, 2), title = "RTA", opacity = 0.7)
    } else {
      map <- map %>% addLegend(position = "bottomright",
                                colors = pal(seq(val_range[2], val_range[1], length.out = 5)),
                                labels = round(seq(val_range[2], val_range[1], length.out = 5), 1),
                                title = ifelse(is_return, "Return (%)", "Value ($M)"), opacity = 0.7)
    }
  }

  map %>% addControl(
    html = paste0("<div style='padding: 6px 12px; background: white; border-radius: 4px; font-weight: bold; font-size: 14px;'>",
                  htmltools::htmlEscape(plot_title), "</div>"),
    position = "topright"
  )
}


# ============================================================================
# duck_plot_uk_regions_map_gg: Static ggplot UK regions map (for PDF export)
# Based on plot_uk_regions_map_gg() from istraxfunctions.R
# ============================================================================

#' Create static ggplot UK regions map for PDF/vector export
#'
#' @param data Pre-computed dataframe with ctry_code (NUTS1), value columns
#' @param value_col Column name (default "mean")
#' @param plot_title Title
#' @param is_return Logical
#' @return A ggplot2 object
duck_plot_uk_regions_map_gg <- function(data,
                                         value_col = "mean",
                                         plot_title = "Returns by UK Region",
                                         is_return = TRUE) {
  library(sf)

  tryCatch({
    uk_regions_names <- c(
      "UKC" = "North East England", "UKD" = "North West England",
      "UKE" = "Yorkshire and The Humber", "UKF" = "East Midlands",
      "UKG" = "West Midlands", "UKH" = "East of England",
      "UKI" = "London", "UKJ" = "South East England",
      "UKK" = "South West England", "UKL" = "Wales",
      "UKM" = "Scotland", "UKN" = "Northern Ireland"
    )

    if (!exists(".uk_nuts1_sf", envir = .GlobalEnv)) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Region boundaries not loaded") + theme_void())
    }
    uk_sf <- get(".uk_nuts1_sf", envir = .GlobalEnv)
    if (is.null(uk_sf)) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Region boundaries not available") + theme_void())
    }
    if (!value_col %in% names(data)) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = paste0("Column '", value_col, "' not found")) + theme_void())
    }

    map_data <- data[data$ctry_code != "All" & data$ctry_code %in% names(uk_regions_names), ]
    map_data$region_name <- uk_regions_names[map_data$ctry_code]
    map_data$map_value <- as.numeric(map_data[[value_col]])
    map_data <- map_data[!is.na(map_data$map_value), ]

    if (nrow(map_data) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available for UK regions") + theme_void())
    }

    is_rta <- value_col == "RTA"
    nuts_col <- if ("NUTS121CD" %in% names(uk_sf)) "NUTS121CD" else
                if ("NUTS1CD" %in% names(uk_sf)) "NUTS1CD" else
                names(uk_sf)[grepl("NUTS.*CD", names(uk_sf), ignore.case = TRUE)][1]

    if (is.null(nuts_col)) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Could not identify NUTS code column") + theme_void())
    }

    uk_sf$nuts_code <- uk_sf[[nuts_col]]
    uk_sf$feature_id <- seq_len(nrow(uk_sf))

    uk_coords <- do.call(rbind, lapply(seq_len(nrow(uk_sf)), function(i) {
      geom <- sf::st_geometry(uk_sf[i, ])[[1]]
      nuts <- uk_sf$nuts_code[i]
      fid <- uk_sf$feature_id[i]
      if (inherits(geom, "MULTIPOLYGON")) polys <- geom else polys <- list(geom)
      do.call(rbind, lapply(seq_along(polys), function(j) {
        ring <- polys[[j]]
        if (is.list(ring)) ring <- ring[[1]]
        coords <- as.data.frame(ring)
        names(coords) <- c("long", "lat")
        coords$nuts_code <- nuts
        coords$group <- paste0(fid, ".", j)
        coords$order <- seq_len(nrow(coords))
        coords
      }))
    }))

    uk_coords <- merge(uk_coords, map_data[, c("ctry_code", "region_name", "map_value")],
                        by.x = "nuts_code", by.y = "ctry_code", all.x = TRUE)

    if (is_rta) {
      max_dev <- max(abs(max(map_data$map_value, na.rm = TRUE) - 1),
                     abs(1 - min(map_data$map_value, na.rm = TRUE)))
      p <- ggplot(uk_coords, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = map_value), color = "white", linewidth = 0.3) +
        scale_fill_gradient2(low = "#08306B", mid = "#FFFFFF", high = "#B2182B",
                             midpoint = 1, na.value = "#E0E0E0", name = "RTA",
                             limits = c(1 - max_dev, 1 + max_dev))
    } else {
      p <- ggplot(uk_coords, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = map_value), color = "white", linewidth = 0.3) +
        scale_fill_viridis_c(na.value = "#E0E0E0",
                             name = if (is_return) "Return (%)" else "Value ($M)")
    }

    p + coord_fixed(1.3) + labs(title = plot_title) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "right",
            panel.grid.major = element_line(color = "grey90"),
            axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())

  }, error = function(e) {
    message("duck_plot_uk_regions_map_gg error: ", e$message)
    return(NULL)
  })
}
