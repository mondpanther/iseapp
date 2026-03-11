#' Centralized theme for ISE app plot titles
#' 
#' Returns a ggplot2 theme element for consistent title styling
#' @param title_size Size for plot title (default 18)
#' @param subtitle_size Size for plot subtitle (default 14)
#' @param title_color Color for title text (default "#4b4b4b")
#' @return A ggplot2 theme object
#' @keywords internal
theme_istrax_titles <- function(title_size = 14, subtitle_size = 12, title_color = "#4b4b4b") {
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = title_size,
      color = title_color,
      face = "bold",
      hjust = 0.5,
      family = "Arial"
    ),
    plot.subtitle = ggplot2::element_text(
      size = subtitle_size,
      color = title_color,
      hjust = 0.5,
      family = "Arial"
    )
  )
}

#' Helper for plotly title styling
#' 
#' Returns a list of plotly title layout parameters
#' @param title_size Size for plot title (default 18)
#' @param title_color Color for title text (default "#4b4b4b")
#' @return A list with plotly title parameters
#' @keywords internal
plotly_title_style <- function(title_size = 24, title_color = "#4b4b4b") {
  list(
    font = list(
      size = title_size,
      color = title_color,
      family = "Arial",
      weight = 800
    ),
    xanchor = "center",
    x = 0.5
  )
}