#' Download button helpers and handlers for plot exports
#'
#' @description Provides UI helpers (download button pairs) and server-side
#' factory functions (SVG/PDF/CSV download handlers) for exporting plots
#' and their underlying data.


# ── UI helpers ────────────────────────────────────────────────────────────────

#' Create SVG + CSV download button pair for a girafe plot
#'
#' @param ns The module's namespace function
#' @param plot_id Character. The plot output ID (without namespace prefix)
#' @return A \code{tags$div} containing two download buttons
#' @export
plot_download_buttons <- function(ns, plot_id) {
  shiny::tags$div(
    class = "plot-download-buttons",
    shiny::downloadButton(ns(paste0("dl_svg_", plot_id)), "SVG",
                          class = "btn-sm btn-outline-secondary"),
    shiny::downloadButton(ns(paste0("dl_csv_", plot_id)), "Data (CSV)",
                          class = "btn-sm btn-outline-secondary")
  )
}

#' Create PDF + CSV download button pair for a map
#'
#' @param ns The module's namespace function
#' @param plot_id Character. The map output ID (without namespace prefix)
#' @return A \code{tags$div} containing two download buttons
#' @export
map_download_buttons <- function(ns, plot_id) {
  shiny::tags$div(
    class = "plot-download-buttons",
    shiny::downloadButton(ns(paste0("dl_pdf_", plot_id)), "PDF",
                          class = "btn-sm btn-outline-secondary"),
    shiny::downloadButton(ns(paste0("dl_csv_", plot_id)), "Data (CSV)",
                          class = "btn-sm btn-outline-secondary")
  )
}


# ── CSS for download buttons ─────────────────────────────────────────────────

#' Return the CSS for download buttons as a \code{tags$style} element
#' @export
download_buttons_css <- function() {
  shiny::tags$style(shiny::HTML("
    .plot-download-buttons {
      display: flex;
      gap: 6px;
      margin: 4px 0 8px 0;
      justify-content: flex-end;
    }
    .plot-download-buttons .btn {
      font-size: 12px;
      padding: 2px 10px;
    }
  "))
}


# ── Server-side download handler factories ────────────────────────────────────

#' Create a downloadHandler that exports a ggplot as SVG
#'
#' @param plot_reactive A reactive expression returning a ggplot object
#' @param filename_prefix Character. Prefix for the downloaded filename
#' @param width Numeric. SVG width in inches (default 10)
#' @param height Numeric. SVG height in inches (default 6)
#' @return A \code{downloadHandler}
#' @export
make_svg_handler <- function(plot_reactive, filename_prefix, width = 10, height = 6) {
  shiny::downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".svg"),
    content = function(file) {
      p <- plot_reactive()
      if (!is.null(p)) {
        ggplot2::ggsave(file, plot = p, device = "svg", width = width, height = height)
      }
    }
  )
}

#' Create a downloadHandler that exports a ggplot as PDF
#'
#' @param plot_reactive A reactive expression returning a ggplot object
#' @param filename_prefix Character. Prefix for the downloaded filename
#' @param width Numeric. PDF width in inches (default 12)
#' @param height Numeric. PDF height in inches (default 7)
#' @return A \code{downloadHandler}
#' @export
make_pdf_handler <- function(plot_reactive, filename_prefix, width = 12, height = 7) {
  shiny::downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      p <- plot_reactive()
      if (!is.null(p)) {
        tryCatch(
          ggplot2::ggsave(file, plot = p, device = "pdf", width = width, height = height),
          error = function(e) {
            message("PDF export error for ", filename_prefix, ": ", e$message)
          }
        )
      }
    }
  )
}

#' Create a downloadHandler that exports data as CSV
#'
#' @param data_reactive A reactive expression returning a data.frame
#' @param filename_prefix Character. Prefix for the downloaded filename
#' @return A \code{downloadHandler}
#' @export
make_csv_handler <- function(data_reactive, filename_prefix) {
  shiny::downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- data_reactive()
      if (!is.null(d)) {
        utils::write.csv(d, file, row.names = FALSE)
      }
    }
  )
}
