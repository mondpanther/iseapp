#' About page module (UI + server).
#'
#' Renders the static "About" content plus a "Technology Definitions" section
#' that shows, for every technology label in the UI tree, a word cloud of the
#' CPC subclasses mapped to it. Each word is sized by the number of distinct
#' docdb_family_ids mapped to that (technology, subclass) pair in techmap.
#'
#' Inputs (via sysdata):
#'   * grouped_techs        — named list of technology labels per UI section.
#'   * tech_subclass_counts — data.table with columns technology, subclass,
#'                            title_short, n_docdb. Precomputed in
#'                            data-raw/02-build-app-sysdata.R.

#' @keywords internal
about_module_ui <- function(id) {
  ns <- shiny::NS(id)

  # Flatten grouped_techs to a picker input; skip UI-only placeholders and
  # technology labels that don't actually have any subclass mappings.
  tsc <- if (exists("tech_subclass_counts")) tech_subclass_counts else NULL
  mapped_techs <- if (is.null(tsc)) character() else
                    unique(as.character(tsc$technology))

  tech_selector_choices <- lapply(grouped_techs, function(entries) {
    labels <- names(entries)
    labels <- setdiff(labels, c("All categories", "All innovations"))
    labels <- intersect(labels, mapped_techs)
    if (!length(labels)) return(NULL)
    as.list(stats::setNames(labels, labels))
  })
  tech_selector_choices <- tech_selector_choices[!vapply(tech_selector_choices,
                                                         is.null, logical(1))]

  tech_def_block <- if (!length(tech_selector_choices)) {
    shiny::tagList(
      shiny::h2("Technology Definitions"),
      shiny::p(shiny::em(
        "Technology definitions unavailable (tech_subclass_counts not found ",
        "in sysdata \u2014 rerun data-raw/02-build-app-sysdata.R)."
      ))
    )
  } else {
    shiny::tagList(
      shiny::h2("Technology Definitions"),
      shiny::p(
        "Each technology label in the ISE Explorer maps to one or more CPC ",
        "subclasses. The word cloud below shows, for the selected technology, ",
        "the subclasses assigned to it \u2014 sized by the number of patent ",
        "families (docdbs) in the app's universe that carry at least one CPC ",
        "code in that subclass. Hover any word to see the exact family count; ",
        shiny::tags$strong("click a word to open its Espacenet CPC entry"),
        " in a new tab. The selector groups technologies exactly as in the ",
        "main 'Technologies Included' input."
      ),
      shinyWidgets::pickerInput(
        inputId  = ns("tech_pick"),
        label    = "Choose a technology:",
        choices  = tech_selector_choices,
        selected = tech_selector_choices[[1]][[1]],
        options  = list(`live-search` = TRUE, size = 15),
        width    = "100%"
      ),
      shiny::tags$style(shiny::HTML("
        .cpc-cloud {
          border: 1px solid #eee;
          border-radius: 6px;
          padding: 18px;
          min-height: 380px;
          display: flex;
          flex-wrap: wrap;
          gap: 8px 14px;
          align-content: flex-start;
          justify-content: center;
          line-height: 1.1;
          background: #fafafa;
        }
        .cpc-cloud a {
          color: #2b5a8c;
          text-decoration: none;
          padding: 2px 4px;
          border-radius: 3px;
          white-space: nowrap;
          transition: background 0.1s;
        }
        .cpc-cloud a:hover {
          background: #eef3f9;
          text-decoration: underline;
        }
      ")),
      shiny::uiOutput(ns("cloud")),
      shiny::uiOutput(ns("cloud_caption"))
    )
  }

  shiny::div(
    style = "max-width: 1000px; margin: 0 auto; padding: 30px 20px;",
    shiny::h1("Innovation Strategy Explorer"),
    shiny::p(
      "The Innovation Strategy Explorer is an interactive analytical tool designed to ",
      "measure and visualise innovation capabilities across technology types, countries, and UK regions."
    ),
    shiny::h2("Key Metrics"),
    shiny::tags$ul(
      shiny::tags$li(
        shiny::tags$strong("Revealed Technological Advantage (RTA)"),
        " \u2013 A symmetric measure ranging from 0\u20132, where values above 1 signal ",
        "comparative advantage in specific technology areas."
      ),
      shiny::tags$li(
        shiny::tags$strong("Average Spillovers"),
        " \u2013 Quantifies indirect and direct knowledge spillover value, ",
        "measured in millions of dollars."
      ),
      shiny::tags$li(
        shiny::tags$strong("Average Returns"),
        " \u2013 Estimates combined returns from R&D investments, encompassing both ",
        "private innovator returns and broader knowledge spillover benefits, expressed as percentages."
      ),
      shiny::tags$li(
        shiny::tags$strong("Marginal Returns"),
        " \u2013 Projects potential returns from incremental investment in specific ",
        "innovation domains, also in percentage terms."
      )
    ),
    shiny::h2("Academic Foundation"),
    shiny::p(
      "This builds on the paper ",
      shiny::tags$a(
        shiny::tags$em("Efficient industrial policy for innovation: Standing on the shoulders of hidden giants"),
        href = "https://cep.lse.ac.uk/_NEW/publications/abstract.asp?index=8614",
        target = "_blank"
      ),
      " by Guillard et al (2021)."
    ),
    shiny::h2("Policy Applications"),
    shiny::p("The indicators developed here have been used in a series of policy reports, including:"),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$a(
        "Pathways to a productive and inclusive net zero",
        href = "https://cep.lse.ac.uk/_NEW/PUBLICATIONS/abstract.asp?index=12029",
        target = "_blank"
      )),
      shiny::tags$li(shiny::tags$a(
        "Innovation in Green Technologies",
        href = "https://www.ifc.org/en/insights-reports/2025/innovation-in-green-technologies",
        target = "_blank"
      )),
      shiny::tags$li(shiny::tags$a(
        "The green industrial policy matrix: Informing an industrial strategy for clean energy technologies",
        href = "https://cep.lse.ac.uk/_NEW/PUBLICATIONS/abstract.asp?index=11272",
        target = "_blank"
      )),
      shiny::tags$li(shiny::tags$a(
        "The UK must recognise its enduring economic strengths",
        href = "https://blogs.lse.ac.uk/businessreview/2022/08/25/the-uk-must-recognise-its-enduring-economic-strengths/",
        target = "_blank"
      ))
    ),
    tech_def_block,
    shiny::h2("Project Context"),
    shiny::p(
      "Built as part of the ",
      shiny::tags$a(
        "PRINZ project",
        href   = "https://www.prinzproject.io/",
        target = "_blank"
      ),
      ", this tool translates academic research on innovation economics into ",
      "accessible, decision-supporting visualisations for policymakers and analysts."
    ),
    shiny::hr(),
    shiny::p(
      style = "color: #888; font-size: 0.9em;",
      "Source code: ",
      shiny::tags$a(
        "github.com/mondpanther/iseapp",
        href   = "https://github.com/mondpanther/iseapp",
        target = "_blank"
      )
    ),
    shiny::p(
      style = "color: #888; font-size: 0.85em;",
      "Version: ",
      shiny::tags$code(app_version())
    )
  )
}

#' @keywords internal
about_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    selected_rows <- shiny::reactive({
      shiny::req(input$tech_pick)
      if (!exists("tech_subclass_counts")) return(NULL)
      tsc <- tech_subclass_counts
      rows <- tsc[tsc$technology == input$tech_pick &
                    !is.na(tsc$n_docdb) & tsc$n_docdb > 0, , drop = FALSE]
      rows
    })

    output$cloud <- shiny::renderUI({
      rows <- selected_rows()
      shiny::req(!is.null(rows), nrow(rows) > 0)

      # Sort by count descending so biggest subclasses appear first.
      ord <- order(-rows$n_docdb)
      rows <- rows[ord, , drop = FALSE]

      # Size scale: map sqrt(n_docdb) linearly to a font-size range that
      # keeps both ends readable. Compressed enough that a 100x count
      # difference becomes ~3x font-size.
      counts <- as.numeric(rows$n_docdb)
      s <- sqrt(counts)
      lo <- min(s); hi <- max(s)
      # Guard against single-row (hi == lo)
      scaled <- if (hi == lo) rep(1, length(s)) else (s - lo) / (hi - lo)
      MIN_PT <- 11
      MAX_PT <- 32
      px <- round(MIN_PT + scaled * (MAX_PT - MIN_PT))

      # Colour ramp: low counts in mid-blue-grey, high counts in deeper blue.
      col_lo <- grDevices::col2rgb("#7fa0c0")
      col_hi <- grDevices::col2rgb("#143d6b")
      rgb_mix <- col_lo + scaled * (col_hi - col_lo)
      cols <- grDevices::rgb(rgb_mix[1, ], rgb_mix[2, ], rgb_mix[3, ],
                             maxColorValue = 255)

      anchors <- lapply(seq_along(counts), function(i) {
        code  <- rows$subclass[i]
        label <- sprintf("%s \u2013 %s", code, rows$title_short[i])
        tip   <- sprintf("%s \u2014 %s families",
                         label, format(counts[i], big.mark = ","))
        url   <- sprintf(
          "https://worldwide.espacenet.com/patent/cpc-browser#!/CPC=%s", code
        )
        shiny::tags$a(
          label,
          href   = url,
          target = "_blank",
          rel    = "noopener noreferrer",
          title  = tip,
          style  = sprintf("font-size: %dpx; color: %s;", px[i], cols[i])
        )
      })

      shiny::div(class = "cpc-cloud", anchors)
    })

    output$cloud_caption <- shiny::renderUI({
      rows <- selected_rows()
      if (is.null(rows) || nrow(rows) == 0) return(NULL)
      shiny::p(
        style = "color: #888; font-size: 0.85em; margin-top: 8px;",
        sprintf(
          "Showing %d CPC subclass%s mapped to '%s'. Total docdbs across shown subclasses: %s.",
          nrow(rows),
          if (nrow(rows) == 1) "" else "es",
          input$tech_pick,
          format(sum(rows$n_docdb), big.mark = ",")
        )
      )
    })
  })
}
