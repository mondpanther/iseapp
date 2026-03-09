#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @importFrom bslib bs_theme page_navbar
#' @importFrom pkgload pkg_name
#' @importFrom shiny h1 tabPanel tags
#'
#' @keywords internal
ui <- function(req) {
  bslib::page_navbar(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cerulean"
    ),
    header = list(
      
      shinyjs::useShinyjs(),
      prompter::use_prompt(),
      sever::useSever(),
      waiter::use_waiter(),
      waiter::waiter_show_on_load(html = landing_ui_content()),
      # waiter::autoWaiter(color = "#ffffff", fadeout = 10),

      shiny::tags$head(
        shiny::tags$link(rel = "icon", type = "image/svg+xml", href = file.path(pkgload::pkg_name(), "img/favicon.svg")),
        shiny::tags$link(href = file.path(pkgload::pkg_name(), "css/custom-styles.min.css"), rel = "stylesheet", type = "text/css"),
        shiny::tags$script(src = file.path(pkgload::pkg_name(), "js/custom-js.js")),
        # Google Analytics 4
        shiny::tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-YY70D2F685"),
        shiny::tags$script(shiny::HTML("
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'G-YY70D2F685');
        "))
      )#,

    ),
    # footer = shiny::tags$footer(
    #   id = "zrsa-footer",
    #   "Developed by ZevRoss Spatial Analysis, LLC"
    # ),
    collapsible = TRUE,
    window_title = "Innovation Strategy Explorer",
    title = shiny::tags$div(
      class = "navbar-title-container",
      shiny::tags$div(
        style = "
          width: 40px;
          height: 40px;
          background-color: #2780e3;
          border-radius: 4px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-right: 10px;
        ",
        shiny::tags$span(
          style = "color: white; font-size: 20px; font-weight: bold;",
          "ISE"  # Innovation Strategy
        )
      ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/prinz_logo.png"),
      #   class = "navbar-logo"
      # ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/zrsa_logo.svg"),
      #   class = "navbar-logo"
      # ),
      # shiny::tags$img(
      #   src = file.path(pkgload::pkg_name(), "img/ifc_logo.svg"),
      #   class = "navbar-logo"
      # ),
      shiny::tags$div(
        class = "navbar-separator"
      ),
      shiny::tags$h1(
        "Innovation Strategy Explorer",
        class = "navbar-title-text"
      )
    ),
    id = "navbar_page",

    bslib::nav_spacer(),

    shiny::tabPanel(
      "Country Explorer",
      country_module_ui("country")
    ),
    # shiny::tabPanel(
    #   "Country Explorer (R)",
    #   country2_module_ui("country2")
    # ),
    shiny::tabPanel(
      "Region Explorer",
      region_module_ui("region")
    ),
    shiny::tabPanel(
      "Globe",
      globe_module_ui("globe")
    ),
    shiny::tabPanel(
      "About",
      shiny::div(
        style = "max-width: 800px; margin: 0 auto; padding: 30px 20px;",
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
          shiny::tags$li(
            shiny::tags$a(
              "Pathways to a productive and inclusive net zero",
              href = "https://cep.lse.ac.uk/_NEW/PUBLICATIONS/abstract.asp?index=12029",
              target = "_blank"
            )
          ),
          shiny::tags$li(
            shiny::tags$a(
              "Innovation in Green Technologies",
              href = "https://www.ifc.org/en/insights-reports/2025/innovation-in-green-technologies",
              target = "_blank"
            )
          ),
          shiny::tags$li(
            shiny::tags$a(
              "The green industrial policy matrix: Informing an industrial strategy for clean energy technologies",
              href = "https://cep.lse.ac.uk/_NEW/PUBLICATIONS/abstract.asp?index=11272",
              target = "_blank"
            )
          ),
          shiny::tags$li(
            shiny::tags$a(
              "The UK must recognise its enduring economic strengths",
              href = "https://blogs.lse.ac.uk/businessreview/2022/08/25/the-uk-must-recognise-its-enduring-economic-strengths/",
              target = "_blank"
            )
          )
        ),
        shiny::h2("Project Context"),
        shiny::p(
          "Built as part of the ",
          shiny::tags$a(
            "PRINZ project",
            href = "https://www.prinzproject.io/",
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
            href = "https://github.com/mondpanther/iseapp",
            target = "_blank"
          )
        )
      )
    )
  )
}
