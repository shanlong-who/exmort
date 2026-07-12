# --- 1. Libraries (shiny is loaded by app.R) ---
# library(shiny)
# library(shinyjs)

# --- 2. Module UI ---
about_module_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = span(title = "About", id = ns("sWtitle")),
    value = "tab1",
    fluidRow(
      column(
        2,
        actionButton(ns("aboutButton"), "About the calculator", class = "btn active", width = "80%"), br(),
        actionButton(ns("citeButton"), "Citing the calculator", class = "btn", width = "80%"), br(),
        actionButton(ns("startButton"), "Get Started", class = "btn btn-primary")
      ),
      column(
        6,
        style = "padding: 0 30px 0 0;",
        div(
          id = ns("aboutbox"),
          p("Welcome to the", strong("WHO WPRO All Cause Mortality/Excess Mortality online-calculator"), "!"),
          p(
            "This calculator was initially developed by the",
            a(
              'World Health Organization, Western Pacific Region',
              href = 'https://www.who.int/westernpacific/',
              target = '_blank'
            ),
            "in collaboration with the",
            a(
              'Department of Statistics at UCLA',
              href = 'http://statistics.ucla.edu/',
              target = '_blank'
            ),
            "in 2022 to assess pandemic related mortality trends. Member States have seen great value in extending its use beyond the pandemic for routine mortality surveillance. In responding to this need, WHO WPRO has enhanced this tool and the current version (2025) is a substantially upgraded release, based on feedback from Member States and evolving needs." 
          ), 
          p("The current version now offers:"),
          tags$ul(
            tags$li("Flexible application beyond COVID-19: Users can define any event period of interest—such as epidemics, natural disasters, or major health interventions—to analyze mortality impacts."),
            tags$li("Multiple statistical models: In addition to Negative Binomial Regression and 5-year historical average, the tool now includes ARIMA, Poisson regression, and linear models to improve robustness."),
            tags$li("Subnational analysis: Users can upload data at provincial or district levels for localized assessments."),
            tags$li("Improved speed and performance through parallel computation and optimized R scripts."),
            tags$li("Interactive visualizations and customized report generation in PDF and HTML formats."),
            tags$li("User-led data processing: All calculations are done locally in the browser or offline environment. No data is shared with WHO unless users choose to do so, preserving privacy and data sovereignty.")
          ),
          p("This calculator aims to strengthen country capacity for multi-source surveillance, risk assessment, and policy dialogue. It can be used by technical staff in Ministries of Health, national statistics offices, and academic institutions."), 
          p("Please share your reports on bugs/comments/suggestions through our", a('GitHub site,',
                                                   href = 'https://github.com/WorldHealthOrganization/ACMCalculator',
                                                   target = '_blank'),
            "or by email to us (see", actionLink(ns("helpLink"), "Help"), "tab)."),
          p("This web application",
            "is written with the Shiny framework and development is via GitHub.  More information",
            "on Shiny and our GitHub repository can be found in the",
            "resource links on the right."), 
          p("The development of the updated ACM/EM Calculator was supported by a voluntary contribution from the Government of the Republic of Korea, through its Ministry of Health and Welfare.")
        ),
        div(
          id = ns("citebox"),
          tabsetPanel(
            tabPanel(
              "BibTeX",
              p(strong("ACMCalculator")),
              tags$pre(id = ns('scitation'), '@Manual{handcock:ACMCalculator,
                  title = {ACMCalculator: Software tools for the Statistical Analysis of Excess Mortality from All Cause Mortality Data
                  author = {Mark S. Handcock},
                  year = {2021},
                  address = {Los Angeles, CA},
                  url = {http://hpmrg.org/}
                }'),
              p(strong("ACMCalculator")),
              tags$pre(id = ns('swcitation'), "@Manual{beylerian:ACMCalculator,
                  title = {\\pkg{ACMCalculator}: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data},
                  author = {Mark S. Handcock},
                  year = {2021},
                  note = {\\proglang{R}~package version~0.1},
                  address = {Los Angeles, CA},
                  url = {https://cran.r-project.org/web/packages/WPROACM/}
                }")
            ),
            tabPanel(
              "Other",
              p(strong("ACMCalculator")),
              tags$pre("Mark S. Handcock (2021). ACMCalculator: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data. URL http://hpmrg.org"),
              p(strong("ACMCalculator")),
              tags$pre("Mark S. Handcock (2021).
                ACMCalculator: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data.")
            )
          ),
          p('If you use ACMCalculator, please cite it')
        )
      ),
      column(
        4,
        wellPanel(
          h5(tags$u('Resources')),
          div(a("The calculator on GitHub", href = "https://github.com/WorldHealthOrganization/ACMCalculator",
                target = "_blank")),
          div(a("WPRO all-cause mortality dashboard", href = "https://lynx.wpro.who.int/viz/allcausedeath.asp",
                target = "_blank")),
          div(a("Shiny: a web application framework for R", href = "http://shiny.rstudio.com/",
                target = "_blank"))
        )
      )
    )
  )
}

# --- 3. Module server ---
about_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Hide the citation panel on first render.
    observe({
      shinyjs::hide(id = "citebox")
    })

    # Reactive store for navigation requests fired from the landing page.
    nav_request <- reactiveVal(NULL)

    # Get Started button -> Data tab.
    observeEvent(input$startButton, {
      session$sendCustomMessage(type = "navigateTab", message = "Data")
    })

    # Get Started inline link -> Data tab.
    observeEvent(input$startButtonLink, {
      session$sendCustomMessage(type = "navigateTab", message = "Data")
    })

    # Help link -> Help and Resources tab.
    observeEvent(input$helpLink, {
      session$sendCustomMessage(type = "navigateTab", message = "Help and Resources")
    })

    # Toggle About / Cite panels on the landing page.
    observeEvent(input$aboutButton, {
      shinyjs::addClass(id = "aboutButton", class = "active")
      shinyjs::removeClass(id = "citeButton", class = "active")
      shinyjs::show(id = "aboutbox")
      shinyjs::hide(id = "citebox")
    })
    
    observeEvent(input$citeButton, {
      shinyjs::removeClass(id = "aboutButton", class = "active")
      shinyjs::addClass(id = "citeButton", class = "active")
      shinyjs::hide(id = "aboutbox")
      shinyjs::show(id = "citebox")
    })
  })
}