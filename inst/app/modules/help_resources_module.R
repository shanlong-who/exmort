# --- 1. Libraries (shiny is loaded by app.R) ---

# --- 2. Module UI ---
help_resources_module_ui <- function(id) {
  ns <- NS(id)
  li  <- tags$li
  ol  <- tags$ol
  pre <- tags$pre
  
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      h5(tags$u('Resources')),
      div(title = "Wiki page for the calculator",
          a("About WPRO all-cause-of-mortality and excess death calculator",
            href = "https://github.com/WorldHealthOrganization/ACMCalculator/wiki",
            target = "_blank")),
      div(title="WPRO all-cause mortality dashboard",
          a("WPRO all-cause mortality dashboard",
            href="https://lynx.wpro.who.int/viz/allcausedeath.asp",
            target="_blank")
      ),
      div(title=paste("Information on the methodology used",
                      "in the calculator"),
          a("About the methodology used in the tool.",
            href = "https://github.com/WorldHealthOrganization/ACMCalculator/wiki/Methodology-used-in-ACMCalculator/", target = "_blank")
      ),
      br(),
      div(a("WPRO all-cause-of-mortality and excess death calculator on GitHub", href="https://github.com/WorldHealthOrganization/ACMCalculator",
            target="_blank")),
      div(a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
            target="_blank"))
    ),
    mainPanel(
      
      ## ---------- Title ----------
      h3("Help with the WPRO All-Cause-of-Mortality & Excess Death Calculator"),
      p(
        "This app is maintained on GitHub. To request new features or report a bug, ",
        "please open an issue in the ",
        a("repository",
          href = "https://github.com/WorldHealthOrganization/ACMCalculator",
          target = "_blank"),
        " or email us at ",
        a(icon("envelope"), " dings@who.int",
          href = "mailto:dings@who.int"),
        ", ",
        a(icon("envelope"), " sedait@who.int",
          href = "mailto:sedait@who.int"), "."
      ),
      
      ## ---------- Offline install guide (collapsible) ----------
      tags$details(
        tags$summary(tags$b(icon("download"), " How to install the app offline (click to expand)")),
        p("The calculator is distributed as an R package, ", code("exmort"), ". ",
          "Installing it locally keeps your data on your machine and lets you run ",
          "the app without an internet connection."),
        p(strong("Install using either of these methods:")),
        p("1. Latest development version from GitHub (needs the ", code("remotes"), " package):"),
        tags$pre('remotes::install_github("shanlong-who/exmort")'),
        p("2. Released version from CRAN:"),
        tags$pre('install.packages("exmort")'),
        p(strong("Then start the app at any time with:")),
        tags$pre('exmort::run_app()'),
        p(class = "text-muted", style = "font-size:0.9em;",
          "The app opens in your default web browser. Everything runs on your ",
          "own computer; no data leaves your machine.")
      ),
      
      hr(),

      ## ---------- User guide (collapsible) ----------
      tags$details(
        tags$summary(tags$b(icon("book"), " ACM Excess Mortality Model User Guide (click to expand)")),
        
        h4("1  Introduction"),
        p("This guide explains how to use the Historical Average, Negative-Binomial, ",
          "and other models to estimate excess mortality correctly."),
        
        h4("2  Basic Model Assumptions"),
        tags$ul(
          li(strong("Event impact:"), " major health events (e.g. COVID-19) may raise deaths."),
          li(strong("Intervention impact:"), " control measures may reduce deaths."),
          li(strong("Baseline prediction:"), " models learn from pre-event ‘normal’ data ",
             "to predict expected deaths during the event.")
        ),
        
        h4("3  Key Points & Tips"),
        
        ## --- 3.1 Event-period consistency ---
        h5("3.1  Data Input & Event-Period Consistency"),
        p("Define the event period precisely; training data should exclude that period ",
          "and any abnormal post-event weeks unless they truly represent a new baseline."),
        div(class = "alert alert-info",
            "Tip: mis-setting the event period (e.g. including data to Dec 2023 ",
            "when analysing up to Aug 2023) will under-estimate excess mortality."),
        
        ## Nested checklist using <ul>/<ol>
        tags$ul(
          li(strong("Checklist before running:"), 
             tags$ul(
               li("Event start & end dates set?"),
               li("Training data excludes event & unstable post-event weeks?"),
               li("Preview the automatic ", code("event_index"), " column in the upload preview.")
             )
          )
        ),
        
        ## --- 3.2 Effect of differing event-period definitions ---
        h5("3.2  Why different event dates give different results"),
        tags$ul(
          li("Changing dates changes the training set → different expected deaths."),
          li("Keep the definition consistent across all analyses unless justified.")
        ),
        
        ## --- 3.3 Whether to include post-event periods ---
        h5("3.3  Including post-event data in training"),
        tags$ul(
          li("May distort baseline if trends shifted after the event."),
          li("Generally avoid unless mortality clearly returned to normal.")
        ),
        
        h4("4  Conclusion"),
        p("Careful data preparation and correct event-period definition are key to reliable ",
          "excess-mortality estimates. For details see the methodology page or contact us.")
      )
    )
  )
}

# --- 3. Module server ---
help_resources_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Email links are handled by their href attributes; the observers
    # exist only to register the input ids in case downstream code wants
    # to react to clicks.
    observeEvent(input$email1, { })
    observeEvent(input$email2, { })
  })
}