# --- 1. Libraries (shiny is loaded by app.R) ---
library(readxl)
library(knitr)
library(kableExtra)
model_info <- read_excel("www/model-info.xlsx")

# --- 2. Module UI ---
methods_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(2), 
    column(8, style="padding: 0 30px 0 0;",
           div(id=ns("methodsbox"),
               p("This page has a description of the statistical methods used in the calculator to compute the expected and excess deaths in countries."),
               
               HTML("<br/>"),
               
               h5(strong("Definitions")),
               
               p(strong("All-cause mortality"), 
                 " refers to the total number of recorded deaths from all causes, 
   regardless of age, sex, or underlying condition."),
               
               p(strong("Excess mortality"), 
                 " is the difference between the observed number of all-cause deaths during 
   a user-defined ", strong("incident period"), 
                 " (e.g., 2020–2021) and the ", strong("expected number of deaths"), 
                 " for the same period, based on historical data."),
               
               p("In other words, excess mortality estimates how many additional deaths occurred 
   compared to the number that would have been expected ", 
                 strong("had no unusual event or incident occurred"), "."),
               
               p("To estimate excess mortality, users can:"),
               
               tags$ul(
                 tags$li("Select a ", strong("baseline period"), 
                         " (commonly the 5 years immediately before the incident, 
           though other ranges can be used)."),
                 tags$li("Choose one or more ", strong("forecasting models"), 
                         " to calculate the expected deaths.")
               ), 
               
               HTML("<br/>"),
               
               h5(strong("Forecasting Models")), 
               
               tableOutput(ns("model_information")), 
               
               HTML("<br/>"),
               
               p("Below is a detailed description of the methods in statistical language.",
                 "It is in PDF format and can be saved for separate study."),
           ),
           # Embedded methodology PDF viewer.
           tags$iframe(
               src = "ACMCalculator_Methodology_210407.pdf",
               width = "100%",
               height = "600px",
               style = "border: none;"
           )
    )
  )
}

# --- 3. Module server ---
methods_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$downloadMethodsPDF <- downloadHandler(
      filename = function() {
        "ACMCalculator_Methodology_210407.pdf"
      },
      content = function(file) {
        file.copy(from = "www/ACMCalculator_Methodology_210407.pdf", to = file)
      }
    )

    output$pdfViewer <- renderUI({
      tags$iframe(
        style = "height:600px; width:100%; scrolling=yes",
        src = session$getResourcePath("ACMCalculator_Methodology_210407.pdf")
      )
    })

    output$model_information <- renderTable({
      model_info
    },
    striped  = TRUE,
    hover    = TRUE,
    spacing  = "xs",
    rownames = FALSE)
  })
}