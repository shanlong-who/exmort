# Best Fit tab
# Displays AIC for each model that produced results and highlights the
# minimum-AIC model as the best fit. Historical Average has no parametric
# AIC and is shown as N/A; it is excluded from the "Best" determination.

library(shiny)
library(DT)
library(dplyr)

best_fit_module_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    conditionalPanel(
      condition = "!output.has_aic_data",
      ns = ns,
      div(
        style = "text-align: center; margin-top: 50px;",
        h3("Model results are not available. Please run the model first...",
           style = "color: #666;"),
        br(),
        actionButton(ns("goto_model_tab"), "Go to Model tab",
          style = "margin-top: 20px; background-color: #337ab7; color: white;")
      )
    ),

    conditionalPanel(
      condition = "output.has_aic_data",
      ns = ns,
      fluidRow(
        column(
          12,
          h3("Best Fit by AIC"),
          p(
            "Lower AIC indicates a better in-sample fit on the baseline period. ",
            "AIC is only comparable across the count-regression models (Negative ",
            "Binomial and Zero-Inflated Poisson). Historical Average has no ",
            "parametric likelihood, the Quasi-Poisson family yields no proper ",
            "AIC, and ARIMA is a Gaussian time-series model whose likelihood is ",
            "not on the same scale as the count models. These three are marked ",
            "N/A and excluded from the best-fit selection."
          ),
          br(),
          uiOutput(ns("best_banner")),
          br(),
          wpro_spinner(DT::DTOutput(ns("aic_table"))),
          br(),
          downloadButton(ns("downloadAIC"), "Download AIC table")
        )
      )
    )
  )
}

best_fit_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    output$has_aic_data <- reactive({
      !is.null(rv$model_aic) && nrow(rv$model_aic) > 0
    })
    outputOptions(output, "has_aic_data", suspendWhenHidden = FALSE)

    output$best_banner <- renderUI({
      req(rv$model_aic)
      if (is.na(rv$best_model) || is.null(rv$best_model)) {
        div(
          class = "alert alert-warning",
          strong("No best fit available."),
          " None of the run models produced a finite AIC."
        )
      } else {
        div(
          class = "alert alert-success",
          strong("Best model by AIC: "),
          rv$best_model
        )
      }
    })

    output$aic_table <- DT::renderDT({
      req(rv$model_aic)
      df <- rv$model_aic %>%
        mutate(
          AIC = ifelse(is.finite(AIC), round(AIC, 2), NA_real_),
          Best = ifelse(Best, "Yes", "")
        ) %>%
        arrange(is.na(AIC), AIC)

      DT::datatable(
        df,
        options = list(
          dom = "t",
          pageLength = nrow(df),
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Best",
          target = "row",
          backgroundColor = DT::styleEqual("Yes", "#dff0d8"),
          fontWeight = DT::styleEqual("Yes", "bold")
        )
    })

    output$downloadAIC <- downloadHandler(
      filename = function() {
        paste0("Model_AIC_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        req(rv$model_aic)
        openxlsx::write.xlsx(rv$model_aic, file, rowNames = FALSE)
      }
    )

    observeEvent(input$goto_model_tab, {
      session$sendCustomMessage("navigateTab", "Model")
    })
  })
}
