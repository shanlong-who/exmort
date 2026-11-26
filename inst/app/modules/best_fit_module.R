# Best Fit tab
# Displays AIC for each model that produced results and highlights the
# minimum-AIC model as the best fit. Historical Average has no parametric
# AIC and is shown as N/A; it is excluded from the "Best" determination.

source_module("modules/interface_language.R")
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
          textOutput(ns("analysis_context")),
          p(
            "Lower AIC indicates a better in-sample fit on the baseline period. ",
            "AIC is compared across the count-regression models: Negative ",
            "Binomial, GAM and Zero-Inflated Poisson. Historical Average, ",
            "Quasi-Poisson, ARIMA and Karlinsky-Kobak are marked N/A and excluded ",
            "from this comparison. Cause-specific runs currently offer only one ",
            "eligible AIC model (Negative Binomial); this does not establish ",
            "superiority over alternative baselines."
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
      output$analysis_context <- renderText(em_context(rv))

    output$has_aic_data <- reactive({
      !is.null(rv$model_aic) && nrow(rv$model_aic) > 0
    })
    outputOptions(output, "has_aic_data", suspendWhenHidden = FALSE)

    output$best_banner <- renderUI({
      req(rv$model_aic)
      if (sum(is.finite(rv$model_aic$AIC)) == 1L && isTRUE(is.na(rv$best_model))) {
        div(class = "alert alert-info", em_label("Only one model has a finite AIC. A comparison requires at least two."))
      } else if (is.null(rv$best_model) || is.na(rv$best_model)) {
        div(
          class = "alert alert-warning",
          strong(em_label("No best fit available.")),
          em_label(" None of the run models produced a finite AIC.")
        )
      } else {
        div(
          class = "alert alert-success",
          strong(em_label("Best model by AIC: ")),
          em_label(rv$best_model)
        )
      }
    })

    output$aic_table <- DT::renderDT({
      req(rv$model_aic)
      df <- rv$model_aic %>%
        mutate(
          AIC = ifelse(is.finite(AIC), round(AIC, 2), NA_real_),
          Best = ifelse(Best, em_tr("Yes"), ""),
          Model = em_tr(Model)
        ) %>%
        arrange(is.na(AIC), AIC)

      em_datatable(
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
          backgroundColor = DT::styleEqual(em_tr("Yes"), "#dff0d8"),
          fontWeight = DT::styleEqual(em_tr("Yes"), "bold")
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
      session$sendCustomMessage("navigateTab", "tab3")
    })
  })
}
