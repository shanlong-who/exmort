# plot_prediction_module_new.R
# Plot module for the "Model prediction results" tab on the Plots page.

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(RColorBrewer)
source_module("modules/plot_functions/plot_utils.R")

plot_prediction_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Data Filtering"),
        uiOutput(ns("sex_selector")),
        uiOutput(ns("age_group_selector")),
        h4("Model Selection"),
        uiOutput(ns("model_selector")),
        hr(),
        h4("Time Range Selection"),
        uiOutput(ns("time_slider")),
        hr(),
        h4("Download Plots (PDF)"),
        downloadButton(ns("downloadACMPlot"), "Download ACM Plot"),
        br(), br(),
        downloadButton(ns("downloadEDPlot"), "Download Excess Deaths Plot"),
        br(), br(),
        downloadButton(ns("downloadEPPlot"), "Download P-Score Plot")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("plotTabs"),
          tabPanel("All Cause Mortality", br(), h4("All Cause Mortality (Recorded vs. Expected)"), wpro_spinner(plotlyOutput(ns("ACMplot"), height = "600px"))),
          tabPanel(
            "Excess Deaths",
            br(),
            tabsetPanel(
              id = ns("edTabs"),
              tabPanel("Bar Plot", br(), h4("Excess Deaths"), wpro_spinner(plotlyOutput(ns("EDplot"), height = "600px"))),
              tabPanel("Line Plot", br(), h4("Excess Deaths (Line Plot)"), wpro_spinner(plotlyOutput(ns("EDLinePlot"), height = "600px")))
            )
          ),
          tabPanel(
            "P-Score",
            br(),
            tabsetPanel(
              id = ns("psTabs"),
              tabPanel("Bar Plot", br(), h4("P-Score of Excess Deaths (%)"), wpro_spinner(plotlyOutput(ns("EPplot"), height = "600px"))),
              tabPanel("Line Plot", br(), h4("P-Score (Line Plot)"), wpro_spinner(plotlyOutput(ns("EPLinePlot"), height = "600px")))
            )
          )
        )
      )
    )
  )
}

plot_prediction_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter selectors are populated from whatever values appear in
    # rv$total_prediction.
    output$sex_selector <- renderUI({
      req(rv$total_prediction)
      unique_sex <- sort(unique(rv$total_prediction$SEX), na.last = TRUE, decreasing = TRUE)
      selectInput(ns("selected_sex"), "Select Sex:", choices = unique_sex, selected = unique_sex[1], multiple = FALSE)
    })

    output$age_group_selector <- renderUI({
      req(rv$total_prediction)
      unique_age_group <- sort(unique(rv$total_prediction$AGE_GROUP), na.last = TRUE, decreasing = TRUE)
      selectInput(ns("selected_age_group"), "Select Age Group:", choices = unique_age_group, selected = unique_age_group[1], multiple = FALSE)
    })

    output$model_selector <- renderUI({
      req(rv$total_prediction)
      unique_models <- sort(unique(rv$total_prediction$Model))
      default_selected <- if ("Negative Binomial Regression" %in% unique_models) "Negative Binomial Regression" else unique_models[1]
      checkboxGroupInput(ns("selected_models"), "Select Models to Display:", choices = unique_models, selected = default_selected)
    })

    output$time_slider <- renderUI({
      req(rv$total_prediction, input$selected_sex, input$selected_age_group, input$selected_models)
      data <- rv$total_prediction %>%
        filter(SEX == input$selected_sex, AGE_GROUP == input$selected_age_group, Model %in% input$selected_models)
      data_dates <- data %>%
        mutate(TimePoint = case_when(
          max(PERIOD, na.rm = TRUE) > 12 ~ as.Date(paste(YEAR, 1, 1, sep = "-")) + weeks(PERIOD - 1) + days(3),
          TRUE ~ as.Date(paste(YEAR, PERIOD, 15, sep = "-"))
        )) %>%
        pull(TimePoint)
      if (length(data_dates) == 0) return(NULL)
      date_range <- range(data_dates, na.rm = TRUE)
      sliderInput(ns("time_range"), "Select Time Period:", min = date_range[1], max = date_range[2], value = date_range, timeFormat = "%Y-%m", step = 7, width = "100%")
    })

    # Filtered prediction data shared by all five renderPlotly outputs.
    filtered_total_data <- reactive({
      req(rv$total_prediction, input$selected_sex, input$selected_age_group, input$selected_models, input$time_range)
      data <- rv$total_prediction %>%
        filter(SEX == input$selected_sex, AGE_GROUP == input$selected_age_group, Model %in% input$selected_models)
      max_period <- max(data$PERIOD, na.rm = TRUE)
      data %>%
        mutate(TimePoint = case_when(
          max_period > 12 ~ as.Date(paste(YEAR, 1, 1, sep = "-")) + weeks(PERIOD - 1) + days(3),
          TRUE ~ as.Date(paste(YEAR, PERIOD, 15, sep = "-"))
        )) %>%
        filter(TimePoint >= input$time_range[1], TimePoint <= input$time_range[2]) %>%
        arrange(TimePoint)
    })

    # plotly chart outputs
    output$ACMplot <- renderPlotly({
      tryCatch(
        {
          create_mortality_plot(filtered_total_data(), plot_type = "ACM", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = TRUE)
        },
        error = function(e) {
          message("Error in ACM plot: ", e$message)
          plot_ly() %>% add_annotations(text = paste("Error:", e$message), x = 0.5, y = 0.5, showarrow = FALSE)
        }
      )
    })

    output$EDplot <- renderPlotly({
      tryCatch(
        {
          create_mortality_plot(filtered_total_data(), plot_type = "ED_bar", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = TRUE)
        },
        error = function(e) {
          message("Error in ED plot: ", e$message)
          plot_ly() %>% add_annotations(text = paste("Error:", e$message), x = 0.5, y = 0.5, showarrow = FALSE)
        }
      )
    })

    output$EDLinePlot <- renderPlotly({
      tryCatch(
        {
          create_mortality_plot(filtered_total_data(), plot_type = "ED_line", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = TRUE)
        },
        error = function(e) {
          message("Error in ED Line plot: ", e$message)
          plot_ly() %>% add_annotations(text = paste("Error:", e$message), x = 0.5, y = 0.5, showarrow = FALSE)
        }
      )
    })

    output$EPplot <- renderPlotly({
      tryCatch(
        {
          create_mortality_plot(filtered_total_data(), plot_type = "EP_bar", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = TRUE)
        },
        error = function(e) {
          message("Error in EP plot: ", e$message)
          plot_ly() %>% add_annotations(text = paste("Error:", e$message), x = 0.5, y = 0.5, showarrow = FALSE)
        }
      )
    })

    output$EPLinePlot <- renderPlotly({
      tryCatch(
        {
          create_mortality_plot(filtered_total_data(), plot_type = "EP_line", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = TRUE)
        },
        error = function(e) {
          message("Error in EP Line plot: ", e$message)
          plot_ly() %>% add_annotations(text = paste("Error:", e$message), x = 0.5, y = 0.5, showarrow = FALSE)
        }
      )
    })

    # PDF download handlers
    output$downloadACMPlot <- downloadHandler(
      filename = function() paste0("ACM_Plot_", input$selected_sex, "_", input$selected_age_group, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        tryCatch(
          {
            plot_obj <- create_mortality_plot(filtered_total_data(), plot_type = "ACM", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = FALSE)
            ggsave(file, plot = plot_obj, device = "pdf", width = 11, height = 7, units = "in")
          },
          error = function(e) message("Error in downloading ACM plot: ", e$message)
        )
      }
    )

    output$downloadEDPlot <- downloadHandler(
      filename = function() paste0("ExcessDeaths_Plot_", input$selected_sex, "_", input$selected_age_group, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        tryCatch(
          {
            plot_obj <- create_mortality_plot(filtered_total_data(), plot_type = "ED_bar", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = FALSE)
            ggsave(file, plot = plot_obj, device = "pdf", width = 11, height = 7, units = "in")
          },
          error = function(e) message("Error in downloading ED plot: ", e$message)
        )
      }
    )

    output$downloadEPPlot <- downloadHandler(
      filename = function() paste0("PScore_Plot_", input$selected_sex, "_", input$selected_age_group, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        tryCatch(
          {
            plot_obj <- create_mortality_plot(filtered_total_data(), plot_type = "EP_bar", selected_sex = input$selected_sex, selected_age_group = input$selected_age_group, for_plotly = FALSE)
            ggsave(file, plot = plot_obj, device = "pdf", width = 11, height = 7, units = "in")
          },
          error = function(e) message("Error in downloading EP plot: ", e$message)
        )
      }
    )

    observe({
      rv$plot_time_data <- filtered_total_data()
    })
  })
}