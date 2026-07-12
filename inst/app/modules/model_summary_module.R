# Load necessary R packages
library(shiny)
library(dplyr)
library(DT)
library(reactable)  # Formatted result tables
library(openxlsx)
library(shinyjs)

# Shared renderer for the three result tables: thousands separators on count
# columns, diverging colour on signed excess and P-score (positive = red,
# negative = blue), event names as pills, and a footer with correct column
# totals (counts summed; P-score recomputed as 100 * sum(excess)/sum(expected)).
render_result_table <- function(d) {
  d <- as.data.frame(d)
  if (nrow(d) == 0) {
    return(reactable::reactable(
      data.frame(Message = "No rows for the current filter."),
      sortable = FALSE, pagination = FALSE
    ))
  }
  count_cols <- intersect(
    c("NO_DEATHS", "EXP_DEATHS", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS",
      "TOTAL_DEATHS", "TOTAL_EXPECTED", "TOTAL_EXCESS", "TOTAL_SE",
      "EXCESS_LOWER", "EXCESS_UPPER"),
    names(d))
  excess_cols <- intersect(c("EXCESS_DEATHS", "TOTAL_EXCESS"), names(d))
  exp_col     <- intersect(c("EXP_DEATHS", "TOTAL_EXPECTED"), names(d))[1]
  exc_col     <- intersect(c("EXCESS_DEATHS", "TOTAL_EXCESS"), names(d))[1]

  sign_style <- function(value) {
    if (is.na(value)) return(list())
    list(color = if (value > 0) "#b2182b" else if (value < 0) "#2166ac" else "#333",
         fontWeight = "600")
  }
  # When several models are shown together, each observed period is repeated
  # once per model, so a naive column sum multiplies counts by the model count
  # (e.g. the recorded-deaths total showed 2x with two models selected).
  # Divide the footer sum by the number of distinct models: exact for
  # model-invariant columns (recorded deaths), and the across-model mean for
  # per-model columns (expected/excess). P-score is a ratio and is unaffected.
  n_models <- if ("Model" %in% names(d)) max(1L, length(unique(d$Model))) else 1L
  fmtnum <- function(x) format(round(sum(x, na.rm = TRUE) / n_models), big.mark = ",")

  col_defs <- list()
  for (cc in count_cols) {
    col_defs[[cc]] <- reactable::colDef(
      align  = "right",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      style  = if (cc %in% excess_cols) sign_style else NULL,
      footer = fmtnum(d[[cc]])
    )
  }
  if ("P_SCORE" %in% names(d)) {
    ps_total <- if (!is.na(exp_col) && !is.na(exc_col) &&
                    isTRUE(sum(d[[exp_col]], na.rm = TRUE) != 0)) {
      sprintf("%.1f%%", 100 * sum(d[[exc_col]], na.rm = TRUE) / sum(d[[exp_col]], na.rm = TRUE))
    } else "—"
    col_defs[["P_SCORE"]] <- reactable::colDef(
      name = "P_SCORE (%)", align = "right",
      cell = function(value) if (is.na(value)) "" else sprintf("%.1f%%", value),
      style = sign_style, footer = ps_total
    )
  }
  if ("event_name" %in% names(d)) {
    col_defs[["event_name"]] <- reactable::colDef(name = "Event", cell = function(v) {
      if (is.na(v) || v == "" || v == "0") htmltools::tags$span(style = "color:#bbb;", "–")
      else htmltools::tags$span(
        style = "background:#eaf3fa;color:#0072B2;padding:1px 7px;border-radius:8px;font-size:0.85em;", v)
    })
  }
  for (yc in intersect(c("YEAR", "PERIOD", "event_index"), names(d))) {
    col_defs[[yc]] <- reactable::colDef(align = "right")  # plain integers, no separators
  }
  label_col <- names(d)[1]
  col_defs[[label_col]] <- reactable::colDef(footer = "Σ shown")

  reactable::reactable(
    d, columns = col_defs,
    filterable = TRUE, searchable = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE,
    defaultPageSize = 12, showPageSizeOptions = TRUE, pageSizeOptions = c(12, 25, 50),
    wrap = FALSE, resizable = TRUE,
    defaultColDef = reactable::colDef(
      align = "left", headerStyle = list(background = "#f7f7f7"),
      footerStyle = list(fontWeight = "700", borderTop = "2px solid #ddd")),
    theme = reactable::reactableTheme(borderColor = "#e5e5e5", stripedColor = "#fafafa")
  )
}

# Define UI interface for model summary
model_summary_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # Add CSS using tags$style
    tags$head(
      tags$style(HTML("
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }
        .dt-buttons { margin-bottom: 10px; }
        .data-not-ready { text-align: center; padding: 50px; color: #666; background: #f9f9f9; border-radius: 5px; margin: 20px; }
      "))
    ),

    # Conditional panel: prompt to run a model first if no results yet.
    conditionalPanel(
      condition = "!output.has_model_data",
      ns = ns,
      div(
        class = "data-not-ready",
        style = "text-align: center; margin-top: 50px;",
        h3("Model results are not available. Please run the model first...", style = "color: #666;"),
        br(),
        div(style = "margin-top: 20px;",
          actionButton(ns("goto_model_tab"), "Go to Model tab", class = "btn-primary")
        )
      )
    ),

    # Main results panel, shown only when model results are present.
    conditionalPanel(
      condition = "output.has_model_data",
      ns = ns,
      fluidRow(
        # Left side for table content
        column(
          9,
          tabsetPanel(
            id = ns("tabsetpanel"),
            tabPanel(
              "Total Prediction Results",
              value = "Total Prediction Results",
              br(),
              wellPanel(
                # Download button
                wpro_spinner(reactable::reactableOutput(ns("total_predictions"))),
                br(), br(),
                downloadButton(ns("downloadTotalPredictions"), "Download Full Data")
              )
            ),
            tabPanel(
              "Summary by Event",
              value = "Summary by Event",
              br(),
              wellPanel(
                # Download button
                wpro_spinner(reactable::reactableOutput(ns("summary_by_event"))),
                br(), br(),
                downloadButton(ns("downloadSummaryByEvent"), "Download Full Data")
              )
            ),
            tabPanel(
              "Overall Summary",
              value = "Overall Summary",
              br(),
              wellPanel(
                # Download button
                wpro_spinner(reactable::reactableOutput(ns("overall_summary"))),
                br(), br(),
                downloadButton(ns("downloadOverallSummary"), "Download Full Data")
              )
            )
          )
        ),
        # Right side for filter controls
        column(
          3,
          wellPanel(
            h4("Filter Results"),
            # Sex filter
            uiOutput(ns("sex_filter")),
            # Age group filter
            uiOutput(ns("age_group_filter")),
            # Model filter
            uiOutput(ns("model_filter")),
            # Event filter (for Summary by Event tab)
            conditionalPanel(
              condition = "input.tabsetpanel === 'Summary by Event'",
              ns = ns,
              uiOutput(ns("event_filter"))
            )
          )
        )
      )
    )
  )
}

# Define server logic for model summary
model_summary_module_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      # Drives the conditionalPanel above.
      output$has_model_data <- reactive({
        !is.null(rv$total_prediction) && !is.null(rv$summary_by_event) && !is.null(rv$overall_summary)
      })
      outputOptions(output, "has_model_data", suspendWhenHidden = FALSE)

      # Track which sub-tab the user is on.
      current_tab <- reactive({
        input$tabsetpanel
      })

      # Filter UI is generated from whatever values appear in the results.
      output$sex_filter <- renderUI({
        req(rv$overall_summary)
        choices <- unique(rv$overall_summary$SEX)
        selectInput(session$ns("selected_sex"), "Select Sex:",
          choices = choices,
          selected = if ("Total" %in% choices) "Total" else choices[1]
        )
      })

      output$age_group_filter <- renderUI({
        req(rv$overall_summary)
        choices <- unique(rv$overall_summary$AGE_GROUP)
        selectInput(session$ns("selected_age_group"), "Select Age Group:",
          choices = choices,
          selected = if ("Total" %in% choices) "Total" else choices[1]
        )
      })

      output$model_filter <- renderUI({
        req(rv$overall_summary)
        choices <- unique(rv$overall_summary$Model)
        checkboxGroupInput(session$ns("selected_models"), "Select Models:",
          choices = choices,
          selected = choices
        )
      })

      output$event_filter <- renderUI({
        req(rv$summary_by_event)
        choices <- unique(rv$summary_by_event$event_name)
        selectInput(session$ns("selected_event"), "Select Event:",
          choices = c("All", choices),
          selected = "All"
        )
      })

      # Filtered views into the three result tables.
      filtered_total_predictions <- reactive({
        req(rv$total_prediction, input$selected_sex, input$selected_age_group, input$selected_models)

        data <- rv$total_prediction %>%
          filter(
            SEX == input$selected_sex,
            AGE_GROUP == input$selected_age_group,
            Model %in% input$selected_models
          )

        return(data)
      })

      filtered_summary_by_event <- reactive({
        req(rv$summary_by_event, input$selected_sex, input$selected_age_group, input$selected_models)

        data <- rv$summary_by_event %>%
          filter(
            SEX == input$selected_sex,
            AGE_GROUP == input$selected_age_group,
            Model %in% input$selected_models
          )

        # If a specific event is picked, narrow the table to that event.
        if (!is.null(input$selected_event) && input$selected_event != "All") {
          data <- data %>% filter(event_name == input$selected_event)
        }

        return(data)
      })

      filtered_overall_summary <- reactive({
        req(rv$overall_summary, input$selected_sex, input$selected_age_group, input$selected_models)

        data <- rv$overall_summary %>%
          filter(
            SEX == input$selected_sex,
            AGE_GROUP == input$selected_age_group,
            Model %in% input$selected_models
          )

        return(data)
      })

      # Table outputs
      output$total_predictions <- reactable::renderReactable({
        req(filtered_total_predictions())
        render_result_table(filtered_total_predictions())
      })

      output$summary_by_event <- reactable::renderReactable({
        req(filtered_summary_by_event())
        render_result_table(filtered_summary_by_event())
      })

      output$overall_summary <- reactable::renderReactable({
        req(filtered_overall_summary())
        render_result_table(filtered_overall_summary())
      })

      # Excel download handlers
      output$downloadTotalPredictions <- downloadHandler(
        filename = function() {
          paste("Total_Predictions_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
        },
        content = function(file) {
          req(filtered_total_predictions())
          openxlsx::write.xlsx(filtered_total_predictions(), file, rowNames = FALSE)
        }
      )

      output$downloadSummaryByEvent <- downloadHandler(
        filename = function() {
          paste("Summary_By_Event_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
        },
        content = function(file) {
          req(filtered_summary_by_event())
          openxlsx::write.xlsx(filtered_summary_by_event(), file, rowNames = FALSE)
        }
      )

      output$downloadOverallSummary <- downloadHandler(
        filename = function() {
          paste("Overall_Summary_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
        },
        content = function(file) {
          req(filtered_overall_summary())
          openxlsx::write.xlsx(filtered_overall_summary(), file, rowNames = FALSE)
        }
      )

      # Navigation back to the Model tab from the empty-state link.
      observeEvent(input$goto_model_tab, {
        session$sendCustomMessage("navigateTab", "Model")
      })
    }
  )
}
