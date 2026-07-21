library(shiny)
library(DT)
library(reactable)   # Formatted preview tables
library(plotly)      # Sparkline of the uploaded death series
library(openxlsx)    # Reading Excel files
library(lubridate)   # Date processing
library(dplyr)       # Data operations
library(shinyjs)     # JavaScript-based UI helpers (progress messages)
library(shinyalert)  # Modal dialogs for validation errors
library(zoo)         # na.locf for forward-filling area labels

source_module("modules/utils.R")
source_module("modules/data_validation.R")

# UI for the Data tab
data_process_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
        /* Download button spacing */
        .dt-buttons {
          margin-bottom: 10px;
        }
        .download-btn {
          margin-bottom: 10px;
        }
      "))
    ),
    fluidRow(
      column(
        7,
        tabsetPanel(
          id = ns("datatabs"),
          tabPanel(
            "Upload Data",
            br(),
            wellPanel(
              p("This tool estimates weekly or monthly excess deaths in the Western Pacific Region."),
              p(
                strong("Expected deaths"), 
                "are estimated using user-selected statistical models, including ", 
                strong("Negative Binomial regression, Poisson regression, "), 
                "and ", 
                strong("ARIMA time series models"), 
                ", based on a user-defined baseline period (typically pre-incident)."
              ),
              p(
                class = "helper", icon("question-circle"),
                span("What format does the Excel file need?", style = "font-size:0.85em;"),
                br(), "Please upload a *.xls or *.xlsx file following the WHO standardized template."
              ),
              selectizeInput(ns("template_country"),
                label = "Download a template:",
                choices = c(
                  "Choose a template" = "",
                  "Australia (empty template)", "Philippines (empty template)", "French Polynesia (empty template)",
                  "Generic Monthly template", "Generic Weekly template",
                  "Australia (filled up to August 2020)", "Japan (filled up to August 2020)",
                  "Republic of Korea (filled up to August 2020)", "New Zealand (filled up to August 2020)",
                  "Philippines (filled up to August 2020)"
                )
              ),
              downloadButton(ns("download_templates"), "Download template"),
              br(), br(),
              selectInput(ns("filetype"),
                label = "Open a data set",
                choices = c(
                  "Excel spreadsheet (*.xls or *.xlsx)" = 1,
                  "Built-in example data" = 2
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("filetype"), "'] == 1"),
                fileInput(ns("rawdatafile"), label = NULL, accept = c(".xls", ".xlsx")),
                uiOutput(ns("selectsheet")),
                uiOutput(ns("selecteventsheet")),
                actionButton(ns("loadEvents"), "Merge Mortality/Events Data"),
                # Progress / status message line for uploaded data.
                div(id = ns("progress_upload"), style = "color: blue; margin-top: 10px;")
              ),
              conditionalPanel(
                condition = paste0("input['", ns("filetype"), "'] == 2"),
                selectizeInput(ns("samplecountry"),
                  label = "Choose a country",
                  choices = c("Choose a country" = "",
                              "Australia", "Japan", "Republic of Korea",
                              "New Zealand", "Philippines")
                ),
                uiOutput(ns("selectbuiltinsheet")),
                uiOutput(ns("selectbuiltineventsheet")),
                actionButton(ns("loadBuiltinEvents"), "Merge Built-in Data"),
                # Progress / status message line for built-in data.
                div(id = ns("progress_builtin"), style = "color: blue; margin-top: 10px;")
              ),
              # Persistent validation feedback (stays visible after the modal is
              # dismissed so the user can fix the file against it).
              uiOutput(ns("validation_panel"))
            )
          ),
          tabPanel(
            "View Data",
            br(),
            p("Below is the all-cause mortality data. Expected columns: 'AREA', 'AGE_GROUP', 'SEX', 'YEAR', 'PERIOD', 'NO_DEATHS'."),
            wellPanel(
              h4("Merged Data Table"),
              p(class = "helper", style = "font-size:0.85em;color:#555;",
                "Mortality data joined with event annotations — the input to the models."),
              reactable::reactableOutput(ns("ACM_table"), width = "100%"),
              # Download button
              div(class = "download-btn", style = "margin-top:10px;",
                downloadButton(ns("downloadMergedData"), "Download Full Data")
              ),
            ),
            wellPanel(
              h4("Mortality Data (processed)"),
              reactable::reactableOutput(ns("raw_mortality_table"), width = "100%"),
              # Download button
              div(class = "download-btn", style = "margin-top:10px;",
                downloadButton(ns("downloadRawData"), "Download Full Data")
              )
            ),
            wellPanel(
              h4("Events Data"),
              reactable::reactableOutput(ns("events_table"), width = "100%"),
              # Download button
              div(class = "download-btn", style = "margin-top:10px;",
                downloadButton(ns("downloadEventsData"), "Download Full Data")
              )
            )
          )
        )
      ),
      column(
        4,
        tabsetPanel(
          tabPanel(
            "Data Summary", br(),
            uiOutput(ns("health_cards")),
            div(style = "font-size:0.82em;color:#555;margin:6px 2px 2px;",
                "Reported deaths over time (national total)"),
            plotly::plotlyOutput(ns("health_spark"), height = "150px"),
            br(),
            reactable::reactableOutput(ns("health_breakdown"))
          )
        )
      )
    )
  )
}

# Server logic for the Data tab
data_process_module_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive store, mirrored into the shared rv after each change.
    data <- reactiveValues(
      acm_rawdata = NULL,
      acm_data = NULL,
      event_data = NULL,
      merged_data = NULL,
      data_description = NULL
    )

    # Mirror local state into the shared reactiveValues so other modules see it.
    observe({
      rv$acm_rawdata <- data$acm_rawdata
      rv$acm_data <- data$acm_data
      rv$event_data <- data$event_data
      rv$merged_data <- data$merged_data
      rv$data_description <- data$data_description
    })

    # Last validation result, persisted so the inline panel survives the modal.
    validation_state <- reactiveVal(NULL)

    # --- Shared formatted preview table -------------------------------------
    # Thousands separators on count columns, event_name shown as a pill, text
    # left-aligned / numerics right-aligned, filterable + striped. One helper so
    # all three preview tables (merged / mortality / events) stay consistent.
    make_preview_table <- function(df, placeholder = "No data loaded yet.") {
      if (is.null(df) || nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(Message = placeholder),
          sortable = FALSE, pagination = FALSE,
          defaultColDef = reactable::colDef(headerStyle = list(background = "#f7f7f7"))
        ))
      }
      df <- as.data.frame(df)
      count_cols <- intersect(c("NO_DEATHS", "DAYS", "EXP_DEATHS"), names(df))
      col_defs <- list()
      for (cc in count_cols) {
        col_defs[[cc]] <- reactable::colDef(
          format = reactable::colFormat(separators = TRUE, digits = 0), align = "right"
        )
      }
      if ("event_name" %in% names(df)) {
        col_defs[["event_name"]] <- reactable::colDef(
          name = "Event",
          cell = function(value) {
            if (is.na(value) || value == "") {
              htmltools::tags$span(style = "color:#bbb;", "–")
            } else {
              htmltools::tags$span(
                style = "background:#eaf3fa;color:#0072B2;padding:1px 7px;border-radius:8px;font-size:0.85em;",
                value)
            }
          }
        )
      }
      reactable::reactable(
        df,
        columns = col_defs,
        filterable = TRUE, searchable = TRUE, striped = TRUE, highlight = TRUE,
        compact = TRUE, defaultPageSize = 12, showPageSizeOptions = TRUE,
        pageSizeOptions = c(12, 25, 50), wrap = FALSE, resizable = TRUE,
        defaultColDef = reactable::colDef(align = "left", headerStyle = list(background = "#f7f7f7")),
        theme = reactable::reactableTheme(borderColor = "#e5e5e5", stripedColor = "#fafafa")
      )
    }

    # --- Data-health helpers -------------------------------------------------
    # National-total slice (most-aggregated SEX/AGE if a "Total" level exists),
    # so headline totals and the sparkline do not sum across subgroups.
    national_total <- reactive({
      df <- as.data.frame(data$merged_data)
      # req(df) alone does NOT stop here: isTruthy() treats a data.frame as
      # truthy whenever it is non-NULL (a data.frame is a list, not atomic),
      # so an empty/NULL merged_data (as.data.frame(NULL) -> 0x0) would slip
      # through and the metrics below would run on absent columns.
      req(is.data.frame(df), nrow(df) > 0)
      if ("AGE_GROUP" %in% names(df) && "Total" %in% df$AGE_GROUP) df <- df[df$AGE_GROUP == "Total", ]
      if ("SEX" %in% names(df) && "Total" %in% df$SEX) df <- df[df$SEX == "Total", ]
      df
    })

    health_metrics <- reactive({
      df <- as.data.frame(data$merged_data)
      req(is.data.frame(df), nrow(df) > 0)
      weekly <- max(df$PERIOD, na.rm = TRUE) > 12
      fmt <- function(y, p) if (weekly) sprintf("%d W%02d", y, p) else paste(y, month.abb[p])
      lo_y <- min(df$YEAR, na.rm = TRUE); lo_p <- min(df$PERIOD[df$YEAR == lo_y], na.rm = TRUE)
      hi_y <- max(df$YEAR, na.rm = TRUE); hi_p <- max(df$PERIOD[df$YEAR == hi_y], na.rm = TRUE)
      n_events <- if ("event_name" %in% names(df)) length(setdiff(unique(df$event_name), c("", NA, "0"))) else NA_integer_
      n_missing <- sum(is.na(df$NO_DEATHS))
      list(
        n_records    = nrow(df),
        period_type  = if (weekly) "Weekly" else "Monthly",
        date_range   = paste0(fmt(lo_y, lo_p), " – ", fmt(hi_y, hi_p)),
        n_events     = n_events,
        total_deaths = sum(national_total()$NO_DEATHS, na.rm = TRUE),
        n_missing    = n_missing,
        pct_missing  = if (nrow(df) > 0) 100 * n_missing / nrow(df) else 0
      )
    })

    output$selectsheet <- renderUI({
      req(input$rawdatafile)
      sheets <- readxl::excel_sheets(input$rawdatafile$datapath)
      selected_sheet <- ifelse("National level" %in% sheets, "National level", sheets[1])
      selectInput(ns("sheetname"), "Select mortality data sheet", choices = sheets, selected = selected_sheet)
    })

    output$selecteventsheet <- renderUI({
      req(input$rawdatafile, input$sheetname)
      sheets <- readxl::excel_sheets(input$rawdatafile$datapath)
      event_sheets <- setdiff(sheets, input$sheetname)
      # Offer ALL candidate event sheets (so events(m) etc. are selectable);
      # only the default selection is a scalar. The old ifelse() collapsed
      # `choices` to a single sheet because ifelse() returns a length-1 value.
      selected_event_sheet <- if ("events" %in% event_sheets) "events" else event_sheets[1]
      selectInput(ns("eventsheetname"), "Select events data sheet",
                  choices = event_sheets, selected = selected_event_sheet)
    })

    output$selectbuiltinsheet <- renderUI({
      req(input$samplecountry)
      file_path <- file.path("XLSX", paste0(gsub(" ", "_", input$samplecountry), "_built_in_data.xlsx"))
      if (file.exists(file_path)) {
        sheets <- readxl::excel_sheets(file_path)
        selected_sheet <- ifelse("National level" %in% sheets, "National level", sheets[1])
        selectInput(ns("builtinsheetname"), "Select mortality data sheet", choices = sheets, selected = selected_sheet)
      }
    })

    output$selectbuiltineventsheet <- renderUI({
      req(input$samplecountry, input$builtinsheetname)
      file_path <- file.path("XLSX", paste0(gsub(" ", "_", input$samplecountry), "_built_in_data.xlsx"))
      if (file.exists(file_path)) {
        sheets <- readxl::excel_sheets(file_path)
        event_sheets <- setdiff(sheets, input$builtinsheetname)
        selected_event_sheet <- ifelse("events" %in% event_sheets, "events", event_sheets)
        selectInput(ns("builtineventsheetname"), "Select events data sheet", choices = event_sheets, selected = selected_event_sheet)
      }
    })

    # Load and merge uploaded data, with a progress bar and validation.
    observeEvent(input$loadEvents, {
      req(input$rawdatafile, input$sheetname, input$eventsheetname)

      shinyjs::html(ns("progress_upload"), "Processing data, please wait...")
      shinyjs::show(ns("progress_upload"))

      temp_file <- tempfile(pattern = "upload_", fileext = ".xlsx")
      file.copy(input$rawdatafile$datapath, temp_file, overwrite = TRUE)

      withProgress(message = "Merging uploaded data...", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Reading mortality data...")
          raw_data_new <- openxlsx::read.xlsx(temp_file, sheet = input$sheetname, colNames = FALSE)

          incProgress(0.4, detail = "Processing mortality data...")
          acm_data_new <- process_mortality_data2(temp_file, input$sheetname)

          incProgress(0.6, detail = "Loading events data...")
          event_data_new <- loadEventsData(temp_file, input$eventsheetname)

          # Schema check BEFORE committing anything to shared state. On failure
          # the previously loaded data$acm_data / event_data / merged_data are
          # left untouched, so the preview tables stay consistent instead of
          # showing a half-loaded rejected file.
          validation_msg <- validate_uploaded_data(acm_data_new, event_data_new)
          if (!is.null(validation_msg)) {
            validation_state(list(ok = FALSE, msg = validation_msg))
            shinyjs::html(ns("progress_upload"), "Validation failed. See dialog for details.")
            shinyalert::shinyalert(
              title = "Data validation failed",
              text  = paste0("<pre style='text-align:left;'>", htmltools::htmlEscape(validation_msg), "</pre>"),
              type  = "error",
              html  = TRUE,
              confirmButtonText = "OK"
            )
            return(invisible(NULL))
          }

          # Validation passed: commit the new data, then merge.
          data$acm_rawdata <- raw_data_new
          data$acm_data <- acm_data_new
          data$event_data <- event_data_new

          incProgress(0.8, detail = "Merging data...")
          data$merged_data <- mergeEventsData(data$acm_data, data$event_data)
          data$data_description <- paste("Uploaded data from", input$rawdatafile$name)
          validation_state(list(ok = TRUE))

          # Clear downstream results so stale predictions/AIC do not linger
          # after a re-upload (best_fit_module gates on rv$model_aic).
          rv$total_prediction <- NULL
          rv$plot_time_data   <- NULL
          rv$summary_by_event <- NULL
          rv$overall_summary  <- NULL
          rv$model_aic        <- NULL
          rv$best_model       <- NULL

          shinyjs::html(ns("progress_upload"), "Data merged successfully!")
        },
        finally = {
          if (file.exists(temp_file)) {
            unlink(temp_file)
          }
        })

        incProgress(0.2, detail = "Complete!")
      })

      shinyjs::delay(2000, shinyjs::hide(ns("progress_upload")))
    })

    # Load and merge built-in country data, with the same validation step.
    observeEvent(input$loadBuiltinEvents, {
      req(input$samplecountry, input$builtinsheetname, input$builtineventsheetname)

      shinyjs::html(ns("progress_builtin"), "Loading built-in data, please wait...")
      shinyjs::show(ns("progress_builtin"))

      # Map UI-friendly country labels to the underscored filenames in XLSX/.
      country_name <- c("Australia", "Japan", "Republic_of_Korea", "New_Zealand", "Philippines")[
        match(input$samplecountry, c(
          "Australia", "Japan", "Republic of Korea", "New Zealand", "Philippines"
        ))
      ]

      file_path <- file.path("XLSX", paste0(country_name, "_built_in_data.xlsx"))

      withProgress(message = "Loading built-in data...", value = 0, {
        if (file.exists(file_path)) {
          incProgress(0.2, detail = "Reading mortality data...")
          raw_data_new <- openxlsx::read.xlsx(file_path, sheet = input$builtinsheetname, colNames = FALSE)

          incProgress(0.4, detail = "Processing mortality data...")
          acm_data_new <- process_mortality_data2(file_path, input$builtinsheetname)

          incProgress(0.6, detail = "Loading events data...")
          event_data_new <- loadEventsData(file_path, input$builtineventsheetname)

          validation_msg <- validate_uploaded_data(acm_data_new, event_data_new)
          if (!is.null(validation_msg)) {
            validation_state(list(ok = FALSE, msg = validation_msg))
            shinyjs::html(ns("progress_builtin"), "Validation failed. See dialog for details.")
            shinyalert::shinyalert(
              title = "Data validation failed",
              text  = paste0("<pre style='text-align:left;'>", htmltools::htmlEscape(validation_msg), "</pre>"),
              type  = "error",
              html  = TRUE,
              confirmButtonText = "OK"
            )
            return(invisible(NULL))
          }

          # Validation passed: commit, then merge.
          data$acm_rawdata <- raw_data_new
          data$acm_data <- acm_data_new
          data$event_data <- event_data_new

          incProgress(0.8, detail = "Merging data...")
          data$merged_data <- mergeEventsData(data$acm_data, data$event_data)
          data$data_description <- paste("Built-in", input$samplecountry, "data")
          validation_state(list(ok = TRUE))

          rv$total_prediction <- NULL
          rv$plot_time_data   <- NULL
          rv$summary_by_event <- NULL
          rv$overall_summary  <- NULL
          rv$model_aic        <- NULL
          rv$best_model       <- NULL

          shinyjs::html(ns("progress_builtin"), "Built-in data loaded successfully!")
        } else {
          shinyjs::html(ns("progress_builtin"), "Error: File not found.")
        }

        incProgress(0.2, detail = "Complete!")
      })

      shinyjs::delay(2000, shinyjs::hide(ns("progress_builtin")))
    })

    # Formatted preview tables (shared reactable helper).
    output$ACM_table <- reactable::renderReactable({
      req(data$merged_data)
      make_preview_table(data$merged_data)
    })

    # Show the tidy processed mortality data (real headers) instead of the
    # headerless raw read that previously rendered as an anonymous X1..Xn grid.
    output$raw_mortality_table <- reactable::renderReactable({
      make_preview_table(data$acm_data, placeholder = "No mortality data loaded yet.")
    })

    output$events_table <- reactable::renderReactable({
      make_preview_table(data$event_data, placeholder = "No events data loaded yet.")
    })

    # Excel-format download handlers
    output$downloadMergedData <- downloadHandler(
      filename = function() {
        paste("Merged_Data_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
      },
      content = function(file) {
        req(data$merged_data)
        openxlsx::write.xlsx(data$merged_data, file, rowNames = FALSE)
      }
    )

    output$downloadRawData <- downloadHandler(
      filename = function() {
        paste("Mortality_Data_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
      },
      content = function(file) {
        req(data$acm_data)
        openxlsx::write.xlsx(data$acm_data, file, rowNames = FALSE)
      }
    )

    output$downloadEventsData <- downloadHandler(
      filename = function() {
        paste("Events_Data_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
      },
      content = function(file) {
        req(data$event_data)
        openxlsx::write.xlsx(data$event_data, file, rowNames = FALSE)
      }
    )

    # --- Data health panel (replaces the old verbatim text dump) ------------
    output$health_cards <- renderUI({
      m <- health_metrics()
      card <- function(value, label, warn = FALSE) {
        bd <- if (warn) "#D55E00" else "#0072B2"
        bg <- if (warn) "#fbece4" else "#f2f8fc"
        div(style = paste0("border-left:4px solid ", bd, ";background:", bg,
                           ";padding:7px 12px;margin-bottom:8px;border-radius:4px;"),
            div(style = paste0("font-size:1.35em;font-weight:700;color:", bd, ";line-height:1.1;"), value),
            div(style = "font-size:0.78em;color:#555;", label))
      }
      tagList(
        card(format(m$n_records, big.mark = ","), "Data rows"),
        card(m$date_range, paste0("Date range · ", m$period_type)),
        card(format(round(m$total_deaths), big.mark = ","), "Total deaths (national total)"),
        card(if (is.na(m$n_events)) "–" else m$n_events, "Events annotated"),
        card(paste0(m$n_missing, if (m$n_missing > 0) sprintf(" (%.1f%%)", m$pct_missing) else ""),
             "Missing death counts", warn = m$n_missing > 0)
      )
    })

    output$health_spark <- plotly::renderPlotly({
      df <- national_total()
      req(nrow(df) > 0)
      weekly <- max(df$PERIOD, na.rm = TRUE) > 12
      s <- df %>%
        group_by(YEAR, PERIOD) %>%
        summarise(
          # NA (a gap in the line) for fully-missing periods, not a dip to 0.
          deaths = if (all(is.na(NO_DEATHS))) NA_real_ else sum(NO_DEATHS, na.rm = TRUE),
          event = any(event_index != 0), .groups = "drop") %>%
        arrange(YEAR, PERIOD) %>%
        mutate(
          t = row_number(),
          lbl = paste0(YEAR, if (weekly) paste0(" W", sprintf("%02d", PERIOD)) else paste0(" ", month.abb[PERIOD]),
                       ": ", format(round(deaths), big.mark = ","))
        )
      ev <- s[s$event, ]
      p <- plotly::plot_ly(s, x = ~t, y = ~deaths, type = "scatter", mode = "lines",
                           line = list(color = "#0072B2", width = 1.5),
                           hoverinfo = "text", text = ~lbl, name = "Deaths")
      if (nrow(ev) > 0) {
        p <- plotly::add_trace(p, data = ev, x = ~t, y = ~deaths, type = "scatter", mode = "markers",
                               marker = list(color = "#D55E00", size = 6),
                               hoverinfo = "text", text = ~lbl, name = "Event period")
      }
      p %>%
        plotly::layout(
          margin = list(l = 40, r = 8, t = 6, b = 16), showlegend = FALSE,
          xaxis = list(visible = FALSE),
          yaxis = list(title = "", tickformat = ",d", zeroline = FALSE),
          paper_bgcolor = "white", plot_bgcolor = "white",
          font = list(family = "Source Sans Pro, Segoe UI, Arial, sans-serif")
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    output$health_breakdown <- reactable::renderReactable({
      df <- as.data.frame(data$merged_data)
      # Guard on rows, not just non-NULL: req(df) passes on an empty data.frame
      # (isTruthy treats a non-NULL list as truthy), which left `grp` as
      # names(df)[1] = NA_character_ on a 0-column frame and crashed group_by().
      req(is.data.frame(df), nrow(df) > 0)
      # Breakdown by sex at the all-ages ("Total") level so subgroups are not
      # double-counted.
      if ("AGE_GROUP" %in% names(df) && "Total" %in% df$AGE_GROUP) df <- df[df$AGE_GROUP == "Total", ]
      grp <- if ("SEX" %in% names(df)) "SEX" else names(df)[1]
      tab <- df %>%
        group_by(.data[[grp]]) %>%
        summarise(Rows = n(),
                  `Total deaths` = sum(NO_DEATHS, na.rm = TRUE),
                  Mean = round(mean(NO_DEATHS, na.rm = TRUE)), .groups = "drop")
      reactable::reactable(
        tab, compact = TRUE, pagination = FALSE, sortable = FALSE, fullWidth = TRUE,
        columns = list(
          `Total deaths` = reactable::colDef(format = reactable::colFormat(separators = TRUE)),
          Mean = reactable::colDef(format = reactable::colFormat(separators = TRUE))
        ),
        defaultColDef = reactable::colDef(headerStyle = list(background = "#f7f7f7"))
      )
    })

    output$validation_panel <- renderUI({
      vs <- validation_state()
      if (is.null(vs)) return(NULL)
      if (isTRUE(vs$ok)) {
        div(class = "alert alert-success", style = "margin-top:10px;padding:8px 12px;",
            icon("check-circle"), " Data validated and merged successfully.")
      } else {
        div(class = "alert alert-danger", style = "margin-top:10px;padding:8px 12px;white-space:pre-wrap;",
            strong("Data validation failed:"), br(), vs$msg)
      }
    })

    # Template download handler
    output$download_templates <- downloadHandler(
      filename = function() {
        paste0(c(
          "Australia (empty template).xlsx", "Philippines (empty template).xlsx", "French Polynesia (empty template).xlsx",
          "Data Entry Template - monthly.xlsx", "Data Entry Template - weekly.xlsx",
          "Australia_built_in_data.xlsx", "Japan_built_in_data.xlsx",
          "Republic_of_Korea_built_in_data.xlsx", "New_Zealand_built_in_data.xlsx",
          "Philippines_built_in_data.xlsx"
        )[
          match(input$template_country, c(
            "Australia (empty template)", "Philippines (empty template)", "French Polynesia (empty template)",
            "Generic Monthly template", "Generic Weekly template",
            "Australia (filled up to August 2020)", "Japan (filled up to August 2020)",
            "Republic of Korea (filled up to August 2020)", "New Zealand (filled up to August 2020)",
            "Philippines (filled up to August 2020)"
          ))
        ])
      },
      content = function(file) {
        file.copy(
          from = paste0(
            "./XLSX/",
            # Portable (CRAN-safe) file names on disk; the download-as names
            # above keep the friendlier spaced labels.
            c(
              "Australia_empty_template.xlsx", "Philippines_empty_template.xlsx", "French_Polynesia_empty_template.xlsx",
              "Data_Entry_Template_monthly.xlsx", "Data_Entry_Template_weekly.xlsx",
              "Australia_built_in_data.xlsx", "Japan_built_in_data.xlsx",
              "Republic_of_Korea_built_in_data.xlsx", "New_Zealand_built_in_data.xlsx",
              "Philippines_built_in_data.xlsx"
            )[
              match(input$template_country, c(
                "Australia (empty template)", "Philippines (empty template)", "French Polynesia (empty template)",
                "Generic Monthly template", "Generic Weekly template",
                "Australia (filled up to August 2020)", "Japan (filled up to August 2020)",
                "Republic of Korea (filled up to August 2020)", "New Zealand (filled up to August 2020)",
                "Philippines (filled up to August 2020)"
              ))
            ]
          ),
          to = file
        )
      }
    )

  }) # End moduleServer
} # Add this closing bracket for the data_process_module_server function