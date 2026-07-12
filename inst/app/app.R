# app.R
# library(shiny)
library(DT)
library(openxlsx) # For reading Excel files
library(lubridate) # For date processing
library(dplyr) # For data operations (like distinct and left_join)
library(ggplot2) # For plotting
library(scales) # For formatting axis labels
library(plotly) # For interactive plots
library(ggrepel) # For labeling points in plots
library(readxl) # For reading Excel files
library(shinyjs)       # used by the About module
library(RColorBrewer)  # palette for event blocks in plots
library(shinycssloaders) # loading spinners on plot/table outputs

# Working and data directories
app_dir <- getwd()
data_dir <- file.path(app_dir, "data")

# Create the data directory on first run if it does not exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Created data directory: ", data_dir)
}

# Clear cached functions from the global env to avoid stale definitions
# carrying over across reloads during development.
rm(list = lsf.str())

# Note: date_midpoint() lives in modules/plot_prediction_module_new.R.

source("modules/utils.R", encoding = "UTF-8")
source("modules/ACM_hist_new.R", encoding = "UTF-8")
source("modules/ACM_nb.R", encoding = "UTF-8")
source("modules/ACM_quasipoisson.R", encoding = "UTF-8")
source("modules/ACM_zip.R", encoding = "UTF-8")
source("modules/data_process_module.R", encoding = "UTF-8")
source("modules/model_run_module.R", encoding = "UTF-8")
source("modules/model_summary_module.R", encoding = "UTF-8")
source("modules/report_module.R", encoding = "UTF-8")

# Lazy-load plot modules on first use. The rm() calls clear stale function
# definitions from earlier sourced versions to avoid name collisions between
# the three plot module files.
load_module <- function(module_name) {
  if (module_name == "plot_prediction" && !exists("plot_prediction_module_ui", mode = "function")) {
    rm(list = ls(pattern = "create_.*_plot"), envir = .GlobalEnv)
    rm(list = ls(pattern = "plot_prediction_module"), envir = .GlobalEnv)
    rm(list = ls(pattern = "date_midpoint"), envir = .GlobalEnv)
    source("modules/plot_prediction_module_new.R", encoding = "UTF-8")
    message("Loaded plot_prediction_module_new.R")
    return(TRUE)
  } else if (module_name == "plot_event" && !exists("plot_event_module_ui", mode = "function")) {
    source("modules/plot_event_module_new.R", encoding = "UTF-8")
    message("Loaded plot_event_module_new.R")
    return(TRUE)
  } else if (module_name == "plot_total" && !exists("plot_total_module_ui", mode = "function")) {
    source("modules/plot_total_module.R", encoding = "UTF-8")
    message("Loaded plot_total_module.R")
    return(TRUE)
  }
  return(FALSE)
}

# Non-plot module sources
source("modules/about_module.R", encoding = "UTF-8")
source("modules/methods_module.R", encoding = "UTF-8")
source("modules/help_resources_module.R", encoding = "UTF-8")
source("modules/best_fit_module.R", encoding = "UTF-8")

# Main UI: navbar layout with About as the landing page.
ui <- navbarPage(
  # title = "WPRO All Cause Mortality and Excess Mortality Calculator (2025)",
  title = "",
  id = "navbar",
  windowTitle = "WPRO All Cause Mortality and Excess Mortality Calculator (2025)",
  collapsible = TRUE,

  # JavaScript that lets the server send navigation messages to the client
  # and bubble nested tabsetPanel changes back as Shiny inputs.
  header = tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('navigateTab', function(tabName) {
        $('a[data-value=\"' + tabName + '\"]').click();
      });

      // Forward changes on nested tabsetPanels back to Shiny.
      $(document).on('change', '.shiny-bound-input', function() {
        if (this.id.indexOf('tabsetpanel') !== -1) {
          Shiny.setInputValue(this.id + '_changed', new Date().getTime());
        }
      });
    "))
  ),

  # About module serves as the landing page.
  tabPanel("WPRO All Cause Mortality and Excess Mortality Calculator (2025)", about_module_ui("about_mod"), value = "tab1"),
  tabPanel(
    "Data",
    useShinyjs(),
    data_process_module_ui("data_process_mod")
  ),
  tabPanel("Model", model_run_module_ui("model_run_mod"), value = "tab3"),
  tabPanel("Tables", model_summary_module_ui("model_summary_mod"), value = "tab4"),
  tabPanel("Best Fit", best_fit_module_ui("best_fit_mod"), value = "tab_best_fit"),
  tabPanel("Plots",
    value = "tab6",
    fluidRow(
      column(
        12,
        # Conditional panels guide users to upload data and run a model
        # before the plots are accessible.
        conditionalPanel(
          condition = "!output.has_merged_data",
          div(
            style = "text-align: center; margin-top: 50px;",
            h3("Data is not ready. Please process data first...", style = "color: #666;"),
            actionButton("goto_data_tab", "Go to Data tab",
              style = "margin-top: 20px; background-color: #337ab7; color: white;")
          )
        ),
        conditionalPanel(
          condition = "output.has_merged_data && !output.has_prediction_data",
          div(
            style = "text-align: center; margin-top: 50px;",
            h3("Expected Death Data is not ready. Please run model first...", style = "color: #666;"),
            actionButton("goto_model_tab", "Go to Model tab",
              style = "margin-top: 20px; background-color: #337ab7; color: white;")
          )
        ),
        conditionalPanel(
          condition = "output.has_merged_data && output.has_prediction_data",
          tabsetPanel(
            id = "plottabs",
            tabPanel(
              "Model prediction results", br(),
              uiOutput("plot_prediction_ui")
            ),
            tabPanel(
              "Event-specific excess mortality", br(),
              uiOutput("plot_event_ui"),
            ),
            tabPanel(
              "Total excess mortality", br(),
              uiOutput("plot_total_ui")
            )
          )
        )
      )
    )
  ),
  tabPanel("Report", report_module_ui("report_mod"), value = "tab5"),
  tabPanel(
    "Methods",
    methods_module_ui("methods_mod")
  ),
  tabPanel(
    "Help and Resources",
    help_resources_module_ui("help_resources_mod")
  ),
)

# Server
server <- function(input, output, session) {
  options(digits = 3)

  # Initialize reactive values shared across modules
  rv <- reactiveValues(
    acm_data = NULL,
    acm_rawdata = NULL,
    acm_temp = NULL,
    data_description = NULL,
    event_data = NULL,
    sheet_name = NULL,
    merged_data = NULL,
    merged_data_ext = NULL,
    total_prediction = NULL,
    plot_time_data = NULL,
    summary_by_event = NULL,
    overall_summary = NULL,
    model_aic = NULL,
    best_model = NULL
  )

  about_module_server("about_mod", rv)
  data_process_module_server("data_process_mod", rv)

  # When merged_data lands, derive merged_data_ext: the same data with
  # synthetic single-series identifiers and a DATE_TO_SPECIFY_WEEK column
  # for weekly inputs.
  observe({
    if (!is.null(rv$merged_data)) {
      message("Merged data is ready.")
      rv_data <- rv$merged_data
      rv_data$CAUSE <- "Total"
      rv_data$AREA <- "Total"
      rv_data$COUNTRY <- "AAA"
      rv_data$ISO3 <- "ISO"
      rv_data$SE_IDENTIFIER <- "END"

      # 1. Extract unique combinations of YEAR and PERIOD.
      #    merged_data_ext is the model INPUT (read only by model_run_module);
      #    keeping PERIOD <= 53 here is harmless but the seasonal models
      #    re-filter to <= 52 by design, so week 53 does not enter the excess
      #    estimates. Week 53 IS retained in the observed data (merged_data),
      #    so it appears in the data-health totals and data preview.
      #    get_week_start_date clamps any out-of-range week safely.
      unique_combinations <- rv_data %>%
        filter(PERIOD <= 53) %>%
        select(YEAR, PERIOD) %>%
        distinct()

      # Save current locale setting
      original_locale <- Sys.getlocale("LC_TIME")
      # Change locale setting to English
      Sys.setlocale("LC_TIME", "en_US.UTF-8")

      # 2. Batch calculate DATE_TO_SPECIFY_WEEK
      unique_combinations$DATE_TO_SPECIFY_WEEK <- apply(unique_combinations, 1, function(row) {
        target_date <- get_week_start_date(as.numeric(row["YEAR"]), as.numeric(row["PERIOD"]))
        format(target_date, "%b-%d")
      })

      # Restore original locale setting
      Sys.setlocale("LC_TIME", original_locale)

      # 3. Merge results back to original dataframe
      rv_data <- rv_data %>%
        filter(PERIOD <= 53) %>%
        left_join(unique_combinations, by = c("YEAR", "PERIOD"))

      rv$merged_data_ext <- rv_data
      # saveRDS(rv_data, "merged_data_ext.rds")
      # openxlsx::write.xlsx(rv_data, file = "merged_data_ext.xlsx", rowNames = FALSE)
    }
  })

  # Model run module returns its results reactive so the parent can react
  # to a completed model run if it ever needs to (currently only for logging).
  model_results <- model_run_module_server("model_run_mod", rv)

  observe({
    req(model_results$model_results())
    results <- model_results$model_results()
    if (!is.null(results)) {
      message("Model results updated successfully")
    }
  })

  # Call the model summary module server
  model_summary_module_server("model_summary_mod", rv)

  # Call the Best Fit module server
  best_fit_module_server("best_fit_mod", rv)

  # Call the report module server
  report_module_server("report_mod", rv)

  # Plot modules are lazy-loaded on first tab visit. Load state is tracked
  # so the matching server can be initialised once the UI is mounted.
  module_loaded <- reactiveValues(
    plot_prediction = FALSE,
    plot_event = FALSE,
    plot_total = FALSE
  )

  output$plot_prediction_ui <- renderUI({
    load_module("plot_prediction")
    module_loaded$plot_prediction <- TRUE
    plot_prediction_module_ui("plot_prediction_mod")
  })

  observe({
    req(module_loaded$plot_prediction)
    load_module("plot_prediction")
    plot_prediction_module_server("plot_prediction_mod", rv)
  })

  output$plot_event_ui <- renderUI({
    load_module("plot_event")
    module_loaded$plot_event <- TRUE
    plot_event_module_ui("plot_event_mod")
  })

  observe({
    req(module_loaded$plot_event)
    load_module("plot_event")
    plot_event_module_server("plot_event_mod", rv)
  })

  output$plot_total_ui <- renderUI({
    load_module("plot_total")
    module_loaded$plot_total <- TRUE
    plot_total_module_ui("plot_total_mod")
  })

  observe({
    req(module_loaded$plot_total)
    load_module("plot_total")
    plot_total_module_server("plot_total_mod", rv)
  })

  # Pre-load the matching plot module when its tab is selected.
  observeEvent(input$plottabs, {
    if (input$plottabs == "Model prediction results") {
      load_module("plot_prediction")
    } else if (input$plottabs == "Event-specific excess mortality") {
      load_module("plot_event")
    } else if (input$plottabs == "Total excess mortality") {
      load_module("plot_total")
    }
  }, ignoreInit = FALSE)

  methods_module_server("methods_mod", rv)
  help_resources_module_server("help_resources_mod", rv)

  # Output flags drive the conditionalPanel logic on the Plots tab.
  output$has_merged_data <- reactive({
    !is.null(rv$merged_data)
  })
  outputOptions(output, "has_merged_data", suspendWhenHidden = FALSE)

  output$has_prediction_data <- reactive({
    !is.null(rv$total_prediction)
  })
  outputOptions(output, "has_prediction_data", suspendWhenHidden = FALSE)

  # Cross-tab navigation buttons shown when prerequisite data is missing.
  observeEvent(input$goto_data_tab, {
    updateNavbarPage(session, "navbar", selected = "Data")
  })

  observeEvent(input$goto_model_tab, {
    # Tab name must match the tabPanel label exactly.
    updateNavbarPage(session, "navbar", selected = "Model")
    session$sendCustomMessage("navigateTab", "tab3")
  })
}

shinyApp(ui = ui, server = server)
