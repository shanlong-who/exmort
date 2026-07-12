# Report Module
# Generates an in-app HTML preview of the report and renders downloads
# in HTML, PDF, or Word (officedown-docx) formats from a shared Rmd
# template at modules/report_template.Rmd.

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(htmltools)
library(rmarkdown)
library(scales)
library(RColorBrewer)
library(base64enc)
library(shinyalert)
# officedown is loaded namespaced (officedown::rdocx_document) so the app
# still starts when officedown is not installed; the docx download path
# surfaces a clear error in that case.

# Source plotting functions
source("modules/plot_functions_report.R")

# Define UI interface for report module
report_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Add necessary CSS
    tags$head(
      tags$style(HTML("
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }

        /* 数据未就绪提示样式 */
        .data-not-ready {
            text-align: center;
            padding: 50px;
            color: #666;
            background: #f9f9f9;
            border-radius: 5px;
            margin: 20px;
        }

        /* 报告生成按钮样式 */
        .report-btn {
            margin-top: 20px;
            margin-bottom: 20px;
        }
      "))
    ),

    # 添加数据检查条件面板
    conditionalPanel(
      condition = "!output.has_model_data",
      ns = ns,
      div(
        class = "data-not-ready",
        style = "text-align: center; margin-top: 50px;",
        h3("Model results are not available. Please run the model first...", style = "color: #666;"),
        br(),
        actionButton(ns("goto_model_tab"), "Go to Model tab",
          style = "margin-top: 20px; background-color: #337ab7; color: white;"
        )
      )
    ),

    # 原有的UI内容放在条件面板中
    conditionalPanel(
      condition = "output.has_model_data",
      ns = ns,
      fluidRow(
        # Left side for report options
        column(
          3,
          wellPanel(
            h4("Report Options"),

            # Sex filter
            uiOutput(ns("sex_filter")),

            # Age group filter
            uiOutput(ns("age_group_filter")),

            # Model filter
            uiOutput(ns("model_filter")),

            # Event filter
            uiOutput(ns("event_filter")),

            # Report sections to include
            h4("Report Sections"),
            checkboxInput(ns("include_summary"), "Include Summary", value = TRUE),
            checkboxInput(ns("include_tables"), "Include Tables", value = TRUE),
            checkboxInput(ns("include_plots"), "Include Plots", value = TRUE),

            # Output format
            h4("Download Format"),
            radioButtons(ns("output_format"), NULL,
              choices = c(
                "HTML"        = "html",
                "PDF"         = "pdf",
                "Word (docx)" = "docx"
              ),
              selected = "html",
              inline = TRUE
            ),
            tags$small(
              "PDF requires LaTeX (e.g. tinytex::install_tinytex()). ",
              "Word uses officedown.",
              style = "color: #666;"
            ),

            # Generate report button
            div(class = "report-btn",
                actionButton(ns("generate_report"), "Generate Report",
                             class = "btn-primary btn-block")
            ),

            # Download report button (only shown after report is generated)
            conditionalPanel(
              condition = "output.report_ready",
              ns = ns,
              downloadButton(ns("download_report"), "Download Report",
                             class = "btn-success btn-block")
            )
          )
        ),

        # Right side for report preview
        column(
          9,
          wellPanel(
            h4("Report Preview"),
            htmlOutput(ns("report_preview"))
          )
        )
      )
    )
  )
}

# Define server logic for report module
report_module_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      # 添加数据状态检查
      output$has_model_data <- reactive({
        !is.null(rv$total_prediction) && !is.null(rv$summary_by_event) && !is.null(rv$overall_summary)
      })
      outputOptions(output, "has_model_data", suspendWhenHidden = FALSE)

      # 报告生成状态
      report_generated <- reactiveVal(FALSE)

      # 报告内容
      report_content <- reactiveVal(NULL)

      # 报告文件路径
      report_file_path <- reactiveVal(NULL)

      # 报告是否准备好
      output$report_ready <- reactive({
        report_generated()
      })
      outputOptions(output, "report_ready", suspendWhenHidden = FALSE)

      # 动态生成过滤器
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

      # 过滤数据
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

        # 如果选择了特定事件，则进一步过滤
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


      # --- Report parameters + rendering -----------------------------------
      # A single Rmd source (modules/report_template.Rmd) drives the HTML preview
      # AND all three downloads (html/pdf/docx), so every format is identical and
      # carries the same embedded figures, branding, and disclaimer placeholder.
      report_params <- reactiveVal(NULL)

      assemble_report_params <- function() {
        figs <- if (isTRUE(input$include_plots)) {
          build_report_figures(filtered_total_predictions(), input$selected_models)
        } else {
          list(acm = NA_character_, excess = NA_character_, pscore = NA_character_)
        }
        logo <- normalizePath("www/WHO-WPRO_Logo_PMS_2925.png", mustWork = FALSE)
        list(
          total_predictions = filtered_total_predictions(),
          summary_by_event  = filtered_summary_by_event(),
          overall_summary   = filtered_overall_summary(),
          model_aic         = if (!is.null(rv$model_aic)) rv$model_aic else data.frame(),
          best_model        = if (!is.null(rv$best_model)) rv$best_model else NA_character_,
          filter_sex        = input$selected_sex,
          filter_age_group  = input$selected_age_group,
          selected_models   = input$selected_models,
          selected_event    = if (is.null(input$selected_event)) "All" else input$selected_event,
          include_summary   = isTRUE(input$include_summary),
          include_tables    = isTRUE(input$include_tables),
          include_plots     = isTRUE(input$include_plots),
          fig_acm           = figs$acm,
          fig_excess        = figs$excess,
          fig_pscore        = figs$pscore,
          logo_path         = if (file.exists(logo)) logo else NA_character_,
          generated_on      = format(Sys.time(), "%Y-%m-%d %H:%M")
        )
      }

      render_report <- function(out_format, params) {
        tmp_rmd <- tempfile(fileext = ".Rmd")
        file.copy("modules/report_template.Rmd", tmp_rmd, overwrite = TRUE)
        rmarkdown::render(
          input = tmp_rmd, output_format = out_format,
          params = params, envir = new.env(), quiet = TRUE
        )
      }

      html_format <- function() {
        rmarkdown::html_document(toc = TRUE, toc_float = TRUE,
                                 self_contained = TRUE, theme = "cosmo")
      }

      # Invalidate a previously generated report whenever the filters or the
      # underlying model results change, so the Download button can never emit
      # a stale snapshot. report_params() is reset to NULL, so the download
      # handler re-assembles fresh params from the current state if the user
      # downloads without regenerating.
      observeEvent(
        list(input$selected_sex, input$selected_age_group, input$selected_models,
             input$selected_event, input$include_summary, input$include_tables,
             input$include_plots, rv$total_prediction),
        {
          report_generated(FALSE)
          report_params(NULL)
          report_content(NULL)
        },
        ignoreInit = TRUE
      )

      # Generate: build figures + render the HTML preview from the template.
      observeEvent(input$generate_report, {
        req(filtered_overall_summary())
        withProgress(message = "Generating report...", value = 0, {
          incProgress(0.3, detail = "Building figures...")
          params <- assemble_report_params()
          report_params(params)

          incProgress(0.4, detail = "Rendering preview...")
          html_out <- tryCatch(
            render_report(html_format(), params),
            error = function(e) { message("HTML preview render failed: ", conditionMessage(e)); NULL }
          )
          if (!is.null(html_out) && file.exists(html_out)) {
            report_content(paste(readLines(html_out, warn = FALSE), collapse = "\n"))
            report_generated(TRUE)
          } else {
            report_generated(FALSE)
            shinyalert::shinyalert(
              title = "Could not render preview",
              text  = "The report could not be generated. See the app logs for details.",
              type  = "error"
            )
          }
          incProgress(1, detail = "Done")
        })
      })

      # Preview: isolate the self-contained report HTML in an iframe so its own
      # <head>/styles do not clash with the app.
      output$report_preview <- renderUI({
        if (report_generated() && !is.null(report_content())) {
          tags$iframe(
            srcdoc = report_content(),
            style  = "width:100%; height:800px; border:1px solid #e5e5e5; border-radius:4px; background:white;"
          )
        } else {
          HTML("<div style='text-align:center; margin-top:50px;'><h3>Click 'Generate Report' to build a report from your selected options.</h3></div>")
        }
      })

      # Download: render the same template to the chosen format.
      output$download_report <- downloadHandler(
        filename = function() {
          ext <- switch(input$output_format, html = "html", pdf = "pdf", docx = "docx", "html")
          paste0("Excess_Mortality_Report_", format(Sys.Date(), "%Y-%m-%d"), ".", ext)
        },
        content = function(file) {
          fmt    <- input$output_format
          params <- report_params()
          if (is.null(params)) params <- assemble_report_params()

          out_format <- switch(
            fmt,
            html = html_format(),
            pdf  = rmarkdown::pdf_document(toc = TRUE, latex_engine = "xelatex"),
            docx = officedown::rdocx_document(),
            stop("Unsupported format: ", fmt)
          )

          rendered <- tryCatch(
            render_report(out_format, params),
            error = function(e) {
              shinyalert::shinyalert(
                title = paste0("Could not render ", toupper(fmt), " report"),
                text  = paste0(
                  "<pre style='text-align:left;'>", htmltools::htmlEscape(conditionMessage(e)),
                  if (fmt == "pdf") "\n\nIf this mentions LaTeX, run tinytex::install_tinytex()." else "",
                  if (fmt == "docx") "\n\nIf this mentions officedown, run install.packages('officedown')." else "",
                  "</pre>"),
                type = "error", html = TRUE
              )
              NULL
            }
          )
          if (!is.null(rendered) && file.exists(rendered)) file.copy(rendered, file, overwrite = TRUE)
        }
      )

      # 在 moduleServer 函数内部添加导航处理器
      observeEvent(input$goto_model_tab, {
        session$sendCustomMessage("navigateTab", "Model")
      })
    }
  )
}