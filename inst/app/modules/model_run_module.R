# Load necessary R packages
library(shiny)
library(dplyr)
library(DT)
library(mgcv)
library(openxlsx)
library(reshape2)
library(readxl)
library(stringr)
library(shinyjs)     # progress-bar driver
library(shinyalert)  # modal dialogs for run results

# Load shared helpers and active model implementations.
source_module("modules/common_functions.R")
source_module("modules/ACM_hist_new.R")
source_module("modules/ACM_nb.R")
source_module("modules/ACM_quasipoisson.R")
source_module("modules/ACM_zip.R")
source_module("modules/ACM_arima.R")
source_module("modules/ACM_gam.R")
source_module("modules/ACM_karlinsky.R")

# Single source of truth for the model line-up. Each entry drives the UI
# checkbox, the run loop, the result aggregation, and the status table.
# Add a new model by appending one entry here and ensuring its fcn is
# loaded by a source() above.
MODELS <- list(
  list(
    key         = "arima",
    label       = "ARIMA/SARIMA Model",
    description = "Time series model capturing temporal dependencies and seasonal patterns in mortality data.",
    fcn         = fcn_arima,
    default     = FALSE
  ),
  list(
    key         = "hist",
    label       = "Historical Average",
    description = "Simple method using average deaths from baseline period to predict expected deaths.",
    fcn         = fcn_hist,
    default     = TRUE
  ),
  list(
    key         = "nb",
    label       = "Negative Binomial Regression",
    description = "Regression model for count data with overdispersion, handles variance greater than mean.",
    fcn         = fcn_nb,
    default     = TRUE
  ),
  list(
    key         = "quasipoisson",
    label       = "Quasi-Poisson Model",
    description = "Similar to Poisson regression but with an estimated dispersion parameter for overdispersed count data.",
    fcn         = fcn_quasipoisson,
    default     = FALSE
  ),
  list(
    key         = "zip",
    label       = "Zero Inflated Poisson Model",
    description = "Two-component model for count data with excess zeros, combining logistic and Poisson regression.",
    fcn         = fcn_zip,
    default     = FALSE
  ),
  list(
    key         = "gam",
    label       = "GAM Spline Model",
    description = "Negative-binomial GAM with a smooth long-term trend spline and a penalized cyclic seasonal spline; a flexible, modern expected-mortality baseline.",
    fcn         = fcn_gam,
    default     = FALSE
  ),
  list(
    key         = "karlinsky",
    label       = "Karlinsky-Kobak Model",
    description = "Internationally recognised reference baseline (World Mortality Dataset / OWID): period-of-year fixed effects plus a linear year trend.",
    fcn         = fcn_karlinsky,
    default     = FALSE
  )
)

# Lookups keyed by the user-facing label.
model_input_id  <- function(key)   paste0("model_", key)
model_by_label  <- function(label) Filter(function(m) m$label == label, MODELS)[[1]]
model_keys      <- vapply(MODELS, `[[`, character(1), "key")
model_labels    <- vapply(MODELS, `[[`, character(1), "label")

# Define UI interface
model_run_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(HTML(sprintf(
        "
        $(document).on('click', '#%s', function() {
          $('#%s').show();
        });
        ",
        ns("run"),
        ns("progress-area")
      )))
    ),
    tags$head(
      tags$style(HTML("
            .shiny-output-error { visibility: hidden; }
            .shiny-output-error:before { visibility: hidden; }

            /* Progress bar */
            .progress-container {
                margin-top: 10px;
                margin-bottom: 10px;
            }
            .progress {
                height: 20px;
            }
            .progress-bar {
                transition: width 0.3s ease;
            }
            #progress-message {
                margin-top: 5px;
                font-weight: bold;
            }
        "))
    ),
    conditionalPanel(
      condition = "!output.has_data",
      ns = ns,
      div(
        class = "data-not-ready",
        style = "text-align: center; margin-top: 50px;",
        h3("Data is not ready. Please process data first...", style = "color: #666;"),
        br(),
        actionButton(ns("goto_data_tab"), "Go to Data tab",
          style = "margin-top: 20px; background-color: #337ab7; color: white;"
        )
      )
    ),
    conditionalPanel(
      condition = "output.has_data",
      ns = ns,
      sidebarLayout(
        mainPanel(
          width = 8,
          wellPanel(
            h4("Data Filtering"),
            fluidRow(
              column(width = 6,
                h5("Sex:"),
                div(style = "margin-bottom: 10px;",
                  div(style = "display: flex; gap: 10px;",
                    actionButton(ns("select_all_sex"), "Select All", class = "btn-sm", style = "flex: 1;"),
                    actionButton(ns("clear_all_sex"), "Clear All", class = "btn-sm", style = "flex: 1;")
                  )
                ),
                uiOutput(ns("sex_selector"))
              ),
              column(width = 6,
                h5("Age Group:"),
                div(style = "margin-bottom: 10px;",
                  div(style = "display: flex; gap: 10px;",
                    actionButton(ns("select_all_age"), "Select All", class = "btn-sm", style = "flex: 1;"),
                    actionButton(ns("clear_all_age"), "Clear All", class = "btn-sm", style = "flex: 1;")
                  )
                ),
                uiOutput(ns("age_group_selector"))
              )
            )
          )
        ),
        sidebarPanel(
          width = 4,
          wellPanel(
            h4("Model Selection"),
            tags$style(HTML("
              .model-option {
                margin-bottom: 12px;
              }
              .model-description {
                font-size: 12px;
                color: #666;
                margin-left: 20px;
                font-style: italic;
                display: block;
              }
            ")),
            div(style = "margin-bottom: 15px;",
              div(style = "display: flex; gap: 10px;",
                actionButton(ns("select_all"), "Select All", class = "btn-sm", style = "flex: 1;"),
                actionButton(ns("clear_all"), "Clear All", class = "btn-sm", style = "flex: 1;")
              )
            ),
            do.call(div, lapply(MODELS, function(m) {
              div(class = "model-option",
                checkboxInput(ns(model_input_id(m$key)), m$label),
                tags$span(class = "model-description", m$description)
              )
            })),
            br(),
            actionButton(ns("run"), "Run Model", class = "btn-primary btn-block"),
            br(),
            hidden(
              div(
                id = ns("progress-area"), class = "progress-container",
                div(
                  class = "progress",
                  div(
                    id = ns("progress-bar"), class = "progress-bar progress-bar-striped active",
                    role = "progressbar", style = "width: 0%", `aria-valuenow` = "0",
                    `aria-valuemin` = "0", `aria-valuemax` = "100"
                  )
                ),
                div(
                  id = ns("progress-message"), class = "text-center",
                  textOutput(ns("progress_text"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Define server logic
model_run_module_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output$has_data <- reactive({
        !is.null(rv$merged_data)
      })
      outputOptions(output, "has_data", suspendWhenHidden = FALSE)

      # Apply per-model default values once at startup.
      observe({
        for (m in MODELS) {
          updateCheckboxInput(session, model_input_id(m$key), value = m$default)
        }
      })

      observeEvent(input$select_all, {
        for (m in MODELS) {
          updateCheckboxInput(session, model_input_id(m$key), value = TRUE)
        }
      })

      observeEvent(input$clear_all, {
        for (m in MODELS) {
          updateCheckboxInput(session, model_input_id(m$key), value = FALSE)
        }
      })

      # utils.R is sourced once at app start by app.R; no per-session reload needed.

      rv_data <- reactive({
        req(rv$merged_data_ext)
        return(rv$merged_data_ext)
      })

      unique_sex <- reactive({
        req(rv_data())
        unique(rv_data()$SEX)
      })

      unique_age_group <- reactive({
        req(rv_data())
        unique(rv_data()$AGE_GROUP)
      })

      output$sex_selector <- renderUI({
        req(unique_sex())
        choices <- unique_sex()
        selected <- if ("Total" %in% choices) "Total" else choices
        checkboxGroupInput(session$ns("sex"), NULL,
          choices = choices,
          selected = selected
        )
      })

      output$age_group_selector <- renderUI({
        req(unique_age_group())
        choices <- unique_age_group()
        selected <- if ("Total" %in% choices) "Total" else choices
        checkboxGroupInput(session$ns("age_group"), NULL,
          choices = choices,
          selected = selected
        )
      })

      observeEvent(input$select_all_sex, {
        req(unique_sex())
        updateCheckboxGroupInput(session, "sex", selected = unique_sex())
      })

      observeEvent(input$clear_all_sex, {
        updateCheckboxGroupInput(session, "sex", selected = character(0))
      })

      observeEvent(input$select_all_age, {
        req(unique_age_group())
        updateCheckboxGroupInput(session, "age_group", selected = unique_age_group())
      })

      observeEvent(input$clear_all_age, {
        updateCheckboxGroupInput(session, "age_group", selected = character(0))
      })

      filtered_data <- reactive({
        req(rv_data(), input$sex, input$age_group)
        rv_data() %>%
          filter(
            SEX %in% input$sex,
            AGE_GROUP %in% input$age_group
          )
      })

      progress_value <- reactiveVal(0)
      progress_message <- reactiveVal("")

      output$progress_text <- renderText({
        progress_message()
      })

      updateProgress <- function(value, message) {
        progress_value(value)
        progress_message(message)
        shinyjs::runjs(sprintf(
          "$('#%s').css('width', '%s%%').attr('aria-valuenow', %s);",
          session$ns("progress-bar"), value, value
        ))
      }

      # Backwards-compatible label -> key map kept as a named list because
      # downstream code still indexes by label.
      model_result_keys <- setNames(as.list(model_keys), model_labels)

      get_selected_models <- reactive({
        Filter(Negate(is.null), lapply(MODELS, function(m) {
          if (isTRUE(input[[model_input_id(m$key)]])) m$label else NULL
        })) |> unlist(use.names = FALSE)
      })

      model_results <- eventReactive(input$run, {
        selected_models <- get_selected_models()
        message("Selected models: ", paste(selected_models, collapse = ", "))
        shinyjs::show(id = "progress-area")
        shinyjs::disable("run")
        updateProgress(10, "Preparing data...")
        req(filtered_data())
        data <- filtered_data()

        if (nrow(data) == 0) {
          shinyjs::html("progress-message", "Error: No data available after filtering")
          shinyjs::delay(1500, {
            shinyjs::hide("progress-area")
            shinyjs::enable("run")
          })
          return(NULL)
        }

        results <- list()
        num_selected_models <- length(selected_models)
        progress_increment <- if (num_selected_models > 0) 80 / num_selected_models else 0 # 10% for prep, 10% for final processing
        current_progress <- 10

        # Run each selected model. Errors are caught per-model so one
        # failure does not abort the whole run; the failure message is
        # attached as an attribute and surfaced in the status table.
        for (m in MODELS) {
          if (!(m$label %in% selected_models)) next

          current_progress <- current_progress + progress_increment / 2
          updateProgress(floor(current_progress), paste0("Running ", m$label, "..."))
          message("\n[INFO] Running ", m$label, "...")
          flush.console()

          results[[m$key]] <- tryCatch({
            result <- m$fcn(data)
            if (is.null(result) || !is.data.frame(result)) {
              dplyr::tibble()
            } else {
              dplyr::as_tibble(result)
            }
          }, error = function(e) {
            message("Error in ", m$label, ": ", e$message)
            structure(dplyr::tibble(), error = e$message)
          })

          current_progress <- current_progress + progress_increment / 2
          updateProgress(floor(current_progress), paste0(m$label, " completed"))
        }

        updateProgress(95, "Processing results...")

        # Check if any model produced results
        successful_models_exist <- any(sapply(results, function(res) !is.null(res) && (is.data.frame(res) && nrow(res) > 0 || is.list(res) && !is.null(res$predictions))))

        # Helper: did this model produce a usable predictions table?
        model_succeeded <- function(res) {
          !is.null(res) && (
            (is.data.frame(res) && nrow(res) > 0) ||
            (is.list(res) && !is.null(res$predictions))
          )
        }

        if (successful_models_exist) {
          message("\n[DEBUG] Results summary:")
          for (m in MODELS) {
            if (m$label %in% selected_models) {
              message(m$label, ": ", model_succeeded(results[[m$key]]))
            }
          }

          success_count <- sum(vapply(MODELS, function(m) {
            m$label %in% selected_models && model_succeeded(results[[m$key]])
          }, logical(1)))
          total_count <- length(selected_models)

          model_status_rows <- sapply(selected_models, function(model_name) {
            result_key_name <- model_result_keys[[model_name]]
            res <- results[[result_key_name]]
            ok  <- model_succeeded(res)
            err <- attr(res, "error")
            status_html <- if (ok) {
              "<span style='color:green; font-weight:bold;'>&#10003; Success</span>"
            } else if (!is.null(err)) {
              paste0(
                "<span style='color:red; font-weight:bold;'>&#10007; Failed</span>",
                "<div style='font-size:11px; color:#666; margin-top:4px;'>",
                htmltools::htmlEscape(err),
                "</div>"
              )
            } else {
              "<span style='color:red; font-weight:bold;'>&#10007; Failed</span>"
            }
            paste(
              "<tr>",
              "<td style='padding:8px; text-align:left; border:1px solid #ddd;'>", model_name, "</td>",
              "<td style='padding:8px; text-align:left; border:1px solid #ddd;'>", status_html, "</td>",
              "</tr>"
            )
          })
          
          model_results_table <- paste(
            "<div style='text-align:center; margin-bottom:15px;'>",
            "<span style='font-size:18px; font-weight:bold;'>", success_count, " of ", total_count, " models completed successfully</span>",
            "</div>",
            "<table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>",
            "<tr style='background-color:#f2f2f2;'>",
            "<th style='padding:8px; text-align:left; border:1px solid #ddd;'>Model</th>",
            "<th style='padding:8px; text-align:center; border:1px solid #ddd;'>Status</th>",
            "</tr>",
            paste(model_status_rows, collapse=""),
            "</table>",
            "<div style='text-align:center; font-style:italic; color:#666;'>",
            "This window will close automatically in 15 seconds.",
            "</div>"
          )

          shinyalert::shinyalert(
            title = "<span style='color:#2c3e50;'>Model Execution Results</span>",
            text = model_results_table,
            type = "success", # Or "info" if some failed
            html = TRUE,
            confirmButtonText = "OK",
            timer = 15000,
            animation = TRUE,
            size = "m",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE
          )

          combined_results <- NULL
          for (method in selected_models) {
            result_key <- model_result_keys[[method]]
            if (!is.null(result_key) && !is.null(results[[result_key]])) {
              current_model_output <- results[[result_key]]
              if (is.data.frame(current_model_output) && nrow(current_model_output) > 0) {
                 message("[INFO] Processing results for: ", method)
                 current_results_df <- dplyr::mutate(current_model_output, Model = method)
                 combined_results <- dplyr::bind_rows(combined_results, current_results_df)
              } else if (is.list(current_model_output) && !is.null(current_model_output$predictions) && is.data.frame(current_model_output$predictions) && nrow(current_model_output$predictions) > 0) {
                 message("[INFO] Processing list-based predictions for: ", method)
                 current_results_df <- dplyr::mutate(current_model_output$predictions, Model = method)
                 combined_results <- dplyr::bind_rows(combined_results, current_results_df)
              } else {
                 message("[WARNING] No valid data frame or predictions found for model: ", method)
              }
            }
          }
          
          if (!is.null(combined_results) && nrow(combined_results) > 0) {
            possible_integer_cols <- c("NO_DEATHS", "EXP_DEATHS", "ESTIMATE", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS",
                                     "TOTAL_EXPECTED", "TOTAL_EXCESS", "TOTAL_SE", "EXCESS_LOWER", "EXCESS_UPPER")
            existing_integer_cols <- intersect(names(combined_results), possible_integer_cols)
            
            combined_results <- combined_results %>%
              mutate(across(all_of(existing_integer_cols), as.numeric))

            if ("P_SCORE" %in% names(combined_results)) {
              combined_results <- combined_results %>% mutate(P_SCORE = round(as.numeric(P_SCORE), 2))
              if ("P_LOWER" %in% names(combined_results)) combined_results <- combined_results %>% mutate(P_LOWER = round(as.numeric(P_LOWER), 2))
              if ("P_UPPER" %in% names(combined_results)) combined_results <- combined_results %>% mutate(P_UPPER = round(as.numeric(P_UPPER), 2))
            } else if (all(c("EXCESS_DEATHS", "EXP_DEATHS") %in% names(combined_results))) {
              combined_results <- combined_results %>%
                mutate(P_SCORE = round(100 * EXCESS_DEATHS / EXP_DEATHS, 2))
              if (all(c("LOWER_LIMIT", "UPPER_LIMIT", "NO_DEATHS") %in% names(combined_results))) {
                 combined_results <- combined_results %>%
                   mutate(
                     # P-score CI: propagate the uncertainty in EXPECTED deaths
                     # (LOWER_LIMIT..UPPER_LIMIT) through P = 100*(observed -
                     # expected)/expected. A higher expected bound gives the
                     # lower P-score and vice versa, so the limits cross over.
                     # The old form 100*(LIMIT - EXP)/EXP ignored observed
                     # deaths entirely and produced an interval around 0 that
                     # did not even contain the P_SCORE point estimate.
                     P_LOWER = round(100 * (NO_DEATHS - UPPER_LIMIT) / EXP_DEATHS, 2),
                     P_UPPER = round(100 * (NO_DEATHS - LOWER_LIMIT) / EXP_DEATHS, 2)
                   )
              }
            }
            
            combined_results <- combined_results %>%
              mutate(across(all_of(existing_integer_cols), round))

            cols_to_remove <- intersect(
              names(combined_results),
              c("COUNTRY", "ISO3", "AREA", "CAUSE", "DATE_TO_SPECIFY_WEEK", "SE_IDENTIFIER")
            )
            if (length(cols_to_remove) > 0) {
              combined_results <- combined_results %>% select(-all_of(cols_to_remove))
            }
            
            # Define desired column order, ensuring all columns exist
            desired_cols <- c("WM_IDENTIFIER", "YEAR", "PERIOD", "SEX", "AGE_GROUP", 
                              "NO_DEATHS", "EXP_DEATHS", "LOWER_LIMIT", "UPPER_LIMIT", 
                              "EXCESS_DEATHS", "P_SCORE", "P_LOWER", "P_UPPER", # Added P_LOWER, P_UPPER
                              "event_index", "event_name", "SERIES", "Model")
            final_cols <- character(0)
            for(col_name in desired_cols){
                if(col_name %in% names(combined_results)){
                    final_cols <- c(final_cols, col_name)
                }
            }
            # Add any remaining columns not in desired_cols (e.g. if new ones are added by models)
            remaining_cols <- setdiff(names(combined_results), final_cols)
            final_cols <- c(final_cols, remaining_cols)

            combined_results <- combined_results %>% select(all_of(final_cols))
            
            rv$total_prediction <- combined_results
          } else {
             rv$total_prediction <- dplyr::tibble() # Ensure it's an empty tibble if no results
          }


          # Process summary_by_event and overall_summary similarly
          # For summary_by_event
          all_summaries_by_event <- list()
          for (method in selected_models) {
            result_key <- model_result_keys[[method]]
            if (!is.null(result_key) && !is.null(results[[result_key]])) {
                current_model_output <- results[[result_key]]
                # Check if 'summary_by_event' attribute exists and is a data frame
                if (!is.null(attr(current_model_output, "summary_by_event")) && is.data.frame(attr(current_model_output, "summary_by_event"))) {
                    summary_df <- attr(current_model_output, "summary_by_event")
                    if (nrow(summary_df) > 0) {
                        all_summaries_by_event[[method]] <- dplyr::mutate(summary_df, Model = method)
                    }
                } else if (is.list(current_model_output) && !is.null(current_model_output$summary_by_event) && is.data.frame(current_model_output$summary_by_event)) {
                    # Handle cases where summary_by_event is a direct list component (e.g. from ARIMA)
                    summary_df <- current_model_output$summary_by_event
                     if (nrow(summary_df) > 0) {
                        all_summaries_by_event[[method]] <- dplyr::mutate(summary_df, Model = method)
                    }
                }
            }
          }
          if (length(all_summaries_by_event) > 0) {
            combined_summary <- dplyr::bind_rows(all_summaries_by_event)
            # Apply formatting similar to combined_results
            possible_integer_cols_sum <- c("observed", "expected", "excess", "excess_lower", "excess_upper", "TOTAL_EXPECTED", "TOTAL_EXCESS", "TOTAL_SE", "EXCESS_LOWER", "EXCESS_UPPER")
            possible_decimal_cols_sum <- c("p_score", "p_score_lower", "p_score_upper", "P_SCORE")
            
            existing_integer_cols_sum <- intersect(names(combined_summary), possible_integer_cols_sum)
            existing_decimal_cols_sum <- intersect(names(combined_summary), possible_decimal_cols_sum)

            if (length(existing_integer_cols_sum) > 0) {
              combined_summary <- combined_summary %>%
                mutate(across(all_of(existing_integer_cols_sum), ~ round(as.numeric(.))))
            }
            if (length(existing_decimal_cols_sum) > 0) {
              combined_summary <- combined_summary %>%
                mutate(across(all_of(existing_decimal_cols_sum), ~ round(as.numeric(.), 2)))
            }
            rv$summary_by_event <- combined_summary
          } else {
            rv$summary_by_event <- dplyr::tibble()
          }

          # For overall_summary
          all_overall_summaries <- list()
            for (method in selected_models) {
                result_key <- model_result_keys[[method]]
                if (!is.null(result_key) && !is.null(results[[result_key]])) {
                    current_model_output <- results[[result_key]]
                    if (!is.null(attr(current_model_output, "overall_summary")) && is.data.frame(attr(current_model_output, "overall_summary"))) {
                        overall_df <- attr(current_model_output, "overall_summary")
                        if (nrow(overall_df) > 0) {
                           all_overall_summaries[[method]] <- dplyr::mutate(overall_df, Model = method)
                        }
                    } else if (is.list(current_model_output) && !is.null(current_model_output$overall_summary) && is.data.frame(current_model_output$overall_summary)) {
                        overall_df <- current_model_output$overall_summary
                        if (nrow(overall_df) > 0) {
                           all_overall_summaries[[method]] <- dplyr::mutate(overall_df, Model = method)
                        }
                    }
                }
            }
          if (length(all_overall_summaries) > 0) {
            combined_overall <- dplyr::bind_rows(all_overall_summaries)
            # Apply formatting
            if (length(existing_integer_cols_sum) > 0) { # Reusing col definitions from summary_by_event
              combined_overall <- combined_overall %>%
                mutate(across(all_of(intersect(names(combined_overall), existing_integer_cols_sum)), ~ round(as.numeric(.))))
            }
            if (length(existing_decimal_cols_sum) > 0) {
              combined_overall <- combined_overall %>%
                mutate(across(all_of(intersect(names(combined_overall), existing_decimal_cols_sum)), ~ round(as.numeric(.), 2)))
            }
            rv$overall_summary <- combined_overall
          } else {
            rv$overall_summary <- dplyr::tibble()
          }

          # Build the per-model AIC table consumed by the Best Fit tab.
          # Reads the "aic" attribute attached by each model's fcn_*().
          aic_rows <- lapply(MODELS, function(m) {
            res <- results[[m$key]]
            if (!model_succeeded(res)) return(NULL)
            aic_val <- attr(res, "aic")
            if (is.null(aic_val)) aic_val <- NA_real_
            data.frame(
              Model = m$label,
              AIC   = as.numeric(aic_val),
              stringsAsFactors = FALSE
            )
          })
          aic_rows <- Filter(Negate(is.null), aic_rows)
          if (length(aic_rows) > 0) {
            aic_df <- do.call(rbind, aic_rows)
            # Lowest AIC wins; rows with NA (e.g. Historical Average) are
            # not eligible for "Best".
            valid_aic <- aic_df$AIC[is.finite(aic_df$AIC)]
            best_label <- if (length(valid_aic) > 0) {
              aic_df$Model[which(aic_df$AIC == min(valid_aic, na.rm = TRUE))[1]]
            } else NA_character_
            aic_df$Best <- aic_df$Model == best_label & !is.na(best_label)
            rv$model_aic  <- aic_df
            rv$best_model <- best_label
          } else {
            rv$model_aic  <- dplyr::tibble()
            rv$best_model <- NA_character_
          }

          message("Model results updated successfully in rv")
        } else {
            message("No models produced valid results. rv will not be updated with predictions.")
            rv$total_prediction <- dplyr::tibble()
            rv$summary_by_event <- dplyr::tibble()
            rv$overall_summary <- dplyr::tibble()
            rv$model_aic       <- dplyr::tibble()
            rv$best_model      <- NA_character_
             shinyalert::shinyalert(
                title = "No Model Results",
                text = "None of the selected models produced any output. Please check your data and model configurations.",
                type = "warning",
                confirmButtonText = "OK"
            )
        }
        
        updateProgress(100, "All calculations completed!")
        shinyjs::delay(1500, {
          shinyjs::hide("progress-area")
          shinyjs::enable("run")
        })
        return(results)
      })

      observeEvent(input$goto_data_tab, {
        session$sendCustomMessage("navigateTab", "Data")
      })

      return(list(
        model_results = model_results
      ))
    }
  )
}
