# --- 1. Load Libraries ---
# Libraries are assumed to be loaded in the main app.R or global.R
# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(DT)
# library(lubridate)
# library(scales)
# library(shinyjs)

# Note: this module used to set LC_TIME to "English" at load time and never
# restored it. Nothing here formats month or day names, so the call was both
# unnecessary and a change to the user's session settings, which CRAN policy
# does not allow. Where English month labels are genuinely needed (app.R), the
# locale is switched inside a function that restores it with on.exit().

# --- Module UI ---
# plot_total_module_ui <- function(id) {
plot_event_module_ui <- function(id) {
    ns <- NS(id)
    
    # Prepare Initial Filter Options reactively in server, define UI structure here
    tagList(
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Data Filtering"),
                # Use uiOutput for dynamic choices based on loaded data
                uiOutput(ns("sex_filter_ui")),
                uiOutput(ns("age_group_filter_ui")),
                uiOutput(ns("model_filter_ui")),
                uiOutput(ns("event_filter_ui")),
                hr(),
                h4("Facet Options"),
                checkboxGroupInput(ns("facet_variables"), "Select Facet Variables (Max 2):",
                    choices = c("Sex" = "SEX", "Age Group" = "AGE_GROUP","Event" = "event_name", "Model" = "Model"),
                    selected = c("SEX", "AGE_GROUP")
                ),
                conditionalPanel(
                    condition = paste0("input['", ns("facet_variables"), "'] && input['", ns("facet_variables"), "'].length > 2"),
                    tags$div(style = "color: red;", "Maximum of 2 facet variables allowed!")
                ),
                hr(),
                h4("Inner Grouping Options"),
                selectInput(ns("primary_group_var"), "Primary Inner Grouping Variable (for Fill Color):",
                    choices = c("Auto Select" = "auto", "Sex" = "SEX", "Age Group" = "AGE_GROUP", "Event" = "event_name","Model" = "Model"),
                    selected = "auto"
                ),
                selectInput(ns("secondary_group_var"), "Secondary Inner Grouping Variable (for X-axis Grouping):",
                    choices = c("Auto Select" = "auto", "None" = "none", "Sex" = "SEX", "Age Group" = "AGE_GROUP","Event" = "event_name", "Model" = "Model"),
                    selected = "auto"
                ),
                hr(),
                h4("Download Charts (PDF)"),
                downloadButton(ns("downloadExcessPlot"), "Download Excess Death Chart"),
                br(), br(),
                downloadButton(ns("downloadPScorePlot"), "Download P-Score Chart")
            ),
            mainPanel(
                width = 9,
                tabsetPanel(
                    id = ns("plotTabs"),
                    tabPanel(
                        "Excess Deaths",
                        br(),
                        h4("Total Excess Deaths"),
                        wpro_spinner(plotly::plotlyOutput(ns("excessPlot"), height = "600px"))
                    ),
                    tabPanel(
                        "P-Score",
                        br(),
                        h4("Total P-Score (%)"),
                        wpro_spinner(plotly::plotlyOutput(ns("pscorePlot"), height = "600px"))
                    )
                )
            )
        )
    )
}

# --- Module Server ---
plot_event_module_server <- function(id, rv) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns # Get the namespace function

        # --- Reactive UI for Filters ---
        output$sex_filter_ui <- renderUI({
            req(rv$summary_by_event)
            unique_sex <- sort(unique(rv$summary_by_event$SEX), na.last = TRUE, decreasing = TRUE)
            default_sex <- if ("Total" %in% unique_sex) "Total" else unique_sex[1]
            checkboxGroupInput(ns("selected_sex"), "Select Sex:",
                               choices = unique_sex,
                               selected = default_sex)
        })

        output$age_group_filter_ui <- renderUI({
            req(rv$summary_by_event)
            unique_age_group <- sort(unique(rv$summary_by_event$AGE_GROUP), na.last = TRUE, decreasing = TRUE)
            default_age <- if ("Total" %in% unique_age_group) "Total" else unique_age_group[1]
            checkboxGroupInput(ns("selected_age_group"), "Select Age Group:",
                               choices = unique_age_group,
                               selected = default_age)
        })

        output$model_filter_ui <- renderUI({
            req(rv$summary_by_event)
            unique_models <- sort(unique(rv$summary_by_event$Model))
            checkboxGroupInput(ns("selected_models"), "Select Models to Display:",
                               choices = unique_models,
                               selected = unique_models)
        })

        output$event_filter_ui <- renderUI({
            req(rv$summary_by_event)
            unique_events <- sort(unique(rv$summary_by_event$event_name))
            # Add event selection
            checkboxGroupInput(ns("selected_events"), "Select Events:",
                choices = unique_events,
                selected = unique_events
            )
        })        

        # Listen for facet variable selection, limit to a maximum of two
        observe({
            if (length(input$facet_variables) > 2) {
                updateCheckboxGroupInput(session, "facet_variables", 
                                        selected = input$facet_variables[1:2])
            }
        })
        
        # --- Reactive Data Filtering ---
        filtered_overall_data <- reactive({
            # Ensure data is loaded from rv and selections are made
            req(rv$summary_by_event, input$selected_sex, input$selected_age_group, input$selected_models)

            message("Number of data rows before filtering (module): ", nrow(rv$summary_by_event))

            filtered_data <- rv$summary_by_event %>%
                filter(
                    SEX %in% input$selected_sex,
                    AGE_GROUP %in% input$selected_age_group,
                    Model %in% input$selected_models,
                    event_name %in% input$selected_events # Add event filtering
                )

            # Add debugging information
            message("Number of data rows after filtering: ", nrow(filtered_data))
            message(
                "Filtering conditions: SEX in [", paste(input$selected_sex, collapse = ", "),
                "], AGE_GROUP in [", paste(input$selected_age_group, collapse = ", "),
                "], Model in [", paste(input$selected_models, collapse = ", "),
                "], event_name in [", paste(input$selected_events, collapse = ", "), "]"
            )


            if (nrow(filtered_data) > 0) {
                message("Data column names (module): ", paste(names(filtered_data), collapse = ", "))
                message("First row of data (module): ", paste(as.character(filtered_data[1, ]), collapse = ", "))
            }

            return(filtered_data)
        })        
        
        # Get all possible grouping variables
        all_group_vars <- reactive({
            c("SEX", "AGE_GROUP","event_name", "Model")
        })
        
        # Get facet variables
        get_facet_vars <- reactive({
            vars <- input$facet_variables
            if (length(vars) > 2) {
                vars <- vars[1:2]
            }
            return(vars)
        })
        
        # Get inner grouping variables - automatically select variables not used for faceting
        get_inner_group_vars <- reactive({
            facet_vars <- get_facet_vars()
            inner_vars <- setdiff(all_group_vars(), facet_vars)
            message("Facet variables (module): ", paste(facet_vars, collapse = ", "))
            message("Available inner grouping variables (module): ", paste(inner_vars, collapse = ", "))
            return(inner_vars)
        })
        
        # Get primary inner grouping variable - used for fill color
        get_primary_inner_group_var <- reactive({
            inner_vars <- get_inner_group_vars()
            if (length(inner_vars) == 0) {
                message("No available inner grouping variables (module)")
                return(NULL)
            }
            
            if (input$primary_group_var != "auto") {
                if (input$primary_group_var %in% inner_vars) {
                    return(input$primary_group_var)
                } else {
                    message("The selected primary grouping variable is not in the available variables, using automatic selection (module)")
                }
            }
            
            if ("SEX" %in% inner_vars) {
                return("SEX")
            } else if (length(inner_vars) > 0) {
                return(inner_vars[1])
            }
            
            return(NULL)
        })
        
        # Get secondary inner grouping variable - used for grouping (if any)
        get_secondary_inner_group_var <- reactive({
            inner_vars <- get_inner_group_vars()
            if (length(inner_vars) <= 1) {
                message("Not enough inner grouping variables for secondary grouping (module)")
                return(NULL)
            }
            
            if (input$secondary_group_var == "none") {
                return(NULL)
            } else if (input$secondary_group_var != "auto" && input$secondary_group_var %in% inner_vars) {
                primary_var <- get_primary_inner_group_var()
                if (!is.null(primary_var) && input$secondary_group_var != primary_var) {
                    return(input$secondary_group_var)
                } else {
                    message("The selected secondary grouping variable is the same as the primary grouping variable or is unavailable, using automatic selection (module)")
                }
            }
            
            primary_var <- get_primary_inner_group_var()
            if (!is.null(primary_var)) {
                secondary_vars <- setdiff(inner_vars, primary_var)
                if (length(secondary_vars) > 0) {
                    return(secondary_vars[1])
                }
            }

            return(NULL)
        })

        # Every grouping variable that must appear on the x-axis: all inner
        # (non-faceted) variables except the fill variable, restricted to those
        # that actually vary in `data`. Ordered with the user's explicit
        # secondary choice first. This guarantees no grouping variable is left
        # unmapped (which used to overlay unrelated groups on the same bar, #21).
        get_x_axis_vars <- function(data, primary_var, facet_vars) {
            if (is.null(primary_var)) return(character(0))
            candidates <- setdiff(setdiff(all_group_vars(), facet_vars), primary_var)
            sec_choice <- input$secondary_group_var
            if (!is.null(sec_choice) && length(sec_choice) == 1 &&
                sec_choice %in% candidates) {
                candidates <- c(sec_choice, setdiff(candidates, sec_choice))
            }
            candidates[vapply(candidates, function(v) {
                v %in% names(data) && dplyr::n_distinct(data[[v]]) > 1
            }, logical(1))]
        }

        # --- Plotting functions ---
        create_excess_plot <- function(data) {
            message("Creating excess death chart (module), number of data rows: ", ifelse(is.null(data), "NULL", nrow(data)))
            
            if (is.null(data) || nrow(data) == 0) {
                message("No data available for plotting (module)")
                return(ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters.") +
                    theme_void())
            }
            
            facet_vars <- get_facet_vars()
            primary_inner_var <- NULL
            secondary_inner_var <- NULL
            
            tryCatch({ primary_inner_var <- get_primary_inner_group_var() }, error = function(e) { message("Error getting primary grouping variable (module): ", e$message) })
            tryCatch({ secondary_inner_var <- get_secondary_inner_group_var() }, error = function(e) { message("Error getting secondary grouping variable (module): ", e$message) })
            
            message("Using facet variables (module): ", paste(facet_vars, collapse = ", "))
            message("Primary inner grouping variable (module): ", ifelse(is.null(primary_inner_var), "None", primary_inner_var))
            message("Secondary inner grouping variable (module): ", ifelse(is.null(secondary_inner_var), "None", secondary_inner_var))

            if (!"TOTAL_EXCESS" %in% names(data)) {
                message("Missing TOTAL_EXCESS column (module)")
                return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "TOTAL_EXCESS column missing.") + theme_void())
            }
            
            has_ci <- "EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data)
            ci_caption <- if(has_ci) "Note: Error bars represent 95% confidence intervals." else NULL
            
            message("Starting to create excess plot (module)...")
            
            p <- NULL # Initialize p
            x_label <- ""
            fill_label <- NULL

            # X-axis carries EVERY non-fill grouping variable that actually
            # varies in the data, so no grouping variable is silently left
            # unmapped and overlaid onto another group's bar (#21). Fill takes
            # the primary variable; facets take the faceted variables. We never
            # aggregate across an unmapped variable here (summing across Model
            # would be wrong), we surface it on the x-axis instead.
            x_vars <- get_x_axis_vars(data, primary_inner_var, facet_vars)

            if (length(x_vars) > 0 && !is.null(primary_inner_var) && primary_inner_var %in% names(data)) {
                data$interaction_group <- interaction(data[x_vars], sep = " - ", drop = TRUE, lex.order = TRUE)
                p <- ggplot(data, aes(x = interaction_group, y = TOTAL_EXCESS, fill = .data[[primary_inner_var]])) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8)
                x_label <- paste(gsub("_", " ", x_vars), collapse = " · ")
                fill_label <- gsub("_", " ", primary_inner_var)
                message("Excess: x = [", paste(x_vars, collapse = ", "), "], fill = ", primary_inner_var, " (module)")
            } else if (!is.null(primary_inner_var) && primary_inner_var %in% names(data)) {
                p <- ggplot(data, aes(x = .data[[primary_inner_var]], y = TOTAL_EXCESS, fill = .data[[primary_inner_var]])) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8)
                x_label <- gsub("_", " ", primary_inner_var)
                fill_label <- gsub("_", " ", primary_inner_var)
                message("Excess: x = fill = ", primary_inner_var, " (module)")
            } else {
                p <- ggplot(data, aes(x = factor(1), y = TOTAL_EXCESS)) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8, fill = "steelblue") +
                    scale_x_discrete(labels = "Total")
                message("No grouping variables used (module)")
            }
            
            p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black")
            
            if (has_ci) {
                p <- p + geom_errorbar(aes(ymin = EXCESS_LOWER, ymax = EXCESS_UPPER),
                    position = position_dodge(width = 0.9), width = 0.25, color = "gray30")
            }
            
            if (!is.null(fill_label)) {
                # viridis discrete: colourblind-safe for any number of groups
                # (Dark2 caps at 8 and warns/recycles for many age bands).
                p <- p + scale_fill_viridis_d(name = fill_label, option = "D", end = 0.92)
            }
            
            p <- p + labs(title = "Total Excess Deaths", caption = ci_caption, x = x_label, y = "Total Excess Deaths") +
                theme_wpro() +
                theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
            
            if (length(facet_vars) == 1 && facet_vars[1] %in% names(data)) {
                p <- p + facet_wrap(as.formula(paste("~", facet_vars)), scales = "free_y")
            } else if (length(facet_vars) == 2 && facet_vars[1] %in% names(data) && facet_vars[2] %in% names(data)) {
                p <- p + facet_grid(as.formula(paste(facet_vars[1], "~", facet_vars[2])), scales = "free_y")
            }
            
            message("Excess plot creation complete (module)")
            return(p)
        }
        
        create_pscore_plot <- function(data) {
            message("Creating P-Score chart (module), number of data rows: ", ifelse(is.null(data), "NULL", nrow(data)))
            
            if (is.null(data) || nrow(data) == 0) {
                message("No data available for P-Score plotting (module)")
                return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available.") + theme_void())
            }
            
            # Ensure TOTAL_EXPECTED exists and filter out zero/near-zero values before calculating P_SCORE
            if (!"TOTAL_EXPECTED" %in% names(data) || !"TOTAL_EXCESS" %in% names(data)) {
                 message("Missing TOTAL_EXPECTED or TOTAL_EXCESS column for P-Score (module)")
                 return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Required columns missing for P-Score.") + theme_void())
            }

            data <- data %>% filter(abs(TOTAL_EXPECTED) > 1e-6) 
            
            if (nrow(data) == 0) {
                message("No non-zero expected mortality data for P-Score (module)")
                return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data with non-zero expected mortality.") + theme_void())
            }

            data <- data %>% mutate(P_SCORE = (TOTAL_EXCESS / TOTAL_EXPECTED) * 100)

            facet_vars <- get_facet_vars()
            primary_inner_var <- NULL
            secondary_inner_var <- NULL
            
            tryCatch({ primary_inner_var <- get_primary_inner_group_var() }, error = function(e) { message("Error getting primary grouping variable (P-Score module): ", e$message) })
            tryCatch({ secondary_inner_var <- get_secondary_inner_group_var() }, error = function(e) { message("Error getting secondary grouping variable (P-Score module): ", e$message) })

            message("Using facet variables (P-Score module): ", paste(facet_vars, collapse = ", "))
            message("Primary inner grouping variable (P-Score module): ", ifelse(is.null(primary_inner_var), "None", primary_inner_var))
            message("Secondary inner grouping variable (P-Score module): ", ifelse(is.null(secondary_inner_var), "None", secondary_inner_var))

            has_ci <- FALSE
            ci_caption <- NULL
            if ("EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data)) {
                data <- data %>% mutate(
                    P_LOWER = (EXCESS_LOWER / TOTAL_EXPECTED) * 100,
                    P_UPPER = (EXCESS_UPPER / TOTAL_EXPECTED) * 100
                )
                has_ci <- TRUE
                ci_caption <- "Note: Error bars represent 95% confidence intervals for P-Score."
            }
            
            message("Starting to create P-Score plot (module)...")
            
            p <- NULL # Initialize p
            x_label <- ""
            fill_label <- NULL

            # Same #21 fix as the excess chart: every non-fill grouping
            # variable that varies goes on the x-axis so nothing is silently
            # unmapped. P-scores are not additive, so we never aggregate here —
            # each bar is one group's P-score.
            x_vars <- get_x_axis_vars(data, primary_inner_var, facet_vars)

            if (length(x_vars) > 0 && !is.null(primary_inner_var) && primary_inner_var %in% names(data)) {
                data$interaction_group <- interaction(data[x_vars], sep = " - ", drop = TRUE, lex.order = TRUE)
                p <- ggplot(data, aes(x = interaction_group, y = P_SCORE, fill = .data[[primary_inner_var]])) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8)
                x_label <- paste(gsub("_", " ", x_vars), collapse = " · ")
                fill_label <- gsub("_", " ", primary_inner_var)
                message("P-Score: x = [", paste(x_vars, collapse = ", "), "], fill = ", primary_inner_var, " (module)")
            } else if (!is.null(primary_inner_var) && primary_inner_var %in% names(data)) {
                p <- ggplot(data, aes(x = .data[[primary_inner_var]], y = P_SCORE, fill = .data[[primary_inner_var]])) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8)
                x_label <- gsub("_", " ", primary_inner_var)
                fill_label <- gsub("_", " ", primary_inner_var)
                message("P-Score: x = fill = ", primary_inner_var, " (module)")
            } else {
                p <- ggplot(data, aes(x = factor(1), y = P_SCORE)) +
                    geom_col(position = position_dodge(width = 0.9), alpha = 0.8, fill = "steelblue") +
                    scale_x_discrete(labels = "Total")
                message("No grouping variables used (P-Score module)")
            }
            
            p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black")
            
            if (has_ci) {
                p <- p + geom_errorbar(aes(ymin = P_LOWER, ymax = P_UPPER),
                    position = position_dodge(width = 0.9), width = 0.25, color = "gray30")
            }
            
            if (!is.null(fill_label)) {
                # viridis discrete: colourblind-safe for any number of groups
                # (Dark2 caps at 8 and warns/recycles for many age bands).
                p <- p + scale_fill_viridis_d(name = fill_label, option = "D", end = 0.92)
            }
            
            p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1)) +
                labs(title = "Excess Death P-Score (%)", caption = ci_caption, x = x_label, y = "P-Score") +
                theme_wpro() +
                theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
            
            if (length(facet_vars) == 1 && facet_vars[1] %in% names(data)) {
                p <- p + facet_wrap(as.formula(paste("~", facet_vars)), scales = "free_y")
            } else if (length(facet_vars) == 2 && facet_vars[1] %in% names(data) && facet_vars[2] %in% names(data)) {
                p <- p + facet_grid(as.formula(paste(facet_vars[1], "~", facet_vars[2])), scales = "free_y")
            }
            
            message("P-Score plot creation complete (module)")
            return(p)
        }
        
        # --- Render plot output and download handlers ---
        # Interactive plotly (consistent with the prediction/total plot tabs);
        # the ggplot builders are reused unchanged for the PDF downloads below.
        empty_state <- function(msg = "Unable to render for the current selection.") {
            ggplot() + annotate("text", x = 0.5, y = 0.5, label = msg) + theme_void()
        }

        output$excessPlot <- plotly::renderPlotly({
            p <- tryCatch(
                create_excess_plot(filtered_overall_data()),
                error = function(e) { message("Error rendering excess chart (module): ", e$message); empty_state() }
            )
            plotly::ggplotly(p) %>%
                plotly::layout(legend = list(orientation = "h", y = -0.2),
                               font = list(family = "Source Sans Pro, Segoe UI, Arial, sans-serif")) %>%
                plotly::config(displayModeBar = FALSE)
        })

        output$pscorePlot <- plotly::renderPlotly({
            p <- tryCatch(
                create_pscore_plot(filtered_overall_data()),
                error = function(e) { message("Error rendering P-Score chart (module): ", e$message); empty_state() }
            )
            plotly::ggplotly(p) %>%
                plotly::layout(legend = list(orientation = "h", y = -0.2),
                               font = list(family = "Source Sans Pro, Segoe UI, Arial, sans-serif")) %>%
                plotly::config(displayModeBar = FALSE)
        })
        
        # --- Download handlers ---
        output$downloadExcessPlot <- downloadHandler(
            filename = function() {
                paste0("Excess_Death_Chart_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(filtered_overall_data())
                plot_obj <- create_excess_plot(filtered_overall_data())
                ggsave(file, plot = plot_obj, device = "pdf", width = 12, height = 8, units = "in")
            }
        )
        
        output$downloadPScorePlot <- downloadHandler(
            filename = function() {
                paste0("P_Score_Chart_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(filtered_overall_data())
                plot_obj <- create_pscore_plot(filtered_overall_data())
                ggsave(file, plot = plot_obj, device = "pdf", width = 12, height = 8, units = "in")
            }
        )

        # Restore original locale when session ends
        # session$onSessionEnded(function() {
        #     Sys.setlocale("LC_TIME", original_locale_module_total)
        #     message("Restored original locale settings (plot_total_module).")
        # })

    }) # End moduleServer
}
