# --- 1. Load Libraries ---
# Libraries are assumed to be loaded in app.R
# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(DT)
# library(lubridate)
# library(shinyjs)
# library(plotly)

# --- 2. Define Module UI ---
plot_total_module_ui <- function(id) {
    ns <- NS(id)

    tagList(
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Data Filtering"),
                # Add data loading status indicator
                uiOutput(ns("data_status")),
                # Dynamically generate UI selectors
                uiOutput(ns("sex_selector")),
                uiOutput(ns("age_group_selector")),
                hr(),
                h4("Model Selection"),
                uiOutput(ns("model_selector")),
                hr(),
                h4("Download Charts (PDF)"),
                downloadButton(ns("downloadExcessPlot"), "Download Excess Deaths Chart"),
                br(), br(),
                downloadButton(ns("downloadPScorePlot"), "Download P-Score Chart")
            ),
            mainPanel(
                width = 9,
                # Add data loading status indicator
                uiOutput(ns("plot_status")),
                tabsetPanel(
                    id = ns("plotTabs"),
                    tabPanel(
                        "Excess Deaths",
                        br(),
                        h4("Overall Excess Deaths"),
                        wpro_spinner(plotlyOutput(ns("excessPlot"), height = "600px"))
                    ),
                    tabPanel(
                        "P-Score",
                        br(),
                        h4("Overall P-Score (%)"),
                        wpro_spinner(plotlyOutput(ns("pscorePlot"), height = "600px"))
                    )
                )
            )
        )
    )
}

# --- 3. Define Module Server ---
plot_total_module_server <- function(id, rv) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Data Monitoring ---
        observe({
            if (!is.null(rv$overall_summary)) {
                message("plot_total_module: rv$overall_summary data loaded, rows: ", nrow(rv$overall_summary))
                if (nrow(rv$overall_summary) > 0) {
                    message("plot_total_module: data column names: ", paste(names(rv$overall_summary), collapse = ", "))
                }
            } else {
                message("plot_total_module: rv$overall_summary data not yet loaded")
            }
        })

        # --- Data Status Indicator ---
        output$data_status <- renderUI({
            if (is.null(rv$overall_summary) || nrow(rv$overall_summary) == 0) {
                div(
                    style = "color: #888; font-style: italic; margin-bottom: 15px;",
                    "Data is loading, please wait..."
                )
            } else {
                # Data loaded, don't display anything
                NULL
            }
        })

        output$plot_status <- renderUI({
            if (is.null(rv$overall_summary) || nrow(rv$overall_summary) == 0) {
                div(
                    style = "text-align: center; margin-top: 100px;",
                    h3("Data is loading, please wait..."),
                    tags$div(
                        style = "margin: 20px auto; width: 50px; height: 50px; border: 5px solid #f3f3f3; border-top: 5px solid #3498db; border-radius: 50%; animation: spin 2s linear infinite;",
                        HTML("<style>@keyframes spin {0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); }}</style>")
                    )
                )
            } else {
                # Data loaded, don't display anything
                NULL
            }
        })

        # --- Data Loading Status ---
        data_loaded <- reactive({
            is_loaded <- !is.null(rv$overall_summary) && nrow(rv$overall_summary) > 0
            message("plot_total_module: data_loaded() = ", is_loaded)
            return(is_loaded)
        })

        # --- Dynamically Generate UI Selectors ---
        output$sex_selector <- renderUI({
            # Ensure data is loaded
            req(rv$overall_summary)

            # Extract unique sex values and sort
            unique_sex <- sort(unique(rv$overall_summary$SEX), na.last = TRUE, decreasing = TRUE)
            message("plot_total_module: generating sex selector, options: ", paste(unique_sex, collapse = ", "))

            # Create selector
            selectInput(ns("selected_sex"), "Select Sex:",
                choices = unique_sex,
                selected = if (length(unique_sex) > 0) unique_sex[1] else NULL,
                multiple = FALSE
            )
        })

        output$age_group_selector <- renderUI({
            # Ensure data is loaded
            req(rv$overall_summary)

            # Extract unique age group values and sort
            unique_age_group <- sort(unique(rv$overall_summary$AGE_GROUP), na.last = TRUE, decreasing = TRUE)
            message("plot_total_module: generating age group selector, options: ", paste(unique_age_group, collapse = ", "))

            # Create selector
            selectInput(ns("selected_age_group"), "Select Age Group:",
                choices = unique_age_group,
                selected = if (length(unique_age_group) > 0) unique_age_group[1] else NULL,
                multiple = FALSE
            )
        })

        output$model_selector <- renderUI({
            # Ensure data is loaded
            req(rv$overall_summary)

            # Extract unique model values and sort
            unique_models <- sort(unique(rv$overall_summary$Model))
            message("plot_total_module: generating model selector, options: ", paste(unique_models, collapse = ", "))

            # Create selector
            checkboxGroupInput(ns("selected_models"), "Select Models to Display:",
                choices = unique_models,
                selected = unique_models
            )
        })

        # --- Data Filtering ---
        filtered_overall_data <- reactive({
            # Ensure data is loaded
            req(data_loaded())

            # Ensure selectors are initialized
            req(input$selected_sex, input$selected_age_group, input$selected_models)

            message("plot_total_module: starting data filtering, original data rows: ", nrow(rv$overall_summary))

            # Use tryCatch to capture potential filtering errors
            tryCatch(
                {
                    filtered_data <- rv$overall_summary %>%
                        filter(
                            SEX == input$selected_sex,
                            AGE_GROUP == input$selected_age_group,
                            Model %in% input$selected_models
                        )

                    message("plot_total_module: data filtering complete, filtered rows: ", nrow(filtered_data))
                    return(filtered_data)
                },
                error = function(e) {
                    message("plot_total_module: error during data filtering: ", e$message)
                    return(data.frame()) # Return empty data frame
                }
            )
        })

        # --- Plotting Functions ---
        create_excess_plotly <- function(data) {
            message("Creating excess deaths chart, data rows: ", ifelse(is.null(data), "NULL", nrow(data)))

            if (is.null(data) || nrow(data) == 0) {
                message("No data available for plotting")
                return(plot_ly() %>%
                    add_annotations(
                        text = "No data matching the selected filter criteria.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            if (!"TOTAL_EXCESS" %in% names(data) || all(is.na(data$TOTAL_EXCESS))) {
                message("Missing or invalid TOTAL_EXCESS column")
                return(plot_ly() %>%
                    add_annotations(
                        text = "Missing or invalid TOTAL_EXCESS column.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            if (!"Model" %in% names(data) || all(is.na(data$Model))) {
                message("Missing or invalid Model column")
                return(plot_ly() %>%
                    add_annotations(
                        text = "Missing or invalid Model column.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            has_ci <- "EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data)

            message("Starting to create excess deaths chart...")

            # Create base plotly object
            p <- plot_ly()

            # Create bar chart for each model
            for (model_name in unique(data$Model)) {
                model_data <- data[data$Model == model_name, ]

                # Define a richer color palette to ensure each model has a distinct color
                # Colourblind-safe model palette shared across plot modules.
                model_colors <- get_model_colors()

                # If the model name is in the predefined colors, use it; otherwise use a default color
                bar_color <- if (model_name %in% names(model_colors)) {
                    model_colors[model_name]
                } else {
                    "#999999" # Default gray
                }

                # Add bar chart
                p <- p %>% add_bars(
                    data = model_data,
                    x = ~Model,
                    y = ~TOTAL_EXCESS,
                    name = model_name,
                    marker = list(color = bar_color),
                    error_y = if (has_ci) {
                        list(
                            type = "data",
                            array = model_data$EXCESS_UPPER - model_data$TOTAL_EXCESS,
                            arrayminus = model_data$TOTAL_EXCESS - model_data$EXCESS_LOWER,
                            color = "gray40",
                            thickness = 0.5,
                            width = 3
                        )
                    } else {
                        NULL
                    },
                    hoverinfo = "text",
                    text = if (has_ci) {
                        paste0("Model: ", model_name,
                              "<br>Excess Deaths: ", round(model_data$TOTAL_EXCESS, 2),
                              "<br>95% CI: ", round(model_data$EXCESS_LOWER, 2), ", ", round(model_data$EXCESS_UPPER, 2))
                    } else {
                        paste("Model:", model_name,
                              "<br>Excess Deaths:", round(model_data$TOTAL_EXCESS, 2))
                    },
                    textposition = "none"
                )
            }

            # Get unique model names for x-axis ordering
            model_names <- unique(data$Model)

            # Set layout
            plot_subtitle <- paste("Sex:", input$selected_sex, "| Age Group:", input$selected_age_group)

            p <- p %>% layout(
                title = list(
                    text = paste("Overall Excess Deaths -", plot_subtitle),
                    font = list(size = 16)
                ),
                xaxis = list(
                    title = "Model",
                    tickangle = 0,
                    categoryorder = "array",
                    categoryarray = model_names,
                    type = "category", # Ensure x-axis type is category
                    showgrid = FALSE, # Close grid lines
                    fixedrange = TRUE, # Fix x-axis range
                    rangeslider = FALSE # Disable range slider
                ),
                yaxis = list(title = "Excess Deaths"),
                barmode = "group",
                showlegend = TRUE,
                legend = list(orientation = "h", y = -0.2),
                hovermode = "closest"
            )

            return(p)
        }

        create_pscore_plotly <- function(data) {
            message("Creating P-Score chart, data rows: ", ifelse(is.null(data), "NULL", nrow(data)))

            if (is.null(data) || nrow(data) == 0) {
                message("No data available for P-Score plotting")
                return(plot_ly() %>%
                    add_annotations(
                        text = "No data matching the selected filter criteria.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            # Ensure TOTAL_EXPECTED exists and filter out zero/near-zero values, then calculate P_SCORE
            if (!"TOTAL_EXPECTED" %in% names(data) || !"TOTAL_EXCESS" %in% names(data) || all(is.na(data$TOTAL_EXPECTED)) || all(is.na(data$TOTAL_EXCESS))) {
                return(plot_ly() %>%
                    add_annotations(
                        text = "Missing or invalid columns required for P-Score calculation.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            data <- data %>% filter(abs(TOTAL_EXPECTED) > 1e-6)

            if (nrow(data) == 0) {
                message("No non-zero expected death rate data for P-Score")
                return(plot_ly() %>%
                    add_annotations(
                        text = "No data with non-zero expected death rates.",
                        x = 0.5, y = 0.5, showarrow = FALSE
                    ))
            }

            data <- data %>% mutate(P_SCORE = (TOTAL_EXCESS / TOTAL_EXPECTED) * 100)

            # Check if confidence intervals are available
            has_ci <- FALSE
            if ("EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data) &&
                "TOTAL_EXPECTED" %in% names(data)) {
                data <- data %>% mutate(
                    P_LOWER = (EXCESS_LOWER / TOTAL_EXPECTED) * 100,
                    P_UPPER = (EXCESS_UPPER / TOTAL_EXPECTED) * 100
                )
                has_ci <- TRUE
            }

            # Create base plotly object
            p <- plot_ly()

            # Create bar chart for each model
            for (model_name in unique(data$Model)) {
                model_data <- data[data$Model == model_name, ]

                # Use the same color palette as in the excess deaths chart
                # Colourblind-safe model palette shared across plot modules.
                model_colors <- get_model_colors()

                # If the model name is in the predefined colors, use it; otherwise use a default color
                bar_color <- if (model_name %in% names(model_colors)) {
                    model_colors[model_name]
                } else {
                    "#999999" # Default gray
                }

                # Add bar chart with hover text
                p <- p %>% add_bars(
                    data = model_data,
                    x = ~Model,
                    y = ~P_SCORE,
                    name = model_name,
                    marker = list(color = bar_color),
                    error_y = if (has_ci) {
                        list(
                            type = "data",
                            array = model_data$P_UPPER - model_data$P_SCORE,
                            arrayminus = model_data$P_SCORE - model_data$P_LOWER,
                            color = "gray40",
                            thickness = 0.5,
                            width = 3
                        )
                    } else {
                        NULL
                    },
                    hoverinfo = "text",
                    text = if (has_ci) {
                        paste0("Model: ", model_name,
                              "<br>P-Score: ", round(model_data$P_SCORE, 2), "%",
                              "<br>95% CI: ", round(model_data$P_LOWER, 2), "%, ", round(model_data$P_UPPER, 2), "%")
                    } else {
                        paste("Model:", model_name,
                              "<br>P-Score:", round(model_data$P_SCORE, 2), "%")
                    },
                    textposition = "none"
                )
            }

            # Get unique model names for x-axis ordering
            model_names <- unique(data$Model)

            # Set layout
            plot_subtitle <- paste("Sex:", input$selected_sex, "| Age Group:", input$selected_age_group)

            p <- p %>% layout(
                title = list(
                    text = paste("Overall P-Score (%) -", plot_subtitle),
                    font = list(size = 16)
                ),
                xaxis = list(
                    title = "Model",
                    tickangle = 0,
                    categoryorder = "array",
                    categoryarray = model_names,
                    type = "category", # Ensure x-axis type is category
                    showgrid = FALSE, # Close grid lines
                    fixedrange = TRUE, # Fix x-axis range
                    rangeslider = FALSE # Disable range slider
                ),
                yaxis = list(
                    title = "P-Score (%)",
                    tickformat = ".1f"
                ),
                barmode = "group",
                showlegend = TRUE,
                legend = list(orientation = "h", y = -0.2),
                hovermode = "closest"
            )

            return(p)
        }

        # --- Render Charts ---
        output$excessPlot <- renderPlotly({
            # Ensure data is loaded and selectors are initialized
            req(data_loaded())
            req(input$selected_sex, input$selected_age_group, input$selected_models)

            message("Rendering excess deaths chart...")
            tryCatch(
                {
                    plot <- create_excess_plotly(filtered_overall_data())
                    message("Excess deaths chart rendering successful")
                    return(plot)
                },
                error = function(e) {
                    message("Error rendering excess deaths chart: ", e$message)
                    return(plot_ly() %>%
                        add_annotations(
                            text = paste("Plot error:", e$message),
                            x = 0.5, y = 0.5, showarrow = FALSE
                        ))
                }
            )
        })

        output$pscorePlot <- renderPlotly({
            # Ensure data is loaded and selectors are initialized
            req(data_loaded())
            req(input$selected_sex, input$selected_age_group, input$selected_models)

            message("Rendering P-Score chart...")
            tryCatch(
                {
                    plot <- create_pscore_plotly(filtered_overall_data())
                    message("P-Score chart rendering successful")
                    return(plot)
                },
                error = function(e) {
                    message("Error rendering P-Score chart: ", e$message)
                    return(plot_ly() %>%
                        add_annotations(
                            text = paste("Plot error:", e$message),
                            x = 0.5, y = 0.5, showarrow = FALSE
                        ))
                }
            )
        })

        # --- Download Handlers ---
        output$downloadExcessPlot <- downloadHandler(
            filename = function() {
                paste("excess_deaths_", input$selected_sex, "_", input$selected_age_group, "_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
            },
            content = function(file) {
                # Ensure data is loaded and selectors are initialized
                req(data_loaded())
                req(input$selected_sex, input$selected_age_group, input$selected_models)

                # Create temporary file
                temp_file <- tempfile(fileext = ".pdf")

                # Open PDF device
                pdf(temp_file, width = 10, height = 7)

                # Create ggplot version of the chart for PDF export
                data <- filtered_overall_data()

                if (!is.null(data) && nrow(data) > 0) {
                    has_ci <- "EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data)

                    plot_subtitle <- paste("Sex:", input$selected_sex, "| Age Group:", input$selected_age_group)

                    p <- ggplot(data, aes(x = Model, y = TOTAL_EXCESS, fill = Model)) +
                        geom_col(position = "dodge", alpha = 0.7) +
                        {
                            if (has_ci) {
                                geom_errorbar(aes(ymin = EXCESS_LOWER, ymax = EXCESS_UPPER),
                                    position = position_dodge(0.9),
                                    width = 0.25, color = "gray40"
                                )
                            }
                        } +
                        scale_fill_manual(values = get_model_colors()) +
                        labs(
                            title = "Overall Excess Deaths",
                            subtitle = plot_subtitle,
                            x = "Model",
                            y = "Excess Deaths"
                        ) +
                        theme_wpro()

                    print(p)
                } else {
                    # Create empty chart
                    p <- ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data matching the selected filter criteria.") +
                        theme_void()
                    print(p)
                }

                # Close PDF device
                dev.off()

                # Copy temporary file to destination file
                file.copy(temp_file, file)
            }
        )

        output$downloadPScorePlot <- downloadHandler(
            filename = function() {
                paste("p_score_", input$selected_sex, "_", input$selected_age_group, "_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
            },
            content = function(file) {
                # Ensure data is loaded and selectors are initialized
                req(data_loaded())
                req(input$selected_sex, input$selected_age_group, input$selected_models)

                # Create temporary file
                temp_file <- tempfile(fileext = ".pdf")

                # Open PDF device
                pdf(temp_file, width = 10, height = 7)

                # Create ggplot version of the chart for PDF export
                data <- filtered_overall_data()

                if (!is.null(data) && nrow(data) > 0 &&
                    "TOTAL_EXPECTED" %in% names(data) && "TOTAL_EXCESS" %in% names(data)) {
                    data <- data %>%
                        filter(abs(TOTAL_EXPECTED) > 1e-6) %>%
                        mutate(P_SCORE = (TOTAL_EXCESS / TOTAL_EXPECTED) * 100)

                    if (nrow(data) > 0) {
                        has_ci <- "EXCESS_LOWER" %in% names(data) && "EXCESS_UPPER" %in% names(data)

                        if (has_ci) {
                            data <- data %>% mutate(
                                P_LOWER = (EXCESS_LOWER / TOTAL_EXPECTED) * 100,
                                P_UPPER = (EXCESS_UPPER / TOTAL_EXPECTED) * 100
                            )
                        }

                        plot_subtitle <- paste("Sex:", input$selected_sex, "| Age Group:", input$selected_age_group)

                        p <- ggplot(data, aes(x = Model, y = P_SCORE, fill = Model)) +
                            geom_col(position = "dodge", alpha = 0.7) +
                            {
                                if (has_ci) {
                                    geom_errorbar(aes(ymin = P_LOWER, ymax = P_UPPER),
                                        position = position_dodge(0.9),
                                        width = 0.25, color = "gray40"
                                    )
                                }
                            } +
                            scale_fill_manual(values = get_model_colors()) +
                            labs(
                                title = "Overall P-Score (%)",
                                subtitle = plot_subtitle,
                                x = "Model",
                                y = "P-Score (%)"
                            ) +
                            theme_wpro()

                        print(p)
                    } else {
                        # Create empty chart
                        p <- ggplot() +
                            annotate("text", x = 0.5, y = 0.5, label = "No data with non-zero expected death rates.") +
                            theme_void()
                        print(p)
                    }
                } else {
                    # Create empty chart
                    p <- ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data matching the selected filter criteria or missing required columns.") +
                        theme_void()
                    print(p)
                }

                # Close PDF device
                dev.off()

                # Copy temporary file to destination file
                file.copy(temp_file, file)
            }
        )
    })
}
