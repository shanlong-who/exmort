library(testthat)
library(shiny)
library(dplyr)
library(openxlsx)
library(withr)

cause_env <- app_module_env(c("data_process_module.R", "model_run_module.R",
  "plot_functions/plot_utils.R", "report_module.R", "plot_total_module.R",
  "plot_event_module_new.R", "best_fit_module.R"))
withr::local_dir(exmort_app_dir())
local({
test_that("both official workbooks preserve monthly counts and cause labels", {
  for (country in c("Australia", "Philippines")) {
    data <- read_mortality_sheet(paste0("XLSX/", country, "_cause_monthly.xlsx"), "National level")
    expect_true(all(data$WM_IDENTIFIER == "Month"))
    expect_true(n_distinct(data$CAUSE) > 5L)
    expect_false(anyDuplicated(data[c("CAUSE", "YEAR", "PERIOD")]) > 0)
    expect_true(all(is.na(data$NO_DEATHS) | data$NO_DEATHS >= 0))
  }
  aus <- read_mortality_sheet("XLSX/Australia_cause_monthly.xlsx", "National level")
  expect_equal(aus$NO_DEATHS[aus$CAUSE == "Ischaemic heart diseases" & aus$YEAR == 2015 & aus$PERIOD == 1], 1145)
  expect_equal(aus$NO_DEATHS[aus$CAUSE == "Ischaemic heart diseases" & aus$YEAR == 2026 & aus$PERIOD == 5], 1012)
  expect_equal(max(aus$PERIOD[aus$YEAR == 2026]), 5)
  phl <- read_mortality_sheet("XLSX/Philippines_cause_monthly.xlsx", "National level")
  expect_equal(n_distinct(phl$CAUSE), 116)
  expect_equal(range(phl$YEAR), c(2019, 2024))
  expect_equal(phl$NO_DEATHS[grepl("1-067", phl$CAUSE) & phl$YEAR == 2019 & phl$PERIOD == 1], 9371)
  expect_equal(phl$NO_DEATHS[grepl("1-067", phl$CAUSE) & phl$YEAR == 2020 & phl$PERIOD == 1], 8609)
  expect_equal(phl$NO_DEATHS[grepl("1-067", phl$CAUSE) & phl$YEAR == 2024 & phl$PERIOD == 1], 11478)
})

test_that("legacy monthly and weekly templates still import", {
  monthly <- read_mortality_sheet("XLSX/Philippines_built_in_data.xlsx", "National level")
  weekly <- read_mortality_sheet("XLSX/Australia_built_in_data.xlsx", "National level")
  expect_true(all(monthly$CAUSE == "Total"))
  expect_true(all(weekly$CAUSE == "Total"))
  expect_equal(unique(monthly$WM_IDENTIFIER), "Month")
  expect_equal(unique(weekly$WM_IDENTIFIER), "Week")
  expect_equal(weekly$NO_DEATHS[weekly$YEAR == 2015 & weekly$PERIOD == 1 & weekly$AGE_GROUP == "Total" & weekly$SEX == "Total"], 2925)
})

test_that("normalization distinguishes missing, zero and invalid counts", {
  x <- data.frame(YEAR = 2019, PERIOD = 1:3, NO_DEATHS = c(0, NA, 3))
  checked <- normalize_mortality_data(x)
  expect_identical(checked$NO_DEATHS, c(0, NA_real_, 3))
  expect_error(normalize_mortality_data(rbind(x, x[1, ])), "Duplicate")
  expect_error(normalize_mortality_data(transform(x, NO_DEATHS = .5)), "whole counts")
  expect_error(normalize_mortality_data(transform(x, NO_DEATHS = -1)), "whole counts")
  expect_error(prepare_mortality_model_input(transform(x, COUNTRY = c("A", "B", "A"))), "one country")
})

cause_test_data <- function(cause = "Ischaemic heart diseases") {
  data <- read_mortality_sheet("XLSX/Australia_cause_monthly.xlsx", "National level")
  data <- data[data$CAUSE == cause, ]
  data$event_index <- ifelse(data$YEAR >= 2020, "1", "0")
  data$event_name <- ifelse(data$YEAR >= 2020, "Analysis", "")
  prepare_mortality_model_input(data)
}

test_that("historical averages and NB retain cause identity and coherent totals", {
  data <- cause_test_data()
  for (model in c("hist", "nb")) {
    result <- run_cause_model(data, model)
    expect_equal(nrow(result), nrow(data))
    expect_identical(unique(result$COUNTRY), "Australia")
    expect_identical(unique(result$CAUSE), "Ischaemic heart diseases")
    expect_true(all(is.finite(result$EXP_DEATHS) & result$EXP_DEATHS > 0))
    expect_equal(result$EXCESS_DEATHS, result$NO_DEATHS - result$EXP_DEATHS)
    expect_true(all(result$LOWER_LIMIT >= 0 & result$UPPER_LIMIT >= result$LOWER_LIMIT))
    expect_true(all(result$P_LOWER <= result$P_UPPER))
    summary <- attr(result, "overall_summary")
    selected <- result$event_index != "0"
    expect_equal(summary$TOTAL_EXCESS, sum(result$EXCESS_DEATHS[selected]))
    expect_equal(summary$TOTAL_EXPECTED, sum(result$EXP_DEATHS[selected]))
    if (model == "hist") {
      expected <- tapply(data$NO_DEATHS[data$YEAR < 2020], data$PERIOD[data$YEAR < 2020], mean)
      expect_equal(result$EXP_DEATHS, as.numeric(expected[as.character(data$PERIOD)]), tolerance = 1e-5)
    }
  }
})

test_that("missing counts, zero baselines and short baselines cannot produce false success", {
  data <- cause_test_data()
  expect_error(run_cause_model(transform(data, NO_DEATHS = NA_real_), "nb"), "missing counts")
  expect_error(run_cause_model(transform(data, NO_DEATHS = 0), "nb"), "No deaths")
  expect_error(run_cause_model(data[data$YEAR >= 2019, ], "nb"), "two complete")
  expect_error(run_cause_model(data[-1, ], "nb"), "partial years")
  phl <- read_mortality_sheet("XLSX/Philippines_cause_monthly.xlsx", "National level")
  phl <- phl[grepl("1-067", phl$CAUSE), ]
  phl <- phl[phl$YEAR >= 2023, ]
  phl$event_index <- ifelse(phl$YEAR == 2024, "1", "0")
  phl$event_name <- "Analysis"
  expect_error(run_cause_model(phl, "hist"), "two complete")
})

test_that("Philippine history provides complete cause baselines without inventing earlier categories", {
  phl <- read_mortality_sheet("XLSX/Philippines_cause_monthly.xlsx", "National level")
  covid <- phl[phl$CAUSE == "COVID-19", ]
  expect_true(all(is.na(covid$NO_DEATHS[covid$YEAR == 2019])))
  notes <- mortality_source_note("XLSX/Philippines_cause_monthly.xlsx")
  expect_match(notes, "not a pre-pandemic counterfactual", fixed = TRUE)
  for (code in c("1-067", "1-026", "1-052")) {
    data <- phl[startsWith(phl$CAUSE, code), ]
    data$event_index <- ifelse(data$YEAR == 2024, "1", "0")
    data$event_name <- ifelse(data$YEAR == 2024, "2024 comparison", "")
    data <- prepare_mortality_model_input(data)
    expect_equal(nrow(validate_cause_baseline(data)), 60L)
    for (model in c("hist", "nb")) {
      result <- run_cause_model(data, model)
      expect_equal(nrow(result), 72L)
      expect_true(all(is.finite(result$EXP_DEATHS) & result$EXP_DEATHS > 0))
      expect_equal(attr(result, "overall_summary")$TOTAL_EXCESS, sum(result$EXCESS_DEATHS[result$YEAR == 2024]))
    }
  }
})

test_that("changing the cause clears results and only exposes the selected series", {
  rv <- reactiveValues()
  testServer(data_process_module_server, args = list(rv = rv), {
    session$setInputs(filetype = "2", samplecountry = "Australia: monthly causes of death",
                      builtinsheetname = "National level", builtineventsheetname = "events", loadBuiltinEvents = 1)
    session$flushReact()
    expect_true(nrow(rv$merged_data) > 100)
    expect_equal(n_distinct(rv$merged_data$CAUSE), 1L)
    rv$total_prediction <- data.frame(previous = 1)
    session$setInputs(selected_country = "Australia", selected_cause = "Ischaemic heart diseases")
    session$flushReact()
    expect_identical(unique(rv$merged_data$CAUSE), "Ischaemic heart diseases")
    expect_null(rv$total_prediction)
    expect_true(grepl("Ischaemic heart diseases", rv$data_description, fixed = TRUE))
  })
})

test_that("long-format uploads keep multiple countries and their separate cause lists", {
  x <- data.frame(COUNTRY = c("Australia", "Philippines"), ISO3 = c("AUS", "PHL"),
                  CAUSE = c("Cause A", "Cause B"), YEAR = 2019, PERIOD = 1, NO_DEATHS = c(0, 5))
  file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(`National level` = x), file)
  expect_equal(read_mortality_sheet(file, "National level")$CAUSE, x$CAUSE)
  expect_equal(read_mortality_sheet(file, "National level")$COUNTRY, x$COUNTRY)
})


test_that("cause fits preserve the caller RNG state", {
  withr::local_seed(73)
  expected <- runif(3)
  set.seed(73)
  run_cause_model(cause_test_data(), "nb")
  expect_identical(runif(3), expected)
})

test_that("country changes invalidate predictions, AIC and report state", {
  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  counts <- data.frame(COUNTRY = c("Australia", "Philippines"), ISO3 = c("AUS", "PHL"),
    CAUSE = c("Heart disease", "Cancer"), YEAR = 2019, PERIOD = 1, NO_DEATHS = c(0, 8))
  events <- data.frame(event_name = "Analysis", start_date = as.Date("2020-01-01"), end_date = as.Date("2020-12-31"))
  openxlsx::write.xlsx(list(`National level` = counts, events = events), file)
  rv <- shiny::reactiveValues()
  shiny::testServer(data_process_module_server, args = list(rv = rv), {
    session$setInputs(rawdatafile = list(name = "Causes.xlsx", datapath = file),
      sheetname = "National level", eventsheetname = "events", loadEvents = 1)
    expect_identical(unique(rv$merged_data$COUNTRY), "Australia")
    rv$total_prediction <- data.frame(old = 1)
    rv$model_aic <- data.frame(Model = "Old", AIC = 1)
    rv$best_model <- "Old"
    session$setInputs(selected_country = "Philippines", selected_cause = "Cancer")
    expect_identical(unique(rv$merged_data$CAUSE), "Cancer")
    expect_identical(rv$merged_data$NO_DEATHS, 8)
    expect_null(rv$total_prediction)
    expect_null(rv$model_aic)
    expect_null(rv$best_model)
    # Preserve the validated current data if a replacement fails validation.
    openxlsx::write.xlsx(list(`National level` = transform(counts, NO_DEATHS = -.5), events = events), file, overwrite = TRUE)
    session$setInputs(loadEvents = 2)
    expect_identical(rv$merged_data$NO_DEATHS, 8)
    expect_false(validation_state()$ok)
  })
})

test_that("the production cause pipeline retains fractional expectations and eligible AIC", {
  data <- cause_test_data()
  data$CAUSE <- "Sparse cause"
  data$NO_DEATHS <- as.numeric(data$YEAR == 2015)
  rv <- shiny::reactiveValues(merged_data = data, merged_data_ext = data,
    data_description = "Australia - Sparse cause", source_note = "Verified source note")
  shiny::testServer(model_run_module_server, args = list(rv = rv), {
    session$setInputs(model_hist = TRUE, model_nb = FALSE, model_gam = TRUE,
      model_karlinsky = TRUE, sex = "Total", age_group = "Total", run = 1)
    fitted <- model_results()
    expect_named(fitted, "hist")
    expect_true(all(rv$total_prediction$EXP_DEATHS > 0 & rv$total_prediction$EXP_DEATHS < 1))
    expect_equal(rv$total_prediction$EXCESS_DEATHS, rv$total_prediction$NO_DEATHS - rv$total_prediction$EXP_DEATHS)
    expect_identical(unique(rv$total_prediction$CAUSE), "Sparse cause")
    expect_equal(rv$overall_summary$TOTAL_EXPECTED, sum(rv$total_prediction$EXP_DEATHS[rv$total_prediction$event_index != "0"]))
    expect_true(all(is.na(rv$model_aic$AIC)))
  })
  predictions <- shiny::isolate(rv$total_prediction)
  predictions$TimePoint <- as.Date(sprintf("%04d-%02d-01", predictions$YEAR, predictions$PERIOD))
  plot <- create_mortality_plot(predictions, "ACM", "Total", "Total")
  expect_match(plot$labels$subtitle, "Sparse cause")
  expect_equal(nrow(plot$data), nrow(data))
  shiny::testServer(report_module_server, args = list(rv = rv), {
    session$setInputs(selected_sex = "Total", selected_age_group = "Total",
      selected_models = "Historical Average", selected_event = "All", include_plots = FALSE)
    params <- assemble_report_params()
    expect_match(params$data_description, "Sparse cause")
    expect_identical(params$source_note, "Verified source note")
    expect_true(params$cause_mode)
    report_generated(TRUE)
    rv$total_prediction <- NULL
    session$flushReact()
    expect_false(report_generated())
  })
  data <- cause_test_data()
  rv <- shiny::reactiveValues(merged_data = data, merged_data_ext = data)
  shiny::testServer(model_run_module_server, args = list(rv = rv), {
    session$setInputs(model_hist = TRUE, model_nb = TRUE, sex = "Total", age_group = "Total", run = 1)
    fitted <- model_results()
    expect_named(fitted, c("hist", "nb"))
    expect_true(is.finite(rv$model_aic$AIC[rv$model_aic$Model == "Negative Binomial Regression"]))
    expect_identical(rv$best_model, "Negative Binomial Regression")
    expect_equal(nrow(rv$total_prediction), nrow(data) * 2)
    expect_true(all(rv$total_prediction$P_LOWER <= rv$total_prediction$P_UPPER))
  })
})

test_that("event and overall charts preserve cause P-score bounds", {
  summary <- attr(run_cause_model(cause_test_data(), "hist"), "overall_summary")
  summary$Model <- "Historical Average"
  summary$P_LOWER <- -80
  summary$P_UPPER <- 20
  rv <- shiny::reactiveValues(overall_summary = summary, summary_by_event = summary)
  shiny::testServer(plot_total_module_server, args = list(rv = rv), {
    session$setInputs(selected_sex = "Total", selected_age_group = "Total", selected_models = "Historical Average")
    built <- plotly::plotly_build(create_pscore_plotly(summary))
    expect_equal(as.numeric(built$x$data[[1]]$error_y$array), summary$P_UPPER - summary$P_SCORE)
    expect_equal(as.numeric(built$x$data[[1]]$error_y$arrayminus), summary$P_SCORE - summary$P_LOWER)
  })
  shiny::testServer(plot_event_module_server, args = list(rv = rv), {
    session$setInputs(selected_sex = "Total", selected_age_group = "Total", selected_models = "Historical Average",
      selected_events = "All Events", facet_variables = character(), primary_group_var = "Model", secondary_group_var = "none")
    plot <- create_pscore_plot(summary)
    expect_equal(plot$data$P_LOWER, summary$P_LOWER)
    expect_equal(plot$data$P_UPPER, summary$P_UPPER)
  })
})

test_that("all seven GitHub models and Best Fit still run on legacy all-cause data", {
  data <- read_mortality_sheet("XLSX/Philippines_built_in_data.xlsx", "National level")
  events <- loadEventsData("XLSX/Philippines_built_in_data.xlsx", "events")
  data <- prepare_mortality_model_input(mergeEventsData(data, events))
  data <- data[data$SEX == "Total" & data$AGE_GROUP == "Total", ]
  expect_false(is_cause_series(data))
  expect_equal(model_keys, c("arima", "hist", "nb", "quasipoisson", "zip", "gam", "karlinsky"))
  rv <- shiny::reactiveValues(merged_data = data, merged_data_ext = data)
  shiny::testServer(model_run_module_server, args = list(rv = rv), {
    session$setInputs(model_hist = TRUE, model_nb = TRUE, model_arima = TRUE,
      model_quasipoisson = TRUE, model_zip = TRUE, model_gam = TRUE, model_karlinsky = TRUE,
      sex = "Total", age_group = "Total", run = 1)
    # The legacy ZIP implementation warns when it uses its regular Poisson branch.
    fitted <- suppressWarnings(model_results())
    expect_named(fitted, model_keys)
    for (key in model_keys) expect_true(nrow(fitted[[key]]) > 0, info = key)
    expect_setequal(unique(rv$total_prediction$Model), model_labels)
    expect_true(is.finite(rv$model_aic$AIC[rv$model_aic$Model == "GAM Spline Model"]))
    expect_true(is.na(rv$model_aic$AIC[rv$model_aic$Model == "Karlinsky-Kobak Model"]))
    expect_identical(rv$best_model, rv$model_aic$Model[which.min(rv$model_aic$AIC)])
  })
  shiny::testServer(best_fit_module_server, args = list(rv = rv), {
    expect_match(output$best_banner$html, shiny::isolate(rv$best_model), fixed = TRUE)
  })
})

test_that("the packaged HTML report renders cause identity and interpretation", {
  skip_if_not_installed("flextable")
  skip_if_not(rmarkdown::pandoc_available())
  fitted <- run_cause_model(cause_test_data(), "hist")
  fitted$Model <- "Historical Average"
  overall <- attr(fitted, "overall_summary")
  overall$Model <- "Historical Average"
  events <- attr(fitted, "summary_by_event")
  events$Model <- "Historical Average"
  rv <- shiny::reactiveValues(total_prediction = fitted, overall_summary = overall,
    summary_by_event = events, data_description = "Australia - Ischaemic heart diseases",
    source_note = "Doctor-certified deaths only.")
  shiny::testServer(report_module_server, args = list(rv = rv), {
    session$setInputs(selected_sex = "Total", selected_age_group = "Total",
      selected_models = "Historical Average", selected_event = "All",
      include_summary = TRUE, include_tables = TRUE, include_plots = TRUE)
    params <- assemble_report_params()
    expect_true(all(file.exists(unlist(params[c("fig_acm", "fig_excess", "fig_pscore")]))))
    rendered <- render_report(html_format(), params)
    html <- paste(readLines(rendered, warn = FALSE), collapse = "\n")
    html <- gsub("[[:space:]]+", " ", html)
    expect_match(html, "Ischaemic heart diseases")
    expect_match(html, "Doctor-certified deaths only.", fixed = TRUE)
    expect_match(html, "expected mean", fixed = TRUE)
    expect_match(html, "data:image/png;base64,", fixed = TRUE)
  })
})

}, envir = cause_env)
