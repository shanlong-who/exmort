# From the repository root: Rscript data-raw/check_cause_models.R [output.csv]
# Release QA only. Use the source checkout and the same calendar and models as the app.
library(pkgload)
library(dplyr)
library(purrr)

load_all(".", quiet = TRUE)
app <- app_module_env(c("utils.R", "cause_data.R", "cause_analysis.R"))
output <- commandArgs(trailingOnly = TRUE)

audit_fit <- function(data, model, period) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers(
    app$run_cause_model(data, model),
    warning = function(w) {
      warnings <<- unique(c(warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }), error = identity)
  row <- tibble(country = data$COUNTRY[1], cause = data$CAUSE[1], model = model,
                evaluation = period, status = "fitted", detail = paste(warnings, collapse = " | "),
                n_months = NA_integer_, inside_interval = NA_integer_,
                observed = NA_real_, expected = NA_real_)
  if (inherits(result, "error")) {
    row$status <- "stopped"
    row$detail <- conditionMessage(result)
    return(row)
  }
  event <- result[result$event_index != "0", ]
  summary <- attr(result, "overall_summary")
  event.summary <- attr(result, "summary_by_event")
  equals <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-7))
  stopifnot(nrow(result) == nrow(data), all(is.finite(result$EXP_DEATHS) & result$EXP_DEATHS > 0),
            equals(result$EXCESS_DEATHS, result$NO_DEATHS - result$EXP_DEATHS),
            equals(result$P_SCORE, 100 * (result$NO_DEATHS / result$EXP_DEATHS - 1)),
            equals(summary$TOTAL_DEATHS, sum(event$NO_DEATHS)),
            equals(summary$TOTAL_EXPECTED, sum(event$EXP_DEATHS)),
            equals(summary$TOTAL_EXCESS, sum(event$EXCESS_DEATHS)),
            equals(summary$P_SCORE, 100 * (sum(event$NO_DEATHS) / sum(event$EXP_DEATHS) - 1)),
            equals(event.summary$TOTAL_EXPECTED, summary$TOTAL_EXPECTED),
            all(is.finite(as.matrix(result[c("LOWER_LIMIT", "UPPER_LIMIT", "P_LOWER", "P_UPPER")]))),
            all(result$LOWER_LIMIT >= 0 & result$LOWER_LIMIT <= result$UPPER_LIMIT),
            all(result$P_LOWER <= result$P_UPPER))
  if (model == "hist") {
    baseline <- data[data$event_index == "0", ]
    means <- tapply(baseline$NO_DEATHS, baseline$PERIOD, mean)
    stopifnot(equals(result$EXP_DEATHS, means[as.character(result$PERIOD)]))
  }
  row$n_months <- nrow(event)
  row$inside_interval <- sum(event$NO_DEATHS >= event$LOWER_LIMIT & event$NO_DEATHS <= event$UPPER_LIMIT)
  row$observed <- sum(event$NO_DEATHS)
  row$expected <- sum(event$EXP_DEATHS)
  row
}

audit_country <- function(country) {
  path <- file.path("inst/app/XLSX", paste0(country, "_cause_monthly.xlsx"))
  counts <- app$read_mortality_sheet(path, "National level")
  calendar <- app$loadEventsData(path, "events")
  joined <- as.data.frame(app$mergeEventsData(counts, calendar))
  map_dfr(split(joined, joined$CAUSE), function(series) {
    data <- app$prepare_mortality_model_input(series)
    history <- data[data$event_index == "0", ]
    holdout.year <- max(history$YEAR)
    history$event_index <- ifelse(history$YEAR == holdout.year, "1", "0")
    history$event_name <- ifelse(history$YEAR == holdout.year, "Held-out year", "")
    map_dfr(c("hist", "nb"), function(model) {
      bind_rows(audit_fit(data, model, "workbook calendar"),
                audit_fit(history, model, paste("holdout", holdout.year)))
    })
  })
}

audit <- map_dfr(c("Australia", "Philippines"), audit_country)
print(count(audit, country, evaluation, model, status), n = Inf)
print(audit %>% filter(startsWith(evaluation, "holdout"), status == "fitted") %>%
        summarise(inside = sum(inside_interval), total = sum(n_months),
                  .by = c(country, model)))
print(count(audit, status, detail, sort = TRUE), n = Inf)
if (length(output)) write.csv(audit, output[1], row.names = FALSE, na = "")
