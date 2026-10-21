# Count baselines for the selected monthly cause. Legacy all-cause models remain available.
library(dplyr)
library(mgcv)
library(withr)

is_cause_series <- function(data) {
  !is.null(data) && nrow(data) > 0L && any(data$CAUSE != "Total")
}

validate_cause_baseline <- function(data) {
  if (!all(data$WM_IDENTIFIER == "Month")) stop("Cause-specific modelling currently supports monthly data.")
  if (anyNA(data$NO_DEATHS)) stop("The selected cause has missing counts. Complete the source data before fitting; missing values are not zeros.")
  period_keys <- sort(data$YEAR * 12L + data$PERIOD)
  if (!identical(as.numeric(period_keys), as.numeric(seq(min(period_keys), max(period_keys))))) {
    stop("The selected cause has missing months. Supply the missing source records before fitting.")
  }
  baseline <- data[data$event_index == "0", , drop = FALSE]
  per_year <- table(baseline$YEAR)
  if (length(per_year) < 2L || sum(per_year == 12L) < 2L) {
    stop("This cause needs at least two complete baseline years before the analysis period. The available data can still be viewed and downloaded.")
  }
  if (any(per_year != 12L)) stop("The baseline has missing or partial years. Use complete baseline years.")
  if (!any(data$event_index != "0")) stop("The event sheet must define at least one analysis period.")
  if (max(baseline$YEAR * 12 + baseline$PERIOD) >= min(data$YEAR[data$event_index != "0"] * 12 + data$PERIOD[data$event_index != "0"])) {
    stop("For cause-specific analysis, all baseline months must precede the analysis period. Extend the event through the last observed month if needed.")
  }
  if (sum(baseline$NO_DEATHS) == 0) stop("No deaths were recorded in the baseline for this cause. A baseline and P-score cannot be estimated reliably.")
  baseline
}

cause_summary <- function(data, expected_draws, predictive_draws, overall = FALSE) {
  event_rows <- which(data$event_index != "0" & !is.na(data$event_index))
  events <- if (overall) list(`999` = event_rows) else split(event_rows, data$event_index[event_rows])
  bind_rows(lapply(names(events), function(event) {
    rows <- events[[event]]
    observed <- sum(data$NO_DEATHS[rows])
    expected <- sum(data$EXP_DEATHS[rows])
    excess_draws <- observed - colSums(predictive_draws[rows, , drop = FALSE])
    expected_sum_draws <- colSums(expected_draws[rows, , drop = FALSE])
    p_draws <- 100 * (observed / expected_sum_draws - 1)
    key <- data[rows[1], c("COUNTRY", "ISO3", "WM_IDENTIFIER", "SEX", "AGE_GROUP", "AREA", "CAUSE"), drop = FALSE]
    transform(key, event_index = if (overall) "999" else event,
              event_name = if (overall) "All Events" else data$event_name[rows[1]],
              TOTAL_DEATHS = observed, TOTAL_EXPECTED = expected, TOTAL_EXCESS = observed - expected,
              TOTAL_SE = stats::sd(excess_draws),
              EXCESS_LOWER = unname(quantile(excess_draws, .025)),
              EXCESS_UPPER = unname(quantile(excess_draws, .975)),
              P_SCORE = if (expected > 0) 100 * (observed / expected - 1) else NA_real_,
              P_LOWER = unname(quantile(p_draws, .025)), P_UPPER = unname(quantile(p_draws, .975)))
  }))
}

fit_cause_pattern <- function(data, model, simulations = 1000L, seed = 1001L) {
  data <- data[order(data$YEAR, data$PERIOD), , drop = FALSE]
  baseline <- validate_cause_baseline(data)
  baseline$time_year <- baseline$YEAR - min(baseline$YEAR)
  data$time_year <- data$YEAR - min(baseline$YEAR)
  baseline$month_factor <- factor(baseline$PERIOD, levels = 1:12)
  data$month_factor <- factor(data$PERIOD, levels = 1:12)
  if (model == "hist") {
    # A month-factor Poisson model reproduces the existing arithmetic monthly averages.
    if (any(tapply(baseline$NO_DEATHS, baseline$PERIOD, sum) == 0)) {
      stop("A baseline month has no deaths for this cause. Use Negative Binomial or a broader cause group.")
    }
    fit <- stats::glm(NO_DEATHS ~ month_factor, family = stats::poisson(), data = baseline)
    design <- stats::model.matrix(~ month_factor, data = data)
    covariance <- stats::vcov(fit)
    offset <- rep(0, nrow(data))
  } else {
    baseline$log_days <- log(baseline$DAYS)
    data$log_days <- log(data$DAYS)
    # Same linear trend and fixed cyclic seasonality as the existing NB baseline.
    fit <- mgcv::gam(NO_DEATHS ~ offset(log_days) + time_year + s(PERIOD, bs = "cc", fx = TRUE, k = 5),
                     data = baseline, family = mgcv::nb(), method = "REML", knots = list(PERIOD = c(.5, 12.5)))
    design <- predict(fit, newdata = data, type = "lpmatrix")
    covariance <- fit$Vp
    offset <- data$log_days
  }
  if (!isTRUE(fit$converged) || any(!is.finite(coef(fit))) || any(!is.finite(covariance))) stop("The count model did not converge for the selected cause.")
  expected <- as.numeric(predict(fit, newdata = data, type = "response"))
  draws <- withr::with_seed(seed, {
    coefficients <- mgcv::rmvn(simulations, mu = coef(fit), V = covariance)
    expected_draws <- exp(sweep(design %*% t(coefficients), 1, offset, "+"))
    if (any(!is.finite(expected_draws)) || any(expected_draws <= 0) || any(!is.finite(expected)) || any(expected <= 0)) stop("The baseline is unstable. Select a broader cause or a longer baseline.")
    predictive_draws <- if (model == "hist") {
      matrix(rpois(length(expected_draws), lambda = as.vector(expected_draws)), nrow = nrow(data))
    } else {
      theta <- fit$family$getTheta(TRUE)
      matrix(rnbinom(length(expected_draws), mu = as.vector(expected_draws), size = theta), nrow = nrow(data))
    }
    list(expected = expected_draws, predictive = predictive_draws)
  })
  expected_draws <- draws$expected
  predictive_draws <- draws$predictive
  if (any(!is.finite(predictive_draws))) stop("The prediction distribution is unstable for this cause.")
  bounds <- t(apply(predictive_draws, 1, quantile, probs = c(.025, .975), names = FALSE))
  p_draws <- 100 * (data$NO_DEATHS / expected_draws - 1)
  p_bounds <- t(apply(p_draws, 1, quantile, probs = c(.025, .975), names = FALSE))
  data$EXP_DEATHS <- expected
  data$LOWER_LIMIT <- bounds[, 1]
  data$UPPER_LIMIT <- bounds[, 2]
  data$EXCESS_DEATHS <- data$NO_DEATHS - expected
  data$P_SCORE <- ifelse(expected > 0, 100 * data$EXCESS_DEATHS / expected, NA_real_)
  data$P_LOWER <- p_bounds[, 1]
  data$P_UPPER <- p_bounds[, 2]
  data$SERIES <- if (model == "hist") "Historical Average model" else "Negative Binomial model"
  data$INTERVAL_TYPE <- "Counterfactual count prediction; P-score bounds use expected-mean uncertainty"
  event_summary <- cause_summary(data, expected_draws, predictive_draws)
  overall_summary <- cause_summary(data, expected_draws, predictive_draws, overall = TRUE)
  data$time_year <- data$month_factor <- data$log_days <- NULL
  attr(data, "aic") <- if (model == "nb") as.numeric(AIC(fit)) else NA_real_
  attr(data, "summary_by_event") <- event_summary
  attr(data, "overall_summary") <- overall_summary
  data
}

run_cause_model <- function(data, model) {
  stopifnot(model %in% c("hist", "nb"))
  if (n_distinct(data$COUNTRY) != 1L || n_distinct(data$CAUSE) != 1L) stop("Select one country and one cause.")
  groups <- data |> group_by(AREA, SEX, AGE_GROUP) |> group_split()
  results <- lapply(groups, fit_cause_pattern, model = model)
  out <- bind_rows(results)
  aic <- vapply(results, attr, numeric(1), "aic")
  attr(out, "aic") <- if (all(is.finite(aic))) sum(aic) else NA_real_
  attr(out, "summary_by_event") <- bind_rows(lapply(results, attr, "summary_by_event"))
  attr(out, "overall_summary") <- bind_rows(lapply(results, attr, "overall_summary"))
  out
}
