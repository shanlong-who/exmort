# GAM (penalized spline) model.
# A negative-binomial GAM with a SMOOTH long-term trend spline and a PENALIZED
# cyclic seasonal spline (method = "REML", fx = FALSE) — a more flexible
# expected-mortality baseline than the fixed-df Negative Binomial model. Count
# prediction intervals are drawn from the fitted NB (predicted mu + estimated
# theta) via qnbinom, so P-score uncertainty is honest.
source_module("modules/common_functions.R")

fit_and_predict_gam <- function(patt_src, hist_src, l_period) {
    t.start <- Sys.time()
    DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    num.cycle <- if (l_period > 51) 52 else 12
    season_k  <- if (l_period > 51) 10 else 6

    # Continuous trend index (fractional years), consistent for baseline + all
    # periods so the fitted trend extrapolates into the event window.
    min_year <- min(patt_src$YEAR, na.rm = TRUE)
    tindex   <- function(d) (d$YEAR - min_year) + (d$PERIOD - 1) / num.cycle

    if (l_period > 51) {
        hist_src$logdays <- log(7)
        src_pandemic <- patt_src %>% mutate(logdays = log(7))
    } else {
        days_h <- DOM[hist_src$PERIOD]
        days_h[hist_src$PERIOD == 2 & (hist_src$YEAR %% 4 == 0)] <- 29
        hist_src$logdays <- log(days_h)

        days_p <- DOM[patt_src$PERIOD]
        days_p[patt_src$PERIOD == 2 & (patt_src$YEAR %% 4 == 0)] <- 29
        src_pandemic <- patt_src %>% mutate(logdays = log(days_p))
    }
    hist_src$t_index     <- tindex(hist_src)
    src_pandemic$t_index <- tindex(src_pandemic)

    # Trend spline basis size capped by the number of distinct baseline times;
    # for short baselines fall back to a linear trend to avoid over-fitting.
    n_t             <- length(unique(hist_src$t_index))
    use_trend_spline <- n_t >= 5
    trend_k         <- max(3, min(10, n_t - 1))
    trend_term      <- if (use_trend_spline) sprintf("s(t_index, bs = 'tp', k = %d)", trend_k) else "t_index"

    form <- as.formula(sprintf(
        "NO_DEATHS ~ offset(logdays) + %s + s(PERIOD, bs = 'cc', k = %d)",
        trend_term, season_k))

    fit <- tryCatch(
        mgcv::gam(form, knots = list(PERIOD = c(0, num.cycle)),
                  method = "REML", family = mgcv::nb(), data = hist_src),
        error = function(e) { warning("GAM fit failed: ", e$message); NULL })
    if (is.null(fit)) return(data.frame())

    model_aic <- tryCatch(as.numeric(AIC(fit)), error = function(e) NA_real_)

    # Predict on the link scale, then draw counts from the fitted NB.
    estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE)
    theta <- fit$family$getTheta(TRUE)

    set.seed(1)
    a <- matrix(rnorm(n = 1000 * length(estim$fit), mean = estim$fit, sd = estim$se.fit),
                ncol = 1000)
    q <- function(x, p) mean(qnbinom(p = p, mu = exp(x), size = theta))
    estim.median <- apply(a, 1, q, p = 0.5)
    estim.lower  <- apply(a, 1, q, p = 0.025)
    estim.upper  <- apply(a, 1, q, p = 0.975)

    estim.median[estim.median < 0] <- 0
    estim.lower[estim.lower < 0]   <- 0
    estim.upper[estim.upper < 0]   <- 0

    message("模式处理时间: ", round(difftime(Sys.time(), t.start, units = "secs"), 1), " 秒")

    list(
        src_pandemic = src_pandemic,
        estim.median = estim.median,
        estim.lower  = estim.lower,
        estim.upper  = estim.upper,
        aic          = model_aic
    )
}

update_output_gam <- function(out_data, model_results, pattern, year_predict) {
    src_pandemic <- model_results$src_pandemic
    estim.median <- model_results$estim.median
    estim.lower  <- model_results$estim.lower
    estim.upper  <- model_results$estim.upper

    if (is.null(src_pandemic) || is.null(estim.median) || is.null(estim.lower) || is.null(estim.upper)) {
        message("警告: 模式 ", pattern, " 的模型结果无效")
        return(out_data)
    }

    pattern_parts <- strsplit(pattern, ";")[[1]]
    result_df <- out_data[0, ]

    l_period <- max(out_data$PERIOD, na.rm = TRUE)
    nyear_predict <- length(year_predict)
    for (iyear_predict in 1:nyear_predict) {
        y <- year_predict[iyear_predict]
        for (k in 1:l_period) {
            a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == k
            current_records <- out_data[
                out_data$SEX == pattern_parts[1] &
                out_data$AGE_GROUP == pattern_parts[2] &
                out_data$AREA == pattern_parts[3] &
                out_data$CAUSE == pattern_parts[4] &
                out_data$YEAR == y &
                out_data$PERIOD == k,
            ]
            if (nrow(current_records) > 0 && sum(a) > 0) {
                current_records$ESTIMATE <- estim.median[a]
                current_records$LOWER_LIMIT <- estim.lower[a]
                current_records$UPPER_LIMIT <- estim.upper[a]
                result_df <- rbind(result_df, current_records)
            }
        }
    }

    return(result_df)
}

fcn_gam <- function(src) {
    message("\n[fcn_gam] Starting GAM (penalized spline) model...")
    flush.console()
    start_time <- Sys.time()

    src <- src %>%
        filter(PERIOD <= 52) %>%
        arrange(SEX, AGE_GROUP, AREA, CAUSE, YEAR, PERIOD) %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS))

    nys <- length(unique(src$YEAR))
    max_period <- max(src$PERIOD, na.rm = TRUE)
    wm_ident <- ifelse(max_period == 12, "Month", "Week")
    l_period <- ifelse(max_period == 12, 12, 52)

    src <- calculate_dates(src, max_period, nys, DOM, MOY)
    out_data <- initialize_output(src, wm_ident, l_period)

    patterns <- src %>%
        select(SEX, AGE_GROUP, AREA, CAUSE) %>%
        distinct() %>%
        mutate(patterns = paste(SEX, AGE_GROUP, AREA, CAUSE, sep = ";")) %>%
        pull(patterns)
    n_pat <- length(patterns)

    results <- list()
    aic_per_pattern <- numeric(0)

    for (j in 1:n_pat) {
        pattern <- patterns[j]
        message("Processing pattern ", j, "/", n_pat, ": ", pattern)

        pattern_parts <- strsplit(pattern, ";")[[1]]
        patt_src <- src[
            src$SEX == pattern_parts[1] &
            src$AGE_GROUP == pattern_parts[2] &
            src$AREA == pattern_parts[3] &
            src$CAUSE == pattern_parts[4],
        ]

        if (nrow(patt_src) < 10) { message("Skipping pattern (insufficient data)"); next }
        hist_src <- patt_src[patt_src$event_index == "0", ]
        if (nrow(hist_src) < 10) { message("Skipping pattern (insufficient baseline)"); next }

        model_results <- fit_and_predict_gam(patt_src, hist_src, l_period)
        if (length(model_results) == 0 || is.data.frame(model_results)) {
            message("Model failed for this pattern"); next
        }

        year_predict <- sort(unique(out_data$YEAR))
        result <- update_output_gam(out_data, model_results, pattern, year_predict)

        if (nrow(result) > 0) {
            results[[j]] <- result
            aic_per_pattern <- c(aic_per_pattern, model_results$aic)
            message("Processed pattern: ", pattern, " (", nrow(result), " rows)")
        }
    }

    if (length(results) > 0) {
        out_data <- do.call(rbind, results[!sapply(results, is.null)])
        message("Combined results from ", length(results[!sapply(results, is.null)]), " patterns")
    } else {
        warning("No patterns produced results.")
        return(NULL)
    }

    out_data <- process_model_results(out_data, "GAM Spline")

    attr(out_data, "aic")             <- if (any(is.finite(aic_per_pattern))) sum(aic_per_pattern, na.rm = TRUE) else NA_real_
    attr(out_data, "aic_per_pattern") <- aic_per_pattern

    total_time <- difftime(Sys.time(), start_time, units = "mins")
    message("GAM (spline) model done, total time: ", round(total_time, 2), " minutes")
    flush.console()

    return(out_data)
}
