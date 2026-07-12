# Karlinsky & Kobak baseline (World Mortality Dataset / Our World in Data method).
# A Gaussian linear model with period-of-year fixed effects and a single linear
# year trend, fit on the baseline period and projected forward. This is the
# internationally recognised reference baseline. Prediction intervals come
# directly from predict(interval = "prediction"). The Gaussian likelihood is not
# on the same scale as the count-model AICs (NB / GAM / ZIP), so its AIC is
# reported as NA and excluded from the Best Fit comparison (as for ARIMA).
source("modules/common_functions.R")

fit_and_predict_karlinsky <- function(patt_src, hist_src, l_period) {
    t.start <- Sys.time()

    src_pandemic <- patt_src

    # Period-of-year as a fixed effect; share factor levels between baseline and
    # projection so predict() sees no unknown levels.
    lvls <- sort(unique(patt_src$PERIOD))
    hist_src$PERIOD_f     <- factor(hist_src$PERIOD, levels = lvls)
    src_pandemic$PERIOD_f <- factor(src_pandemic$PERIOD, levels = lvls)

    fit <- tryCatch(
        lm(NO_DEATHS ~ PERIOD_f + YEAR, data = hist_src),
        error = function(e) { warning("Karlinsky-Kobak fit failed: ", e$message); NULL })
    if (is.null(fit)) return(data.frame())

    pred <- tryCatch(
        as.data.frame(predict(fit, newdata = src_pandemic, interval = "prediction", level = 0.95)),
        error = function(e) { warning("Karlinsky-Kobak predict failed: ", e$message); NULL })
    if (is.null(pred) || nrow(pred) != nrow(src_pandemic)) return(data.frame())

    estim.median <- pmax(pred$fit, 0)
    estim.lower  <- pmax(pred$lwr, 0)
    estim.upper  <- pmax(pred$upr, 0)

    message("模式处理时间: ", round(difftime(Sys.time(), t.start, units = "secs"), 1), " 秒")

    list(
        src_pandemic = src_pandemic,
        estim.median = estim.median,
        estim.lower  = estim.lower,
        estim.upper  = estim.upper,
        aic          = NA_real_  # Gaussian LM AIC not comparable to count models
    )
}

update_output_karlinsky <- function(out_data, model_results, pattern, year_predict) {
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

fcn_karlinsky <- function(src) {
    message("\n[fcn_karlinsky] Starting Karlinsky-Kobak baseline model...")
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

        model_results <- fit_and_predict_karlinsky(patt_src, hist_src, l_period)
        if (length(model_results) == 0 || is.data.frame(model_results)) {
            message("Model failed for this pattern"); next
        }

        year_predict <- sort(unique(out_data$YEAR))
        result <- update_output_karlinsky(out_data, model_results, pattern, year_predict)

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

    out_data <- process_model_results(out_data, "Karlinsky-Kobak")

    # Gaussian LM AIC is not comparable to the count models, so this stays NA and
    # the model is excluded from the Best Fit winner selection.
    attr(out_data, "aic")             <- if (any(is.finite(aic_per_pattern))) sum(aic_per_pattern, na.rm = TRUE) else NA_real_
    attr(out_data, "aic_per_pattern") <- aic_per_pattern

    total_time <- difftime(Sys.time(), start_time, units = "mins")
    message("Karlinsky-Kobak model done, total time: ", round(total_time, 2), " minutes")
    flush.console()

    return(out_data)
}
