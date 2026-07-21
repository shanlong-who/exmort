# Load shared helpers
source_module("modules/common_functions.R")

# Helper function: Calculate historical average
calculate_historical_average_hist <- function(hist_src, src_pandemic, l_period) {
    # Index the per-period baseline means BY PERIOD VALUE (1..l_period), not by
    # position. tapply() returns one element per period actually present in the
    # baseline, so a missing baseline period (e.g. a country with no data in a
    # given week) used to shift every later period's mean to the wrong week when
    # update_output_hist() read ave_deaths[k]. Aligning to periods 1..l_period
    # by name puts NA where a period is genuinely absent and keeps every other
    # period on its correct index.
    periods <- as.character(seq_len(l_period))
    ave_map <- tapply(hist_src$NO_DEATHS, hist_src$PERIOD, mean, na.rm = TRUE)
    var_map <- tapply(hist_src$NO_DEATHS, hist_src$PERIOD, var,  na.rm = TRUE)
    num_map <- tapply(hist_src$NO_DEATHS, hist_src$PERIOD, length)

    ave_deaths <- as.numeric(ave_map[periods])
    var_deaths <- as.numeric(var_map[periods])
    num_deaths <- as.numeric(num_map[periods])
    ave_deaths_lower <- ave_deaths - qnorm(0.975) * sqrt(var_deaths * (1 + 1 / num_deaths))
    ave_deaths_upper <- ave_deaths + qnorm(0.975) * sqrt(var_deaths * (1 + 1 / num_deaths))

    return(list(
        ave_deaths = ave_deaths,
        ave_deaths_lower = ave_deaths_lower,
        ave_deaths_upper = ave_deaths_upper
    ))
}

# Helper function: update output for historical average
update_output_hist <- function(out_hist, pattern, l_period, year_predict, ave_deaths, ave_deaths_lower, ave_deaths_upper) {
    # Extract pattern data
    pattern_parts <- strsplit(pattern, ";")[[1]]

    # Initialize result data frame
    result_df <- out_hist[0, ]

    # Process each year and period
    for (iyear_predict in 1:length(year_predict)) {
        y <- year_predict[iyear_predict]
        for (k in 1:l_period) {
            # Find matching records
            current_records <- out_hist[
                out_hist$SEX == pattern_parts[1] &
                out_hist$AGE_GROUP == pattern_parts[2] &
                out_hist$AREA == pattern_parts[3] &
                out_hist$CAUSE == pattern_parts[4] &
                out_hist$YEAR == y &
                out_hist$PERIOD == k,
            ]

            # Update records if found
            if (nrow(current_records) > 0) {
                current_records$ESTIMATE <- ave_deaths[k]
                current_records$LOWER_LIMIT <- ave_deaths_lower[k]
                current_records$UPPER_LIMIT <- ave_deaths_upper[k]
                result_df <- rbind(result_df, current_records)
            }
        }
    }

    return(result_df)
}

# Helper function: serial processing for historical average
process_serial_hist <- function(n_pat, patterns, src, l_period, out_hist) {
    message("Using serial processing...")
    start_time <- Sys.time()

    # Initialize results list
    results <- list()

    # Process each pattern
    for (j in 1:n_pat) {
        pattern <- patterns[j]
        message("Processing pattern: ", pattern)

        # Extract pattern data
        pattern_parts <- strsplit(pattern, ";")[[1]]
        patt_src <- src[
            src$SEX == pattern_parts[1] &
            src$AGE_GROUP == pattern_parts[2] &
            src$AREA == pattern_parts[3] &
            src$CAUSE == pattern_parts[4],
        ]

        # Skip if insufficient data
        if (nrow(patt_src) < 10) {
            message("Skipping pattern due to insufficient data")
            next
        }

        # Prepare historical data
        hist_src <- patt_src[patt_src$event_index == "0", ]

        # Skip if insufficient historical data
        if (nrow(hist_src) < 10) {
            message("Skipping pattern due to insufficient historical data")
            next
        }

        # Calculate historical average
        src_pandemic <- patt_src # [patt_src$event_index != "0", ]
        year_predict <- sort(unique(out_hist$YEAR))

        # Calculate historical average
        hist_avg <- calculate_historical_average_hist(hist_src, src_pandemic, l_period)

        # Update output
        result <- update_output_hist(
            out_hist, pattern, l_period, year_predict,
            hist_avg$ave_deaths, hist_avg$ave_deaths_lower, hist_avg$ave_deaths_upper
        )

        # Store result
        if (nrow(result) > 0) {
            results[[j]] <- result
            message("Successfully processed pattern: ", pattern, " (", nrow(result), " rows)")
        }
    }

    # Combine results
    if (length(results) > 0) {
        out_hist <- do.call(rbind, results[!sapply(results, is.null)])
        message("Successfully combined results from ", length(results[!sapply(results, is.null)]), " patterns")
    } else {
        warning("No successfully processed patterns!")
        return(NULL)
    }

    end_time <- Sys.time()
    total_time <- difftime(end_time, start_time, units = "mins")
    message("Serial processing completed, total time: ", round(total_time, 2), " minutes")

    # Apply shared post-processing
    out_hist <- process_model_results(out_hist, "Historical Average")

    # Historical Average has no parametric likelihood — no proper AIC.
    # The Best Fit tab renders NA as "N/A".
    attr(out_hist, "aic")             <- NA_real_
    attr(out_hist, "aic_per_pattern") <- numeric(0)

    message("\n[fcn_hist] Historical average calculation completed")
    flush.console()
    return(out_hist)
}

# Main function
fcn_hist <- function(src) {
    message("\n[fcn_hist] Starting historical average calculation...")
    flush.console()
    # Preprocess data
    src <- src %>%
        filter(PERIOD <= 52) %>%
        arrange(SEX, AGE_GROUP, AREA, CAUSE, YEAR, PERIOD) %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS))

    nys <- length(unique(src$YEAR))
    max_period <- max(src$PERIOD, na.rm = TRUE)
    wm_ident <- ifelse(max_period == 12, "Month", "Week")
    l_period <- ifelse(max_period == 12, 12, 52)

    # Calculate dates
    src <- calculate_dates(src, max_period, nys, DOM, MOY)

    # Initialize output data frame
    out_hist <- initialize_output(src, wm_ident, l_period)

    # Get unique patterns
    patterns <- src %>%
        select(SEX, AGE_GROUP, AREA, CAUSE) %>%
        distinct() %>%
        mutate(patterns = paste(SEX, AGE_GROUP, AREA, CAUSE, sep = ";")) %>%
        pull(patterns)
    n_pat <- length(patterns)

    # Serial processing
    out_hist <- process_serial_hist(n_pat, patterns, src, l_period, out_hist)

    return(out_hist)
}
