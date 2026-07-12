# 加载公共函数
source("modules/common_functions.R")

# 负二项回归模型拟合和预测函数
fit_and_predict_nb <- function(patt_src, hist_src, l_period) {
    t.start <- Sys.time()

    # 获取年份数量
    nys <- length(unique(patt_src$YEAR))

    # 设置月份天数
    DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    # Continuous day-offset per year, anchored to the first year in the data
    # (not a hardcoded 2015). loc_DATE is not a model predictor, but the old
    # `YEAR - 2014` indexing threw a mixed-subscript error for any pre-2015
    # year, aborting the fit; sizing to the actual span keeps it safe.
    yr0 <- min(patt_src$YEAR, na.rm = TRUE)
    year_offsets <- cumsum(c(0, rep(365, max(patt_src$YEAR, na.rm = TRUE) - yr0 + 1)))
    # Leap-aware days in each month, from the calendar (no positional hacks).
    month_days <- function(df) as.numeric(lubridate::days_in_month(
        as.Date(sprintf("%04d-%02d-01", df$YEAR, df$PERIOD))))

    # 根据周期类型设置参数（月/周）
    if (l_period > 51) {
        # 周数据
        day <- cumsum(c(0, rep(7, 52)))[patt_src$PERIOD] + 3.5
        aDATE <- year_offsets[patt_src$YEAR - yr0 + 1] + day
        num.cycle <- 52
        len.cycle <- 7
        hist_src$logdays <- log(7)
        src_pandemic <- patt_src %>%
            mutate(loc_DATE = aDATE, logdays = log(7))

        # Fit the model on the historical (non-event) data.
        fit <- tryCatch({
            mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR +
                     s(PERIOD, bs = "cc", fx = TRUE, k = 9),
                     knots = list(PERIOD = c(0, num.cycle)),
                     method = "REML", family = nb(), data = hist_src)
        }, error = function(e) {
            warning("Negative Binomial fit failed: ", e$message)
            return(NULL)
        })
        if (is.null(fit)) {
            return(data.frame())
        }
    } else {
        # Monthly data
        day <- cumsum(c(0, DOM))[patt_src$PERIOD] + 15
        aDATE <- year_offsets[patt_src$YEAR - yr0 + 1] + day
        num.cycle <- 12
        len.cycle <- 30
        # Exposure offset = actual number of days in each month, computed from
        # the calendar. Replaces a positional days[14]/days[62] <- 29 hack that
        # assumed the series started Jan 2015 and put the 29-day February on
        # the wrong month for any other start.
        hist_src$logdays <- log(month_days(hist_src))
        src_pandemic <- patt_src %>%
            mutate(loc_DATE = aDATE, logdays = log(month_days(patt_src)))

        fit <- tryCatch({
            mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR +
                     s(PERIOD, bs = "cc", fx = TRUE, k = 5),
                     knots = list(PERIOD = c(0, num.cycle)),
                     method = "REML", family = nb(), data = hist_src)
        }, error = function(e) {
            warning("Negative Binomial fit failed: ", e$message)
            return(NULL)
        })
        if (is.null(fit)) return(data.frame())
    }

    # AIC for model comparison; some families (quasi) return NA, which is fine.
    model_aic <- tryCatch(as.numeric(AIC(fit)), error = function(e) NA_real_)

    # 模型预测
    estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE)
    theta <- fit$family$getTheta(TRUE)

    # 从后验分布模拟
    set.seed(1)
    a <- matrix(rnorm(n = 1000 * length(estim$fit),
                     mean = estim$fit, sd = estim$se.fit),
               ncol = 1000)
    estim.median <- apply(a, 1, function(x) {
        mean(qnbinom(p = 0.5, mu = exp(x), size = theta))
    })
    estim.lower <- apply(a, 1, function(x) {
        mean(qnbinom(p = 0.025, mu = exp(x), size = theta))
    })
    estim.upper <- apply(a, 1, function(x) {
        mean(qnbinom(p = 0.975, mu = exp(x), size = theta))
    })

    # 确保非负值
    estim.median[estim.median < 0] <- 0
    estim.lower[estim.lower < 0] <- 0
    estim.upper[estim.upper < 0] <- 0

    message("模式处理时间: ", round(difftime(Sys.time(), t.start, units = "secs"), 1), " 秒")

    return(list(
        src_pandemic = src_pandemic,
        estim.median = estim.median,
        estim.lower = estim.lower,
        estim.upper = estim.upper,
        aic         = model_aic
    ))
}

# 更新输出函数
update_output_nb <- function(out_data, model_results, pattern, year_predict) {
    # 提取模型结果
    src_pandemic <- model_results$src_pandemic
    estim.median <- model_results$estim.median
    estim.lower <- model_results$estim.lower
    estim.upper <- model_results$estim.upper

    # 检查模型结果是否有效
    if (is.null(src_pandemic) || is.null(estim.median) || is.null(estim.lower) || is.null(estim.upper)) {
        message("警告: 模式 ", pattern, " 的模型结果无效")
        return(out_data)
    }

    # 提取模式数据
    pattern_parts <- strsplit(pattern, ";")[[1]]

    # 初始化结果数据框
    result_df <- out_data[0, ]

    # 处理每个年份和周期
    l_period <- max(out_data$PERIOD, na.rm = TRUE)
    nyear_predict <- length(year_predict)
    for (iyear_predict in 1:nyear_predict) {
        y <- year_predict[iyear_predict]
        for (k in 1:l_period) {
            # 在src_pandemic中查找匹配的记录
            a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == k

            # 在out_data中查找匹配的记录
            current_records <- out_data[
                out_data$SEX == pattern_parts[1] &
                out_data$AGE_GROUP == pattern_parts[2] &
                out_data$AREA == pattern_parts[3] &
                out_data$CAUSE == pattern_parts[4] &
                out_data$YEAR == y &
                out_data$PERIOD == k,
            ]

            # 如果找到匹配的记录并且有匹配的src_pandemic记录，则更新记录
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

# 负二项回归模型主函数
fcn_nb <- function(src) {
    message("\n[fcn_nb] 开始负二项回归模型计算...")
    flush.console()
    start_time <- Sys.time()

    # 预处理数据
    src <- src %>%
        filter(PERIOD <= 52) %>%
        arrange(SEX, AGE_GROUP, AREA, CAUSE, YEAR, PERIOD) %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS))

    # 获取基本参数
    nys <- length(unique(src$YEAR))
    max_period <- max(src$PERIOD, na.rm = TRUE)
    wm_ident <- ifelse(max_period == 12, "Month", "Week")
    l_period <- ifelse(max_period == 12, 12, 52)

    # 计算日期
    src <- calculate_dates(src, max_period, nys, DOM, MOY)

    # 初始化输出数据框
    out_data <- initialize_output(src, wm_ident, l_period)

    # 获取唯一模式
    patterns <- src %>%
        select(SEX, AGE_GROUP, AREA, CAUSE) %>%
        distinct() %>%
        mutate(patterns = paste(SEX, AGE_GROUP, AREA, CAUSE, sep = ";")) %>%
        pull(patterns)
    n_pat <- length(patterns)

    # Per-pattern results plus a parallel vector of AIC values for the
    # Best Fit tab.
    results <- list()
    aic_per_pattern <- numeric(0)

    for (j in 1:n_pat) {
        pattern <- patterns[j]
        message("处理模式 ", j, "/", n_pat, ": ", pattern)

        # 提取模式数据
        pattern_parts <- strsplit(pattern, ";")[[1]]
        patt_src <- src[
            src$SEX == pattern_parts[1] &
            src$AGE_GROUP == pattern_parts[2] &
            src$AREA == pattern_parts[3] &
            src$CAUSE == pattern_parts[4],
        ]

        # 如果数据不足，则跳过
        if (nrow(patt_src) < 10) {
            message("由于数据不足，跳过模式")
            next
        }

        # 准备历史数据
        hist_src <- patt_src[patt_src$event_index == "0", ]

        # 如果历史数据不足，则跳过
        if (nrow(hist_src) < 10) {
            message("由于历史数据不足，跳过模式")
            next
        }

        # Fit and predict for this pattern.
        model_results <- fit_and_predict_nb(patt_src, hist_src, l_period)

        if (length(model_results) == 0 || is.data.frame(model_results)) {
            message("Model failed for this pattern")
            next
        }

        year_predict <- sort(unique(out_data$YEAR))
        result <- update_output_nb(out_data, model_results, pattern, year_predict)

        if (nrow(result) > 0) {
            results[[j]] <- result
            aic_per_pattern <- c(aic_per_pattern, model_results$aic)
            message("Processed pattern: ", pattern, " (", nrow(result), " rows)")
        }
    }

    # 合并结果
    if (length(results) > 0) {
        out_data <- do.call(rbind, results[!sapply(results, is.null)])
        message("成功合并 ", length(results[!sapply(results, is.null)]), " 个模式的结果")
    } else {
        warning("没有成功处理的模式!")
        return(NULL)
    }

    out_data <- process_model_results(out_data, "Negative Binomial")

    # Attach AIC summary across patterns for the Best Fit tab.
    attr(out_data, "aic")             <- sum(aic_per_pattern, na.rm = TRUE)
    attr(out_data, "aic_per_pattern") <- aic_per_pattern

    end_time <- Sys.time()
    total_time <- difftime(end_time, start_time, units = "mins")
    message("Negative Binomial model done, total time: ", round(total_time, 2), " minutes")
    flush.console()

    return(out_data)
}
