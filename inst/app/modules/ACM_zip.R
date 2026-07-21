# 加载公共函数
source_module("modules/common_functions.R")

# 零膨胀泊松回归模型拟合和预测函数
fit_and_predict_zip <- function(patt_src, hist_src, l_period, nys, DOM) {
    message("Using Zero-Inflated Poisson Model...")
    flush.console()

    t.start <- Sys.time()

    # 数据预处理，与 fit_and_predict_poisson 保持一致
    src_pandemic <- patt_src
    hist_src$logdays <- log(hist_src$DAYS)
    src_pandemic$logdays <- log(src_pandemic$DAYS)

    # 周期参数
    if (l_period > 51) {
        num.cycle <- 52
        len.cycle <- 7
        num_k <- 9
    } else {
        num.cycle <- 12
        len.cycle <- 30
        num_k <- 5
    }

    # 检查零膨胀适用性
    check_zip_applicability <- function(data) {
        # na.rm so a single missing baseline count does not make zero_ratio NA
        # and crash the `if (zero_ratio < 0.05)` test below (the per-pattern
        # loop has no error handling around this).
        zero_ratio <- mean(data$NO_DEATHS == 0, na.rm = TRUE)
        message("Zero proportion: ", round(zero_ratio * 100, 1), "%")
        if (zero_ratio < 0.05) {
            warning("Low zero proportion, switching to regular Poisson")
            return(FALSE)
        }
        return(TRUE)
    }

    if (!check_zip_applicability(hist_src)) {
        family <- poisson()
    } else {
        # Let mgcv estimate the zero-inflation parameters. The previous fixed
        # scalar theta = log(mean(zeros)) was malformed (ziP's theta is a
        # length-2 slope/intercept vector, not a zero probability) and often
        # made the fit fail so the pattern was silently dropped.
        family <- ziP()
    }

    # 模型拟合
    fit <- tryCatch(
        {
            mgcv::gam(
                NO_DEATHS ~ offset(logdays) + YEAR +
                    s(PERIOD, bs = "cc", fx = TRUE, k = num_k),
                knots = list(PERIOD = c(0, num.cycle)),
                method = "REML",
                family = family,
                data = hist_src
            )
        },
        error = function(e) {
            warning("GAM fitting failed: ", conditionMessage(e))
            return(NULL)
        }
    )

    if (is.null(fit) || !fit$converged || any(is.na(coef(fit)))) {
        warning("Model failed to converge, using fallback")
        return(data.frame())
    }

    # AIC for model comparison; ziP family has limited support so wrap defensively.
    model_aic <- tryCatch(as.numeric(AIC(fit)), error = function(e) NA_real_)

    # Prediction branch depends on which family ended up being used.
    if (family$family == "poisson") {
        message("Running Poisson prediction branch...")
        estim <- mgcv::predict.gam(fit, newdata = src_pandemic, type = "link", se.fit = TRUE)

        # 调试信息
        message("Range of estim$fit: ", min(estim$fit), " to ", max(estim$fit))
        message("Range of estim$se.fit: ", min(estim$se.fit), " to ", max(estim$se.fit))

        set.seed(1)
        a <- matrix(
            rnorm(n = 1000 * length(estim$fit), mean = estim$fit, sd = estim$se.fit),
            ncol = 1000
        )
        message("Range of a before cap: ", min(a), " to ", max(a))
        a <- pmin(a, 700) # 防止溢出
        lambda_sim <- exp(a)
        message("Range of lambda_sim: ", min(lambda_sim), " to ", max(lambda_sim))

        estim.median <- apply(lambda_sim, 1, function(x) {
            mean(qpois(p = 0.5, lambda = x))
        })
        estim.lower <- apply(lambda_sim, 1, function(x) {
            mean(qpois(p = 0.025, lambda = x))
        })
        estim.upper <- apply(lambda_sim, 1, function(x) {
            mean(qpois(p = 0.975, lambda = x))
        })
    } else {
        message("Running ZIP prediction branch...")
        # For mgcv::ziP, predict on the RESPONSE scale to get the marginal
        # expected count E[Y], which already folds in the zero-inflation
        # probability. The previous code took exp() of the LINK prediction,
        # i.e. the latent (zero-truncated) Poisson mean lambda; under genuine
        # zero-inflation lambda exceeds E[Y], so expected deaths were overstated
        # and excess biased negative. Intervals are built with a normal
        # approximation on the response scale (floored at 0), then discretised
        # to counts, mirroring the Poisson branch's count-interval shape.
        estim <- mgcv::predict.gam(fit, newdata = src_pandemic, type = "response", se.fit = TRUE)

        set.seed(1)
        a <- matrix(
            rnorm(n = 1000 * length(estim$fit), mean = estim$fit, sd = estim$se.fit),
            ncol = 1000
        )
        a[a < 0] <- 0  # expected counts cannot be negative
        estim.median <- apply(a, 1, function(x) mean(qpois(p = 0.5, lambda = x)))
        estim.lower  <- apply(a, 1, function(x) mean(qpois(p = 0.025, lambda = x)))
        estim.upper  <- apply(a, 1, function(x) mean(qpois(p = 0.975, lambda = x)))
    }

    # 确保非负值
    estim.median[estim.median < 0] <- 0
    estim.lower[estim.lower < 0] <- 0
    estim.upper[estim.upper < 0] <- 0

    message(
        "Zero-Inflated Poisson pattern processing time: ",
        round(difftime(Sys.time(), t.start, units = "secs"), 1), " sec"
    )
    flush.console()

    return(list(
        src_pandemic = src_pandemic,
        estim.median = estim.median,
        estim.lower = estim.lower,
        estim.upper = estim.upper,
        aic         = model_aic
    ))
}

update_output_zip <- function(out_data, model_results, pattern, year_predict) {
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

# 零膨胀泊松回归模型主函数
fcn_zip <- function(src) {
    message("\n[fcn_zip] 开始零膨胀泊松回归模型计算...")
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
        message("Processing pattern ", j, "/", n_pat, ": ", pattern)

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

        # 模型拟合和预测
        model_results <- fit_and_predict_zip(patt_src, hist_src, l_period)

        # 如果模型失败，则跳过
        if (length(model_results) == 0 || is.data.frame(model_results)) {
            message("此模式的模型失败")
            next
        }

        # 更新输出
        year_predict <- sort(unique(out_data$YEAR))
        result <- update_output_zip(out_data, model_results, pattern, year_predict)

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

    out_data <- process_model_results(out_data, "Zero Inflated Poisson")

    attr(out_data, "aic")             <- if (length(aic_per_pattern) > 0 && !all(is.na(aic_per_pattern))) sum(aic_per_pattern, na.rm = TRUE) else NA_real_
    attr(out_data, "aic_per_pattern") <- aic_per_pattern

    end_time <- Sys.time()
    total_time <- difftime(end_time, start_time, units = "mins")
    message("零膨胀泊松回归模型计算完成，总时间: ", round(total_time, 2), " 分钟")
    flush.console()

    return(out_data)
}
