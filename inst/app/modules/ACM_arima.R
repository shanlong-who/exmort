# 加载必要的包
library(forecast)
library(dplyr)
library(lubridate)
library(ISOweek)

# 加载公共函数（假设与 NB、BSTS 和 KFAS 模型相同）
source("modules/common_functions.R")

# ARIMA 模型拟合和预测函数
fit_and_predict_arima <- function(patt_src, hist_src, l_period) {
  t.start <- Sys.time()
  message("[DEBUG] 开始 ARIMA 模型分段拟合和预测...")

  # 获取年份数量
  nys <- length(unique(patt_src$YEAR))
  message("[DEBUG] 年份数量: ", nys)

  # 检查数据是否存在
  if (nrow(hist_src) == 0) {
    message("[ERROR] 历史数据为空")
    return(data.frame())
  }
  message("[DEBUG] 历史数据行数: ", nrow(hist_src))

  # 根据周期类型设置参数（月/周）
  if (l_period > 51) {
    # 周数据
    num.cycle <- 52
    len.cycle <- 7

    if (!"DAYS" %in% names(hist_src)) {
      message("[INFO] 添加 DAYS 列到历史数据")
      hist_src$DAYS <- 7
    }

    if (!"DAYS" %in% names(patt_src)) {
      message("[INFO] 添加 DAYS 列到全部数据")
      patt_src$DAYS <- 7
    }

    hist_src$logdays <- log(hist_src$DAYS)
    src_pandemic <- patt_src %>%
      mutate(logdays = log(DAYS))
  } else {
    # 月数据
    num.cycle <- 12
    len.cycle <- 30

    if (!"DAYS" %in% names(hist_src)) {
      message("[INFO] 添加 DAYS 列到历史数据")
      DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      hist_src$DAYS <- DOM[hist_src$PERIOD]
      hist_src$DAYS[hist_src$PERIOD == 2 & hist_src$YEAR %% 4 == 0] <- 29
    }

    if (!"DAYS" %in% names(patt_src)) {
      message("[INFO] 添加 DAYS 列到全部数据")
      DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      patt_src$DAYS <- DOM[patt_src$PERIOD]
      patt_src$DAYS[patt_src$PERIOD == 2 & patt_src$YEAR %% 4 == 0] <- 29
    }

    hist_src$logdays <- log(hist_src$DAYS)
    src_pandemic <- patt_src %>%
      mutate(logdays = log(DAYS))
  }

  # 构造日期列
  if ("date" %in% names(hist_src) && "date" %in% names(src_pandemic)) {
    message("[INFO] 数据已包含 date 列，使用现有日期")
  } else {
    message("[INFO] 添加日期列")
    if (l_period > 51) {
      hist_src <- hist_src %>%
        arrange(YEAR, PERIOD) %>%
        mutate(
          iso_week = sprintf("%d-W%02d-4", YEAR, PERIOD),
          date = ISOweek2date(iso_week)
        )

      src_pandemic <- src_pandemic %>%
        arrange(YEAR, PERIOD) %>%
        mutate(
          iso_week = sprintf("%d-W%02d-4", YEAR, PERIOD),
          date = ISOweek2date(iso_week)
        )
    } else {
      hist_src <- hist_src %>%
        arrange(YEAR, PERIOD) %>%
        mutate(date = as.Date(paste(YEAR, PERIOD, "15", sep = "-")))

      src_pandemic <- src_pandemic %>%
        arrange(YEAR, PERIOD) %>%
        mutate(date = as.Date(paste(YEAR, PERIOD, "15", sep = "-")))
    }
  }

  # 检查日期列是否有效
  if (any(is.na(hist_src$date))) {
    message("[WARNING] 历史数据中有缺失的日期值")
    min_date <- min(hist_src$date, na.rm = TRUE)
    hist_src$date[is.na(hist_src$date)] <- min_date
  }

  if (any(is.na(src_pandemic$date))) {
    message("[WARNING] 全部数据中有缺失的日期值")
    min_date <- min(src_pandemic$date, na.rm = TRUE)
    src_pandemic$date[is.na(src_pandemic$date)] <- min_date
  }

  # 确保数据按时间顺序排列
  hist_src <- hist_src %>% arrange(date)
  src_pandemic <- src_pandemic %>% arrange(date)

  # 构造时间序列（用于存储实际值和预测值）
  message("[DEBUG] 构造时间序列对象...")
  if (any(is.na(hist_src$NO_DEATHS))) {
    message("[WARNING] 历史数据中有缺失的死亡数据，将替换为 NA（分段策略中会处理）")
  }

  # 初始化完整时间序列（包括实际值和预测值）
  full_ts <- rep(NA, nrow(src_pandemic))
  full_ts[1:nrow(hist_src)] <- hist_src$NO_DEATHS
  full_ts <- ts(full_ts, frequency = num.cycle)

  # 识别事件区间和非事件区间
  message("[DEBUG] 识别事件区间和非事件区间...")
  event_indices <- which(src_pandemic$event_index != "0")
  non_event_indices <- which(src_pandemic$event_index == "0")

  # 按时间顺序分组事件区间
  event_groups <- rle(src_pandemic$event_index)
  event_starts <- cumsum(c(1, event_groups$lengths))[event_groups$values != "0"]
  event_lengths <- event_groups$lengths[event_groups$values != "0"]
  event_periods <- lapply(1:length(event_starts), function(i) {
    start_idx <- event_starts[i]
    end_idx <- start_idx + event_lengths[i] - 1
    list(start_idx = start_idx, end_idx = end_idx)
  })

  message("[DEBUG] 找到 ", length(event_periods), " 个事件区间")

  # Segmented forecasting
  message("[DEBUG] Starting segmented forecast...")
  current_end_idx <- min(event_starts) - 1  # last non-event point before the first event
  full_estim.median <- rep(NA, nrow(src_pandemic))
  full_estim.lower <- rep(NA, nrow(src_pandemic))
  full_estim.upper <- rep(NA, nrow(src_pandemic))

  # AIC across all SARIMA fits in this pattern; collected here, summed by fcn_arima.
  arima_segment_aics <- numeric(0)

  for (i in 1:length(event_periods)) {
    event_start_idx <- event_periods[[i]]$start_idx
    event_end_idx <- event_periods[[i]]$end_idx
    n_forecast <- event_end_idx - event_start_idx + 1

    # 提取当前训练数据（从开始到 current_end_idx）
    # Keep the series at full length and interpolate internal NAs instead of
    # dropping them. Dropping NAs and re-wrapping in ts(frequency = num.cycle)
    # reset the seasonal phase (the first surviving point became "season 1"),
    # so any missing week/month misaligned the whole seasonal cycle. na.interp
    # preserves the phase; fall back to the old NA-drop only if it errors.
    train_data <- ts(full_ts[1:current_end_idx], frequency = num.cycle)
    train_data <- tryCatch(
      if (anyNA(train_data)) forecast::na.interp(train_data) else train_data,
      error = function(e) ts(as.numeric(train_data)[!is.na(as.numeric(train_data))],
                             frequency = num.cycle)
    )

    if (length(train_data) < 10) {
      message("[WARNING] 训练数据不足，跳过事件区间 ", i)
      next
    }

    # Fit SARIMA model
    message("[DEBUG] Fitting SARIMA (event interval ", i, ")...")
    sarima_model <- tryCatch({
      auto.arima(train_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
    }, error = function(e) {
      message("[ERROR] ARIMA fit failed (event interval ", i, "): ", e$message)
      return(NULL)
    })

    if (is.null(sarima_model)) {
      message("[ERROR] ARIMA fit failed (event interval ", i, "), skipping")
      next
    }

    # Capture AIC from this segment fit; aggregated across all segments below.
    arima_segment_aics <- c(arima_segment_aics, tryCatch(as.numeric(AIC(sarima_model)), error = function(e) NA_real_))

    # 预测当前事件区间
    message("[DEBUG] 预测事件区间 ", i, "（", n_forecast, " 个周期）...")
    forecast_result <- tryCatch({
      forecast(sarima_model, h = n_forecast, level = 95)
    }, error = function(e) {
      message("[ERROR] 预测失败（事件区间 ", i, "）: ", e$message)
      return(NULL)
    })

    if (is.null(forecast_result)) {
      message("[ERROR] 预测失败（事件区间 ", i, "），跳过")
      next
    }

    # 提取预测值和置信区间
    pred.median <- as.numeric(forecast_result$mean)
    pred.lower <- as.numeric(forecast_result$lower[, 1])
    pred.upper <- as.numeric(forecast_result$upper[, 1])

    # 填入预测值
    full_estim.median[event_start_idx:event_end_idx] <- pred.median
    full_estim.lower[event_start_idx:event_end_idx] <- pred.lower
    full_estim.upper[event_start_idx:event_end_idx] <- pred.upper

    # 将预测值填入训练数据
    full_ts[event_start_idx:event_end_idx] <- pred.median

    # 更新 current_end_idx 为下一个非事件区间的结束
    if (i < length(event_periods)) {
      next_event_start_idx <- event_periods[[i + 1]]$start_idx
      current_end_idx <- next_event_start_idx - 1
    } else {
      current_end_idx <- nrow(src_pandemic)
    }
  }

  # 填充非事件区间的拟合值
  message("[DEBUG] 填充非事件区间的拟合值...")
  # Interpolate internal NAs rather than dropping them, so the fitted-value
  # length stays aligned with non_event_indices (a shorter NA-dropped series
  # made the positional assignment below land on the wrong periods).
  non_event_data <- ts(full_ts[non_event_indices], frequency = num.cycle)
  non_event_data <- tryCatch(
    if (anyNA(non_event_data)) forecast::na.interp(non_event_data) else non_event_data,
    error = function(e) ts(as.numeric(non_event_data)[!is.na(as.numeric(non_event_data))],
                           frequency = num.cycle)
  )

  if (length(non_event_data) > 0) {
    sarima_model <- tryCatch({
      auto.arima(non_event_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
    }, error = function(e) {
      message("[ERROR] 非事件区间拟合失败: ", e$message)
      return(NULL)
    })

    if (!is.null(sarima_model)) {
      arima_segment_aics <- c(arima_segment_aics, tryCatch(as.numeric(AIC(sarima_model)), error = function(e) NA_real_))
      fit_result <- fitted(sarima_model)
      sigma <- sqrt(sarima_model$sigma2)
      fit.median <- as.numeric(fit_result)
      fit.lower <- fit.median - 1.96 * sigma
      fit.upper <- fit.median + 1.96 * sigma

      # 确保长度匹配
      n_fit <- min(length(non_event_indices), length(fit.median))
      full_estim.median[non_event_indices[1:n_fit]] <- fit.median[1:n_fit]
      full_estim.lower[non_event_indices[1:n_fit]] <- fit.lower[1:n_fit]
      full_estim.upper[non_event_indices[1:n_fit]] <- fit.upper[1:n_fit]
    }
  }

  # 处理剩余的缺失值
  missing_indices <- which(is.na(full_estim.median))
  if (length(missing_indices) > 0) {
    message("[INFO] 有 ", length(missing_indices), " 个时间点缺失预测值，使用平均值填充")
    valid_indices <- which(!is.na(full_estim.median))
    if (length(valid_indices) > 0) {
      avg_median <- mean(full_estim.median[valid_indices], na.rm = TRUE)
      avg_lower <- mean(full_estim.lower[valid_indices], na.rm = TRUE)
      avg_upper <- mean(full_estim.upper[valid_indices], na.rm = TRUE)

      full_estim.median[missing_indices] <- avg_median
      full_estim.lower[missing_indices] <- avg_lower
      full_estim.upper[missing_indices] <- avg_upper
    }
  }

  # 确保非负值
  full_estim.median[full_estim.median < 0] <- 0
  full_estim.lower[full_estim.lower < 0] <- 0
  full_estim.upper[full_estim.upper < 0] <- 0

  message("模式处理时间: ", round(difftime(Sys.time(), t.start, units = "secs"), 1), " 秒")

  return(list(
    src_pandemic    = src_pandemic,
    estim.median    = full_estim.median,
    estim.lower     = full_estim.lower,
    estim.upper     = full_estim.upper,
    forecast_result = data.frame(),
    # ARIMA fits one SARIMA per event segment; summing their AICs is NOT a
    # comparable model-selection statistic (the sum scales with the number of
    # events and mixes fits on different data subsets), and a Gaussian
    # time-series likelihood is not on the same scale as the count-regression
    # AICs (NB/Poisson) in any case. Report NA so the Best Fit tab excludes
    # ARIMA, as it already does for Historical Average and Quasi-Poisson.
    aic             = NA_real_,
    aic_segments    = arima_segment_aics
  ))
}



# 更新输出函数（统一处理所有预测结果）
update_output_arima <- function(out_data, model_results, pattern, year_predict) {
  message("[DEBUG] update_output_arima 开始处理模式: ", pattern)

  # 检查模型结果是否存在
  if (is.null(model_results)) {
    message("[ERROR] 模型结果为 NULL")
    return(out_data)
  }

  src_pandemic <- model_results$src_pandemic
  estim.median <- model_results$estim.median
  estim.lower <- model_results$estim.lower
  estim.upper <- model_results$estim.upper

  if (is.null(src_pandemic) || is.null(estim.median) || is.null(estim.lower) || is.null(estim.upper)) {
    message("[WARNING] 模式 ", pattern, " 的模型结果缺失必要组件")
    return(out_data)
  }

  # 检查数据结构
  message("[DEBUG] src_pandemic 行数: ", nrow(src_pandemic))
  message("[DEBUG] estim.median 长度: ", length(estim.median))
  message("[DEBUG] 模式分解: ", paste(strsplit(pattern, ";")[[1]], collapse = ", "))

  # 分解模式
  pattern_parts <- tryCatch({
    strsplit(pattern, ";")[[1]]
  }, error = function(e) {
    message("[ERROR] 无法分解模式: ", e$message)
    return(c("Total", "Total", "Total", "Total"))
  })

  # 确保模式有四个部分
  if (length(pattern_parts) < 4) {
    message("[WARNING] 模式部分不足 4 个，添加缺失的部分")
    pattern_parts <- c(pattern_parts, rep("Total", 4 - length(pattern_parts)))
  }

  # 初始化结果数据框
  result_df <- out_data[0, ]

  # 获取周期数和预测年份数
  l_period <- max(out_data$PERIOD, na.rm = TRUE)
  nyear_predict <- length(year_predict)

  message("[DEBUG] 周期数: ", l_period, ", 预测年份数: ", nyear_predict)
  message("[DEBUG] 预测年份: ", paste(year_predict, collapse = ", "))

  # 处理每个年份和周期
  for (iyear_predict in 1:nyear_predict) {
    y <- year_predict[iyear_predict]
    for (k in 1:l_period) {
      # 在 src_pandemic 中查找匹配的记录
      a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == k

      # 如果没有匹配的记录，跳过
      if (sum(a) == 0) {
        next
      }

      # 在 out_data 中查找匹配的记录
      current_records <- tryCatch({
        out_data[
          out_data$SEX == pattern_parts[1] &
          out_data$AGE_GROUP == pattern_parts[2] &
          out_data$AREA == pattern_parts[3] &
          out_data$CAUSE == pattern_parts[4] &
          out_data$YEAR == y &
          out_data$PERIOD == k,
        ]
      }, error = function(e) {
        message("[ERROR] 查询当前记录失败: ", e$message)
        return(out_data[0, ])
      })

      # 如果找到匹配的记录并且有匹配的 src_pandemic 记录，则更新记录
      if (nrow(current_records) > 0 && sum(a) > 0) {
        # 安全获取索引
        a_indices <- which(a)
        if (length(a_indices) > 0) {
          # 只使用第一个匹配的索引，避免多个匹配导致的问题
          idx <- a_indices[1]
          if (idx <= length(estim.median) && idx <= length(estim.lower) && idx <= length(estim.upper)) {
            current_records$ESTIMATE <- estim.median[idx]
            current_records$LOWER_LIMIT <- estim.lower[idx]
            current_records$UPPER_LIMIT <- estim.upper[idx]
            result_df <- rbind(result_df, current_records)
          } else {
            message("[WARNING] 索引超出范围: ", idx, " > ",
                    min(length(estim.median), length(estim.lower), length(estim.upper)))
          }
        }
      }
    }
  }

  message("[DEBUG] update_output_arima 完成，返回行数: ", nrow(result_df))
  return(result_df)
}

# ARIMA 模型主函数
fcn_arima <- function(src) {
  message("\n[fcn_arima] 开始 ARIMA 模型计算...")
  flush.console()
  start_time <- Sys.time()

  # 检查输入数据
  if (is.null(src) || nrow(src) == 0) {
    message("[ERROR] 输入数据为空")
    return(list(predictions = data.frame()))
  }

  message("[INFO] 输入数据行数: ", nrow(src))

  # 预处理数据
  src <- tryCatch({
    src %>%
      filter(PERIOD <= 52) %>%
      arrange(SEX, AGE_GROUP, AREA, CAUSE, YEAR, PERIOD) %>%
      mutate(NO_DEATHS = as.numeric(NO_DEATHS))
  }, error = function(e) {
    message("[ERROR] 数据预处理失败: ", e$message)
    return(list(predictions = data.frame()))
  })

  if (is.null(src) || nrow(src) == 0) {
    message("[ERROR] 预处理后数据为空")
    return(list(predictions = data.frame()))
  }

  # 获取基本参数
  nys <- length(unique(src$YEAR))
  max_period <- max(src$PERIOD, na.rm = TRUE)
  wm_ident <- ifelse(max_period == 12, "Month", "Week")
  l_period <- ifelse(max_period == 12, 12, 52)

  message("[INFO] 年份数: ", nys, ", 最大周期: ", max_period, ", 标识符: ", wm_ident)

  # 计算日期 - 使用自定义方法而不是 calculate_dates 函数
  message("[INFO] 计算日期...")

  # 检查是否已经有日期列
  if ("date" %in% names(src)) {
    message("[INFO] 数据已包含 date 列，使用现有日期")
  } else {
    message("[INFO] 数据中没有 date 列，创建日期")

    # 如果没有日期列，根据周期类型创建
    if (max_period == 12) {
      # 月数据
      src$date <- as.Date(paste(src$YEAR, src$PERIOD, "15", sep = "-"))
    } else {
      # 周数据 - 使用 ISOweek 包
      dates <- character(nrow(src))
      for (i in 1:nrow(src)) {
        dates[i] <- sprintf("%d-W%02d-4", src$YEAR[i], src$PERIOD[i])
      }
      src$date <- as.Date(ISOweek2date(dates))
    }
  }

  # 检查是否已经有 DAYS 列
  if (!"DAYS" %in% names(src)) {
    message("[INFO] 添加 DAYS 列")
    if (max_period == 12) {
      # 月数据
      DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      src$DAYS <- DOM[src$PERIOD]
      # 闰年调整
      src$DAYS[src$PERIOD == 2 & src$YEAR %% 4 == 0] <- 29
    } else {
      # 周数据
      src$DAYS <- 7
    }
  }

  # 初始化输出数据框
  out_data <- tryCatch({
    initialize_output(src, wm_ident, l_period)
  }, error = function(e) {
    message("[ERROR] 初始化输出数据框失败: ", e$message)
    return(list(predictions = data.frame()))
  })

  if (is.null(out_data) || nrow(out_data) == 0) {
    message("[ERROR] 初始化输出数据框为空")
    return(list(predictions = data.frame()))
  }

  # 获取唯一模式
  patterns <- tryCatch({
    src %>%
      select(SEX, AGE_GROUP, AREA, CAUSE) %>%
      distinct() %>%
      mutate(patterns = paste(SEX, AGE_GROUP, AREA, CAUSE, sep = ";")) %>%
      pull(patterns)
  }, error = function(e) {
    message("[ERROR] 获取唯一模式失败: ", e$message)
    return(character(0))
  })

  n_pat <- length(patterns)
  if (n_pat == 0) {
    message("[ERROR] 没有可用的模式")
    return(list(predictions = data.frame()))
  }

  message("[INFO] 找到 ", n_pat, " 个模式")

  # Per-pattern results plus a parallel vector of AIC values for the
  # Best Fit tab.
  results <- list()
  forecast_results <- list()
  aic_per_pattern <- numeric(0)

  for (j in 1:n_pat) {
    pattern <- patterns[j]
    message("[INFO] 处理模式 ", j, "/", n_pat, ": ", pattern)

    # 提取模式数据
    pattern_parts <- tryCatch({
      strsplit(pattern, ";")[[1]]
    }, error = function(e) {
      message("[ERROR] 分解模式失败: ", e$message)
      return(NULL)
    })

    if (is.null(pattern_parts) || length(pattern_parts) < 4) {
      message("[WARNING] 模式格式不正确，跳过")
      next
    }

    patt_src <- tryCatch({
      src[
        src$SEX == pattern_parts[1] &
        src$AGE_GROUP == pattern_parts[2] &
        src$AREA == pattern_parts[3] &
        src$CAUSE == pattern_parts[4],
      ]
    }, error = function(e) {
      message("[ERROR] 提取模式数据失败: ", e$message)
      return(NULL)
    })

    if (is.null(patt_src)) {
      message("[WARNING] 无法提取模式数据，跳过")
      next
    }

    # 如果数据不足，则跳过
    if (nrow(patt_src) < 10) {
      message("[WARNING] 由于数据不足，跳过模式 (", nrow(patt_src), " 行)")
      next
    }

    # 准备历史数据
    # hist_src <- tryCatch({
    #   patt_src[patt_src$event_index == "0", ]
    # }, error = function(e) {
    #   message("[ERROR] 提取历史数据失败: ", e$message)
    #   return(NULL)
    # })
    hist_src <- patt_src
    # hist_src$NO_DEATHS[hist_src$event_index != "0"] <- NA

    if (is.null(hist_src)) {
      message("[WARNING] 无法提取历史数据，跳过")
      next
    }

    # 如果历史数据不足，则跳过
    if (nrow(hist_src) < 10) {
      message("[WARNING] 由于历史数据不足，跳过模式 (", nrow(hist_src), " 行)")
      next
    }

    # 模型拟合和预测
    message("[INFO] 开始模型拟合和预测...")
    model_results <- tryCatch({
      fit_and_predict_arima(patt_src, hist_src, l_period)
    }, error = function(e) {
      message("[ERROR] 模型拟合和预测失败: ", e$message)
      return(NULL)
    })

    # 如果模型失败，则跳过
    if (is.null(model_results) || length(model_results) == 0 || is.data.frame(model_results)) {
      message("[WARNING] 此模式的模型失败")
      next
    }

    # 更新输出
    message("[INFO] 更新输出结果...")
    year_predict <- sort(unique(out_data$YEAR))
    result <- tryCatch({
      update_output_arima(out_data, model_results, pattern, year_predict)
    }, error = function(e) {
      message("[ERROR] 更新输出失败: ", e$message)
      return(NULL)
    })

    if (is.null(result)) {
      message("[WARNING] 更新输出结果为空，跳过")
      next
    }

    if (nrow(result) > 0) {
      results[[j]] <- result
      forecast_results[[j]] <- model_results$forecast_result
      aic_per_pattern <- c(aic_per_pattern, model_results$aic)
      message("[INFO] Processed pattern: ", pattern, " (", nrow(result), " rows)")
    } else {
      message("[WARNING] Pattern produced empty result: ", pattern)
    }
  }

  # 合并结果
  message("[INFO] 合并所有模式的结果...")
  out_data <- NULL
  if (length(results) > 0) {
    # 过滤掉空结果
    valid_results <- results[!sapply(results, is.null)]
    if (length(valid_results) > 0) {
      out_data <- tryCatch({
        do.call(rbind, valid_results)
      }, error = function(e) {
        message("[ERROR] 合并结果失败: ", e$message)
        return(NULL)
      })
      message("[INFO] 成功合并 ", length(valid_results), " 个模式的结果")
    } else {
      message("[WARNING] 没有有效的模式结果可合并")
    }
  } else {
    message("[WARNING] 没有成功处理的模式!")
  }

  if (is.null(out_data) || nrow(out_data) == 0) {
    message("[ERROR] 最终结果为空")
    return(list(predictions = data.frame()))
  }

  # 处理结果
  message("[INFO] 处理最终结果...")
  if (!is.null(out_data)) {
    out_data <- tryCatch({
      process_model_results(out_data, "ARIMA")
    }, error = function(e) {
      message("[ERROR] 处理最终结果失败: ", e$message)
      return(out_data)  # 返回原始结果，而不是失败
    })
  }

  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  message("[INFO] ARIMA 模型计算完成，总时间: ", round(total_time, 2), " 分钟")
  flush.console()

  if (!is.null(out_data)) {
    message("[INFO] Converting ARIMA result to tbl_df")
    out_data <- dplyr::as_tibble(out_data)
  }

  # Attach AIC summary for the Best Fit tab. ARIMA per-pattern AIC is NA (see
  # fit_and_predict_arima), so this resolves to NA and ARIMA is excluded from
  # the best-fit comparison rather than collapsing a sum of NAs to 0.
  attr(out_data, "aic")             <- if (any(is.finite(aic_per_pattern))) sum(aic_per_pattern, na.rm = TRUE) else NA_real_
  attr(out_data, "aic_per_pattern") <- aic_per_pattern

  return(out_data)
}

# 计算超额死亡
calculate_excess_deaths <- function(recorded_deaths, expected_deaths) {
  excess_deaths <- ifelse(is.na(recorded_deaths), NA, recorded_deaths - expected_deaths$ESTIMATE)
  excess_deaths_lower <- ifelse(is.na(recorded_deaths), NA, recorded_deaths - expected_deaths$UPPER_LIMIT)
  excess_deaths_upper <- ifelse(is.na(recorded_deaths), NA, recorded_deaths - expected_deaths$LOWER_LIMIT)
  return(data.frame(
    Excess_Deaths = excess_deaths,
    Excess_Deaths_Lower = excess_deaths_lower,
    Excess_Deaths_Upper = excess_deaths_upper
  ))
}

# 主函数：计算超额死亡
calculate_acm_excess_arima <- function(src) {
  # 运行 ARIMA 模型
  arima_results <- fcn_arima(src)

  # 提取预测结果
  predictions <- arima_results$predictions
  forecast_results <- arima_results$forecast_results

  # 提取实际死亡人数
  recorded_deaths <- src %>%
    arrange(SEX, AGE_GROUP, AREA, CAUSE, YEAR, PERIOD) %>%
    pull(NO_DEATHS)

  # 计算超额死亡
  if (!is.null(predictions)) {
    excess_deaths <- calculate_excess_deaths(recorded_deaths, predictions)
    predictions <- cbind(predictions, excess_deaths)
  }

  return(list(
    predictions = predictions,
    forecast_results = forecast_results  # 返回 forecast_results 用于可视化
  ))
}