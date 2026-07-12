# 公共函数文件
# 包含所有模型共用的功能函数

# 加载必要的库
library(dplyr)
library(reshape2)
library(readxl)
library(stringr)
library(mgcv)

# 定义常量
DOM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
MOY <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 1. Date calculation.
# The continuous DATE index is anchored to the first year in the data
# (min(YEAR)) rather than a hardcoded 2015. The old `src$YEAR - 2014` indexing
# produced NA for any series not starting in 2015 and, worse, threw a
# "can't mix positive and negative subscripts" error for any pre-2015 year,
# which killed every model that calls this. Sizing the offset vector to the
# actual span keeps the index in [1, span+1] for any input.
calculate_dates <- function(src, max_period, nys, DOM, MOY) {
    yr0 <- min(src$YEAR, na.rm = TRUE)
    span <- max(src$YEAR, na.rm = TRUE) - yr0 + 1
    year_offsets <- cumsum(c(0, rep(365, span)))
    if (max_period == 12) {
        day <- cumsum(c(0, DOM))[src$PERIOD] + 15
        src$DATE <- year_offsets[src$YEAR - yr0 + 1] + day
    } else {
        day <- as.numeric(substr(src$DATE_TO_SPECIFY_WEEK, 5, 6))
        if (all(!is.na(day))) {
            Date <- match(substr(src$DATE_TO_SPECIFY_WEEK, 1, 3), MOY)
            day <- cumsum(c(0, DOM))[Date] + day - 3.5
            loc_YEAR <- src$YEAR
            src$DATE <- year_offsets[loc_YEAR - yr0 + 1] + day
        } else {
            day <- cumsum(c(0, rep(7, 52)))[src$PERIOD] + 3.5
            src$DATE <- year_offsets[src$YEAR - yr0 + 1] + day
        }
    }
    return(src)
}

# 2. 初始化输出函数
initialize_output <- function(src, wm_ident, l_period) {
    # 创建输出数据框
    out_data <- src %>%
        # filter(event_index != "0") %>%
        mutate(
            WM_IDENTIFIER = wm_ident,
            SE_IDENTIFIER = "ACM",
            ESTIMATE = NA,
            LOWER_LIMIT = NA,
            UPPER_LIMIT = NA,
            EXCESS_DEATHS = NA
        )

    return(out_data)
}

# 3. 最终处理函数
finalize_output <- function(out_data, wm_ident, model_type = "Generic") {
    # 调试输出
    message("[DEBUG] finalize_output called with model_type: ", model_type)
    message("[DEBUG] out_data dimensions: ", nrow(out_data), " x ", ncol(out_data))
    message("[DEBUG] out_data columns: ", paste(names(out_data), collapse = ", "))
    flush.console()

    # 检查数据是否有效
    if (nrow(out_data) == 0) {
        message("[DEBUG] Warning: out_data is empty")
        flush.console()
        return(out_data)
    }

    # 安全转换列为数值类型
    out_data <- out_data %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS)) %>%
        mutate(ESTIMATE = as.numeric(ESTIMATE)) %>%
        mutate(EXCESS_DEATHS = NO_DEATHS - ESTIMATE)

    # 检查是否存在必要的列
    required_cols <- c("COUNTRY", "ISO3", "WM_IDENTIFIER", "YEAR", "PERIOD",
                      "SEX", "AGE_GROUP", "AREA", "CAUSE", "DATE_TO_SPECIFY_WEEK",
                      "SE_IDENTIFIER", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS",
                      "event_name", "event_index")

    missing_cols <- setdiff(required_cols, names(out_data))
    if (length(missing_cols) > 0) {
        message("[DEBUG] Warning: Missing columns in out_data: ", paste(missing_cols, collapse = ", "))
        flush.console()
        # 添加缺失的列，值为NA
        for (col in missing_cols) {
            out_data[[col]] <- NA
        }
    }

    # 继续处理
    out_data <- out_data %>%
        melt(id.vars = c("COUNTRY", "ISO3", "WM_IDENTIFIER", "YEAR", "PERIOD",
                        "SEX", "AGE_GROUP", "AREA", "CAUSE", "DATE_TO_SPECIFY_WEEK",
                        "SE_IDENTIFIER", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS",
                        "event_name", "event_index")) %>%
        rename(SERIES = variable, NO_DEATHS = value) %>%
        mutate(SERIES = case_when(
            SERIES == "NO_DEATHS" ~ "Current deaths",
            SERIES == "ESTIMATE" ~ paste0(model_type, " model"),
            TRUE ~ as.character(SERIES)
        )) %>%
        filter(!is.na(SERIES), YEAR != "") %>%
        group_by(COUNTRY, ISO3, WM_IDENTIFIER, YEAR, PERIOD,
                 SEX, AGE_GROUP, AREA, CAUSE, DATE_TO_SPECIFY_WEEK,
                 SE_IDENTIFIER, LOWER_LIMIT, UPPER_LIMIT, EXCESS_DEATHS,
                 event_name, event_index) %>%
        reframe(
            EXP_DEATHS = NO_DEATHS[SERIES == paste0(model_type, " model")],
            NO_DEATHS = NO_DEATHS[SERIES == "Current deaths"],
            SERIES = paste0(model_type, " model")
        ) %>%
        arrange(AREA, SEX, AGE_GROUP, YEAR, PERIOD)

    message("[DEBUG] finalize_output completed with result dimensions: ", nrow(out_data), " x ", ncol(out_data))
    flush.console()
    return(out_data)
}

# 4. 计算按事件和模式的汇总统计
calculate_summary_by_event <- function(out_data) {
    # 调试输出
    message("[DEBUG] calculate_summary_by_event called")
    message("[DEBUG] out_data dimensions: ", nrow(out_data), " x ", ncol(out_data))
    message("[DEBUG] out_data columns: ", paste(names(out_data), collapse = ", "))
    flush.console()

    # 检查数据是否有效
    if (nrow(out_data) == 0) {
        message("[DEBUG] Warning: out_data is empty")
        flush.console()
        return(data.frame())
    }

    # 检查是否存在必要的列
    required_cols <- c("NO_DEATHS", "EXCESS_DEATHS", "EXP_DEATHS", "UPPER_LIMIT", "LOWER_LIMIT", "event_index")
    missing_cols <- setdiff(required_cols, names(out_data))

    if (length(missing_cols) > 0) {
        message("[DEBUG] Warning: Missing columns in out_data: ", paste(missing_cols, collapse = ", "))
        flush.console()
        # 创建最小的汇总，使用可用数据
        if ("NO_DEATHS" %in% names(out_data)) {
            summary_by_event <- out_data %>%
                mutate(NO_DEATHS = as.numeric(NO_DEATHS)) %>%
                group_by(COUNTRY, ISO3, WM_IDENTIFIER, SEX, AGE_GROUP, AREA, CAUSE, event_index, event_name) %>%
                reframe(
                    TOTAL_DEATHS = sum(NO_DEATHS, na.rm = TRUE),
                    TOTAL_EXPECTED = NA,
                    TOTAL_EXCESS = NA,
                    TOTAL_SE = NA,
                    EXCESS_LOWER = NA,
                    EXCESS_UPPER = NA,
                    P_SCORE = NA
                )
            return(summary_by_event)
        } else {
            # 如果没有NO_DEATHS列，无法创建汇总
            message("[DEBUG] Cannot create summary: NO_DEATHS column missing")
            flush.console()
            return(data.frame())
        }
    }

    # 计算按事件和模式的汇总统计
    tryCatch({
        summary_by_event <- out_data %>%
            mutate(NO_DEATHS = as.numeric(NO_DEATHS)) %>%
            mutate(EXCESS_DEATHS = as.numeric(EXCESS_DEATHS)) %>%
            mutate(EXP_DEATHS = as.numeric(EXP_DEATHS)) %>%
            mutate(UPPER_LIMIT = as.numeric(UPPER_LIMIT)) %>%
            mutate(LOWER_LIMIT = as.numeric(LOWER_LIMIT)) %>%
            filter(event_index != "0" & !is.na(NO_DEATHS) & !is.na(event_index)) %>%
            group_by(COUNTRY, ISO3, WM_IDENTIFIER, SEX, AGE_GROUP, AREA, CAUSE, event_index, event_name) %>%
            reframe(
                TOTAL_DEATHS = sum(NO_DEATHS, na.rm = TRUE),
                TOTAL_EXPECTED = sum(EXP_DEATHS, na.rm = TRUE),
                TOTAL_EXCESS = sum(NO_DEATHS - EXP_DEATHS, na.rm = TRUE),
                # 从置信区间反向计算标准误差并计算总方差
                TOTAL_SE = sqrt(sum(((UPPER_LIMIT - LOWER_LIMIT) / (1.96*2))^2, na.rm = TRUE))
            ) %>%
            mutate(
                EXCESS_LOWER = TOTAL_EXCESS - 1.96 * TOTAL_SE,
                EXCESS_UPPER = TOTAL_EXCESS + 1.96 * TOTAL_SE,
                P_SCORE = 100 * TOTAL_EXCESS / TOTAL_EXPECTED
            )

        message("[DEBUG] calculate_summary_by_event completed with result dimensions: ", nrow(summary_by_event), " x ", ncol(summary_by_event))
        flush.console()
        return(summary_by_event)
    }, error = function(e) {
        message("[DEBUG] Error in calculate_summary_by_event: ", e$message)
        flush.console()
        return(data.frame())
    })
}

# 5. 计算总体汇总统计
calculate_overall_summary <- function(out_data) {
    # 调试输出
    message("[DEBUG] calculate_overall_summary called")
    message("[DEBUG] out_data dimensions: ", nrow(out_data), " x ", ncol(out_data))
    message("[DEBUG] out_data columns: ", paste(names(out_data), collapse = ", "))
    flush.console()

    # 检查数据是否有效
    if (nrow(out_data) == 0) {
        message("[DEBUG] Warning: out_data is empty")
        flush.console()
        return(data.frame())
    }

    # 检查是否存在必要的列
    required_cols <- c("NO_DEATHS", "EXCESS_DEATHS", "EXP_DEATHS", "UPPER_LIMIT", "LOWER_LIMIT", "event_index")
    missing_cols <- setdiff(required_cols, names(out_data))

    if (length(missing_cols) > 0) {
        message("[DEBUG] Warning: Missing columns in out_data: ", paste(missing_cols, collapse = ", "))
        flush.console()
        # 创建最小的汇总，使用可用数据
        if ("NO_DEATHS" %in% names(out_data)) {
            overall_summary <- out_data %>%
                mutate(NO_DEATHS = as.numeric(NO_DEATHS)) %>%
                group_by(COUNTRY, ISO3, WM_IDENTIFIER, SEX, AGE_GROUP, AREA, CAUSE) %>%
                reframe(
                    TOTAL_DEATHS = sum(NO_DEATHS, na.rm = TRUE),
                    TOTAL_EXPECTED = NA,
                    TOTAL_EXCESS = NA,
                    TOTAL_SE = NA,
                    EXCESS_LOWER = NA,
                    EXCESS_UPPER = NA,
                    P_SCORE = NA,
                    event_name = "All Events",
                    event_index = "999"
                )
            return(overall_summary)
        } else {
            # 如果没有NO_DEATHS列，无法创建汇总
            message("[DEBUG] Cannot create summary: NO_DEATHS column missing")
            flush.console()
            return(data.frame())
        }
    }

    # 计算总体汇总统计
    tryCatch({
        overall_summary <- out_data %>%
            mutate(NO_DEATHS = as.numeric(NO_DEATHS)) %>%
            mutate(EXP_DEATHS = as.numeric(EXP_DEATHS)) %>%
            mutate(EXCESS_DEATHS = as.numeric(EXCESS_DEATHS)) %>%
            mutate(UPPER_LIMIT = as.numeric(UPPER_LIMIT)) %>%
            mutate(LOWER_LIMIT = as.numeric(LOWER_LIMIT)) %>%
            filter(event_index != "0" & !is.na(NO_DEATHS) & !is.na(event_index)) %>%
            group_by(COUNTRY, ISO3, WM_IDENTIFIER, SEX, AGE_GROUP, AREA, CAUSE) %>%
            reframe(
                TOTAL_DEATHS = sum(NO_DEATHS, na.rm = TRUE),
                TOTAL_EXPECTED = sum(EXP_DEATHS, na.rm = TRUE),
                TOTAL_EXCESS = sum(NO_DEATHS - EXP_DEATHS, na.rm = TRUE),
                # 从置信区间反向计算标准误差并计算总方差
                TOTAL_SE = sqrt(sum(((UPPER_LIMIT - LOWER_LIMIT) / (2*1.96))^2, na.rm = TRUE))
            ) %>%
            mutate(
                EXCESS_LOWER = TOTAL_EXCESS - 1.96 * TOTAL_SE,
                EXCESS_UPPER = TOTAL_EXCESS + 1.96 * TOTAL_SE,
                P_SCORE = 100 * TOTAL_EXCESS / TOTAL_EXPECTED,
                event_name = "All Events",
                event_index = "999"
            )

        message("[DEBUG] calculate_overall_summary completed with result dimensions: ", nrow(overall_summary), " x ", ncol(overall_summary))
        flush.console()
        return(overall_summary)
    }, error = function(e) {
        message("[DEBUG] Error in calculate_overall_summary: ", e$message)
        flush.console()
        return(data.frame())
    })
}

# 6. 处理模型结果并添加属性
process_model_results <- function(out_data, model_type = "Generic") {
    # 调试输出
    message("[DEBUG] process_model_results called with model_type: ", model_type)
    flush.console()

    # 检查数据是否有效
    if (nrow(out_data) == 0) {
        message("[DEBUG] Warning: out_data is empty")
        flush.console()
        return(out_data)
    }

    # 最终处理
    out_data <- finalize_output(out_data, "ACM", model_type)

    # 计算汇总统计
    summary_by_event <- calculate_summary_by_event(out_data)
    overall_summary <- calculate_overall_summary(out_data)
    all_summaries <- bind_rows(summary_by_event, overall_summary)

    # 添加属性
    attr(out_data, "summary_by_event") <- summary_by_event
    attr(out_data, "overall_summary") <- overall_summary
    attr(out_data, "all_summaries") <- all_summaries

    message("[DEBUG] process_model_results completed")
    flush.console()

    return(out_data)
}
