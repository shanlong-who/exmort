# 加载所需的包
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(tidyr)
library(lubridate)
library(data.table)
library(parallel)
library(lubridate)
library(dplyr)
library(parallel)


# --- Shared WHO WPRO plot styling ------------------------------------------
# Defined here because utils.R is sourced eagerly and globally in app.R and is
# never cleared by the lazy load_module() rm() dance, so all plot modules can
# rely on these being in scope. Kept dependency-free (no DSIR) to avoid pinning
# DSIR into the shinyapps.io deploy; swap in DSIR::theme_dsi() + DSIR palettes
# once DSIR is available in the deploy library.

# Okabe-Ito colourblind-safe palette (black is reserved for observed/recorded).
wpro_palette <- function() {
    c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00",
      "#56B4E9", "#F0E442", "#999999")
}

# Colour map keyed by the Model label that appears in result tables.
# Keep keys aligned with MODELS in modules/model_run_module.R.
get_model_colors <- function() {
    pal <- wpro_palette()
    c(
        "ARIMA/SARIMA Model"           = pal[1],
        "Historical Average"           = pal[2],
        "Negative Binomial Regression" = pal[3],
        "Quasi-Poisson Model"          = pal[4],
        "Zero Inflated Poisson Model"  = pal[5],
        "GAM Spline Model"             = pal[6],
        "Karlinsky-Kobak Model"        = "#882255"  # wine (Tol muted; colourblind-safe)
    )
}

# WHO WPRO house style for ggplot output; dependency-free stand-in for theme_dsi().
theme_wpro <- function(base_size = 12, base_family = "") {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
            plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
            plot.subtitle    = ggplot2::element_text(colour = "grey30", size = base_size - 1),
            plot.caption     = ggplot2::element_text(colour = "grey45", size = base_size - 3),
            axis.title       = ggplot2::element_text(colour = "grey20"),
            axis.text        = ggplot2::element_text(colour = "grey30"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.4),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_text(face = "bold", size = base_size - 1),
            strip.text       = ggplot2::element_text(face = "bold", colour = "grey20"),
            plot.margin      = ggplot2::margin(10, 14, 10, 10)
        )
}
# Consistent loading spinner for plot/table outputs (shinycssloaders).
wpro_spinner <- function(ui_element) {
    shinycssloaders::withSpinner(ui_element, type = 4, color = "#0072B2", proxy.height = "300px")
}
# ---------------------------------------------------------------------------


annotate_events_dt <- function(df_date, df_event, period_type = "week") {
    # 转换为data.table并保留原始列顺序
    df_date <- df_date %>%
        select(YEAR, PERIOD) %>%
        distinct()
    dt_date <- as.data.table(df_date)[, PERIOD_ORIG := PERIOD]
    dt_event <- as.data.table(df_event)

    # 增强型日期预处理（支持ISO周和月份）
    dt_event[, `:=`(
        event_idx = .I,
        start_date = as.Date(start_date, optional = TRUE),
        end_date = as.Date(end_date, optional = TRUE)
    )][, date_valid := !is.na(start_date) & !is.na(end_date)]

    # 智能周期类型检测
    period_type <- if (max(dt_date$PERIOD, na.rm = TRUE) > 12 & period_type == "week") {
        "week"
    } else {
        "month"
    }

    # 检查和清理 YEAR 和 PERIOD 组合
    dt_date[, valid_period := {
        if (period_type == "month") {
            between(PERIOD, 1, 12) & !is.na(PERIOD) & !is.na(YEAR)
        } else {
            max_week <- isoweek(make_date(YEAR, 12, 28)) # 每年的最大周数
            between(PERIOD, 1, max_week) & !is.na(PERIOD) & !is.na(YEAR) & PERIOD <= 53
        }
    }, by = .(YEAR, PERIOD)]

    # 删除无效的组合
    original_rows <- nrow(dt_date)
    dt_date <- dt_date[valid_period == TRUE]
    if (nrow(dt_date) < original_rows) {
        message(sprintf("已删除 %d 行无效的 YEAR-PERIOD 组合", original_rows - nrow(dt_date)))
    }

    # 向量化日期范围计算
    dt_date[, c("error", "start_date", "end_date") := {
        year <- YEAR
        period <- PERIOD

        if (period_type == "month") {
            start <- as.Date(paste(year, period, "01"), "%Y %m %d")
            end <- as.Date(paste(year, period, days_in_month(month(start))), "%Y %m %d")
        } else {
            start <- ISOweek::ISOweek2date(paste0(year, "-W", sprintf("%02d", period), "-1"))
            end <- start + days(6)
        }

        error_msg <- NA_character_
        list(error_msg, start, end)
    }, by = .I]

    # 优化区间匹配（使用非等值连接）
    setkey(dt_event, start_date, end_date)

    dt_date[, c("event_index", "event_name") := {
        if (is.na(start_date) || is.na(end_date)) {
            list("0", "日期组合错误")
        } else {
            current_start <- start_date
            current_end <- end_date
            matches <- dt_event[start_date <= current_end & end_date >= current_start]

            if (nrow(matches) == 0) {
                list("0", "")
            } else {
                list(
                    paste(sort(matches$event_idx), collapse = ","),
                    paste(matches$event_name[order(matches$event_idx)], collapse = ",")
                )
            }
        }
    }, by = .I]

    # 后处理与验证
    dt_date[, `:=`(
        PERIOD = PERIOD_ORIG,
        error = NULL,
        start_date = NULL,
        end_date = NULL,
        PERIOD_ORIG = NULL,
        valid_period = NULL
    )]

    # 结果验证
    if (any(dt_date$event_index == "" & dt_date$event_name != "")) {
        warning("事件索引与名称不匹配")
    }

    return(dt_date[])
}

#' Validate event dates
#'
#' This function reads event data and validates date formats and ranges.
#'
#' @param data data frame containing event data
#' @return A list containing validation results and processed data
validate_event_dates <- function(data) {
    library(tibble)
    tryCatch(
        {
            # Read Excel file
            # data <- read.xlsx(file_path, sheet = sheet, colNames = TRUE)

            # Check if data was successfully read
            if (nrow(data) == 0) {
                stop("Error: Excel file is empty or data was not read correctly.")
            }

            # Print original data for inspection
            cat("Original data:\n")
            print(head(data, 3))

            # Convert start_date and end_date to date type
            # data$start_date <- as.Date(data$start_date, origin = "1899-12-30", tryFormats = c("%Y-%m-%d", "%d-%b-%y"))
            # data$end_date <- as.Date(data$end_date, origin = "1899-12-30", tryFormats = c("%Y-%m-%d", "%d-%b-%y"))
            # 转换日期列
            data$start_date <- sapply(data$start_date, convert_to_date)
            data$end_date <- sapply(data$end_date, convert_to_date)

            # # 验证转换后的日期
            # is_date_start <- all(!is.na(data$start_date))
            # is_date_end <- all(!is.na(data$end_date))

            # if (!is_date_start) {
            #     warning("警告：部分开始日期转换失败")
            # }
            # if (!is_date_end) {
            #     warning("警告：部分结束日期转换失败")
            # }
            # Validate if dates are of correct type
            is_date_start <- all(sapply(data$start_date, function(x) inherits(x, "Date")))
            is_date_end <- all(sapply(data$end_date, function(x) inherits(x, "Date")))

            if (!is_date_start) {
                warning("Warning: start_date contains non-date values, conversion may have failed.")
            }
            if (!is_date_end) {
                warning("Warning: end_date contains non-date values, conversion may have failed.")
            }

            # Check if all dates are after 1990
            is_after_1990_start <- all(data$start_date >= as.Date("1990-01-01"), na.rm = TRUE)
            is_after_1990_end <- all(data$end_date >= as.Date("1990-01-01"), na.rm = TRUE)

            if (!is_after_1990_start) {
                warning("Warning: start_date contains dates earlier than 1990-01-01.")
            }
            if (!is_after_1990_end) {
                warning("Warning: end_date contains dates earlier than 1990-01-01.")
            }

            # Output validation results
            cat("\nValidation results:\n")
            cat("All start_dates are date type:", is_date_start, "\n")
            cat("All end_dates are date type:", is_date_end, "\n")
            cat("All start_dates are after 1990:", is_after_1990_start, "\n")
            cat("All end_dates are after 1990:", is_after_1990_end, "\n")

            # Return data and validation results
            return(list(
                data = data,
                is_date_start = is_date_start,
                is_date_end = is_date_end,
                is_after_1990_start = is_after_1990_start,
                is_after_1990_end = is_after_1990_end
            ))
        },
        error = function(e) {
            cat("Error:", conditionMessage(e), "\n")
            return(NULL)
        }
    )
}


create_period_indicator_bak <- function(mortality_data, event_data) {
    # 转换为data.table并保留原始列名
    death_data <- as.data.table(mortality_data)
    original_cols <- copy(names(death_data))

    # 增强日期验证（处理POSIXct和字符串混合类型）
    validate_event_dates <- function(dt) {
        dt <- as.data.table(dt)

        # 统一日期格式转换（处理常见日期格式）
        convert_date <- function(x) {
            if (inherits(x, "POSIXct")) {
                as.Date(x)
            } else {
                as.Date(x, format = "%Y-%m-%d") # 支持多种分隔符
            }
        }

        dt[, start_date := convert_date(start_date)]
        dt[, end_date := convert_date(end_date)]

        # 验证结果报告
        invalid_dates <- dt[is.na(start_date) | is.na(end_date)]
        if (nrow(invalid_dates) > 0) {
            warning("发现", nrow(invalid_dates), "条无效日期记录，已自动过滤")
            print(invalid_dates)
        }

        list(
            data = dt[!is.na(start_date) & !is.na(end_date)],
            invalid = invalid_dates
        )
    }

    # 事件数据预处理
    event_validation <- validate_event_dates(event_data)
    if (nrow(event_validation$data) == 0) {
        stop("所有事件日期转换失败，请检查输入格式（支持：YYYY-MM-DD或POSIXct）")
    }
    events <- event_validation$data

    # 关键列验证（保留原始大小写）
    required_cols <- c("YEAR", "PERIOD")
    missing_cols <- setdiff(tolower(required_cols), tolower(names(death_data)))
    if (length(missing_cols) > 0) {
        stop(
            "死亡数据缺失必要列：", paste(missing_cols, collapse = ", "),
            "\n当前存在列：", paste(names(death_data), collapse = ", ")
        )
    }

    # 生成唯一周期组合（保留原始列名）
    year_periods <- unique(death_data[, .(YEAR, PERIOD)])

    # 周期类型判断（基于实际数据范围）
    period_stats <- year_periods[, .(
        min_period = min(PERIOD, na.rm = TRUE),
        max_period = max(PERIOD, na.rm = TRUE)
    )]

    period_type <- if (period_stats$max_period > 52) {
        "week"
    } else if (between(period_stats$max_period, 1, 12)) {
        "month"
    } else {
        stop(
            "无法识别周期类型（PERIOD范围：",
            period_stats$min_period, "-", period_stats$max_period, ")"
        )
    }
    cat(sprintf(
        "周期类型：%s（有效范围：%d-%d）\n",
        period_type,
        period_stats$min_period,
        period_stats$max_period
    ))

    # 执行整体计算
    result <- annotate_events_dt(year_periods, events, period_type)

    # 合并结果（保持原始数据结构）
    death_data <- merge(
        death_data,
        cbind(year_periods, result),
        by = c("YEAR", "PERIOD"),
        all.x = TRUE
    )[, ..original_cols] # 保持原始列顺序

    return(death_data)
}



# Process mortality data function
process_mortality_data <- function(raw_data, sheet_name) {
    if (raw_data[4, 3] == "WEEKS") {
        len.header <- 6
    } else {
        len.header <- 5
    }
    is.data <- apply(!is.na(as.matrix(raw_data[len.header:nrow(raw_data), 3:ncol(raw_data)])), 1, sum, na.rm = TRUE)
    is.time <- apply(!is.na(as.matrix(raw_data[len.header:nrow(raw_data), 3:ncol(raw_data)])), 2, sum, na.rm = TRUE) > 0
    last.data <- which.max(cumsum(is.time)) + 2
    year <- max(as.numeric(raw_data[1, 3:last.data]), na.rm = TRUE)
    last.year <- match(year + 1, raw_data[1, ]) - 1
    if (is.na(last.year)) last.year <- ncol(raw_data)
    a <- as.matrix(raw_data[len.header:nrow(raw_data), 3:last.year])[is.data > 12, , drop = FALSE]
    mode(a) <- "numeric"
    a <- round(a)
    age <- as.data.frame(raw_data[, 1])[seq(len.header, max(len.header, nrow(raw_data)), by = 3), 1]
    age <- age[age != ""]

    filtered_indices <- which(is.data > 12)
    filtered_age_rep <- rep(age, each = 3)[filtered_indices]
    filtered_sex_rep <- as.data.frame(raw_data[len.header:nrow(raw_data), 2])[filtered_indices, ]

    total_elements <- length(as.vector(t(a)))
    ACM_var <- data.frame(
        REGION = rep(sheet_name, total_elements),
        AGE_GROUP = rep(filtered_age_rep, ncol(a))[1:total_elements],
        SEX = rep(filtered_sex_rep, ncol(a))[1:total_elements],
        YEAR = rep(as.numeric(raw_data[1, 3:last.year]), each = nrow(a))[1:total_elements],
        DAYS = rep(as.numeric(raw_data[3, 3:last.year]), each = nrow(a))[1:total_elements],
        PERIOD = rep(as.numeric(raw_data[ifelse(len.header == 6, 5, 3), 3:last.year]), each = nrow(a))[1:total_elements],
        NO_DEATHS = as.vector(t(a))[1:total_elements]
    )
    return(ACM_var)
}

# Load events data function
loadEventsData_old <- function(file_path, sheet_name) {
    events_data <- tryCatch(
        {
            data <- readxl::read_xlsx(file_path, sheet = sheet_name, col_names = TRUE)
            if (nrow(data) == 0) stop("Events data sheet is empty")
            data
        },
        error = function(e) {
            showNotification(paste("Error loading events:", e$message), type = "error")
            NULL
        }
    )
    return(events_data)
}


loadEventsData <- function(file_path,
                           sheet = 1,
                           target_pattern = "_date|_time", # 新增参数，更灵活
                           excel_origin = "1899-12-30",
                           convert_types = c("character", "numeric"), # 只转换这些原始类型
                           ...) {
    # 添加错误处理
    tryCatch({
        # 读取数据
        data <- read_excel(file_path, sheet = sheet, ...)

        # 检查数据是否为空
        if (nrow(data) == 0) {
            message("Warning: Events data sheet is empty: ", sheet)
            return(NULL)
        }

        # Canonicalise the event column names to the lowercase event_name /
        # start_date / end_date that the merge step (create_period_indicator)
        # requires. Validation accepts case- and _time/_date variants, so a
        # sheet using e.g. START_DATE would otherwise pass validation and then
        # crash the merge. Rename only the first match per target to avoid
        # creating duplicate column names.
        rename_first <- function(nms, regex, target) {
            if (target %in% nms) return(nms)          # already canonical
            hit <- grep(regex, nms, ignore.case = TRUE)
            if (length(hit) > 0) nms[hit[1]] <- target
            nms
        }
        cur <- names(data)
        cur <- rename_first(cur, "^event[_ ]?name$|^event$", "event_name")
        cur <- rename_first(cur, "^start[_ ]?(date|time)$",  "start_date")
        cur <- rename_first(cur, "^end[_ ]?(date|time)$",    "end_date")
        names(data) <- cur

        # 找到所有列名
        all_cols <- names(data)

        # 根据模式识别目标列名
        target_cols <- all_cols[grepl(target_pattern, all_cols, ignore.case = TRUE)]

        if (length(target_cols) == 0) {
            message(paste("没有找到列名匹配模式 '", target_pattern, "' 的列。"))
            return(data)
        }

        message(paste("找到以下可能需要转换的列:", paste(target_cols, collapse = ", ")))

        # 转换目标列
        for (col_name in target_cols) {
            col_data <- data[[col_name]]
            original_class <- class(col_data)[1] # 获取主要类

            # 1. 如果已经是日期/时间类型，则跳过 (readxl 可能已处理)
            if (inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
                message(paste("列", col_name, "已经是日期/时间类型，跳过转换。"))
                next
            }

            # 2. 处理数值型 (仅当其原始类型在 convert_types 中)
            if (is.numeric(col_data) && "numeric" %in% convert_types) {
                # 注意：Excel 的日期时间也可能是数值，但 readxl 通常会处理。
                # 这个转换主要用于 readxl 未能识别的数值型日期/时间。
                # 转换为 POSIXct 以保留时间信息（如果原始数值包含小数部分）
                # 如果只需要日期，可以使用 as.Date
                converted_data <- as.POSIXct(col_data * 86400, origin = excel_origin, tz = "UTC") # Excel stores datetime as days since origin
                # 或者仅日期: converted_data <- as.Date(col_data, origin = excel_origin)

                # 检查转换是否引入过多NA (可能原本就不是日期数值)
                if (sum(is.na(converted_data)) > sum(is.na(col_data))) {
                    warning(paste("尝试将数值列", col_name, "转换为日期/时间时引入了额外的NA值，请检查数据。原始类型:", original_class))
                } else {
                    data[[col_name]] <- converted_data
                    message(paste("将数值列", col_name, "转换为 POSIXct (可能来自Excel数值)。原始类型:", original_class))
                }
                next # 处理完继续下一列
            }

            # 3. 处理字符型 (仅当其原始类型在 convert_types 中)
            if (is.character(col_data) && "character" %in% convert_types) {
                # 尝试自动解析多种日期和时间格式
                # 保留时间信息，使用 parse_date_time
                # 如果只需要日期，可以后续使用 as.Date()
                parsed <- parse_date_time(col_data,
                    orders = c("ymd HMS", "ymd", "dmy HMS", "dmy", "mdy HMS", "mdy", "ymd HM", "dmy HM", "mdy HM"), # 添加常见时间格式
                    tz = "UTC", # 指定时区以避免歧义，或根据需要调整
                    quiet = TRUE
                ) # 减少控制台输出

                # 检查是否所有非NA原始值都成功解析
                original_non_na <- !is.na(col_data) & col_data != ""
                parsed_na <- is.na(parsed)

                if (all(parsed_na[original_non_na])) { # 如果所有原本有内容的都解析失败
                    warning(paste("无法自动解析字符列", col_name, "中的日期/时间格式。原始类型:", original_class))
                } else {
                    # 如果部分失败，也给个提示
                    if (any(parsed_na[original_non_na])) {
                        warning(paste("部分值在字符列", col_name, "中无法解析为日期/时间。原始类型:", original_class))
                    }
                    data[[col_name]] <- parsed
                    message(paste("将字符列", col_name, "自动解析为 POSIXct。原始类型:", original_class))
                }
                next # 处理完继续下一列
            }

            # 4. 如果列类型不匹配或者不在 convert_types 中，则不处理
            if (!(original_class %in% convert_types)) {
                message(paste("列", col_name, "类型为", original_class, "，不在指定的 convert_types 中 ('", paste(convert_types, collapse = "', '"), "')，跳过转换。"))
            } else {
                # 此处理论上不应该到达，因为 numeric/character 已处理
                warning(paste("列", col_name, "类型为", original_class, "，未进行转换。"))
            }
        } # 结束 for 循环

        return(data)
    }, error = function(e) {
        message("Error loading events data: ", e$message)
        return(NULL)
    })
}


# 定义一个模拟函数接口，因为它需要文件路径，我们直接传入 data.frame
read_excel_dates_refined_mock <- function(data, # 直接接收 data.frame
                                          target_pattern = "_date|_time",
                                          excel_origin = "1899-12-30",
                                          convert_types = c("character", "numeric"),
                                          ...) {
    # ... (内部逻辑和上面的 read_excel_dates_refined 相同，除了没有 read_excel) ...
    all_cols <- names(data)
    target_cols <- all_cols[grepl(target_pattern, all_cols, ignore.case = TRUE)]
    if (length(target_cols) == 0) {
        message(paste("没有找到列名匹配模式 '", target_pattern, "' 的列。"))
        return(data)
    }
    message(paste("找到以下可能需要转换的列:", paste(target_cols, collapse = ", ")))
    for (col_name in target_cols) {
        col_data <- data[[col_name]]
        original_class <- class(col_data)[1]
        if (inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
            message(paste("列", col_name, "已经是日期/时间类型，跳过转换。"))
            next
        }
        if (is.numeric(col_data) && "numeric" %in% convert_types) {
            converted_data <- as.POSIXct(col_data * 86400, origin = excel_origin, tz = "UTC")
            if (sum(is.na(converted_data)) > sum(is.na(col_data))) {
                warning(paste("尝试将数值列", col_name, "转换为日期/时间时引入了额外的NA值，请检查数据。原始类型:", original_class))
            } else {
                data[[col_name]] <- converted_data
                message(paste("将数值列", col_name, "转换为 POSIXct。原始类型:", original_class))
            }
            next
        }
        if (is.character(col_data) && "character" %in% convert_types) {
            # 尝试解析日期时间 或 只有时间
            parsed <- parse_date_time(col_data, orders = c("ymd HMS", "ymd", "dmy HMS", "dmy", "mdy HMS", "mdy", "ymd HM", "dmy HM", "mdy HM", "H M S", "H M"), tz = "UTC", quiet = TRUE) # 添加时间格式
            # 如果只有时间被解析，结果会是当天日期+时间

            original_non_na <- !is.na(col_data) & col_data != ""
            parsed_na <- is.na(parsed)
            if (all(parsed_na[original_non_na])) {
                warning(paste("无法自动解析字符列", col_name, "中的日期/时间格式。原始类型:", original_class))
            } else {
                if (any(parsed_na[original_non_na])) {
                    warning(paste("部分值在字符列", col_name, "中无法解析为日期/时间。原始类型:", original_class))
                }
                data[[col_name]] <- parsed
                message(paste("将字符列", col_name, "自动解析为 POSIXct。原始类型:", original_class))
            }
            next
        }
        if (!(original_class %in% convert_types)) {
            message(paste("列", col_name, "类型为", original_class, "，不在指定的 convert_types 中，跳过转换。"))
        } else {
            warning(paste("列", col_name, "类型为", original_class, "，未进行转换。"))
        }
    }
    return(data)
}

create_period_indicator <- function(mortality_data, event_data) {
    # 转换为data.table并保留原始列名
    death_data <- as.data.table(mortality_data)
    original_cols <- copy(names(death_data)) # 捕获所有原始列名

    # 增强日期验证（处理POSIXct和字符串混合类型）
    validate_event_dates <- function(dt) {
        dt <- as.data.table(dt)

        # 统一日期格式转换（处理常见日期格式）
        convert_date <- function(x) {
            if (inherits(x, "POSIXct")) {
                as.Date(x)
            } else {
                as.Date(x, format = "%Y-%m-%d") # 支持多种分隔符
            }
        }

        dt[, start_date := convert_date(start_date)]
        dt[, end_date := convert_date(end_date)]

        # 验证结果报告
        invalid_dates <- dt[is.na(start_date) | is.na(end_date)]
        if (nrow(invalid_dates) > 0) {
            warning("发现", nrow(invalid_dates), "条无效日期记录，已自动过滤")
            print(invalid_dates)
        }

        list(
            data = dt[!is.na(start_date) & !is.na(end_date)],
            invalid = invalid_dates
        )
    }

    # 事件数据预处理
    event_validation <- validate_event_dates(event_data)
    if (nrow(event_validation$data) == 0) {
        stop("所有事件日期转换失败，请检查输入格式（支持：YYYY-MM-DD或POSIXct）")
    }
    events <- event_validation$data

    # 关键列验证（保留原始大小写）
    required_cols <- c("YEAR", "PERIOD")
    missing_cols <- setdiff(tolower(required_cols), tolower(names(death_data)))
    if (length(missing_cols) > 0) {
        stop(
            "死亡数据缺失必要列：", paste(missing_cols, collapse = ", "),
            "\n当前存在列：", paste(names(death_data), collapse = ", ")
        )
    }

    # 生成唯一周期组合（保留原始列名）
    year_periods <- unique(death_data[, .(YEAR, PERIOD)])

    # 周期类型判断（基于实际数据范围）
    period_stats <- year_periods[, .(
        min_period = min(PERIOD, na.rm = TRUE),
        max_period = max(PERIOD, na.rm = TRUE)
    )]

    period_type <- if (period_stats$max_period >= 52) {
        "week"
    } else if (between(period_stats$max_period, 1, 12)) {
        "month"
    } else {
        stop(
            "无法识别周期类型（PERIOD范围：",
            period_stats$min_period, "-", period_stats$max_period, ")"
        )
    }
    cat(sprintf(
        "周期类型：%s（有效范围：%d-%d）\n",
        period_type,
        period_stats$min_period,
        period_stats$max_period
    ))

    # 执行整体计算
    result <- annotate_events_dt(year_periods, events, period_type)

    # 选择事件相关列，避免重复
    result_subset <- result[, .(YEAR, PERIOD, event_index, event_name)]

    # 合并结果，保留所有原始列
    death_data <- merge(
        death_data,
        result_subset,
        by = c("YEAR", "PERIOD"),
        all.x = TRUE
    )

    # 确保返回所有原始列加上新的事件列
    final_cols <- unique(c(original_cols, "event_index", "event_name"))
    death_data <- death_data[, ..final_cols]

    # 按原数据其他变量、YEAR、PERIOD 排序
    # other_cols <- setdiff(original_cols, c("YEAR", "PERIOD")) # 获取除 YEAR 和 PERIOD 外的列
    # if (length(other_cols) > 0) {
    #     setorderv(death_data, c(other_cols, "YEAR", "PERIOD"))
    # } else {
    #     setorderv(death_data, c("YEAR", "PERIOD"))
    # }

    return(death_data)
}


# Merge data function
mergeEventsData <- function(acm_data, event_data) {
    merged_data <- create_period_indicator(acm_data, event_data)
    return(merged_data)
}

# Process mortality data function
process_mortality_data2 <- function(data_file, sheetname) {
    # 添加错误处理
    tryCatch({
        mor <- read.xlsx(data_file, sheet = sheetname, startRow = 1, colNames = FALSE, rows = 5)

        # 检查mor是否为空或者mor[1]是否存在
        if (length(mor) > 0 && !is.null(mor[1]) && length(mor[1]) > 0) {
            if (mor[1] == "WEEKS") {
                mor <- weekly_data_process(data_file, sheetname)
            } else {
                mor <- monthly_data_process(data_file, sheetname)
            }
            mor$AREA <- sheetname
            return(mor[, c("AREA", "AGE_GROUP", "SEX", "YEAR", "PERIOD", "DAYS", "NO_DEATHS")])
        } else {
            message("Warning: No valid data found in sheet: ", sheetname)
            return(NULL)
        }
    }, error = function(e) {
        message("Error processing mortality data: ", e$message)
        return(NULL)
    })
}

monthly_data_process <- function(data_file, sheetname) {
    mor <- read.xlsx(data_file, sheet = sheetname, startRow = 1, colNames = FALSE, rows = c(2, 4))
    YEAR <- as.numeric(mor[1, ])
    PERIOD <- as.numeric(mor[2, ])
    DAYS <- lubridate::days_in_month(as.Date(paste0(YEAR, "-", PERIOD, "-", 1)))
    mor <- read.xlsx(data_file, sheet = sheetname, startRow = 1, colNames = FALSE)[-c(1:5), ]
    mor[, 1] <- zoo::na.locf(mor[, 1])
    cols <- dim(mor)[2]

    # Threshold only needs to exclude structural/label rows (no numeric cells);
    # it must not scale with series length. It was 24 (~2 years of monthly
    # data), which silently emptied shorter uploads. `> 1` keeps any genuine
    # series and drops empty rows.
    mor <- mor %>%
        mutate(across(all_of(3:cols), as.numeric)) %>%
        filter(rowSums(!is.na(select(., 3:ncol(.)))) > 1)

    patterns <- dim(mor)[1]

    mor <- mor %>%
        pivot_longer(cols = all_of(3:cols), names_to = "variable", values_to = "NO_DEATHS") %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS))

    names(mor)[1:2] <- c("AGE_GROUP", "SEX")

    mor$YEAR <- rep(YEAR, times = patterns)
    mor$PERIOD <- rep(PERIOD, times = patterns)
    mor$DAYS <- rep(as.numeric(DAYS), times = patterns)
    mor <- mor[, -3] %>%
        filter(PERIOD < 13 & PERIOD > 0)
    return(mor)
}

# type <- "WEEKLY"

weekly_data_process <- function(data_file, sheetname) {
    mor <- read.xlsx(data_file, sheet = sheetname, startRow = 1, colNames = FALSE, rows = c(2, 4, 6))

    YEAR <- as.numeric(mor[1, ])
    DAYS <- as.numeric(mor[2, ])
    PERIOD <- as.numeric(mor[3, ])

    mor <- read.xlsx(data_file, sheet = sheetname, startRow = 1, colNames = FALSE)[-c(1:6), ]
    mor[, 1] <- zoo::na.locf(mor[, 1])

    # Define cols before using it
    cols <- dim(mor)[2]

    # Keep rows that carry real data. A data row spans all time columns, so its
    # non-missing count equals the number of weeks in the upload; structural /
    # label rows have no numeric cells after coercion. The threshold only needs
    # to exclude the latter, so it must NOT scale with series length. It was 100
    # (~2 years of weekly data), which silently emptied any shorter upload;
    # `> 1` keeps any genuine series (even a few weeks) and drops empty rows.
    mor <- mor %>%
        mutate(across(all_of(3:cols), as.numeric)) %>%
        filter(rowSums(!is.na(select(., 3:ncol(.)))) > 1)

    patterns <- dim(mor)[1]

    mor <- mor %>%
        pivot_longer(cols = all_of(3:cols), names_to = "variable", values_to = "NO_DEATHS") %>%
        mutate(NO_DEATHS = as.numeric(NO_DEATHS))

    names(mor)[1:2] <- c("AGE_GROUP", "SEX")

    # Fix typo: YREAR -> YEAR
    mor$YEAR <- rep(YEAR, times = patterns)
    mor$PERIOD <- rep(PERIOD, times = patterns)
    mor$DAYS <- rep(DAYS, times = patterns)
    # Keep ISO week 53. 2015 and 2020 are 53-week ISO years with real deaths in
    # week 53; the old `PERIOD < 53` silently dropped them from every total,
    # table, and plot. (Seasonal models still cap at 52 internally.)
    mor <- mor[, -3] %>%
        filter(PERIOD <= 53 & PERIOD > 0)
    return(mor)
}

# 定义 get_iso_weeks_in_year 函数，用于计算某年的 ISO 周数
get_iso_weeks_in_year <- function(year) {
    # Input validation
    if (!is.numeric(year) || length(year) != 1) {
        stop("Year must be a single numeric value")
    }
    if (year < 1800 || year > 2999) {
        stop("Year must be between 1800 and 2999")
    }

    # Calculate ISO week number for December 28th (always in last week)
    dec_28 <- as.Date(paste0(year, "-12-28"))
    week_num <- isoweek(dec_28)

    # For years like 2000 and 2016, we need to check if Dec 31 falls in week 52
    dec_31 <- as.Date(paste0(year, "-12-31"))
    if (isoweek(dec_31) == 1) {
        return(52)
    }

    return(week_num)
}

# get_week_start_date: ISO-correct Monday of the given ISO week.
# This is the canonical definition. A second, buggy copy used to be defined
# lower in this file and silently overrode this one (it returned the first
# Sunday, not the ISO Monday); that duplicate has been removed.
# Out-of-range weeks are clamped rather than raising an error, so a stray
# week 53 in a 52-week year cannot crash the batch date calculation in app.R.
# NB: wday() is namespaced to lubridate:: on purpose — data.table (loaded by
# several modules) also exports wday(), which has no week_start argument, and
# whichever package is attached last would otherwise win and error at runtime.
get_week_start_date <- function(year, week) {
    if (!is.numeric(year) || !is.numeric(week)) {
        stop("Year and week must be numeric values")
    }
    total_weeks <- get_iso_weeks_in_year(year)
    week <- min(max(week, 1), total_weeks)
    jan_4 <- lubridate::ymd(paste0(year, "-01-04"))
    week1_monday <- jan_4 - lubridate::days(lubridate::wday(jan_4, week_start = 1) - 1)
    target_date <- week1_monday + lubridate::days(7 * (week - 1))
    return(target_date)
}


# Helper function: date calculation.
# Anchored to min(YEAR) rather than a hardcoded 2015 so it never produces NA
# (non-2015 start) or a mixed-subscript crash (pre-2015 year). See the twin
# definition in common_functions.R.
calculate_dates <- function(src, max_period, nys, DOM, MOY) {
    yr0 <- min(src$YEAR, na.rm = TRUE)
    span <- max(src$YEAR, na.rm = TRUE) - yr0 + 1
    year_offsets <- cumsum(c(0, rep(365, span + 1)))
    if (max_period == 12) {
        day <- cumsum(c(0, DOM))[src$PERIOD] + 15
        src$DATE <- year_offsets[src$YEAR - yr0 + 1] + day
    } else {
        day <- as.numeric(substr(src$DATE_TO_SPECIFY_WEEK, 5, 6))
        if (all(!is.na(day))) {
            Date <- match(substr(src$DATE_TO_SPECIFY_WEEK, 1, 3), MOY)
            day <- cumsum(c(0, DOM))[Date] + day - 3.5
            loc_YEAR <- src$YEAR
            bad_day <- (day - 0) < 0 & !is.na(day)
            loc_YEAR[bad_day] <- loc_YEAR[bad_day] + 1
            src$DATE <- year_offsets[loc_YEAR - yr0 + 1] + day
        } else {
            day <- cumsum(c(0, rep(7, 52)))[src$PERIOD] + 3.5
            src$DATE <- year_offsets[src$YEAR - yr0 + 1] + day
        }
    }
    return(src)
}


# (Removed) A second get_week_start_date defined here used to override the
# ISO-correct one above. Its (8 - wday(jan_first)) %% 7 offset returned the
# first Sunday on/after Jan 1 (wday defaults to Sunday = 1), not the ISO
# Monday, shifting every derived week date by up to 6 days. The canonical
# definition above is now the only one.

# print("--- 测试模拟数据 ---")
# print(str(events_data_test))
# print(head(events_data_test))


# # events_data <- read_excel(data_file, "events")

# # events_data_test <- read_excel_dates_refined_mock(events_data)

# events_data <- read_excel_dates_refined(data_file, "events")
# head(events_data_test)



# library(openxlsx)

# data_file <- "XLSX\\Australia_built_in_data.xlsx"
# raw_data <- read.xlsx(data_file, sheet = 2)

# events_data <- loadEventsData(data_file, "events")
# # head(events_data)
# # events_data <- as.data.table(events_data)

# # head(events_data)

# death_data <- process_mortality_data(raw_data, "National")


# death_data <- as.data.table(death_data)
# year_periods <- unique(death_data[, .(YEAR, PERIOD)])

# head(death_data)

# dt <- annotate_events_dt(year_periods, events_data, period_type = "week")

# dim(dt)

# dt %>%
#     distinct() %>%
#     dim()



# # write.csv(dt, "dt.csv")

# res <- create_period_indicator(death_data, events_data)

# write.csv(res, "res.csv")

# # write.csv(death_data, "death_data.csv")
# loadEventsData <- function(file_path, sheet_name) {
#   tryCatch({
#     # 读取事件数据
#     events_data <- read.xlsx(file_path, sheet = sheet_name, colNames = FALSE)

#     # 获取列名所在行
#     header_row <- which(events_data[,1] == "EVENT_NAME" | events_data[,1] == "Event Name")
#     if (length(header_row) == 0) {
#       return(NULL)
#     }

#     # 设置列名
#     colnames(events_data) <- as.character(unlist(events_data[header_row,]))

#     # 只保留数据行
#     events_data <- events_data[(header_row + 1):nrow(events_data),]

#     # 标准化列名
#     colnames(events_data) <- toupper(gsub(" ", "_", colnames(events_data)))

#     # 处理日期列
#     events_data$START_DATE <- as.Date(as.numeric(events_data$START_DATE), origin = "1899-12-30")
#     events_data$END_DATE <- as.Date(as.numeric(events_data$END_DATE), origin = "1899-12-30")

#     # 删除事件名称为空的行
#     events_data <- events_data[!is.na(events_data$EVENT_NAME), ]

#     # 确保标注位置在合理范围内（对于非空事件）
#     events_data$LABEL_POSITION <- pmin(pmax(as.numeric(events_data$LABEL_POSITION), 0.1), 0.9)

#     return(events_data)
#   }, error = function(e) {
#     warning("Error loading events data: ", e$message)
#     return(NULL)
#   })
# }
