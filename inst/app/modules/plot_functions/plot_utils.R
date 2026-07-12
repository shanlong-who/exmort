# plot_utils.R
# Shared plotting helpers used by ggplot2 and plotly outputs.

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)

# wpro_palette(), get_model_colors() and theme_wpro() are defined once in
# modules/utils.R (sourced eagerly and globally in app.R) so every plot module
# shares one colourblind-safe palette and theme. Referenced here at call time.

# Pull a one-row-per-event table out of merged prediction data.
extract_event_data <- function(data) {
  if ("event_index" %in% names(data) && "event_name" %in% names(data)) {
    event_data <- data %>%
      filter(!is.na(event_index), event_index != "0", !is.na(event_name)) %>%
      group_by(event_index, event_name) %>%
      summarise(
        start_time = min(TimePoint, na.rm = TRUE),
        end_time = max(TimePoint, na.rm = TRUE),
        .groups = 'drop'
      )

    if (nrow(event_data) > 0) {
      event_colors <- RColorBrewer::brewer.pal(min(nrow(event_data), 12), "Set3")
      names(event_colors) <- unique(event_data$event_name)
      attr(event_data, "event_colors") <- event_colors
    }

    return(event_data)
  }
  return(NULL)
}

# Format time-axis labels. For weekly data, pair the ISO week with the ISO
# YEAR (lubridate::isoyear), not the calendar year: near a year boundary the
# calendar year and ISO week belong to different years (e.g. 2024-12-30 is ISO
# 2025-W01), so format(d, "%Y") mislabelled those ticks.
format_time_labels <- function(dates, data) {
  max_period <- max(data$PERIOD, na.rm = TRUE)
  if (max_period > 12) {
    sapply(dates, function(d) {
      sprintf("%d-W%02d", lubridate::isoyear(d), lubridate::isoweek(d))
    })
  } else {
    format(dates, "%Y-%m")
  }
}

# 通用绘图函数：生成 ACM、ED 或 EP 图表
create_mortality_plot <- function(data, plot_type = c("ACM", "ED_bar", "ED_line", "EP_bar", "EP_line"),
                                 selected_sex, selected_age_group, for_plotly = FALSE) {
  plot_type <- match.arg(plot_type)
  if (is.null(data) || nrow(data) == 0) {
    if (for_plotly) {
      return(plot_ly() %>%
               add_annotations(text = "No data available for selected filters.", x = 0.5, y = 0.5, showarrow = FALSE))
    } else {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters.") +
               theme_void())
    }
  }

  # 数据预处理
  # Keep zero-death periods: for small populations (e.g. Pacific island areas)
  # a week/month with zero recorded deaths is real data, not missingness.
  # The old `NO_DEATHS > 0` silently dropped those periods from every plot.
  # Only genuine NA counts are excluded (they cannot be plotted anyway).
  data <- data %>%
    filter(!is.na(NO_DEATHS), if (plot_type %in% c("EP_bar", "EP_line")) abs(EXP_DEATHS) > 1e-6 else TRUE) %>%
    mutate(
      EXCESS_DEATHS = NO_DEATHS - EXP_DEATHS,
      P_SCORE = if (plot_type %in% c("EP_bar", "EP_line")) (EXCESS_DEATHS / EXP_DEATHS) * 100 else NA_real_
    )

  if (nrow(data) == 0) {
    if (for_plotly) {
      return(plot_ly() %>%
               add_annotations(text = "No data with reported deaths available.", x = 0.5, y = 0.5, showarrow = FALSE))
    } else {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data with reported deaths available.") +
               theme_void())
    }
  }

  # 计算置信区间（保持不变）
  has_ci <- "LOWER_LIMIT" %in% names(data) && "UPPER_LIMIT" %in% names(data)
  if (has_ci) {
    data <- data %>%
      mutate(
        EXCESS_LOWER = NO_DEATHS - UPPER_LIMIT,
        EXCESS_UPPER = NO_DEATHS - LOWER_LIMIT,
        P_SCORE_LOWER = if (plot_type %in% c("EP_bar", "EP_line")) (NO_DEATHS - UPPER_LIMIT) / EXP_DEATHS * 100 else NA_real_,
        P_SCORE_UPPER = if (plot_type %in% c("EP_bar", "EP_line")) (NO_DEATHS - LOWER_LIMIT) / EXP_DEATHS * 100 else NA_real_
      )
  }

  # 计算 y 轴范围（保持不变）
  if (plot_type == "ACM") {
    y_min <- min(data$NO_DEATHS, data$EXP_DEATHS, na.rm = TRUE)
    y_max <- max(data$NO_DEATHS, data$EXP_DEATHS, na.rm = TRUE)
    if (has_ci) {
      y_min <- min(y_min, min(data$LOWER_LIMIT, na.rm = TRUE))
      y_max <- max(y_max, max(data$UPPER_LIMIT, na.rm = TRUE))
    }
    y_range <- y_max - y_min
    y_min <- y_min - 0.1 * y_range
    y_max <- y_max + 0.1 * y_range
  } else if (plot_type %in% c("ED_bar", "ED_line")) {
    y_min <- min(data$EXCESS_DEATHS, na.rm = TRUE)
    y_max <- max(data$EXCESS_DEATHS, na.rm = TRUE)
    if (has_ci) {
      y_min <- min(y_min, min(data$EXCESS_LOWER, na.rm = TRUE))
      y_max <- max(y_max, max(data$EXCESS_UPPER, na.rm = TRUE))
    }
    y_min <- min(y_min, 0)
    y_max <- max(y_max, 0)
    y_min <- y_min * 1.1
    y_max <- y_max * 1.1
  } else if (plot_type %in% c("EP_bar", "EP_line")) {
    y_min <- min(data$P_SCORE, na.rm = TRUE)
    y_max <- max(data$P_SCORE, na.rm = TRUE)
    if (has_ci) {
      y_min <- min(y_min, min(data$P_SCORE_LOWER, na.rm = TRUE))
      y_max <- max(y_max, max(data$P_SCORE_UPPER, na.rm = TRUE))
    }
    y_min <- min(y_min, 0)
    y_max <- max(y_max, 0)
    y_min <- y_min * 1.1
    y_max <- y_max * 1.1
  }

  # 提取事件数据（保持不变）
  event_data <- extract_event_data(data)
  event_colors <- if (!is.null(event_data)) attr(event_data, "event_colors") else NULL

  # 时间轴刻度（保持不变）
  date_range <- range(data$TimePoint, na.rm = TRUE)
  time_breaks <- seq(date_range[1], date_range[2], by = "3 months")

  # 标题和副标题（保持不变）
  title <- switch(plot_type,
                  ACM = "All Cause Mortality: Recorded vs. Expected",
                  ED_bar = "Excess Deaths",
                  ED_line = "Excess Deaths (Line Plot)",
                  EP_bar = "P-Score of Excess Deaths (%)",
                  EP_line = "P-Score (Line Plot)")
  subtitle <- paste("Sex:", selected_sex, "| Age Group:", selected_age_group)
  ci_caption <- if (has_ci) "Note: Shaded areas represent 95% CI." else NULL

  # 模型颜色（保持不变）
  model_colors <- get_model_colors()

  if (for_plotly) {
    # 创建 plotly 图表
    p <- plot_ly()

    # 添加数据图层
    if (plot_type == "ACM") {
      # 为 Recorded 数据添加悬停信息
      recorded_data <- data %>%
        mutate(hover_text = paste("Time:", format(TimePoint, "%Y-%m-%d"),
                                  "<br>Recorded Deaths:", round(NO_DEATHS)))
      p <- p %>%
        add_trace(
          data = recorded_data,
          x = ~TimePoint,
          y = ~NO_DEATHS,
          name = "Recorded",
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 2),
          hoverinfo = "text",
          text = ~hover_text
        )

      # 为每个模型添加数据和置信区间
      for (model_name in unique(data$Model)) {
        model_data <- data %>%
          filter(Model == model_name) %>%
          mutate(hover_text = if (has_ci) {
            paste0("Time: ", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model: ", Model,
                  "<br>Expected Deaths: ", round(EXP_DEATHS),
                  "<br>95% CI: ", round(LOWER_LIMIT), ", ", round(UPPER_LIMIT))
          } else {
            paste0("Time: ", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model: ", Model,
                  "<br>Expected Deaths: ", round(EXP_DEATHS))
          })
        line_color <- model_colors[model_name] %||% "#999999"
        line_dash <- switch(model_name,
                            "Historical Average" = "dash",
                            "Historical Average and Trend Model" = "dotdash",
                            "dot")
        p <- p %>%
          add_trace(
            data = model_data,
            x = ~TimePoint,
            y = ~EXP_DEATHS,
            name = paste(model_name, "Expected"),
            type = "scatter",
            mode = "lines",
            line = list(color = line_color, width = 1.5, dash = line_dash),
            hoverinfo = "text",
            text = ~hover_text
          )
        if (has_ci) {
          p <- p %>%
            add_ribbons(
              data = model_data,
              x = ~TimePoint,
              ymin = ~LOWER_LIMIT,
              ymax = ~UPPER_LIMIT,
              name = paste(model_name, "95% CI"),
              line = list(color = "transparent"),
              fillcolor = adjustcolor(line_color, alpha.f = 0.2),
              showlegend = FALSE,
              hoverinfo = "none"
            )
        }
      }
    } else if (plot_type %in% c("ED_bar", "EP_bar")) {
      for (model_name in unique(data$Model)) {
        model_data <- data %>%
          filter(Model == model_name) %>%
          mutate(hover_text = if (has_ci) {
            paste0("Time: ", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model: ", Model,
                  "<br>", if (plot_type == "ED_bar") "Excess Deaths: " else "P-Score: ",
                  round(if (plot_type == "ED_bar") EXCESS_DEATHS else P_SCORE, 2),
                  if (plot_type == "EP_bar") "%" else "",
                  "<br>95% CI: ",
                  round(if (plot_type == "ED_bar") EXCESS_LOWER else P_SCORE_LOWER, 2), ", ",
                  round(if (plot_type == "ED_bar") EXCESS_UPPER else P_SCORE_UPPER, 2),
                  if (plot_type == "EP_bar") "%" else "")
          } else {
            paste0("Time: ", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model: ", Model,
                  "<br>", if (plot_type == "ED_bar") "Excess Deaths: " else "P-Score: ",
                  round(if (plot_type == "ED_bar") EXCESS_DEATHS else P_SCORE, 2),
                  if (plot_type == "EP_bar") "%" else "")
          })
        bar_color <- model_colors[model_name] %||% "rgba(150, 150, 150, 0.7)"
        p <- p %>%
          add_bars(
            data = model_data,
            x = ~TimePoint,
            y = if (plot_type == "ED_bar") ~EXCESS_DEATHS else ~P_SCORE,
            name = model_name,
            marker = list(color = bar_color),
            hoverinfo = "text",
            text = ~hover_text,
            error_y = if (has_ci) {
              list(
                type = "data",
                array = model_data[[if (plot_type == "ED_bar") "EXCESS_UPPER" else "P_SCORE_UPPER"]] - model_data[[if (plot_type == "ED_bar") "EXCESS_DEATHS" else "P_SCORE"]],
                arrayminus = model_data[[if (plot_type == "ED_bar") "EXCESS_DEATHS" else "P_SCORE"]] - model_data[[if (plot_type == "ED_bar") "EXCESS_LOWER" else "P_SCORE_LOWER"]],
                color = "gray40",
                thickness = 0.5,
                width = 3
              )
            } else {
              NULL
            }
          )
      }
      p <- p %>%
        add_trace(
          x = c(min(data$TimePoint, na.rm = TRUE), max(data$TimePoint, na.rm = TRUE)),
          y = c(0, 0),
          type = "scatter",
          mode = "lines",
          line = list(color = "black", dash = "dot", width = 1),
          showlegend = FALSE,
          hoverinfo = "none"
        )
    } else if (plot_type %in% c("ED_line", "EP_line")) {
      for (model_name in unique(data$Model)) {
        model_data <- data %>%
          filter(Model == model_name) %>%
          mutate(hover_text = if (has_ci) {
            paste0("Time: ", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model: ", Model,
                  "<br>", if (plot_type == "ED_line") "Excess Deaths: " else "P-Score: ",
                  round(if (plot_type == "ED_line") EXCESS_DEATHS else P_SCORE, 2),
                  if (plot_type == "EP_line") "%" else "",
                  "<br>95% CI: ",
                  round(if (plot_type == "ED_line") EXCESS_LOWER else P_SCORE_LOWER, 2), ", ",
                  round(if (plot_type == "ED_line") EXCESS_UPPER else P_SCORE_UPPER, 2),
                  if (plot_type == "EP_line") "%" else "")
          } else {
            paste("Time:", format(TimePoint, "%Y-%m-%d"),
                  "<br>Model:", Model,
                  "<br>", if (plot_type == "ED_line") "Excess Deaths:" else "P-Score:",
                  round(if (plot_type == "ED_line") EXCESS_DEATHS else P_SCORE, 2),
                  if (plot_type == "EP_line") "%" else "")
          })
        line_color <- model_colors[model_name] %||% "#999999"
        line_dash <- switch(model_name,
                            "Historical Average" = "dash",
                            "Historical Average and Trend Model" = "dotdash",
                            "dot")
        p <- p %>%
          add_trace(
            data = model_data,
            x = ~TimePoint,
            y = if (plot_type == "ED_line") ~EXCESS_DEATHS else ~P_SCORE,
            name = model_name,
            type = "scatter",
            mode = "lines",
            line = list(color = line_color, width = 2, dash = line_dash),
            hoverinfo = "text",
            text = ~hover_text
          )
        if (has_ci) {
          p <- p %>%
            add_ribbons(
              data = model_data,
              x = ~TimePoint,
              ymin = if (plot_type == "ED_line") ~EXCESS_LOWER else ~P_SCORE_LOWER,
              ymax = if (plot_type == "ED_line") ~EXCESS_UPPER else ~P_SCORE_UPPER,
              name = paste(model_name, "95% CI"),
              line = list(color = "transparent"),
              fillcolor = adjustcolor(line_color, alpha.f = 0.2),
              showlegend = FALSE,
              hoverinfo = "none"
            )
        }
      }
      p <- p %>%
        add_trace(
          x = c(min(data$TimePoint, na.rm = TRUE), max(data$TimePoint, na.rm = TRUE)),
          y = c(0, 0),
          type = "scatter",
          mode = "lines",
          line = list(color = "black", dash = "dot", width = 1),
          showlegend = FALSE,
          hoverinfo = "none"
        )
    }

    # 添加事件矩形作为背景（使用 shapes，保持不变）
    shapes <- list()
    annotations <- list()
    if (!is.null(event_data) && nrow(event_data) > 0) {
      for (i in 1:nrow(event_data)) {
        color_index <- ((i - 1) %% length(event_colors)) + 1
        shapes[[i]] <- list(
          type = "rect",
          x0 = event_data$start_time[i],
          x1 = event_data$end_time[i],
          y0 = y_min,
          y1 = y_max,
          fillcolor = event_colors[color_index],
          opacity = 0.08,
          line = list(color = "transparent"),
          layer = "below",
          editable = FALSE
        )
        annotations[[i]] <- list(
          x = date_midpoint(event_data$start_time[i], event_data$end_time[i]),
          y = y_max * 0.95,
          text = event_data$event_name[i],
          showarrow = FALSE,
          font = list(color = "darkred", size = 10),
          xref = "x",
          yref = "y"
        )
      }
    }

    # 设置布局
    p <- p %>%
      layout(
        font = list(family = "Source Sans Pro, Segoe UI, Arial, sans-serif", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        hoverlabel = list(font = list(family = "Source Sans Pro, Segoe UI, Arial, sans-serif")),
        title = list(text = paste0(title, "<br><sup>", subtitle, "</sup>"), font = list(size = 16)),
        xaxis = list(
          title = "Time",
          tickangle = 45,
          tickvals = time_breaks,
          ticktext = format_time_labels(time_breaks, data)
        ),
        yaxis = list(
          title = switch(plot_type,
                         ACM = "Number of Deaths",
                         ED_bar = "Excess Deaths",
                         ED_line = "Excess Deaths",
                         EP_bar = "P-Score (%)",
                         EP_line = "P-Score (%)"),
          range = c(y_min, y_max)
        ),
        shapes = shapes,
        annotations = annotations,
        barmode = if (plot_type %in% c("ED_bar", "EP_bar")) "group" else NULL,
        legend = list(orientation = "h", y = -0.2),
        hovermode = "x unified"
      )

    return(p)
  } else {
    # 创建 ggplot2 图表（保持不变）
    p <- ggplot(data, aes(x = TimePoint))

    if (plot_type == "ACM") {
      p <- p +
        geom_line(aes(y = NO_DEATHS, color = "Recorded"), linewidth = 1) +
        geom_line(aes(y = EXP_DEATHS, color = Model, linetype = Model), linewidth = 0.8)
      if (has_ci) {
        p <- p +
          geom_ribbon(aes(ymin = LOWER_LIMIT, ymax = UPPER_LIMIT, fill = Model), alpha = 0.2)
      }
    } else if (plot_type %in% c("ED_bar", "EP_bar")) {
      # Scale bar width to the data's period spacing (~7 days weekly, ~30 monthly)
      # instead of a hard-coded 30, which overlapped/mis-sized weekly bars.
      period_days <- as.numeric(stats::median(diff(sort(unique(data$TimePoint)))))
      if (!is.finite(period_days) || period_days <= 0) period_days <- 30
      bar_w <- period_days * 0.8
      p <- p +
        geom_col(aes(y = if (plot_type == "ED_bar") EXCESS_DEATHS else P_SCORE, fill = Model),
                 position = position_dodge(width = bar_w), width = bar_w, alpha = 0.7)
      if (has_ci) {
        p <- p +
          geom_errorbar(aes(ymin = if (plot_type == "ED_bar") EXCESS_LOWER else P_SCORE_LOWER,
                            ymax = if (plot_type == "ED_bar") EXCESS_UPPER else P_SCORE_UPPER),
                        position = position_dodge(width = bar_w), width = bar_w * 0.3, color = "gray40")
      }
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    } else if (plot_type %in% c("ED_line", "EP_line")) {
      p <- p +
        geom_line(aes(y = if (plot_type == "ED_line") EXCESS_DEATHS else P_SCORE, color = Model, linetype = Model), linewidth = 1)
      if (has_ci) {
        p <- p +
          geom_ribbon(aes(ymin = if (plot_type == "ED_line") EXCESS_LOWER else P_SCORE_LOWER,
                          ymax = if (plot_type == "ED_line") EXCESS_UPPER else P_SCORE_UPPER,
                          fill = Model), alpha = 0.2, color = NA)
      }
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    }

    # 添加事件矩形和标签
    if (!is.null(event_data) && nrow(event_data) > 0) {
      p <- p +
        geom_rect(
          data = event_data,
          aes(xmin = start_time, xmax = end_time, ymin = y_min, ymax = y_max, fill = event_name),
          alpha = 0.08,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = event_data,
          aes(x = start_time + (end_time - start_time) / 2, y = y_max * 0.95, label = event_name),
          hjust = 0.5, vjust = -0.5, size = 3, color = "darkred"
        ) +
        scale_fill_brewer(name = "Events", palette = "Set3")
    }

    # 添加轴和主题
    p <- p +
      scale_x_date(breaks = time_breaks, labels = function(x) format_time_labels(x, data), expand = expansion(mult = 0.02)) +
      scale_y_continuous(
        limits = c(y_min, y_max),
        labels = if (plot_type %in% c("EP_bar", "EP_line")) scales::percent_format(scale = 1) else scales::number_format(big.mark = ",")
      ) +
      scale_color_manual(name = "Series", values = c("Recorded" = "black", model_colors), breaks = c("Recorded", names(model_colors))) +
      scale_fill_manual(name = "95% CI", values = model_colors, breaks = names(model_colors)) +
      scale_linetype_manual(
        name = "Series",
        values = c(
          "Historical Average" = "dashed",
          "Negative Binomial Regression" = "dotted",
          "Quasi-Poisson Model" = "longdash",
          "Zero Inflated Poisson Model" = "twodash",
          "BSTS Model" = "solid",
          "State Space Model" = "solid",
          "ARIMA/SARIMA Model" = "solid",
          "Historical Average and Trend Model" = "dotdash"
        ),
        guide = "none"
      ) +
      labs(title = title, subtitle = subtitle, caption = ci_caption, x = "Time", y = switch(plot_type, ACM = "Number of Deaths", ED_bar = "Excess Deaths", ED_line = "Excess Deaths", EP_bar = "P-Score (%)", EP_line = "P-Score (%)")) +
      theme_wpro() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10)
      )

    return(p)
  }
}

# 辅助函数：日期中点计算（保持不变）
date_midpoint <- function(date1, date2) {
  if (inherits(date1, "Date") || inherits(date1, "POSIXt")) {
    date1_num <- as.numeric(date1)
  } else {
    date1_num <- date1
  }
  if (inherits(date2, "Date") || inherits(date2, "POSIXt")) {
    date2_num <- as.numeric(date2)
  } else {
    date2_num <- date2
  }
  mid_num <- (date1_num + date2_num) / 2
  if (inherits(date1, "Date")) {
    return(as.Date(mid_num, origin = "1970-01-01"))
  } else if (inherits(date1, "POSIXt")) {
    return(as.POSIXct(mid_num, origin = "1970-01-01"))
  } else {
    return(mid_num)
  }
}