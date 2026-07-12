# Report figures
# Builds the static figures embedded in the exported report (HTML/PDF/docx).
# Returns temp PNG paths so the Rmd template can knitr::include_graphics() them
# in every format. Uses the shared theme_wpro() + get_model_colors() (defined in
# modules/utils.R, loaded globally) so report charts match the app.

library(ggplot2)
library(dplyr)
library(scales)        # axis label formatting

# Build the three report figures and save them to temporary PNGs.
# Returns list(acm, excess, pscore) of file paths (NA_character_ when no data).
build_report_figures <- function(total_predictions, selected_models, dpi = 150) {
  na_res <- list(acm = NA_character_, excess = NA_character_, pscore = NA_character_)
  if (is.null(total_predictions) || nrow(total_predictions) == 0) return(na_res)

  # Detect weekly vs monthly. Building the date as "YEAR-PERIOD-15" treats
  # PERIOD as a month, so for weekly data every week >= 13 became an invalid
  # date (NA) and ~80% of the series silently vanished from the figures.
  is_weekly <- ("WM_IDENTIFIER" %in% names(total_predictions) &&
                any(total_predictions$WM_IDENTIFIER == "Week", na.rm = TRUE)) ||
               suppressWarnings(max(total_predictions$PERIOD, na.rm = TRUE)) > 12

  plot_data <- total_predictions %>%
    filter(Model %in% selected_models) %>%
    mutate(
      TimePoint     = if (is_weekly) {
        ISOweek::ISOweek2date(sprintf("%d-W%02d-4", YEAR, PERIOD))
      } else {
        as.Date(sprintf("%d-%02d-15", YEAR, PERIOD))
      },
      EXCESS_DEATHS = NO_DEATHS - EXP_DEATHS,
      P_SCORE       = ifelse(abs(EXP_DEATHS) > 1e-6, EXCESS_DEATHS / EXP_DEATHS * 100, NA_real_)
    )
  if (nrow(plot_data) == 0) return(na_res)

  model_cols <- get_model_colors()

  # Event windows for shading.
  ev <- NULL
  if (all(c("event_index", "event_name") %in% names(plot_data))) {
    ev <- plot_data %>%
      filter(!is.na(event_index), as.character(event_index) != "0",
             !is.na(event_name), event_name != "") %>%
      group_by(event_name) %>%
      summarise(start = min(TimePoint, na.rm = TRUE), end = max(TimePoint, na.rm = TRUE),
                .groups = "drop")
    if (nrow(ev) == 0) ev <- NULL
  }
  add_events <- function(p, ymin, ymax) {
    if (is.null(ev) || !is.finite(ymin) || !is.finite(ymax)) return(p)
    p +
      annotate("rect", xmin = ev$start, xmax = ev$end, ymin = ymin, ymax = ymax,
               fill = "#D55E00", alpha = 0.07) +
      annotate("text", x = ev$start + (ev$end - ev$start) / 2, y = ymax,
               label = ev$event_name, vjust = 1.2, size = 3, colour = "#7a2d00")
  }
  save_png <- function(p) {
    f <- tempfile(fileext = ".png")
    ggsave(f, p, width = 10, height = 5.2, dpi = dpi)
    f
  }
  bar_w <- {
    sp <- as.numeric(stats::median(diff(sort(unique(plot_data$TimePoint)))))
    if (!is.finite(sp) || sp <= 0) sp <- 30
    sp * 0.8
  }

  # 1) Recorded vs expected
  yr <- range(c(plot_data$NO_DEATHS, plot_data$EXP_DEATHS), na.rm = TRUE)
  acm <- ggplot(plot_data, aes(x = TimePoint)) +
    geom_line(aes(y = NO_DEATHS, colour = "Recorded"), linewidth = 0.9) +
    geom_line(aes(y = EXP_DEATHS, colour = Model), linetype = "dashed", linewidth = 0.7)
  acm <- add_events(acm, yr[1], yr[2]) +
    scale_colour_manual(name = NULL, values = c("Recorded" = "black", model_cols)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "All-cause mortality: recorded vs expected", x = NULL, y = "Number of deaths") +
    theme_wpro() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 2) Excess deaths
  er <- range(plot_data$EXCESS_DEATHS, na.rm = TRUE)
  exc <- ggplot(plot_data, aes(x = TimePoint, y = EXCESS_DEATHS, fill = Model)) +
    geom_col(position = position_dodge(width = bar_w), width = bar_w, alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
  exc <- add_events(exc, er[1], er[2]) +
    scale_fill_manual(values = model_cols) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Excess deaths", x = NULL, y = "Excess deaths") +
    theme_wpro() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 3) P-score
  pd <- plot_data %>% filter(!is.na(P_SCORE))
  ps <- ggplot(pd, aes(x = TimePoint, y = P_SCORE, fill = Model)) +
    geom_col(position = position_dodge(width = bar_w), width = bar_w, alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    scale_fill_manual(values = model_cols) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "P-score (excess as % of expected)", x = NULL, y = "P-score (%)") +
    theme_wpro() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  out <- tryCatch(
    list(acm = save_png(acm), excess = save_png(exc), pscore = save_png(ps)),
    error = function(e) { message("build_report_figures error: ", e$message); na_res }
  )
  out
}
