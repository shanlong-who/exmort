# Event annotations
# Adds start/end vertical lines, a horizontal span, and a label for each
# event row to a base ggplot.

library(ggplot2)
library(dplyr)

add_event_annotations <- function(p, event_data, y_min, y_max) {
  if (is.null(event_data) || nrow(event_data) == 0) {
    return(p)
  }

  # Use any colour palette already attached to event_data; otherwise
  # generate a Set3 palette keyed by event name.
  event_colors <- attr(event_data, "event_colors")
  if (is.null(event_colors)) {
    event_colors <- RColorBrewer::brewer.pal(min(nrow(event_data), 9), "Set3")
    names(event_colors) <- unique(event_data$event_name)
  }

  for (i in 1:nrow(event_data)) {
    event <- event_data[i, ]
    color <- event_colors[event$event_name]

    # Vertical line at event start.
    p <- p + geom_vline(
      xintercept = event$start_time,
      color = color,
      alpha = 0.5,
      linetype = "solid",
      linewidth = 1
    )

    # Vertical line at event end.
    p <- p + geom_vline(
      xintercept = event$end_time,
      color = color,
      alpha = 0.5,
      linetype = "solid",
      linewidth = 1
    )

    # Horizontal span with arrow heads, drawn near the bottom of the plot.
    p <- p + annotate(
      "segment",
      x = event$start_time,
      xend = event$end_time,
      y = y_min + (y_max - y_min) * 0.1, # 10% up the y axis
      yend = y_min + (y_max - y_min) * 0.1,
      color = color,
      alpha = 0.7,
      linewidth = 0.8,
      arrow = arrow(ends = "both", type = "open", length = unit(0.1, "inches"))
    )

    # Event label centred on the span.
    p <- p + geom_text(
      data = data.frame(
        x = event$start_time + (event$end_time - event$start_time) / 2,
        y = y_min + (y_max - y_min) * 0.15,
        label = event$event_name
      ),
      aes(x = x, y = y, label = label),
      hjust = 0.5,
      vjust = -0.5,
      size = 3,
      color = "darkred",
      inherit.aes = FALSE
    )
  }

  return(p)
}
