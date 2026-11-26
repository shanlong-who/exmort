# Run from the package root to refresh the figure used on the homepage.
library(ggplot2)
library(DSIR)

# The six observations are the arithmetic example in models-and-interpretation.Rmd.
monthly.example <- data.frame(
  month = 1:6,
  observed = c(980, 1010, 1120, 1230, 1190, 1060),
  expected = 1000
)

example.plot <- ggplot(monthly.example, aes(x = month)) +
  geom_line(aes(y = expected), colour = "#555555", linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = observed), colour = "#0072B2", linewidth = 1.2) +
  geom_point(aes(y = observed), colour = "#0072B2", size = 2.2) +
  scale_y_continuous(breaks = c(950, 1000, 1100, 1200, 1250),
                     labels = scales::label_comma(), limits = c(945, 1265)) +
  labs(x = NULL, y = NULL) +
  theme_dsi(base_size = 15, grid = "y") +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_text(size = 14, colour = "#425F71"),
        plot.margin = margin(3, 5, 0, 0))

dir.create("pkgdown/assets", recursive = TRUE, showWarnings = FALSE)
english.plot <- example.plot + scale_x_continuous(breaks = 1:6, labels = month.abb[1:6])
chinese.plot <- example.plot + scale_x_continuous(breaks = 1:6, labels = paste0(1:6, "月"))
ggsave("pkgdown/assets/monthly-example.png", english.plot, device = grDevices::png,
       width = 6, height = 3.1, dpi = 200, bg = "white")
ggsave("pkgdown/assets/monthly-example-zh.png", chinese.plot, device = grDevices::png,
       width = 6, height = 3.1, dpi = 200, bg = "white")
