library(dplyr)
library(ggplot2)

data <- Sodium_Levy_Jennings |>
  select(1:2)

results <- as.numeric(as.character(data[[2]]))
results <- results[is.finite(results)]

mean_value <- 142
sd_value <- 3

chart_data <- data.frame(
  days = seq_along(results),
  result = results
)

control_lines <- data.frame(
  level = mean_value + c(-3, -2, -1, 0, 1, 2, 3) * sd_value,
  label = c("-3 SD", "-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD", "+3 SD")
)

ggplot(chart_data, aes(x = days, y = result)) +
  geom_hline(
    data = control_lines,
    aes(yintercept = level, color = label),
    linetype = "dashed",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "-3 SD" = "red", "-2 SD" = "orange", "-1 SD" = "gold",
    "Mean" = "blue",
    "+1 SD" = "gold", "+2 SD" = "orange", "+3 SD" = "red"
  ), guide = "none") +
  scale_x_continuous(breaks = chart_data$days) +
  scale_y_continuous(
    breaks = control_lines$level,
    sec.axis = sec_axis(
      ~ .,
      breaks = control_lines$level,
      labels = control_lines$label,
      name = NULL
    )
  ) +
  labs(
    title = "Sodium Levey-Jennings Chart",
    x = "Days",
    y = "Sodium (mg/dL)"
  ) +
  theme_minimal()

