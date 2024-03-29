library(triptych)
library(ggplot2)
library(patchwork)
library(dplyr)


# Color values
colour_values_crossings <- c("X1" = "#D55E00",
                             "X2" = "#0072B2")

# generate calibrated example
n1 <- 150
n0 <- 100
y <- rep.int(c(0, 1), c(n0, n1))

getX <- function(w0, w1) {
  (w1 / (w0 + w1)) |>
    (\(x) c(rep.int(x, w0), rep.int(x, w1)))()
}

w1_0 <- c(14, 40, 28, 18, 0)
w1_1 <- c(0, 10, 28, 26, 36) * n1 / n0
w2_0 <- c(34, 8, 4, 12, 10, 9, 14, 3, 3, 2, 1)
w2_1 <- c(4, 2, 2, 8, 8, 8, 14, 6, 14, 16, 18) * n1 / n0
x1 <- getX(w1_0, w1_1)
x2 <- getX(w2_0, w2_1)

# ROC calculations
ROC_curves <- roc(list(X1 = x1, X2 = x2, y = y)) |>
  estimates() |>
  relocate(Forecast = forecast, .after = last_col()) |>
  arrange(Forecast, FAR, HR)

# Murphy calculations
mdf <- bind_rows(
  murphy(list(X1 = x1, y = y)) |> estimates(at = sort(unique(c(x1, y)))),
  murphy(list(X2 = x2, y = y)) |> estimates(at = sort(unique(c(x2, y))))
) |>
  rename(Forecast = forecast, theta = knot, elem_score = mean_score)

# CDF calculations
distdf <- bind_rows(
  tibble(
    Forecast = "X1",
    q = c(0, w1_1 / (w1_0 + w1_1), 1),
    p = c(0, cumsum(w1_0 + w1_1) / (n0 + n1), 1)
  ),
  tibble(
    Forecast = "X2",
    q = c(0, w2_1 / (w2_0 + w2_1), 1),
    p = c(0, cumsum(w2_0 + w2_1) / (n0 + n1), 1)
  )
)

# ROC curves
cc <- seq(0, 1, len = 11)
p0 <- n0 / (n0 + n1)
p1 <- n1 / (n0 + n1)
r_segments <- data.frame(
  x = 1 - pmin(1, cc / p0),
  y = pmin(1, (1 - cc) / p1),
  xend = 1 - pmax(0, (cc - p1) / p0),
  yend = pmax(0, 1 - cc / p1)
)
gg_ROC <- ggplot() +
  geom_segment(
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    data = r_segments,
    color = "lightgrey"
  ) +
  geom_line(
    mapping = aes(x = FAR, y = HR, col = Forecast),
    data = ROC_curves,
    linewidth = 1
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    aspect.ratio = 1
  ) +
  labs(
    x = "False alarm rate",
    y = "Hit rate",
    title = "(a) ROC"
  ) +
  scale_colour_manual(values = colour_values_crossings)


# Murphy diagrams
gg_Murphy <- ggplot() +
  geom_segment(
    mapping = aes(
      x = seq(0, 1, len = 11),
      xend = seq(0, 1, len = 11),
      y = 0,
      yend = max(mdf$elem_score)
    ),
    col = "lightgrey"
  ) +
  geom_line(
    mapping = aes(
      x = theta,
      y = elem_score,
      col = Forecast
    ),
    data = mdf,
    linewidth = 1
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    aspect.ratio = 1
  ) +
  labs(
    x = expression("Threshold" ~ theta),
    y = "Mean elementary score",
    title = "(b) Murphy"
  ) +
  scale_colour_manual(values = colour_values_crossings)


# Unconditional distributions
blocks <- data.frame(
  x = with(
    arrange(distdf, q),
    c(q[c(1, 4, 4, 5, 5, 8, 8, 13, 13)], rep(0.5 * sum(q[15:16]), 2), 1)
  ),
  ymin = with(
    arrange(distdf, p),
    c(p[c(1, 1, 3, 3)], rep(0.5 * sum(p[4:5]), 2), p[c(7, 7, 12, 12, 14, 14)])
  ),
  ymax = with(
    arrange(distdf, p),
    c(p[c(3, 3)], rep(0.5 * sum(p[4:5]), 2), p[c(7, 7, 12, 12, 14, 14, 20, 20)])
  )
)
gg_CDF <- ggplot() +
  theme_bw() +
  geom_ribbon(
    mapping = aes(x = x, ymin = ymin, ymax = ymax),
    data = blocks,
    fill = "lightgrey",
    col = NA
  ) +
  geom_step(
    mapping = aes(q, p, col = Forecast),
    data = distdf,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = c(0.075, 0.38, 0.78),
    y = c(0.033, 0.25, 0.74),
    label = "-"
  ) +
  annotate(
    "text",
    x = c(0.21, 0.56, 0.64, 0.965),
    y = c(.11, 0.31, 0.6, 0.83),
    label = "+"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    aspect.ratio = 1
  ) +
  labs(
    x = "Forecast value",
    y = "Level",
    title = "(c) Unconditional CDF/Quantile Function"
  ) +
  scale_colour_manual(values = colour_values_crossings)

gg_composite <- gg_ROC +
  gg_Murphy +
  gg_CDF +
  plot_layout(
    ncol = 3,
    nrow = 2,
    design = "133\n233",
    guides = "collect"
  ) &
  theme(legend.position = "bottom")

ggsave(
  filename = "plots/Fig12_Crossingpoints.pdf",
  plot = gg_composite,
  width = 24, height = 17.9, units = "cm"
)
