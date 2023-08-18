library(triptych)
library(dplyr)
library(ggplot2)
library(patchwork)


###########################################################################
# Illustrations with analytical calculations

# Assign colors
sim_plot_cols <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")
names(sim_plot_cols) <- c("X0", "X1", "X2", "X3")


t_seq <- seq(0, 1, length.out = 1001)
df_t <- tibble(t = t_seq) %>%
  slice(c(-1, -n()))

# Setting A
df_A_X0 <- df_t %>%
  mutate(
    CEP = t,
    CEP_point = NA,
    HR = (1 - t^2),
    FAR = (1 - t)^2
  ) %>%
  bind_rows(tibble(t = c(0, 1), HR = c(1, 0), FAR = c(1, 0))) %>%
  mutate(
    density = 1,
    density_max = max(density, na.rm = TRUE),
    density_rel = density / 10,
    point_mass = 0
  ) %>%
  arrange(t, desc(HR), desc(FAR)) %>%
  mutate(Setting = "A", FC = "X0")

df_A_X1 <- df_t %>%
  filter(((3 / 8 <= t) & (t <= 5 / 8))) %>%
  mutate(
    CEP = 4 * t - 1.5,
    CEP_point = NA,
    HR = (1 - (4 * t - 1.5)^2),
    FAR = (2.5 - 4 * t)^2
  ) %>%
  bind_rows(tibble(t = c(0, 1), HR = c(1, 0), FAR = c(1, 0))) %>%
  mutate(
    density = ifelse(((3 / 8 <= t) & (t <= 5 / 8)), 4, NA),
    density_max = max(density, na.rm = TRUE),
    density_rel = density / 10,
    point_mass = 0
  ) %>%
  arrange(t, desc(HR), desc(FAR)) %>%
  mutate(Setting = "A", FC = "X1")


# Setting B
df_B_X1 <- df_t %>%
  mutate(
    CEP = ifelse((t <= 0.25 | t >= 0.75), t, NA),
    CEP_point = ifelse((t == 0.5), t, NA),
    HR = ifelse(t < 0.5,
      pmax((1 - t^2), 15 / 16),
      pmin((1 - t^2), 7 / 16)
    ),
    FAR = ifelse(t < 0.5,
      pmax((1 - t)^2, 9 / 16),
      pmin((1 - t)^2, 1 / 16)
    )
  ) %>%
  bind_rows(tibble(t = c(0, 1), HR = c(1, 0), FAR = c(1, 0))) %>%
  mutate(
    density = ifelse((t < 0.25 | t > 0.75), 1, NA),
    density_max = max(density, na.rm = TRUE),
    density_rel = density / 10,
    point_mass = ifelse(t == 0.5, 0.5, NA)
  ) %>%
  arrange(t, desc(HR), desc(FAR)) %>%
  mutate(Setting = "B", FC = "X1")


df_B_X2 <- df_t %>%
  mutate(
    CEP = t,
    CEP = ifelse((t >= 0.25 & t <= 0.75), t, NA),
    CEP_point = ifelse((t == 0.125 | t == 0.875), t, NA),
    HR = 1 * as.numeric(t < 1 / 8) +
      15 / 16 * as.numeric((t < 1 / 4) & (t >= 1 / 8)) +
      (1 - t^2) * as.numeric((t <= 3 / 4) & (t >= 1 / 4)) +
      7 / 16 * as.numeric((t < 7 / 8) & (t > 3 / 4)),
    FAR = 1 * as.numeric(t < 1 / 8) +
      9 / 16 * as.numeric((t < 1 / 4) & (t >= 1 / 8)) +
      (1 - t)^2 * as.numeric((t <= 3 / 4) & (t >= 1 / 4)) +
      1 / 16 * as.numeric((t < 7 / 8) & (t > 3 / 4))
  ) %>%
  bind_rows(tibble(t = c(0, 1), HR = c(1, 0), FAR = c(1, 0))) %>%
  mutate(
    density = ifelse((t >= 0.25 & t <= 0.75), 1, NA),
    density_max = max(density, na.rm = TRUE),
    density_rel = density / 10,
    point_mass = ifelse((t == 1 / 8 | t == 7 / 8), 0.25, NA)
  ) %>%
  arrange(t, desc(HR), desc(FAR)) %>%
  mutate(Setting = "B", FC = "X2")



# Setting C, based on approximating the integral numerically
j_seq <- 0:3
Fj <- function(x, j) {
  pnorm(qnorm(x, sd = sqrt((j + 1) / (4 - j))))
}


df_C <- df_t %>%
  bind_rows(tibble(t = c(0, 1))) %>%
  arrange(t) %>%
  summarize(
    t = rep(t, each = length(j_seq)),
    j = rep(j_seq, n()),
    FC = paste0("X", rep(j_seq, n()))
  ) %>%
  group_by(FC) %>%
  mutate(
    CEP = t,
    CEP_point = NA,
    point_mass = 0,
    density = ifelse(t %in% c(0, 1),
      ifelse(FC %in% c("X2", "X3"), 0, NA),
      sqrt((j + 1) / (4 - j)) * dnorm(sqrt((j + 1) / (4 - j)) * qnorm(t)) / dnorm(qnorm(t))
    ),
    density_max = max(density, na.rm = TRUE),
    density_rel = pmin(density / 10, 1, na.rm = TRUE),
    CDF = Fj(t, j)
  ) %>%
  rowwise() %>%
  mutate(Integral_hlp = integrate(Fj, 0, t, j)$value) %>%
  ungroup() %>%
  mutate(
    HR = 1 - 2 * (t * CDF - Integral_hlp),
    FAR = 1 - 2 * CDF + 2 * t * CDF - 2 * Integral_hlp,
    Setting = "C"
  ) %>%
  select(c("t", "CEP", "HR", "FAR", "density", "density_max", "density_rel", "point_mass", "Setting", "FC"))



# Join all tibbles
df_ROC <- bind_rows(
  df_A_X0,
  df_A_X1,
  df_B_X1,
  df_B_X2,
  df_C
)
# df_C_sim)


# Plot settings
size_title <- 14
size_legend <- 12
size_axislabels <- 10
size_axisticks <- 10
plot_legend_title <- "Forecast"


for (setting_choice in c("A", "B", "C")) {
  df_setting <- filter(df_ROC, Setting == setting_choice)
  scale_colour_values <- sim_plot_cols[unique(df_setting$FC)]

  # Manual ROC Curve Plot
  p_ROC <- ggplot(df_setting) +
    geom_line(aes(x = FAR, y = HR, col = FC)) +
    theme_bw() +
    xlab("False alarm rate") +
    ylab("Hit rate") +
    ggtitle("ROC") +
    scale_colour_manual(values = scale_colour_values) +
    theme_bw() +
    theme(
      legend.title = element_text(size = size_legend),
      legend.text = element_text(size = size_legend),
      plot.title = element_text(hjust = 0.5, size = size_title),
      axis.title = element_text(size = size_axislabels),
      axis.text.x = element_text(size = size_axisticks),
      axis.text.y = element_text(size = size_axisticks),
      legend.position = "bottom",
      legend.key.size = grid::unit(2, "lines"),
      aspect.ratio = 1
    ) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1))

  # Tibble for the Murphy Diagram
  df_Murphy <- df_setting %>%
    filter(t <= 1, t >= 0) %>%
    group_by(Setting, FC) %>%
    mutate(
      theta = t,
      el_score = (1 - theta) * (1 - HR) + theta * FAR
    )

  # Manual Murphy Diagram Plot
  p_Murphy <- ggplot(df_Murphy) +
    geom_line(aes(x = theta, y = el_score, col = FC)) +
    theme_bw() +
    xlab("Threshold value") +
    ylab("Mean elementary score") +
    ggtitle("Murphy") +
    scale_colour_manual(values = scale_colour_values) +
    theme_bw() +
    theme(
      legend.title = element_text(size = size_legend),
      legend.text = element_text(size = size_legend),
      plot.title = element_text(hjust = 0.5, size = size_title),
      axis.title = element_text(size = size_axislabels),
      axis.text.x = element_text(size = size_axisticks),
      axis.text.y = element_text(size = size_axisticks),
      legend.position = "bottom",
      legend.key.size = grid::unit(2, "lines"),
      aspect.ratio = 1
    ) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1))

  # Manual Reliability Diagram
  p_RelDiag <- ggplot(df_setting) +
    geom_col(aes(x = t, y = point_mass), width = 0.05, fill = "black") +
    geom_line(aes(x = t, y = density_rel), col = "darkgray") +
    geom_ribbon(aes(x = t, ymin = 0, ymax = density_rel), fill = "darkgray", alpha = 0.4) +
    geom_line(aes(x = t, y = CEP, col = FC)) +
    geom_point(aes(x = t, y = CEP_point, col = FC), size = 2, alpha = 1, show.legend = FALSE) +
    facet_wrap(~FC, nrow = 2) +
    xlab("Forecast value") +
    ylab("Conditional event probability") +
    ggtitle("Reliability") +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    scale_colour_manual(values = scale_colour_values) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text.y = element_blank(),
      strip.text.x = element_blank(),
      legend.title = element_text(size = size_legend),
      legend.text = element_text(size = size_legend),
      plot.title = element_text(hjust = 0.5, size = size_title),
      axis.title = element_text(size = size_axislabels),
      axis.text.x = element_text(size = size_axisticks),
      axis.text.y = element_text(size = size_axisticks),
      axis.text.y.right = element_text(color = "darkgray"),
      legend.position = "bottom",
      legend.key.size = grid::unit(2, "lines"),
      aspect.ratio = 1
    ) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1))



  # Join plots together with patchwork
  plot_margins <- grid::unit(c(0, 0.02, 0, 0.02), "npc")

  p_triptych <- p_Murphy + (p_RelDiag + theme(plot.margin = plot_margins)) + p_ROC +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")

  # Rename the x-axis in the Murphy diagram
  rename_Murphy_axis <- function(p_patchwork) {
    p_patchwork[[1]] <- p_patchwork[[1]] + xlab(expression("Threshold " * theta))
    p_patchwork
  }

  p_triptych <- rename_Murphy_axis(p_triptych)

  p_triptych

  ggsave(
    filename = paste0("./plots/Fig05_", setting_choice, "_PopQuantities.pdf"),
    plot = p_triptych,
    width = 24, height = 10.5, units = "cm"
  )
}
