library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", all_of(C1_FC_names)))

# Figure 3
colour_values <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")
names(colour_values) <- C1_FC_names


p1 <- df_RunExmpl |>
  mutate(
    ASSA = ifelse(y == 0, 1, 0),
    MCSTAT = ifelse(y == 0, 1, 0)
  ) |>
  murphy(ref = df_RunExmpl$NOAA) |>
  autoplot() +
  ylab("Mean elem. score difference") +
  ggtitle("(a) SIDC - NOAA") +
  ylim(c(-0.01, 0.1)) +
  theme(plot.title = element_text(size = 14, hjust = 0)) +
  xlab(expression("Threshold " * theta))

p2 <- df_RunExmpl |>
  murphy() |>
  autoplot() +
  ggtitle("(b) Murphy") +
  xlab(expression("Threshold " * theta))

p3 <- df_RunExmpl |>
  mutate(
    NOAA = ifelse(y == 0, 1, 0),
    SIDC = ifelse(y == 0, 1, 0)
  ) |>
  murphy(ref = df_RunExmpl$MCSTAT) |>
  autoplot() +
  ylab("Mean elem. score difference") +
  ggtitle("(c) ASSA - MCSTAT") +
  ylim(c(-0.06, 0.06)) +
  theme(plot.title = element_text(size = 14, hjust = 0)) +
  xlab(expression("Threshold " * theta))

p_Murphy <- p1 + p2 + p3 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  scale_colour_manual(
    values = colour_values,
    guide = guide_legend(title = "Forecast")
  )

ggsave(
  filename = "plots/Fig03_MurphyIllustration_C1Flares.pdf",
  plot = p_Murphy,
  width = 24, height = 10.5, units = "cm"
)

