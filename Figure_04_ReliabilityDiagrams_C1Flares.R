library(calibrationband)
source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

set.seed(20230123)

# Load an filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")
C1_FC_colours <- gg_color_hue(4)[c(2, 1, 3, 4)]

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", all_of(C1_FC_names)))

trpt <- triptych(df_RunExmpl)


#      Illustration Reliability Diagrams     ########
# Panel (a)
p_RelDiag_RunExmpl <- ggplot2::autoplot(
    object = trpt,
    plot_type = "ReliabilityDiagram",
    plot_linetypes = "solid",
    RelDiag_breaks = seq(0, 1, length.out = 11),
    plot_cols = C1_FC_colours
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  ggtitle("")

p_RelDiag_RunExmpl

# Panel (b)
# data of universally valid confidence bands
Cal_Bands_tbl <- tibble()
for (FC_name in C1_FC_names) {
  Cal_Bands_tbl <- calibrationband::calibration_bands(
    x = df_RunExmpl[[FC_name]],
    y = df_RunExmpl$y,
    nc = TRUE,
    method = "round",
    digits = 2,
    alpha = 0.1
  )$bands %>%
    mutate(forecast = FC_name) %>%
    bind_rows(Cal_Bands_tbl, .)
}
# data of reliabilitydiagram
RelDiag_hlp <- trpt$RelDiag

# Histogram data, binning, and normalization factor for equal histogram areas
r_cases <- bind_rows(lapply(RelDiag_hlp, function(l) l$cases), .id = "forecast")
RelDiag_breaks <- seq(0, 1, length.out = 11)
max_density <- r_cases %>%
  group_by(forecast) %>%
  summarize(density = hist(x, RelDiag_breaks, plot = FALSE)$density) %>%
  pull(density) %>%
  max()

# plot for panel (b)
p_RelDiag_CalBands <- autoplot(
    object = RelDiag_hlp,
    params_diagonal = NA,
    params_CEPline = NA
  ) +
  geom_histogram(
    mapping = aes(x = x, y = 0.2 / max_density * after_stat(density)),
    data = r_cases,
    breaks = RelDiag_breaks,
    colour = "black",
    fill = NA
  ) +
  geom_ribbon(
    data = Cal_Bands_tbl,
    aes(x = x, ymin = lwr, ymax = upr),
    fill = "grey50", alpha = 0.3
  ) +
  autolayer(
    object = RelDiag_hlp,
    params_diagonal = NULL,
    params_CEPline = list(size = 0.5)
  ) +
  facet_wrap(. ~ factor(forecast, levels = names(RelDiag_hlp)), nrow = 2) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_manual(values = C1_FC_colours, limits = C1_FC_names) +
  scale_linetype_manual(values = rep("solid", 4), limits = C1_FC_names) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )


# Add together with patchwork and save
p.titles <- c("(a) Calibration Bands", "(b) Universally Valid Confidence Bands")
p.theme <- theme(plot.title = element_text(size = 14, hjust = 0))
pp <- (p_RelDiag_RunExmpl + ggtitle(p.titles[1]) + p.theme) +
  (p_RelDiag_CalBands + ggtitle(p.titles[2]) + p.theme)

ggsave(
  filename = "plots/Fig04_ReliabilityDiagrams_C1Flares.pdf",
  plot = pp,
  width = 24, height = 14, units = "cm"
)
