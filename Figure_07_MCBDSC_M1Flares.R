library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and filter data
load(file = "data/M1_flares.rda")

# MCB-DSC Plot of all M1 forecasts
trpt_M1full <- triptych(df_M1full)

# Assign plot colors
plot_cols_M1 <- c(
  "NICT" = "#56B4E9",
  "NOAA" = "#E69F00",
  "ASSA" = "#D55E00",
  "MCSTAT" = "#CC79A7"
)

MCBDSC_point_cols_M1 <- c(
  "AMOS" = "black",
  "ASAP" = "black",
  plot_cols_M1[3], # ASSA
  "BOM" = "black",
  "CLIM120" = "black",
  "DAFFS" = "black",
  "DAFFS-G" = "black",
  "MAG4VW" = "black",
  "MAG4VWF" = "black",
  "MAG4W" = "black",
  "MAG4WF" = "black",
  "MCEVOL" = "black",
  plot_cols_M1[4], # MCSTAT
  "MOSWOC" = "black",
  plot_cols_M1[1], # NICT
  plot_cols_M1[2], # NOAA
  "SIDC" = "black"
)


# Figure 7: Brier and Log Score MCB-DSC Plots.
MCBDSC_M1_BrierScore <- mcbdsc(df_M1full) |>
  autoplot(
    MCBDSC_repel = TRUE,
    colour_values = MCBDSC_point_cols_M1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.0485),
    size_axislabels = 12) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))

MCBDSC_M1_LogScore <- mcbdsc(df_M1full, score = "log_score") |>
  autoplot(
    MCBDSC_repel = TRUE,
    colour_values = MCBDSC_point_cols_M1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.2),
    size_axislabels = 12
    ) +
  annotate("text", x = 0.2, y = 0.0405, label = "MAG4VWF", size = 3, hjust = 1) +
  ggtitle("(b) Logarithmic Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))

MCBDSC_M1_BrierLogScores <- MCBDSC_M1_BrierScore + MCBDSC_M1_LogScore

ggsave(
  filename = "plots/Fig07_M1Flares_MCBDSC.pdf",
  plot = MCBDSC_M1_BrierLogScores,
  width = 24, height = 12, units = "cm"
)


