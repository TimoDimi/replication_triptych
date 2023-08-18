library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and filter data
load(file = "data/M1_flares.rda")

# Subset of forecasts for the C1 flares running example
M1_FCnames_short <- c("NICT", "NOAA", "ASSA", "MCSTAT")

df_M1_short <- df_M1full |>
  select(c("y", all_of(M1_FCnames_short)))

# Assign plot colors
plot_cols_M1 <- c(
  "NICT" = "#56B4E9",
  "NOAA" = "#E69F00",
  "ASSA" = "#D55E00",
  "MCSTAT" = "#CC79A7"
)


# Figure 8: Triptych for the M1 forecasts
trpt_M1 <- triptych(df_M1_short) |>
  add_consistency()

p_M1 <- autoplot(trpt_M1,
                breaks= seq(0, 1, length.out = 21)) &
  scale_colour_manual(
    values = plot_cols_M1,
    guide = guide_legend(title = "Forecast"))

# Rename the x-axis in the Murphy diagram
rename_Murphy_axis <- function(p_patchwork) {
  p_patchwork[[1]] <- p_patchwork[[1]] + xlab(expression("Threshold " * theta))
  p_patchwork
}
p_M1 <- rename_Murphy_axis(p_M1)


ggsave(
  filename = "plots/Fig08_triptych_M1Flares.pdf",
  plot = p_M1,
  width = 24, height = 10.5, units = "cm"
)
