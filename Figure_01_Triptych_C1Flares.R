# Install the triptych package from Github:
# devtools::install_github("aijordan/triptych")

library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full |>
  select(c("y", all_of(C1_FC_names)))

colour_values <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")
names(colour_values) <- C1_FC_names

trpt_RunExmpl <- triptych(df_RunExmpl) |>
  add_consistency()


# Figure 1 Triptych
p_RunExmpl <- ggplot2::autoplot(
  object = trpt_RunExmpl,
  breaks = seq(0, 1, length.out = 11)
) &
  scale_colour_manual(
    values = colour_values,
    guide = guide_legend(title = "Forecast"))

# Rename the x-axis in the Murphy diagram
rename_Murphy_axis <- function(p_patchwork) {
  p_patchwork[[1]] <- p_patchwork[[1]] + xlab(expression("Threshold " * theta))
  p_patchwork
}
p_RunExmpl <- rename_Murphy_axis(p_RunExmpl)

ggsave(
  filename = "plots/Fig01_triptych_C1Flares.pdf",
  plot = p_RunExmpl,
  width = 24, height = 10.5, units = "cm"
)



# Table 1
estimates(mcbdsc(df_RunExmpl)) |>
  select(forecast, mean_score) |>
  rename(Brier_Score = mean_score) |>
  full_join(
    estimates(mcbdsc(df_RunExmpl, score="log_score")) |>
      select(forecast, mean_score),
    by="forecast") |>
  full_join(
    estimates(mcbdsc(df_RunExmpl, score="MR_score")) |>
      select(forecast, mean_score),
    by="forecast")


# Table 2
mcbdsc(df_RunExmpl) |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))
mcbdsc(df_RunExmpl, score = "log_score") |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))
mcbdsc(df_RunExmpl, score = "MR_score") |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))



