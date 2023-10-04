library(triptych)
library(dplyr)

# Load and filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full |>
  select(c("y", all_of(C1_FC_names)))

table1 <- estimates(mcbdsc(df_RunExmpl)) |>
  select(forecast, mean_score) |>
  rename(Brier_score = mean_score) |>
  full_join(
    estimates(mcbdsc(df_RunExmpl, score="log_score")) |>
      select(forecast, log_score = mean_score),
    by="forecast") |>
  full_join(
    estimates(mcbdsc(df_RunExmpl, score="MR_score")) |>
      select(forecast, MR_score = mean_score),
    by="forecast")

cat(
  file = "tables/Table_01_Mean_Scores_C1Flares.txt",
  capture.output(table1),
  sep = "\n"
)
