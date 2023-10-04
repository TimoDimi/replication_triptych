library(triptych)
library(dplyr)

# Load and filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full |>
  select(c("y", all_of(C1_FC_names)))

table2_BS <- mcbdsc(df_RunExmpl) |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))
table2_LS <- mcbdsc(df_RunExmpl, score = "log_score") |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))
table2_MR <- mcbdsc(df_RunExmpl, score = "MR_score") |>
  estimates() |>
  mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))

cat(
  file = "tables/Table_02_Score_Decompositions_C1Flares.txt",
  c(
    "Brier Score",
    capture.output(table2_BS),
    "\n",
    "Logarithmic Score",
    capture.output(table2_LS),
    "\n",
    "Misclassification Rate",
    capture.output(table2_MR)
  ),
  sep = "\n"
)
