library(tidyr)
source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

set.seed(202301123)

# Load an filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", all_of(C1_FC_names)))

# Figure 1 Triptych
trpt_RunExmpl <- triptych(df_RunExmpl, confidence = TRUE)
p_RunExmpl <- ggplot2::autoplot(
  object = trpt_RunExmpl,
  Murphy_scoretype = "score",
  RelDiag_breaks = seq(0, 1, length.out = 11),
  plot_cols = gg_color_hue(4)[c(2, 1, 3, 4)],
  plot_linetypes = "solid"
)

p_RunExmpl

ggsave(
  filename = "plots/Fig01_triptych_C1Flares.pdf",
  plot = p_RunExmpl,
  width = 24, height = 10.5, units = "cm"
)


# Table 1: Brier scores, Log scores and misclassication rates  ################
# see "R/summary.R" for score definition

df_RunExmpl %>%
  tidyr::pivot_longer(cols = !y, names_to = "variable") %>%
  group_by(variable) %>%
  summarize(
    Brier_score = mean(brier_score(y, value)),
    log_score = mean(log_score(y, value)),
    MR_score = mean(MR_score(y, value))
  ) %>%
  arrange(Brier_score)


# Table 2: Score decompositions        #########################################

# CORP Brier score decomposition
summary(trpt_RunExmpl, score = brier_score) %>%
  arrange(factor(forecast, levels = c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  mutate(across(mean_score:uncertainty, round, digits = 3))

# CORP Log score decomposition
summary(trpt_RunExmpl, score = log_score) %>%
  arrange(factor(forecast, levels = c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  mutate(across(mean_score:uncertainty, round, digits = 3))

# CORP MR score decomposition
summary(trpt_RunExmpl, score = MR_score) %>%
  arrange(factor(forecast, levels = c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  mutate(across(mean_score:uncertainty, round, digits = 3))
