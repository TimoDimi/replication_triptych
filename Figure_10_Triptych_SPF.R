library(lubridate)
library(tidyr)
source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

set.seed(20230123)

load("data/spf.gdp.long.rda")

# Fix
SPF_individual <- 65

# Clean SPF forecast-realization tibble
h_set <- 0:4

# Clean tibble with SPF consensus and #65 forecaster
SPF_clean <- spf.gdp.long %>%
  as_tibble() %>%
  dplyr::filter(ID %in% c(0, SPF_individual) & FC.Horizon %in% h_set) %>%
  select(DATE.issued, DATE.FC.due, FC.Horizon, ID, Prob.Forecast, gdp.first.recess) %>%
  mutate(ID = as.factor(ID))


################################################################################
# Figure 10: Compare the SPF average on different forecast horizons

h_set_avg <- c(0, 1, 2, 4)

# FCs in wide tibble format including the climatology
SPFavg_wide <- SPF_clean %>%
  filter(ID == 0, FC.Horizon %in% h_set_avg) %>%
  select(-c("DATE.issued", "ID")) %>%
  pivot_wider(names_from = FC.Horizon, values_from = Prob.Forecast) %>%
  arrange(DATE.FC.due) %>%
  filter(DATE.FC.due >= as_date("1971-04-01")) %>% # Before that, even many NAs in the avg 4Q ahead forecasts!
  na.omit()

# Triptych and plot
trpt_SPFavg_horizons <- SPFavg_wide %>%
  select(-DATE.FC.due) %>%
  rename(y = gdp.first.recess) %>%
  triptych()

summary(trpt_SPFavg_horizons)

fig10 <- ggplot2::autoplot(
  object = trpt_SPFavg_horizons,
  RelDiag_breaks = seq(0, 1, length.out = 21),
  plot_cols = gg_color_hue(4)[c(2, 1, 3, 4)],
  plot_linetypes = "solid",
  plot_legend_title = "Forecast horizon"
)

ggsave(
  filename = paste0("plots/Fig10_triptych_SPF_Consensus.pdf"),
  plot = fig10,
  width = 24, height = 10.5, units = "cm"
)



################################################################################
# Table 3: Compare SPF average and SPF #65

# FCs in wide tibble format including the climatology
SPF_wide <- SPF_clean %>%
  pivot_wider(names_from = ID, values_from = Prob.Forecast) %>%
  filter(DATE.FC.due >= as_date("1971-04-01"))


# Filter data such that we only use Date.FC.due for which all horizon forecasts
# in h_set_plot are available!
h_set_plot <- c(1, 2, 4)

SPF_wide_complete_cases <- SPF_wide %>%
  filter(FC.Horizon %in% h_set_plot) %>%
  na.omit() %>%
  arrange(DATE.FC.due) %>%
  group_by(DATE.FC.due) %>%
  mutate(n_horizons = n()) %>%
  filter(n_horizons == length(h_set_plot)) %>%
  ungroup()


# Compare SPF average against individual forecasts
SPF_score_decomp <- tibble()
for (h in h_set_plot) {
  df_SPF_trpt <- SPF_wide_complete_cases %>%
    filter(FC.Horizon == h) %>%
    rename(y = gdp.first.recess, "SPF Average" = "0", "SPF #65" = "65") %>%
    select(-c("DATE.issued", "DATE.FC.due", "FC.Horizon", "n_horizons"))

  trpt_SPF <- triptych(df_SPF_trpt)
  SPF_score_decomp <- bind_rows(
    SPF_score_decomp,
    summary(trpt_SPF$RelDiag) %>% mutate(h = h)
  )
}

# Table 3:
SPF_score_decomp %>%
  mutate(across(mean_score:uncertainty, round, digits = 3))
