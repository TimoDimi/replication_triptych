library(tidyverse)
library(lubridate)
library(patchwork)

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

load("data/spf.gdp.long.rda")

# Fix
SPF_individual <- 65

# Clean SPF forecast-realization tibble
h_set <- 0:4

# Clean tibble with SPF consensus and #65 forecaster
SPF_clean <- spf.gdp.long %>%
  tibble::as_tibble() %>%
  dplyr::filter(ID %in% c(0, SPF_individual) & FC.Horizon %in% h_set) %>%
  dplyr::select(DATE.issued, DATE.FC.due, FC.Horizon, ID, Prob.Forecast, gdp.first.recess) %>%
  dplyr::mutate(ID = as.factor(ID))


################################################################################
# Part (A) Compare the SPF average on different forecast horizons

h_set_avg <- c(0,1,2,4)

# FCs in wide tibble format including the climatology
SPFavg_wide <- SPF_clean %>%
  dplyr::filter(ID==0, FC.Horizon %in% h_set_avg) %>%
  select(-c("DATE.issued", "ID")) %>%
  tidyr::pivot_wider(names_from = FC.Horizon, values_from = Prob.Forecast) %>%
  arrange(DATE.FC.due) %>%
  dplyr::filter(DATE.FC.due >= as_date("1971-04-01")) %>%  # Before that, even many NAs in the avg 4Q ahead forecasts!
  na.omit()

# Triptych and plot
trpt_SPFavg_horizons <- triptych(SPFavg_wide %>%
                                   dplyr::select(-DATE.FC.due) %>%
                                   rename(y=gdp.first.recess))

summary(trpt_SPFavg_horizons)

ggsave(paste0("plots/Fig10_triptych_SPF_Consensus.pdf"),
       autoplot(trpt_SPFavg_horizons,
                RelDiag_breaks=seq(0,1,length.out=21),
                plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                plot_linetypes = "solid",
                plot_legend_title = "Forecast horizon"),
       width=24, height=10.5, units="cm")



################################################################################
# Part (B): Compare SPF average and SPF #65

# FCs in wide tibble format including the climatology
SPF_wide <- SPF_clean %>%
  tidyr::pivot_wider(names_from = ID, values_from = Prob.Forecast) %>%
  dplyr::filter(DATE.FC.due >= as_date("1971-04-01"))


# Filter data such that we only use Date.FC.due for which all horizon forecasts
# in h_set_plot are available!
h_set_plot <- c(1,2,4)

SPF_wide_complete_cases <- SPF_wide %>%
  dplyr::filter(FC.Horizon %in% h_set_plot) %>%
  na.omit() %>%
  arrange(DATE.FC.due) %>%
  group_by(DATE.FC.due) %>%
  mutate(n_horizons=n()) %>%
  dplyr::filter(n_horizons==length(h_set_plot)) %>%
  ungroup()


# Compare SPF average against individual forecasts
SPF_score_decomp <- tibble()
for (h in h_set_plot){
  df_SPF_trpt <- SPF_wide_complete_cases %>%
    dplyr::filter(FC.Horizon==h) %>%
    dplyr::rename(y=gdp.first.recess, "SPF Average"="0", "SPF #65"="65") %>%
    dplyr::select(-c("DATE.issued","DATE.FC.due", "FC.Horizon","n_horizons"))

  trpt_SPF <- triptych(df_SPF_trpt)
  SPF_score_decomp <- bind_rows(SPF_score_decomp,
                                summary(trpt_SPF$RelDiag) %>% mutate(h=h))

  ggsave(paste0("applications/plots/SPF_",h,"StepAhead.pdf"),
         autoplot(trpt_SPF,
                  Murphy_scoretype = "score",
                  plot_cols= gg_color_hue(3)[c(2,1,3)]),
         width=24, height=10.5, units="cm")
}

SPF_score_decomp




