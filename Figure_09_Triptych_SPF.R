library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)
library(tidyr)

# Load
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
# Figure 9: Compare the SPF average on different forecast horizons

h_set_avg <- c(0, 1, 2, 4)

# Colors
colour_values <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")
names(colour_values) <- c("0", "1", "2", "4")


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
  triptych() |>
  add_consistency()

fig09 <- autoplot(trpt_SPFavg_horizons) &
  scale_colour_manual(
    values = colour_values,
    guide = guide_legend(title = "Forecast"))

# Rename the x-axis in the Murphy diagram
rename_Murphy_axis <- function(p_patchwork) {
  p_patchwork[[1]] <- p_patchwork[[1]] + xlab(expression("Threshold " * theta))
  p_patchwork
}
fig09 <- rename_Murphy_axis(fig09)


ggsave(
  filename = paste0("plots/Fig09_triptych_SPF_Consensus.pdf"),
  plot = fig09,
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

  mcbdsc_SPF <- mcbdsc(df_SPF_trpt) |>
    estimates() |>
    mutate(across(mean_score:UNC, \(x) round(x, digits = 3)))

  SPF_score_decomp <- bind_rows(
    SPF_score_decomp,
    mcbdsc_SPF %>% mutate(h = h)
  )
}

# Table 3:
SPF_score_decomp

