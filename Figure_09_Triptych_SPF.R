library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)
library(tidyr)

set.seed(20231005) # reproducible resampling in triptych::add_consistency()

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
