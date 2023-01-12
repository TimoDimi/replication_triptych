library(lubridate)
library(patchwork)

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

# Load an filter data
load(file = "data/M1_flares.rda")

M1_FCnames_short <- c("NICT", "NOAA", "ASSA", "MCSTAT")

# Short data frame for a triptych with 4 forecasts
df_M1_short <- df_M1full %>%
  select(all_of(c("y",M1_FCnames_short)))

# Assign plot colors
plot_cols_M1 <- c("NICT"=gg_color_hue(6)[2],
                  "NOAA"=gg_color_hue(4)[2],
                  "ASSA"=gg_color_hue(6)[5],
                  "MCSTAT"=gg_color_hue(4)[4])

# Figure 9: Triptych for the M1 forecasts
trpt_M1 <- triptych(df_M1_short)
p_M1 <- autoplot(trpt_M1,
                 RelDiag_breaks=seq(0,1,length.out=21),
                 plot_linetypes="solid",
                 plot_cols = plot_cols_M1,
                 size_axislabels=12)
p_M1

ggsave(paste0("plots/Fig09_triptych_M1Flares.pdf"),
       p_M1,
       width=24, height=10.5, units="cm")
