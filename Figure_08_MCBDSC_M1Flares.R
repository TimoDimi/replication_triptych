library(lubridate)
library(patchwork)
library(ggrepel)

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

# Load an filter data
load(file = "data/M1_flares.rda")

# MCB-DSC Plot of all M1 forecasts
trpt_M1full <- triptych(df_M1full, confidence_level = NA)


# Assign plot colors
plot_cols_M1 <- c("NICT"=gg_color_hue(6)[2],
                  "NOAA"=gg_color_hue(4)[2],
                  "ASSA"=gg_color_hue(6)[5],
                  "MCSTAT"=gg_color_hue(4)[4])

MCBDSC_point_cols_M1 <- c("AMOS"="black",
                          "ASAP"="black",
                          plot_cols_M1[3],    # ASSA
                          "BOM"="black",
                          "CLIM120"="black",
                          "DAFFS"= "black",
                          "DAFFS-G"="black",
                          "MAG4VW"="black",
                          "MAG4VWF"= "black",
                          "MAG4W"= "black",
                          "MAG4WF"="black",
                          "MCEVOL"="black",
                          plot_cols_M1[4],    #MCSTAT
                          "MOSWOC"="black",
                          plot_cols_M1[1],   #NICT
                          plot_cols_M1[2],    #NOAA
                          "SIDC"="black")


# Figure 8: Brier and Log Score MCB-DSC Plots.
MCBDSC_M1_BrierScore <- autoplot(trpt_M1full,
                                 plot_type="MCBDSC",
                                 MCBDSC_score=brier_score,
                                 MCBDSC_point_cols=MCBDSC_point_cols_M1,
                                 MCBDSC_repel=TRUE,
                                 MCBDSC_MCB_xlim=c(0,0.0485)) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))


MCBDSC_M1_LogScore <- autoplot(trpt_M1full,
                               plot_type="MCBDSC",
                               MCBDSC_score=log_score,
                               MCBDSC_point_cols=MCBDSC_point_cols_M1,
                               MCBDSC_repel=TRUE,
                               MCBDSC_MCB_xlim=c(0,0.2)) +
  annotate("text", x=0.2, y=0.0405, label="MAG4VW", size=3, hjust=1) +
  ggtitle("(b) Logarithmic Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))


MCBDSC_M1_BrierLogScores <- MCBDSC_M1_BrierScore + MCBDSC_M1_LogScore

ggsave(paste0("plots/Fig08_M1Flares_MCBDSC.pdf"),
       MCBDSC_M1_BrierLogScores,
       width=24, height=12, units="cm")

