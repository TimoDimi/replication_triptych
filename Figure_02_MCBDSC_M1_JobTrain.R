library(tidyverse)
library(patchwork)

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")


### Figure 2a) MCB-DSC Plot for the C1 Solar Flares
load(file = "data/C1_flares.rda")
trpt_C1full <- triptych(df_C1full, confidence_level = NA)
summary(trpt_C1full)

# Plot colors
MCBDSC_point_cols <- c("ASSA"= gg_color_hue(4)[3],
                       "CLIM120"="black",
                       "DAFFS"= "black",
                       "DAFFS-G"="black",
                       "MCEVOL"="black",
                       "MCSTAT"=gg_color_hue(4)[4],
                       "NICT"="black",
                       "NOAA"=gg_color_hue(4)[2],
                       "SIDC"=gg_color_hue(4)[1])

# MCB DSC Plot for C1 flares
p_C1_MCBDSC <- autoplot(trpt_C1full,
                        plot_type="MCBDSC",
                        MCBDSC_point_cols=MCBDSC_point_cols,
                        MCBDSC_MCB_xlim = c(0,0.05),
                        size_axislabels=12) +
  ggtitle("(a) Class C1.0+ Solar Flares") +
  theme(plot.title = element_text(size = 14, hjust = 0))

p_C1_MCBDSC




### Figure 2b) MCB-DSC Plot for "Job Training" in the Fragile Family Challenge

load("data/FFC_JobTraining.rda")

trpt.jobtrain.full <- triptych(FFC.jobtrain.full, confidence_level = NA)

# Set point colors
MCBDSC_point_cols_JobTrain <- rep("black", length(trpt.jobtrain.full$FC_names))
names(MCBDSC_point_cols_JobTrain) <- trpt.jobtrain.full$FC_names
MCBDSC_point_cols_JobTrain[str_subset(names(MCBDSC_point_cols_JobTrain), pattern = "benchmark")] <- gg_color_hue(5)[3]

MCBDSC_JobTrain <- autoplot(trpt.jobtrain.full,
                            plot_type="MCBDSC",
                            MCBDSC_MCB_xlim=c(0,0.011),
                            MCBDSC_point_cols=MCBDSC_point_cols_JobTrain,
                            size_axislabels=12) +
  ggtitle("(b) Job Training in Fragile Families Challenge") +
  theme(plot.title = element_text(size = 14, hjust = 0))


### Combine (a) and (b) and save jointly
ggsave(paste0("plots/Fig02_MCBDSC_Illustration.pdf"),
       p_C1_MCBDSC + MCBDSC_JobTrain,
       width=24, height=12.5, units="cm")

