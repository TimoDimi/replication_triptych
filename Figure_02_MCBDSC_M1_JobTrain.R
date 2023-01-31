source("R/Imports.R")

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
MCBDSC_point_cols <- c(
  "ASSA" = gg_color_hue(4)[3],
  "CLIM120" = "black",
  "DAFFS" = "black",
  "DAFFS-G" = "black",
  "MCEVOL" = "black",
  "MCSTAT" = gg_color_hue(4)[4],
  "NICT" = "black",
  "NOAA" = gg_color_hue(4)[2],
  "SIDC" = gg_color_hue(4)[1]
)

# MCB DSC Plot for C1 flares
p_C1_MCBDSC <- ggplot2::autoplot(
  object = trpt_C1full,
  plot_type = "MCBDSC",
  MCBDSC_point_cols = MCBDSC_point_cols,
  MCBDSC_MCB_xlim = c(0, 0.05),
  size_axislabels = 12
) +
  ggtitle("(a) Class C1.0+ Solar Flares") +
  theme(plot.title = element_text(size = 14, hjust = 0))

p_C1_MCBDSC




### Figure 2b) MCB-DSC Plot for "Job Training" in the Fragile Family Challenge

load("data/FFC_JobTraining.rda")

trpt.jobtrain.full <- triptych(FFC.jobtrain.full, confidence_level = NA)

# Set point colors
is_benchmark <- grepl("^benchmark", trpt.jobtrain.full$FC_names)
names(is_benchmark) <- trpt.jobtrain.full$FC_names
MCBDSC_point_cols_JobTrain <- ifelse(is_benchmark, gg_color_hue(5)[3], "black")

MCBDSC_JobTrain <- autoplot(
  object = trpt.jobtrain.full,
  plot_type = "MCBDSC",
  MCBDSC_MCB_xlim = c(0, 0.011),
  MCBDSC_point_cols = MCBDSC_point_cols_JobTrain,
  size_axislabels = 12
) +
  ggtitle("(b) Job Training in Fragile Families Challenge") +
  theme(plot.title = element_text(size = 14, hjust = 0))


### Combine (a) and (b) and save jointly
ggsave(
  filename = "plots/Fig02_MCBDSC_Illustration.pdf",
  plot = p_C1_MCBDSC + MCBDSC_JobTrain,
  width = 24, height = 12.5, units = "cm"
)
