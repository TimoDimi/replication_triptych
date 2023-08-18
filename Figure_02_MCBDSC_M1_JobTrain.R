library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


### Figure 2a) MCB-DSC Plot for the C1 Solar Flares

# Load and filter data
load(file = "data/C1_flares.rda")

colour_values <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")

MCBDSC_point_cols_C1 <- c(
  "ASSA" = colour_values[3],
  "CLIM120" = "black",
  "DAFFS" = "black",
  "DAFFS-G" = "black",
  "MCEVOL" = "black",
  "MCSTAT" = colour_values[4],
  "NICT" = "black",
  "NOAA" = colour_values[1],
  "SIDC" = colour_values[2]
)

p_MCBDSC_C1 <- mcbdsc(df_C1full) |>
  autoplot(
    colour_values = MCBDSC_point_cols_C1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.05),
    size_axislabels = 12) +
  ggtitle("(a) Class C1.0+ Solar Flares") +
  theme(plot.title = element_text(size = 14, hjust = 0))




### Figure 2b) MCB-DSC Plot for "Job Training" in the Fragile Family Challenge

load("data/FFC_JobTraining.rda")

mcbdsc_jobtrain <- mcbdsc(FFC.jobtrain.full)

# Set point colors
is_benchmark <- grepl("^benchmark", names(mcbdsc_jobtrain))
names(is_benchmark) <- names(mcbdsc_jobtrain)
MCBDSC_point_cols_JobTrain <- ifelse(is_benchmark, "#009E73", "black")


p_MCBDSC_jobtrain <- mcbdsc_jobtrain |>
  autoplot(
    colour_values = MCBDSC_point_cols_JobTrain,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.011),
    size_axislabels = 12) +
  ggtitle("(b) Job Training in Fragile Families Challenge") +
  theme(plot.title = element_text(size = 14, hjust = 0))


### Combine (a) and (b) and save jointly
ggsave(
  filename = "plots/Fig02_MCBDSC_Illustration.pdf",
  plot = p_MCBDSC_C1 + p_MCBDSC_jobtrain,
  width = 24, height = 12.5, units = "cm"
)




