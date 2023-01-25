source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")


# Load  data
load(file = "data/FFC_Eviction.rda")

# Omit some forecasts for better visibility of the forecasts chosen for Figure 12
trpt.evict.full <- FFC.evict.full %>%
  dplyr::select(-c("kouyang", "hamidrezaomidvar", "haixiaow", "amusse")) %>%
  triptych(confidence_level = NA)


# Set point colors
is_benchmark <- grepl("^benchmark", trpt.evict.full$FC_names)
names(is_benchmark) <- trpt.evict.full$FC_names
MCBDSC_point_cols_Evict <- ifelse(is_benchmark, gg_color_hue(5)[3], "black")
MCBDSC_point_cols_Evict[c("mdrc", "Justajwu", "bjgoode")] <- gg_color_hue(5)[c(1, 2, 5)]


# MCB-DSC plot with Brier score
MCBDSC_Evict <- ggplot2::autoplot(
  object = trpt.evict.full,
  plot_type = "MCBDSC",
  MCBDSC_MCB_xlim = c(0, 0.006),
  MCBDSC_point_cols = MCBDSC_point_cols_Evict
) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))

# MCB-DSC plot with log score
MCBDSC_LogScore_Evict <- autoplot(
  object = trpt.evict.full,
  plot_type = "MCBDSC",
  MCBDSC_score = "log_score",
  MCBDSC_UNC_hjust = 0.7,
  MCBDSC_MCB_xlim = c(0, 0.038),
  MCBDSC_point_cols = MCBDSC_point_cols_Evict
) +
  ggtitle("(b) Logarithmic Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))


# Save Plot
ggsave(
  filename = "plots/Fig11_FFC_Eviction_MCBDSC.pdf",
  plot = MCBDSC_Evict + MCBDSC_LogScore_Evict,
  width = 24, height = 12, units = "cm"
)
