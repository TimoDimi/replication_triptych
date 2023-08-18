library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)

# Load  data
load(file = "data/FFC_Eviction.rda")

# Omit some forecasts for better visibility of the forecasts chosen for Figure 11
FFC.evict.reduced <- FFC.evict.full %>%
  dplyr::select(-c("kouyang", "hamidrezaomidvar", "haixiaow", "amusse", "mb298", "amaatouq"))

trpt.evict.full  <- triptych(FFC.evict.reduced)

# Set point colors
is_benchmark <- grepl("^benchmark", trpt.evict.full$forecast)
names(is_benchmark) <- trpt.evict.full$forecast
MCBDSC_point_cols_Evict <- ifelse(is_benchmark, "#009E73", "black")
MCBDSC_point_cols_Evict[c("mdrc", "Justajwu", "bjgoode")] <- c("#E69F00", "#0072B2", "#CC79A7")


# MCB-DSC plot with Brier score
MCBDSC_Evict <- trpt.evict.full$mcbdsc |>
  autoplot(
    colour_values = MCBDSC_point_cols_Evict,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.006),
    size_axislabels = 12) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))


# MCB-DSC plot with log score
MCBDSC_LogScore_Evict <- mcbdsc(FFC.evict.reduced, score = "log_score") |>
  autoplot(
    MCBDSC_repel = FALSE,
    colour_values = MCBDSC_point_cols_Evict,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.038),
    DSC_lim = c(0, 0.038),
    size_axislabels = 12) +
  ggtitle("(b) Logarithmic Score") +
  theme(plot.title = element_text(size = 14, hjust = 0))


# Save Plot
ggsave(
  filename = "plots/Fig10_FFC_Eviction_MCBDSC.pdf",
  plot = MCBDSC_Evict + MCBDSC_LogScore_Evict,
  width = 24, height = 12, units = "cm"
)
