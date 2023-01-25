source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

set.seed(20230123)

# Load  data
load(file = "data/FFC_Eviction.rda")

# Select forecasts
FFC.evict.short <- FFC.evict.full %>%
  dplyr::select(c("y", "mdrc", "Justajwu", "bjgoode", "benchmark_logit_full"))

# Triptych
trpt.evict <- triptych(FFC.evict.short)


# Plot
p.trpt.evict <- ggplot2::autoplot(
  object = trpt.evict,
  RelDiag_breaks = seq(0, 1, length.out = 31),
  Murphy_scoretype = "score",
  plot_cols = gg_color_hue(5)[c(1, 2, 5, 3)],
  Murphy_RelDiag_range = c(0, 0.4)
)


# Save file
ggsave(
  filename = "plots/Fig12_Triptych_FFC_Eviction.pdf",
  plot = p.trpt.evict,
  width = 24, height = 10.5, units = "cm"
)
