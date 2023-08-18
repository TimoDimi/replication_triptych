library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load  data
load(file = "data/FFC_Eviction.rda")

# Select forecasts
FFC.evict.short <- FFC.evict.full %>%
  dplyr::select(c("y", "mdrc", "Justajwu", "bjgoode", "benchmark_logit_full"))

# Assign plot colors
plot_cols_evict <- c(
  "mdrc" = "#E69F00",
  "Justajwu" = "#0072B2",
  "bjgoode" = "#CC79A7",
  "benchmark_logit_full" = "#009E73"
)

# Triptych
trpt.evict <- triptych(FFC.evict.short) |>
  add_consistency()

# Plot
p.trpt.evict <- ggplot2::autoplot(
  object = trpt.evict,
  breaks = seq(0, 1, length.out = 31)
) &
  scale_colour_manual(
    values = plot_cols_evict,
    guide = guide_legend(title = "Forecast"))

# Rename the x-axis in the Murphy diagram
shrink_Murphy_axis <- function(p_patchwork, xlimits) {
  p_patchwork[[1]] <- p_patchwork[[1]] +
    xlab(expression("Threshold " * theta)) +
    coord_cartesian(xlim=xlimits)

  p_patchwork[[2]] <- p_patchwork[[2]] +
    coord_cartesian(xlim=xlimits)

  p_patchwork
}
p.trpt.evict <- shrink_Murphy_axis(p_patchwork=p.trpt.evict, xlimits=c(0,0.4))



# Save file
ggsave(
  filename = "plots/Fig11_Triptych_FFC_Eviction.pdf",
  plot = p.trpt.evict,
  width = 24, height = 10.5, units = "cm"
)
