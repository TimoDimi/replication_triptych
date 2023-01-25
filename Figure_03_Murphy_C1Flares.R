source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")


# Load an filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", all_of(C1_FC_names)))

# Run a triptych
trpt_RunExmpl <- triptych(df_RunExmpl, confidence_level = NA)

# Entire Murphy diagram
p_Murphy_all <- ggplot2::autoplot(
  object = trpt_RunExmpl,
  plot_type = "Murphy",
  Murphy_scoretype = "score",
  plot_cols = gg_color_hue(4)[c(2, 1, 3, 4)],
  plot_linetypes = c("solid"),
  size_axislabels = 12
) +
  ggtitle("(b) Murphy Curves") +
  theme(plot.title = element_text(size = 14, hjust = 0))


# Dominating Murphy diagram with all FCs in legend (important for patchwork legend)
trpt_RunExmpl_dom2 <- df_RunExmpl %>%
  mutate(
    MCSTAT = ifelse(y == 0, 1, 0),
    ASSA = ifelse(y == 0, 1, 0)
  ) %>%
  triptych()

p_Murphy_dom2 <- autoplot(
  object = trpt_RunExmpl_dom2,
  plot_type = "Murphy",
  Murphy_scoretype = "score",
  Murphy_benchmark = "NOAA",
  plot_cols = gg_color_hue(4)[c(2, 1, 3, 4)],
  plot_linetypes = c("solid")
) +
  ylab("Mean elem. score difference") +
  ggtitle("(a) SIDC - NOAA") +
  ylim(c(-0.01, 0.1)) +
  theme(plot.title = element_text(size = 14, hjust = 0))

p_Murphy_dom2



# Non-dominating Murphy diagram with all FCs in legend
trpt_RunExmpl_nodom2 <- df_RunExmpl %>%
  mutate(
    NOAA = ifelse(y == 0, 1, 0),
    SIDC = ifelse(y == 0, 1, 0)
  ) %>%
  triptych()

p_Murphy_nodom2 <- autoplot(
  object = trpt_RunExmpl_nodom2,
  plot_type = "Murphy",
  Murphy_scoretype = "score",
  Murphy_benchmark = "MCSTAT",
  plot_cols = gg_color_hue(4)[c(2, 1, 3, 4)],
  plot_linetypes = c("solid")
) +
  ylab("Mean elem. score difference") +
  ggtitle("(c) ASSA - MCSTAT") +
  ylim(c(-0.06, 0.06)) +
  theme(plot.title = element_text(size = 14, hjust = 0))

p_Murphy_nodom2


# Combine all three Murphy diagrams with patchwork
p_Murphy_combined <-
  (p_Murphy_dom2 + theme(legend.position = "none")) +
    p_Murphy_all +
    (p_Murphy_nodom2 + theme(legend.position = "none")) +
    patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

p_Murphy_combined

ggsave(
  filename = "plots/Fig03_MurphyIllustration_C1Flares.pdf",
  plot = p_Murphy_combined,
  width = 24, height = 10.5, units = "cm"
)
