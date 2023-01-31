source("R/Imports.R")

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")

# Load Job Training data
load("data/FFC_JobTraining.rda")

trpt.jobtrain.full <- triptych(FFC.jobtrain.full, confidence_level = NA)


### Scatter Plot of AUC against Score and DSC
AUC_CORP_tbl <- trpt.jobtrain.full$auc %>%
  dplyr::filter(PAV == TRUE) %>%
  rename(AUC = auc) %>%
  arrange(desc(AUC)) %>%
  full_join(summary(trpt.jobtrain.full)) %>%
  filter(mean_score <= 0.195, miscalibration <= 0.01)


# Set point colors
is_benchmark <- grepl("^benchmark", trpt.jobtrain.full$FC_names)
names(is_benchmark) <- trpt.jobtrain.full$FC_names
MCBDSC_point_cols_JobTrain <- ifelse(is_benchmark, gg_color_hue(5)[3], "black")


# Scatter Plot of AUC against Score
p_AUC_score <- ggplot2::ggplot(AUC_CORP_tbl) +
  geom_point(aes(x = AUC, y = mean_score, colour = factor(forecast))) +
  scale_colour_manual(values=MCBDSC_point_cols_JobTrain) +
  coord_cartesian(ylim = c(NA, 0.1925), xlim = c(NA, 0.67)) +
  ylab("Mean Brier Score") +
  theme_bw() +
  ggtitle("(a) AUC vs. Brier score") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0))


# Scatter Plot of AUC against DSC
p_AUC_DSC <- ggplot(AUC_CORP_tbl) +
  geom_point(aes(x = AUC, y = discrimination, colour = factor(forecast))) +
  scale_colour_manual(values = MCBDSC_point_cols_JobTrain) +
  coord_cartesian(xlim = c(NA, 0.67)) +
  ylab("DSC") +
  theme_bw() +
  ggtitle("(b) AUC vs. DSC") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0))

# Save plots jointly
ggsave(
  filename = paste0("plots/Fig06_AUC_Scatter.pdf"),
  plot = p_AUC_score + p_AUC_DSC,
  width = 24, height = 13, units="cm"
)
