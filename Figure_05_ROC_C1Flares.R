library(tidyverse)
library(patchwork)

source("R/triptych.R")
source("R/plot.R")
source("R/print.R")
source("R/summary.R")
source("R/utils.R")


# Load an filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA","SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", C1_FC_names))


#     Illustration ROC Curves     ##############################################
ROC_RunExmlp_Raw <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
                             plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                             PAV_ROC="FALSE",
                             size_legend=12,
                             size_axislabels=12,
                             size_axisticks=12) +
  ggtitle("(a) Raw ROC Curve")


# Annotate the ROC curve with AUC values
x_annot <- 0.625
y_annot <- 0.45
dodge <- y_annot/5
annot_size <- 10/.pt

ROC_RunExmlp_Raw_annot <- ROC_RunExmlp_Raw +
  annotate(geom = "text",
           x = x_annot,
           y = y_annot - (0:4) * dodge,
           label = c("AUC:", trpt_RunExmpl$auc %>% filter(PAV==FALSE) %>% mutate(label=paste(sprintf("%0.3f", auc))) %>% pull(label)),
           color = c("black",gg_color_hue(4)[c(2,1,3,4)]),
           size=annot_size,
           fontface = c(1,1,1,1,1),
           hjust = 0)


# ROC Curve of PAV-recalibrated forecasts
ROC_RunExmlp_PAV <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
                             plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                             size_legend=12,
                             size_axislabels=12,
                             size_axisticks=12) +
  ggtitle("(b) PAV-Recalibrated ROC Curve")


# Annotate the PAV-recalibrated ROC curve with AUC values
x_annot <- 0.625
y_annot <- 0.45
dodge <- y_annot/5
annot_size <- 10/.pt

ROC_RunExmlp_PAV_annot <- ROC_RunExmlp_PAV +
  annotate(geom = "text",
           x = x_annot,
           y = y_annot - (0:4) * dodge,
           label = c("AUC:", trpt_RunExmpl$auc %>% filter(PAV==TRUE) %>% mutate(label=paste(sprintf("%0.3f", auc))) %>% pull(label)),
           color = c("black",gg_color_hue(4)[c(2,1,3,4)]),
           size=annot_size,
           fontface = c(1,1,1,1,1),
           hjust = 0)



# Zoomed version of the comparison

# Options to copy&paste the plotting functions form the triptych autoplot function
plot_cols=gg_color_hue(4)[c(2,1,3,4)]
names(plot_cols) <- C1_FC_names
plot_linetypes="solid"
plot_linewidth=0.5
plot_margins=grid::unit(c(0,0.02,0,0.02), "npc")
plot_theme=ggplot2::theme_bw()
plot_legend_title="forecast"
size_title=14
size_legend=12
size_axislabels=12
size_axisticks=10

df_plot <- trpt_RunExmpl$roc %>%
  dplyr::filter(forecast %in% c("NOAA", "MCSTAT"))

df_plot_join <- full_join(df_plot %>% filter(PAV==T) %>% select(-c(PAV)),
                          df_plot %>% filter(PAV==F) %>% select(-c(PAV)),
                          by=c("forecast", "specificities"),
                          suffix = c("_PAV", "_raw")) %>%
  arrange(forecast, desc(specificities), sensitivities_raw) %>%
  group_by(forecast) %>%
  mutate(sensitivities_PAV_interpol = approx(x=1-specificities, y=sensitivities_PAV, xout=1-specificities, ties="ordered")$y)


# Difference between raw and PAV-recalibrated ROC curve
p_ROC <- ggplot2::ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               linewidth=plot_linewidth/3, colour="black") +
  geom_ribbon(data=df_plot_join,
              aes(x=1-specificities, ymin=sensitivities_raw, ymax=sensitivities_PAV_interpol, fill=forecast, colour=forecast)) +
  geom_path(data = df_plot %>% filter(PAV==TRUE),
            aes(x=1-specificities, y=sensitivities, col=forecast),
            linewidth=0.2, linetype="solid", col="black") +
  geom_path(data = df_plot %>% filter(PAV==FALSE),
            aes(x=1-specificities, y=sensitivities, col=forecast),
            linewidth=0.2, linetype="solid", col="black") +
  ggplot2::scale_fill_manual(values = gg_color_hue(4)[c(2,4)]) +
  ggplot2::scale_colour_manual(values = gg_color_hue(4)[c(2,4)]) +
  xlab("FAR") +
  ylab("HR") +
  ggtitle("ROC Curve") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size=size_legend),
        legend.text = element_text(size=size_legend),
        plot.title = element_text(hjust = 0.5, size = size_title),
        axis.title = element_text(size=size_axislabels),
        axis.text.x = element_text(size=size_axisticks),
        axis.text.y = element_text(size=size_axisticks),
        legend.position="bottom",
        legend.key.size = grid::unit(2, "lines"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(paste(plot_legend_title), nrow = 1),
         linetype = guide_legend(paste(plot_legend_title), nrow = 1)) +
  ggtitle("")


# Zoomed inset plot
p_ROC_inset <- p_ROC + coord_cartesian(xlim = c(0.05,0.3),
                                       ylim = c(0.45,0.7),
                                       expand = FALSE) +
  scale_x_continuous(breaks = c(0.1, 0.2), position = "top") +
  scale_y_continuous(breaks = c(0.5, 0.6),
                     position = "left") +
  theme(legend.position="none",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "grey90"))


# Add the zoomed inset plot
p_ROC_annot <- p_ROC +
  annotation_custom(ggplotGrob(p_ROC_inset),
                    xmin = 0.33, xmax = 1.09,
                    ymin = 0, ymax = 0.66) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               colour="black") +
  geom_rect(data = tibble(x=c(0.05,0.3), y=c(0.45,0.7)),
            aes(xmin = x[1], xmax = x[2],
                ymin = y[1], ymax = y[2]),
            fill = "grey70", alpha=0.2, color = "black", linewidth=0.1)


# Combine all three ROC plots with patchwork
p_ROC_combined <- (ROC_RunExmlp_Raw_annot + ggtitle("(a) Original ROC Curve") + theme(plot.title = element_text(size = 14, hjust = 0))) +
  (p_ROC_annot + ggtitle("(b) Comparison") + theme(legend.position="none") + theme(plot.title = element_text(size = 14, hjust = 0))) +
  (ROC_RunExmlp_PAV_annot + ggtitle("(c) Concave ROC Curve") + theme(plot.title = element_text(size = 14, hjust = 0))) +
  plot_layout(guides = "collect") & theme(legend.position = 'none')

p_ROC_combined


# Save Joint ROC Curve (smaller height due to missing legend)
ggsave(paste0("plots/Fig05_ROC_Illustration_C1Flares.pdf"),
       p_ROC_combined,
       width=24, height=9, units="cm")


