library(triptych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


# Load and filter data
load(file = "data/C1_flares.rda")

# Subset of forecasts for the C1 flares running example
C1_FC_names <- c("NOAA", "SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full |>
  select(c("y", all_of(C1_FC_names)))

colour_values <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")
names(colour_values) <- C1_FC_names



# Figure 4 (a)
ROC_raw <- triptych::roc(df_RunExmpl, concave = FALSE)
AUC_raw <- purrr::map(ROC_raw, \(o) o$estimate$auc) |> unlist()
p_ROC_a <- ROC_raw |> autoplot() &
  scale_colour_manual(
    values = colour_values,
    guide = guide_legend(title = "Forecast"))


# Figure 4 (c)
ROC_PAV <- triptych::roc(df_RunExmpl, concave = TRUE)
AUC_PAV <- purrr::map(ROC_PAV, \(o) o$estimate$auc) |> unlist()
p_ROC_c <- ROC_PAV |> autoplot() &
  scale_colour_manual(
    values = colour_values,
    guide = guide_legend(title = "Forecast"))


# Annotate the ROC curve diagrams with AUC values
annotate_auc <- function(auc_values) {
  x_annot <- 0.625
  y_annot <- 0.45
  dodge <- y_annot / 5
  annot_size <- 10 / .pt

  annotate(
    geom = "text",
    x = x_annot,
    y = y_annot - (0:4) * dodge,
    label = c("AUC:", sprintf("%0.3f", auc_values)),
    color = c("black", colour_values),
    size = annot_size,
    fontface = c(1, 1, 1, 1, 1),
    hjust = 0
  )
}

p_ROC_a_annot <- p_ROC_a + annotate_auc(AUC_raw)
p_ROC_c_annot <- p_ROC_c + annotate_auc(AUC_PAV)



# Zoomed version of the comparison

# Options to copy&paste the plotting functions form the triptych autoplot function
plot_cols <- colour_values
plot_linewidth <- 0.5
size_legend <- 10
size_axislabels <- 11
size_axisticks <- 9

# Generate a data frame for plotting
df_plot <- bind_rows(estimates(ROC_raw) %>%
                           dplyr::filter(forecast %in% c("NOAA", "MCSTAT")) %>%
                           mutate(sensitivities = HR,
                                  specificities = 1-FAR,
                                  PAV = FALSE),
                         estimates(ROC_PAV) %>%
                           dplyr::filter(forecast %in% c("NOAA", "MCSTAT")) %>%
                           mutate(sensitivities = HR,
                                  specificities = 1-FAR,
                                  PAV = TRUE)
) %>%
  select(forecast, specificities, sensitivities, PAV) %>%
  arrange(forecast, desc(specificities), sensitivities) %>%
  group_by(forecast) %>%
  mutate(sensitivities_PAV_interpol = approx(
    x = 1 - specificities[PAV],
    y = sensitivities[PAV],
    xout = 1 - specificities,
    ties = "ordered"
  )$y)






# Difference between raw and PAV-recalibrated ROC curve
p_ROC <- ggplot2::ggplot() +
  geom_segment(
    mapping = aes(x = 0, y = 0, xend = 1, yend = 1),
    linewidth = plot_linewidth / 3,
    colour = "black"
  ) +
  geom_ribbon(
    data = df_plot,
    mapping = aes(
      x = 1 - specificities,
      ymin = sensitivities,
      ymax = sensitivities_PAV_interpol,
      fill = forecast,
      colour = forecast
    )
  ) +
  geom_rect(
    data = tibble(x = c(0.05, 0.3), y = c(0.45, 0.7)),
    mapping = aes(xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]),
    fill = "grey70",
    alpha = 0.2,
    linewidth = 0.1,
    color = "black"
  ) +
  geom_path(
    data = df_plot %>% filter(PAV == TRUE),
    mapping = aes(x = 1 - specificities, y = sensitivities),
    linewidth = 0.2,
    col = "black"
  ) +
  geom_path(
    data = df_plot,
    mapping = aes(x = 1 - specificities, y = sensitivities),
    linewidth = 0.2,
    col = "black"
  ) +
  scale_fill_manual(values = plot_cols) +
  scale_colour_manual(values = plot_cols) +
  xlab("False alarm rate") +
  ylab("Hit rate") +
  ggtitle("ROC Curve") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = size_axislabels),
    axis.text = element_text(size = size_axisticks),
    legend.position = "none",
    aspect.ratio = 1
  ) +
  ggtitle("")


# Zoomed inset plot
p_ROC_inset <- p_ROC +
  coord_cartesian(xlim = c(0.05, 0.3), ylim = c(0.45, 0.7), expand = FALSE) +
  scale_x_continuous(breaks = c(0.1, 0.2), position = "top") +
  scale_y_continuous(breaks = c(0.5, 0.6), position = "left") +
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    plot.background = element_rect(fill = "transparent", colour = "transparent")
  )


# Add the zoomed inset plot
p_ROC_annot <- p_ROC +
  annotation_custom(
    grob = ggplotGrob(p_ROC_inset),
    xmin = 0.33, xmax = 1.09,
    ymin = 0, ymax = 0.66
  )

# Combine all three ROC plots with patchwork
p.theme <- theme(plot.title = element_text(size = 14, hjust = 0))
p_ROC_combined <-
  (p_ROC_a_annot + ggtitle("(a) Original ROC Curve") + p.theme) +
  (p_ROC_annot + ggtitle("(b) Comparison") + p.theme) +
  (p_ROC_c_annot + ggtitle("(c) Concave ROC Curve") + p.theme) +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "none")

p_ROC_combined


# Save Joint ROC Curve (smaller height due to missing legend)
ggsave(
  filename = "plots/Fig04_ROC_Illustration_C1Flares.pdf",
  plot = p_ROC_combined,
  width = 24, height = 9, units = "cm"
)


