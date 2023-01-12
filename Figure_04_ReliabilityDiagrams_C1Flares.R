library(tidyverse)
library(patchwork)
library(calibrationband)

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



#      Illustration Reliability Diagrams     ########
p_RelDiag_RunExmpl <- df_RunExmpl %>%
  triptych() %>%
  autoplot(plot_type="ReliabilityDiagram",
           plot_linetypes="solid",
           RelDiag_breaks=seq(0,1,length.out=11),
           plot_cols = gg_color_hue(4)[c(2,1,3,4)]) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position="none") +
  ggtitle("")

p_RelDiag_RunExmpl


# Add calibration bands to the reliability diagrams!
Cal_Bands_tbl <- tibble()
for (FC_name in C1_FC_names){
  Cal_Bands_tbl <- bind_rows(Cal_Bands_tbl,
                             calibration_bands(x=df_RunExmpl[[FC_name]], y=df_RunExmpl$y, nc=TRUE, method="round", digits=2, alpha=0.1)$bands %>%
                               mutate(forecast=FC_name))
}

RelDiag_hlp <- df_RunExmpl %>%
  triptych() %>%
  .$RelDiag

### Histograms with the same area
r_cases <- dplyr::bind_rows(lapply(RelDiag_hlp, function(l) l$cases), .id = "forecast")

# Manual histogram computations to set a good default
RelDiag_breaks <- seq(0,1,length.out=11)
RelDiag_hist_factor <- 0.2

hist_manual <- r_cases %>%
  group_by(forecast) %>%
  summarize(breaks = tail(RelDiag_breaks,-1),
            bin_height = cut(x, breaks=RelDiag_breaks, include.lowest=TRUE) %>% table() %>% as.numeric() / (n()* diff(RelDiag_breaks)))


p_RelDiag_hlp <- autoplot(RelDiag_hlp, params_diagonal = NA, params_CEPline = NA) +
  geom_histogram(aes(x, y = RelDiag_hist_factor/max(hist_manual$bin_height)*after_stat(density)),
                 data = r_cases,
                 breaks=RelDiag_breaks,
                 colour = "black",
                 fill = NA) +
  autolayer(
    object = RelDiag_hlp,
    params_diagonal = NULL,
    params_CEPline=list(size = 0.5)) +
  facet_wrap(.~factor(forecast, levels=names(RelDiag_hlp)), nrow=2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())


# Add calibration bands
p_RelDiag_CalBands <- p_RelDiag_hlp +
  geom_ribbon(data=Cal_Bands_tbl,
              aes(x=x, ymin=lwr, ymax=upr),
              fill = "grey50", alpha = 0.3) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  ggplot2::scale_color_manual(values = gg_color_hue(4)[c(2,1,3,4)], limits=C1_FC_names) +
  ggplot2::scale_linetype_manual(values = rep("solid",4), limits=C1_FC_names) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position="none",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))


# Add together with patchwork and save
ggsave(paste0("plots/Fig04_ReliabilityDiagrams_C1Flares.pdf"),
       (p_RelDiag_RunExmpl + ggtitle("(a) Calibration Bands") + theme(plot.title = element_text(size = 14, hjust = 0))) +
         (p_RelDiag_CalBands + ggtitle("(b) Universally Valid Confidence Bands") + theme(plot.title = element_text(size = 14, hjust = 0))),
       width=24, height=14, units="cm")

