library(tidyverse)
library(triptych)
library(patchwork)


##### OLD
# FFC.pred.full <- readRDS("applications/data/FFC_predictions.rds")
# FC_names <- c("mdrc", "Katy_P", "Tamkinat", "the_Brit")
# df_RunExmpl <- FFC.pred.full %>%
#   filter(outcome=="jobTraining" & account %in% FC_names) %>%
#   select(challengeID, account, truth, prediction) %>%
#   mutate(prediction = pmax(0, pmin(1,prediction))) %>%
#   dcast(challengeID + truth ~ account) %>%
#   tibble::as_tibble() %>%
#   select(-challengeID) %>%
#   rename(y=truth)  %>%
#   na.omit()

#####
# Interesting findings for "NOAA", "GDAFFS","SIDC","MCSTAT"
#  - SIDC and MCSTAT have equal discrimination, but MCSTAT miscalibrated., hence different Murphy
#  - Murphy diagram is interesting for GDAFFS, SIDC and MCSTAT in the regions smaller and larger 0.5
#  - GDAFFS forecasts too small probabilities, MCSTAT rather too large ones
#  - Using ASSA is interesting as it has an Inf log score!



#   C1 SOLAR FLARE FORECASTS ###################################################
load("applications/data/SF.FC.C1.rda")

C1_FCnames_all <- c("ASSA", "CLIM120", "DAFFS", "GDAFFS", "MCEVOL", "MCSTAT", "NICT", "NOAA","SIDC")

# Filter the data for dates where ALL employed forecasts are available
df_C1full <- SF.FC.C1 %>%
  dplyr::select(c("rlz.C1",C1_FCnames_all)) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

dim(df_C1full)



C1_FC_names <- c("NOAA","SIDC", "ASSA", "MCSTAT")

df_RunExmpl <- df_C1full %>%
  dplyr::select(c("y", C1_FC_names))


# Triptych
trpt_RunExmpl <- triptych(df_RunExmpl)
p_RunExmpl <- autoplot(trpt_RunExmpl,
                       Murphy_scoretype = "score",
                       plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                       plot_linetypes="solid",
                       size_axislabels=12)

p_RunExmpl

ggsave(paste0("applications/plots/triptych_RunExmpl.pdf"),
       p_RunExmpl,
       width=24, height=10.5, units="cm")


# Brier, Log and Beta Scores
MR_score <- function(y,x){as.numeric(x<0.5 & y==1) + as.numeric(x>0.5 & y==0) + 0.5*as.numeric(x==0.5)}


# Table 1
df_RunExmpl %>%
  reshape2::melt(id="y") %>%
  group_by(variable) %>%
  summarize(Brier_score = mean((y-value)^2),
            log_score = mean(log_score(y, value)),
            MR_score = mean(MR_score(y, value)),
            Beta_score24 = mean(Beta_score(y, value, alpha=2, beta=4)),
            Beta_score42 = mean(Beta_score(y, value, alpha=4, beta=2))) %>%
  arrange(Brier_score)



#### Alternative running example triptych. Not used anymore!
# df_RunExmpl2 <- SF.FC.C1 %>%
#   dplyr::select(c("rlz.C1", "CLIM120", "DAFFS", "MCEVOL", "NJIT")) %>%
#   as_tibble() %>%
#   mutate_all(funs(replace(., .<0, NA))) %>%
#   na.omit() %>%
#   rename(y=rlz.C1)
#
# # Triptych
# trpt_RunExmpl2 <- triptych(df_RunExmpl2)
# p_RunExmpl2 <- autoplot(trpt_RunExmpl2,
#                        plot_type="triptych",
#                        Murphy_scoretype = "score",
#                        # plot_cols = gg_color_hue(4)[c(2,1,3,4)],
#                        plot_linetypes="solid",
#                        size_axislabels=12)
#
# p_RunExmpl2
#
# ggsave(paste0("applications/plots/triptych_RunExmpl2.pdf"),
#        p_RunExmpl2,
#        width=24, height=10.5, units="cm")



#   Illustration Murphy Diagrams   ##########################################################################

# Dominating Murphy diagram
trpt_RunExmpl_dom <- triptych(df_RunExmpl %>% dplyr::select("y", "NOAA", "SIDC"))
p_Murphy_dom <- autoplot(trpt_RunExmpl_dom,
                       plot_type="Murphy",
                       Murphy_scoretype = "score",
                       plot_cols=gg_color_hue(4)[c(2,1)],
                       plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(0,0.32))

p_Murphy_dom

# Non-dominating Murphy diagram
trpt_RunExmpl_nodom <- triptych(df_RunExmpl %>% dplyr::select("y", "ASSA", "MCSTAT"))
p_Murphy_nodom <- autoplot(trpt_RunExmpl_nodom,
                       plot_type="Murphy",
                       Murphy_scoretype = "score",
                       plot_cols=gg_color_hue(4)[c(3,4)],
                       plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(0,0.32))

p_Murphy_nodom

ggsave(paste0("applications/plots/MurphyDominance.pdf"),
       p_Murphy_dom,
       width=11, height=12, units="cm")

ggsave(paste0("applications/plots/MurphyNonDominance.pdf"),
       p_Murphy_nodom,
       width=11, height=12, units="cm")



# Hacky way to get facets into the Murphy diagram!
trpt_RunExmpl$Murphy <- trpt_RunExmpl$Murphy %>%
  mutate(facet_class = ifelse(forecast %in% c("NOAA", "SIDC"), "(a) Forecast Dominance\n", "(b) No Forecast Dominance\n"))

p_Murphy_joint <- autoplot(trpt_RunExmpl,
                           plot_type="Murphy",
                           Murphy_scoretype = "score",
                           plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                           size_legend=14,
                           size_axislabels=14,
                           size_axisticks=14) +
  facet_wrap(.~facet_class) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 16)) +
  ggtitle("") +
  theme(panel.spacing = unit(2, "lines"))

p_Murphy_joint


ggsave(paste0("applications/plots/MurphyJoint.pdf"),
       p_Murphy_joint,
       width=22, height=14, units="cm")




###### 3-way joint reliability diagram

# Entire Murphy diagram
p_Murphy_all <- autoplot(trpt_RunExmpl,
                         plot_type="Murphy",
                         Murphy_scoretype = "score",
                         Murphy_benchmark = "NOAA",
                         plot_cols=gg_color_hue(4)[c(2,1,3,4)],
                         plot_linetypes = c("solid")) +
  ylab("Mean elem. score difference to NOAA") +
  ggtitle("")



# Second variant of dominating Murphy diagram with all FCs in legend
trpt_RunExmpl_dom2 <- triptych(df_RunExmpl %>% dplyr::mutate(MCSTAT=ifelse(y==0, 1, 0),
                                                             ASSA=ifelse(y==0, 1, 0)))

p_Murphy_dom2 <- autoplot(trpt_RunExmpl_dom2,
                          plot_type="Murphy",
                          Murphy_scoretype = "score",
                          plot_cols=gg_color_hue(4)[c(2,1,3,4)],
                          plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(0,0.32))

p_Murphy_dom2

# Second variant of non-dominating Murphy diagram with all FCs in legend
trpt_RunExmpl_nodom2 <- triptych(df_RunExmpl %>% dplyr::mutate(NOAA=ifelse(y==0, 1, 0),
                                                               SIDC=ifelse(y==0, 1, 0)))

p_Murphy_nodom2 <- autoplot(trpt_RunExmpl_nodom2,
                            plot_type="Murphy",
                            Murphy_scoretype = "score",
                            plot_cols=gg_color_hue(4)[c(2,1,3,4)],
                            plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(0,0.32))

p_Murphy_nodom2


# All three Murphy diagrams together
p_Murphy_combined <- (p_Murphy_dom2 + theme(legend.position="none")) + p_Murphy_all + (p_Murphy_nodom2 + theme(legend.position="none")) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(paste0("applications/plots/Murphy_3combined.pdf"),
       p_Murphy_combined,
       width=24, height=10.5, units="cm")






#      Illustration Reliability Diagrams     ##########################################################################
library(calibrationband)

p_RelDiag_RunExmpl <- df_RunExmpl %>%
  triptych() %>%
  autoplot(plot_type="ReliabilityDiagram",
           plot_linetypes="solid",
           plot_cols = gg_color_hue(4)[c(2,1,3,4)]) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        legend.position="none") +
  ggtitle("")

p_RelDiag_RunExmpl

ggsave(paste0("applications/plots/ReliabilityDiagramIllustration.pdf"),
       p_RelDiag_RunExmpl,
       width=14, height=15, units="cm")



# Add calibration bands to the reliability diagrams!
Cal_Bands_tbl <- tibble()
for (FC_name in C1_FC_names){
  Cal_Bands_tbl <- bind_rows(Cal_Bands_tbl,
                             calibration_bands(x=df_RunExmpl[[FC_name]], y=df_RunExmpl$y, nc=TRUE, method="round", digits=2, alpha=0.1)$bands %>%
                               mutate(forecast=FC_name))
}

p_RelDiag_hlp <- df_RunExmpl %>%
  triptych() %>%
  .$RelDiag %>%
  autoplot(region.level = NA,
           # params_ribbon = list(fill = "grey50", alpha = 0.3),
           params_CEPline=list(size = 0.5),
           params_histogram=list(yscale = 0.2, colour = "black", fill = NA)) +
  facet_wrap(.~factor(forecast, levels=C1_FC_names), nrow=2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())


p_RelDiag_CalBands <- p_RelDiag_hlp +
  geom_ribbon(data=Cal_Bands_tbl,
              aes(x=x, ymin=lwr, ymax=upr),
              fill = "grey50", alpha = 0.3) +
  theme(legend.position="bottom") +
  ggplot2::scale_color_manual(values = plot_cols, limits=C1_FC_names) +
  ggplot2::scale_linetype_manual(values = rep("solid",4), limits=C1_FC_names) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        legend.position="none")

ggsave(paste0("applications/plots/ReliabilityDiagram_CalibrationBands.pdf"),
       p_RelDiag_CalBands,
       width=14, height=15, units="cm")



#      Score decompositions        ##########################################################################

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# CORP Brier score decomposition
summary(triptych(df_RunExmpl)) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(3)

# CORP Log score decomposition
summary(triptych(df_RunExmpl), score=log_score) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(3)

# CORP MR score decomposition
summary(triptych(df_RunExmpl), score=MR_score) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(3)


# CORP Beta(2,4) score decomposition
Beta24_score <- function(y,x){Beta_score(y=y, x=x, alpha=2, beta=4, c_length=1000)}
summary(triptych(df_RunExmpl), score=Beta24_score) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(5)




#     Illustration ROC Curves     ##########################################################################

ROC_RunExmlp_Raw <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
                             plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                             PAV_ROC="FALSE",
                             size_legend=14,
                             size_axislabels=14,
                             size_axisticks=14) +
  ggtitle("(a) Raw ROC Curve")

ROC_RunExmlp_PAV <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
                             plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                             size_legend=14,
                             size_axislabels=14,
                             size_axisticks=14) +
  ggtitle("(b) PAV-Recalibrated ROC Curve")


ROC_RunExmlp_Joint <- ROC_RunExmlp_Raw + ROC_RunExmlp_PAV +
  plot_layout(guides = "collect", width=c(1,1)) & theme(legend.position = 'bottom')

ggsave(paste0("applications/plots/ROCJoint.pdf"),
       ROC_RunExmlp_Joint,
       width=22, height=13.5, units="cm")




# PAV-transformed ROC curve
ggsave(paste0("applications/plots/ROCCurveIllustration.pdf"),
       autoplot(triptych(df_RunExmpl),
                plot_type="ROC",
                plot_linetypes="solid") +
         ggtitle(""),
       width=14, height=15, units="cm")

# Raw ROC curve
ggsave(paste0("applications/plots/ROCCurveRawIllustration.pdf"),
       autoplot(triptych(df_RunExmpl),
                plot_type="ROC",
                plot_linetypes="solid",
                PAV_ROC="FALSE") +
         ggtitle(""),
       width=14, height=15, units="cm")



#      3-way ROC Curve Plot      ##########################################################################

# Copy, Paste & Edit from the package code
plot_cols=gg_color_hue(4)[c(2,1,3,4)]
names(plot_cols) <- FC_names
plot_linetypes="solid"
plot_linewidth=0.5
plot_margins=grid::unit(c(0,0.02,0,0.02), "npc")
plot_theme=ggplot2::theme_bw()
plot_legend_title="forecast"
size_title=14
size_legend=14
size_axislabels=14
size_axisticks=14

df_plot <- trpt_RunExmpl$roc %>%
  dplyr::filter(forecast %in% c("NOAA", "MCSTAT"))

df_plot_join <- full_join(df_plot %>% filter(PAV==T) %>% select(-c(PAV)),
                          df_plot %>% filter(PAV==F) %>% select(-c(PAV)),
                          by=c("forecast", "specificities"),
                          suffix = c("_PAV", "_raw")) %>%
  arrange(forecast, desc(specificities), sensitivities_raw) %>%
  group_by(forecast) %>%
  mutate(sensitivities_PAV_interpol = approx(x=1-specificities, y=sensitivities_PAV, xout=1-specificities, ties="ordered")$y)


df_plot_join %>% filter(forecast=="SIDC") %>% print(n=40)


p_ROC <- ggplot2::ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               linewidth=plot_linewidth/3, colour="black") +
  geom_ribbon(data=df_plot_join,
              aes(x=1-specificities, ymin=sensitivities_raw, ymax=sensitivities_PAV_interpol, fill=forecast, colour=forecast)) +
  # geom_path(data = df_plot_join,
  #           aes(x=1-specificities, y=sensitivities_PAV_interpol, col=forecast),
  #           linewidth=0.2, linetype="solid", col="black") +
  # geom_path(data = df_plot_join,
  #           aes(x=1-specificities, y=sensitivities_raw, col=forecast),
  #           linewidth=0.2, linetype="solid", col="black") +
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
  plot_theme +
  theme(legend.title = element_text(size=size_legend),
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



# All three Murphy diagrams together
p_ROC_combined <- (ROC_RunExmlp_Raw + ggtitle("")) + (p_ROC + theme(legend.position="none")) + (ROC_RunExmlp_PAV + ggtitle("")) +
  plot_layout(guides = "collect") & theme(legend.position = 'none')

p_ROC_combined


# Save Joint ROC Curve
ggsave(paste0("applications/plots/ROCCurveJointIllustration.pdf"),
       p_ROC_combined,
       width=24, height=10.5, units="cm")





#  Illustration MCB-DSC Plot ##########################################################################

# The tibble  df_C1full  is loaded all in the beginning of the script
trpt_C1full <- triptych(df_C1full, confidence_level = NA)

MCBDSC_point_cols <- c("ASSA"= gg_color_hue(4)[3],
                       "CLIM120"="black",
                       "DAFFS"= "black",
                       "GDAFFS"="black",
                       "MCEVOL"="black",
                       "MCSTAT"=gg_color_hue(4)[4],
                       "NICT"="black",
                       "NJIT"="black",
                       "NOAA"=gg_color_hue(4)[2],
                       "SIDC"=gg_color_hue(4)[1])

p_C1_MCBDSC <- autoplot(trpt_C1full,
                   plot_type="MCBDSC",
                   MCBDSC_point_cols=MCBDSC_point_cols,
                   MCBDSC_MCB_xlim = c(0,0.06))

p_C1_MCBDSC

summary(trpt_C1full)



ggsave(paste0("applications/plots/C1Flares_DSCMCB.pdf"),
       p_C1_MCBDSC,
       width=14, height=14, units="cm")


# Compare Brier and Log Score MCB-DSC Plots. The results change here, most prominently look at NICT
p_C1_MCBDSC_LogScore <- autoplot(trpt_C1full,
                                 plot_type="MCBDSC",
                                 MCBDSC_score = log_score,
                                 MCBDSC_point_cols=MCBDSC_point_cols)

p_C1_MCBDSC_LogScore

summary(trpt_C1full, score=log_score)

ggsave(paste0("applications/plots/C1Flares_MCBDSC_BrierLogScores.pdf"),
       (p_C1_MCBDSC + ggtitle("Brier Score") ) +
         (p_C1_MCBDSC_LogScore+ ggtitle("Log Score")),
       width=32, height=16, units="cm")





#    M1 Flares     ###########################################################################

load("applications/data/SF.FC.M1.rda")
M1_FCnames_all <- c("AMOS", "ASAP", "ASSA", "BOM", "CLIM120", "DAFFS", "GDAFFS",
                    "MAG4VW", "MAG4VWF", "MAG4W", "MAG4WF", "MCEVOL", "MCSTAT",
                    "MOSWOC", "NICT", "NOAA","SIDC")

M1_FCnames_short <- c("NICT", "NOAA", "ASSA", "MCSTAT")

df_M1full <- SF.FC.M1 %>%
  as_tibble() %>%
  dplyr::select(all_of(c("rlz.M1", M1_FCnames_all))) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)


df_M1_short <- df_M1full %>%
  select(all_of(c("y",M1_FCnames_short)))

plot_cols_M1 <- c("NICT"=gg_color_hue(6)[2],
                  "NOAA"=gg_color_hue(4)[2],
                  "ASSA"=gg_color_hue(6)[5],
                  "MCSTAT"=gg_color_hue(4)[4])

# M1 Triptych
trpt_M1 <- triptych(df_M1_short)
p_M1 <- autoplot(trpt_M1,
                 plot_linetypes="solid",
                 plot_cols = plot_cols_M1,
                 size_axislabels=12)
p_M1

ggsave(paste0("applications/plots/triptych_M1Flares.pdf"),
       p_M1,
       width=24, height=10.5, units="cm")


summary(trpt_M1, score = "brier")
summary(trpt_M1, score = "log_score")


# MCB-DSC Plot of all (many) M1 forecasts
trpt_M1full <- triptych(df_M1full, confidence_level = NA)

MCBDSC_point_cols_M1 <- c("AMOS"="black",
                          "ASAP"="black",
                          plot_cols_M1[3],    # ASSA
                          "BOM"="black",
                          "CLIM120"="black",
                          "DAFFS"= "black",
                          "GDAFFS"="black",
                          "MAG4VW"="black",
                          "MAG4VWF"= "black",
                          "MAG4W"= "black",
                          "MAG4WF"="black",
                          "MCEVOL"="black",
                          plot_cols_M1[4],    #MCSTAT
                          "MOSWOC"="black",
                          plot_cols_M1[1],   #NICT
                          plot_cols_M1[2],    #NOAA
                          "SIDC"="black")

p_trpt_M1_MCBDSC <- autoplot(trpt_M1full,
                             plot_type="MCBDSC",
                             MCBDSC_score=brier_score,
                             MCBDSC_point_cols=MCBDSC_point_cols_M1)
p_trpt_M1_MCBDSC


ggsave(paste0("applications/plots/M1Flares_MCBDSC.pdf"),
       p_trpt_M1_MCBDSC,
       width=14, height=14, units="cm")


# CORP decomposition and AUC values
summary(trpt_M1full) %>%
  left_join(trpt_M1full$auc %>% filter(PAV==TRUE) %>% select(forecast, auc) %>% rename(auc_PAV=auc)) %>%
  left_join(trpt_M1full$auc %>% filter(PAV==FALSE) %>% select(forecast, auc) %>% rename(auc_raw=auc))


# Compare Brier and Log Score MCB-DSC Plots. The results change here, most prominently look at NICT
ggsave(paste0("applications/plots/M1Flares_MCBDSC_BrierLogScores.pdf"),
       (autoplot(trpt_M1full,
                 plot_type="MCBDSC",
                 MCBDSC_score=brier_score,
                 MCBDSC_point_cols=MCBDSC_point_cols_M1) +
          ggtitle("Brier Score") ) +
         (autoplot(trpt_M1full,
                   plot_type="MCBDSC",
                   MCBDSC_score=log_score,
                   MCBDSC_point_cols=MCBDSC_point_cols_M1,
                   MCBDSC_MCB_xlim=c(0,0.17))) +
         ggtitle("Log Score"),
       width=32, height=16, units="cm")



# Plot a DSC, MCB, score Murphy diagram. Does this really help?
# trpt_M1_DSC <- SF.FC.M1 %>%
#   as_tibble() %>%
#   dplyr::select(c("rlz.M1","NOAA","MCSTAT","NICT", "MOSWOC")) %>%
#   mutate_all(funs(replace(., .<0, NA))) %>%
#   na.omit() %>%
#   rename(y=rlz.M1) %>%
#   triptych()
#
# # CORP Decomposition Triptych
# (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="DSC") + ggtitle("DSC Murphy") ) +
#   (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="MCB") + ggtitle("MCB Murphy") ) +
#   (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="score") + ggtitle("Score Murphy") )  +
#   plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# ROC vs DSC Murphy
# (autoplot(trpt_M1_DSC, plot_type="ROC") + ggtitle("ROC Curve") ) +
#   (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="DSC") + ggtitle("DSC Murphy") )



