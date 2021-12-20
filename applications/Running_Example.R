library(triptych)


###########################################################################
###    Select data and forecasts
###########################################################################



###########################################################################
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
###########################################################################

###########################################################################
# Interesting findings for "NOAA", "GDAFFS","SIDC","MCSTAT"
#  - SIDC and MCSTAT have equal discrimination, but MCSTAT miscalibrated., hence different Murphy
#  - Murphy diagram is interesting for GDAFFS, SIDC and MCSTAT in the regions smaller and larger 0.5
#  - GDAFFS forecasts too small probabilities, MCSTAT rather too large ones
#  - Using ASSA is interesting as it has an Inf log score!
load("applications/data/SF.FC.C1.rda")
FC_names <- c("NOAA","MCSTAT", "SIDC", "ASSA")

df_RunExmpl <- SF.FC.C1 %>%
  dplyr::select(c("rlz.C1", FC_names)) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

# Triptych
trpt_RunExmpl <- triptych(df_RunExmpl)
p_RunExmpl <- autoplot(trpt_RunExmpl,
                plot_linetypes="solid",
                size_axislabels=12)

p_RunExmpl

ggsave(paste0("applications/plots/triptych_RunExmpl.pdf"),
       p_RunExmpl,
       width=24, height=10.5, units="cm")


# Brier, Log and Beta Scores
df_RunExmpl %>%
  reshape2::melt(id="y") %>%
  group_by(variable) %>%
  summarize(Brier_score = mean((y-value)^2),
            log_score = mean(log_score(y, value)),
            Beta_score24 = mean(Beta_score(y, value, alpha=2, beta=4)),
            Beta_score42 = mean(Beta_score(y, value, alpha=4, beta=2))) %>%
  arrange(Brier_score)



###########################################################################
##### Illustration Murphy Diagram

trpt_RunExmpl_dom <- triptych(df_RunExmpl %>% dplyr::select("y", "NOAA", "SIDC"))
p_Murphy_dom <- autoplot(trpt_RunExmpl_dom,
                       plot_type="Murphy",
                       plot_cols=gg_color_hue(4)[c(1,3)],
                       plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(-0.21,0.06))

p_Murphy_dom

trpt_RunExmpl_nodom <- triptych(df_RunExmpl %>% dplyr::select("y", "MCSTAT", "ASSA"))
p_Murphy_nodom <- autoplot(trpt_RunExmpl_nodom,
                       plot_type="Murphy",
                       plot_cols=gg_color_hue(4)[c(2,4)],
                       plot_linetypes = c("solid")) +
  ggtitle("") +
  ylim(c(-0.21,0.06))

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


###########################################################################
##### Illustration Reliability Diagram

p_RelDiag_RunExmpl <- df_RunExmpl %>%
  triptych() %>%
  autoplot(plot_type="ReliabilityDiagram",
           plot_linetypes="solid") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        legend.position="none") +
  ggtitle("")

ggsave(paste0("applications/plots/ReliabilityDiagramIllustration.pdf"),
       p_RelDiag_RunExmpl,
       width=14, height=15, units="cm")




#####
# Score decompositions

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
  round_df(4)

# CORP Log score decomposition
summary(triptych(df_RunExmpl), score=log_score) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(4)

# CORP Beta(2,4) score decomposition
Beta24_score <- function(y,x){Beta_score(y=y, x=x, alpha=2, beta=4, c_length=1000)}
summary(triptych(df_RunExmpl), score=Beta24_score) %>%
  as.data.frame() %>%
  arrange(factor(forecast, levels=c("NOAA", "SIDC", "ASSA", "MCSTAT"))) %>%
  round_df(5)


###########################################################################
##### Illustration ROC Curve

ROC_RunExmlp_Raw <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
                             PAV_ROC="FALSE",
                             size_legend=14,
                             size_axislabels=14,
                             size_axisticks=14) +
  ggtitle("(a) Raw ROC Curve")

ROC_RunExmlp_PAV <- autoplot(triptych(df_RunExmpl),
                             plot_type="ROC",
                             plot_linetypes="solid",
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


###########################################################################
##### Illustration MCB-DSC Plot

# full C1 data frame
df_C1full <- SF.FC.C1 %>%
  dplyr::select(-c("VALID_DATE", "ASAP", "BOM", "MAG4VW", "MAG4VWF", "MAG4W", "MAG4WF", "MOSWOC")) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

trpt_C1full <- triptych(df_C1full, confidence_level = NA)

ggsave(paste0("applications/plots/C1Flares_DSCMCB.pdf"),
       autoplot(trpt_C1full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=0.65),
       width=14, height=14, units="cm")


# Compare Brier and Log Score MCB-DSC Plots. The results change here, most prominently look at NICT
ggsave(paste0("applications/plots/C1Flares_MCBDSC_BrierLogScores.pdf"),
       (autoplot(trpt_C1full, plot_type="MCBDSC") + ggtitle("Brier Score") ) +
         (autoplot(trpt_C1full, plot_type="MCBDSC", MCBDSC_score=log_score) + ggtitle("Log Score")),
       width=32, height=16, units="cm")




###########################################################################
###    M1 Flares
###########################################################################

df_M1 <- SF.FC.M1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.M1","NOAA","MCSTAT","NICT", "CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)


# M1 Triptych
trpt_M1 <- triptych(df_M1)
p_M1 <- autoplot(trpt_M1,
                 plot_linetypes="solid",
                 size_axislabels=12)
ggsave(paste0("applications/plots/triptych_M1Flares.pdf"),
       p_M1,
       width=24, height=10.5, units="cm")


summary(trpt_M1, score = "brier")
summary(trpt_M1, score = "log_score")


# MCB-DSC Plot:
df_M1full <- SF.FC.M1 %>%
  dplyr::select(-c("VALID_DATE", "NJIT")) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)

trpt_M1full <- triptych(df_M1full, confidence_level = NA)

ggsave(paste0("applications/plots/M1Flares_MCBDSC.pdf"),
       autoplot(trpt_M1full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1),
       width=14, height=14, units="cm")

# CORP decomposition and AUC values
summary(trpt_M1full) %>%
  left_join(trpt_M1full$auc %>% filter(PAV==TRUE) %>% select(forecast, auc) %>% rename(auc_PAV=auc)) %>%
  left_join(trpt_M1full$auc %>% filter(PAV==FALSE) %>% select(forecast, auc) %>% rename(auc_raw=auc))

# Compare Brier and Log Score MCB-DSC Plots. The results change here, most prominently look at NICT
ggsave(paste0("applications/plots/M1Flares_MCBDSC_BrierLogScores.pdf"),
       (autoplot(trpt_M1full, plot_type="MCBDSC") + ggtitle("Brier Score") ) +
         (autoplot(trpt_M1full, plot_type="MCBDSC", MCBDSC_score=log_score, MCBDSC_maxslope=5) + ggtitle("Log Score")),
       width=32, height=16, units="cm")



# Plot a DSC, MCB, score Murphy diagram. Does this really help?
trpt_M1_DSC <- SF.FC.M1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.M1","NOAA","MCSTAT","NICT", "MOSWOC")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1) %>%
  triptych()

# CORP Decomposition Triptych
(autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="DSC") + ggtitle("DSC Murphy") ) +
  (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="MCB") + ggtitle("MCB Murphy") ) +
  (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="score") + ggtitle("Score Murphy") )  +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# ROC vs DSC Murphy
(autoplot(trpt_M1_DSC, plot_type="ROC") + ggtitle("ROC Curve") ) +
  (autoplot(trpt_M1_DSC, plot_type="Murphy", Murphy_scoretype="DSC") + ggtitle("DSC Murphy") )



