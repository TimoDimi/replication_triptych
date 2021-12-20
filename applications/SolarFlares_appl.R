library(triptych)
load("applications/data/SF.FC.M1.rda")
load("applications/data/SF.FC.C1.rda")

###########################################################################
###    C1 Flares
###########################################################################

df_C1 <- SF.FC.C1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.C1", "NOAA", "DAFFS","SIDC","CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)


# C1 Triptych
trpt_C1 <- triptych(df_C1)

p_C1 <- autoplot(trpt_C1,
                plot_linetypes="solid",
                size_axislabels=12)

ggsave(paste0("applications/plots/triptych_C1Flares.pdf"),
       p_C1,
       width=24, height=10.5, units="cm")



##### Illustration Different rankings of scores
## Brier and Log score values
mean((df_C1$NOAA - df_C1$y)^2) %>% round(4)
mean((df_C1$DAFFS - df_C1$y)^2) %>% round(4)
mean((df_C1$SIDC - df_C1$y)^2) %>% round(4)
mean((df_C1$CLIM120 - df_C1$y)^2) %>% round(4)

mean(log_score(x=df_C1$NOAA, y=df_C1$y)) %>% round(4)
mean(log_score(x=df_C1$DAFFS, y=df_C1$y)) %>% round(4)
mean(log_score(x=df_C1$SIDC, y=df_C1$y)) %>% round(4)
mean(log_score(x=df_C1$CLIM120, y=df_C1$y)) %>% round(4)



###########################################################################
##### Illustration Murphy Diagram

trpt_C1_dom <- triptych(df_C1 %>% dplyr::select("y", "NOAA", "CLIM120"))

ggsave(paste0("applications/plots/MurphyDomination.pdf"),
       autoplot(trpt_C1_dom,
                plot_type="Murphy",
                plot_cols=gg_color_hue(4)[1:2],
                plot_linetypes = c("solid")) +
         ggtitle("") +
         ylim(c(-0.21,0.05)),
       width=11, height=12, units="cm")


trpt_C1_nodom <- triptych(df_C1 %>% dplyr::select("y", "DAFFS", "SIDC"))

ggsave(paste0("applications/plots/MurphyNonDomination.pdf"),
       autoplot(trpt_C1_nodom,
                plot_type="Murphy",
                plot_cols=gg_color_hue(4)[3:4],
                plot_linetypes = c("solid")) +
         ggtitle("") +
         ylim(c(-0.21,0.05)),
       width=11, height=12, units="cm")



###########################################################################
##### Illustration Reliability Diagram

ggsave(paste0("applications/plots/ReliabilityDiagramIllustration.pdf"),
       df_C1 %>%
         triptych() %>%
         autoplot(plot_type="ReliabilityDiagram",
                  plot_linetypes="solid") +
         theme(strip.background = element_blank(),
               strip.text.x = element_text(size = 16),
               legend.position="none") +
         ggtitle(""),
       width=14, height=15, units="cm")

# CORP score decomposition
summary(triptych(df_C1)) %>%
  as.data.frame() %>%
  print(digits=4)



###########################################################################
##### Illustration ROC Curve

# PAV-transformed ROC curve
ggsave(paste0("applications/plots/ROCCurveIllustration.pdf"),
       autoplot(triptych(df_C1),
                plot_type="ROC",
                plot_linetypes="solid") +
         ggtitle(""),
       width=14, height=15, units="cm")

# Raw ROC curve
ggsave(paste0("applications/plots/ROCCurveRawIllustration.pdf"),
       autoplot(triptych(df_C1),
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
                MCBDSC_maxslope=1),
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



