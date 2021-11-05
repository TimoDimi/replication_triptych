library(triptych)
load("applications/data/SF.FC.M1.rda")
load("applications/data/SF.FC.C1.rda")

###########################################################################
###    M1 Flares
###########################################################################

df.M1 <- SF.FC.M1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.M1","NOAA","MCSTAT","NICT", "CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)

trpt.M1 <- triptych(df.M1)

ggsave(paste0("applications/plots/triptych_M1Flares.pdf"),
       autoplot(trpt.C1,
                Murphy.scoretype="MCB-DSC",
                plot.linetypes="solid",
                size.axislabels=12),
       width=24, height=10.5, units="cm")

# ggsave(paste0("applications/plots/triptych_M1Flares_MurphyBormalized.pdf"),
#        autoplot(trpt.M1, plot.linetypes = "solid", Murphy.benchmark = "NOAA"),
#        width=24, height=10, units="cm")

plot(trpt.M1, plot.linetypes = "solid", plot.type="Murphy")
summary(trpt.M1, score = "brier")
summary(trpt.M1, score = "log_score")


###########################################################################
### DSC-MCB Plot:
df.M1.full <- SF.FC.M1 %>%
  dplyr::select(-c("VALID_DATE", "NJIT")) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)

trpt.M1.full <- triptych(df.M1.full, confidence_level = NA)

ggsave(paste0("applications/plots/M1Flares_DSCMCB.pdf"),
       autoplot(trpt.M1.full,
                RelDiag.joint = TRUE,
                plot.type="MCBDSC",
                MCBDSC.maxslope=1),
       width=14, height=14, units="cm")


ggsave(paste0("applications/plots/M1Flares_DSCMCB_BrierLogScores.pdf"),
       (autoplot(trpt.M1.full, plot.type="MCBDSC") + ggtitle("Brier Score") ) +
         (autoplot(trpt.M1.full, plot.type="MCBDSC", MCBDSC.score=log_score) + ggtitle("Log Score")),
       width=32, height=16, units="cm")


###########################################################################
# Test truncated log-score...
# Result: One can taylor ones decision to the truncation parameter...
trunc_log_score <- function(y,x){
  eps <- 0.00001
  x_capped <- pmin(1-eps, pmax(x,eps))
  -y * log(x_capped) - (1-y) * log(1-x_capped)
}

summary(trpt.M1, score = trunc_log_score)
summary(trpt.M1, score = "log_score")
summary(trpt.M1, score = "brier")
###########################################################################


# Comment: Somehow include that the score decomposition reported by the triptych!
r <- reliabilitydiag(NOAA=df.M1 %>% dplyr::pull("NOAA"),
                     NICT=df.M1 %>% dplyr::pull("NICT"),
                     CLIM120=df.M1 %>% dplyr::pull("CLIM120"),
                     y=df.M1 %>% dplyr::pull("y"))






###########################################################################
###    C1 Flares
###########################################################################

df.C1 <- SF.FC.C1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.C1", "NOAA", "DAFFS","SIDC","CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

trpt.C1 <- triptych(df.C1)

p <- autoplot(trpt.C1,
              Murphy.scoretype="MCB-DSC",
              plot.linetypes="solid",
              size.axislabels=12)


ggsave(paste0("applications/plots/triptych_C1Flares.pdf"),
       p,
       width=24, height=10.5, units="cm")




##### Illustration Different rankings of scores
## Brier and Log score values
mean((SF.FC.C1$NOAA - SF.FC.C1$rlz.C1)^2) %>% round(4)
mean((SF.FC.C1$DAFFS - SF.FC.C1$rlz.C1)^2) %>% round(4)
mean((SF.FC.C1$SIDC - SF.FC.C1$rlz.C1)^2) %>% round(4)
mean((SF.FC.C1$CLIM120 - SF.FC.C1$rlz.C1)^2) %>% round(4)


log.score(SF.FC.C1$NOAA, SF.FC.C1$rlz.C1) %>% round(4)
log.score(SF.FC.C1$DAFFS, SF.FC.C1$rlz.C1) %>% round(4)
log.score(SF.FC.C1$SIDC, SF.FC.C1$rlz.C1) %>% round(4)
log.score(SF.FC.C1$CLIM120, SF.FC.C1$rlz.C1) %>% round(4)



###########################################################################
##### Illustration Murphy Diagram

df.C1.Murphy <- SF.FC.C1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.C1","NOAA", "DAFFS","SIDC","CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

trpt.C1.dom <- triptych(df.C1.Murphy %>% dplyr::select("y", "NOAA", "CLIM120"))

ggsave(paste0("applications/plots/MurphyDomination.pdf"),
       autoplot(trpt.C1.dom,
                plot.type="Murphy",
                plot.cols=gg_color_hue(4)[1:2],
                plot.linetypes = c("solid")) +
         ggtitle("") +
         ylim(c(0,0.33)),
       width=11, height=12, units="cm")



trpt.C1.nodom <- triptych(df.C1.Murphy %>% dplyr::select("y", "DAFFS", "SIDC"))

ggsave(paste0("applications/plots/MurphyNonDomination.pdf"),
       autoplot(trpt.C1.nodom,
                plot.type="Murphy",
                plot.cols=gg_color_hue(4)[3:4],
                plot.linetypes = c("solid")) +
         ggtitle("") +
         ylim(c(0,0.33)),
       width=11, height=12, units="cm")



###########################################################################
##### Illustration Reliability Diagram

trpt.C1.short <- triptych(df.C1.Murphy)

ggsave(paste0("applications/plots/ReliabilityDiagramIllustration.pdf"),
       autoplot(trpt.C1.short,
                plot.type="ReliabilityDiagram",
                plot.linetypes="solid") +
         theme(strip.background = element_blank(),
               strip.text.x = element_text(size = 16),
               legend.position="none") +
         ggtitle(""),
       width=14, height=15, units="cm")



summary(trpt.C1.short) %>%
  as.data.frame() %>%
  print(digits=4)


###########################################################################
##### Illustration ROC Curve

ggsave(paste0("applications/plots/ROCCurveIllustration.pdf"),
       autoplot(trpt.C1.short,
                plot.type="ROC",
                plot.linetypes="solid") +
         ggtitle(""),
       width=14, height=15, units="cm")

ggsave(paste0("applications/plots/ROCCurveRawIllustration.pdf"),
       autoplot(trpt.C1.short,
                plot.type="ROC",
                plot.linetypes="solid",
                PAV.ROC="FALSE") +
         ggtitle(""),
       width=14, height=15, units="cm")



### DSC-MCB Plot:
df.C1.full <- SF.FC.C1 %>%
  dplyr::select(-c("VALID_DATE", "ASAP", "BOM", "MAG4VW", "MAG4VWF", "MAG4W", "MAG4WF", "MOSWOC")) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

trpt.C1.full <- triptych(df.C1.full, confidence_level = NA)

ggsave(paste0("applications/plots/C1Flares_DSCMCB.pdf"),
       autoplot(trpt.C1.full,
                RelDiag.joint = TRUE,
                plot.type="MCBDSC",
                MCBDSC.maxslope=1),
       width=14, height=14, units="cm")

