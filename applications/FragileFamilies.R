library(triptych)


# Load the data
FFC.pred.full <- readRDS("applications/data/FFC_predictions.rds")

###########################################################################
###   Layoff
###########################################################################

FFC.layoff.full <- FFC.pred.full %>%
  filter(outcome=="layoff") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.layoff.full <- triptych(FFC.layoff.full, confidence_level = NA)



# Score Decomposition
summary(trpt.layoff.full)
summary(trpt.layoff.full, score="log_score")


# MCB-DSC Plots
autoplot(trpt.layoff.full,
         plot_type="MCBDSC",
         MCBDSC_maxslope=1)

autoplot(trpt.layoff.full,
         plot_type="MCBDSC",
         MCBDSC_maxslope=1,
         MCBDSC_score="log_score")

# Save the MCB-DSC Plots
ggsave(paste0("applications/plots/FFC_layoff_MCBDSC_BrierScore.pdf"),
       autoplot(trpt.layoff.full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")


ggsave(paste0("applications/plots/FFC_layoff_MCBDSC_LogScore.pdf"),
       autoplot(trpt.layoff.full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1,
                MCBDSC_score="log_score") +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")



### Manual Forecast Selection
FFC.layoff <- FFC.pred.full %>%
  filter(outcome=="layoff" & account %in% c("amaatouq", "ruoyangp", "Pentlandians", "benchmark_logit_full")) %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.layoff <- triptych(FFC.layoff)

ggsave(paste0("applications/plots/FFC_layoff.pdf"),
       autoplot(trpt.layoff),
       width=24, height=10.5, units="cm")

summary(trpt.layoff)
summary(trpt.layoff, score="log_score")



###########################################################################
###   Job Training
###########################################################################

FFC.jobtrain.full <- FFC.pred.full %>%
  filter(outcome=="jobTraining") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.jobtrain.full <- triptych(FFC.jobtrain.full, confidence_level = NA)

autoplot(trpt.jobtrain.full, plot_type="MCBDSC", MCBDSC_maxslope=1)
autoplot(trpt.jobtrain.full, plot_type="MCBDSC", MCBDSC_score="log_score", MCBDSC_maxslope=1)
summary(trpt.jobtrain.full)
summary(trpt.jobtrain.full, score="log_score")

ggsave(paste0("applications/plots/FFC_jobtrain_MCBDSC_BrierScore.pdf"),
       autoplot(trpt.jobtrain.full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")

ggsave(paste0("applications/plots/FFC_jobtrain_MCBDSC_LogScore.pdf"),
       autoplot(trpt.jobtrain.full,
                plot_type="MCBDSC",
                MCBDSC_score="log_score",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")



### Manual Forecast Selection
FFC.jobtrain <- FFC.pred.full %>%
  filter(outcome=="jobTraining" & account %in% c("mdrc", "benchmark_logit_full", "gsharma298", "Pentlandians")) %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.jobtrain <- triptych(FFC.jobtrain)
autoplot(trpt.jobtrain, plot.linetypes = "solid", Murphy.benchmark = "benchmark_logit_full")

ggsave(paste0("applications/plots/FFC_jobtrain.pdf"),
       autoplot(trpt.jobtrain),
       width=24, height=10.5, units="cm")



summary(trpt.jobtrain)
summary(trpt.jobtrain, score="log_score")




###########################################################################
###   Eviction
###########################################################################

FFC.evict.full <- FFC.pred.full %>%
  filter(outcome=="eviction") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.evict.full <- triptych(FFC.evict.full, confidence_level = NA)

autoplot(trpt.evict.full, plot_type="MCBDSC", MCBDSC_maxslope=1)
autoplot(trpt.evict.full, plot_type="MCBDSC", MCBDSC_score="log_score", MCBDSC_maxslope=1)
summary(trpt.evict.full)
summary(trpt.evict.full, score="log_score")


ggsave(paste0("applications/plots/FFC_evict_MCBDSC_BrierScore.pdf"),
       autoplot(trpt.evict.full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")

ggsave(paste0("applications/plots/FFC_evict_MCBDSC_LogScore.pdf"),
       autoplot(trpt.evict.full,
                plot_type="MCBDSC",
                MCBDSC_score="log_score",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
       width=14, height=12, units="cm")


ggsave(paste0("applications/plots/FFC_evict_MCBDSC_BrierScore_Square.pdf"),
       autoplot(trpt.evict.full,
                plot_type="MCBDSC",
                MCBDSC_maxslope=1) +
         theme(aspect.ratio=NULL),
      width=14, height=14, units="cm")



# Manual Forecast Selection
FFC.evict <- FFC.pred.full %>%
  filter(outcome=="eviction" & account %in% c("mdrc", "bjgoode", "benchmark_logit_full", "Justajwu")) %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.evict <- triptych(FFC.evict)

ggsave(paste0("applications/plots/FFC_evict.pdf"),
       autoplot(trpt.evict),
       width=24, height=10.5, units="cm")


summary(trpt.evict)
summary(trpt.evict, score="log_score")


# Score decomposition triptych
# CORP Decomposition Triptych
(autoplot(trpt.evict, plot_type="Murphy", Murphy_scoretype="DSC", Murphy_benchmark="benchmark_logit_full") + ggtitle("DSC Murphy") ) +
  (autoplot(trpt.evict, plot_type="Murphy", Murphy_scoretype="MCB", Murphy_benchmark="benchmark_logit_full") + ggtitle("MCB Murphy") ) +
  (autoplot(trpt.evict, plot_type="Murphy", Murphy_scoretype="MCB-DSC", Murphy_benchmark="benchmark_logit_full") + ggtitle("MCB-DSC Murphy") )  +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')











###########################################################################
### Other Development Stuff
###########################################################################




# Print score decomposition
df.decomp <- trpt.evict$score_decomposition %>%
  dplyr::rename(Forecast=forecast,
                Score=mean_score,
                MSC=miscalibration,
                DSC=discrimination,
                UNC=uncertainty)
PowerOf10 <- df.decomp %>%
  select(-Forecast) %>%
  max() %>% log(.,10) %>% floor()
df.decomp <- df.decomp %>%
  dplyr::mutate_at(vars(-("Forecast")), funs(. / 10^PowerOf10)) %>%
  dplyr::mutate_at(vars(-("Forecast")), round, 3)
p.table <- gridExtra::tableGrob(df.decomp, theme=gridExtra::ttheme_default())
ggsave(paste0("applications/plots/FFC_evict_score_decomp.pdf"), p.table, width=4, height=3, units="in")





###########################################################################
### Check for anomalies in the eviction prediction data
###########################################################################

### Layoff
FFC.layoff.full <- FFC.pred.full %>%
  filter(outcome=="layoff") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.layoff.full <- triptych(FFC.layoff.full, RelDiag.joint = TRUE)
trpt.layoff.full$score_decomposition %>% arrange(mean_score) %>% print(n=20)
trpt.layoff.full$score_decomposition %>% arrange(desc(discrimination)) %>% print(n=30)

trpt.layoff.full$plots$MCBDSC
trpt.layoff.full$plots$score_table

## Findings for Layoff from these manual calls
# - amaatouq, and bjgoode have good discrimination, but bad calibration
# - fperraudeau is particularly badly calibrated,
# - LouisR even more!
# - ruoyangp might also be intersting


# MCB-DSC Plot: First filter on the XX% of the best calibrated forecasts...
FC.names.filter <- trpt.layoff.full$score_decomposition %>%
  filter(miscalibration <= quantile(miscalibration, 0.7)) %>%
  pull(forecast)

trpt.layoff.DSCMCB <- triptych(FFC.layoff.full %>% select(c("y",FC.names.filter)), RelDiag.joint = TRUE)
trpt.layoff.DSCMCB$plots$p.DSC.MSC
ggsave(paste0("applications/plots/FFC_layoff_DSCMCB.pdf"), trpt.layoff.DSCMCB$plots$p.DSC.MSC, width=10, height=10, units="in")

# Illustrate why  the plots have to be babysitted for now...
ggsave(paste0("applications/plots/FFC_layoff_DSCMCB_ALL.pdf"),
       triptych(FFC.layoff.full, RelDiag.joint = TRUE)$plots$p.DSC.MSC,
       width=10, height=10, units="in")




### Job Training
FFC.jobtrain.full <- FFC.pred.full %>%
  filter(outcome=="jobTraining") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.jobtrain.full <- triptych(FFC.jobtrain.full, RelDiag.joint = TRUE)
trpt.jobtrain.full$score_decomposition %>% arrange(mean_score) %>% print(n=100)
trpt.jobtrain.full$score_decomposition %>% arrange(desc(miscalibration)) %>% print(n=80)

## Findings for Job Training from these manual calls
# - mdrc has the best Brier score
# - both benchmarks perform similar to the best model here!!!
# - sy has best discrimination but bad calibration
# - Pentlandians similarly
# - gsharma298 has very bad calibration.. KAG similarly

# MCB-DSC Plot: First filter on the XX% of the best calibrated forecasts...
FC.names.filter <- trpt.jobtrain.full$score_decomposition %>%
  filter(miscalibration <= quantile(miscalibration, 0.7)) %>%
  pull(forecast)

trpt.jobtrain.DSCMCB <- triptych(FFC.jobtrain.full %>% select(c("y",FC.names.filter)), RelDiag.joint = TRUE)
trpt.jobtrain.DSCMCB$plots$p.DSC.MSC
ggsave(paste0("applications/plots/FFC_jobtrain_DSCMCB.pdf"), trpt.jobtrain.DSCMCB$plots$p.DSC.MSC, width=10, height=10, units="in")





### Eviction
FFC.evict.full <- FFC.pred.full %>%
  filter(outcome=="eviction") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

trpt.evict.full <- triptych(FFC.evict.full, RelDiag.joint = TRUE)
trpt.evict.full$score_decomposition %>% arrange(mean_score) %>% print(n=100)

## Findings for Evict from these manual calls
# - bjgoode has good discrimination, but bad calibration
# - Justajwu has very good discrimination, but not so good calibration
# - Tamkinat similary
# - nmandell and ETsurkov are well calibrated, but lacks a bit of discrimination


# MCB-DSC Plot: First filter on the XX% of the best calibrated forecasts...
FC.names.filter <- trpt.evict.full$score_decomposition %>%
  filter(miscalibration <= quantile(miscalibration, 0.8)) %>%
  pull(forecast)

trpt.evict.DSCMCB <- triptych(FFC.evict.full %>% select(c("y",FC.names.filter)), RelDiag.joint = TRUE)
trpt.evict.DSCMCB$plots$p.DSC.MSC
ggsave(paste0("applications/plots/FFC_evict_DSCMCB.pdf"), trpt.evict.DSCMCB$plots$p.DSC.MSC, width=10, height=10, units="in")




###########################################################################
# Try out scatter plot!


df.test <- trpt.evict.full$score_decomposition %>% filter(miscalibration <= 0.008)

df.abline <- data.frame(slope=1,
                        intercept=seq(-max(df.test$miscalibration),
                                      max(df.test$discrimination),
                                      length.out=15))

p <- ggplot(data=df.test) +
  theme_classic() +
  geom_abline(data=df.abline, aes(intercept=intercept, slope=slope, color=intercept)) +
  scale_colour_gradient(low = "white", high = "black", name="UNC - Brier Score", guide = "colourbar") +
  guides(color = guide_colourbar(barwidth = 20, barheight = 0.5)) +
  geom_point(aes(x=miscalibration, y=discrimination)) +
  geom_text(aes(x=miscalibration, y=discrimination, label=forecast), size=3, vjust=-0.1, hjust=-0.1, check_overlap=TRUE) +
  theme(legend.position = "bottom")

  ggsave("applications/plots/FFC_evict_MCBDSCPlot.pdf", p)




###########################################################################

# devtools::install_git("https://github.com/aijordan/murphydiagram2")
library(murphydiagram2)
library(patchwork)

source('Mean_RelDiag/reldiag_function.R', echo=TRUE)


### Material Hardship
account.names <- c("haixiaow", "aemack", "mb298")
FFC.materialHardship <- FFC.pred %>%
  filter(outcome=="materialHardship" & account %in% account.names) %>%
  select(challengeID, account, truth, prediction) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

r1 <- reldiag(x=FFC.materialHardship[["haixiaow"]], y=FFC.materialHardship$y)
r2 <- reldiag(x=FFC.materialHardship[["aemack"]], y=FFC.materialHardship$y)
r3 <- reldiag(x=FFC.materialHardship[["mb298"]], y=FFC.materialHardship$y, lim=range(FFC.materialHardship$y))


### Grit
account.names <- c("sy", "ADSgrp5", "rfjz")
FFC.Grit <- FFC.pred %>%
  filter(outcome=="grit" & account %in% account.names) %>%
  select(challengeID, account, truth, prediction) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()


r1 <- reldiag(x=FFC.Grit[["sy"]], y=FFC.Grit$y)
r2 <- reldiag(x=FFC.Grit[["ADSgrp5"]], y=FFC.Grit$y)
r3 <- reldiag(x=FFC.Grit[["rfjz"]], y=FFC.Grit$y)


### GPA
account.names <- c("sy", "aprilfeifei", "Justajwu", "Tamkinat", "signoret", "pkrafft")
account.names <- c("sy", "hamidrezaomidvar", "khiguera")
FFC.GPA <- FFC.pred %>%
  filter(outcome=="gpa" & account %in% account.names) %>%
  select(challengeID, account, truth, prediction) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()




r.list <- list()
r.tbl.list <- list()
for (acc.name in account.names) {
  r <- reldiag(x=FFC.GPA[[acc.name]], y=FFC.GPA$y)
  r.list[[acc.name]] <- recordPlot()
  r.tbl.list[[acc.name]] <- tibble::tibble(x=r$x, y=r$y, x_rc=r$x_rc)
}


# Raw and recalibrated Murphy diagram

# Function for isotonic recalibration
FC.recal <- function(x,y){
  iso.reg <- isoreg(x=x, y=y)
  iso.reg.tbl <- tibble(x=iso.reg$x,
                        y=iso.reg$y,
                        FC_rec=iso.reg$yf[order(iso.reg$ord)])
  return(iso.reg.tbl$FC_rec)
}

# Recalibrate
FFC.GPA <- FFC.GPA %>%
  mutate_at(.vars=vars(-"y"), .funs=list(recal=~FC.recal(.,y)))

# Raw Murphy
m <- murphydiagram2::murphydiag(f1=FFC.GPA$hamidrezaomidvar,
                           f2=FFC.GPA$khiguera,
                           f3=FFC.GPA$sy,
                           y=FFC.GPA$y,
                           type="mean")
m.raw.ggplot <- autoplot(m) +
  theme(legend.position = "bottom")


m_recal <- murphydiagram2::murphydiag(f1=FFC.GPA$hamidrezaomidvar_recal,
                                      f2=FFC.GPA$khiguera_recal,
                                      f3=FFC.GPA$sy_recal,
                                      y=FFC.GPA$y,
                                      type="mean")
m.recal.ggplot <- autoplot(m_recal) +
  theme(legend.position = "bottom")


m.raw.ggplot + cowplot::as_grob(r.list[[3]]) + m.recal.ggplot


