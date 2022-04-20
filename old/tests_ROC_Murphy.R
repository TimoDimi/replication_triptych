

##### Precipitation
FC_names <- c("Logistic", "EMOS", "ENS", "EPC")
FC_names <- c("EPC", "Logistic")

df.precip.recal <- df.precip %>%
  dplyr::select(c("y", FC_names)) %>%
  dplyr::mutate(across(all_of(FC_names), ~ isotone::gpava(z=., y=y)$x))

trpt.precip.recal <- triptych(df.precip.recal)

ggsave(paste0("./applications/plots/triptych_precip_recal.pdf"), autoplot(trpt.precip.recal), width = 30, height = 12, units = "cm")


p <- (autoplot(trpt.precip.recal, Murphy.scoretype = "score", plot.type="ROC") + theme(legend.position="none") + geom_vline(xintercept = 0.855, col="blue")) +
  (cost_curves(df.precip.recal, cost_type="Kendall")$plot + theme(legend.position="none")+ geom_vline(xintercept = 0.075, col="blue")) +
  (cost_curves(df.precip.recal, cost_type="rate-driven")$plot+ theme(legend.position="none") + geom_vline(xintercept = 0.075, col="blue")) +
  (autoplot(trpt.precip.recal, Murphy.scoretype = "score", plot.type="Murphy") + theme(legend.position="none")+ geom_vline(xintercept = 0.27, col="blue"))

ggsave(paste0("./applications/plots/ROC_Kendall_Murphy_precip.pdf"), p, width = 20, height = 20, units = "cm")

ggsave(paste0("./applications/plots/ROC_Kendall_Murphy_precip_1Row.pdf"), p + plot_layout(nrow = 1), width = 30, height = 9, units = "cm")




## Test something!
S_Rm1 <- S_Rm1_c(x_sorted=df.precip.recal$EPC,
        S=function(theta) mean(murphydiagram::extremal_score(x=df.precip.recal$EPC, y=df.precip.recal$y, theta=theta, functional = "expectile", alpha = 0.5)))

ggplot(data.frame(c=seq(0,1,length.out=101), S_Rm1=S_Rm1)) +
  geom_line(aes(x=c, y=S_Rm1))



cc_precip <- cost_curves(df.precip.recal, cost_type="Kendall")
ecdf_EPC <- ecdf(df.precip.recal$EPC)

costs_df <- cc_precip$costs_df %>%
  filter(Forecast=="EPC") %>%
  mutate(Fc = ecdf_EPC(c))

p_test <- ggplot(costs_df,
            aes(x=Fc, y=cost, color=Forecast, linetype=Forecast)) +
  theme_bw() +
  geom_line() +
  theme(legend.position = "bottom",
        legend.key.size = grid::unit(2, "lines"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(nrow = 1))


df_Murphy_trans <- trpt.precip.recal$Murphy %>%
  filter(Forecast=="EPC") %>%
  select(theta, elementary.score) %>%
  mutate(Fc = ecdf_EPC(theta))

p_test_Murphy <- ggplot(df_Murphy_trans,
                        aes(x=Fc, y=elementary.score)) +
  theme_bw() +
  geom_line() +
  theme(legend.position = "bottom",
        legend.key.size = grid::unit(2, "lines"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(nrow = 1))



## Illustrate the ROC and Kendall equivalence
pi.frac <- mean(df.precip.recal$y)/(1-mean(df.precip.recal$y))
df.isometrics <- data.frame(intercept = seq(0,1+pi.frac, length.out=7),
                            slope = -pi.frac,
                            xintercept=seq(1,0, length.out=7))

p <- (autoplot(trpt.precip.recal, Murphy.scoretype = "score", plot.type="ROC") +
        geom_vline(xintercept = 0.855, col="grey") +
        geom_abline(data=df.isometrics, aes(intercept=intercept, slope=slope), col="blue", linetype="dashed", size=0.5) +
        geom_abline(intercept=1.5726496, slope=-1.358974, col="blue", size=1)) +
  (cost_curves(df.precip.recal, cost_type="Kendall") +
     geom_vline(xintercept = 0.075, col="grey") +
     geom_vline(data=df.isometrics, aes(xintercept=xintercept), col="blue", linetype="dashed", size=0.5) + xlim(c(0,1)) +
     geom_vline(xintercept=0.333, col="blue", size=1))

p
ggsave(paste0("./applications/plots/ROC_Kendall_Precip.pdf"), p, width = 30, height = 17, units = "cm")




##### C1 Flares
df.C1.recal <- df.C1.Murphy %>%
  dplyr::select("y", "DAFFS", "SIDC") %>%
  dplyr::mutate(across(c("DAFFS", "SIDC"), ~ isotone::gpava(z=., y=y)$x))


trpt.C1.recal <- triptych(df.C1.recal)
p <- autoplot(trpt.C1.recal, Murphy.scoretype = "score", plot.type="ROC") +
  autoplot(trpt.C1.recal, Murphy.scoretype = "score", plot.type="Murphy") +
  cost_curves(df.C1.recal, cost_type="Kendall")
ggsave(paste0("./applications/plots/ROC_Kendall_Murphy_C1Flares.pdf"), p, width = 30, height = 12, units = "cm")



trpt.C1 <- triptych(df.C1.Murphy %>% dplyr::select("y", "DAFFS", "SIDC"))
autoplot(trpt.C1, Murphy.scoretype = "score", plot.type="ROC") +
  autoplot(trpt.C1, Murphy.scoretype = "score", plot.type="Murphy") +
  cost_curves(df.C1.Murphy %>% dplyr::select("y", "DAFFS", "SIDC"), cost_type="Kendall")



##### Recid Forecasts

FC_names <- c("Logit", "GBM", "MTurk", "COMPAS")
FC_names <- c("GBM", "MTurk")

df.recid.recal <- df.recid %>%
  dplyr::select(c("y", FC_names)) %>%
  dplyr::mutate(across(all_of(FC_names), ~ isotone::gpava(z=., y=y)$x))

trpt.recid.recal <- triptych(df.recid.recal)
p <- (autoplot(trpt.recid.recal, Murphy.scoretype = "score", plot.type="ROC") + theme(legend.position="none")) +
  (cost_curves(df.recid.recal, cost_type="Kendall")$plot + theme(legend.position="none")) +
  (cost_curves(df.recid.recal, cost_type="rate-driven")$plot + theme(legend.position="none")) +
  (autoplot(trpt.recid.recal, Murphy.scoretype = "score", plot.type="Murphy") + theme(legend.position="none")) +
  plot_layout(nrow = 1)

ggsave(paste0("./applications/plots/ROC_Kendall_Murphy_recid.pdf"), p+ plot_layout(nrow = 1), width = 30, height = 9, units = "cm")





### test again:
ecdf_DAFFS <- ecdf(df.C1.recal$DAFFS)

df_Murphy_trans <- trpt.C1.recal$Murphy %>%
  filter(Forecast=="DAFFS") %>%
  select(theta, elementary.score) %>%
  mutate(Fc = ecdf_DAFFS(theta))

p_test <- ggplot(df_Murphy_trans, aes(x=Fc, y=elementary.score)) +
  theme_bw() +
  geom_line()



### Test with the simulation example!
set.seed(1)
n <- 10000
beta <- 1
p0 <- runif(n)
rlz <- rbinom(n, 1, p0)
p1 <- pmax(1/3,pmin(p0^beta, 2/3))
p1[p1 == 1/3] = 1/6
p1[p1 == 2/3] = 5/6
p2 <- p0
p2[(p2>1/3 & p2<2/3)] <- 1/2

p.df <- data.frame(y=rlz, p1=p1, p2=p2)
trpt_D2 <- triptych(p.df)
autoplot(trpt_D2, plot.type="Murphy") +
  cost_curves(p.df, cost_type="Kendall")$plot




### Try out different Murphy plots

trpt.precip <- triptych(df.precip %>% select(y, Logistic, EMOS, EPC))

autoplot(trpt.precip, plot.type="Murphy") +
  autoplot(trpt.precip, plot.type="Murphy", Murphy.benchmark="Logistic") +
  autoplot(trpt.precip, plot.type="Murphy", Murphy.benchmark="EMOS")



p <- (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="score") + ggtitle("El. Score") ) +
  (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="score", Murphy.benchmark="EMOS") + ggtitle("El. Score Diff.") ) +
  (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="recalibrated") + ggtitle("Recal. El. Score")) +
  (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="DSC") + ggtitle("DSC") ) +
  (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="MCB-DSC") + ggtitle("MCB-DSC") ) +
  (autoplot(trpt.precip, plot.type="Murphy", Murphy.scoretype="R") + ggtitle("(MCB-DSC)/UNC")) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(paste0("./applications/plots/Murphy_comparison_precip.pdf"), p, width = 30, height = 21, units = "cm")




### M1 Solar Flares
FC_names <- c("NICT", "NOAA")

df.M1.recal <- df.M1 %>%
  dplyr::select(c("y", FC_names)) %>%
  dplyr::mutate(across(all_of(FC_names), ~ isotone::gpava(z=., y=y, ties = "secondary")$x))


M1.trpt <- triptych(df.M1 %>% select(y, NICT, NOAA))

ggsave(paste0("./applications/plots/M1Flares_triptych.pdf"), autoplot(M1.trpt), width = 27, height = 12, units = "cm")



p <- autoplot(M1.trpt, plot.type="ROC") +
  autoplot(M1.trpt, Murphy.scoretype="DSC", plot.type="Murphy") +
  cost_curves(df.M1 %>% select(y, NICT, NOAA), cost_type="Kendall") +
  cost_curves(df.M1 %>% select(y, NICT, NOAA), cost_type="rate-driven")
ggsave(paste0("./applications/plots/Curves_Comparison_M1Flares.pdf"), p, width = 20, height = 21, units = "cm")


M1.trpt.recal <- triptych(df.M1.recal)
p <- autoplot(M1.trpt.recal, plot.type="ROC") +
  (autoplot(M1.trpt.recal, Murphy.scoretype="DSC", plot.type="Murphy") + ggtitle("DSC Murphy")) +
  cost_curves(df.M1.recal, cost_type="Kendall") +
  cost_curves(df.M1.recal, cost_type="rate-driven")
ggsave(paste0("./applications/plots/Curves_Comparison_M1Flares_recal.pdf"), p, width = 20, height = 21, units = "cm")


M1.trpt.recal$auc
summary(M1.trpt.recal$RelDiag)
### Conclusion:
# There still seems to be a bug in the Rate-driven/Kendall curve implementation!
# The curves should not change under PAV-recalibration (as ROC and DSC are invariant!) But they do towards the "sides"





### OLD


library(plotROC)
p.test <- ggplot( data=trpt$df_PAV %>%
                    filter(PAV==TRUE) %>%
                    as_tibble() ) +
  geom_roc(aes(d=y, m=forecast_value, color=forecast_name), n.cuts = 30, labelround=2) +
  #  facet_wrap(~variable)
  xlab("False alarm rate") +
  ylab("Hit rate") +
  theme(legend.position = "bottom")

ggsave(paste0("./applications/plots/Test_ROCMurphyEquiv.pdf"), p.test, width = 50, height = 50, units = "cm")



set.seed(1)
n <- 50000
p1 <- rbeta(n,5,1)
p2 <- rbeta(n,1,5)


CEP <- 0.4*p1+0.6*p2

rlz <- rbinom(n, 1,CEP)
df.sim <- data.frame(p1=p1, p2=p2, CEP=CEP, y=rlz)


trpt.sim <- triptych(df.sim)
autoplot(trpt.sim, Murphy.scoretype = "DSC", Murphy.benchmark = "p1")

ggplot( data=trpt.sim$df_PAV %>%
          filter(PAV==TRUE & forecast_name!="CEP") %>%
          as_tibble()) +
  geom_roc(aes(d=y, m=forecast_value, color=forecast_name), n.cuts = 20, labelround=2) +
  #  facet_wrap(~variable)
  xlab("False alarm rate") +
  ylab("Hit rate") +
  theme(legend.position = "bottom")

trpt.sim$roc

roc_res <- pROC::roc(response = trpt.sim$df_PAV %>% filter(PAV==TRUE & forecast_name=="p1") %>% pull(y),
                     predictor = trpt.sim$df_PAV %>% filter(PAV==TRUE & forecast_name=="p1") %>% pull(forecast_value),
                     quiet=TRUE)

df.roc <- data.frame(roc_res$thresholds, roc_res$sensitivities, roc_res$specificities)


