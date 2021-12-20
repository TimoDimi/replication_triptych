library(devtools)

# Installation of the package from Github:
install_github("TimoDimi/triptych", auth_token = "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")

library(triptych)


# Data frame
df_precip <- precip_Niamey_2016 %>%
  select(obs, Logistic, EMOS, ENS, EPC) %>%
  rename(y=obs)

# Triptych with all 4 forecasts
trpt_precip <- triptych::triptych(df_precip)
p <- autoplot(trpt_precip,
              plot_linetypes="solid")
p

ggsave(paste0("applications/plots/triptych_precip.pdf"),
       p,
       width=24, height=10.5, units="cm")




# Triptych Research Proposal No ENS
trpt_precip_DFG <- triptych::triptych(df.precip %>% dplyr::select(c(y,Logistic,EMOS,EPC)))

p <- (autoplot(trpt_precip_DFG, plot.type="Murphy", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none")) +
  (autoplot(trpt_precip_DFG, plot.type="ROC", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none")) +
  (autoplot(trpt_precip_DFG, plot.type="ReliabilityDiagram", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none"))

ggsave(paste0("applications/plots/triptych_precip_DFG.pdf"),
       p,
       width=24, height=9, units="cm")





