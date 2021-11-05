
# Installation of the package from Github:
# creds = git2r::cred_ssh_key("/Users/timodimi/.ssh/id_rsa.pub",
#                             "/Users/timodimi/.ssh/id_rsa")
# devtools::install_git("ssh://git@github.com/TimoDimi/triptych.git", credentials = creds)



library(triptych)

# Construct data frame for the triptych package
df.precip <- precip_Niamey_2016 %>%
  select(obs, Logistic, EMOS, ENS, EPC) %>%
  rename(y=obs)

# Triptych with all 4 forecasts
trpt_precip <- triptych::triptych(df.precip)
ggsave(paste0("applications/plots/triptych_precip.pdf"),
       autoplot(trpt_precip,
                Murphy.scoretype = "MCB-DSC",
                plot.linetypes="solid"),
       width=24, height=10.5, units="cm")


# Triptych Research Proposal No ENS
trpt_precip_DFG <- triptych::triptych(df.precip %>% dplyr::select(c(y,Logistic,EMOS,EPC)))

p <- (autoplot(trpt_precip_DFG, plot.type="Murphy", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none")) +
  (autoplot(trpt_precip_DFG, plot.type="ROC", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none")) +
  (autoplot(trpt_precip_DFG, plot.type="ReliabilityDiagram", Murphy.scoretype = "score", plot.linetypes="solid") + theme(legend.position="none"))

ggsave(paste0("applications/plots/triptych_precip_DFG.pdf"),
       p,
       width=24, height=9, units="cm")





