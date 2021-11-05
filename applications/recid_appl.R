
library(triptych)

# Read recid file from the "replication_DGJ" Github repo
load("applications/data/recid.rda")

df.recid <- recid %>%
  dplyr::select(two_year_recid, logitpredprobs, gbmpredprobs, mturkpredprobs, compaspredprobs.linear) %>%
  dplyr::rename(y=two_year_recid,
                Logit=logitpredprobs,
                GBM=gbmpredprobs,
                MTurk=mturkpredprobs,
                COMPAS=compaspredprobs.linear) %>%
  tibble::as_tibble()

trpt.recid <- triptych::triptych(df.recid)


ggsave(paste0("applications/plots/triptych_recid.pdf"),
       autoplot(trpt.recid,
                plot.linetype="solid",
                Murphy.scoretype = "MCB-DSC"),
       width=24, height=10.5, units="cm")




###### Tests:

# ggsave(paste0("applications/plots/triptych_recid_normalizedDSCMurphy.pdf"),
#        autoplot(triptych(df.recid %>% select(y, COMPAS, MTurk)),
#                 plot.linetype="solid", plot.linewidth=0.2, Murphy.scoretype = "DSC"),
#        width=24, height=10, units="cm")
#
# ggsave(paste0("applications/plots/triptych_recid_all.pdf"),
#        autoplot(trpt.recid),
#        width=24, height=10, units="cm")
#
#
# autoplot(triptych(df.recid %>% select(y, COMPAS) %>% mutate(COMPAS2=sqrt(COMPAS))), Murphy.scoretype = "DSC")
#
# # plotROC package
# library(plotROC)
# ggplot(data=df.recid %>% select(y, COMPAS) %>% mutate(COMPAS2=COMPAS^3+5) %>% melt(measure.vars=c("COMPAS", "COMPAS2")) %>% as_tibble())+
#   geom_roc(aes(d=y, m=value, color=variable), n.cuts = 10) +
#   facet_wrap(~variable)
#   xlab("False alarm rate") +
#   ylab("Hit rate") +
#   theme(legend.position = "bottom")

