library(tidyverse)
library(patchwork)

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

dim(df_RunExmpl)

# Figure 1 Triptych
trpt_RunExmpl <- triptych(df_RunExmpl)
p_RunExmpl <- autoplot(trpt_RunExmpl,
                       Murphy_scoretype = "score",
                       RelDiag_breaks=seq(0,1,length.out=11),
                       plot_cols = gg_color_hue(4)[c(2,1,3,4)],
                       plot_linetypes="solid")

p_RunExmpl

ggsave(paste0("plots/Fig01_triptych_C1Flares.pdf"),
       p_RunExmpl,
       width=24, height=10.5, units="cm")




# Table 1: Brier scores, Log scores and misclassication rates  ################
MR_score <- function(y,x){as.numeric(x<0.5 & y==1) + as.numeric(x>0.5 & y==0) + 0.5*as.numeric(x==0.5)}

df_RunExmpl %>%
  reshape2::melt(id="y") %>%
  group_by(variable) %>%
  summarize(Brier_score = mean((y-value)^2),
            log_score = mean(log_score(y, value)),
            MR_score = mean(MR_score(y, value))) %>%
  arrange(Brier_score)




# Table 2: Score decompositions        #########################################
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

