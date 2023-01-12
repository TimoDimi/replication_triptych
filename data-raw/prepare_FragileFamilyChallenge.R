library(readr)
library(tidyverse)

# This file generates the data file data/FFC_predictions.rds from the replication material of the FFC PNAS Paper

# File downloaded from https://github.com/atkindel/ffc_replication/tree/master/data/derived/submissions.csv.zip
FFC.pred <- readr::read_csv("data-raw/submissions.csv")

# The file benchmarks_long.csv is NOT included in the replication material.
# This file must be generated through the replication material https://github.com/atkindel/ffc_replication
# by using the data in the 'private' folder, which can be obtained through
# an online registration throught the Office of Population Research Data Archive
# at Princeton University: https://opr.princeton.edu/archive/.
# For details on this, we refer to https://github.com/atkindel/ffc_replication.
FFC.benchmarks <- readr::read_csv("data-raw/benchmarks_long.csv")

# Combine both data sets
FFC.pred.full <- rbind(FFC.pred,
                       FFC.benchmarks)


# Job training data
FFC.jobtrain.full <- FFC.pred.full %>%
  filter(outcome=="jobTraining") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

save(FFC.jobtrain.full, file = "data/FFC_JobTraining.rda")



# Eviction data
FFC.evict.full <- FFC.pred.full %>%
  filter(outcome=="eviction") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1,prediction))) %>%
  dcast(challengeID + truth ~ account) %>%
  tibble::as_tibble() %>%
  select(-challengeID) %>%
  rename(y=truth)  %>%
  na.omit()

save(FFC.evict.full, file="data/FFC_Eviction.rda")



