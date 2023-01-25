library(readr)
library(dplyr)
library(tidyr)

# The file `submissions.csv` is NOT included in the replication material.
# Download file "data/derived/submissions.csv.zip" of
# Salganik et al (2020, https://doi.org/10.7910/DVN/CXSECU)
# alternative source:
#   https://github.com/atkindel/ffc_replication/tree/9edd6d2b0537ba3074e2532eb54b85792bc4ae25/data/derived/submissions.csv.zip
FFC.pred <- readr::read_csv("data-raw/submissions.csv")

# The file `benchmarks_long.csv` is NOT included in the replication material.
# This file must be generated through the replication material of Salganik et al (2020)
# (alternative:
#    https://github.com/atkindel/ffc_replication/tree/9edd6d2b0537ba3074e2532eb54b85792bc4ae25
# )
# by using the data in the 'private' folder, which can be obtained through
# an online registration through the Office of Population Research Data Archive
# at Princeton University: https://opr.princeton.edu/archive/.
# For details on this, we refer to Salganik et al (2020)
FFC.benchmarks <- readr::read_csv("data-raw/benchmarks_long.csv")

# Combine both data sets
FFC.pred.full <- rbind(
  FFC.pred,
  FFC.benchmarks
)

# Job training data
FFC.jobtrain.full <- FFC.pred.full %>%
  filter(outcome == "jobTraining") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1, prediction))) %>%
  pivot_wider(names_from = account, values_from = prediction) %>%
  select(-challengeID) %>%
  rename(y = truth) %>%
  na.omit()

save(FFC.jobtrain.full, file = "data/FFC_JobTraining.rda")

# Eviction data
FFC.evict.full <- FFC.pred.full %>%
  filter(outcome == "eviction") %>%
  select(challengeID, account, truth, prediction) %>%
  mutate(prediction = pmax(0, pmin(1, prediction))) %>%
  pivot_wider(names_from = account, values_from = prediction) %>%
  select(-challengeID) %>%
  rename(y = truth) %>%
  na.omit()

save(FFC.evict.full, file = "data/FFC_Eviction.rda")
