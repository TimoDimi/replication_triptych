# This file generates the data file data/FFC_predictions.rds from the replication material of the FFC PNAS Paper

# File downloaded from https://github.com/atkindel/ffc_replication/blob/master/data/submissions.csv.zip
FFC.pred <- readr::read_csv("applications/data-raw/submissions.csv")
# File generated through the replication material on Github and the data which required easy online registration at some Princeton office
FFC.benchmarks <- readr::read_csv("applications/data-raw/benchmarks_long_updated.csv")

FFC.benchmarks %>% filter(outcome=="layoff" & account=="benchmark_logit_full")

FFC.pred.full <- rbind(FFC.pred, FFC.benchmarks)
saveRDS(FFC.pred.full, "applications/data/FFC_predictions.rds")
