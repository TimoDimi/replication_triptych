library(tidyverse)

# Prepare C1 SOLAR FLARE FORECASTS ############################################
load("data-raw/SF.FC.C1.rda")

C1_FCnames_all <- c("ASSA", "CLIM120", "DAFFS", "DAFFS-G", "MCEVOL", "MCSTAT", "NICT", "NOAA","SIDC")

# Filter the data for dates where ALL employed forecasts are available
df_C1full <- SF.FC.C1 %>%
  rename("DAFFS-G" = "GDAFFS") %>%
  dplyr::select(c("rlz.C1",C1_FCnames_all)) %>%
  as_tibble() %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

dim(df_C1full)

save(df_C1full, file = "data/C1_flares.rda")



# Prepare M1 SOLAR FLARE FORECASTS ############################################
load("data/SF.FC.M1.rda")

M1_FCnames_all <- c("AMOS", "ASAP", "ASSA", "BOM", "CLIM120", "DAFFS", "DAFFS-G",
                    "MAG4VW", "MAG4VWF", "MAG4W", "MAG4WF", "MCEVOL", "MCSTAT",
                    "MOSWOC", "NICT", "NOAA","SIDC")

# Filter data frame to obtain a unified test sample
df_M1full <- SF.FC.M1 %>%
  rename("DAFFS-G" = "GDAFFS") %>%
  as_tibble() %>%
  dplyr::select(all_of(c("rlz.M1", M1_FCnames_all))) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.M1)

save(df_M1full, file = "data/M1_flares.rda")

