# Replication package for "Evaluating Probabilistic Classifiers: The Triptych"

Timo Dimitriadis and Alexander I. Jordan

## Overview & contents

The code in this replication material generates the 12 figures and 2 tables for
the paper "Evaluating Probabilistic Classifiers: The Triptych".
Each figure is generated separately by its corresponding script file
`Figure[xx]*.R`.

The main contents of the repository are the following:

- `R/`: folder of R code containing the `triptych' functions
- `plots/`: folder of generated plots as PDF files
- `data-raw/`: folder of raw data files and the functions are processing them
- `data/`: folder of processed data files
- `Figure[xx]*.R`: R scripts to create the respective figures


The content of Tables 1 and 2 is printed in the console by running
`Figure_01_Triptych_C1Flares.R` and the content of Table 3 is obtained by running
`Figure_10_Triptych_SPF.R`.


## Data availability and provenance

### Solar Flare Forecasts

The solar flare forecasts (for the classes C1.0+ and M1.0+) are obtained from
https://doi.org/10.7910/DVN/HYP74O. They are then processed with the file
`data-raw/prepare_SolarFlares.R` and then saved in the `data/` folder.


### SPF Forecasts for Economic Recessions

The already processed data file can be downloaded under https://github.com/TimoDimi/replication_DGJ20/tree/master/data.
We refer to the files in https://github.com/TimoDimi/replication_DGJ20/tree/master/data-raw/data_SPF
for a description of the processing.


### Fragile Family Challenge

The Fragile Family Challenge (FFC) is a scientific mass collaboration where 160
teams built predictions for six variables, where we analyze two binary ones (evition and
job training). The forecasts (submissions) of the 160 teams together with the
realizations can be downloaded under
https://github.com/atkindel/ffc_replication/tree/master/data/derived/submissions.csv.zip.
The 9 benchmark forecasts have to be generated separately by obtaining the data files from
https://opr.princeton.edu/archive/ as described under https://github.com/atkindel/ffc_replication.
We prepare the FFC data using these two (in this repository unavailable) files within the
script `prepare_FragileFamilyChallenge.R` and save the resulting files as
`data/FFC_Eviction.rda` and `data/FFC_JobTraining.rda`, both of which are available
in this repository.

## Instructions & computational requirements.

The analysis files `Figure[xx]*.R` can be run individually, in any order. Set the working
directory to the root of the replication package, or open the `.Rproj` file
using RStudio.

The software versions that were used to run these analyses are

- R 4.2.2
  - `geomtextpath` (0.1.1)
  - `ggrepel` (0.9.2)
  - `grid` (4.2.2)
  - `murphydiagram` (0.12.2)
  - `lubridate` (1.9.0)
  - `patchwork` (1.1.2)
  - `pROC` (1.18.0)
  - `readr` (2.1.3)
  - `reliabilitydiag` (0.2.1)
  - `reshape2` (1.4.4)
  - `tidyverse` (1.3.2)

