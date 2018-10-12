# Air-Quality-Modeling
R Functions for processing Climo data.

Contact: Shrikanth Yadav yshrikanth@iisc.ac.in
## Description
This repository contains functions for working on Climo Data that was collected outside the Robert Bosch Center, Indian Institute of Science.

There are two main data sets used
- August dataset: Carbon Monoxide concentration that is used only for working on weekly seasonality.
- September dataset: CO, wind direction and wind speed data.  

## Organisation
- The file `AQM Functions Summary.md` is a tutorial for the data processing functions.  Use-cases are presented when necessary.
- The file `main.md` presents all the plots of the analysis on the sample data set.  To perform the analysis on your own data set, you will have to run the `main.Rmd` using `R`.
- The `Raw Sensor Data` directory contains the main datasets.
- The `TSA Cache Data` directory functions as cache. Computations like seasonality and downsampling performed in the main function for unique averaging times will be saved here for future reference--to prevent recomputation.  There are two types of .csv files-
    - The master data frames over which we perform the analysis. The `xx` in `master_sep_xx.csv` represents the averaging time in munitues.
    - The sesonality computed from the August dataset.  The `xx` in `sea_CO_aug_xx.csv` represents the averaging time in munitues.
- The `plots` directory contains folders based on the plotting function name.  Each subdirectory will store the respective plots in PNG and PDF formats. 

### Function description
- The `TSA Data Source.R` contains functions that work on the cache data. 
- The `src` directory contains the 2 classes of source files
    - `TSA Source.R` contains the data prcessing functions.
    - `plotting_functions.R` contains the plotting functions.

## Required

- library(png)
- library(grid)
- library(quantreg)
- library(TTR)
- library(stats)
- library(lubridate)
- library(timeSeries)
- library(dplyr)
- library(ggplot2)
- library("gridExtra")
- library("cowplot")
- library(reshape2)
- library(scales)
- library(tidyverse)

