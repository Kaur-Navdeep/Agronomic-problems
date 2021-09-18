## For NDVI data ##
rm(list = ls())
library(tidyverse)
library(dplyr)
data = read_csv("Grain_N_trial_NDVI_NDRE_data_2021.csv")
Bialobrzeskie= filter(data,  Variety == "Bialobrzeskie")
X_59 = filter(data, Variety == "X-59")
group_by(data, data$N_rates_Kg_per_ha)
Grouped_data = group_by(data, N_rates_Kg_per_ha, Variety)
## average of NDVI and NDRE data
NDREaverage = summarise(Grouped_data,  NDRE_2021_05_25_average = mean(NDRE_2021_05_25), NDRE_2021_06_09_average = mean(NDRE_2021_06_09), NDRE_2021_06_23_average = mean(NDRE_2021_06_23), NDRE_2021_07_08_average = mean(NDRE_2021_07_08),NDRE_2021_07_19_average = mean(NDRE_2021_07_19), na.rm = TRUE)
NDVIaverage = summarise(Grouped_data,  NDVI_2021_05_25_average = mean(NDVI_2021_05_25), NDVI_2021_06_09_average = mean(NDVI_2021_06_09), NDVI_2021_06_23_average = mean(NDVI_2021_06_23), NDVI_2021_07_08_average = mean(NDVI_2021_07_08),NDVI_2021_07_19_average = mean(NDVI_2021_07_19), na.rm = TRUE)

## Standard deviation of NDVI and NDRE data
NDREsd = summarise(Grouped_data,  NDRE_2021_05_25_sd = sd(NDRE_2021_05_25), NDRE_2021_06_09_sd = sd(NDRE_2021_06_09), NDRE_2021_06_23_sd = sd(NDRE_2021_06_23), NDRE_2021_07_08_sd = sd(NDRE_2021_07_08),NDRE_2021_07_19_sd = sd(NDRE_2021_07_19), na.rm = TRUE)
NDVIsd = summarise(Grouped_data,  NDVI_2021_05_25_sd = sd(NDVI_2021_05_25), NDVI_2021_06_09_sd = sd(NDVI_2021_06_09), NDVI_2021_06_23_sd = sd(NDVI_2021_06_23), NDVI_2021_07_08_sd = sd(NDVI_2021_07_08),NDVI_2021_07_19_sd = sd(NDVI_2021_07_19), na.rm = TRUE)


