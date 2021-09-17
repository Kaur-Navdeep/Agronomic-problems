## For inseason data ##
rm(list = ls())
library(tidyverse)
library(dplyr)
data = read_csv("Grain_N_trial_inseason_data_2021.csv")
data%>%select(Variety,N_rates_Kg_per_ha,Rep, Germination_percentage, Stand_establishment, Plant_height_cm_2021_05_25, Plant_height_cm_2021_06_09, Plant_height_cm_2021_06_23, Plant_height_cm_2021_07_08, Plant_height_cm_2021_07_19, Days_50_per_flowering)
filter(data,  Variety == "Bialobrzeskie")
filter(data, Variety == "X-59")
str(data$No_of_female_plants)
data$No_of_female_plants = as.numeric(data$No_of_female_plants)
data$No_of_male_plants = as.numeric(data$No_of_male_plants)
## Female to male ratio using mutate command 
(Female_to_male_ratio <- mutate(data,
                                female_to_male_ratio = No_of_female_plants/No_of_male_plants))
Grouped_data = group_by(data, N_rates_Kg_per_ha, Variety)
Selected_data = data%>%select(Germination_percentage, Stand_establishment, Plant_height_cm_2021_05_25, Plant_height_cm_2021_06_09, Plant_height_cm_2021_06_23, Plant_height_cm_2021_07_08, Plant_height_cm_2021_07_19, Days_50_per_flowering)
## average data
Average = summarise(Grouped_data,  Germination_percentage_average = mean(Germination_percentage), Stand_establishment_average = mean(Stand_establishment), Plant_height_cm_2021_05_25_average = mean(Plant_height_cm_2021_05_25), Plant_height_cm_2021_06_09_average = mean(Plant_height_cm_2021_06_09), Plant_height_cm_2021_06_23_average = mean(Plant_height_cm_2021_06_23), Plant_height_cm_2021_07_08_average = mean(Plant_height_cm_2021_07_08), Plant_height_cm_2021_07_19_average = mean(Plant_height_cm_2021_07_19), Days_50_per_flowering_average = mean(Days_50_per_flowering), na.rm = TRUE)

## Standard deviation 
sd = summarise(Grouped_data,  Germination_percentage_sd = sd(Germination_percentage), Stand_establishment_sd = sd(Stand_establishment), Plant_height_cm_2021_05_25_sd = sd(Plant_height_cm_2021_05_25), Plant_height_cm_2021_06_09_sd = sd(Plant_height_cm_2021_06_09),Plant_height_cm_2021_06_23_sd = sd(Plant_height_cm_2021_06_23), Plant_height_cm_2021_07_08_sd = sd(Plant_height_cm_2021_07_08), Plant_height_cm_2021_07_19_sd = sd(Plant_height_cm_2021_07_19), Days_50_per_flowering_sd = sd(Days_50_per_flowering), na.rm = TRUE)

