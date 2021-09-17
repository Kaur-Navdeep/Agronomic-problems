> install.packages("tidyverse") 
library(tidyverse)
library(dplyr)

### For harvest data analysis ##
data = read_csv("Grain_N_rate_trial_harvest_data_20210901.csv")
data%>%select(Variety,N_rates_Kg_per_ha,Rep, Plant_height_cm, Dry_biomass_yield_kg_per_ha, Seed_yield_kg_per_ha)
filter(data,  Variety == "Bialobrzeskie")
filter(data, Variety == "X-59")

## For inseason data ##
library(tidyverse)
library(dplyr)
data2 = read_csv("Grain_N_trial_data_2021_09_10.csv")
data2%>%select(Variety,N_rates_Kg_per_ha,Rep, Germination_percentage, Stand_establishment, Plant_height_cm_2021_05_25, Plant_height_cm_2021_06_09, Plant_height_cm_2021_06_23, Plant_height_cm_2021_07_08, Plant_height_cm_2021_07_19, Days_50_per_flowering)
filter(data2,  Variety == "Bialobrzeskie")
filter(data2, Variety == "X-59")
str(data2$No_of_female_plants)
data2$No_of_female_plants = as.numeric(data2$No_of_female_plants)
data2$No_of_male_plants = as.numeric(data2$No_of_male_plants)
mutate(data2,
       female_to_male_ratio = No_of_female_plants/No_of_male_plants)


## For NDVI data ##
data3 = read_csv("Grain_N_trial_NDVI_NDRE_data_2021_09_10.csv")

