install.packages("tidyverse") 

rm(list = ls())
library(tidyverse)
library(dplyr)

### For harvest data analysis ##
data = read_csv("Grain_N_rate_trial_harvest_data_2021.csv") ## opening file 
data$N_rates_Kg_per_ha = as.integer(data$N_rates_Kg_per_ha) ## converting N rates into integers

 ## converting Replications and varieties as factors
data$Rep = as.factor(data$Rep)
data$Variety = as.factor(data$Variety)

str(data$Seed_yield_kg_per_ha) ## command tells the structure of data, seed yield is in numeric

data%>%select(Variety,N_rates_Kg_per_ha,Rep, Plant_height_cm, Dry_biomass_yield_kg_per_ha, Seed_yield_kg_per_ha) ## to select specific columns

## filtering data according to two varieties
A = filter(data,  Variety == "Bialobrzeskie")
B = filter(data, Variety == "X-59")

## grouping data according to N rates and then get average yield in all N rates using summarise command 
averyield = group_by(data, N_rates_Kg_per_ha ); averyield
TotalDataSummarybyYield = summarise(averyield, average_yield = mean(Seed_yield_kg_per_ha, na.rm = TRUE))

# grouping yield data of Bialobrezeskie and average yield as per N rates 
Bialobrzeskiegroup = group_by(A, N_rates_Kg_per_ha ); Bialobrzeskiegroup
Bialobrzeskieaverageyield = summarise(Bialobrzeskiegroup, average_yield = mean(Seed_yield_kg_per_ha, na.rm = TRUE))

# grouping yield data of X-59 and average yield as per N rates 
X59group = group_by(B, N_rates_Kg_per_ha ); X59group
X59averageyield = summarise(X59group, average_yield = mean(Seed_yield_kg_per_ha, na.rm = TRUE))
options(scipen = 999)

## MRTN Bialobrezeskie using averged data, taking N price as $2.83/Kg and Hemp seed price as $550/Kg ##
MTRN_Bia <- mutate(Bialobrzeskieaverageyield,
                   Fertilizer_price =X59averageyield$N_rates_Kg_per_ha*2.83, Maximum_return_to_N_Approach = ((Bialobrzeskieaverageyield$average_yield- 53.3375)*550) - (Bialobrzeskieaverageyield$N_rates_Kg_per_ha*2.83))

## MRTN X-59 using averaged data, taking N price as $2.83/Kg and Hemp seed price as $550/Kg

MTRN_X59 <- mutate(X59averageyield,
                   Fertilizer_price = X59averageyield$N_rates_Kg_per_ha*2.83, Maximum_return_to_N_Approach = ((X59averageyield$average_yield- 39.6600)*550) - (X59averageyield$N_rates_Kg_per_ha*2.83))
                   
Grouped_data = group_by(data, N_rates_Kg_per_ha, Variety)
## average data
Average = summarise(Grouped_data,  No_of_plants_average = mean(No_of_plants), Dry_biomass_yield_kg_per_ha_average = mean(Dry_biomass_yield_kg_per_ha), Aboveground_plant_residue_weight_kg_per_ha_average = mean(Aboveground_plant_residue_weight_kg_per_ha), Plant_height_cm_average = mean(Plant_height_cm), Seed_yield_kg_per_ha_average = mean(Seed_yield_kg_per_ha), na.rm = TRUE)

## Standard deviation 
sd = summarise(Grouped_data,  No_of_plants_ha_sd = sd(No_of_plants), Dry_biomass_yield_kg_per_ha_sd = sd(Dry_biomass_yield_kg_per_ha), Aboveground_plant_residue_weight_kg_per_ha_sd = sd(Aboveground_plant_residue_weight_kg_per_ha), Plant_height_cm_sd = sd(Plant_height_cm),Seed_yield_kg_per_ha_sd = sd(Seed_yield_kg_per_ha), na.rm = TRUE)






