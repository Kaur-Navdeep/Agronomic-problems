library(tidyverse)
library(dplyr)
data = read_csv("Grain_N_rate_trial_harvest_data_20210901.csv")


str(data$Seed_yield_kg_per_ha)
yield = group_by(data,data$Seed_yield_kg_per_ha, data$N_rates_Kg_per_ha, data$Variety)

?group_by
?na.r

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data, mapping = aes(x = data$N_rates_Kg_per_ha, y = data$Seed_yield_kg_per_ha)) +
  geom_point() +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'