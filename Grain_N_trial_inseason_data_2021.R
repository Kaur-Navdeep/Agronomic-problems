## For inseason data ##
setwd("~/Desktop/Agronomic-problems/Agronomic-problems")
rm(list = ls())
library(tidyverse)
library(dplyr)
mydata <- read_csv("Grain_N_trial_inseason_data_2021.csv")
mydata%>%select (variety, trt, block, germination_percentage, stand_establishment, plant_ht_1, plant_ht_2, plant_ht_3, plant_ht_4, plant_ht_5, days_50_per_flowering)
filter(mydata,  variety == "Bialobrzeskie")
filter(mydata, variety == "X-59")
str(mydata$no_of_female_plants)

mydata$block <-  as.factor(mydata$block)
mydata$variety <- as.factor(mydata$variety)
######3
mydata$trt_fac <- as.factor(mydata$trt)
mydata$plot_no.<- as.factor(mydata$plot_no.)

A <- subset(mydata, mydata$variety == "X-59")

#######################
A$No_of_female_plants <- as.numeric(A$no_of_female_plants)
A$No_of_male_plants <- as.numeric(A$no_of_male_plants)
## Female to male ratio using mutate command 
mutate(A,
       female_to_male_ratio = no_of_female_plants/no_of_male_plants)

grouped_data <- group_by(mydata, trt_fac, variety)
selected_data <- mydata%>%select(germination_percentage, stand_establishment, plant_ht_1, plant_ht_2, plant_ht_3, plant_ht_4, plant_ht_5, days_50_per_flowering)
## average data
average <- summarise(grouped_data,  germination_percentage_average = mean(germination_percentage), stand_establishment_average = mean(stand_establishment), plant_ht_1_average = mean(plant_ht_1), plant_ht_2_average = mean(plant_ht_2), plant_ht_3_average = mean(plant_ht_3), plant_ht_4_average = mean(plant_ht_4), plant_ht_5_average = mean(plant_ht_5), days_50_per_flowering_average = mean(days_50_per_flowering), na.rm = TRUE)

## Standard deviation 
sd <-  summarise(grouped_data,  germination_percentage_sd = sd(germination_percentage), stand_establishment_sd = sd(stand_establishment), plant_ht_1_sd = sd(plant_ht_1), plant_ht_2_sd = sd(plant_ht_2), plant_ht_3_sd = sd(plant_ht_3), plant_ht_4_sd = sd(plant_ht_4), plant_ht_5_sd = sd(plant_ht_5), days_50_per_flowering_sd = sd(days_50_per_flowering), na.rm = TRUE)
################################

anovas <- aov(sulphur~trt_fac*variety + block, data = mydata)
summary(anovas)

A <- subset(mydata, mydata$variety == "X-59")

LSDsulphur <- with(A,LSD.test(sulphur, trt_fac, 33, 0.4715, console=TRUE))

HSDsulphur <-  with(A,HSD.test(sulphur, trt_fac, 33, 0.4715,console=TRUE))



sulphur = as.data.frame(HSDsulphur$means); sulphur
sulphur$trt_fac <- c("0", "112","168", "224", "280", "56")
sulphur
str(sulphur)

sulphur$trt_fac <- factor(sulphur$trt_fac,levels = c("0","56","112","168","224","280"))

a <- ggplot(sulphur, aes(x = trt_fac, y = sulphur, fill=trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = " Sulphur mg/kg", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = sulphur - ((std)/sqrt(r)), ymax = sulphur + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() + 
  geom_text(data = sulphur,
            aes( x = trt_fac, y = sulphur + ((std)/sqrt(r)) + 0.5, label=c("a","b","b","b","b", "b")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
a











