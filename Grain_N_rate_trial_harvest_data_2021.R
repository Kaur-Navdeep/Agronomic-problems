install.packages("tidyverse") 

### Week 4 and 5 ###
rm(list = ls())
library(tidyverse)
setwd("~/Desktop/Agronomic-problems")
### For harvest data analysis ##
data = read_csv("Grain_N_rate_trial_harvest_data_2021.csv") ## opening file 
data$N_rates_Kg_per_ha = as.numeric(data$N_rates_Kg_per_ha) ## converting N rates into numeric values for calculation 
A$N_rates_Kg_per_ha = as.factor(A$N_rates_Kg_per_ha)
## converting Replications and varieties as factors
data$Rep = as.factor(data$Rep)
data$Variety = as.factor(data$Variety)
data$N_rates_Kg_per_ha = as.factor(data$N_rates_Kg_per_ha)
str(data$Seed_yield_kg_per_ha) ## command tells the structure of data, seed yield is in numeric
data$Plot_no.<- as.factor(data$Plot_no.)
data%>%select(Variety,N_rates_Kg_per_ha,Rep, Plant_height_cm, Dry_biomass_yield_kg_per_ha, Seed_yield_kg_per_ha) ## to select specific columns

## filtering data according to two varieties
A = filter(data,  data$Variety == "Bialobrzeskie")
B = filter(data, data$Variety == "X-59")


boxplot(data$Seed_yield_kg_per_ha~data$N_rates_Kg_per_ha)
hist(data$Seed_yield_kg_per_ha)

## grouping data according to N rates and then get average yield in all N rates using summarise command 
averyield = group_by(data, N_rates_Kg_per_ha ); averyield
TotalDataSummarybyYield = summarise(averyield, average_yield = mean(Seed_yield_kg_per_ha, na.rm = TRUE)); TotalDataSummarybyYield
data$Dry_biomass_yield_kg_per_ha[data$Milk>19.9] = NA   
outier = which.max(data$Seed_yield_kg_per_ha, data$N_rates_Kg_per_ha == "168")
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


## data visualization week 6 and 7

ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha, color = Variety)) +
  geom_smooth(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha, color = Variety))
?geom_smooth
ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha, size = Variety)) 
ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha, alpa = Variety)) 
ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha, shape = Variety))   
ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha), color = "red")

ggplot(data = data )+ 
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Seed_yield_kg_per_ha))+
  facet_wrap(~Variety, nrow=2, ncol = 2)+
  ggplot(data = data )+   
  geom_point(mapping = aes(x = N_rates_Kg_per_ha, y = Dry_biomass_yield_kg_per_ha))+
  facet_wrap(~Variety, nrow=2, ncol = 2)+
  facet_wrap(~Variety, nrow = 1, ncol = 2)


ggplot(data = data )+ 
  geom_bar(mapping = aes( x = Seed_yield_kg_per_ha))

ggplot(data=data)+
  geom_histogram(mapping = aes( x = Seed_yield_kg_per_ha))

ggplot (data = data) +
  stat_summary(
    mapping = aes (x= data$N_rates_Kg_per_ha, y= data$Dry_biomass_yield_kg_per_ha, color = Variety),
    fun.max = max,
    fun.min = min,
    fun = median
  ) +
  facet_wrap(~Variety, nrow=1)


ggplot(data = data)+ 
  geom_boxplot(mapping = aes(x = data$N_rates_Kg_per_ha, y = data$Seed_yield_kg_per_ha), stat = "boxplot")+
  facet_wrap(~Variety, nrow=1)+
  theme_bw()
str(data$N_rates_Kg_per_ha)
data$N_rates_Kg_per_ha = as.factor(data$N_rates_Kg_per_ha)




## week 8 statistics
str(data)
##  Seed yield analysis
fit = lm(Seed_yield_kg_per_ha ~ N_rates_Kg_per_ha*Variety + Rep, data = data)
anova = anova (fit); anova
summary(anova(fit))
plot(fit,1:3)         ## test for normality
library(agricolae)
HSDdata <- HSD.test (fit, "N_rates_Kg_per_ha")
HSDdata[["groups"]]
shapiro.test(fit$resid)


data2 <- data %>%
  filter(Plot_no. !=  "107") %>%
  filter(Plot_no. !=  "105") %>%
  filter(Plot_no. !=  "211") 

## analysis after removing outliers
fit2 = lm(Seed_yield_kg_per_ha ~ N_rates_Kg_per_ha*Variety + Rep, data = data2)
anova = anova (fit2); anova
summary(anova(fit2))
plot(fit2,1:3)         ## test for normality
library(agricolae)
HSDdata <- HSD.test (fit2, "N_rates_Kg_per_ha")
HSDdata[["groups"]]
shapiro.test(fit2$resid)


## seed yield per plant
fit1 = lm(data$Seed_yield_per_plant ~ N_rates_Kg_per_ha*Variety + Rep, data = data2)
anova = anova (fit1); anova
summary(anova(fit))

library(agricolae)

HSDdata <- HSD.test (fit, "N_rates_Kg_per_ha")
HSDdata[["groups"]]
shapiro.test(fit$resid)

boxplot(data2$Seed_yield_kg_per_ha~data2$N_rates_Kg_per_ha)
plot(fit1)

## plant height analysis
fitplantheight = lm(Plant_height_cm ~ N_rates_Kg_per_ha*Variety + Rep, data = data)
anova = anova (fitplantheight); anova
summary(anova(fitplantheight))
HSDdata <- HSD.test (fitplantheight, trt = c("N_rates_Kg_per_ha", "Variety"))
HSDdata <- HSD.test (fitplantheight, data$N_rates_Kg_per_ha)
HSDdata[["groups"]]


## Biomass analysis
fitbiomass = lm(data$Dry_biomass_yield_kg_per_ha ~ N_rates_Kg_per_ha*Variety + Rep, data = data)
anova = anova (fitbiomass); anova
summary(anova(fitbiomass))
HSDdata <- HSD.test (fitbiomass, trt = c("N_rates_Kg_per_ha"))
HSDdata[["groups"]]