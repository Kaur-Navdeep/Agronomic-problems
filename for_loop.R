rm(list = ls())
setwd("~/Desktop/Agronomic-problems/Agronomic-problems")
library(tidyverse)
data1 = read.csv("Grain_N_rate_trial_harvest_data_2021.csv")
data1$Rep = as.factor(data1$Rep)
data1$Variety = as.factor(data1$Variety)
data1$N_rates_Kg_per_ha = as.factor(data1$N_rates_Kg_per_ha)
1:ncol(data1)
data1$Plant_height = log(data1$Plant_height)
data1$Seed_yield = log(data1$Seed_yield)
table(data1$N_rates_Kg_per_ha)
plot = list()
model1= list()
HSD = list()
Shapiro = list()
MSerror = list()
for(i in 5:ncol(data1)){
  column = names(data1[i])
  model1 = lm((data1[,i]~ data1$N_rates_Kg_per_ha*Variety + Rep), data = data1)
  anova = anova(lm((data1[,i]~ data1$N_rates_Kg_per_ha*Variety + Rep), data = data1))
  HSD = HSD.test (anova, trt = c("N_rates_Kg_per_ha","Variety"), group = TRUE, console=TRUE)
  library(agricolae)
  Shapiro = shapiro.test(model1$resid)
  print(column)
  print(anova)
  print(Shapiro)
  print(HSD)
}

data$

 

