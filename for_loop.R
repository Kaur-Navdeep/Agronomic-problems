rm(list = ls())
setwd("~/Desktop/Agronomic-problems/Agronomic-problems")
library(tidyverse)
data1 = read.csv("Grain_N_rate_trial_harvest_data_2021.csv")
data1$Block= as.factor(data1$Block)
data1$Variety = as.factor(data1$Variety)
data1$N_rates = as.factor(data1$N_rates)
1:ncol(data1)
data1$Plant_height = log(data1$Plant_height)
data1$Seed_yield = log(data1$Seed_yield)
data1$TS_1 = log(data1$TS_1)
data1$TS_2 = log(data1$TS_2)
data1$TP_1= log(data1$TP_1)
table(data1$N_rates)
plot = list()
model1= list()
HSD = list()
Shapiro = list()
MSerror = list()
library(agricolae)
sink("Anova_Results.doc")
for(i in 5:ncol(data1)){
  column = names(data1[i])
  model1 = lm((data1[,i]~ data1$N_rates* Variety + Block), data = data1)
  anova = anova(model1)
  Shapiro = shapiro.test(model1$resid)
  print(column)
  print(anova)
  print(Shapiro)
  print(HSD)
}
sink()
summary = list()
  ## model using aov   
for(i in 5:ncol(data1)){
       column = names(data1[i])
       anova = aov((data1[,i]~ data1$N_rates* Variety + Block), data = data1)
       summary = summary(anova)
       Shapiro = shapiro.test(anova$resid)
       HSD = HSD.test (anova, trt = c("N_rates", "Variety"), group = TRUE, console=TRUE)
       print(column)
       print(anova)
       print(Shapiro)
       print(HSD)
}
anova = list()
## 
for(i in 5:ncol(data1)){
  column = names(data1[i])
  anova= aov(data1[,i]~ data1[,i]$N_rates + data1[,i]Variety + data1[,i]Block)
  Shapiro = shapiro.test(model1$resid)
  HSD = HSD.test (model1, trt = c("N_rates", "Variety"), group = TRUE, console=TRUE)
  print(column)
  print(anova)
  print(Shapiro)
  print(HSD)
}
## Extra
data$
  HSD = HSD.test (model1, trt = c("N_rates"), group = TRUE, console=TRUE)

df = df.residual(model1)
MSerror = deviance(model1)/df
with(data1,HSD.test (data1[i], N_rates, df, MSerror, group = TRUE, console=TRUE)    