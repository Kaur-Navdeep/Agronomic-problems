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
  HSD = HSD.test (model1, "N_rates", group = TRUE, console=TRUE)
  Shapiro = shapiro.test(model1$resid)
  print(column)
  print(anova)
  print(Shapiro)
  print(HSD)
}
sink()

model_seed_yield = lm((Seed_yield~N_rates* Variety + Block), data = data1)
anova_seed_yield = aov((Seed_yield~data1$N_rates* Variety + Block), data = data1); anova_seed_yield
HSD_seed_yield = HSD.test(model, "N_rates"); HSD_seed_yield
dev.off()  

model_seed_yield = lm((Seed_yield~N_rates* Variety + Block), data = data1)
anova_seed_yield = anova((model_seed_yield), data = data1); anova_seed_yield
HSD_seed_yield = HSD.test(model_seed_yield, "N_rates"); HSD_seed_yield
dev.off()  

tky = as.data.frame((HSD_seed_yield$means)); tky
tky$N_rates <- c("0", "112","168", "224", "280", "56")
tky

library(ggplot2)

ggplot(tky, aes(x = N_rates, y = Seed_yield, fill=N_rates))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Yield (Kg/ha)", x = "N rates (Kg/ha)") + ggtitle("Tukey test - dynamite plot")+
  scale_color_manual(values=c("#2ca02c","#d62728"))+
  geom_errorbar(aes(ymin=Seed_yield-((std)/sqrt(4)), ymax=Seed_yield+((std)/sqrt(4))), width=.2,
                position=position_dodge(0.9))+
  theme_light() + ylim(c(0,7)) + 
  geom_text(data=tky,
            aes( y = Seed_yield + ((std)/sqrt(4)) + 0.5, label=c("b","a","a","a","a","a")),vjust=0)


model_plant_height =  (lm((Plant_height~N_rates + Block), data = data1))

model_plant_height = lm(Plant_height ~ N_rates*Variety + Block, data = data1)
anova = aov(model_plant_height); anova
summary(anova(model_plant_height))
plot(model_plant_height,1:3)         ## test for normality
library(agricolae)
HSDdata <- HSD.test (model_plant_height, "N_rates")
HSDdata[["groups"]]
shapiro.test(model_plant_height$resid)



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
  anova = aov(data1[,i]~ data1[,i]$N_rates + data1[,i]Variety + data1[,i]Block)
  Shapiro = shapiro.test(model1$resid)
  HSD = HSD.test (anova, trt = c("N_rates", "Variety"), group = TRUE, console=TRUE)
  print(column)
  print(anova)
  print(Shapiro)
  print(HSD)
}
## Extra
data$
  HSD = HSD.test (model1, trt = "N_rates", group = TRUE, console=TRUE)

df = df.residual(model1)
MSerror = deviance(model1)/df
with(data1,HSD.test (data1[i], N_rates, df, MSerror, group = TRUE, console=TRUE)    