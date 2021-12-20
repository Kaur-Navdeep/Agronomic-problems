# Analysis of data collected from sowing to harvesting stage (excludes data collected during harvesting) of hemp (Cannabis sativa L.) in summer 2021 in PSREU, Citra
# Reminder: create and set your working directory!
setwd("~/Desktop/Agronomic-problems")
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(agricolae)
library(ggpubr)
inseason <- read_csv("Grain_N_trial_inseason_data_2021.csv")

# converting data into factors and numeric ####
inseason$block <-  as.factor(inseason$block)
inseason$variety <- as.factor(inseason$variety)
inseason$trt_fac <- as.factor(inseason$trt)
inseason$plot_no <- as.factor(inseason$plot_no)
inseason$days_50_per_flowering <- as.numeric(inseason$days_50_per_flowering)


# filtering data based on varieties ####

bia = filter(inseason,  variety == "Bialobrzeskie")
x59 = filter(inseason, variety == "X-59")

x59$no_of_female_plants <- as.numeric(x59$no_of_female_plants)
x59$no_of_male_plants <- as.numeric(x59$no_of_male_plants)


# Female to male ratio using mutate command #####

sex_ratio <- mutate(x59,
                    female_to_male_ratio = no_of_female_plants/no_of_male_plants)


# anova for sex ratio ####
sex_ratio_anova = aov(female_to_male_ratio ~ trt_fac + block, data = sex_ratio)
summary(sex_ratio_anova)

hsd_sex_ratio <- HSD.test(sex_ratio_anova, "block")
hsd_sex_ratio


# grouping data based on N rates and varieties ####

grouped_data <- group_by(inseason, trt_fac, variety)

# average data across different variables ####
average <- summarise(grouped_data,  germination_percentage_average = mean(sulfur), stand_establishment_average = mean(stand_establishment), plant_ht_1_average = mean(plant_ht_1), plant_ht_2_average = mean(plant_ht_2), plant_ht_3_average = mean(plant_ht_3), plant_ht_4_average = mean(plant_ht_4), plant_ht_5_average = mean(plant_ht_5), days_50_per_flowering_average = mean(days_50_per_flowering), na.rm = TRUE)

# standard deviation across different variables ####
sd <-  summarise(grouped_data,  germination_percentage_sd = sd(germination_percentage), stand_establishment_sd = sd(stand_establishment), plant_ht_1_sd = sd(plant_ht_1), plant_ht_2_sd = sd(plant_ht_2), plant_ht_3_sd = sd(plant_ht_3), plant_ht_4_sd = sd(plant_ht_4), plant_ht_5_sd = sd(plant_ht_5), days_50_per_flowering_sd = sd(days_50_per_flowering), na.rm = TRUE)


# ANOVA for sulfur percentage in leaves ####
anova_s <- aov(sulfur ~ trt_fac*variety + block, data = inseason)
summary(anova_s)
shapiro.test(anova_s$residuals)
par(mfrow = c(2,2))

plot (anova_s) ## data has some outliers, will be removed after two year data
## there is interaction between varieties and N rates therefore HSD is caried out separately for each variety 


HSD_x59 <-  with(x59, HSD.test(x59$sulfur, x59$trt_fac, 33, 0.004715 ,console=TRUE))

HSD_bia <-  with(bia, HSD.test(bia$sulfur, bia$trt_fac, 33, 0.004715,console=TRUE))

mydata = inseason%>%select(trt_fac, variety, sulfur) 

D <-  mydata %>%
  group_by( variety, trt_fac) %>%
  summarize(mean = mean(sulfur), sd = sd(sulfur), se = sd/sqrt(4))

a <-  ggplot(D, aes(fill = trt_fac, y = mean, x = variety)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
  theme_bw() + labs(y = "Sulfur concentration in plants (%)", x = "Varieties")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "N rates")+
  annotate("text",x=0.1,y=0.5,label="a", size=3)+
  annotate("text",x=1.78,y=0.25,label="b", size=3)+
  annotate("text",x=1.92,y=0.22, label="b", size=3)+
  annotate("text",x=2.08,y=0.25,label="b", size=3)+
  annotate("text",x=2.24,y=0.23,label="b", size=3)+
  annotate("text",x=2.39,y=0.22,label="b", size=3)+
  annotate("text",x=0.62,y= 0.25,label="a", size=3)+
  annotate("text",x=0.78,y=0.39,label="a", size=3)+
  annotate("text",x=0.92,y=0.21, label="a", size=3)+
  annotate("text",x=1.08,y=0.23,label="a", size=3)+
  annotate("text",x=1.24,y=0.21,label="a", size=3)+
  annotate("text",x=1.39,y=0.21,label="a", size=3)
a


# ANOVA for potassium percentage in leaves ####

anova_k <- aov(potassium ~ trt_fac*variety + block, data = inseason)
summary(anova_k)
## both nitrogen rates and varieties have impact on potassium percent in leaves
shapiro.test(anova_k$residuals)
par(mfrow = c(2,2))
plot(anova_k)  ## data seems to follow ANOVA assumptions 

## HSD based N rates ####
hsd_k<- HSD.test (anova_k, "trt_fac")
hsd_k


K = as.data.frame(hsd_k$means); K
K$trt_fac <- c("0","112","168", "224", "280", "56")
K

K$trt_fac <- factor(K$trt_fac, levels = c("0","56","112","168","224","280"))


b <- ggplot(K, aes(x = trt_fac, y = potassium, fill= trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Potassium (%) in leaves", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = potassium- ((std)/sqrt(r)), ymax = potassium + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() + ylim(c(0, 3.5)) +
  geom_text(data = K,
            aes( x = trt_fac, y = potassium + ((std)/sqrt(r)) + 0.1, label=c("b","ab","ab","ab","ab", "a")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
b

## HSD based on varieties ####

hsd_kv<- HSD.test (anova_k, "variety")
hsd_kv


Kv = as.data.frame(hsd_kv$means); Kv
Kv$variety <- c("Bialobrzeskie", "X-59")
Kv


c <- ggplot(Kv, aes(x = variety, y = potassium, fill= variety))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Potassium (%) in leaves", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = potassium - ((std)/sqrt(r)), ymax = potassium + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() + ylim(c(0, 3.5)) +
  geom_text(data = Kv,
            aes( x = variety, y = potassium + ((std)/sqrt(r)) + 0.2, label=c("b","a")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
c

# ANOVA for nitrogen percentage in plants ####

anova_n <- aov(nitrogen ~ trt_fac*variety + block, data = inseason)
summary(anova_n)

hsd_n<- HSD.test (anova_n, "trt_fac")
hsd_n

## using N rates as a factor ####
N = as.data.frame(hsd_n$means); N
N$trt_fac <- c("0","112","168", "224", "280", "56")
N

N$trt_fac <- factor(N$trt_fac, levels = c("0","56","112","168","224","280"))


d <- ggplot(N, aes(x = trt_fac, y = nitrogen, fill= trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Nitrogen (%) in leaves", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = nitrogen - ((std)/sqrt(r)), ymax = nitrogen + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light()  +
  geom_text(data = N,
            aes( x = trt_fac, y = nitrogen + ((std)/sqrt(r)) + 0.2, label=c("b","a","a","a","a", "a")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
d


## using varieties as a factor ####

hsd_nv<- HSD.test (anova_n, "variety")
hsd_nv


nv = as.data.frame(hsd_nv$means); nv
nv$variety <- c("Bialobrzeskie", "X-59")
nv


e <- ggplot(nv, aes(x = variety, y = nitrogen, fill= variety))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Nitrogen (%) in leaves", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = nitrogen - ((std)/sqrt(r)), ymax = nitrogen + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() +
  geom_text(data = nv,
            aes( x = variety, y = nitrogen + ((std)/sqrt(r)) + 0.2, label=c("a","b")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
e


# ANOVA for phosphorus percent in leaves

anova_p <- aov(phosphorus ~ trt_fac*variety + block, data = inseason)
summary(anova_p)

hsd_p<- HSD.test (anova_p, "trt_fac")
hsd_p

P = as.data.frame(hsd_p$means); P
P$trt_fac <- c("0","112","168", "224", "280", "56")
P

P$trt_fac <- factor(P$trt_fac, levels = c("0","56","112","168","224","280"))


f <- ggplot(P, aes(x = trt_fac, y = phosphorus, fill= trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Phosphorus (%) in leaves", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = phosphorus - ((std)/sqrt(r)), ymax = phosphorus + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light()  +
  geom_text(data = P,
            aes( x = trt_fac, y = phosphorus + ((std)/sqrt(r)) + 0.01, label=c("a","ab","abc","abc","c","abc" )),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
f






