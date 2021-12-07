

### Week 4 and 5 ###
rm(list = ls())
library(tidyverse)
library(agricolae)
library(ggplot2)
library(ggpubr)

setwd("~/Desktop/Agronomic-problems")
### For harvest data analysis ##
harvest <- read.csv("Grain_N_rate_trial_harvest_data_2021.csv") ## opening file 
 ## converting N rates into numeric values for calculation 

# converting variables into factors factors ####
harvest$block <-  as.factor(harvest$block)
harvest$variety <-  as.factor(harvest$variety)
str(harvest$variety)
harvest$trt_fac <- as.factor(harvest$trt)
harvest$plot_no.<- as.factor(harvest$plot_no.)


# filtering data according to two varieties ####
bia <-  filter(harvest, harvest$variety == "Bialobrzeskie")
x59 <-  filter(harvest, harvest$variety == "X-59")

# grain yield visualization based on varieties separately ####
      
boxplot(bia$grain_yield ~ bia$trt_fac)   ## high variability in data 
hist(bia$grain_yield)      ## histogram does not look normal due to few data points


boxplot(x59$grain_yield ~ x59$trt)
hist(x59$grain_yield)    ## histogram of X-59 does not look normally distibuted

## for loop to check normal distribution of data ####

mydata <- harvest%>%select(-16,)
str(mydata) 
table(mydata$trt)
1:ncol(mydata)
boxplot <- list()
histogram <- list()
plot <- list()

for (i in 6:ncol (mydata)){
  column <- names(mydata[i])
  boxplot <- boxplot(mydata[,i] ~ mydata$trt, ylab = column, main = column) 
  histogram <- hist(mydata[,i], ylab = column, main = column) 
}

## based on graphical visualization, grain yield, plant height and grain yield per plant does not seem to be normally distributed. 

# grouping data according to N rates and then get average yield in all N rates using summarise command ####
averyield <-  group_by(harvest, trt); averyield
summary_yield <-  summarise(averyield, average_yield = mean(grain_yield), average_grain_yield_per_plant = mean(grain_yield_per_plant), na.rm = TRUE);summary_yield


## grouping yield data of Bialobrezeskie and average yield as per N rates ####
biagroup <-  group_by(bia, trt); biagroup
bia_yield <-  summarise(biagroup, average_yield = mean(grain_yield, na.rm = TRUE)); bia_yield



## grouping yield data of X-59 and average yield as per N rates ####
x59group <-  group_by(x59, trt); x59group
x59_yield <-  summarise(x59group, average_yield = mean(grain_yield, na.rm = TRUE)); x59_yield

options(scipen = 999)

## MRTN Bialobrezeskie using averged data, taking N price as $2.83/Kg and Hemp grain price as $550/Kg ##
mrtn_bia <- mutate(bia_yield,
                   fertilizer_price = trt * 2.83, mrtn = ((average_yield- 53.3375)*550) - (fertilizer_price))
mrtn_bia
## MRTN X-59 using averaged data, taking N price as $2.83/Kg and Hemp grain price as $550/Kg

mrtn_x59 <- mutate(x59_yield,
                   fertilizer_price = x59_yield$trt*2.83, mrtn = ((x59_yield$average_yield- 39.6600)*550) - (x59_yield$trt*2.83))
mrtn_x59

grouped_data <-  group_by(harvest, trt)
## average data ####
average <-  summarise(grouped_data,  no_of_plants_average = mean(no_of_plants), biomass_yield_average = mean(biomass_yield), aboveground_residue_wt_average = mean(aboveground_residue_wt), plant_ht = mean(plant_ht), grain_yield_average = mean(grain_yield), na.rm = TRUE)
average 
## Standard deviation ####
sd <-  summarise(grouped_data,  no_of_plants_sd = sd(no_of_plants), biomass_yield_sd = sd(biomass_yield), aboveground_plant_residue_weight_sd = sd(aboveground_residue_wt), plant_ht_sd = sd(plant_ht),grain_yield_sd = sd(grain_yield), na.rm = TRUE)
sd

# data visualization using ggplot week 6 and 7 ####

ggplot(data = harvest, aes(x = trt, y = grain_yield, color = variety))+ 
  geom_point() +
  geom_smooth(formula=(y~x + x^2), position = "identity", stat = "smooth", se=FALSE, method = "loess") +
  theme_linedraw() + labs (x = "N rates (kg/ha)", y = "Grain yield (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))## loess is locally weighted least squares regression, it uses more local data to estimate our Y variable

ggplot(harvest, aes(x = trt_fac, y = plant_ht, color = variety) ) + 
  geom_violin() + labs (x = "N rates (kg/ha)", y = "Plant height (cm)") + theme_linedraw()

ggplot(data = harvest)+ 
  geom_point(mapping = aes(x = trt_fac, y = grain_yield_per_plant, shape = variety))+
  labs (x = "N rates (kg/ha)", y = "Grain yield per plant (grams)") + theme_linedraw()

ggplot(data = harvest)+ 
  geom_point(mapping = aes(x = trt_fac, y = N)) +
  facet_wrap(~variety, nrow=2, ncol = 2)+ 
  labs (x = "N rates (kg/ha)", y = "Percent N in leaves") + theme_linedraw()

ggplot(data = harvest )+   
  geom_point(mapping = aes(x = trt_fac, y = biomass_yield))+
  facet_wrap(~variety, nrow=1, ncol = 2)+
  labs (x = "N rates (kg/ha)", y = "Dry biomass yield (kg/ha)") + theme_linedraw()
  

ggplot (data = harvest) +
  stat_summary(
    mapping = aes (x = trt_fac, y = root_wt, color = variety),
    fun.max = max,
    fun.min = min,
    fun = median
  ) +
  facet_wrap(~variety, nrow=1)+  labs (x = "N rates (kg/ha)", y = "Roots dry weight (kg/ha)") + theme_linedraw() 


ggplot (data = harvest) +
  stat_summary(
    mapping = aes (x= trt, y = S, color = variety),
    fun.max = max,
    fun.min = min,
    fun = median
  )  +
  facet_wrap(~variety, nrow=1)+  labs (x = "Percent sulphur in leaves", y = "Dry biomass yield (kg/ha)") + theme_linedraw() +
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))


ggplot(data = harvest)+ 
  geom_boxplot(mapping = aes(x = trt_fac, y = K), stat = "boxplot")+
  facet_wrap(~variety, nrow=1)+
  theme_bw() + labs (x = "N rates (kg/ha)", y = "Percent K in leaves")


## week 8 statistics

##  regression analysis of grain yield ####
grainyield <- harvest%>%select(grain_yield, biomass_yield, plant_ht, root_wt, no_of_plants, trt)

pairs(grainyield)

y <- harvest$grain_yield
x1 <- harvest$biomass_yield
x2 <- harvest$plant_ht
x3 <- harvest$root_wt
x4 <- harvest$no_of_plants
x5 <- harvest$trt

fit1 <- lm(y ~ x1)
fit2 <- lm(y ~ x1 + x2)
fit3 <- lm(y ~ x1 + x2 + x3)
fit4 <- lm(y ~ x1 + x2 + x3 + x4 + x5)

summary(fit1)
summary(fit2)
summary (fit3)
summary(fit4)

## best adjusted R2 is for fit 3 and fit 4

anova(fit3, fit4) 

## there is no significant difference between these two models, there we can consider reduced model i.e. fit3

anova (fit2, fit4)

## there is no significant difference between these two models, there we can consider reduced model i.e. fit2

anova (fit1, fit2)

## there is difference between models, model fit2 is best and explains 

backAIC <- step(fit4, direction = "backward")

## based on backward selection model, fit 2 is best



# ANOVA analysis ####
# anova grain yield without transformation (here data does not follow assumptions of ANOVA #####################

anova_gy <-  aov (grain_yield ~ trt_fac*variety + block, data = harvest)
summary(anova_gy)
shapiro.test(anova_gy$resid)
plot(anova_gy)


# grian yield ####
## log transformation of grain yield ####

harvest$grain_yield_log = log(harvest$grain_yield)
str(harvest$grain_yield_log)


## anova of log transformed grain yield #####

anova_gy_log <-  aov (grain_yield_log ~ trt_fac*variety + block, data = harvest)
summary(anova_gy_log)
shapiro.test(anova_gy_log$resid)

plot(anova_gy_log)  ## plots look good with some outliers, which could be removed by two year data

hsdgrainyield <- HSD.test (anova_gy_log, "trt_fac")
hsdgrainyield

mydata1 = harvest%>%select(trt_fac, variety, grain_yield) 

A <-  mydata1 %>%
  group_by( trt_fac) %>%
  summarize(mean = mean(grain_yield), sd = sd(grain_yield), se = sd/sqrt(8))

a <-  ggplot(A, aes(y = mean, x = trt_fac, fill = trt_fac)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
  theme_bw() + labs(y = "Grain yield (kg/ha)", x = "N rates (kg/ha)")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "Races")+
  annotate("text", x = 1, y = 65,label = "b", size = 4)+
  annotate("text",x = 2, y = 190, label = "a", size = 4)+
  annotate("text",x = 3, y = 255, label = "a", size = 4)+
  annotate("text",x = 4, y = 320,label = "a", size = 4)+
  annotate("text",x = 5, y = 310,label = "a", size = 4)+
  annotate("text",x = 6, y = 305,label ="a", size = 4)+
  theme(legend.position = "none") 
a


ggsave("Grain_yield.png", a, dpi=600, width = 8, height = 8, units = "cm" )
## test for normality



## anova grain yield per plant ####
anova_pp <-  aov (grain_yield_per_plant ~ trt_fac*variety + block, data = harvest);
summary(anova_pp)
shapiro.test(anova_pp$resid)  

plot(anova_pp)  # based on shapiro and graphs data does not follow assumptions of ANOVA

### data not follow assumptions of ANOVA

## log transformation of grain yield per plant and ANOVA ####
harvest$grain_yield_per_plant_log = log(harvest$grain_yield_per_plant + 1)   ## log transformation of data 

anova_pp_log <-  aov (grain_yield_per_plant_log ~ trt_fac*variety + block, data = harvest); 
summary(anova_pp_log)
shapiro.test(anova_pp_log$resid)
plot(anova_pp_log)

hsd_pp <- HSD.test(anova_pp_log, "trt_fac"); hsd_pp
#### results indicate there is no interaction between grain yield per plant and variety thus results are presented with N rates


mydata = harvest%>%select(trt_fac, variety, grain_yield_per_plant) 

D <-  mydata %>%
  group_by( trt_fac) %>%
  summarize(mean = mean(grain_yield_per_plant), sd = sd(grain_yield_per_plant), se = sd/sqrt(4))

b <-  ggplot(D, aes(y = mean, x = trt_fac, fill = trt_fac)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
theme_bw() + labs(y = "Grain yield per plant (grams)", x = "N rates (kg/ha)")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "Races")+
  annotate("text", x = 1, y = 0.75,label = "b", size = 4)+
  annotate("text",x = 2, y = 2.7, label = "a", size = 4)+
  annotate("text",x = 3, y = 3.2, label = "a", size = 4)+
  annotate("text",x = 4, y = 4.1,label = "a", size = 4)+
  annotate("text",x = 5, y = 4,label = "a", size = 4)+
  annotate("text",x = 6, y = 5.5,label ="a", size = 4) +
  theme(legend.position = "none") 
b


# plant height ####
## regression of plant height ####

fitplantheight <-  lm(log(plant_ht) ~ trt, data = harvest)
summary(fitplantheight)

## ANOVA plant height ####
anova_ph <-  aov (plant_ht ~ trt_fac*variety+ block, data = harvest); 
summary(anova_ph)
shapiro.test(anova_ph$residuals) ## data does not follow assumptions of ANOVA
par(mfrow=c(2,2)) 
plot(anova_ph)
## log transformation of plant height ####

harvest$plant_ht_log = log(harvest$plant_ht)


## anova of log transformed plant height ####

anova_plht <-  aov (plant_ht_log ~ trt_fac*variety+ block, data = harvest); 
summary(anova_plht)
shapiro.test(anova_plht$residuals)
h = plot(anova_plht); h

## HSD using nitrogen rates as factor ####

hsd_plant_height <- HSD.test (anova_plht, trt = c("trt_fac"))
hsd_plant_height

mydata2 = harvest%>%select(trt_fac, variety, plant_ht) 

D <-  mydata2 %>%
  group_by( trt_fac) %>%
  summarize(mean = mean(plant_ht), sd = sd(plant_ht), se = sd/sqrt(8))

b <-  ggplot(D, aes(y = mean, x = trt_fac, fill = trt_fac)) + 
  geom_bar(position = "dodge", stat = "identity", color="black", width = 0.8) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
  theme_bw() + labs(y = "Plant height (cm)", x = "N rates (kg/ha)")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "Races")+
  annotate("text", x = 1, y = 37,label = "b", size = 4)+
  annotate("text",x = 2, y = 69, label = "a", size = 4)+
  annotate("text",x = 3, y = 70, label = "a", size = 4)+
  annotate("text",x = 4, y = 102,label = "a", size = 4)+
  annotate("text",x = 5, y = 74,label = "a", size = 4)+
  annotate("text",x = 6, y = 74,label ="a", size = 4) +
  theme(legend.position = "none") 
b

ggsave("Plant_height_n_rates.png", b, dpi=600, width = 8, height = 8, units = "cm" )


## HSD using varieties as factor ####

hsd_plant_height2 <-  HSD.test(anova_plht, trt = c( "variety")); hsd_plant_height2

mydata2 = harvest%>%select(trt_fac, variety, plant_ht) 

C <-  mydata2 %>%
  group_by( variety) %>%
  summarize(mean = mean(plant_ht), sd = sd(plant_ht), se = sd/sqrt(8))

c <-  ggplot(C, aes(y = mean, x = variety, fill = variety)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.8, color = "black") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
  theme_bw() + labs(y = "Plant height (cm)", x = "Varieties")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "Races")+
  annotate("text", x = 1, y = 95,label = "a", size = 4)+
  annotate("text",x = 2, y = 45, label = "b", size = 4)+
  theme(legend.position = "none") 
c
  
ggsave("Plant_height_variety.png", c, dpi=600, width = 8, height = 8, units = "cm" )


## Biomass analysis
fitbiomass <-  lm(biomass_yield ~ trt, data = harvest)
summary(fitbiomass) ## p value is significant 


anova_biomass <- aov(biomass_yield ~ trt_fac*variety + block, data = harvest)
summary(anova_biomass)
shapiro.test(anova_biomass$resid)  #### data follows normal distribution
par(mfrow=c(2,2)) 
plot(fitbiomass)
HSDbiomass<- HSD.test (anova_biomass, "trt_fac")
HSDbiomass

shapiro.test(anova_biomass$resid)
biomass <-  as.data.frame(HSDbiomass$means); biomass
biomass$trt_fac <- c("0","112","168", "224", "280", "56")
biomass

biomass$trt_fac <- factor(biomass$trt_fac, levels = c("0","56","112","168","224","280"))

library(ggplot2)

f <- ggplot(biomass, aes(x = trt_fac, y = biomass_yield  , fill = trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black", 
           width = 0.8)+
  labs(y = " Dry biomass yield (kg/ha)", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = biomass_yield - ((std)/sqrt(r)), ymax = biomass_yield + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light()+ 
  geom_text(data=biomass,
            aes( y = biomass_yield + ((std)/sqrt(r)) + 10, label=c("b","a","a","ab","ab","ab")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size = 9,face= "bold" ),
        axis.title=element_text(size = 9,face= "bold"))+
  scale_y_continuous(breaks = seq(0, 1500, 200))
f



ggsave("biomass_yield.png", f, dpi=600, width = 8, height = 8, units = "cm" )

# regression analysis of plant nutrient concentration with nitrogen applied ####

nitrogen = lm (N ~ trt, data = harvest)
summary(nitrogen)

nitrogen2 = lm (N ~ trt + I(trt^2), data = harvest)
summary(nitrogen2)  ## nitrogen application has not shown impact on nitrogen concentration in plant

A = ggplot(harvest, aes(trt, N)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = T)+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 2, raw = TRUE)) +
  theme_bw()+ ylim(c(0,4))+
  labs(y = "N concentration (%)", x = "N rates (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))


sulphur = lm (S ~ trt + variety +trt:variety, data = harvest)
summary(sulphur)  ## linear equation fits better 

sulphur2 = lm(S ~ trt + I(trt^2), data = harvest)
summary(sulphur2)

B = ggplot(harvest, aes(trt, S)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = T)+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 2, raw = TRUE)) +
  theme_bw()+ ylim(c(0,0.45))+
  labs(y = "S concentration (%)", x = "N rates (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))

phosphorus = lm(P ~ trt + variety, data = harvest)
summary(phosphorus)

phosphorus2 = lm(P~ trt + I(trt^2), data = harvest)
summary(phosphorus2) ## quadratic equation fits better

C = ggplot(harvest, aes(trt, P)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = T)+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 2, raw = TRUE)) +
  theme_bw()+ ylim(c(0,2))+
  labs(y = "P concentration (%)", x = "N rates (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))

potassium = lm(K ~ trt + variety + trt:variety , data = harvest)
summary(potassium) 

potassium2 = lm(K ~ trt+ I(trt^2), data = harvest)
summary(potassium2) ## quadratic equation fits better

D = ggplot(harvest, aes(trt, K)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = T)+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 2, raw = TRUE)) +
  theme_bw()+ ylim(c(0,3.5))+
  labs(y = "K concentration (%)", x = "N rates (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
D

E= ggarrange(A, B, C , D,  
             labels = c("A", "B", "C", "D"),
             ncol = 2, nrow = 2)
E

ggsave("Regression_nutrient_concentration.png", E ,dpi=600, width = 30, height = 20, units = "cm" )










 

