

### Week 4 and 5 ###
rm(list = ls())
library(tidyverse)
library(agricolae)
library(ggplot2)

setwd("~/Desktop/Agronomic-problems/Agronomic-problems")
### For harvest data analysis ##
data <- read_csv("Grain_N_rate_trial_harvest_data_2021.csv") ## opening file 
 ## converting N rates into numeric values for calculation 

# converting replications and varieties as factors ####
data$block <-  as.factor(data$block)
data$trt_num <- as.numeric(data$trt)
data$variety <-  as.factor(data$variety)
str(data$variety)
data$trt_fac <- as.factor(data$trt)
data$plot_no.<- as.factor(data$plot_no.)
class(data$trt)

str(data) 

data%>%select(variety,trt,block, plant_ht, dry_biomass_yield, seed_yield) ## to select specific columns

# filtering data according to two varieties ####
bia <-  filter(data, data$variety == "Bialobrzeskie")
x59 <-  filter(data, data$variety == "X-59")


# data visualization based on varieties separately ####
      
boxplot(bia$seed_yield ~ bia$trt)
hist(bia$seed_yield)


boxplot(x59$seed_yield ~ x59$trt)
hist(x59$seed_yield)

boxplot(data$seed_yield ~ data$trt)
hist(data$seed_yield)

bia_anova <- aov(seed_yield ~ trt + block, data = bia)
summary(bia_anova)
shapiro.test(bia_anova$resid)


hsd_bia <- HSD.test (bia_anova, "trt")
hsd_bia



x59_anova <- aov(seed_yield ~ trt + block, data = x59)
summary(x59_anova)
shapiro.test(x59_anova$resid)


HSDdata <- HSD.test (x59_anova, "trt")
HSDdata



# grouping data according to N rates and then get average yield in all N rates using summarise command ####
averyield <-  group_by(data, trt); averyield
summary_yield <-  summarise(averyield, average_yield = mean(seed_yield, na.rm = TRUE));summary_yield


## grouping yield data of Bialobrezeskie and average yield as per N rates ####
biagroup <-  group_by(bia, trt); biagroup
bia_yield <-  summarise(biagroup, average_yield = mean(seed_yield, na.rm = TRUE)); bia_yield

class(biagroup$trt)


## grouping yield data of X-59 and average yield as per N rates ####
x59group <-  group_by(x59, trt); x59group
x59_yield <-  summarise(x59group, average_yield = mean(seed_yield, na.rm = TRUE)); x59_yield

options(scipen = 999)

## MRTN Bialobrezeskie using averged data, taking N price as $2.83/Kg and Hemp seed price as $550/Kg ##
mrtn_bia <- mutate(bia_yield,
                   fertilizer_price = trt * 2.83, mrtn = ((average_yield- 53.3375)*550) - (trt*2.83))
mrtn_bia
## MRTN X-59 using averaged data, taking N price as $2.83/Kg and Hemp seed price as $550/Kg

mrtn_x59 <- mutate(x59_yield,
                   fertilizer_price = x59_yield$trt*2.83, mrtn = ((x59_yield$average_yield- 39.6600)*550) - (x59_yield$trt*2.83))
mrtn_x59

grouped_data <-  group_by(data, trt, variety)
## average data ####
average <-  summarise(grouped_data,  no_of_plants_average = mean(no_of_plants), dry_biomass_yield_average = mean(dry_biomass_yield), aboveground_residue_wt_average = mean(aboveground_residue_wt), plant_ht = mean(plant_ht), seed_yield_average = mean(seed_yield), na.rm = TRUE)
average 
## Standard deviation ####
sd <-  summarise(grouped_data,  no_of_plants_sd = sd(no_of_plants), dry_biomass_yield_sd = sd(dry_biomass_yield), aboveground_plant_residue_weight_sd = sd(aboveground_residue_wt), plant_ht_sd = sd(plant_ht),seed_yield_sd = sd(seed_yield), na.rm = TRUE)
sd

# data visualization week 6 and 7 ####

ggplot(data = data, aes(x = trt_num, y = seed_yield, color = variety))+ 
  geom_point() +
  geom_smooth(formula=(y~x + x^2), position = "identity", stat = "smooth", se=FALSE, method = "loess") +
  theme_linedraw() + labs (x = "N rates (kg/ha)", y = "Seed yield (kg/ha)")+
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))## loess is locally weighted least squares regression, it uses more local data to estimate our Y variable

ggplot(data, aes(x = trt_fac, y = seed_yield, color = variety) ) + 
  geom_violin() + labs (x = "N rates (kg/ha)", y = "Seed yield (kg/ha)") + theme_linedraw()

ggplot(data = data )+ 
  geom_point(mapping = aes(x = trt_fac, y = seed_yield, shape = variety))+
  labs (x = "N rates (kg/ha)", y = "Seed yield (kg/ha)") + theme_linedraw()

ggplot(data = data )+ 
  geom_point(mapping = aes(x = trt_fac, y = seed_yield)) +
  facet_wrap(~variety, nrow=2, ncol = 2)+ 
  labs (x = "N rates (kg/ha)", y = "Seed yield (kg/ha)") + theme_linedraw()

ggplot(data = data )+   
  geom_point(mapping = aes(x = trt_fac, y = dry_biomass_yield))+
  facet_wrap(~variety, nrow=1, ncol = 2)+
  labs (x = "N rates (kg/ha)", y = "Dry biomass yield (kg/ha)") + theme_linedraw()
  
ggplot(data=data)+
  geom_histogram(mapping = aes( x = seed_yield), binwidth = 28)



ggplot (data = data) +
  stat_summary(
    mapping = aes (x= data$trt_num, y= data$dry_biomass_yield, color = variety),
    fun.max = max,
    fun.min = min,
    fun = median
  ) +
  facet_wrap(~variety, nrow=1)+  labs (x = "N rates (kg/ha)", y = "Seed yield (kg/ha)") + theme_linedraw() +
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))

ggplot (data = data) +
  stat_summary(
    mapping = aes (x= data$trt, y= data$dry_biomass_yield, color = variety),
    fun.max = max,
    fun.min = min,
    fun = median
  )  +
  facet_wrap(~variety, nrow=1)+  labs (x = "N rates (kg/ha)", y = "Dry biomass yield (kg/ha)") + theme_linedraw() +
  scale_x_continuous(breaks = c(0, 56, 112, 168, 224, 280))


ggplot(data = data)+ 
  geom_boxplot(mapping = aes(x = data$trt_fac, y = data$seed_yield), stat = "boxplot")+
  facet_wrap(~variety, nrow=1)+
  theme_bw() + labs (x = "N rates (kg/ha)", y = "Dry biomass yield (kg/ha)")


## week 8 statistics

##  seed yield analysis
fit <-  lm(seed_yield ~ trt, data = data)
fit
summary(fit)


# anova seed yield without transformation (here data does not follow assumptions of ANOVA #####################

anova_sy <-  aov (seed_yield ~ trt_fac*variety + block, data = data)
summary(anova_sy)
shapiro.test(anova_sy$resid)
hsdseed<- HSD.test (anova_sy, "trt_fac")
hsdseed


Gseeds = as.data.frame(hsdseed$means); Gseeds
Gseeds$trt_fac <- c("0","112","168", "224", "280", "56")
Gseeds
str(Gseeds)

Gseeds$trt_fac<-factor(Gseeds$trt_fac,levels = c("0","56","112","168","224","280"))
str(Gseeds)
library(ggplot2)

e <- ggplot(Gseeds, aes(x = trt_fac, y = seed_yield, fill=trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = " Grain yield (kg/ha)", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = seed_yield - ((std)/sqrt(r)), ymax = seed_yield + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() + ylim(c(0, 400)) +
  geom_text(data=Gseeds,
            aes( x = trt_fac, y = seed_yield + ((std)/sqrt(r)) + 10, label=c("b","ab","a","ab","ab", "ab")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
e



ggsave("Grain_yield.png", e, dpi=600, width = 8, height = 8, units = "cm" )






# seed yield ####
## log transformation of seed yield ####

data$seed_yield_log = log(data$seed_yield)
str(data$seed_yield_log)
   
## regression of log transformed seed yield ####   

fit2 <- lm(seed_yield_log ~ trt, data = data)
fit2
summary(fit2)
## anova of log transformed seed yield #####

anova_sy_log <-  aov (seed_yield_log ~ trt_fac*variety + block, data = data)
summary(anova_sy_log)
shapiro.test(anova_sy_log$resid)
hsdseedyield<- HSD.test (anova_sy_log, "trt_fac")
hsdseedyield


log_seed_yield = as.data.frame(hsdseedyield$means); log_seed_yield
log_seed_yield$trt_fac <- c("0","112","168", "224", "280", "56")
log_seed_yield

log_seed_yield$trt_fac <- factor(log_seed_yield$trt_fac, levels = c("0","56","112","168","224","280"))
str(log_seed_yield)
library(ggplot2)

l <- ggplot(log_seed_yield, aes(x = trt_fac, y = seed_yield_log, fill= trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = " log (Grain yield (kg/ha))", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = seed_yield_log - ((std)/sqrt(r)), ymax = seed_yield_log + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light() + ylim(c(0, 6.5)) +
  geom_text(data=log_seed_yield,
            aes( x = trt_fac, y = seed_yield_log + ((std)/sqrt(r)) + 0.2, label=c("b","a","a","a","a", "a")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
l

ggsave("Log_Grain_yield.png", l, dpi=600, width = 8, height = 8, units = "cm" )
## test for normality


# seed yield per plant ####
## regression seed yield per plant ####

fit1 <-  lm(seed_yield_per_plant ~ trt_num, data = data)
summary(fit1)
## anova seed yield per plant ####
anova_pp <-  aov (data$seed_yield_per_plant ~ trt_fac*variety + block, data = data);
summary(anova_pp)
shapiro.test(fit$resid)      ### data not follow assumptions of ANOVA

## log transformation of seed yield per plant and ANOVA ####
data$seed_yield_per_plant_log = log(data$seed_yield_per_plant)   ## log transformation of data 

anova_pp <-  aov (data$seed_yield_per_plant_log ~ trt_fac*variety + block, data = data); 
summary(anova_pp)
shapiro.test(anova_pp$resid)

#### results indicate there is interaction between seed yield per plant and variety 
## HSD by subsetting data based on varieties because of interaction ####

bia$seed_yield_per_plant_log = log(bia$seed_yield_per_plant)
hsd_bia_pp= with(bia,HSD.test(seed_yield_per_plant_log, trt_fac, 33,0.6551,console=TRUE))

x59$seed_yield_per_plant_log = log(x59$seed_yield_per_plant)
hsd_x59_pp= with(x59,HSD.test(seed_yield_per_plant_log, trt_fac, 33,0.6551,console=TRUE))

mydata = data%>%select(trt_fac, variety, seed_yield_per_plant_log) 

D <-  mydata %>%
  group_by( variety, trt_fac) %>%
  summarize(mean = mean(seed_yield_per_plant_log), sd = sd(seed_yield_per_plant_log), se = sd/sqrt(4))

a <-  ggplot(D, aes(fill = trt_fac, y = mean, x = variety)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2,
                position = position_dodge(0.9))+
theme_bw() + labs(y = "Log(Seed yield per plant (grams)", x = "Varieties")+
  scale_fill_viridis_d(option = "D", direction = -1, name = "Races")+
  annotate("text",x=1.62,y=2.9,label="AB", size=3)+
  annotate("text",x=1.78,y=3.55,label="A", size=3)+
  annotate("text",x=1.92,y=3.1, label="AB", size=3)+
  annotate("text",x=2.08,y=1.75,label="B", size=3)+
  annotate("text",x=2.24,y=2.9,label="AB", size=3)+
  annotate("text",x=2.39,y=2.8,label="AB", size=3)
a


# plant height ####
## regression of plant height ####
fitplantheight <-  lm(data$plant_ht ~ trt_num, data = data)
summary(fitplantheight)

## ANOVA plant height ####
anova <-  aov (plant_ht ~ trt_fac*variety+ block, data = data); 
summary(anova)
shapiro.test(anova$residuals) ## data does not follow assumptions of ANOVA

## log transformation of plant height ####

data$plant_ht_log = log(data$plant_ht)

## regression of plant height ####

fitplantheight <-  lm(data$plant_ht_log ~ trt_num, data = data)
summary(fitplantheight)

## anova of log transformed plant height ####

anova_plht <-  aov (plant_ht_log ~ trt_fac*variety+ block, data = data); 
summary(anova_plht)
shapiro.test(anova_plht$residuals)

## HSD using nitrogen rates as factor ####

hsd_plant_height <- HSD.test (anova_plht, trt = c("trt_fac"))
hsd_plant_height

plht = as.data.frame((hsd_plant_height$means)); plht
plht$trt_fac <- c("0", "112","168", "224", "280", "56")
plht

plht$trt_fac <- factor(plht$trt_fac,levels = c("0","56","112","168","224","280"))

b = ggplot(plht, aes(x = trt_fac, y = plant_ht_log, fill = trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "Log(Plant height (cm))", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = plant_ht_log - ((std)/sqrt(24)), ymax = plant_ht_log + ((std)/sqrt(24))), width=.2,
                position=position_dodge(0.9))+
  theme_bw() + 
  geom_text(data = plht,
            aes(y = plant_ht_log + ((std)/sqrt(24)) + 0.2, label=c("b","a","a","a","a","a"),vjust=0))+
  theme(legend.position = "none") + 
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))+
  scale_y_continuous(breaks = seq(0, 5, 1))
b 


ggsave("Plant_height_n_rates_log.png", b, dpi=600, width = 8, height = 8, units = "cm" )

## HSD using varieties as factor ####

hsd_plant_height2 <-  HSD.test(anova_plht, trt = c( "variety")); hsd_plant_height2

plht2 <-  as.data.frame((hsd_plant_height2$means)); plht2
plht2$variety <- c("Bialobrzeskie", "X-59")

plht2


a <-  ggplot(plht2, aes(x = variety, y = plant_ht_log, fill = variety))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = "log (Plant height (cm))", x = "Variety") +
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = plant_ht_log - ((std)/sqrt(24)), ymax = plant_ht_log + ((std)/sqrt(24))), width=.2,
                position=position_dodge(0.9))+
  theme_bw() + ylim(c(0,5)) + 
  geom_text(data = plht2,
            aes( y = plant_ht_log + ((std)/sqrt(24)) + 0.2, label=c("a","b"),vjust=0))+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))
  
  
  a 



ggsave("Plant_height_variety_log.png", a, dpi=600, width = 8, height = 8, units = "cm" )




## Biomass analysis
fitbiomass <-  lm(dry_biomass_yield ~ trt, data = data)
summary(fitbiomass)

anova <- aov(dry_biomass_yield ~ trt_fac*variety + block, data = data)
summary(anova)
shapiro.test(anova$resid)  #### data follows normal distribution

HSDbiomass<- HSD.test (anova, "trt_fac")
HSDbiomass

shapiro.test(anova$resid)
biomass <-  as.data.frame(HSDbiomass$means); biomass
biomass$trt_fac <- c("0","112","168", "224", "280", "56")
biomass

biomass$trt_fac <- factor(biomass$trt_fac, levels = c("0","56","112","168","224","280"))

library(ggplot2)

f <- ggplot(biomass, aes(x = trt_fac, y = dry_biomass_yield  , fill = trt_fac))+
  geom_bar(stat="identity",position=position_dodge(), color="black",  ## stat equal to identity means avde jehra yield ala result a ohi number use krna na k jive hunda bar graph ch count use krn ala)
           width = 0.7)+
  labs(y = " Dry biomass yield (kg/ha)", x = "N rates (kg/ha)") + 
  scale_fill_viridis_d(option = "D", direction = -1)+
  geom_errorbar(aes(ymin = dry_biomass_yield - ((std)/sqrt(r)), ymax = dry_biomass_yield + ((std)/sqrt(r))), width =.2,
                position = position_dodge(0.9))+
  theme_light()+ 
  geom_text(data=biomass,
            aes( y = dry_biomass_yield + ((std)/sqrt(r)) + 10, label=c("b","a","a","ab","ab","ab")),vjust=0)+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size = 9,face= "bold" ),
        axis.title=element_text(size = 9,face= "bold"))+
  scale_y_continuous(breaks = seq(0, 1500, 200))
f



ggsave("dry_biomass_yield.png", f, dpi=600, width = 8, height = 8, units = "cm" )


