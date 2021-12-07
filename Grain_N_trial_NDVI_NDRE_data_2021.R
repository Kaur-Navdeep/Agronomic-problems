## For NDVI data ##\
setwd("~/Desktop/Agronomic-problems")
rm(list = ls())
library(tidyverse)
library(dplyr)
library(agricolae)
library(ggplot2)
library(ggpubr)


 # selecting data and converting variables into factors ####
mydata <-  read.csv("Grain_N_trial_NDVI_NDRE_data_2021.csv")
mydata$block <-  as.factor(mydata$block)
mydata$variety <-  as.factor(mydata$variety)
mydata$trt_fac <- as.factor(mydata$trt)
mydata$plot_no.<- as.factor(mydata$plot_no.)

# filtering data based on varieties ####

bia <-  filter(mydata,  variety == "Bialobrzeskie")
x59 <-  filter(mydata, variety == "X-59")

# grouping data ####
group_by(mydata, mydata$trt)
grouped_data = group_by(mydata, trt, variety)
# average of NDVI and NDRE data ####
ndre_average <-  summarise(grouped_data,  ndre_1_average = mean(ndre_1), ndre_2_average = mean(ndre_2), ndre_3_average = mean(ndre_3), ndre_4_average = mean(ndre_4),ndre_5_average = mean(ndre_5), na.rm = TRUE)
ndvi_average <-  summarise(grouped_data,  ndvi_1_average = mean(ndvi_1), ndvi_2_average = mean(ndvi_2), ndvi_3_average = mean(ndvi_3), ndvi_4_average = mean(ndvi_4),ndvi_5_average = mean(ndvi_5), na.rm = TRUE)

# standard deviation of NDVI and NDRE data ####
ndre_sd <-  summarise(grouped_data,  ndre_1_sd = sd(ndre_1), ndre_2_sd = sd(ndre_2), ndre_3_sd = sd(ndre_3), ndre_4_sd = sd(ndre_4),ndre_5_sd = sd(ndre_5), na.rm = TRUE)
ndvi_sd <-  summarise(grouped_data,  ndvi_1_sd = sd(ndvi_1), ndvi_2_sd = sd(ndvi_2), ndvi_3_sd = sd(ndvi_3), ndvi_4_sd = sd(ndvi_4),ndvi_5_sd = sd(ndvi_5), na.rm = TRUE)


# log transformation of data ####

mydata$grain_yield_log <-  log(mydata$grain_yield)
mydata$ndvi_5_log <-  log(mydata$ndvi_5+1)
ndvi_5_log_sq <-  (mydata$ndvi_5_log^2)
mydata$dry_biomass_yield_log = log(mydata$dry_biomass_yield)


# fitting regression model 
fit1<- lm(grain_yield_log~ ndvi_5_log, data=mydata)
e <-  summary(fit1); e   ## model is not a good fit 


## quadratic model #### 

fit2 <- lm(grain_yield_log ~ poly(ndvi_5_log, 2, raw = TRUE), data = mydata)
summary (fit2)
coefficients(fit2)

fit3 <- lm(grain_yield_log ~ ndvi_5_log, data = mydata)
summary (fit3)
coefficients(fit3)

fit4 <- lm(dry_biomass_yield  ~ poly(ndvi_5_log, 2, raw = TRUE), data = mydata)
summary (fit4)
coefficients(fit4)

### log transformation has not helped to improve the model fit

datalong <-  gather(mydata, key = "measure", value = "value", c("ndvi_1", "ndvi_2", "ndvi_3", "ndvi_4", "ndvi_5"))

variable_names <- c(
  ndvi_1 = "21 DAS",
  ndvi_2 = "36 DAS",
  ndvi_3 = "50 DAS",
  ndvi_4 = "65 DAS",
  ndvi_5 = "78 DAS")

# fitting linear model between dry biomass yield and ndvi ####

ggplot(datalong, aes(value, dry_biomass_yield)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 1, raw = TRUE), se = T) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 1, raw = TRUE)) +
  theme_bw()+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))+
  facet_wrap(~ measure, labeller = labeller (
                                            measure = variable_names), nrow = 2)+
  labs( y = "Dry biomass yield (kg/ha)", x = "NDVI")


# fitting linear model between grain yield and ndvi ####
ggplot(datalong, aes(value, grain_yield)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 1, raw = TRUE), se = T) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x, 1, raw = TRUE)) +
  theme_bw()+
  theme(axis.text=element_text(size=9,face="bold" ),
        axis.title=element_text(size=9,face="bold"))+
  facet_wrap(~measure, labeller = labeller (
    measure = variable_names), nrow = 2)+
  labs( y = "Grain yield (kg/ha)", x = "NDVI")

## regression between dry biomass and ndvi; none of the model is good fit. Reason could be high weed pressure in the field 

fit_1 <-  lm(dry_biomass_yield  ~  ndvi_5 + ndvi_4 + ndvi_3, data = mydata)
summary(fit_1)

fit_12 <-  lm(ndvi_1 ~ dry_biomass_yield  + I(dry_biomass_yield^2), data = mydata)
summary(fit_12)

fit_2 <-  lm(ndvi_2 ~ dry_biomass_yield , data = mydata)
summary(fit_2)

fit_22 <-  lm(ndvi_2 ~ dry_biomass_yield + I(dry_biomass_yield)^2, data = mydata)
summary(fit_22)

fit_3 <-  lm(ndvi_3 ~ dry_biomass_yield , data = mydata)
summary(fit_3)

fit_32 <-  lm(ndvi_3 ~ dry_biomass_yield + I(dry_biomass_yield ^2), data = mydata)
summary(fit_32)

fit_4 <-  lm(ndvi_4 ~ dry_biomass_yield , data = mydata)
summary(fit_4)

fit_42 <-  lm(ndvi_4 ~ dry_biomass_yield + I(dry_biomass_yield^2), data = mydata)
summary(fit_42)

fit_5 <-  lm(ndvi_5 ~ dry_biomass_yield, data = mydata)
summary(fit_5)

fit_52 <-  lm(ndvi_5 ~ dry_biomass_yield + I(dry_biomass_yield^2), data = mydata)
summary(fit_52)

## these models are not good fit






