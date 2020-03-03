EPI_data <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab2/EPI_data.csv')

attach(EPI_data)
fix(EPI_data)

library(dplyr)

summary(EPI)
summary(DALY)
hist(EPI)
hist(DALY)

sample_n(EPI_data['EPI'],5)
sample_n(EPI_data['DALY'],5)

sample_frac(EPI_data['EPI'],size = 0.1)
sample_frac(EPI_data['DALY'],size = 0.1)

new_decs_EPI <- arrange(EPI_data['EPI'],desc(EPI))
new_decs_DALY <- arrange(EPI_data['DALY'],desc(DALY))

double_EPI <- mutate(EPI_data['EPI']*2)
new_decs_DALY <- mutate(EPI_data['DALY']*2)

summarise(EPI_data,means = mean(EPI,na.rm=TRUE))
summarise(EPI_data,means = mean(DALY,na.rm=TRUE))

boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)

DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV <- predict(lmENVH,NEW,interval='prediction')
cENV <- predict(lmENVH,NEW,interval='confidence')
