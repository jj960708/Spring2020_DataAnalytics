EPI_data <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab2/EPI_data.csv')
View(EPI_data)
attach(EPI_data)
fix(EPI_data)

library(dplyr)


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
