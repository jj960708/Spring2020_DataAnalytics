
library("plyr")
library(dplyr)
library(ggplot2)

solar <- read.csv("C:/Users/dingj/Desktop/dsproject/historical_tsi.csv") ##the data for solar irradiance
temp<-read.csv("C:/Users/dingj/Desktop/dsproject/GlobalLandTemperaturesByCountry.csv") ## data for land monthly average temperature
globalTemp <- read.csv("C:/Users/dingj/Desktop/dsproject/GlobalTemperatures.csv")
ghg <- read.csv("C:/Users/dingj/Desktop/dsproject/greenhouse-gas-emissions-by-gas.csv") ## data for greenhouse gas emission

## visualize the solar irradiance plot
head(solar)
attach(solar)
ggplot(solar, aes(x=time, y=Irradiance))+geom_line()


## dataset that after merge of greenhouse gases emission dataset and average land temperature

dates<- format(as.Date(temp$dt), format = "%Y")

temp2 <- ddply(temp, .( Country,dates), summarise,
               mean_TSO4 = mean(AverageTemperature),
               mean_TNO3 = mean(AverageTemperatureUncertainty))
colnames(temp2) <- c("Entity", "Year","MeanT","MeanUnT")
total <- merge(temp2,ghg,by=c("Entity","Year"))
colnames(total) <- c("Entity", "Year","MeanT","MeanUnT",
                     "Code","SFA","PFC","HFC","NO2","CH4","CO2")
# plot the global average temperature

attach(globalTemp)
globalTemp$Year <- format(as.Date(globalTemp$dt),"%Y")

globalTemp <- globalTemp%>%group_by(globalTemp$Year)%>%summarize(Avg_Temp=mean(globalTemp$LandAverageTemperature,na.rm=T))
ggplot(globalTemp, aes(x=globalTemp$Year, y=globalTemp$Avg_Temp,
                          color=Avg_Temp,group= 1))+geom_line()+
  labs(y= "Average Temperature", x = "Year")+scale_x_discrete(breaks = seq(1750, 2013, by = 30))


##plot the temperature for different countries
colnames(temp2) <- c("Entity", "Year","MeanT","MeanUnT")

attach(temp2)
temp3 <- temp2 %>% filter(Year>=1850 & Year<= 2012)
## explore the temperature dataset
library(scales)



## exploration on greenhouse gas
attach(total)
x <- subset(total,total$Entity=="United States")

plot(x$CO2,type = "l",col = "red", xlab = "Month", ylab = "Rain fall", 
     main = "Rain fall chart")

x <- subset(total,total$Entity=="United States"|
              total$Entity== "China"|total$Entity=="India"|
              total$Entity== "United Kindom"|total$Entity== "France"|
              total$Entity== "Australia"|total$Entity== "Japan"|
               total$Entity== "Germany")
## plot of the temperature 
ggplot(aes(x = Year, y = MeanT,group =Entity), data = x)+geom_line(aes(color=Entity))+
  labs(y= "Average Temperature", x = "Year")+scale_x_discrete(breaks=seq(1960, 2012, 10))

## plots of six major greenhouse gases
ggplot(aes(x = Year, y = CO2,group =Entity  ), data = x)+geom_line(aes(color=Entity))+
  labs(y= "CO2 emission", x = "Year")+scale_x_discrete(breaks=seq(1970, 2012, 10))

ggplot(aes(x = Year, y = CH4,group =Entity  ), data = x)+geom_line(aes(color=Entity))+
  labs(y= "CH4 emission", x = "Year")+scale_x_discrete(breaks=seq(1970, 2012, 10))

ggplot(aes(x = Year, y = NO2,group =Entity  ), data = x)+geom_line(aes(color=Entity))+
  labs(y= "NO2 emission", x = "Year")+scale_x_discrete(breaks=seq(1970, 2012, 10))



### ARIMA Modelling
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)
## use models on the temperature for United States
US_temp <- subset(temp,temp$Country=="United States")
US_temp <- subset(temp2,temp2$Entity=="United States")
attach(US_temp)
## clearn the dataset e.g outliers missing NA
US_temp$clean_temp <- tsclean(MeanT)
ts_temp = ts(na.omit(US_temp$clean_temp), frequency=30) 
ggplot(US_temp, aes(x=Year, y=MeanT,group=1))+geom_line()+scale_x_discrete(breaks=seq(1750, 2012, 50))

ggplot() + 
  geom_line(data = US_temp, aes(x = Year, y = MeanT, group=1)) +scale_x_discrete(breaks=seq(1750, 2012, 50))
ggplot( ) + 
  geom_line(data = US_temp, aes(x = Year, y = ts_temp,group=1))+scale_x_discrete(breaks=seq(1750, 2012, 50))
decomp = stl(ts_temp, s.window="periodic")
plot(decomp)
deseasonal_tmp <- seasadj(decomp)
## test if stationary
adf.test(ts_temp, alternative = "stationary")
## find parameters using auo arima
deseasonal_tmp
## use the first 236 as training dataset and last 10 as test
fit<-auto.arima(deseasonal_tmp[c(1:236)], seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='Model Residuals') 
fit
fcast <- forecast(fit, h=10)
fcast
plot(fcast, main="Prdiction plot",ylab="Temperature",xlab="Year")
lines(ts(deseasonal_tmp))

## Regression model
## using the dataset after mnerge
attach(total)
regression_temp <- subset(total,total$Entity=="United States"|
                    total$Entity== "China"|
                    total$Entity== "Germany")
## clean the dataset
attach(regression_temp)
## plot before data cleaning

regression_temp$NO2 <- tsclean(NO2)
regression_temp$CH4 <- tsclean(CH4)
ggplot() + 
  geom_point(data = regression_temp, aes(x = MeanT, y = CO2, group=1),colour="red") + 
  geom_point(data = regression_temp, aes(x = MeanT, y = CH4, group=1),colour="blue")+
  geom_point(data = regression_temp, aes(x = MeanT, y = NO2, group=1),colour="yellow")+
  labs(y= "Greenhouse Gas Emission", x = "Temperature")

trainingRowIndex <- sample(1:nrow(regression_temp), 0.9*nrow(regression_temp))
trainingRowIndex
trainingData <- regression_temp[trainingRowIndex, ]
testData  <- regression_temp[-trainingRowIndex, ] 
regression <- lm(MeanT~NO2+CH4+CO2,data= trainingData)
summary(regression)
plot(regression)

myvars <- c("CO2", "CH4", "NO2")

pred <- predict.lm(regression,newdata = testData[myvars])
plot(pred)
actuals_preds <- data.frame(cbind(actuals=testData$MeanT, predicteds=pred))
pred
actual_temp <- testData$MeanT
plot(actual_temp,type='l',col = "red",main = "Prediction Vs Actual",ylab="Temperature")
lines(pred,col="blue")
abline(a=0,b=1)
## predict the temperature in 2025
data2025 <- data.frame(CO2=5305000000,CH4=674000000,NO2=335000000)
temp2025 <-predict.lm(regression,newdata = data2025)
temp2025
