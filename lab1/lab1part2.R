library(dplyr)
EPI_data <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab1/2010EPI_data.csv',skip=1,header=T)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)

tf <- is.na(EPI)
E <- EPI[!tf]
EPI

plot(ecdf(EPI_data$EPI),do.points = FALSE,verticals= TRUE)
plot(ecdf(EPI_data$EPI),do.points = TRUE, verticals= TRUE)

par(pty='s')
help('qqnorm')
help('qqplot')
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)
x <- seq(30,95,1)
x2 <- seq(30,95,2)
x2 <- seq(30,96,2)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot')
qqline(x)

plot(ecdf(EPI_data$DALY),do.points = FALSE,verticals= TRUE)
plot(ecdf(EPI_data$DALY),do.points = TRUE, verticals= TRUE)
par(pty='s')

qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)
x <- seq(30,95,1)
x2 <- seq(30,95,2)
x2 <- seq(30,96,2)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot')
qqline(x)


plot(ecdf(EPI_data$WATER_H),do.points = FALSE,verticals= TRUE)
plot(ecdf(EPI_data$WATER_H),do.points = TRUE, verticals= TRUE)
par(pty='s')

qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)
x <- seq(30,95,1)
x2 <- seq(30,95,2)
x2 <- seq(30,96,2)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot')
qqline(x)

boxplot(EPI_data$EPI,EPI_data$DALY)

multivariate <-read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm <- lm(Homeowners~Immigrant)
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant=c(0,20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)

