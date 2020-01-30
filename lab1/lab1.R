EPI_data <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab1/2010EPI_data.csv',skip=1,header=T)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)

tf <- is.na(EPI)
E <- EPI[!tf]
EPI

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
lines(density(EPI,na.rm=TRUE,bw='SJ'))
rug(EPI)
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
par(pty='s')
qqnorm(EPI);qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot fot tdsn')
qqline(x)

plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)
par(pty='s')
qqnorm(DALY);qqline(DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot fot tdsn')
qqline(x)

plot(ecdf(WATER_H),do.points=FALSE,verticals=TRUE)
par(pty='s')
qqnorm(WATER_H);qqline(WATER_H)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot fot tdsn')
qqline(x)

boxplot(EPI,DALY)

EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
