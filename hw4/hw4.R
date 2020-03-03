
ny <- read.csv('C:/Users/dingj/Desktop/data_analytics/hw4/rollingsales_manhattan.csv',, stringsAsFactors = FALSE,skip=4)
attach(ny)

summary(`SALE
PRICE`)
boxplot(`SALE
PRICE`)    
plot(`TAX CLASS AT TIME OF SALE`,`SALE
PRICE`)
plot(`YEAR BUILT`,`SALE
PRICE`)
plot(`YEAR BUILT`,`SALE
     PRICE`)
plot(`GROSS SQUARE FEET`,`SALE
PRICE`)
plot(`SALE DATE`,`SALE
PRICE`)

new = apply(ny[c(15,16,17)],1,function(z) all(z!=0))
ny2 <- ny[new,]
new2 = apply(ny2[c(20)],1,function(z) all(z!=0))
ny2 <- ny2[new2,]



train_ny2 <- ny2[1:2000,]
attach(train_ny2)
mm <- lm(SALE.PRICE~GROSS.SQUARE.FEET+YEAR.BUILT+ZIP.CODE+TAX.CLASS.AT.TIME.OF.SALE+LAND.SQUARE.FEET)
newdata <- data.frame(GROSS.SQUARE.FEET=2465,YEAR.BUILT=46200,ZIP.CODE=10034,TAX.CLASS.AT.TIME.OF.SALE=4,LAND.SQUARE.FEET=14980)
predict.lm(mm,newdata)

summary(mm)
