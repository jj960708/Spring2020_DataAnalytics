wine<-read.csv("C:/Users/dingj/Desktop/data_analytics/hw7/winequality-red.csv",sep=";")
View(wine)
attach(wine)
hist(quality)
library(ggplot2)
ggplot(aes(x=alcohol,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=sulphates,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=pH,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=density,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=total.sulfur.dioxide,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=free.sulfur.dioxide,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=chlorides,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=citric.acid,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=volatile.acidity,y=quality),data=wine)+
  geom_point()
ggplot(aes(x=fixed.acidity,y=quality),data=wine)+
  geom_point()
wine.pca <- prcomp(wine,scale=TRUE)
biplot(wine.pca,scale=0)
var = wine.pca$sdev^2
pve = var/sum(var)

regression <- lm(quality~fixed.acidity+volatile.acidity+citric.acid+
                   chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                   density+pH+sulphates+alcohol )
plot(regression)
regression

plot(x = quality,                          
     y = regression$fitted.values,
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of quality values")
abline(b = 1, a = 0)

library('e1071')

classifier<-naiveBayes(wine[,1:11],as.factor(wine[,12]))
classifier

pred <- predict(classifier,wine[,-12])

table(predict(classifier,wine[,1:11]),as.factor(wine[,12]))

library(rpart)
library(rpart.plot)

wine_train <- wine[1:1400,]
wine_test <- wine[1400:1598,]
attach(wine_train)
decisionTree <- rpart(wine_train[,12]~.,wine_train[,1:11],method="class")
decisionTree
rpart.plot(decisionTree)
table(predict(decisionTree, wine_test[,1:11], type = "class"),wine_test[,12])
