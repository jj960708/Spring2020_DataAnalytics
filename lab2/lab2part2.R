multivariate <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab2/dataset_multipleRegression.csv')

View(multivariate)
attach(multivariate)
mm <- lm(ROLL~UNEM+HGRAD)
newdata <- data.frame(UNEM=7,HGRAD=90000)
predict.lm(mm,newdata)

mm2 <- lm(ROLL~UNEM+HGRAD+INC)
newdata2 <- data.frame(UNEM=7,HGRAD=90000,INC = 25000)
predict.lm(mm2,newdata2)


abalone <- read.csv('C:/Users/dingj/Desktop/data_analytics/lab2/abalone.csv')

attach(abalone)

library(ISLR)
library(class)
any(is.na(abalone))
purchase <- abalone[,9]
Standardizedabalone <- scale(abalone[,2:8])

test_index <- 1:1000
test_data <- Standardizedabalone[test_index,]
test_purchase <- purchase[test_index]

train_data <- Standardizedabalone[-test_index,]

train_purchase <- purchase[-test_index]

predicted_purchase <- NULL
error_rate <- NULL
for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k =i)
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)

# Plot the K value on a graph
library(ggplot2)
k_values <- 1:20



set.seed(101)
irisClusters <- kmeans(iris[,1:4], 3, 1000) # nstart is the number of random start
print(irisClusters)
table(iris[,5],irisClusters$cluster)
library(cluster)
clusplot(iris,irisClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)