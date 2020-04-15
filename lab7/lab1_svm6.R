install.packages("kernlab")
data(spam,package="kernlab")
library("kernlab")
library(e1071)
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter

mailtype <- predict(filter,spamtest[,-58])

table(mailtype,spamtest[,58])
