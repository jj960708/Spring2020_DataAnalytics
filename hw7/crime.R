crime<-read.csv("C:/Users/dingj/Desktop/data_analytics/hw7/communities.data",na.strings=c("?",NA))
names(crime) <- c("state","county","community","communityname","fold","population",
                 "householdsize","racepctblack","racePctWhite","racePctAsian","racePctHisp",
                 "agePct12t21","agePct12t29","agePct16t24","agePct65up","numbUrban",
                 "pctUrban","medIncome","pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec",
                 "pctWPubAsst","pctWRetire","medFamInc","perCapInc","whitePerCap","blackPerCap",
                 "indianPerCap","AsianPerCap","OtherPerCap","HispPerCap","NumUnderPov","PctPopUnderPov","PctLess9thGrade",
                 "PctNotHSGrad","PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu",
                 "PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam","PctFam2Par",
                 "PctKids2Par","PctYoungKids2Par","PctTeen2Par","PctWorkMomYoungKids","PctWorkMom","NumIlleg","PctIlleg",
                 "NumImmig","PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10","PctRecentImmig","PctRecImmig5",
                 "PctRecImmig8","PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam","PctLargHouseOccup","PersPerOccupHous",
                 "PersPerOwnOccHous","PersPerRentOccHous","PctPersOwnOccup","PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant",
                 "PctHousOccup","PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt","PctHousNoPhone","PctWOFullPlumb",
                 "OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart","RentLowQ","RentMedian",
                 "RentHighQ","MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg",
                 "NumInShelters","NumStreet","PctForeignBorn","PctBornSameState","PctSameHouse85",
                 "PctSameCity85","PctSameState85","LemasSwornFT","LemasSwFTPerPop","LemasSwFTFieldOps",
                 "LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop",
                 "RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp","PctPolicAsian",
                 "PctPolicMinor","OfficAssgnDrugUnits","NumKindsDrugsSeiz","PolicAveOTWorked","LandArea",
                 "PopDens","PctUsePubTrans","PolicCars","PolicOperBudg","LemasPctPolicOnPatr",
                 "LemasGangUnitDeploy","LemasPctOfficDrugUn","PolicBudgPerPop","ViolentCrimesPerPop"
    
                 )
install.packages("ggbiplot")
library(ggbiplot)
attach(crime)
library(ggplot2)
hist(ViolentCrimesPerPop)
ggplot(aes(x=medIncome,y=ViolentCrimesPerPop),data=crime)+
  geom_point()

ggplot(aes(x=agePct12t21,y=ViolentCrimesPerPop),data=crime)+
  geom_point(col="red")
boxplot(medIncome)
par(new=TRUE)
ggplot(aes(x=agePct12t29,y=ViolentCrimesPerPop),data=crime)+
  geom_point()
ggplot( crime,aes(y=ViolentCrimesPerPop)) +                   
  geom_point(aes(x=agePct12t29), colour="red") + 
  geom_point(aes(x=agePct12t21), colour="blue")  +
  geom_point(aes(x=agePct16t24), colour="green")  +
  geom_point(aes(x=agePct65up), colour="yellow")  +
  ylab(label = "ViolentCrimesPerPop") +
  xlab(label = "Ages")
  


crime[colSums(!is.na(crime)) > 0]
crime.pca <- prcomp(crime[,c(6:30,34:100,128)])
crime.pca
plot(crime.pca$x, col = ViolentCrimesPerPop)

biplot(crime.pca)
summary(crime.pca$x)

train <- subset(crime, select=c("population", "householdsize", "agePct12t21",
                                           "agePct12t29","agePct16t24","agePct65up",
                                           "medIncome","MedRent","PolicOperBudg",
                                           "ViolentCrimesPerPop"))
train_regression <- na.omit(train)
train_regression
attach(train_regression)
regression <- lm(ViolentCrimesPerPop~population+householdsize+agePct12t21+agePct12t29+agePct16t24+agePct65up+medIncome+MedRent+PolicOperBudg )
plot(regression)

regression

plot(x = ViolentCrimesPerPop,                          
     y = regression$fitted.values,
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of violent crime values")
abline(b = 1, a = 0)


train <- subset(crime, select=c("population", "householdsize", "agePct12t21",
                                "agePct12t29","agePct16t24","agePct65up",
                                "medIncome","MedRent","PolicOperBudg",
                                "ViolentCrimesPerPop"))
train_KNN <- na.omit(train)
ind <- sample(2,nrow(train_KNN),replace=TRUE,prob=c(0.9,0.1))
KNNtrain <- train_KNN[ind==1,]      
KNNtest <- train_KNN[ind==2,]
sqrt(300)
library(class)
KNNpred <- knn(train = KNNtrain[,1:9],test=KNNtest[,1:9],cl=KNNtrain$ViolentCrimesPerPop,k=17)
table(KNNpred)

train_Kmeans <- na.omit(train)
set.seed(300)
wss<- sapply(1:12,function(k){kmeans(train_Kmeans[,1:9],k,nstart=20,iter.max=20)$tot.withinss})
plot(1:12,wss,type='b',xlab='Number of clusters(k)',ylab="Within cluster sum of squares")

icluster 
table(icluster$cluster,train_Kmeans$ViolentCrimesPerPop)
plot(icluster,train_Kmeans)
install.packages("factoextra")
library("factoextra")
icluster <- kmeans(train_Kmeans[,1:9],4,nstart=20)
fviz_cluster(icluster, geom = "point", data = train_Kmeans)
