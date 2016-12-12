library(MASS)

#We Load the data
load("data/netdataPreprocessed.Rdata")
load("data/testDataPreprocessed.Rdata")
netData <- netData.preprocessed
testData <- testData.preprocessed
rm(netData.preprocessed)
rm(testData.preprocessed)

library(nnet)

#Train a multinomial regression with all the data
multinom.model <- multinom(main_attack ~ ., data = netData)
multinom.train <- predict(multinom.model)

#Training error
(1 - (sum(diag(table(netData$main_attack,multinom.train))) / length(netData$main_attack))) * 100

#Test Error
testData <- testData[testData$service != "icmp",]
multinom.test<- predict(multinom.model,testData)
(1 - (sum(diag(table(testData$main_attack,multinom.test))) / length(testData$main_attack))) * 100

table(netData$main_attack,multinom.train)

#Looks good? look at the confusion matrix of test data
table(testData$main_attack,multinom.test)

#Less frequent classes are less accuratly predicted. To solve this we undersample the most used classess
library(glmnet)

#We use regularization and 10CV to select the best model.
bestModel <- NULL
minCV.error <- Inf
for (i in 1:100){
  
  #Create sample of data
  sub.dos <- sample(row.names(netData[netData$main_attack == "dos",]),70)
  sub.normal <- sample(row.names(netData[netData$main_attack == "normal",]),70)
  sub.probe <- sample(row.names(netData[netData$main_attack == "probe",]),70)
  sub.r2l <- sample(row.names(netData[netData$main_attack == "r2l",]),70)
  rows.subNet <- c(row.names(netData[netData$main_attack == "u2r",]),sub.dos,sub.normal,sub.probe,sub.r2l)
  
  #By default the model uses regularization
  sub.model <- cv.glmnet(data.matrix(netData[rows.subNet,-40]),netData[rows.subNet,"main_attack"],family = "multinomial")
  cve <- tail(sub.model$cvm,n=1)
  if(cve < minCV.error){
    minCV.error = cve
    bestModel <- sub.model
  }
}

sub.train <- predict(bestModel,type="class",newx =data.matrix(netData[-40]))
sub.test <- predict(bestModel,type="class",newx =data.matrix(testData[-40]))

#Training error
(1 - (sum(diag(table(netData$main_attack,sub.train))) / length(netData$main_attack))) * 100

#Test Error
(1 - (sum(diag(table(testData$main_attack,sub.test))) / length(testData$main_attack))) * 100

#Contingency tables
table(netData$main_attack,sub.train)
table(testData$main_attack,sub.test)




