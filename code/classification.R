library(MASS)
library(class)
load("data/netdataPreprocessed.Rdata")
load("data/testDataPreprocessed.Rdata")
netData <- netData.preprocessed
testData <- testData.preprocessed
rm(netData.preprocessed)
rm(testData.preprocessed)

library(nnet)

multinom.model <- multinom (main_attack ~ ., data = netData)
multinom.train <- predict(multinom.model)

#Training error
(1 - (sum(diag(table(netData$main_attack,multinom.train))) / length(netData$main_attack))) * 100

#Test Error
testData <- testData[testData$service != "icmp",]
multinom.test <- predict(multinom.model,newdata = testData)
(1 - (sum(diag(table(testData$main_attack,multinom.test))) / length(testData$main_attack))) * 100

#Looks good? look at the confusion matrix
table(testData$main_attack,multinom.test)

#Less frequent classes are less accuratly predicted. To solve this we use undersampling of the most frequent classes
#and create subsets of data, a model for each subset and a model to predict classes based on the ouput of the sub-models



train_n_models <- function(n,model_func,...){
  models <- vector("list",n)
  for(i in 1:n){
    sub.dos <- sample(row.names(netData[netData$main_attack == "dos",]),98000)
    rows.subNet <- c(row.names(netData[!netData$main_attack %in% c("dos"),]),sub.dos)
    sub.model <- model_func(main_attack ~ ., data = netData,subset=rows.subNet,...)
    sub.train <- predict(sub.model,newdata = netData)
    sub.test <- predict(sub.model,newdata = testData)
    print(table(netData$main_attack,sub.train))
    print(table(testData$main_attack,sub.test))
    models[[i]] <- sub.model
  }
  return(models)
}

transform_data <- function(models,data){
  model_out <- lapply(models,function(model){
    predict(model,newdata=data,"probs")
  })
  newData <- data.frame(model_out,main_attack = data$main_attack)
  newData
}

models <- train_n_models(1,multinom,trace=F)
train <- transform_data(models,netData)
test <- transform_data(models,testData)
sub.model <- multinom (main_attack ~ ., data = train, MaxNWts = 3000)
sub.train <- predict(sub.model)
sub.test <- predict(sub.model,newdata = test)
table(netData$main_attack,sub.train)
table(testData$main_attack,sub.test)

reg.model <- multinom(main_attack ~., data = train, decay=0.01)
reg.train <- predict(reg.model)
reg.test <- predict(reg.model,newdata = test)
table(netData$main_attack,reg.train)
table(testData$main_attack,reg.test)

