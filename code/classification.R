####################################################################
#                        - LINEAR CLASSIFICATION -                         
#
# The aim of this script is to perform linear classificaction with 3 
# different techniques: multinomial, LDA and Logistic Regression
#
# Authors: Lluc Bové and Aleix Trasserra
# Autumn 2016
####################################################################


####################################################################
#                       -MULTINOMIAL-     
###################################################################

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

####################################################################
#                       -LDA-     
###################################################################

categoricalVars <- c("attack_type","protocol_type","service","flag","land","root_shell","su_attempted","logged_in","is_guest_login","main_attack")
numericalVars <- colnames(netData.preprocessed)[!(colnames(netData.preprocessed) %in% categoricalVars)]

library(MASS)
#We separate the predicted variable from the others, and apply it with numerical variables
main_attack <- netData.preprocessed$main_attack
netData <- netData.preprocessed[,numericalVars]

lda.model <- lda (main_attack ~., netData)

#training error

train <- predict(lda.model)
trainTable <- table(train$class,netData.preprocessed$main_attack)
trainError <- 100*(1-sum(diag(trainTable))/nrow(netData))

#test error
testData <-  testData.preprocessed[,numericalVars]
test <- predict(lda.model, newdata = testData)
testTable <- table(testData.preprocessed$main_attack, test$class)
testTable
testError <- 100*(1-sum(diag(testTable))/nrow(testData))
testError


####################################################################
#                       -Logistic regression-     
###################################################################


### Now we have two classes, attack or normal connection
normal <- netData.preprocessed[netData.preprocessed$main_attack == "normal",]

restData <- netData.preprocessed[netData.preprocessed$main_attack != "normal",]

restData$main_attack <- "attack"

trainLogistic <- rbind(normal,restData) 
trainLogistic$main_attack <- droplevels(trainLogistic$main_attack)
trainLogistic$main_attack <- as.factor(trainLogistic$main_attack)
# the same with test data
normalTest <- testData.preprocessed[testData.preprocessed$main_attack == "normal",]

restDataTest <- testData.preprocessed[testData.preprocessed$main_attack != "normal",]

restDataTest$main_attack <- "attack"

testLogistic <- rbind(normalTest,restDataTest) 
testLogistic$main_attack <- droplevels(testLogistic$main_attack)
testLogistic <- testLogistic[testLogistic$service != "icmp",]
testLogistic$main_attack <- as.factor(testLogistic$main_attack)
### Logistic regression for classifiying attack connections ###

library(kernlab)  

## Fit a GLM in the learning data
attackModel <- glm (main_attack ~ ., data=trainLogistic, family=binomial)

attack.accs <- function (P=0.5, model)
{
  ## Compute accuracy in learning data
  
  attack.pred <- NULL
  attack.pred[model$fitted.values<P] <- 0
  attack.pred[model$fitted.values>=P] <- 1
  
  attack.pred <- factor(attack.pred, labels=c("normal","attack"))
  
  print("Training confusion table:")
  
  print(M1.TRtable <- table(Truth=trainLogistic$main_attack,Pred=attack.pred))
  
  print("Training error:")
  
  print(100*(1-sum(diag(M1.TRtable))/nrow(trainLogistic)))
  
  ## Compute accuracy in test data
  
  gl1t <- predict(model, newdata=testLogistic,type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  
  gl1predt <- factor(gl1predt, labels=c("normal","attack"))
  
  print("Test confusion table:")
  
  print(M1.TEtable <- table(Truth=testLogistic$main_attack,Pred=gl1predt))
  
  print("Test error:")
  
  print(100*(1-sum(diag(M1.TEtable))/nrow(testLogistic)))
}

p <- c(0.4,0.5,0.6,0.7)

for (e in p){
  print(paste("P",e,sep="="))
  attack.accs(e,attackModel)
  print("-----------------------------")
}


#### We try it with simpliifyng(using AIC)

attackModel.AIC <- step (attackModel)
for (e in p){
  print(paste("P",e,sep="="))
  attack.accs(e,attackModel.AIC)
  print("-----------------------------")
}


##### The results are so bad, we try to downsample dos attacks

table(netData.preprocessed$main_attack)

# we hace to choose 91987 dos rows

trainLogistic2 <- netData.preprocessed[-sample(which(netData.preprocessed$main_attack == "dos"),299471),]
restData <- trainLogistic2[trainLogistic2$main_attack != "normal",]

restData$main_attack <- "attack"
trainLogistic2 <- rbind(normal,restData) 
trainLogistic2$main_attack <- droplevels(trainLogistic2$main_attack)
trainLogistic2$main_attack <- as.factor(trainLogistic2$main_attack)

attackModel2 <- glm (main_attack ~ ., data=trainLogistic2, family=binomial)

attackModel2.AIC <- step (attackModel2)

for (e in p){
  print(paste("P",e,sep="="))
  attack.accs(e,attackModel2.AIC)
  print("-----------------------------")
}

