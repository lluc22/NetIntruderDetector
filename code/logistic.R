
prop.table(table(netData.preprocessed$main_attack))
# dos      normal       probe         r2l         u2r 
# 0.792401040 0.196900904 0.008313513 0.002279283 0.000105260


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

