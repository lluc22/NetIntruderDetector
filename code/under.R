
prop.table(table(netData.preprocessed$main_attack))
# dos      normal       probe         r2l         u2r 
# 0.792401040 0.196900904 0.008313513 0.002279283 0.000105260


### Now we have two classes, attack or normal connection
normal <- netData.preprocessed[netData.preprocessed$main_attack == "normal",]

restData <- netData.preprocessed[netData.preprocessed$main_attack != "normal",]

restData$main_attack <- "attack"

trainLogistic <- rbind(normal,restData) 
trainLogistic$main_attack <- droplevels(trainLogistic$main_attack)

# the same with test data
normalTest <- testData.preprocessed[testData.preprocessed$main_attack == "normal",]

restDataTest <- testData.preprocessed[testData.preprocessed$main_attack != "normal",]

restDataTest$main_attack <- "attack"

testLogistic <- rbind(normalTest,restDataTest) 
testLogistic$main_attack <- droplevels(testLogistic$main_attack)

### Logistic regression for classifiying attack connections ###

library(kernlab)  

## Fit a GLM in the learning data
attackModel <- glm (main_attack ~ ., data=trainLogistic, family=binomial)

attack.accs <- function (P=0.5)
{
  ## Compute accuracy in learning data
  
  attack.pred <- NULL
  attack.pred[attackModel$fitted.values<P] <- 0
  attack.pred[attackModel$fitted.values>=P] <- 1
  
  attack.pred <- factor(attack.pred, labels=c("normal","attack"))
  
  print(M1.TRtable <- table(Truth=trainLogistic$main_attack,Pred=attack.pred))
  
  print(100*(1-sum(diag(M1.TRtable))/nrow(trainLogistic)))
  
  ## Compute accuracy in test data
  
  gl1t <- predict(attackModel, newdata=testLogistic,type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  
  gl1predt <- factor(gl1predt, labels=c("normal","attack"))
  
  print(M1.TEtable <- table(Truth=trainLogistic$main_attack,Pred=gl1predt))
  
  print(100*(1-sum(diag(M1.TEtable))/nrow(testLogistic)))
}

attack.accs()
