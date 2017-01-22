#We Load the data
load("data/netdataPreprocessed.Rdata")
load("data/testDataPreprocessed.Rdata")
load("data/netDataSmall.Rdata")
netData <- netData.preprocessed
testData <- testData.preprocessed
rm(netData.preprocessed)
rm(testData.preprocessed)




library(randomForest)
service <- netData$service
serviceTest <- testData$service
serviceSmall <- netDataSmall$service

netData$service <- as.integer(service)
testData$service <- as.integer(serviceTest)
netDataSmall$service <- as.integer(serviceSmall)

model.rf <- randomForest(main_attack ~ ., data = netData, ntree=100, proximity=FALSE)

pred.rf <- predict (model.rf, testData[-40], type="class")

(ct <- table(Truth=testData$main_attack, Pred=pred.rf))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

rf.error <- round(100*(1-sum(diag(ct))/sum(ct)),2)


model.rf.small <- randomForest(main_attack ~ ., data = netDataSmall, ntree=100, proximity=FALSE)

pred.rf.small <- predict (model.rf.small, testData[-40], type="class")

(ct2 <- table(Truth=testData$main_attack, Pred=pred.rf.small))

# percent by class
prop.table(ct2, 1)
# total percent correct
sum(diag(ct2))/sum(ct2)

# real test error is 

rf.small.error <- round(100*(1-sum(diag(ct2))/sum(ct2)),2)

ntrees <- round(10^seq(1,3.8,by=0.2))

# prepare the structure to store the partial results
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(main_attack ~ ., data = netDataSmall, ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}

rf.results

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

model.final <- randomForest(main_attack ~ ., data = netDataSmall, ntree=ntrees.best, proximity=FALSE)

pred.final <- predict (model.final, testData[-40], type="class")

(ct3 <- table(Truth=testData$main_attack, Pred=pred.final))

# percent by class
prop.table(ct3, 1)
# total percent correct
sum(diag(ct3))/sum(ct3)

# real test error is 

(final.error <- round(100*(1-sum(diag(ct3))/sum(ct3)),2))
