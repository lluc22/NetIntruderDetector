#first of all we need to scale the numerical variables in order to avoid premature convergence.
categoricalVars <- c("attack_type","protocol_type","service","flag","land","root_shell","su_attempted","logged_in","is_guest_login","main_attack")
numericalVars <- colnames(netDataSmall)[!(colnames(netDataSmall) %in% categoricalVars)]
trainData <- netDataSmall
testData <- testData.preprocessed
testData <- testData[testData$service != "icmp",]
for(i in 1:length(numericalVars)){
  scale(trainData[numericalVars[i]])
  scale(testData[numericalVars[i]])
}


#first look about the behaviour of neural networks in our DataSet.
library(nnet)
model.nnet <- nnet(main_attack ~ ., data = trainData, size=10,MaxNWts = 99999, maxit=1000, decay=0)

#comput training error
p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,trainData$main_attack)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(trainData))
error_rate.learn


# Compute test error

p2 <- as.factor(predict (model.nnet, newdata=testData, type="class"))

t2 <- table(p2,testData$main_attack)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(testData))
error_rate.test

# now we're going to perform CV with a fixed size= 20 and changing decays values
library(caret)
trc <- trainControl (method="repeatedcv", number=2, repeats=2)
decays <- 10^seq(-1,0,by=0.1)
model.10x10CV <- train (main_attack ~., data = trainData, method='nnet', maxit = 1000, trace = FALSE,
                        tuneGrid = expand.grid(.size=20,.decay=decays),MaxNWts = 99999, trControl=trc)


save(model.10x10CV, file = "model.10x10CV.regul")

load ("model.10x10CV.regul")

#test error
p2 <- as.factor(predict (model10x10CV, newdata=testData, type="class"))

t2 <- table(p2,testData$main_attack)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(testData))
error_rate.test


# Now we try to use a balanced dataset (choosing only 40 rows for each main attack type)


normal <- trainData[trainData$main_attack == "normal",]
normalSample <- sample(1:nrow(normal), 52)
normal <- normal[normalSample,]

dos <- trainData[trainData$main_attack == "dos",]
dosSample <- sample(1:nrow(dos), 52)
dos <- dos[dosSample,]

probe <- trainData[trainData$main_attack == "probe",]
probeSample <- sample(1:nrow(probe), 52)
probe <- probe[probeSample,] 

r2l <- trainData[trainData$main_attack == "r2l",]
r2lSample <- sample(1:nrow(r2l), 52)
r2l <- r2l[r2lSample,] 

u2r <- trainData[trainData$main_attack == "u2r",]
u2rSample <- sample(1:nrow(u2r), 52)
u2r <- u2r[u2rSample,] 

sampledTrainData <- rbind(normal,dos,probe,r2l,u2r)


model.nnet <- nnet(main_attack ~ ., data = sampledTrainData, size=20, maxit=1000, decay=0.00001,MaxNWts = 99999)

#comput training error
p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,sampledTrainData$main_attack)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(sampledTrainData))
error_rate.learn


# Compute test error

p2 <- as.factor(predict (model.nnet, newdata=testData, type="class"))

t2 <- table(p2,testData$main_attack)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(testData))
error_rate.test


