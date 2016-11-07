####################################################################
#                        - DATA EXPLORATION -                         
#
# The aim of this script is to take a first look to our data and 
# to do preprocessing, feature extraction/selection, clustering
# and visualitzation.
#
# Authors: Lluc Bové and Aleix Trasserra
# Autumn 2016
####################################################################


####################################################################
#                       PREPROCESSING     
####################################################################

netData <- read.csv("data/initialData.csv", header=FALSE, quote="") 
testData <- read.csv("data/initialTestData.csv", header=FALSE, quote="") 
dim(netData)

names(netData)
#First thing we see that the variables have no name. We fix this:

colnames(netData) <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes",
                        "land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
                        "num_compromised","root_shell","su_attempted","num_root","num_file_creations",
                        "num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login",
                        "count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate",
                        "same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count",
                        "dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate",
                        "dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate",
                        "dst_host_rerror_rate","dst_host_srv_rerror_rate","attack_type")

colnames(testData) <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes",
                       "land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
                       "num_compromised","root_shell","su_attempted","num_root","num_file_creations",
                       "num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login",
                       "count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate",
                       "same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count",
                       "dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate",
                       "dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate",
                       "dst_host_rerror_rate","dst_host_srv_rerror_rate")

#This variable is our target
summary(netData$attack_type)
table(netData$attack_type)

#Delete final dot
levels(netData$attack_type) <- substr(levels(netData$attack_type),1,nchar(levels(netData$attack_type))-1)
table(netData$attack_type)

categoricalVars <- c("attack_type","protocol_type","service","flag","land","root_shell","su_attempted","logged_in","is_guest_login","is_host_login")
for (c in categoricalVars){print(c);print(table(netData[c]))}
categoricalVars.index <- which(colnames(netData) %in% categoricalVars)

 #We can remmove "is_host_login" since its values are all the same
netData$is_host_login <- NULL
testData$is_host_login <- NULL
categoricalVars <- categoricalVars[1:9]

#All Categorical as factors
netData[categoricalVars[]] <- lapply(netData[categoricalVars[]],factor)
testData[,categoricalVars[2:9]] <- lapply(testData[categoricalVars[2:9]],factor)

#Boolen categoricals with bools
levels(netData[,"land"]) <- c(F,T)
levels(netData[,"logged_in"]) <- c(F,T)
levels(netData[,"is_guest_login"]) <- c(F,T)
levels(netData[,"root_shell"]) <- c(F,T)
summary(netData)

levels(testData[,"land"]) <- c(F,T)
levels(testData[,"logged_in"]) <- c(F,T)
levels(testData[,"is_guest_login"]) <- c(F,T)
levels(testData[,"root_shell"]) <- c(F,T)

table(netData$su_attempted)
#In the metadata description tells su_attempted is a boolean, then what is 2?
#We might consider NA and delete rows, first let's see which atacks include

table(netData$attack_type[netData$su_attempted == 2])

#Since all normal connections, and they are the most, we will consider incorrect value's and delete them
netData <- netData[netData$su_attempted != 2,]
testData <- testData[testData$su_attempted != 2,]
dim(netData)

#and we categorize it again
netData$su_attempted <- factor(netData$su_attempted)
testData$su_attempted <- factor(testData$su_attempted)
(levels(netData[,"su_attempted"]) <- c(F,T))
(levels(testData[,"su_attempted"]) <- c(F,T))

#Let's analyze numerical variables
summary(netData[,!(colnames(netData) %in% categoricalVars)])

#Some variables have a lot of 0's but they are well accounted so they aren't NA's
#Let's analize num_outbound_cmds:
table(netData$num_outbound_cmds)

#It's all zeros so we will drop it too.
netData$num_outbound_cmds <- NULL
testData$num_outbound_cmds <- NULL
####################################################################
#                 FEATURE EXTRACTION/SELECTION
####################################################################

#We introduce a new categorical variable: main_attack. In this category we generalize attack_type as follows:
# - dos: back,land,neptune,smurf,teardrop
# - u2r: buffer_overflow,loadmodule,perl,rootkit
# - r2l: ftp_write,guess_passwd,imap,multihop,phf,spy,warezclient,warezmaster
# - probe: ipsweep,nmap,portsweep,satan
# This variable is an alternative objective variable t2 to the original t variable attack_type.

main_attack <- as.character(netData$attack_type)
main_attack <-replace(main_attack,main_attack %in% c("back","land","neptune","pod","smurf","teardrop"),"dos")
main_attack <-replace(main_attack,main_attack %in% c("buffer_overflow","loadmodule","perl","rootkit"),"u2r")
main_attack <-replace(main_attack,main_attack %in% c("ftp_write","guess_passwd","imap","multihop","phf","spy","warezclient","warezmaster"),"r2l")
main_attack <-replace(main_attack,main_attack %in% c("ipsweep","nmap","portsweep","satan"),"probe")
table(main_attack)
netData$main_attack <- factor(main_attack)


####################################################################
# Plots
####################################################################
library(ggplot2)

#Several useful functions...

hist.with.normal <- function (x, main="", xlabel=deparse(substitute(x)),path, ...)
{
  h <- hist(x,plot=F, ...)
  s <- sd(x)
  m <- mean(x)
  ylim <- range(0,h$density,dnorm(0,sd=s))
  p <- ggplot(data.frame(f = x),aes(x)) + 
    geom_histogram(aes(y = ..density..)) + 
    stat_function(fun=function(x) dnorm(x,m,s), colour = "red") +
    xlab(xlabel) +
    ggtitle(main)
  ggsave(file=path,plot = p)
}

save.boxplot <- function(x,main="",ylabel=deparse(substitute(x)),path, ...){
  p <- ggplot(data.frame(f = x),aes(x="",y=x)) + 
    geom_boxplot() + 
    theme(axis.title.x = element_blank()) +
    ggtitle(main) +
    ylab(ylabel)
  ggsave(file=path,plot = p)
}

barplot.percent <- function(df,df.x,df.y,main="",xlabel=deparse(substitute(df.x)),ylabel=deparse(substitute(df.y)),col=2){
  library(reshape2)
  library(scales)
  DF1 <- melt(prop.table(table(df.x,df.y), col), id.var=xlabel)
  p <- 0
  if(col == 2){
    p <- ggplot(DF1, aes(x=df.x,y=value,fill=df.y)) + 
      geom_bar(position = "fill",stat = "identity") + 
      xlab(xlabel) +
      ylab("percent") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_discrete(name=ylabel) +
      ggtitle(main) 
  }
  else{
    p <- ggplot(DF1, aes(x=df.y,y=value,fill=df.x)) + 
      geom_bar(position = "fill",stat = "identity") + 
      xlab(ylabel) +
      ylab("percent") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_discrete(name=xlabel) +
      ggtitle(main) +
      coord_flip()
  }
  print(p)
  
}


barplot.facet <- function(df,df.x,df.y,main="",xlabel=deparse(substitute(df.x)),ylabel=deparse(substitute(df.y))){
  library(reshape2)
  DF1 <- melt(prop.table(table(df.x,df.y), 2), id.var=xlabel)
  p <- ggplot(DF1, aes(df.x,y=value)) + geom_bar(stat="identity") +
    facet_wrap(~ df.y) +
    ggtitle(main)
  print(p)
}

categoricalVars <- c(categoricalVars,"main_attack")
numericalVars <- colnames(netData)[!(colnames(netData) %in% categoricalVars)]


#For every numerical, do a histogram and a boxplot with: 
# The original data
# The original data != 0
# log(data + 1)
# log(data +1 ) != 0
#This is because many data have a lot of 0 values(for example boolean values that are rare)
#And it may help to vizualize plots without them.
#Also we apply a log transformation that may help us too.
#This may take a while...
#(It is saved as file because they are a plenty of numerical variables)

# for (i in numericalVars){
#   print(i)
#   x <- netData[,i]
#   x.nonZero <- x[x!=0]
#   x.log <- log(x + 1)
#   x.logNonZero <- log(x.nonZero + 1)
#   
#   hist.with.normal(x,path=paste("graphs/",i,".png",sep=""),main=paste("Histogram of",i),xlabel = i)
#   hist.with.normal(x.nonZero,path=paste("graphs/",i,"_nonZero",".png",sep=""),main=paste("Histogram of non-zero",i),xlabel= paste("non-zero",i))
#   hist.with.normal(x.log,path=paste("graphs/",i,"log",".png",sep=""),paste("Histogram of log",i),xlabel= paste("log",i))
#   hist.with.normal(x.logNonZero,path=paste("graphs/",i,"log_nonZero",".png",sep=""),main=paste("Histogram of non-zero log",i),xlabel= paste("non-zero log",i))
#   
#   save.boxplot(x,path=paste("graphs/Box_",i,".png",sep=""),main=paste("Boxplot of",i),ylabel = i)
#   save.boxplot(x.nonZero,path=paste("graphs/Box_",i,"_nonZero",".png",sep=""),main=paste("Boxplot of non-zero",i),ylabel= paste("non-zero",i))
#   save.boxplot(x.log,path=paste("graphs/Box_",i,"log",".png",sep=""),main=paste("Boxplot of log",i),ylabel= paste("log",i))
#   save.boxplot(x.logNonZero,path=paste("graphs/Box_",i,"log_nonZero",".png",sep=""),main=paste("Boxplot of non-zero log",i),ylabel= paste("non-zero log",i))
#   
# }


#Bar charts for qualitative....
for(i in categoricalVars){
  p <- ggplot(netData,aes_string(i)) + geom_bar() + coord_flip() + ggtitle(paste("barplot of",i))
  print(p)
}

attach(netData)
#Some contingency tables
prop.table(table(main_attack,protocol_type), 2)
prop.table(table(main_attack,service), 2)
prop.table(table(main_attack,flag), 2) 
prop.table(table(main_attack,su_attempted), 2) 
#We plot them...
barplot.percent(netData,main_attack,protocol_type,main="main_attack vs protocol")
barplot.percent(netData,main_attack,protocol_type,col=1,main="protocol vs main_attack")
barplot.percent(netData,main_attack,service,main="main_attack vs service")
barplot.percent(netData,main_attack,service,col=1,main="service vs main_attack")
barplot.percent(netData,main_attack,flag,main="main_attack vs flag")
barplot.percent(netData,main_attack,flag,col=1,main="flag vs main_attack")
barplot.percent(netData,main_attack,su_attempted,main="main_attack vs su_attempted")
barplot.percent(netData,main_attack,su_attempted,col=1,main="su_attempted vs main_attack")
detach(netData)
####################################################################
# Save final preprocessed data
####################################################################
netData.preprocessed <- netData[,colnames(netData)!= "attack_type"]
netData.preprocessed <- netData.preprocessed[sample.int(nrow(netData.preprocessed)),] #This is the final data

testData.preprocessed <- testData[,colnames(testData)!= "attack_type"]
testData.preprocessed <- testData.preprocessed[sample.int(nrow(testData.preprocessed)),] #This is the final test data

netData <- netData[sample.int(nrow(netData)),] #We keep this just in case we want to use the specific atack as a classifier

save(netData.preprocessed, file = "netdataPreprocessed.Rdata")
save(netData,file="netdata.Rdata")
save(testData,file="testData.Rdata")

rm(list = ls())
####################################################################
#                        - VISUALITZATION -                         
####################################################################

####################################################################
# FISCHER DISCRIMINANT ANALYSIS
####################################################################

library(ggplot2)
library(MASS)
library(rgl)

#We load the data
load("data/netdataPreprocessed.Rdata")

categoricalVars <- c("attack_type","protocol_type","service","flag","land","root_shell","su_attempted","logged_in","is_guest_login","main_attack")
numericalVars <- colnames(netData.preprocessed)[!(colnames(netData.preprocessed) %in% categoricalVars)]

#We separate the predicted variable from the others, and apply it with numerical variables
main_attack <- netData.preprocessed$main_attack
netData <- netData.preprocessed[,numericalVars]

(lda.model <- lda (x=netData, grouping=main_attack))
#As we know, the probability of each attack is unequal. There are many DOS attacks and very little r2l and u2r attacks.
#In fact, they represent less than 0.22% and 0.01% each. 

sort(abs(lda.model$scaling[,1]))
sort(abs(lda.model$scaling[,2]))

#So for the first factorial plane, LD1 separates the groups mainly using same_srv,srv_serror_rate, srv_rerror_rate and rerror_rate.
#LD2 separates groups mainly using dst_host_srv_diff_host_rate, dst_host_diff_srv_rate, diff_srv_rate and dst_host_rerror_rate.

#Trace representation: LD1 + LD2 = 98.02%, third and fourth LD don't add up much difference
loadings <- as.matrix(netData) %*% as.matrix(lda.model$scaling)
netData.lda <- data.frame (loadings,main_attack)

#We create a function that plots all the loadings, inputed in data.frame mode.
loadings.plot <- function (loadings,main="")
{
  ggplot(loadings,aes(x=LD1,y=LD2)) + geom_point(aes(color=main_attack),alpha = I(0.2)) + ggtitle(main)
}

#Plot loadings
loadings.plot(netData.loglda,"LDA model")

plot3d(loadings[,1], loadings[,2], loadings[,3], "LD1", "LD2", "LD3",
       size = 4, col=palette()[unclass(main_attack)])
legend3d("topright",legend=levels(main_attack),pch = 16, col = palette(), cex=1, inset=c(0.02))




(lda.logmodel <- lda (x=log(netData + 1), grouping=main_attack))
#Trace representation: LD1 + LD2 = 98.49%, so it is sligthly better!

loadings.log <- as.matrix(log(netData + 1)) %*% as.matrix(lda.logmodel$scaling)
netData.loglda <- data.frame (loadings.log, main_attack)

#Plot loadings
loadings.plot(netData.loglda,"LDA with log-transformation")

# 3D scatterplot (can be rotated and zoomed in/out with the mouse)
plot3d(loadings.log[,1], loadings.log[,2], loadings.log[,3], "LD1", "LD2", "LD3",
       size = 4, col=palette()[unclass(main_attack)])
legend3d("topright",legend=levels(main_attack),pch = 16, col = palette(), cex=1, inset=c(0.02))




