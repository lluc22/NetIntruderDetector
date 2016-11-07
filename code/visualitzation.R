####################################################################
#                        - VISUALITZATION -                         
#
# The aim of this script is to take a first look to our data and 
# vizualize it, using FDA or PCA
#
# Authors: Lluc Bové and Aleix Trasserra
# Autumn 2016
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

