prep10 <- read.csv("data/prep10.csv", header=TRUE)

numericalVariables <- subset(prep10, select = -c(main_attack,num_outbound_cmds,protocol_type,service,flag,land,root_shell,su_attempted,logged_in,is_guest_login))

##### K-MEANS
library (cclust)

CH.K <- function(Kaux){ ### AQUI has de tocar
  CH.K <- 0
  for (i in 1:2) {
    kmeans.K <- cclust (as.matrix(numericalVariables),Kaux,iter.max=100,method="kmeans",dist="euclidean")
    (CH.K = CH.K + clustIndex(kmeans.K,numericalVariables, index="calinski")) ## aqui has de canviar l'index
  }
  return(CH.K)
}

# A res em guardo les mitjanes de l'experiment per tots els valors de K.
res <- vector("numeric",12)
res[2] = CH.K(40)/2
res[3] = CH.K(41)/2
res[4] = CH.K(42)/2
res[5] = CH.K(43)/2
res[6] = CH.K(44)/2
res[7] = CH.K(45)/2
res[8] = CH.K(46)/2
res[9] = CH.K(47)/2
res[10] = CH.K(48)/2
res[11] = CH.K(49)/2
res[12] = CH.K(50)/2

plot(res,type = "l")

CHL.K <- function(Kaux){ ### AQUI has de tocar
  CH.K <- 0
  for (i in 1:2) {
    kmeans.K <- cclust (as.matrix(numericalVariables),Kaux,iter.max=100,method="kmeans",dist="euclidean")
    (CH.K = CH.K + clustIndex(kmeans.K,numericalVariables, index="likelihood")) ## aqui has de canviar l'index
  }
  return(CH.K)
}

# A res em guardo les mitjanes de l'experiment per tots els valors de K.
resl <- vector("numeric",12)
resl[2] = CH.K(40)/2
resl[3] = CH.K(41)/2
resl[4] = CH.K(42)/2
resl[5] = CH.K(43)/2
resl[6] = CH.K(44)/2
resl[7] = CH.K(45)/2
resl[8] = CH.K(46)/2
resl[9] = CH.K(47)/2
resl[10] = CH.K(48)/2
resl[11] = CH.K(49)/2
resl[12] = CH.K(50)/2
labels <- c(40:50)
plot(resl,labels,type = "l")


kmeans.k <- cclust(as.matrix(numericalVariables),50,iter.max = 200,method = "kmeans",dist = "euclidean")
print(kmeans.k)
clustIndex(kmeans.k,numericalVariables, index="likelihood")