rm(list=ls(all=TRUE))

#make sure you use R 3.6 or higher
library(help = "datasets")

if(!require("bootnet")){
  install.packages("bootnet")
  library("bootnet")
}

if(!require("qgraph")){
  install.packages("qgraph")
  library("qgraph")
}

if(!require("KernSmoothIRT")){
  install.packages("KernSmoothIRT")
  library("KernSmoothIRT")
}


data(BDI)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#1. Inspect BDI dataset, 
scores=rowSums(BDIresponses)
hist(scores,30)


#2 how many at least minimally /mildly/ moderately / severely depressed?
scores2=na.exclude(scores)
sum(scores2>8)
sum(scores2>13)
sum(scores2>19)
sum(scores2>28)



#3. Build a Network without regularization
NetworkBDI <- estimateNetwork(BDIresponses, default = "pcor")
#3a)
plot(NetworkBDI)
#3b)
sum(NetworkBDI$graph > 0)/2
#3c)
# There are 121 edges estimated using only 242 observations for each node. 
# --> Results might be very specific for the current sample.


#4. Regularized model
NetworkLassoBDI <- estimateNetwork(BDIresponses, default = "EBICglasso")
#4a)
sum(NetworkLassoBDI$graph > 0)/2

#5. Effect of lambda parameters
NetworkLassoBDI1 <- estimateNetwork(BDIresponses, default = "EBICglasso", lambda.min.ratio=0.2)
NetworkLassoBDI2 <- estimateNetwork(BDIresponses, default = "EBICglasso", lambda.min.ratio=0.5)

plot(NetworkLassoBDI1, layout = "circular" , labels = T)
plot(NetworkLassoBDI2, layout = "circular" , labels = T)

#5a minimum Lambda
min(NetworkLassoBDI1$results$lambda)
max(NetworkLassoBDI1$results$lambda)
min(NetworkLassoBDI2$results$lambda)
max(NetworkLassoBDI2$results$lambda)


#5b
# 100 lambda vaues are tested, network with best EBIC is chosen
MinEBIC1 = which.min(NetworkLassoBDI1$results$ebic)
MinEBIC2 = which.min(NetworkLassoBDI2$results$ebic)
NetworkLassoBDI1$results$lambda[MinEBIC1]
NetworkLassoBDI2$results$lambda[MinEBIC2]

#5c
#Lamba doesnt change the number of nodes, only edges!

# How many edges?
sum(NetworkLassoBDI1$graph > 0)/2
sum(NetworkLassoBDI2$graph > 0)/2

#6 Investigate the network properties

#6a
# which symptoms are most relevant?
centralityPlot(NetworkLassoBDI2)

# -> 1,3,4,7,10
# Traurigkeit, Frühere Misserfolge, Verlust von Freude, Abneigung gegen sich selbst, Weinen

#6b
# which 5 edges / connections are most relevant?
g<-NetworkLassoBDI2$graph

g2<-g
indSav<-matrix(0,6,2)
maxval<-matrix(0,6,1)

for (i in 1:6) {
  
  ind<- which(g2==max(g2),arr.ind=TRUE)
  
  indSav[i,1]<-ind[1,1]
  indSav[i,2]<-ind[1,2]

  maxval[i]<-g2[ind[1,1], ind[1,2]] #not really needed by task description
  g2[ind[1,1],ind[1,2]]<- 0
  g2[ind[2,1],ind[2,2]]<- 0

}

#Most important connections are:
# Frühere Misserfolge - Abneigung gegen sich selbst
# Weinen - Verlust von Freude
# Pessimismus - Traurigkeit
# Selbstmordgedanken oder -wünsche - Traurigkeit
# Selbstvorwürfe - Frühere Misserfolge
# Selbstvorwürfe - Abneigung gegen sich selbst


