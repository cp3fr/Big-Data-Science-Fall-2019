
if(!require("bootnet")){
  install.packages("bootnet")
  library("bootnet")
}

if(!require("qgraph")){
  install.packages("qgraph")
  library("qgraph")
}


Data = readRDS('NetworkData.rds')

# pairwise Markov random field
# When data are multivariate normal, such a conditional independence would correspond to a partial correlation being equal to zero. 
# multivariate normal density: Gaussian graphical model: edges: partial correlation coefficients
# estimate of the covariance matrix as input

# One problem: small N (10 node network = 45 pairs to be estimated) 
# Solution: Regularization LASSO

# dataset of 359 women enrolled in community-based substance abuse treatment programs across the United States

#1. Show a Network without regularization
NetworkFull <- estimateNetwork(Data, default = "pcor")

plot(NetworkFull, layout = "circular" , labels = T)

# How many edges? 
sum(NetworkFull$graph > 0)/2


# One problem: small N for this amount of free parameters (edges) to estimate
# Solution: Regularization LASSO

#2.Regularize the model:
#First : use bonferroni correction when estimating partical correlations, only use edges surviving that

NetworkBonf <- estimateNetwork(Data, default = "pcor", threshold = 'bonferroni', alpha = 0.05)
plot(NetworkBonf)

# Now do porper Lasso regression
NetworkLasso <- estimateNetwork(Data, default = "EBICglasso")
plot(NetworkLasso, layout = "circular" , labels = T)

# looks a lot better, at least almost all negative edges gone
# How many edges?
sum(NetworkLasso$graph > 0)/2

# which lambda was chosen? 
# EBIC is extended Bayesian information criteria
MinEBIC = which.min(NetworkLasso$results$ebic)
NetworkLasso$results$lambda[MinEBIC]

#3 Calculate the network properties
# which symptoms are most relevant?

# (A) Node strength / Degree: sum of all edges of a node with all other directly connected nodes of the network
# (B) Closeness: how strongly a node is indirectly connected to other nodes in the network?
# sum of all the shortest paths between one node and all other nodes in the network
# C) betweenness looks at how many of the shortest paths between two nodes go
# through the node in question; the higher the betweenness, the more important a
# node is in connecting other node
centrality(NetworkLasso)
centralityPlot(NetworkLasso,include=c("Betweenness","Closeness","Strength"))

#4. Assess the accuracy of edge-weights
bootNetworkLasso <- bootnet(NetworkLasso, nBoots = 100, nCores = 4)

#sizable bootstrapped CIs around the estimated edge-weights, indicating that
#many edge-weights likely do not significantly differ from one-another. The
#generally large bootstrapped CIs imply that interpreting the order of most
#edges in the network should be done with care. 16 - 17, 3 - 4, 5 - 11 

plot(bootNetworkLasso, labels = TRUE, order = "sample")

# Centrality 
# C S (cor = 0.7) represent the maximum proportion of cases that can be dropped,
#such that with 95 % probability the correlation between original centrality
#indices and centrality of networks based on subsets is 0.7 or higher. 0.5 is ok.
bootNetworkLasso2 <- bootnet(NetworkLasso, nBoots = 100, type = "case", nCores = 4)


plot(bootNetworkLasso2)


corStability(bootNetworkLasso2)

#5 Compare if edges differ significantly 
# here we take a look at the node strenght
differenceTest(nodeootNetworkLasso, 3, 17, "strength")
differenceTest(bootNetworkLasso, 1, 16, "strength")



plot(bootNetworkLasso, "strength")
#diagnoal is node strength

# Edges differ significantly?
# For example, the following code plots the difference tests of node strength between all pairs of edge-weights:
plot(bootNetworkLasso, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")




