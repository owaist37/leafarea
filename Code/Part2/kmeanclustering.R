#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")

#source function to run k means k times
source("./kmeansRs.R")
#test values 


kmeanclustering = function(dataMeanRepN0){
  
  #K-means
  #km = kmeans(dataMeanRepN0[,2:19],5,iter.max=100) #applies k-means with 3 clusters and 100 iterations
  #kmClustRes = km$cluster
  #kmr = getMetrics(kmClustRes,dist(dataMeanRepN0[,2:19]))
  #rm(km,kmClustRes)
  print('Running k means for 100 iterations and taking an average')
  kmr = runKmeans(dataMeanRepN0[,2:19],5,100)
  
  #find the best centroids
  #randomly pick 5 centroids, ocasially crashes due to randomly 
  bestC = NULL
  bestAccC = 0
  accC=0
  set.seed(13)
  print("finding best centroids example")
  #1000 in example to allow for decent runtime, seed set to avoid error
  for (i in 1:1000) { #10000
    points = sample(c(1:724),5)
    #print(points)
    centC = dataMeanRepN0[points,2:19]
    #carry out k means on those cenroids
    kmC = kmeans(dataMeanRepN0[,2:19],centers = centC,iter.max=100)
    accC = allignDiag(table(dataMeanRepN0$Class,kmC$cluster))[[2]]
    if(accC > bestAccC){
      bestAccC=accC
      bestC = centC
    }
  }
  #not equal to best c anymore so it can be run more than once and ge the same results
  kmCentersIds = c(416,178,604,48,422) #  bestC
  kmCenters = dataMeanRepN0[kmCentersIds,][,2:19]
  
  
  rm(bestC,bestAccC,points,centC,kmC,accC,i)
  print("Performing k-means for up to 100 iterations over a number of methods")
  methods = factor(c("Hartigan-Wong","Lloyd","MacQueen"))
  kmHv = rep(0,12)
  kmLV = rep(0,12)
  kmMcV = rep(0,12)
  #optimising over iterations 
  for (i in 1:100) {
    kmH = kmeans(dataMeanRepN0[,2:19],centers = kmCenters,algorithm = "Hartigan-Wong",iter.max=i)
    kmL = kmeans(dataMeanRepN0[,2:19],centers = kmCenters,algorithm = "Lloyd",iter.max=i)
    kmMc = kmeans(dataMeanRepN0[,2:19],centers = kmCenters,algorithm = "MacQueen",iter.max=i)
    
    kmHv = rbind(kmHv,c(i,getMetrics(kmH$cluster,dist(dataMeanRepN0[,2:19]))))
    kmLV = rbind(kmLV,c(i,getMetrics(kmL$cluster,dist(dataMeanRepN0[,2:19]))))
    kmMcV = rbind(kmMcV,c(i,getMetrics(kmMc$cluster,dist(dataMeanRepN0[,2:19]))))
  }
  #warnings from algorithm not converging 
  print("Saving data")
  colnames(kmHv) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  kmHv=kmHv[-1,]
  write.csv(kmHv,"Hartigan-Wong results.csv")
  colnames(kmLV) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  kmLV=kmLV[-1,]
  write.csv(kmLV,"Lloyd results.csv")
  colnames(kmMcV) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  kmMcV=kmMcV[-1,]
  write.csv(kmMcV,"MacQueen results.csv")
  
  
  #figures not proving useful 
  #v1 = cbind(seq(1,20,1),rep("Hartigan-Wong",20),kmHv[20,7])
  #v2 = cbind(seq(1,20,1),rep("Lloyd",20),kmLV[20,7])
  #v3 = cbind(seq(1,20,1),rep("MacQueen",20),kmMcV[20,7])
  #tab = as.data.frame(rbind(v1,v2,v3))
  #colnames(tab)= c("Iteration","Method","Value")
  #ggplot(tab, aes(x=tab$Iteration, y=tab$Value, col=tab$Method)) + geom_point()
  
  toReturn = list(kmr, kmCenters)
  names(toReturn) = c("K-means clustering metrics for section 2 part1","best centroids to start with")
  return(toReturn)
  
}
















