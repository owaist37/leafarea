#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")

#source function to run k means k times
source("kmeansRs.R")


kmeanDS = function(PCA10,dataAIs,data0rep,dataMeanRep,dataMedianRep){
  
  
  #running on pca,dataAIs as they are different sizes to the others
  #get the centers for pca
  #kmm = kmeans(PCA10[,2:11],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
  #resultsDs = getMetrics(kmm,dist(PCA10[,2:11]))
  
  #kmm = kmeans(dataAIs[,2:11],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
  #resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataAIs[,2:11]))) 
  
  #kmm = kmeans(data0rep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
  #resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(data0rep[,2:19]))) 
  
  #kmm = kmeans(dataMeanRep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
  #resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataMeanRep[,2:19]))) 
  
  #kmm = kmeans(dataMedianRep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
  #resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataMedianRep[,2:19]))) 
  #write.csv(resultsDs,"k means across datasets results.csv")
  
  #toReturn = list(resultsDs)
  #names(toReturn) = c("results")
  #return(toReturn)
  print("running k means for 100 iterations for 5 datasets and taking average (500 iterations total) please wait")
  #repeating each 100-times then taking average
  resultsPCA = runKmeans2(PCA10[,2:11],5,100, nstart = 20,algorithm = "Lloyd")
  pca1 = resultsPCA[[1]]
  pca2 = resultsPCA[[2]]
  
  resultsAIC = runKmeans2(dataAIs[,2:11],5,100, nstart = 20,algorithm = "Lloyd")
  aic1 = resultsAIC[[1]]
  aic2 = resultsAIC[[2]]
  
  results0R = runKmeans2(data0rep[,2:19],5,100, nstart = 20,algorithm = "Lloyd")
  o1 = results0R[[1]]
  o2 = results0R[[2]]
  
  resultsmAR = runKmeans2(dataMeanRep[,2:19],5,100, nstart = 20,algorithm = "Lloyd")
  a1 = resultsmAR[[1]]
  a2 = resultsmAR[[2]]
  
  resultsmER = runKmeans2(dataMedianRep[,2:19],5,100, nstart = 20,algorithm = "Lloyd")
  e1 = resultsmER[[1]]
  e2 = resultsmER[[2]]
  
  results2 = rbind(pca1,aic1,o1,a1,e1)
  sds = rbind(pca2,aic2,o2,a2,e2)
  
  
  toReturn = list(results2, sds)
  names(toReturn) = c("K-means average metrics","K-means sds")
  return(toReturn)
}



