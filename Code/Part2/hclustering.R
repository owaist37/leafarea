#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")


hclustering = function(dataMeanRepN0){
  #Hierarchical clustering:
  hClust = hclust(dist(dataMeanRepN0[,2:19]))
  dend = as.dendrogram(hClust)
  #plot(cut(dend, h = 0.15)$upper)
  hClustRes = cutree(hClust,5) # we want 5 clusters
  hmetrics = getMetrics(hClustRes,dist(dataMeanRepN0[,2:19]))
  rm(hClust,dend,hClustRes)
  
  #optmisation
  clustM = c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
  distM = c("euclidean","maximum","manhattan","canberra","binary","minkowski")
  accs = rep(0,11)
  ordertested = c("a")
  for (i in 1:length(clustM)) {
    for(j in 1:length(distM)){
      currentClust = hclust(dist(dataMeanRepN0[,2:19], method = distM[j]), method = clustM[i])
      currentClustRes = cutree(currentClust,5) #get the results
      currentRes = getMetrics(currentClustRes,dist(dataMeanRepN0[,2:19], method = distM[j])) #allignDiag(table(dataMeanRepN0$Class,currentClustRes))[[2]] #return the accuracy
      accs= rbind(accs, currentRes)
      ordertested = rbind(ordertested, paste(clustM[i],distM[j],sep = " "))
    }
  }
  #write.csv(accs,"accs.csv")
  #write.csv(ordertested,"ordertested.csv")
  rm(i,j,currentClustRes,currentRes,clustM,distM,currentClust)
  
  
  
  toReturn = list(hmetrics, accs,ordertested)
  names(toReturn) = c("Hierarchical clustering metrics for section 2 part1","metrics from the optimisation","order of paramters tested")
  return(toReturn)

}

