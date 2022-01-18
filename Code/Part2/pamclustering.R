#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")


pamclustering = function(dataMeanRepN0){
  #PAM, gives same values each time
  pm = pam(dist(dataMeanRepN0[,2:19]),5)
  pmClusterRes = pm$clustering
  pm1 = getMetrics(pmClusterRes,dist(dataMeanRepN0[,2:19]))
  
  #optimsation of different distance metrics
  distVs = c("euclidean","maximum","manhattan","canberra","minkowski")
  pmc1R = rep(0,11)
  pmc2R = rep(0,11)
  
  for(i in 1:length(distVs)){
    pmc1  = pam(dist(dataMeanRepN0[,2:19],distVs[i]),5,do.swap = TRUE)$clustering
    pmc2  = pam(dist(dataMeanRepN0[,2:19],distVs[i]),5,do.swap = FALSE)$clustering
    pmc1R = rbind(pmc1R,getMetrics(pmc1,dist(dataMeanRepN0[,2:19],distVs[i])))
    pmc2R = rbind(pmc2R,getMetrics(pmc2,dist(dataMeanRepN0[,2:19],distVs[i])))
    
  }
  rm(i,pmc1,pmc2)
  pmc1R=as.data.frame(pmc1R[-1,])
  colnames(pmc1R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  pmc2R=as.data.frame(pmc2R[-1,])
  colnames(pmc2R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  write.csv(pmc1R,"PAM dtaince metrics and swap on.csv")
  write.csv(pmc2R,"PAM dtaince metrics and swap off.csv")
  
  #pamonce optimsation
  pmc3R = rep(0,11)
  for(i in seq(0,5,1)){
    pmc3  = pam(dist(dataMeanRepN0[,2:19],"maximum"),5,do.swap = TRUE, pamonce = i)$clustering
    pmc3R = rbind(pmc3R,getMetrics(pmc3,dist(dataMeanRepN0[,2:19],"maximum")))
  }
  rm(i,pmc3)
  pmc3R=as.data.frame(pmc3R[-1,])
  colnames(pmc3R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Entropy","Accuracy","Recall","Precesion","F1","Purity")
  write.csv(pmc3R,"PAM pamonce.csv")
  

  toReturn = list(pm1)
  names(toReturn) = c("PAM clustering metrics for section 2 part1")
  return(toReturn)
}
















