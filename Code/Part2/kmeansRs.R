#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")
#source to get metrics
source("./ald.R")
source("./metrics.R")
#test values 

runKmeans = function(data,clusters,iterations){
  kmr = rep(0,11)
  for(i in 1:100){
    km = kmeans(data,clusters,iterations) 
    kmr = rbind(kmr,getMetrics(km$cluster,dist(data)))
  }
  kmr = kmr[-1,]
  result = apply(kmr, 2, mean)
  sds= apply(kmr,2,sd)
  
  toReturn = list(result, sds)
  names(toReturn) = c("K-means average metrics","K-means sds")
  return(toReturn)
}

runKmeans2 = function(data,clusters,iterations,nstartv,algorithmv){
  kmr = rep(0,11)
  for(i in 1:100){
    km = kmeans(data,clusters,iter.max=iterations, nstart = nstartv,algorithm = algorithmv)
    kmr = rbind(kmr,getMetrics(km$cluster,dist(data)))
  }
  kmr = kmr[-1,]
  result = apply(kmr, 2, mean)
  sds= apply(kmr,2,sd)
  
  toReturn = list(result, sds)
  names(toReturn) = c("K-means average metrics","K-means sds")
  return(toReturn)
}


