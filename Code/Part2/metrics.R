#Name: Owais Tariq
#Student ID: 10039105
library("lpSolveAPI")
library("fpc") 
library("cluster")

#function to return metrics in a vector
getMetrics = function(x,dMat){
  data = read.csv(file="cw_dataset.csv", header=TRUE, sep=",")
  
  #handle case where instance selection is used
  if(length(x) != length(data[,1])){
    dataAIs = read.csv(file="data attribute and instance selection.csv", header=TRUE, sep=",")
    data = data[data$Sample_ID %in% dataAIs$Sample_ID,]
  }
  
  stats = cluster.stats(dMat,x)
  ## between-cluster distance/inter
  bD = stats$average.between
  ##within-cluster distance/ intra
  wD = stats$average.within
  ##avg cluster diameter
  cD = mean(stats$diameter)
  ##avg cluster distance
  dD = mean(stats$average.distance)
  ##Dun
  dunn = stats$dunn
  entropy = stats$entropy
  ##allign the matrix for the method
  allignedRes = allignDiag(table(data$Class,x))
  ##get the matrix
  alM = allignedRes[[1]]
  #get the accuracy
  acur = allignedRes[[2]]
  #get the Recall
  instC = c(118,133,152,152,169)
  rec = mean((diag(alM)/rowSums(alM)))
  #precesion
  pres = mean((diag(alM)/colSums(alM)))
  f1 =  2 *((pres*rec)/(pres+rec))
  #purity
  purity = sum(apply(alM, 2, max))/sum(alM)
  
  tr = cbind(bD,wD,cD,dD,dunn,entropy,acur,rec,pres,f1,purity)
  return(tr)
}