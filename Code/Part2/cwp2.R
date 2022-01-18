#Name: Owais Tariq
#Student ID: 10039105
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") # libarary for cluster stats
library("cluster")


#function to maximise the diagonal of a matrix in order to get the classification accuracy 
#created by using the example from the demo on a 6x6 matrix and then downsizing
#probably a nicer way to this than solving linear equations 
allignDiag = function(x){
  #x is a table, convert into matrix for use
  x = as.matrix(x)
  model = make.lp(10,25)
  add.constraint(model, c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0),"=",1)
  add.constraint(model, c(0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0),"=",1)
  add.constraint(model, c(0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0),"=",1)
  add.constraint(model, c(0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0),"=",1)
  add.constraint(model, c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),"=",1)
  add.constraint(model, c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),"=",1)
  for (j in 1:10) {
    set.type(model, j, "binary")
  }
  set.objfn(model,as.vector(t(x)))
  lp.control(model,sense='max')
  solve(model)
  #get the accuracy
  accuracy = (get.objective(model)/sum(x))*100
  #get the alligned model
  orders = matrix(get.variables(model), nrow = 5,byrow = T)
  confusion_matrix=x[,which.max(orders[1,])]
  for (k in 2:5) {
    confusion_matrix = rbind(confusion_matrix,x[,which.max(orders[k,])])
  }
  confusion_matrix=t(confusion_matrix)
  return(list(confusion_matrix,accuracy))
}

#function to return metrics in a vector
getMetrics = function(x,dMat){
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
  ##allign the matrix for the method
  allignedRes = allignDiag(table(dataMeanRepN0$Class,x))
  ##get the matrix
  alM = allignedRes[[1]]
  #get the accuracy
  acur = allignedRes[[2]]
  #get the weighted Recall
  instC = c(118,133,152,152,169)
  Wrec = mean((diag(alM)/rowSums(alM))/instC)
  #weighted precesion
  Wpres = mean((diag(alM)/colSums(alM))/instC)
  tr = cbind(bD,wD,cD,dD,dunn,acur,Wrec,Wpres)
  return(tr)
}


#Hierarchical clustering:
hClust = hclust(dist(dataMeanRepN0[,2:19]))
dend = as.dendrogram(hClust)
plot(cut(dend, h = 0.15)$upper)
hClustRes = cutree(hClust,5) # we want 5 clusters
getMetrics(hClustRes,dist(dataMeanRepN0[,2:19]))
rm(hClust,dend,hClustRes)

#optmisation
clustM = c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
distM = c("euclidean","maximum","manhattan","canberra","binary","minkowski")
accs = rep(0,8)
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
write.csv(accs,"accs.csv")
write.csv(ordertested,"ordertested.csv")
rm(i,j,currentClustRes,currentRes,ordertested,clustM,distM,accs,currentClust)



#K-means
km = kmeans(dataMeanRepN0[,2:19],5,iter.max=100) #applies k-means with 3 clusters and 100 iterations
kmClustRes = km$cluster
getMetrics(kmClustRes,dist(dataMeanRepN0[,2:19]))
rm(km,kmClustRes)

#find the best centroids
#randomly pick 5 centroids, ocasially crashes due to randomly 
bestC = NULL
bestAccC = 0
accC=0
set.seed(1)
for (i in 1:10000) {
  points = sample(c(1:724),5)
  print(points)
  centC = dataMeanRepN0[points,2:19]
  #carry out k means on those cenroids
  kmC = kmeans(dataMeanRepN0[,2:19],centers = centC,iter.max=100)
  accC = allignDiag(table(dataMeanRepN0$Class,kmC$cluster))[[2]]
  if(accC > bestAccC){
    bestAccC=accC
    bestC = centC
  }
}
kmCenters = c(416,178,604,48,422) # = bestC
#not equal to best c anymore so it can be run more than once and ge the same results
rm(bestC,bestAccC,points,centC,kmC,accC,i)



methods = factor(c("Hartigan-Wong","Lloyd","MacQueen"))


kmHv = rep(0,9)
kmLV = rep(0,9)
kmMcV = rep(0,9)
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

colnames(kmHv) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion")
kmHv=kmHv[-1,]
write.csv(kmHv,"kmHv.csv")
colnames(kmLV) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion")
kmLV=kmLV[-1,]
write.csv(kmLV,"kmLV.csv")
colnames(kmMcV) = c("Iteration","Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion")
kmMcV=kmMcV[-1,]
write.csv(kmMcV,"kmMcV.csv")


#figures not proving useful 
#v1 = cbind(seq(1,20,1),rep("Hartigan-Wong",20),kmHv[20,7])
#v2 = cbind(seq(1,20,1),rep("Lloyd",20),kmLV[20,7])
#v3 = cbind(seq(1,20,1),rep("MacQueen",20),kmMcV[20,7])
#tab = as.data.frame(rbind(v1,v2,v3))
#colnames(tab)= c("Iteration","Method","Value")
#ggplot(tab, aes(x=tab$Iteration, y=tab$Value, col=tab$Method)) + geom_point()


#PAM, gives same values each time
pm = pam(dist(dataMeanRepN0[,2:19]),5)
pmClusterRes = pm$clustering
getMetrics(pmClusterRes,dist(dataMeanRepN0[,2:19]))
pmRes = allignDiag(table(dataMeanRepN0$Class,pmClusterRes))

#optimisation
###different distances used
###medois location
###do.swap? changes algorithm to be different from the orignial 
###pamonce from 0:5 allowing for different speed of optimisation

distVs = c("euclidean","maximum","manhattan","canberra","minkowski")

pmc1R = rep(0,8)
pmc2R = rep(0,8)

for(i in 1:length(distVs)){
  pmc1  = pam(dist(dataMeanRepN0[,2:19],distVs[i]),5,do.swap = TRUE)$clustering
  pmc2  = pam(dist(dataMeanRepN0[,2:19],distVs[i]),5,do.swap = FALSE)$clustering
  pmc1R = rbind(pmc1R,getMetrics(pmc1,dist(dataMeanRepN0[,2:19],distVs[i])))
  pmc2R = rbind(pmc2R,getMetrics(pmc2,dist(dataMeanRepN0[,2:19],distVs[i])))
  
}
rm(i,pmc1,pmc2)
pmc1R=as.data.frame(pmc1R[-1,])
colnames(pmc1R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion")
pmc2R=as.data.frame(pmc2R[-1,])
colnames(pmc2R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion")
write.csv(pmc1R,"pmc1R.csv")
write.csv(pmc2R,"pmc2R.csv")


#pamonce
pmc3R = rep(0,8)
for(i in seq(0,5,1)){
  pmc3  = pam(dist(dataMeanRepN0[,2:19],"maximum"),5,do.swap = TRUE, pamonce = i)$clustering
  pmc3R = rbind(pmc3R,getMetrics(pmc3,dist(dataMeanRepN0[,2:19],"maximum")))
}
rm(i,pmc3)
pmc3R=as.data.frame(pmc3R[-1,])
colnames(pmc3R) = c("Iner-cluster distance","Intra-cluster distance","Mean cluster diameter","Mean cluster distance","Dunn index","Accuracy","Weighted Recall","Weighted Precesion") 
write.csv(pmc3R,"pmc3R.csv")

#k-means over a range of data sets
datasets = c("data0rep[,2:19]","dataMeanREp[,2:19]","dataMeanRep[,2:19]")
#running on pca,dataAIs as they are different sizes to the others
#get the centers for pca
kmm = kmeans(PCA10[,2:11],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
resultsDs = getMetrics(kmm,dist(PCA10[,2:11]))

kmm = kmeans(dataAIs[,2:11],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataAIs[,2:11]))) 

kmm = kmeans(data0rep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(data0rep[,2:19]))) 

kmm = kmeans(dataMeanRep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataMeanRep[,2:19]))) 

kmm = kmeans(dataMedianRep[,2:19],5,iter.max=40, nstart = 20,algorithm = "Lloyd")$cluster 
resultsDs =  rbind(resultsDs,getMetrics(kmm,dist(dataMedianRhahaep[,2:19]))) 
write.csv(resultsDs,"resultsDs.csv")











