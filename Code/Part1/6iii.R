#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")

q6iii = function(dataMeanRepS){
  #Source Mercedes bip function
  source("./bip.R")
  #PCA
  #extract field for PCA to be carried out on
  PCAdat = dataMeanRepS[,c(4:18)]
  PCA1 =  prcomp(PCAdat,scale=T)
  sum = summary(PCA1) # shows variation explained
  #screeplot(PCA1, type="lines",main="Variance explained by each PC", ylim=c(0,8)) #dont like the look of this plot 
  yvals  = ((PCA1$sdev^2)/sum(PCA1$sdev^2))*100 #percenatge of variation explainted
  png('scree.png')
  plot(x=1:15, y=yvals, type = "l", xlab="Principle Component", ylab="Percentage of varaince explained",main = "Percentage of vairance explained by each PC")
  points(x=1:15, y=yvals, type="p",col="red")
  dev.off()
  png('pc12.png')
  palette(c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"))
  bip(PCA1,1:2, col=data$Class, main = "PC1/PC2 according to Class")
  legend("topright",legend = c("A","B","C","D","E"),col=c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"),pch=16,title = "Class")
  dev.off()
  png('pc23.png')
  bip(PCA1, 2:3,col=data$Class, main = "PC2/PC3 according to Class")
  legend("topright",legend = c("A","B","C","D","E"),col=c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"),pch=16,title = "Class")
  dev.off()
  png('pc34.png')
  bip(PCA1,3:4, col=data$Class, main = "PC3/PC4 according to Class")
  legend("topright",legend = c("A","B","C","D","E"),col=c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"),pch=16,title = "Class")
  dev.off()
  PCA8= as.data.frame(cbind(dataMeanRepS$Sample_ID,predict(PCA1,PCAdat)[,1:8]))# input data
  PCA8$Class = dataMeanRepS$Class
  #adding back the class 
  PCA10= as.data.frame(cbind(dataMeanRepS$Sample_ID,predict(PCA1,PCAdat)[,1:10]))# input data
  PCA10$Class = dataMeanRepS$Class
  
  write.csv(PCA10, file="PCA10.csv",row.names=FALSE)
  write.csv(PCA8, file="PCA8.csv",row.names=FALSE)

  toReturn = list(sum, yvals)
  names(toReturn) = c("summary of PCA","% variance explained by each PC")
  return(toReturn)

}





