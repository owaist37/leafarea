#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")

q6ii = function(data){
  #correlation selection
  #sum(is.na(data$Leaf.weight))/length(data[,1])
  dataAs = data[,-17]
  #corelationWhole =  cor(dataAs[complete.cases(dataAs),c(2:18)])
  corelationWhole = cor(dataAs[,c(-1,-19)], use = "complete.obs")
  #dataAs without nas to work out values
  dataAsW = dataAs[complete.cases(dataAs),c(7:16)]
  #
  cor2 = cor(dataAsW)
  #write to csv for report
  #write.csv(cor(dataAsW), file="CorRaw.csv")
  #function to work out values
  pvals = matrix(0,ncol(dataAsW),ncol(dataAsW))
  for ( i in 1:ncol(dataAsW)){
    for (j in i:ncol(dataAsW))  {
      pvals[i,j] = cor.test(dataAsW[,i],dataAsW[,j], method ="pearson")$p.value
    }
  }
  pvals[lower.tri(pvals)] = t(pvals)[lower.tri(pvals)]
  pvals = as.data.frame(pvals)
  #write to csv for report
  #write.csv(pvals, file="pvalsRaw.csv")
  
  pvals<0.005
  #removing appropriate columns
  dataAs = data[,c(1:2,4:6,9,13,16:20)]
  
  meanVals = dataAs %>%
    group_by(Class) %>%
    summarise_all(mean,na.rm=TRUE)
  #changing column names for join
  colnames(meanVals) = paste(colnames(meanVals),"mean",sep='')
  #replacing values using join
  dataCs = dataAs %>%
    left_join(.,meanVals, by= c("Class" = "Classmean")) %>%
    mutate(Mass = ifelse(is.na(Mass),Massmean,Mass)) %>%
    mutate(Width = ifelse(is.na(Width),Widthmean,Width)) %>%
    mutate(Depth = ifelse(is.na(Depth),Depthmean,Depth)) %>%
    mutate(Orientation..2 = ifelse(is.na(Orientation..2),Orientation..2mean,Orientation..2)) %>%
    mutate(Orientation..6 = ifelse(is.na(Orientation..6),Orientation..6mean,Orientation..6)) %>%
    mutate(Orientation..9 = ifelse(is.na(Orientation..9),Orientation..9mean,Orientation..9)) %>%
    mutate(Leaf.weight = ifelse(is.na(Leaf.weight),Leaf.weightmean,Leaf.weight)) %>%
    mutate(Leaf.Area = ifelse(is.na(Leaf.Area),Leaf.Areamean,Leaf.Area)) %>%
    mutate(Leaf.Hue = ifelse(is.na(Leaf.Hue),Leaf.Huemean,Leaf.Hue)) %>%
    select(colnames(dataAs))
  rm(meanVals)
  
  write.csv(dataCs, file="data correlation selection.csv",row.names=FALSE)
  toReturn = list(corelationWhole, cor2,pvals)
  names(toReturn) = c("correlations only a subset used in report","correlations for the orientation attributes","p values for orientation attribute correlations")
  return(toReturn)
}






