#Name: Owais Tariq
#Student ID: 10039105#libraries
library ("ggplot2")
library ("dplyr")

q6i = function(data){
  #Attribute selection 
  #instances with missing values
  a = rowSums(is.na(data))
  a=a[a!=0]
  naR = length(a)
  rm(a)
  nar0 =  as.data.frame(table(rowSums(is.na(data))))
  #(max(naR)/length(data[2:19]))*100
  #min(naR)
  #(71+338+15)/length(data[,1])
  
  #removing instances that have more 2 mising values and replacing the others with class mean
  nainR = rowSums(is.na(data));
  dataIs = data[nainR<3,]
  #check to see only 2 missing valuesg
  naR2 = rowSums(is.na(dataIs))
  max(naR2)
  rm(naR2)
  
  #relacing with class means
  meanVals = dataIs %>%
    group_by(Class) %>%
    summarise_all(mean,na.rm=TRUE)
  #changing column names for join
  colnames(meanVals) = paste(colnames(meanVals),"mean",sep='')
  #replacing values using join
  dataIs = dataIs %>%
    left_join(.,meanVals, by= c("Class" = "Classmean")) %>%
    mutate(Mass = ifelse(is.na(Mass),Massmean,Mass)) %>%
    mutate(Width = ifelse(is.na(Width),Widthmean,Width)) %>%
    mutate(Depth = ifelse(is.na(Depth),Depthmean,Depth)) %>%
    mutate(Orientation..0 = ifelse(is.na(Orientation..0),Orientation..0mean,Orientation..0)) %>%
    mutate(Orientation..1 = ifelse(is.na(Orientation..1),Orientation..1mean,Orientation..1)) %>%
    mutate(Orientation..2 = ifelse(is.na(Orientation..2),Orientation..2mean,Orientation..2)) %>%
    mutate(Orientation..3 = ifelse(is.na(Orientation..3),Orientation..3mean,Orientation..3)) %>%
    mutate(Orientation..4 = ifelse(is.na(Orientation..4),Orientation..4mean,Orientation..4)) %>%
    mutate(Orientation..5 = ifelse(is.na(Orientation..5),Orientation..5mean,Orientation..5)) %>%
    mutate(Orientation..6 = ifelse(is.na(Orientation..6),Orientation..6mean,Orientation..6)) %>%
    mutate(Orientation..7 = ifelse(is.na(Orientation..7),Orientation..7mean,Orientation..7)) %>%
    mutate(Orientation..8 = ifelse(is.na(Orientation..8),Orientation..8mean,Orientation..8)) %>%
    mutate(Orientation..9 = ifelse(is.na(Orientation..9),Orientation..9mean,Orientation..9)) %>%
    mutate(Leaf.weight = ifelse(is.na(Leaf.weight),Leaf.weightmean,Leaf.weight)) %>%
    mutate(Leaf.Area = ifelse(is.na(Leaf.Area),Leaf.Areamean,Leaf.Area)) %>%
    mutate(Leaf.Hue = ifelse(is.na(Leaf.Hue),Leaf.Huemean,Leaf.Hue)) %>%
    select(colnames(data))
  rm(meanVals,naR)
  dataIs = dataIs[,-17] 
  write.csv(dataIs, file="data attribute and instance selection.csv",row.names=FALSE)
}





