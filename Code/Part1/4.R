#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")


q4 = function(data){
  #function to create plots for the replacement processes 
  #creattion of plots and new dataframes to make plot creation easier
  #all inputs are strings other than dataSet which is the dataset to use
  #single method for one attribute
  genRepPlot = function(dataSet,attribute,replacementMethod){
    dfD01 = data%>%
      select(toString(attribute)) %>%
      mutate(Data.set = "Original")
    
    dfD02 = dataSet%>%
      select(toString(attribute)) %>%
      mutate(Data.set = toString(replacementMethod))
    
    dfD0 = rbind(dfD01,dfD02)
    
    plotTs = ggplot(dfD0, aes(dfD0[,1], fill=Data.set)) + geom_density(color = "black", alpha = 0.4) + scale_fill_manual(values = c("#E69F00", "#56B4E9")) + ggtitle(paste("Density plot for the ",attribute," comparing the ",replacementMethod," transformation", sep="")) + xlab(toString(attribute)) +  theme(text = element_text(size=20))
    ggsave(filename =paste(paste("tr",replacementMethod,attribute,sep=""),".png",sep=""),plot=plotTs)   
    #remove to keep workspace clean
    rm(dfD01,dfD02,dfD0)
  }
  
  #multiple attribute method for the three attributes that are for the report
  genRepPlots = function(dataSet1,replacementMethod1){
    attributes = c("Depth","Leaf.weight","Orientation..9")
    dataSet2 = list(dataSet1,dataSet1,dataSet1)
    replacementMethod2 = list(replacementMethod1,replacementMethod1,replacementMethod1)
    mapply(genRepPlot,dataSet2,attributes,replacementMethod1)
  }
  
  
  #0replacement
  data0rep = data
  data0rep[is.na(data0rep)] = 0
  write.csv(data0rep, file="0 replacement.csv",row.names=FALSE)
  #check
  s1 = summary(data0rep)
  #graphs for 0rep
  genRepPlots(data0rep,"0 replacement")
  
  #Mean replacment
  #mean values of each of the groups 
  meanVals = data %>%
    group_by(Class) %>%
    summarise_all(mean,na.rm=TRUE)
  #changing column names for join
  colnames(meanVals) = paste(colnames(meanVals),"mean",sep='')
  #replacing values using join
  dataMeanRep = data %>%
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
  rm(meanVals)
  genRepPlots(dataMeanRep,"mean replacement")
  write.csv(dataMeanRep, file="mean replacement.csv",row.names=FALSE)
  s2 = summary(dataMeanRep)
  
  
  #Median Replacement
  medianVals = data %>%
    group_by(Class) %>%
    summarise_all(median,na.rm=TRUE)
  #changing column names for join
  colnames(medianVals) = paste(colnames(medianVals),"median",sep='')
  #replacing values using join
  dataMedianRep = data %>%
    left_join(.,medianVals, by= c("Class" = "Classmedian")) %>%
    mutate(Mass = ifelse(is.na(Mass),Massmedian,Mass)) %>%
    mutate(Width = ifelse(is.na(Width),Widthmedian,Width)) %>%
    mutate(Depth = ifelse(is.na(Depth),Depthmedian,Depth)) %>%
    mutate(Orientation..0 = ifelse(is.na(Orientation..0),Orientation..0median,Orientation..0)) %>%
    mutate(Orientation..1 = ifelse(is.na(Orientation..1),Orientation..1median,Orientation..1)) %>%
    mutate(Orientation..2 = ifelse(is.na(Orientation..2),Orientation..2median,Orientation..2)) %>%
    mutate(Orientation..3 = ifelse(is.na(Orientation..3),Orientation..3median,Orientation..3)) %>%
    mutate(Orientation..4 = ifelse(is.na(Orientation..4),Orientation..4median,Orientation..4)) %>%
    mutate(Orientation..5 = ifelse(is.na(Orientation..5),Orientation..5median,Orientation..5)) %>%
    mutate(Orientation..6 = ifelse(is.na(Orientation..6),Orientation..6median,Orientation..6)) %>%
    mutate(Orientation..7 = ifelse(is.na(Orientation..7),Orientation..7median,Orientation..7)) %>%
    mutate(Orientation..8 = ifelse(is.na(Orientation..8),Orientation..8median,Orientation..8)) %>%
    mutate(Orientation..9 = ifelse(is.na(Orientation..9),Orientation..9median,Orientation..9)) %>%
    mutate(Leaf.weight = ifelse(is.na(Leaf.weight),Leaf.weightmedian,Leaf.weight)) %>%
    mutate(Leaf.Area = ifelse(is.na(Leaf.Area),Leaf.Areamedian,Leaf.Area)) %>%
    mutate(Leaf.Hue = ifelse(is.na(Leaf.Hue),Leaf.Huemedian,Leaf.Hue)) %>%
    select(colnames(data))
  rm(medianVals)
  write.csv(dataMedianRep, file="median replacement.csv",row.names=FALSE)
  genRepPlots(dataMedianRep,"median replacement")
  s3 = summary(data0rep)
  
  #working out the sd for every attribute
  sd1 = apply(data0rep[,2:19], 2, sd,na.rm=TRUE)
  #working out the sd for every attribute
  sd2 = apply(dataMeanRep[,2:19], 2, sd,na.rm=TRUE)
  #working out the sd for every attribute
  sd3 = apply(dataMedianRep[,2:19], 2, sd,na.rm=TRUE)
  
  
  toReturn = list(data0rep,s1,sd1,dataMeanRep,s2,sd2,dataMedianRep,s3,sd3)
  names(toReturn) = c("0 replaced data","0 replaced data summary","0 rep sds","mean replaced data","mean replaced data summary","mean rep sds","median replaced data","median replaced data summary","median rep sds")
  return(toReturn)
}








