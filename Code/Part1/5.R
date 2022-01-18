#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")



#TRANSFORMING THE DATA

q5 = function(data0rep,dataMeanRep,dataMedianRep){
  #function to normalise data between 0 and 1
  normaliseZero = function(x){
    return(((x-min(x))/(max(x)-min(x))))
  }
  normaliseOne = function(x){
    return((2*x-min(x)-max(x))/(max(x)-min(x)))
  }
  
  #Graphs for transforming the data
  genTransPlots = function(dataSet,dataMC,dataS,dataSN0,dataSN1,dataSNS,method){
    #Only works with par, cant plot on the same graph due to the different scales
    dfD1 = dataSet%>%
      select("Orientation..9") %>%
      mutate(Data.set = "Original")
    
    dfD2 = dataMC%>%
      select("Orientation..9") %>%
      mutate(Data.set = paste(method, " mean centered", sep=""))
    
    dfD3 = dataS%>%
      select("Orientation..9") %>%
      mutate(Data.set =  paste(method, " standardised", sep=""))
    
    dfD4 = dataSN0%>%
      select("Orientation..9") %>%
      mutate(Data.set =  paste(method, " normalised [0,1]", sep=""))
    
    dfD5 = dataSN1%>%
      select("Orientation..9") %>%
      mutate(Data.set = paste(method, " normalised [-1,1]", sep=""))
    
    dfD6 = dataSNS%>%
      select("Orientation..9") %>%
      mutate(Data.set = paste(method, " normalised [0,1] and standardised", sep=""))
    
    dfD = rbind(dfD1,dfD2,dfD3,dfD4,dfD5,dfD6)
    
    trPlot=ggplot(dfD, aes(Orientation..9, fill=Data.set), hei) + geom_density(alpha = 1)  + facet_wrap(~Data.set, scales = "free") + ggtitle(paste("Density plots for Orientation..9 from the transformed, ",method," dataset",sep="")) + scale_color_brewer(palette="Dark2") +  theme(text = element_text(size=20))
    ggsave(filename =paste(paste("ts",method,sep=""),".png",sep=""),plot=trPlot, width = 27,height = 27, dpi = 300, units = "cm")
    #clean workspace
    rm(dfD1,dfD2,dfD3,dfD4,dfD5,dfD6,dfD)
  }
  
  #for 0 rep
  #Mean Centered, does not have sd1
  data0repMc = data.frame(cbind(data0rep[,1], scale(data0rep[2:19], scale=FALSE, center = TRUE)))
  data0repMc$Class = data0rep$Class
  colnames(data0repMc) = colnames(data)
  write.csv(data0repMc, file="0 replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  data0repS = data.frame(cbind(data0rep[,1], scale(data0rep[2:19], scale=TRUE)))
  data0repS$Class = data0rep$Class
  colnames(data0repS) = colnames(data)
  write.csv(data0repS, file="0 replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  data0repN0 = data.frame(cbind(data0rep[,1], normaliseZero(data0rep[2:19])))
  data0repN0$Class = data0rep$Class
  colnames(data0repN0) = colnames(data)
  write.csv(data0repN0, file="0 replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  data0repN1 = data.frame(cbind(data0rep[,1], normaliseOne(data0rep[2:19])))
  data0repN1$Class = data0rep$Class
  colnames(data0repN1) = colnames(data)
  write.csv(data0repN1, file="mean replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  dataN1 = scale(data0rep[2:19])
  data0repNS = data.frame(cbind(data0rep[,1], normaliseOne(dataN1)))
  data0repNS$Class = data0rep$Class
  colnames(data0repNS) = colnames(data)
  write.csv(data0repNS, file="mean replacement NS.csv",row.names=FALSE)
  rm(dataN1)
  
  genTransPlots(data0rep,data0repMc,data0repS,data0repN0,data0repN1,data0repNS," 0 replacement")
  
  #For mean replaced
  #Mean Centered, does not have sd1
  dataMeanRepMc = data.frame(cbind(dataMeanRep[,1], scale(dataMeanRep[2:19], scale=FALSE, center = TRUE)))
  dataMeanRepMc$Class = dataMeanRep$Class
  colnames(dataMeanRepMc) = colnames(data)
  write.csv(dataMeanRepMc, file="mean replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  dataMeanRepS = data.frame(cbind(dataMeanRep[,1], scale(dataMeanRep[2:19], scale=TRUE)))
  dataMeanRepS$Class = dataMeanRep$Class
  colnames(dataMeanRepS) = colnames(data)
  write.csv(dataMeanRepS, file="mean replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  dataMeanRepN0 = data.frame(cbind(dataMeanRep[,1], normaliseZero(dataMeanRep[2:19])))
  dataMeanRepN0$Class = dataMeanRep$Class
  colnames(dataMeanRepN0) = colnames(data)
  write.csv(dataMeanRepN0, file="mean replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  dataMeanRepN1 = data.frame(cbind(dataMeanRep[,1], normaliseOne(dataMeanRep[2:19])))
  dataMeanRepN1$Class = dataMeanRep$Class
  colnames(dataMeanRepN1) = colnames(data)
  write.csv(dataMeanRepN1, file="mean replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  dataN1 = scale(dataMeanRep[2:19])
  dataMeanRepNS = data.frame(cbind(dataMeanRep[,1], normaliseOne(dataN1)))
  dataMeanRepNS$Class = dataMeanRep$Class
  colnames(dataMeanRepNS) = colnames(data)
  write.csv(dataMeanRepNS, file="mean replacement NS.csv",row.names=FALSE)
  rm(dataN1)
  genTransPlots(dataMeanRep,dataMeanRepMc,dataMeanRepS,dataMeanRepN0,dataMeanRepN1,dataMeanRepNS," mean replacement")
  
  #For median replaced
  #Mean Centered, does not have sd1
  dataMedianRepMc = data.frame(cbind(dataMedianRep[,1], scale(dataMedianRep[2:19], scale=FALSE, center = TRUE)))
  dataMedianRepMc$Class = dataMedianRep$Class
  colnames(dataMedianRepMc) = colnames(data)
  write.csv(dataMedianRepMc, file="median replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  dataMedianRepS = data.frame(cbind(dataMedianRep[,1], scale(dataMedianRep[2:19], scale=TRUE)))
  dataMedianRepS$Class = dataMedianRep$Class
  colnames(dataMedianRepS) = colnames(data)
  write.csv(dataMedianRepS, file="median replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  dataMedianRepN0 = data.frame(cbind(dataMedianRep[,1], normaliseZero(dataMedianRep[2:19])))
  dataMedianRepN0$Class = dataMedianRep$Class
  colnames(dataMedianRepN0) = colnames(data)
  write.csv(dataMedianRepN0, file="median replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  dataMedianRepN1 = data.frame(cbind(dataMedianRep[,1], normaliseOne(dataMedianRep[2:19])))
  dataMedianRepN1$Class = dataMedianRep$Class
  colnames(dataMedianRepN1) = colnames(data)
  write.csv(dataMedianRepN1, file="median replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  dataN1 = scale(dataMedianRep[2:19])
  dataMedianRepNS = data.frame(cbind(dataMedianRep[,1], normaliseOne(dataN1)))
  dataMedianRepNS$Class = dataMedianRep$Class
  colnames(dataMedianRepNS) = colnames(data)
  write.csv(dataMedianRepNS, file="median replacement NS.csv",row.names=FALSE)
  genTransPlots(dataMedianRep,dataMedianRepMc,dataMedianRepS,dataMedianRepN0,dataMedianRepN1,dataMedianRepNS," median replacement")
  rm(dataN1)
}











