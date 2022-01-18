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
  #function to correct column names and add class
  accc = function(dataset){
    dataset$Class = data$Class
    colnames(dataset) = colnames(data)
    return(dataset)
  }
  #function to create mean centred data
  Mc = function(data1){
    #Mean Centered, does not have sd1
    data1 = data.frame(cbind(data1[,1], scale(data1[2:19], scale=FALSE, center = TRUE)))
    data1 = accc(data1)
    return(data1)
  }
  #STANDARDISATION, has sd 1 and mean 0
  Strd = function(data1){
    data1 = data.frame(cbind(data1[,1], scale(data1[2:19], scale=TRUE)))
    data1 = accc(data1)
    return(data1)
  }
  #Nomralisation 0 
  Norm0 = function(data1){
    data1 = data.frame(cbind(data1[,1], normaliseZero(data1[2:19])))
    data1 = accc(data1)
    return(data1)
  }
  #Nomralisation 1 
  Norm1 = function(data1){
    data1 = data.frame(cbind(data0rep[,1], normaliseOne(data0rep[2:19])))
    data1 = accc(data1)
    return(data1)
  }
  #NS 0 rep
  NS0 = function(data1){
    dataN1 = scale(data0rep[2:19])
    data1 = data.frame(cbind(data1[,1], normaliseOne(dataN1)))
    data1 = accc(data1)
    return(data1)
  }
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
  data0repMc = Mc(data0rep)
  write.csv(data0repMc, file="0 replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  data0repS = Strd(data0rep)
  write.csv(data0repS, file="0 replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  data0repN0 = Norm0(data0rep)
  write.csv(data0repN0, file="0 replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  data0repN1 = Norm1(data0rep)
  write.csv(data0repN1, file="0 replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  data0repNS = NS0(data0rep)
  write.csv(data0repNS, file="0 replacement NS.csv",row.names=FALSE)
  genTransPlots(data0rep,data0repMc,data0repS,data0repN0,data0repN1,data0repNS," 0 replacement")
  
  #for mean rep
  #Mean Centered, does not have sd1
  dataMeanRepMc = Mc(dataMeanRep)
  write.csv(data0repMc, file="mean replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  dataMeanRepS = Strd(dataMeanRep)
  write.csv(data0repS, file="mean replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  dataMeanRepN0 = Norm0(dataMeanRep)
  write.csv(data0repN0, file="mean replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  dataMeanRepN1 = Norm1(dataMeanRep)
  write.csv(data0repN1, file="mean replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  dataMeanRepNS = NS0(dataMeanRep)
  write.csv(data0repNS, file="mean replacement NS.csv",row.names=FALSE)
  genTransPlots(dataMeanRep,dataMeanRepMc,dataMeanRepS,dataMeanRepN0,dataMeanRepN1,dataMeanRepNS," mean replacement")
  
  #for mean rep
  #Mean Centered, does not have sd1
  dataMedianRepMc = Mc(dataMedianRep)
  write.csv(data0repMc, file="median replacement mc.csv",row.names=FALSE)
  #STANDARDISATION, has sd 1 and mean 0
  dataMedianRepS = Strd(dataMedianRep)
  write.csv(data0repS, file="median replacement STANDARDISATION.csv",row.names=FALSE)
  #Nomralisation 0 
  dataMedianRepN0 = Norm0(dataMedianRep)
  write.csv(data0repN0, file="median replacement Nomralisation.csv",row.names=FALSE)
  #Nomralisation 1
  dataMedianRepN1 = Norm1(dataMedianRep)
  write.csv(data0repN1, file="median replacement Nomralisation1.csv",row.names=FALSE)
  #NS 0 rep
  dataMedianRepNS = NS0(dataMedianRep)
  write.csv(data0repNS, file="median replacement NS.csv",row.names=FALSE)
  genTransPlots(dataMedianRep,dataMedianRepMc,dataMedianRepS,dataMedianRepN0,dataMedianRepN1,dataMedianRepNS," median replacement")
  
  #function given a dataset creates all the appropraite datasets. Approach did not work, not viable
  #transformData = function(dataset,fileName,dataName){
  #create all the dataset
  #assign(paste(dataName,"Mc",sep=""), Mc(dataset))
  #write.csv(data0repMc, file=paste(paste(fileName,"replacement", "Mc",sep=" "),".csv",sep=""),row.names=FALSE)
  #assign(paste(dataName,"S",sep=""), Mc(dataset))
  #write.csv(data0repMc, file=paste(paste(fileName,"replacement", "S",sep=" "),".csv",sep=""),row.names=FALSE)
  #assign(paste(dataName,"N0",sep=""), Mc(dataset))
  #write.csv(data0repMc, file=paste(paste(fileName,"replacement", "N0",sep=" "),".csv",sep=""),row.names=FALSE)
  #assign(paste(dataName,"N1",sep=""), Mc(dataset))
  #write.csv(data0repMc, file=paste(paste(fileName,"replacement", "N1",sep=" "),".csv",sep=""),row.names=FALSE)
  #assign(paste(dataName,"NS",sep=""), Mc(dataset))
  #write.csv(data0repMc, file=paste(paste(fileName,"replacement", "NS",sep=" "),".csv",sep=""),row.names=FALSE)
  #}
  #transformData(data0rep,"0","data0rep")
}











