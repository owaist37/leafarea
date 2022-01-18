#Name: Owais Tariq
#Student ID: 10039105
#function for question 1 i

q1i = function(data){

  #number of distinct element in each attribute
  ndistinct = function(x){
    return(length(unique(x)))
  }

  #look at dataset
  str(data)
  #retreive summary statistics

  #removing repeated instances
  #data = data[!duplicated(data[,c(-1,-17)], rm.na=TRUE),]


  #look at the summary data
  summaryOfData = summary(data)
  #save into CSV so they can be added into the report
  #write.csv(summary(data), file="SummaryRaw.csv")

  #need a fucntion to calculate the mode (univariate mode only)
  mode = function(x){
    #remove nas
    x = x[!is.na(x)]
    values = unique(x)
    currentMax = 0;
    modeVal = NA;
    for(i in 1:length(values)){
      val = sum(values[i] == x)
      #condition for when val is equal to NA
      if(is.na(val)){
        val=0;
      }
      if(val>currentMax){
        currentMax=val;
        modeVal = values[i];
      }
    }
    return(modeVal)
  }
  
  #number of instances with a missing value
  a = rowSums(is.na(data))
  a=a[a!=0]
  naR = length(a)
  
  #working out the mode for every attribute
  modes = apply(data, 2, mode)
  #checking to make sure values are repeated
  length(data[data$Width == 226,][,1])
  length(data[data$Depth == 694,][,1])
  
  #working out the sd for every attribute
  sds = apply(data[,2:19], 2, sd,na.rm=TRUE)
  

  toReturn = list(summaryOfData,naR,modes,sds)
  names(toReturn) = c("summary","number of instances with missing values","modal Values","standard deviations")
  
  return(toReturn)
  
  
}

























  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

