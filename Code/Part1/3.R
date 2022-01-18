#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")


q3 = function(data){
  #Genenearl conclusions
  #coirrelations between all attributes
  c1 = cor(data[2:19], use = "complete.obs")
  #just for orientations
  c2 = cor(data[7:16], use = "complete.obs")
  
  
  
  toReturn = list(c1, c2)
  names(toReturn) = c("correlations between all attributes","correlation between orientations")
  
  return(toReturn)
}


