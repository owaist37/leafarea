#Name: Owais Tariq
#Student ID: 10039105
library ("ggplot2")
library ("dplyr")

q2 = function(data){
  #linear model for correlation for orienation 7 and 4 lm for all classes
  g1 = ggplot(data,aes(x=data$Orientation..4, y=data$Orientation..7)) + geom_point(aes(colour=data$Class),alpha=0.4) + 
    geom_smooth(na.rm=TRUE,se=FALSE,method = lm, col="black") +scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) +
    theme_minimal() + xlab("Orientation..4") + ylab("Orientation..7") + ggtitle("Scatterplot of Orientation..4 against Orientation..7 and overall correlation") + guides(colour=guide_legend(title="Class"))
  ggsave(filename ="cor74.png",plot=g1) +  theme(text = element_text(size=20))
  #Linear model per class
  g2 = ggplot(data,aes(x=data$Orientation..4, y=data$Orientation..7, col=data$Class)) + geom_point(alpha=0.4) + 
    geom_smooth(na.rm=TRUE,se=FALSE,method = lm) +scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) + 
    theme_minimal() + xlab("Orientation..4") + ylab("Orientation..7") + ggtitle("Plot of Orientation..4 against Orientation..7 with correlation per class") + guides(colour=guide_legend(title="Class"))
  ggsave(filename ="cor74v2.png",plot=g2) +  theme(text = element_text(size=20))
  rm(g1,g2)
  #correlation value of the two attributes
  correlation = cor(data$Orientation..4, data$Orientation..7,  use = "complete.obs")
  #correlations per class
  data2 = data[,c(20,11,14)]
  mats = list(data2[data2$Class == "A",c(2,3)],data2[data2$Class == "B",c(2,3)],data2[data2$Class == "C",c(2,3)],data2[data2$Class == "D",c(2,3)],data2[data2$Class == "E",c(2,3)])
  corpC = lapply(mats, cor,  use = "complete.obs")
  
  #does not give correct result
  #data[,c(20,11,14)] %>%
    #group_by(Class) %>%
    #summarise(cor(.$Orientation..4,.$Orientation..7,use = "complete.obs"))
    
  #pairs plot that was produced but not used
  #data3 = data[,c(11,13,18,20)]
  #setting up the colours to look the same as the rest of the plots
  #colsDat3 = rep("black",length(data3[,1]))
  #colsDat3[data3$Class == "A"] = "#BB2433"
  #colsDat3[data3$Class == "B"] = "#FAA916"
  #colsDat3[data3$Class == "C"] = "#143642"
  #colsDat3[data3$Class == "D"] = "#A6A7A8"
  #colsDat3[data3$Class == "E"] = "#561643"
  #png('pairs.png')
  #pairs(data3,col=colsDat3, main="Pairs plot for attributes;Orientation..4,Orientation..7 and Leaf.Area")
  #dev.off()
  #remove to keep workspace clean
  #rm(data3,colsDat3)
  
  #creating it across class
  g4 = ggplot(data,aes(x=data$Orientation..4, y=data$Class)) + geom_point(aes(colour=data$Class),alpha=0.4) +scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) +
    theme_minimal() + xlab("Orientation..4") + ylab("Class") + ggtitle("Scatterplot of Class against Orientation..4") + guides(colour=guide_legend(title="Class")) +  theme(text = element_text(size=20))
  ggsave(filename ="clasOr4.png",plot=g4) 
  
  g5 = ggplot(data,aes(x=data$Orientation..7, y=data$Class)) + geom_point(aes(colour=data$Class),alpha=0.4) +scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) +
    theme_minimal() + xlab("Orientation..7") + ylab("Class") + ggtitle("Scatterplot of Class against Orientation..6") + guides(colour=guide_legend(title="Class")) +  theme(text = element_text(size=20))
  ggsave(filename ="clasOr7.png",plot=g5) 
  
  g6 = ggplot(data,aes(x=data$Leaf.Area, y=data$Class)) + geom_point(aes(colour=data$Class),alpha=0.4) +scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) +
    theme_minimal() + xlab("Leaf.Area") + ylab("Class") + ggtitle("Scatterplot of Class against Area") + guides(colour=guide_legend(title="Class")) +  theme(text = element_text(size=20))
  ggsave(filename ="clasOrA.png",plot=g6) 

  
  #boxplot for every attribute acording to class, using loop
  #plot for all the boxplots
  #for (i in 2:(length(data)-1)) {
    #ggplot(data,aes(y=data[,i],x=Class, col=data$Class, fill=data$Class)) + geom_boxplot(alpha=0.5) +
      #scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"))+ scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000"))  +
      #guides(col=FALSE, fill = FALSE) +ylab(colnames(data)[i]) + ggtitle(paste(paste("Boxplot for the ",colnames(data)[i],sep="")," attribute for each Class",sep ="")) +  theme(text = element_text(size=20))
    #ggsave(filename =paste(paste("bp",i,sep=""),".png",sep=""),plot=last_plot())
  #}
  #using mapply to  get rid of the loop
  createBox = function(xdata,column){
   ggplot(data,aes(y=xdata,x=data$Class, col=data$Class, fill=data$Class)) + geom_boxplot(alpha=0.5) +
     scale_colour_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643"))+ 
      scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000"))  +
     guides(col=FALSE, fill = FALSE) +ylab(column) + ggtitle(paste(paste("Boxplot for the ",column,sep="")," attribute for each Class",sep ="")) + 
      theme(text = element_text(size=20))
   ggsave(filename =paste(paste("bp",column,sep=""),".png",sep=""),plot=last_plot())
  }
  
  columns = colnames(data[,c(2:19)])
  mapply(createBox,data[columns],columns)
  
  #function to count the number of NA's per row, to be used inside dply
  countNas = function(x){
    return(sum(is.na(x)))
  }
  
  #calculating number of NA's per attribute
  NApC = data %>%
    group_by(Class) %>%
    summarise_all(countNas)
  #save into csv to be used in report
  #write.csv(NApC, file="Nas.csv")
  #correlation between orientation 4 and 6
  data3 = data[!is.na(data$Orientation..4) & !is.na(data$Orientation..6),]
  cor2 = cor(data3$Orientation..4, data3$Orientation..6)
  rm(data3)
  toReturn = list(correlation,corpC,NApC,cor2)
  names(toReturn) = c("Corr of Orientation..4 and 7","Corr per clas for Orientation..4 and 7", "number of NAs","Corr of Orientation..4 and 6")
  return(toReturn)
  
  
  
}










