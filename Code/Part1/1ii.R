
#Name: Owais Tariq
#Student ID: 10039105
library ("ggplot2")


q1ii = function(data){
  #Histograms
  #Freedman-Diaconis' formula for bin width
  FDw = function(x){
    x = x[!is.na(x)]
    w = 2*(IQR(x)/(length(x)^(1/3)))
    return(w)
  }
  #Sturges' formula for bin width
  SRw = function(x){
    x = x[!is.na(x)]
    w = log2(length(x)) + 1
    return(w)
  }
  
  #plot for all the histograms old method not used, need to remove loops
  #for (i in 2:(length(data)-1)) {
    #ggplot(data, aes(x=data[,i], col = data$Class, fill=data$Class)) +geom_histogram(binwidth = FDw(data[,i]), alpha=0.5) + 
      #scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000")) + 
      #scale_colour_manual("fill", values=c("black","black","black","black","black"))  + theme_minimal()+ ggtitle(paste("Histogram for ",colnames(data)[i],sep="")) + 
      #xlab(colnames(data)[i]) + labs(fill="Leaf Class") + guides(col=FALSE, fill = FALSE) +
      #geom_vline(aes(xintercept = mean(data[,i],na.rm = TRUE)), colour="darkturquoise") + geom_vline(aes(xintercept = median(data[,i],na.rm = TRUE)), colour="midnightblue") +
      #geom_vline(aes(xintercept = quantile(data[,i],0.75,na.rm = TRUE)), colour="orangered2") + geom_vline(aes(xintercept = quantile(data[,i],0.25,na.rm = TRUE)), colour="mediumvioletred") + theme(text = element_text(size=20))
   #ggsave(filename =paste(paste("hist",i,sep=""),".png",sep=""),plot=last_plot())
  #}
  
  #Histogram for R vs F-D
  ggplot(data, aes(x=data$Leaf.Area, col = data$Class, fill=data$Class)) +geom_histogram(binwidth = FDw(data$Leaf.Area), alpha=0.5) + 
    scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000")) + 
    scale_colour_manual("fill", values=c("black","black","black","black","black"))  + theme_minimal()+ ggtitle("Histogram for the Leaf Area using Freedman-Diaconsis") + 
    xlab("Leaf Area") + labs(fill="Leaf Class") + guides(col=FALSE, fill = FALSE) +
    ggsave(filename ="histComp1.png",plot=last_plot())

  ggplot(data, aes(x=data$Leaf.Area, col = data$Class, fill=data$Class)) +geom_histogram(alpha=0.5) + 
    scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000")) + 
    scale_colour_manual("fill", values=c("black","black","black","black","black"))  + theme_minimal()+ ggtitle("Histogram for the Leaf Area using deafult R") + 
    xlab("Leaf Area") + labs(fill="Leaf Class") + guides(col=FALSE, fill = FALSE) +
    ggsave(filename ="histComp2.png",plot=last_plot())
  
  
  #method 2 uses mapply
  createHist = function(xdata,column){
    ggplot(data, aes(x=xdata, col = data$Class, fill=data$Class)) +geom_histogram(binwidth = FDw(xdata), alpha=0.5) + 
      scale_fill_manual("Histogram Legend",values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643","#000000")) +
      scale_colour_manual("fill", values=c("black","black","black","black","black"))  + theme_minimal()+ ggtitle(paste("Histogram for ",column,sep="")) +
      xlab(column) + labs(fill="Leaf Class") + guides(col=FALSE, fill = FALSE) +
      geom_vline(aes(xintercept = mean(xdata,na.rm = TRUE)), colour="darkturquoise") + geom_vline(aes(xintercept = median(xdata,na.rm = TRUE)), colour="midnightblue") +
      geom_vline(aes(xintercept = quantile(xdata,0.75,na.rm = TRUE)), colour="orangered2") + geom_vline(aes(xintercept = quantile(xdata,0.25,na.rm = TRUE)), colour="mediumvioletred") + 
      theme(text = element_text(size=20))
    ggsave(filename =paste(paste("hist",column,sep=""),".png",sep=""),plot=last_plot())
  }
  
  columns = colnames(data[,c(2:19)])
  mapply(createHist,data[columns],columns)

  #plot for the box plot of the class attribute if needed
  ggplot(data, aes(x=data$Class, col = data$Class, fill=data$Class)) +geom_histogram(stat="count",alpha=0.5) +
    scale_fill_manual(values = c("#BB2433","#FAA916","#143642","#A6A7A8","#561643")) + theme_minimal()+ ggtitle("Count of each of the classes") + 
    xlab("Leaf Class") + labs(fill="Leaf Class") + guides(col=FALSE, fill = FALSE) +  theme(text = element_text(size=20))
  ggsave(filename ="hist20.png",plot=last_plot())
  q1i(data)

}






