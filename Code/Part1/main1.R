#Name: Owais Tariq
#Student ID: 10039105
#libraries
library ("ggplot2")
library ("dplyr")

#set cwd if opened from root or after part 2
setwd("../Part1")
#dataset
data = read.csv(file="cw_dataset.csv", header=TRUE, sep=",")
#data = read.csv(file=file.choose(), header=TRUE, sep=",")

#question 1i, summary of the data 
source("./1i.R")
q1i(data)

#question 1ii greating histograms for the data
#will save histograms to cwd (please check folder)
#warnings are regarding na values being removed from the data when creating histograms
source("./1ii.R")
q1ii(data)

#question 2, for all 3 parts
#errors regarding missing values when plotting graphs
source("./2.R")
q2(data)

#general conclusions, new information used
source("./3.R")
q3(data)

#question 4 all plots and information
source("./4.R")
q4(data)
#running again to collect the datasets, also creats csv so they can be used in part 2
q4Res = q4(data)

#question 5 attribute transformaion
#datasets are saved to csv to be used later, do not need to be loaded for this section, graphs saved to cwd
#check cwd to see all files created using this method
#source("./5.R")
source("./5-3.R")
#q5(data0replaced,dataMeanReplaced,dataMedianReplaced)
q5(q4Res[[1]],q4Res[[4]],q4Res[[7]])

#question 6 instance selection
##attribute and instance selection
source("./6i.R")
q6i(data)
##correlation selection, data set wriiten to csv
##only a subest of correlations used, first table shows all correlation
source("./6ii.R")
q6ii(data)
##dataset for PCA, data set wriiten to csv
##loading the mean replaced standardised, data set created ealier 
dataMeanRepalcedStandardised =  read.csv(file="./mean replacement STANDARDISATION.csv", header=TRUE, sep=",")
source("./6iii.R")
q6iii(dataMeanRepalcedStandardised)

#end of section 1

#CODE USED IN SECTION 3, NO CODE MARKS RELATED TO THIS SECTION
#CODE TO CREATE TEST AND TRAIN SAMPLE FOR OVERFITTING COMPARISION in seciton 3
#creating a stratisfied sample for section 3
set.seed(1) #seed set to get the same sample every time
dataToSample = as.data.frame(q4Res[[4]])
ssTrainData = dataToSample %>%
  group_by(Class) %>%
  sample_frac(0.8) %>%
  ungroup
ssTestData = dataToSample[!dataToSample$Sample_ID %in% ssTrainData$Sample_ID,]
write.csv(ssTrainData, file="ssTrainData.csv",row.names=FALSE)
write.csv(ssTestData, file="ssTestData.csv",row.names=FALSE)

#OTHER GRAPHS FOR PART 3
results1 = read.csv(file="opt1.csv", header=TRUE, sep=",")
dataR11= results1[1:19,]
dataR12= results1[-c(1:19),]
results2 = read.csv(file="opt2.csv", header=TRUE, sep=",")
dataR21= results2[1:19,]
dataR22= results2[20:38,]

ggplot(dataR11, aes(x=Number.of.folds)) +  geom_line(aes(y = Train.Accuracy, col="Training")) +  geom_line(aes(y = Test.Accuracy, col="Test"))  + scale_color_manual(name="Accuracy", values=c(Training="red",Test="blue")) + ggtitle("J48 Accuracy with at least 1 instance per node while varying number of folds") + ylab("Accuracy") + xlab("Number of folds")
    theme(text = element_text(size=20))

ggplot(dataR12, aes(x=Number.of.folds)) +  geom_line(aes(y = Train.Accuracy, col="Training")) +  geom_line(aes(y = Test.Accuracy, col="Test"))  + scale_color_manual(name="Accuracy", values=c(Training="red",Test="blue")) + ggtitle("J48 Accuracy with at least 2 items per node while varying number of folds") + ylab("Accuracy") + xlab("Number of folds")
  theme(text = element_text(size=20))

  
ggplot(dataR21, aes(x=Confidence.Factor)) +  geom_line(aes(y = Train.Accuracy, col="Training")) +  geom_line(aes(y = Test.Accuracy, col="Test"))  + scale_color_manual(name="Accuracy", values=c(Training="red",Test="blue")) + ggtitle("J48 Accuracy with at least 1 item per node while varying Confidence Factor") + ylab("Accuracy") + xlab("Confidence Factor")
  theme(text = element_text(size=20))
  
ggplot(dataR22, aes(x=Confidence.Factor)) +  geom_line(aes(y = Train.Accuracy, col="Training")) +  geom_line(aes(y = Test.Accuracy, col="Test"))  + scale_color_manual(name="Accuracy", values=c(Training="red",Test="blue")) + ggtitle("J48 Accuracy with at least 2 items per node while varying Confidence Factor") + ylab("Accuracy") + xlab("Confidence Factor")
  theme(text = element_text(size=20))
  

