#Name: Owais Tariq
#Student ID: 10039105

#libraries
library ("ggplot2")
library ("dplyr")
library("lpSolveAPI")
library("fpc") 
library("cluster")


#Function the allign the diagonal of a matrix and get the relavent metrics
#make sure cwd is set to current folder location, if opened from part1, needs to be changed to load files
setwd("../Part2")
source("./ald.R")


source("./metrics.R")
source("./kmeansRs.R")
#uses the mean replaced normalised data
dataMeanRepN = read.csv(file="mean replacement Nomralisation.csv", header=TRUE, sep=",")

#Hierarchical clustering and its optimisation
source("./hclustering.R")
hclustering(dataMeanRepN)

#kmeans clustering and its optimsation
#some optimsation results written to csv, results can be found in cwd as a csv
source("./kmeanclustering.R")
kmeanclustering(dataMeanRepN)
#wraning due to k-means not converging for the lower number of iterations


#pam optimsation results written to csv, results can be found in cwd as a csv
source("./pamclustering.R")
pamclustering(dataMeanRepN)

#running k means over a number of datasets for section 2 part 3
source("./kmeanDS.R")
#loading the datasets using the files that were created in the main1 code
#if using mak or linux need to repalce '\\' with '/' due to windows defining relative file paths differently 
PCA10 = read.csv(file="PCA10.csv", header=TRUE, sep=",")
dataAIs = read.csv(file="data attribute and instance selection.csv", header=TRUE, sep=",")
data0rep = read.csv(file="0 replacement.csv", header=TRUE, sep=",")
dataMeanRep = read.csv(file="mean replacement.csv", header=TRUE, sep=",")  
dataMedianRep = read.csv(file="median replacement.csv", header=TRUE, sep=",")
#slighly different results each time due to the randomness of not choosing fixed centroids, but average over 100 iterations is taken
kmeanDS(PCA10,dataAIs,data0rep,dataMeanRep,dataMedianRep)
#results are in csv file

