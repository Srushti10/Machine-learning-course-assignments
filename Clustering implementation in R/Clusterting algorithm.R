# Assignment 2- Clustering

airbnbdata <- read.csv(file="C:/Studymaterial/Big Data/Assignment2/Clustering/listings1.csv",head=TRUE,sep=",")


#Calculation of dissimilarity matrix
library(cluster)
dissimilarity <- daisy(airbnbdata, metric = c("gower"), stand = TRUE, type = list())
dissimilarityMatrix <- as.matrix(dissimilarity)

#List to maintain all cluster points
cluster <- list()
#Assigning the no of clusters
k<-5

#Randomly choose k-medoids
set.seed(100)
randomMedoidsRowIndex <- round(runif(k, 0, nrow(airbnbdata)))
for (i in seq(nrow(airbnbdata))) {
  cluster[[i]] <- vector()
}

randomMedoidsRowIndex <- assignRandomMedoids()

#function to check the distance with medoids and assign points to clusters
assignPointsToClusters <- function(medoidValues) {
  clusterColumn <-  vector()
  #Looping over every instance
  for (irow in seq(nrow(airbnbdata))) {
    minIndex <- medoidValues[1]
    #Finding the minimum distance wrt every medoid
    for (jcol in medoidValues) {
      if(dissimilarityMatrix[irow,jcol] < dissimilarityMatrix[irow,minIndex]) {
        minIndex <- jcol
      }
    }
    #Storing the point for medoid with min distance
    cluster[[minIndex]] <- c(cluster[[minIndex]],irow)
    clusterColumn[irow] <- minIndex
  }
  return(list(cluster=cluster,column=clusterColumn))
}

#Function to find new medoids for existing clusters if any
generateNewMedoids <- function(cluster,oldMedoids) {
  medoidRowIndex <- 1
  
  for(i in oldMedoids) {
    total <- vector()
    index <- vector()
    #Checking every point of old cluster to find new medoid
    for(j in seq(length(cluster[[i]]))) {
      tk <- 0
      dataPoint <- cluster[[i]][j]
      for(k in cluster[[i]]) {
        tk <- tk + dissimilarityMatrix[dataPoint,k]
      }
      #Storing total distance of every point in that cluster
      total <- c(total,tk)
      index <- c(index,dataPoint)
      
    }
    #Finding the point with minimum distance and replacing the old medoid
    dfCluster <- data.frame(total,index)
    minIndex <- which(dfCluster == min(dfCluster$total))
    newMedoid <- dfCluster$index[minIndex]
    oldMedoids[medoidRowIndex] <- newMedoid
    medoidRowIndex <- medoidRowIndex+1
  }
  return(oldMedoids)
}

#function to check if change in cluster points is required
checkClusterChange <- function(cluster,randomMedoidsRowIndex,newMedoids){
  
  #Flag to check cluster change
  flagChange <- FALSE
  
  #Loop to check difference between old and new cluster points
  for(index in seq(length(newMedoids))){
    #Condition to see if old and new clusters are different
    if(length(setdiff(cluster[[randomMedoidsRowIndex[index]]], cluster[[newMedoids[index]]]))){
      randomMedoidsRowIndex[index] <- newMedoids[index]
      flagChange <- TRUE
    }
  }
  return(list(flag=flagChange,newMedoids=randomMedoidsRowIndex))
}


#Function to calculate silhoutte width
calculateSilhoutteWidth <- function(randomMedoidsRowIndex,cluster) {
  
  silhouetteWidth <- vector()
  
  #Looping over every final cluster
  for (i in randomMedoidsRowIndex) {
    #Looping over individual cluster points
    for(j in seq(length(cluster[[i]]))){
      dataPoint <- cluster[[i]][j]
      total1 <- 0
      #Total1 is pair-wise distance with the cluster
      for(k in cluster[[i]]) {
        total1 <- total1 + dissimilarityMatrix[dataPoint,k]
      }
      a <- total1/length(cluster[[i]])
      total2 <- 0
      averageDistancePairWiseCluster <- vector()
      #Total2 is pair-wise distance with other cluster
      for(index in seq(length(randomMedoidsRowIndex))){
        clustPoint <- randomMedoidsRowIndex[index]
        if(clustPoint != i) {
          for(index in seq(length(cluster[[clustPoint]]))){
            total2<-total2+dissimilarityMatrix[dataPoint,cluster[[clustPoint]][index]]
          }
        }
        averagePairWiseDist<-total2/length(cluster[[clustPoint]])
        averageDistancePairWiseCluster<-c(averageDistancePairWiseCluster,averagePairWiseDist)
      }
      #Finding the minimum average within the other clusters
      b <- min(averageDistancePairWiseCluster)
      #Calculating the width
      width <- (b-a)/max(a,b)
      silhouetteWidth <- c(silhouetteWidth,width)
    }
  }
  cat("Average Silhoutte Width of dataset is: ",sum(silhouetteWidth)/nrow(airbnbdata))
}

#setting initial cluster change flag to 0
clusterCheckCount <- 0
main <- function(randomMedoidsRowIndex) {
  #Finding the assigned points to medoids
  assignedPoints <- assignPointsToClusters(randomMedoidsRowIndex)
  cluster <- assignedPoints$cluster
  #Generating new medoids
  newMedoids <- generateNewMedoids(cluster,randomMedoidsRowIndex)
  clusterChangeResult <- checkClusterChange(cluster,randomMedoidsRowIndex,newMedoids)
  randomMedoidsRowIndex <- clusterChangeResult$newMedoids
  #Condition to do cluster change or stop clustering
  if(clusterChangeResult$flag == TRUE && clusterCheckCount<100){
    main(randomMedoidsRowIndex)
    clusterCheckCount <- clusterCheckCount+1
  } else {
    #Execute with cluseting is done
    calculateSilhoutteWidth(randomMedoidsRowIndex,cluster)
    clusterPoint <- assignedPoints$column
    finalOutputData <- data.frame(airbnbdata,clusterPoint)
    #browser()
    write.table(finalOutputData,"Cluster_5.csv",append = FALSE,sep=",",col.names = TRUE,row.names = FALSE)
    
  }
}

finalResult <- main(randomMedoidsRowIndex)


