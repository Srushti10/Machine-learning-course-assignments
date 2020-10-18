# Assignment 1

airbnbdata <- read.csv(file="C:/Studymaterial/R/AirbnbNYC.csv",head=TRUE,sep=",")
airbnbdata[,1] <- as.numeric(airbnbdata[,1])
globalErrorMeasure <- data.frame()

#Function to get data from csv and scale it based on scaleFlag
getSampleDataForComputation <- function(scaleFlag) {
  # feature1 is categorical, feature2 and feature3 are numeric 
  airbnbdata <- read.csv(file="C:/Studymaterial/R/AirbnbNYC.csv",head=TRUE,sep=",")
  sampleDataset <- airbnbdata
  # converting the caterogical variable into interger value for distance calculation #
  sampleDataset[,1] <- as.numeric(sampleDataset[,1])
  
  if(scaleFlag) {
    #scaling first numeric feature using min-max normalization
    sampleDataset[ , 2] <- (sampleDataset[ , 2] - min(sampleDataset[ , 2])) / (max(sampleDataset[ , 2]) - min(sampleDataset[ , 2]))
    
    #scaling second numeric feature using Mean normalization
    sampleDataset[ , 3] <- (sampleDataset[ , 3] - sum(sampleDataset[ , 3])/length(sampleDataset[ , 3])) / (max(sampleDataset[ , 3]) - min(sampleDataset[ , 3]))
    
  }
  return(sampleDataset)
}

#For initial missing data computation
initialise <- function(percent,featureno,sampleDataset) {
  generateMissingDataset <- function(percent , featureno) {
    sample1ength = percent*nrow(sampleDataset)
    missingValuesDataset <- generateMissingValues(featureno,sample1ength)
    return(missingValuesDataset)
  }
  # generation of missing values in sample data
  generateMissingValues <- function(featureNo , sample1ength) {
    set.seed(100)
    missingIndices <- round(runif(sample1ength, 0, nrow(sampleDataset)))
    for(i in missingIndices) {
      sampleDataset[i,featureNo] <- NA
    }
    return(list(missingIndices = missingIndices , missingDataset = sampleDataset))
  }
   
  missingValuesDataset <- generateMissingDataset(percent,featureno)
  return(missingValuesDataset)
  
}

#f3 is feature with missing value, f1,f2 are used to compute distances
computation <- function(missingValuesDataset,f1,f2,f3) {
  #Distance computation using three distance measures
  calculateDistances <-  function(missingValuesDataset , f1, f2, f3) {
    missingIndices <- missingValuesDataset$missingIndices
    missingDataset <- missingValuesDataset$missingDataset
    listIndex <- 1
    computedDistance <- list()
    for(i in missingIndices) {
      x <- missingDataset[i,f1]
      y <- missingDataset[i,f2]
      #setting value of p for Minkowski distance
      p <- 1.5
      distance1 <- c()
      indexDist1 <- c()
      distance2 <- c()
      indexDist2 <- c()
      distance3 <- c()
      indexDist3 <- c()
      for(j in seq(nrow(missingDataset))) {
        #skipping the missing values
        if(is.na(missingDataset[j,f3])) {
          next
        } else {
          a <- missingDataset[j,f1]
          b <- missingDataset[j,f2]
          #calculting euclidean distance
          distance1 <- c(distance1 , sqrt((x-a)^2 + (y-b)^2))
          indexDist1 <- c(indexDist1 , j)
          # calculating Manhattan distance
          distance2 <- c(distance2, abs((x-a) + (y-b)))
          indexDist2 <- c(indexDist2 , j)
          #calculating Minkowski distance
          distance3 <- c(distance3, (abs(x-a)^p + abs(y-b)^p)^(1/p))
          indexDist3 <- c(indexDist3 , j)
          
        }
      }
      distances <- data.frame(distance1,indexDist1,distance2,indexDist2,distance3,indexDist3)
      computedDistance[[listIndex]] <- distances
      listIndex <- listIndex+1
    }
    return(computedDistance)
  }
  computedDistance <- calculateDistances(missingValuesDataset,f1,f2,f3)
  
  # Missing value computation using weighted KNN
  onennmethod <- function(computedDistance , missingValuesDataset , f3) {
    listIndex <- 1
    computedValuesforDistances <- data.frame()
    if(f3 == 1) {
      #condition for catergorical feature
      originalData <- missingValuesDataset$missingDataset
    } else {
      originalData <- airbnbdata
    }
    for(i in missingValuesDataset$missingIndices) {
      #temp sorted values based on 3 distances
      tempdf1 <- computedDistance[[listIndex]][(1:2)]
      tempdf1 <- tempdf1[order(tempdf1$distance1),]
      tempdf2 <- computedDistance[[listIndex]][(3:4)]
      tempdf2 <- tempdf2[order(tempdf2$distance2),]
      tempdf3 <- computedDistance[[listIndex]][(5:6)]
      tempdf3 <- tempdf3[order(tempdf3$distance3),]
      computedValue <- c(i,originalData[tempdf1[1,2],f3],originalData[tempdf2[1,2],f3], originalData[tempdf3[1,2],f3])
      computedValuesforDistances <- rbind(computedValuesforDistances ,computedValue)
      listIndex <- listIndex+1
      
    }
    return(list(feature = f3, computedValues = computedValuesforDistances))
  }
  computedValuesOneNN <- onennmethod(computedDistance , missingValuesDataset, f3)
  
  # Missing value computation using weighted KNN
  knnmethod <- function(computedDistance , missingValuesDataset , f3) {
    listIndex <- 1
    computedValuesforDistances <- data.frame()
    if(f3 == 1) {
      originalData <- missingValuesDataset$missingDataset
    } else {
      originalData <- airbnbdata
    }
    for(i in missingValuesDataset$missingIndices) {
      k <- 5
      # total distances computed by three distance computation measures
      totalDistance1 <- 0
      totalDistance2 <- 0
      totalDistance3 <- 0
      tempdf1 <- computedDistance[[listIndex]][(1:2)]
      tempdf1 <- tempdf1[order(tempdf1$distance1),]
      tempdf2 <- computedDistance[[listIndex]][(3:4)]
      tempdf2 <- tempdf2[order(tempdf2$distance2),]
      tempdf3 <- computedDistance[[listIndex]][(5:6)]
      tempdf3 <- tempdf3[order(tempdf3$distance3),]
      for(index in seq(k)) {
        totalDistance1 <- totalDistance1 + originalData[tempdf1$indexDist1[index] , f3]
        totalDistance2 <- totalDistance2 + originalData[tempdf2$indexDist2[index] , f3]
        totalDistance3 <- totalDistance3 + originalData[tempdf3$indexDist3[index] , f3]
      }
      #computation of mean value
      avgDistance1 <- totalDistance1 / k
      avgDistance2 <- totalDistance2 / k
      avgDistance3 <- totalDistance3 / k
      computedValue <- c(i, avgDistance1,avgDistance2,avgDistance3)
      computedValuesforDistances <- rbind(computedValuesforDistances ,computedValue)
      listIndex <- listIndex+1
    }
    
    if(f3 == 1) {
      #handling for categorical feature
      computedValuesforDistances <- round(computedValuesforDistances)
    }
    return(list(feature = f3, computedValues = computedValuesforDistances))
  }
  computedValuesKNN <- knnmethod(computedDistance,missingValuesDataset,f3)
  
  # Missing value computation using weighted KNN 
  weightedknn <- function(computedDistance , missingValuesDataset , f3) {
    listIndex <- 1
    computedValuesforWeights <- data.frame()
    if(f3 == 1) {
      originalData <- missingValuesDataset$missingDataset
    } else {
      originalData <- airbnbdata
    }
    for(i in missingValuesDataset$missingIndices) {
      k <- 5
      # total weighted values computed by three distance computation measures
      weightedValues1 = 0
      weightedValues2 = 0
      weightedValues3 = 0
      tempdf1 <- computedDistance[[listIndex]][(1:2)]
      tempdf1 <- tempdf1[order(tempdf1$distance1),]
      tempdf2 <- computedDistance[[listIndex]][(3:4)]
      tempdf2 <- tempdf2[order(tempdf2$distance2),]
      tempdf3 <- computedDistance[[listIndex]][(5:6)]
      tempdf3 <- tempdf3[order(tempdf3$distance3),]
      for(index in seq(k)) {
        weight1 <- 1/round(tempdf1$distance1[index])
        weight2 <- 1/round(tempdf2$distance2[index])
        weight3 <- 1/round(tempdf3$distance3[index])
        if(is.infinite(weight1)) {
          weight1 = 1
        }
        if(is.infinite(weight2)){
          weight2 = 1
        } 
        if (is.infinite(weight3)) {
          weight3 = 1
        }
        #computation of weighted values
        weightedValues1 <- weightedValues1 + (weight1 * originalData[tempdf1$indexDist1[index] , f3])
        weightedValues2 <- weightedValues2 + (weight2 * originalData[tempdf2$indexDist2[index] , f3])
        weightedValues3 <- weightedValues3 + (weight3 * originalData[tempdf3$indexDist3[index] , f3])
        
      }
      computedValue <- c(i, weightedValues1/k,weightedValues2/k,weightedValues3/k)
      computedValuesforWeights <- rbind(computedValuesforWeights ,computedValue)
      listIndex <- listIndex+1
    }
    if(f3 == 1) {
      #handling for categorical value
      computedValuesforWeights <- round(computedValuesforWeights)
    }
    return(list(feature = f3, computedValues = computedValuesforWeights))
  }
  computedValuesWeightedKNN <- weightedknn(computedDistance,missingValuesDataset,f3)
  
  accuracyData <- determineAccuracy(computedValuesOneNN ,computedValuesKNN, computedValuesWeightedKNN,missingValuesDataset)
  return(accuracyData)
  
}

#To determing relative error for all three algorithms
determineAccuracy <- function(computedValuesOneNN ,computedValuesKNN, computedValuesWeightedKNN,missingValuesDataset) {
  names(computedValuesOneNN$computedValues) <- c("index","distance1","distance2","distance3")
  names(computedValuesKNN$computedValues) <- c("index","distance1","distance2","distance3")
  names(computedValuesWeightedKNN$computedValues) <- c("index","distance1","distance2","distance3")
  
  errorDist1 <- c()
  errorDist2 <- c()
  errorDist3 <- c()
  #for 1NN
  for(index in seq(nrow(computedValuesOneNN$computedValues))) {
    originalValue <- airbnbdata[computedValuesOneNN$computedValues[index,1] , computedValuesOneNN$feature]
    errorDist1 <- c(errorDist1 ,(abs(originalValue - computedValuesOneNN$computedValues[index,2])/originalValue))
    errorDist2 <- c(errorDist2 ,(abs(originalValue - computedValuesOneNN$computedValues[index,2])/originalValue))
    errorDist3 <- c(errorDist3 ,(abs(originalValue - computedValuesOneNN$computedValues[index,2])/originalValue))
  }
  if(computedValuesOneNN$feature == 1) {
    lengthValues <- nrow(computedValuesOneNN$computedValues)
    oneNNerror <- c(sum(errorDist1)/lengthValues,sum(errorDist2)/lengthValues,sum(errorDist3)/lengthValues)
  } else {
    oneNNerror <- c(sum(errorDist1),sum(errorDist2),sum(errorDist3))
  }

  errorDist1 <- c()
  errorDist2 <- c()
  errorDist3 <- c()
  #for kNN
  for(index in seq(nrow(computedValuesKNN$computedValues))) {
    originalValue <- airbnbdata[computedValuesKNN$computedValues[index,1] , computedValuesKNN$feature]
    errorDist1 <- c(errorDist1 ,(abs(originalValue - computedValuesKNN$computedValues[index,2])/originalValue))
    errorDist2 <- c(errorDist2 ,(abs(originalValue - computedValuesKNN$computedValues[index,2])/originalValue))
    errorDist3 <- c(errorDist3 ,(abs(originalValue - computedValuesKNN$computedValues[index,2])/originalValue))
  }
  if(computedValuesKNN$feature == 1) {
    lengthValues <- nrow(computedValuesKNN$computedValues)
    kNNerror <- c(sum(errorDist1)/lengthValues,sum(errorDist2)/lengthValues,sum(errorDist3)/lengthValues)
  } else {
    kNNerror <- c(sum(errorDist1),sum(errorDist2),sum(errorDist3))
  }
  
  errorDist1 <- c()
  errorDist2 <- c()
  errorDist3 <- c()
  #for weighted kNN
  for(index in seq(nrow(computedValuesWeightedKNN$computedValues))) {
    originalValue <- airbnbdata[computedValuesWeightedKNN$computedValues[index,1] , computedValuesWeightedKNN$feature]
    errorDist1 <- c(errorDist1 ,(abs(originalValue - computedValuesWeightedKNN$computedValues[index,2])/originalValue))
    errorDist2 <- c(errorDist2 ,(abs(originalValue - computedValuesWeightedKNN$computedValues[index,2])/originalValue))
    errorDist3 <- c(errorDist3 ,(abs(originalValue - computedValuesWeightedKNN$computedValues[index,2])/originalValue))
  }
  if(computedValuesWeightedKNN$feature == 1) {
    lengthValues <- nrow(computedValuesWeightedKNN$computedValues)
    weightedKNNerror <- c(sum(errorDist1)/lengthValues,sum(errorDist2)/lengthValues,sum(errorDist3)/lengthValues)
  } else {
    weightedKNNerror <- c(sum(errorDist1),sum(errorDist2),sum(errorDist3))
  }
  
  allErrors <- data.frame(oneNNerror,kNNerror,weightedKNNerror)
  row.names(allErrors) <- c("Euclidean","Manhattan","Minkowski")
  return(allErrors)
}

determineAccuracyMeasure <- function(featureno, percent, scaleFlag) {
  #pass true for scaled values and false for non-scaled values
  sampleDataset <- getSampleDataForComputation(scaleFlag)
  
  #first parameter determines the percentage of values
  #second parameter is the feature for which missing values and computation will be done
  #third parameter is the sample dataset with missing values
  missingValuesDataset <- initialise(percent,featureno,sampleDataset)
  
  #2nd and 3rd parameter will consist of the feature no to be considered for calculating distances
  #4th parameter will consist of the feature no for which value needs to be computated
  if(featureno == 1) {
    accuracyData <- computation(missingValuesDataset,2,3,1)
  } else if(featureno == 2) {
    accuracyData <- computation(missingValuesDataset,1,3,2)
  } else if(featureno == 3) {
    accuracyData <- computation(missingValuesDataset,1,2,3)
  }
  
  View(accuracyData)
  globalErrorMeasure <- rbind(globalErrorMeasure,accuracyData)
  return(globalErrorMeasure)
}

globalErrorMeasure <- determineAccuracyMeasure(3,0.05,FALSE)

#uncomment the below line to write the accuracy measure in another csvfile
#write.table(globalErrorMeasure,"Output.csv",append = FALSE)
