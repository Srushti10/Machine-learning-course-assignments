# Assignment 2- Feature Scaling

networkDataset <- read.csv(file="C:/Studymaterial/Big Data/Assignment2/FeatureSelection/feature2.csv",head=TRUE,sep=",")
#Setting the value of no of folds in cross validation
k=5
library(randomForest)

#View(networkDataset)

#Function to create stratified groups of data for cross-validation
createFolds <- function(networkDataset){
  #k=5
  classYes <- (which(networkDataset[,21] == "NoBlock"))
  classNo <- (which(networkDataset[,21] == "Wait"))
  #Diving factor for equal no of class variables
  n1 <- round(length(classYes)/k)
  n2 <- round(length(classNo)/k)
  groups <- list()
  i1 <- 0
  i2 <- 0
  #Generate k groups of data
  for (index in seq(k)) {
    #Condition to add the initial values and left out values after splitting uneven data
    if(index != k){
      group <- classYes[i1:(i1+n1)]
      group <- c(group,classNo[i2:(i2+n2)])
    } else {
      group <- classYes[i1:length(classYes)]
      group <- c(group,classNo[i2:length(classNo)])
    }
    i1 <- i1+n1
    i2 <- i2+n2
    groups[[index]] <- group
  }
  return(groups)
}

datagroups <- createFolds(networkDataset)

#Function to select the best features
featureSelection <- function(groups) {
  #Will store the final feature subset
  featureSubset <- c(ncol(networkDataset))
  #Accuracy for final feature subset
  featureSubsetAccuracy <- 0
  #Accuracy for subset with individual features
  individualFeatureAccuracy <- 0
  #Selected feature for every new subset
  selectedFeature <- 0
  featureLength <- ncol(networkDataset)
  
  #Break when there is no change in accuracy
  while (TRUE) {
    #Looping on every feature that is not in the subset
    for (feature in seq(featureLength-1)) {
      
      checkFeature <- feature
      featureSubset <- c(featureSubset,checkFeature)
      
      
      noOfFolds <- seq(length(datagroups))
      accuracy <- vector()
      #Application of cross validation to calculate accuracy
      for (index in noOfFolds) {
        test <- networkDataset[datagroups[[index]],featureSubset]
        trainIndex <- noOfFolds[noOfFolds!=index]
        
        #Generating models and predicting class value for each fold
        data1 <- networkDataset[datagroups[[trainIndex[1]]],featureSubset]
        model1 <- randomForest(Class ~ ., data = data1, importance = TRUE, ntree = 500)
        predict1 <- predict(model1, test, type = 'class')
        accuracy1 <- mean(predict1 == test$Class) * 100
        
        data2 <- networkDataset[datagroups[[trainIndex[2]]],featureSubset]
        model2 <- randomForest(Class ~ ., data = data2, importance = TRUE, ntree = 500)
        predict2 <- predict(model2, test, type = 'class')
        accuracy2 <- mean(predict2 == test$Class) * 100
        
        data3 <- networkDataset[datagroups[[trainIndex[3]]],featureSubset]
        model3 <- randomForest(Class ~ ., data = data3, importance = TRUE, ntree = 500)
        predict3 <- predict(model3, test, type = 'class')
        accuracy3 <- mean(predict3 == test$Class) * 100
        
        data4 <- networkDataset[datagroups[[trainIndex[4]]],featureSubset]
        model4 <- randomForest(Class ~ ., data = data4, importance = TRUE, ntree = 500)
        predict4 <- predict(model4, test, type = 'class')
        accuracy4 <- mean(predict4 == test$Class) * 100
        
        accuracy <- c(accuracy,mean(accuracy1,accuracy2,accuracy3,accuracy4))
      }
      
      featureSubset <- featureSubset[-length(featureSubset)]
      #Selecting the one with maximum accuracy
      if(individualFeatureAccuracy < max(accuracy)){
        individualFeatureAccuracy <- max(accuracy)
        selectedFeature <- feature
      }
      
      
    }
    
    #Final check to stop the selection or continue
    if(individualFeatureAccuracy > featureSubsetAccuracy){
      featureSubsetAccuracy <- individualFeatureAccuracy
      featureSubset <- c(featureSubset,selectedFeature)
      featureLength <- featureLength[-selectedFeature]
    } else {
      cat("Important set of feature column nos are: ",featureSubset[-1])
      break()
    }
  }
  
}

featureSelection(datagroups)



