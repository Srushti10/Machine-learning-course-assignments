# Assignment 2- Naive Bayes

balancesheet <- read.csv(file="C:/Studymaterial/Big Data/Assignment2/Naive/Naive.csv",head=TRUE,sep=",")
#Splitting training testing data into 70-30
smp_size <- floor(0.70 * nrow(balancesheet))
set.seed(123)
train_index <- sample(seq_len(nrow(balancesheet)), size = smp_size)

#Splitting data into training and testing
train <- balancesheet[train_index, ]
test <- balancesheet[-train_index, ]

#Function to calculate m-estimate probability from training data
calculateProbabilities <- function(){
  #Defining total no of Positive and Negative instances
  noOfPositives <- length(which(train$Balance_Scale2 == 'Positive'))
  noOfNegatives <- length(which(train$Balance_Scale2 == 'Negative'))
  #browser()
  probPositive <- noOfPositives/nrow(train)
  probNegative <- noOfNegatives/nrow(train)
  #List to store all probablities
  probList <- list()
  
  #Looping on every feature
  for (index in seq(ncol(train)-1)) {
    #Defining the values for m and p
    featureValues <- unique(train[,index])
    m <- length(featureValues)
    p <- 1/m
    probDataframe <- data.frame()
    data <- split(train,train[,index])
    #Checking probablities for all unique values in 1 feature
    for (i in featureValues) {
      totalValues <- nrow(data[[i]])
      lengthPos <- length(which(data[[i]]$Balance_Scale2 == 'Positive'))
      #calculation of m-estimate of probability
      conditionalProbPositive <- (lengthPos+m*p)/(totalValues+m)
      conditionalProbNegative <- 1-conditionalProbPositive
      #storing probability for positive and negative for 1 value of a feature
      probValues <- c(conditionalProbPositive,conditionalProbNegative)
      probDataframe <- rbind(probDataframe,probValues)
    }
    
    #Appending total probability of positive and negative
    colnames(probDataframe) <- c('Positive','Negative')
    #Storing probabilites of all values of a feature
    probList[[index]] <- probDataframe
    
  }
  probList[['Total']]<-c(probPositive,probNegative)
  return(probList)
}

calculatedProbabilities <- calculateProbabilities()

#Function to classify test instances
classifyTestInstance <- function(calculatedProbabilities){
  classLabel <- vector()
  
  #Looping over every test instance
  for (index in seq(nrow(test))) {
    totalPos <- 1
    totalNeg <- 1
    #Looping over all the feature values in one test instance
    for (j in seq(length(test[index,])-1)) {
      value <- test[index,j]
      #Finding the product of positives and negetives probability
      probabilites <- calculatedProbabilities[[j]]
      probPositive <- probabilites[value,'Positive']/calculatedProbabilities[['Total']][1]
      probNegative <- probabilites[value,'Negative']/calculatedProbabilities[['Total']][2]
      totalPos <- totalPos*probPositive
      totalNeg <- totalNeg*probNegative
    }
    #Check to classify on the calculated product of probabilities
    if(totalPos > totalNeg) {
      classLabel <- c(classLabel,'Positive')
    } else {
      classLabel <- c(classLabel,'Negative')
    }
  }
  return(classLabel)
}

classificationResult <- classifyTestInstance(calculatedProbabilities)

#Function to calculate accuracy,sensitivity and specicifity
calculateAccurary <- function(classificationResult) {
  
  actualClass <- test[,'Balance_Scale2']
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  FF <- 0
  TT <- 0
  #Looping over every test instance
  for (index in seq(nrow(test))) {
    
    #calculating the no of true positives
    if(classificationResult[index] == actualClass[index] && classificationResult[index] == 'Positive'){
      TP <- TP+1
      TT <- TT+1
    }
    #calculating the no of true negatives
    if(classificationResult[index] == actualClass[index] && classificationResult[index] == 'Negative'){
      TN <- TN+1
      FF <- FF+1
    }
    #calculating the no of false positives
    if(classificationResult[index] == 'Positive' && actualClass[index] == 'Negative'){
      FP <- FP+1
      FF <- FF+1
    }
    #calculating the no of false negatives
    if(classificationResult[index] == 'Negative' && actualClass[index] == 'Positive'){
      FN <- FN+1
      TT <- TT+1
    }
  }
  #Applying the formula for calculation
  accuracy <- (TP+TN)/TT+FF
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  cat("Accuracy is: ",round(accuracy),"%")
  cat("\nSensitivity is: ",sensitivity)
  cat("\nSpecificity is: ",specificity)
}

calculateAccurary(classificationResult)

#Writing the classified test data in csv file
finalOutputData <- data.frame(test,classificationResult)
write.table(finalOutputData,"Prediction.csv",append = FALSE,sep=",",col.names = TRUE,row.names = FALSE)