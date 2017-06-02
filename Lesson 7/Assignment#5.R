rm(list=ls())
# Clear Console:
cat("\014")
# Set repeatable random seed.
set.seed(123)
library(e1071)
#Load the dataset.
read.vehicles = function(file = 'veh-prime.arff.arff'){
  ## Read the arff file
  library(foreign)
  vehicles <- read.arff(file)
  vehicles[complete.cases(vehicles), ]
}

# Partition the data into test and training data sets.
PartitionExact = function(dataSet, fractionOfTest = 0.3)
{
  #  browser()
  random <-runif(nrow(dataSet))
  quant <- quantile(random,fractionOfTest)
  testFlag <- random <= quant
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
}

#Calculate Specificity (TPR)
Specificity <- function(Table.X)
{
  Specificity.X <- (Table.X[2,2])/(Table.X[1,2]+Table.X[2,2])
  return(Specificity.X)
}
#calculate FPR from Confusion MAtrix
FPR <- function(Table.X)
{
  FPR.X <- (Table.X[1,2])/(Table.X[1,2]+Table.X[2,2])
  return(FPR.X)
}
Accuracy <- function(Table.X)
{
  Accuracy.X <- (Table.X[1,1]+Table.X[2,2])/(Table.X[1,1]+Table.X[1,2]+Table.X[2,1]+Table.X[2,2])
  return(Accuracy.X)
}

# Load and cleanse the csv file.
vehicles = read.vehicles()
#Check the structure of the dataset.
str(vehicles)

#check the data set header
head(vehicles)
tail(vehicles)
#Convert CLASS to numeric
library(dplyr)
vehiclessvm <- vehicles

#Convert to factor with numeric values for SVM
vehiclessvm %>% mutate_if(is.factor, as.character) -> vehiclessvm
vehiclessvm$CLASS[vehiclessvm$CLASS == "car"] <- 1
vehiclessvm$CLASS[vehiclessvm$CLASS == "noncar"] <- -1
vehiclessvm %>% mutate_if(is.character, as.factor) -> vehiclessvm
str(vehiclessvm)
#Split dataset into test and training data sets.
vehiclesDatasetsvm = PartitionExact(vehiclessvm)
Testvehiclessvm <- vehiclesDatasetsvm$testingData
Trainvehiclessvm <-vehiclesDatasetsvm$trainingData
nrow(Testvehiclessvm)
nrow(Trainvehiclessvm)

modelsvm <- svm(CLASS ~ . , Trainvehiclessvm)

print(modelsvm)
summary(modelsvm)
predictedsvmCLASS <- predict(modelsvm, Testvehiclessvm)
#Check Accuracy
tablesvm <- table(predictedsvmCLASS, Testvehiclessvm$CLASS)
#Accuracy
Accuracysvm <- Accuracy(tablesvm)
Accuracysvm
# Calculate TPR of SVM
TPRsvm <- Specificity(tablesvm)
TPRsvm
# Calculate FPR of SVM
FPRsvm <- FPR(tablesvm)
FPRsvm

#Tune SVM model
OptModelsvm=tune(svm, CLASS ~ ., data=Trainvehiclessvm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

## Select the best model

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict CLASS using best model
PredBstCLASSsvm=predict(BstModel,Testvehiclessvm)
#Check Accuracy
tablebstsvm <- table(PredBstCLASSsvm, Testvehiclessvm$CLASS)
#Accuracy
Accuracybstsvm <- Accuracy(tablebstsvm)
Accuracybstsvm
# Calculate TPR of SVM
TPRbstsvm <- Specificity(tablebstsvm)
TPRbstsvm
# Calculate FPR of SVM
FPRbstsvm <- FPR(tablebstsvm)
FPRbstsvm

# Try more parameters.

#Tune SVM model further
TunedModelsvmmore=tune(svm, CLASS ~ ., data=Trainvehiclessvm,ranges=list(elsilon=seq(0,.2,0.1), cost=1:100))

#Print optimum value of parameters
print(TunedModelsvmmore)

#Plot the perfrormance of SVM Regression model
plot(TunedModelsvmmore)

## Select the best model

#Find out the best model
BstModelmore=TunedModelsvmmore$best.model

#Predict CLASS using best model
PredBstCLASSsvmmore=predict(BstModelmore,Testvehiclessvm)
#Check Accuracy
tablebstsvmmore <- table(PredBstCLASSsvmmore, Testvehiclessvm$CLASS)
#Accuracy
Accuracybstsvmmore <- Accuracy(tablebstsvmmore)
Accuracybstsvmmore
# Calculate TPR of SVM
TPRbstsvmmore <- Specificity(tablebstsvmmore)
TPRbstsvmmore
# Calculate FPR of SVM
FPRbstsvmmore <- FPR(tablebstsvmmore)
FPRbstsvmmore
