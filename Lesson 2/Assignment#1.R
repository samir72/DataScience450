rm(list=ls())
# Clear Console:
cat("\014")
# Set repeatable random seed.
set.seed(123)
#Load the dataset.
read.wine = function(file = 'RedWhiteWine.arff'){
  ## Read the arff file
  library(foreign)
  wine <- read.arff(file)
  wine[complete.cases(wine), ]
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
# Load and cleanse the csv file.
wine = read.wine()
colnames(wine)[1] <- "fixed.acidity"
colnames(wine)[2] <- "volatile.acidity"
colnames(wine)[3] <- "citric.acid"
colnames(wine)[4] <- "residual.sugar"
colnames(wine)[6] <- "free.sulfur.dioxide"
colnames(wine)[7] <- "total.sulfur.dioxide"

#Check the structure of the dataset.
str(wine)

#Adding a new numerica attribute based on the R/W column.
wine$kind <- as.numeric(wine$`R/W` == "R")
str(wine)

#check the data set header
head(wine)
tail(wine)

# Remove R/W column
wine$`R/W` <- NULL
# Remove Quality column
wine$quality <- NULL

lapply(wine, summary)
#Data Exploration
library(reshape2)
library(ggplot2)

ggplot(data = melt(wine), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')



#Split dataset into test and training data sets.
WineDataset = PartitionExact(wine)
TestWine <- WineDataset$testingData
TrainWine <-WineDataset$trainingData

nrow(TestWine)
head(TestWine)
nrow(TrainWine)
head(TrainWine)


formula <- kind ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + density
formulaOne <- kind ~ chlorides
formulaAll <- kind ~ .


# Classification Tree with rpart
library(rpart)
# grow tree 
winetree <- rpart(formula = formula, data = TrainWine,method="class")
winetreeOne <- rpart(formula = formulaOne, data = TrainWine,method="class")
winetreeAll <- rpart(formula = formulaAll, data = TrainWine,method="class")

#Plot tree using rpart.plot
library("rpart.plot")
rpart.plot(winetree)
rpart.plot(winetreeOne)
rpart.plot(winetreeAll)

#Use the model to make the predictions.
kindPrediction <- as.numeric(predict(winetree, newdata = TestWine, type="class"))
kindPredictionOne <- as.numeric(predict(winetreeOne, newdata = TestWine, type="class"))
kindPredictionAll <- as.numeric(predict(winetreeAll, newdata = TestWine, type="class"))
#Calculate Confusion Matrix with six attributes.
crosstab <- table(kindPrediction, TestWine$kind)
crosstab

#Calculate Confusion Matrix for model with one attribute
crosstabOne <- table(kindPredictionOne, TestWine$kind)
crosstabOne

#Calculate Confusion Matrix for all atributes.
crosstabAll <- table(kindPredictionAll, TestWine$kind)
crosstabAll

#Calculate Specificity (TPR)
Specificity <- function(Table.X)
{
  Specificity.X <- (Table.X[2,2])/(Table.X[1,2]+Table.X[2,2])
  return(Specificity.X)
}
# Calculate TPR for all attributes
TPRAll <- Specificity(crosstabAll)
TPRAll
# Calculate TPR for selected attributes
TPR <- Specificity(crosstab)
TPR
# Calculate TPR for one attributes
TPROne <- Specificity(crosstabOne)
TPROne
#calculate FPR from Confusion MAtrix
FPR <- function(Table.X)
{
  FPR.X <- (Table.X[1,2])/(Table.X[1,2]+Table.X[2,2])
  return(FPR.X)
}
# Calculate FPR for all attributes
FPRAll <- FPR(crosstabAll)
FPRAll
# Calculate FPR for six attributes
FPRSix <- FPR(crosstab)
FPRSix
# Calculate FPR for one attributes
FPROne <- FPR(crosstabOne)
FPROne

#Calculate AUC
library(pROC)
roc_obj <- roc(TestWine$kind, kindPrediction)
auc(roc_obj)

roc_obj_All <- roc(TestWine$kind, kindPredictionAll)
auc(roc_obj_All)

roc_obj_One <- roc(TestWine$kind, kindPredictionOne)
auc(roc_obj_One)

FPRCheck <- 1- roc_obj$sensitivities
TPRCheck <- roc_obj$specificities
head(kindPrediction)
