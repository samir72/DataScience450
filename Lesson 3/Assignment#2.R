rm(list=ls())
# Clear Console:
cat("\014")
# Set repeatable random seed.
set.seed(123)
#Load the dataset.
read.bank = function(file = 'Bank Data.csv'){
  ## Read the csv file
  bank <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  bank[complete.cases(bank), ]
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
bank = read.bank()
#Check the structure of the dataset.
str(bank)

#Converting character columns to numeric.
bank$pep <- as.numeric(bank$pep == "YES")
bank$sex <- as.numeric(bank$sex == "MALE")
bank$region[bank$region == "INNER_CITY"] <- 1
bank$region[bank$region == "TOWN"] <- 2
bank$region[bank$region == "RURAL"] <- 3
bank$region[bank$region == "SUBURBAN"] <- 4
bank$region <- as.numeric(bank$region)
bank$married <- as.numeric(bank$married == "YES")
bank$car <- as.numeric(bank$car == "YES")
bank$save_act <- as.numeric(bank$save_act == "YES")
bank$current_act <- as.numeric(bank$current_act == "YES")
bank$mortgage <- as.numeric(bank$mortgage == "YES")
str(bank)

#check the data set header
head(bank)
tail(bank)


lapply(bank, summary)
#Data Exploration
library(reshape2)
library(ggplot2)

ggplot(data = melt(bank), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')



#Split dataset into test and training data sets.
bankDataset = PartitionExact(bank)
Testbank <- bankDataset$testingData
Trainbank <-bankDataset$trainingData

nrow(Testbank)
head(Testbank)
nrow(Trainbank)
head(Trainbank)


formula <- pep ~ .
formula3 <- pep ~ . - sex - region - current_act


# Classification Tree with rpart
library(rpart)
# grow tree 
banktree <- rpart(formula = formula, data = Trainbank,method="class")
banktree3 <- rpart(formula = formula3, data = Trainbank,method="class")

#Plot tree using rpart.plot
library("rpart.plot")
rpart.plot(banktree)
rpart.plot(banktree3)

#Prune Tree
prune_banktree <- prune(banktree, cp = 0.03)
prune_banktree3 <- prune(banktree3, cp = 0.03)
#Plot tree after pruning.
rpart.plot(prune_banktree)
rpart.plot(prune_banktree3)

#Use the model to make the predictions.
PEPPrediction <- as.numeric(predict(banktree, newdata = Testbank, type="class"))
PEPPrediction3 <- as.numeric(predict(banktree3, newdata = Testbank, type="class"))
PrunePEPPrediction <- as.numeric(predict(prune_banktree, newdata = Testbank, type="class"))
PrunePEPPrediction3 <- as.numeric(predict(prune_banktree3, newdata = Testbank, type="class"))

#Calculate Confusion Matrix for all attributes.
crosstabAll <- table(PEPPrediction, Testbank$pep)
crosstabAll

#Calculate Confusion Matrix for selected features
crosstab3 <- table(PEPPrediction3, Testbank$pep)
crosstab3

#Calculate Confusion Matrix for all pruned attributes.
PrunecrosstabAll <- table(PrunePEPPrediction, Testbank$pep)
PrunecrosstabAll

#Calculate Confusion Matrix for selected pruned features
Prunecrosstab3 <- table(PrunePEPPrediction3, Testbank$pep)
Prunecrosstab3

Accuracy <- function(Table.X)
{
  Accuracy.X <- (Table.X[1,1]+Table.X[2,2])/(Table.X[1,1]+Table.X[1,2]+Table.X[2,1]+Table.X[2,2])
  return(Accuracy.X)
}
# Calculate Accuracy for all attributes
AccuracyAll <- Accuracy(crosstabAll)
cat("Accuracy for all columns is : ", AccuracyAll)

# Calculate Accuracy for selected attributes
AccuracyA3 <- Accuracy(crosstab3)
cat("Accuracy for selected columns is : ", AccuracyA3)


#Calculate Specificity (TPR)
Specificity <- function(Table.X)
{
  Specificity.X <- (Table.X[2,2])/(Table.X[1,2]+Table.X[2,2])
  return(Specificity.X)
}
# Calculate TPR for all attributes
TPRAll <- Specificity(crosstabAll)
cat("TPR for all columns is : ", TPRAll)

# Calculate TPR for selected attributes
TPRA3 <- Specificity(crosstab3)
cat("TPR for selected columns is : ", TPRA3)

# Calculate Accuracy for all pruned attributes
PruneAccAll <- Accuracy(PrunecrosstabAll)
cat("Accuracy of pruned model for all columns is : ", PruneAccAll)

# Calculate Accuracy for certain pruned attributes
PruneAcc3 <- Accuracy(Prunecrosstab3)
cat("Accuracy of pruned model for certain columns is : ", PruneAcc3)

# Calculate TPR for all pruned attributes
PruneTPRAll <- Specificity(PrunecrosstabAll)
cat("TPR of pruned model for all columns is : ", PruneTPRAll)

# Calculate TPR for selected pruned attributes
PruneTPRA3 <- Specificity(Prunecrosstab3)
cat("TPR of pruned model for selected columns is : ", PruneTPRA3)

#calculate FPR from Confusion MAtrix
FPR <- function(Table.X)
{
  FPR.X <- (Table.X[1,2])/(Table.X[1,2]+Table.X[2,2])
  return(FPR.X)
}
# Calculate FPR for all attributes
FPRAll <- FPR(crosstabAll)
cat("FPR of all columns is : ", FPRAll)

# Calculate FPR for selected attributes
FPR3 <- FPR(crosstab3)
cat("FPR of selected columns is : ", FPR3)

# Calculate FPR for all pruned attributes
PruneFPRAll <- FPR(PrunecrosstabAll)
cat("FPR of pruned model for all columns is : ", PruneFPRAll)

# Calculate FPR for selected pruned attributes
PruneFPR3 <- FPR(Prunecrosstab3)
cat("FPR of pruned model for certain columns is : ", PruneFPR3)

#Calculate AUC
library(pROC)
roc_obj_All <- roc(Testbank$pep, PEPPrediction)
auc(roc_obj_All)

roc_obj_3 <- roc(Testbank$pep, PEPPrediction3)
auc(roc_obj_3)

#Calculate AUC for Pruned tree
roc_obj_Prune_All <- roc(Testbank$pep, PrunePEPPrediction)
auc(roc_obj_Prune_All)

roc_obj_Prune_3 <- roc(Testbank$pep, PrunePEPPrediction3)
auc(roc_obj_Prune_3)
