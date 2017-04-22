Name <- "RedWhiteWine.csv"
Wine <- read.csv(Name, header=T, stringsAsFactors=FALSE)
Students <- Students[order(-Students$ParentIncome), ]
Students$CollegePlans <- as.numeric(Students$CollegePlans == "Plans to attend")
formula <- CollegePlans ~ Gender + ParentIncome + IQ + ParentEncouragement

# Set repeatable random seed. Do not change the following line
set.seed(4)

PartitionWrong <- function(dataSet, fractionOfTest = 0.3)
{
  #browser()
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  return(dataSetSplit)
}

PartitionExact <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  #browser()
  random <-runif(nrow(dataSet))
  quant <- quantile(random,fractionOfTest)
  testFlag <- random <= quant
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  return(dataSetSplit)
}

PartitionFast <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  #browser()
  random <-runif(nrow(dataSet))
  testFlag <- random <=fractionOfTest
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  
  return(dataSetSplit)
}
