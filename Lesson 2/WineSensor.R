# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

source("PartitionData.R")

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
DataSplit <- PartitionFast(Students, fractionOfTest=0.4) # ********** Change here
TimeTakenFast <- system.time(PartitionFast(Students, fractionOfTest=0.4)) # ********** Change here
StudentsSplit <- PartitionExact(Students, fractionOfTest=0.4) # ********** Change here
TimeTakenExact <- system.time(PartitionExact(Students, fractionOfTest=0.4)) # ********** Change here
TestStudents <- StudentsSplit$testingData
TrainStudents <-StudentsSplit$trainingData

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
# ********** add code here
glmmodel <-glm(formula=formula, family="binomial",data= TrainStudents)
#glmmodel <-glm(formula=formula, family="gaussian",data= TrainStudents)
# Predict the outcomes for the test data. (predict type="response")
# ********** add code here
predictedProbabilities.GLM <- predict(glmmodel,newdata= TestStudents, type = "response")
###################################################

# Naive Bayes

# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf
# Get the algorithm

reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

# Create Naive Bayes model
# ********** add code here
NaiveBayesModel <-naiveBayes(formula=formula, data = TrainStudents)
# Predict the outcomes for the test data. (predict type="raw")
# ********** add code here
predictedProbabilities.NaiveBayes <-predict(NaiveBayesModel, newdata = TestStudents, type = "raw")
# Question # 5(a) : How many rows are there in the outcome? 2880
# Question # 5(b) : How many columns are there in the outcome? 2
# Question # 5(c) : How many columns are in the output for the logistic regression? 1

###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.5

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
# ********** add code here
predicted.GLM <- ifelse(predictedProbabilities.GLM > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
# ********** add code here
Table.GLM <- table(predicted.GLM,actual,dnn = c("Predicted","Actual"))
print(Table.GLM)
Accuracy.GLM <- (Table.GLM[1,1]+Table.GLM[2,2])/(Table.GLM[1,1]+Table.GLM[1,2]+Table.GLM[2,1]+Table.GLM[2,2])
print("Accuracy of GLM")
print(Accuracy.GLM)
#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
# ********** add code here
predicted.NaiveBayes <- ifelse(predictedProbabilities.NaiveBayes > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
# ********** add code here
#print(table(predicted.NaiveBayes[,1],actual,dnn = c("Predicted","Actual")))
Table.NaiveBayes <- table(predicted.NaiveBayes[,2],actual,dnn = c("Predicted","Actual"))
print(Table.NaiveBayes)
Accuracy.NB <- (Table.NaiveBayes[1,1]+Table.NaiveBayes[2,2])/(Table.NaiveBayes[1,1]+Table.NaiveBayes[1,2]+Table.NaiveBayes[2,1]+Table.NaiveBayes[2,2])
print("Accuracy of Naive Bayes")
print(Accuracy.NB)
###################################################
# Bad Partition; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       325        84
# NotAttend   1368      1103
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (325 + 1103)/(325 + 1368 + 84 + 1103) = 50%

# Fill in the rest:

# Exact Partition; threshold = 0.5 # Question 8 (b)
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      687       227   add results here
# NotAttend   260      1706   add results here 
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (687+1706)/(687+260+227+1706) = .8309   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      570       209   add results here
# NotAttend   377      1724   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (570+1724)/(570+377+209+1724) = .796   add results here

# Fast Partition; threshold = 0.5----Question 8 (a)
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      691       227   add results here
# NotAttend   262      1715   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (691+1715)/(691+262+227+1715) = 0.831   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      572       211   add results here
# NotAttend   381      1731   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (572+1731)/(572+381+211+1731) = 0.795   add results here

# Exact Partition;  threshold = 0.7 8(c)
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      498        81   add results here
# NotAttend   449      1852   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (498+1852)/(498+449+81+1852) = 0.815   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      425       100   add results here
# NotAttend   522      1833   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (425+1833)/(425+522+100+1833) = 0.784   add results here