# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
# Set repeatable random seed.
set.seed(123)
library(e1071)
library(ggplot2)
library(data.table)
library(lubridate)
library(caret)
library(stringr)
library(dplyr)
library(purrr)
library(mice)
library(Boruta)
Loaddata <- function(file)
{
  #browser()
  ## Read the csv file
  #Dataload <- read.csv(file, header = TRUE,stringsAsFactors = FALSE)
  # fread function is more efficent for larger data file and it creates a data table not a data frame in the process.
  Dataload <- fread(file, stringsAsFactors=FALSE)
  #Replace all blanks with NA
  Dataload[Dataload == ""] <- NA
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  Dataload[complete.cases(Dataload), ]
  return(Dataload)
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


# Load the file.
#testdt <- Loaddata('test.csv')
sbertraindt <- Loaddata('train.csv')
copysbertraindt <- sbertraindt
is.data.table(copytraindt)
str(copytraindt)
str(sbertraindt)

##Data Cleansing
sbertraindt <- sbertraindt %>% 
  mutate(max_floor = as.numeric(max_floor), kitch_sq=as.numeric(kitch_sq), num_room=as.numeric(num_room), build_year=as.numeric(build_year)
         #, sub_area=as.factor(sub_area)
         )
sbertraindt <- sbertraindt %>% 
  filter(build_year < 2020 | is.na(build_year))
#sbertraindt <- sbertraindt %>% mutate(strange_full_sq = ifelse(full_sq <= 1, full_sq+1,0), full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))
#sbertraindt <- sbertraindt %>% mutate(strange_life_sq = ifelse(life_sq <= 1, life_sq+1,0), strange_life_sq= ifelse(is.na(strange_life_sq),0,strange_life_sq), life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))
#sbertraindt <- sbertraindt %>% mutate(kitch_sq = as.numeric(kitch_sq),strange_kitch_sq = ifelse(kitch_sq <= 1, kitch_sq+1,0),kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))
#sbertraindt <- sbertraindt %>% mutate(build_year = as.numeric(build_year), strange_build_year = ifelse(build_year <= 1, build_year+1,0), build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))
#sbertraindt <- sbertraindt %>% mutate(max_floor = as.numeric(max_floor), strange_max_floor = ifelse(max_floor <= 1, max_floor+1,0), max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))

sbertraindt <- sbertraindt %>% mutate(full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))
sbertraindt <- sbertraindt %>% mutate(life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))
sbertraindt <- sbertraindt %>% mutate(kitch_sq = as.numeric(kitch_sq),kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))

sbertraindt <- sbertraindt %>% mutate(num_room = as.numeric(num_room))
sbertraindt <- sbertraindt %>% mutate(build_year = as.numeric(build_year),build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))
sbertraindt <- sbertraindt %>% mutate(floor = ifelse(floor > 45, NA, floor))
sbertraindt <- sbertraindt %>% mutate(max_floor = as.numeric(max_floor),max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))
sbertraindt <- sbertraindt %>% mutate(state = as.numeric(state), state = ifelse(state > 4, NA, state))
#sbertraindt <- sbertraindt %>% mutate(material = as.factor(material), material = ifelse(material == 3, NA, material))
sbertraindt <- sbertraindt %>% mutate(material = ifelse(material == 3, NA, material))
#sbertraindt <- sbertraindt %>% mutate(product_type = factor(product_type))
#sbertraindt <- sbertraindt %>% mutate(sub_area = factor(sub_area))
sbertraindt <- sbertraindt %>% filter(kitch_sq < full_sq | is.na(kitch_sq))
sbertraindt <- sbertraindt %>% filter(kitch_sq < life_sq | is.na(kitch_sq))
sbertraindt <- sbertraindt %>% mutate(num_room = ifelse(num_room==0,NA,num_room))
sbertraindt <- sbertraindt %>% mutate(product_type = ifelse(product_type == "Investment", 1, 0))
sbertraindt <- sbertraindt %>% mutate(thermal_power_plant_raion = ifelse(thermal_power_plant_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(culture_objects_top_25 = ifelse(culture_objects_top_25 == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(incineration_raion = ifelse(incineration_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(oil_chemistry_raion = ifelse(oil_chemistry_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(radiation_raion = ifelse(radiation_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(railroad_terminal_raion = ifelse(railroad_terminal_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(big_market_raion = ifelse(big_market_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(nuclear_reactor_raion = ifelse(nuclear_reactor_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(detention_facility_raion = ifelse(detention_facility_raion == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(big_road1_1line = ifelse(big_road1_1line == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(water_1line = ifelse(water_1line == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(railroad_1line = ifelse(railroad_1line == "yes", 1, 0))
sbertraindt <- sbertraindt %>% mutate(ecology = ifelse(ecology == "excellent", 1, ifelse(ecology == "good", 2, ifelse(ecology == "satisfactory", 3, ifelse(ecology == "no data", 4, 5)))))
str(sbertraindt)
#Write dataframe to a csv file.
#fwrite(sbertraindt, file = "sbertraindt.csv")
## Get Year, Month, Week and day.
# Year of the date
sbertraindt <- sbertraindt %>% 
  mutate(year_of_date = year(sbertraindt$timestamp))

# month of year
sbertraindt <- sbertraindt %>% 
  mutate(month_of_year = month(sbertraindt$timestamp))

# week of year
sbertraindt <- sbertraindt %>% 
  mutate(week_of_year = week(sbertraindt$timestamp))

# day of month
sbertraindt <- sbertraindt %>% 
  mutate(day_of_month = mday(sbertraindt$timestamp))

# weekday
sbertraindt <- sbertraindt %>% 
  mutate(day_of_week = wday(sbertraindt$timestamp))

#Check sale patterns across different date patterns
ggplot(data = sbertraindt, aes(x = as.factor(sbertraindt$day_of_month), y = price_doc)) + geom_boxplot(fill = "#5C7457") + labs(title = "Date of the month vs Price", x = "Date", y = "Price")
ggplot(data = sbertraindt, aes(x = as.factor(sbertraindt$month_of_year), y = price_doc)) + geom_boxplot(fill = "#EAC435") + labs(title = "Month vs Price", x = "Month", y = "Price")
ggplot(data = sbertraindt, aes(x = as.factor(sbertraindt$year_of_date), y = price_doc)) + 
  geom_boxplot(fill = "#345995") +
  coord_cartesian(ylim = c(0,10000000)) + labs(title = "Year vs Price", x = "Year", y = "Price")

ggplot(data = sbertraindt, aes(x = as.factor(sbertraindt$week_of_year), y = price_doc)) + geom_boxplot(fill = "#E40066") + labs(title = "Day of the week vs Price", x = "Day", y = "Price")

##Features.
# number of floors to the top of house
sbertraindt <- sbertraindt %>% 
  mutate(floor_from_top = max_floor - floor)

# relative position of floor in house
sbertraindt <- sbertraindt %>% 
  mutate(floor_by_maxfloor = floor/max_floor)

# average room size
sbertraindt <- sbertraindt %>% 
  mutate(roomsize = (life_sq-kitch_sq)/num_room) 

# relative proportion of living area
sbertraindt <- sbertraindt %>% 
  mutate(life_proportion = life_sq/full_sq)

# relative proportion of kitchen area
sbertraindt <- sbertraindt %>% 
  mutate(kitchen_proportion = kitch_sq/full_sq)

# extra area
sbertraindt <- sbertraindt %>% 
  mutate(extra_area = full_sq - life_sq)

# age of house at time of sale
sbertraindt <- sbertraindt %>% 
  mutate(age_at_sale = interval(make_date(year=build_year),timestamp) / years(1))  

#Filter homes with known age.
#sbertraindt <- sbertraindt %>% filter(!is.na(age_at_sale))

#Group Apartment
# assign a common name to them
sbertraindt <- sbertraindt %>% 
  mutate(apartment_name = factor(str_c(sub_area,format(metro_km_avto,digits=3))))
# get the number of apartments in group  
sbertraindt <- sbertraindt %>% 
  count(apartment_name) %>%
  right_join(sbertraindt,by="apartment_name") 

#Is there a sesonal aspect to the price.
# Months of April and June have the highest price, with November being the lowest.
sbertraindt %>% 
  mutate(month=month(timestamp)) %>%
  group_by(month) %>% 
  summarize(med_price=median(price_doc)) %>%
  ggplot(aes(x=as.integer(month), y=med_price)) +
  geom_line(color='red', stat='identity') + 
  geom_point(color='red', size=2) + 
  scale_x_continuous(breaks=seq(1,12,1)) + 
  labs(x='Month', title='Price by month of year')
#Is there an yearly aspect to the price.
#Median home prices are growing steadily over the years, with a steep jump 2014.
sbertraindt %>% 
  mutate(year=year(timestamp)) %>%
  group_by(year) %>% 
  summarize(med_price=median(price_doc)) %>%
  ggplot(aes(x=year, y=med_price)) +
  geom_line(color='red', stat='identity') + 
  geom_point(color='red', size=2) + 
  labs(x='Year', title='Price by year')

#missing data with ggplot
miss_pct <- map_dbl(sbertraindt, function(x) { round((sum(is.na(x)) / length(x)) * 100, digits = 1) })
cat("Features with 100 % data are given below and their count is : ", length(which(miss_pct==0)))

miss_pct <- miss_pct[miss_pct > 0]
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Add all the new features in test data.

# zero variance variables
insignificant <- nearZeroVar(sbertraindt)
print(names(sbertraindt[ , insignificant]))

#Remove all zero variance variables from training data after converting to data frame
sbertrainwithoutvardt <- as.data.frame(sbertraindt)
sbertrainwithoutvardt[,insignificant] <- NULL

#Remove all factor variables from test data
#x <-  sapply(testwithoutvardt, class) == "factor"
#testwithoutvardt[,x] <- NULL
#testwithoutfactordt <- testwithoutvardt[, x]

#Remove all factor variables from train data
#y <- sapply(trainwithoutvardt, class) == "factor"
#trainwithoutvardt[,y] <- NULL
#trainwithoutfactordt <- trainwithoutvardt[, y]


#Convert back to data tables.
#trainwithoutvardt <- as.data.table(trainwithoutvardt)
#testwithoutvardt <- as.data.table(testwithoutvardt)

#Fix missing values.
md.pattern(sbertrainwithoutvardt)
imputed_Data <- mice(sbertrainwithoutvardt, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)


#Split this dataset in test and train datasets.
sbertraindtsvm = PartitionExact(sbertrainwithoutvardt)
testdt <- sbertraindtsvm$testingData
traindt <-sbertraindtsvm$trainingData

cat("Count of Train Dataset : ", nrow(traindt),"\n" )
cat("Count of Test Dataset : ", nrow(testdt),"\n" )

#User Boruta to remove unnecessary features.
boruta.traindt <- Boruta(price_doc ~ . - id, data = traindt, doTrace = 2)

# Create a svm model
svmmodel <- svm(price_doc ~ . - id, traindt)
#lmmodel <- lm(price_doc ~ . - id, traindt)
summary(svmmodel)
#summary(lmmodel)

# Run PCA to pick up important features
predictedsvm <- predict(svmmodel, testdt)
#predictedlm <- predict(lmmodel, testdt)

#Check Accuracy
tablesvm <- table(predictedsvm,testdt$price_doc)
#tablesvm <- table(predictedlm,testdt$price_doc)
#Accuracy
Accuracysvm <- Accuracy(tablesvm)
cat("Model Accuracy On Test Dataset : ", Accuracysvm,"\n" )
# Calculate TPR of SVM
TPRsvm <- Specificity(tablesvm)
cat("Model TPR On Test Dataset : ", TPRsvm,"\n" )
# Calculate FPR of SVM
FPRsvm <- FPR(tablesvm)
cat("Model FPR On Test Dataset : ", FPRsvm,"\n" )
