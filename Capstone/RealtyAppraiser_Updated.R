# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
# Set repeatable random seed.
set.seed(123)
#install.packages("xgboost")
library("xgboost")
#install.packages("Matrix")
library(Matrix)
library(ggplot2)
library(data.table)
#install.packages("caret")
library(caret)
library(plyr)
library(dplyr)
#install.packages("purrr")
library(purrr)
#install.packages("Boruta")
library(Boruta)
#install.packages("Hmisc")
library(Hmisc)
Loaddata <- function(file)
{
  # fread function is more efficent for larger data file and it creates a data table not a data frame in the process.
  Dataload <- fread(file, stringsAsFactors=FALSE)
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  Dataload[complete.cases(Dataload), ]
  return(Dataload)
}

# Load the file.
sbermacrodt <- Loaddata('macro.csv')
sbertestdt <- Loaddata('test.csv')
sbertraindt <- Loaddata('train.csv')
sbersubsampdt <- Loaddata('sample_submission.csv')
copysbertraindt <- sbertraindt
copysbertestdt <- sbertestdt

#Add price_doc for rbind
sbertestdt$price_doc <- NA
#Combine train and test data.
sbertraintestdt <- rbind(sbertraindt, sbertestdt)

#Merge macro data to training/test data.
sbertraintestdt <- merge(x = sbertraintestdt, y = sbermacrodt, by = "timestamp", all.x = TRUE)

#Transform all factor features to numeric.
sbertraintestdt_ft <- colnames(sbertraintestdt)

for (ft in sbertraintestdt_ft) 
{
  if ((class(sbertraintestdt[[ft]])=="factor") || (class(sbertraintestdt[[ft]])=="character"))
  {
    levels <- unique(sbertraintestdt[[ft]])
    sbertraintestdt[[ft]] <- as.numeric(factor(sbertraintestdt[[ft]], levels=levels))
  }
}
#Carve out train dataset.
sbertraindt = sbertraintestdt[1:nrow(sbertraindt),]
#Carve out test dataset.
sbertestdt = sbertraintestdt[(nrow(sbertraindt)+1):(nrow(sbertraindt)+nrow(sbertestdt)),]


##Data Cleansing
#Replace all blanks with NA
sbertraindt[sbertraindt == ""] <- NA
sbertraindt <- sbertraindt %>% 
  mutate(max_floor = as.numeric(max_floor), kitch_sq=as.numeric(kitch_sq), num_room=as.numeric(num_room), build_year=as.numeric(build_year)
         #, sub_area=as.factor(sub_area)
  )
sbertraindt <- sbertraindt %>% 
  filter(build_year < 2020 | is.na(build_year))
sbertraindt <- sbertraindt %>% mutate(full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))
sbertraindt <- sbertraindt %>% mutate(life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))
sbertraindt <- sbertraindt %>% mutate(kitch_sq = as.numeric(kitch_sq),kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))

sbertraindt <- sbertraindt %>% mutate(num_room = as.numeric(num_room))
sbertraindt <- sbertraindt %>% mutate(build_year = as.numeric(build_year),build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))
sbertraindt <- sbertraindt %>% mutate(floor = ifelse(floor > 45, NA, floor))
sbertraindt <- sbertraindt %>% mutate(max_floor = as.numeric(max_floor),max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))
sbertraindt <- sbertraindt %>% mutate(state = as.numeric(state), state = ifelse(state > 4, NA, state))
sbertraindt <- sbertraindt %>% mutate(material = ifelse(material == 3, NA, material))
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


# zero variance variables
insignificant <- nearZeroVar(sbertraindt)

#Remove all zero variance variables from training data after converting to data frame
sbertrainwithoutvardt <- as.data.frame(sbertraindt)
sbertestwithoutvardt <- as.data.frame(sbertestdt)
print(names(sbertrainwithoutvardt[ , insignificant]))
sbertrainwithoutvardt[,insignificant] <- NULL
#Remove all zero variance variable from test data.
sbertestwithoutvardt[,insignificant] <- NULL

miss_pct <- map_dbl(sbertrainwithoutvardt, function(x) { round((sum(is.na(x)) / length(x)) * 100, digits = 1) })
cat("Count of Features with 100 % data is : ", length(which(miss_pct==0)))

#missing data with ggplot
miss_pct <- miss_pct[miss_pct > 0]
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Remove features with more than 5 % missing data
miss_pctgt5 <- miss_pct[miss_pct >= 5]
#Get features with less than 5 % data
miss_pctlt5 <- miss_pct[miss_pct < 5]
#remove <- miss_pctgt5["price_doc"]# Remove price_doc as its required in feature selection.
#miss_pctgt5 <- miss_pctgt5 [! miss_pctgt5 %in% remove]
#miss_pctgt5 <- miss_pctgt5[-38]
sbertrainwithoutvardt[,names(miss_pctgt5)] <- NULL
#Remove from test data.
sbertestwithoutvardt[,names(miss_pctgt5)] <- NULL

#Using Hmisc package
# impute with the median
sbertrainwithoutvardt$full_sq <- with(sbertrainwithoutvardt, impute(full_sq, median))
sbertrainwithoutvardt$floor <- with(sbertrainwithoutvardt, impute(floor, median))
sbertrainwithoutvardt$metro_min_walk <- with(sbertrainwithoutvardt, impute(metro_min_walk, median))
sbertrainwithoutvardt$metro_km_walk <- with(sbertrainwithoutvardt, impute(metro_km_walk, median))
sbertrainwithoutvardt$railroad_station_walk_km <- with(sbertrainwithoutvardt, impute(railroad_station_walk_km, median))
sbertrainwithoutvardt$railroad_station_walk_min <- with(sbertrainwithoutvardt, impute(railroad_station_walk_min, median))
sbertrainwithoutvardt$ID_railroad_station_walk <- with(sbertrainwithoutvardt, impute(ID_railroad_station_walk, median))
sbertrainwithoutvardt$cafe_sum_3000_min_price_avg <- with(sbertrainwithoutvardt, impute(cafe_sum_3000_min_price_avg, median))
sbertrainwithoutvardt$cafe_avg_price_3000 <- with(sbertrainwithoutvardt, impute(cafe_avg_price_3000, median))
sbertrainwithoutvardt$cafe_sum_3000_max_price_avg <- with(sbertrainwithoutvardt, impute(cafe_sum_3000_max_price_avg, median))
sbertrainwithoutvardt$prom_part_5000 <- with(sbertrainwithoutvardt, impute(prom_part_5000, median))
sbertrainwithoutvardt$cafe_sum_5000_min_price_avg <- with(sbertrainwithoutvardt, impute(cafe_sum_5000_min_price_avg, median))
sbertrainwithoutvardt$cafe_sum_5000_max_price_avg <- with(sbertrainwithoutvardt, impute(cafe_sum_5000_max_price_avg, median))
sbertrainwithoutvardt$cafe_avg_price_5000 <- with(sbertrainwithoutvardt, impute(cafe_avg_price_5000, median))

#User Boruta to remove unnecessary features.
boruta.traindt <- Boruta(price_doc ~ ., data = sbertrainwithoutvardt, doTrace = 0)
print(boruta.traindt)
plot(boruta.traindt, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.traindt$ImpHistory),function(i) boruta.traindt$ImpHistory[is.finite(boruta.traindt$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.traindt$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.traindt$ImpHistory), cex.axis = 0.7)
#Decide on Boruta tentative features.
final.boruta <- TentativeRoughFix(boruta.traindt)
print(final.boruta)
#Get the final list of confirmed features.
ConfirmedAttrributes <- getSelectedAttributes(final.boruta, withTentative = F)
print(ConfirmedAttrributes)
#Create a dataframe based on the final result from Boruta.
boruta.df <- attStats(final.boruta)
print(boruta.df)

finalsbertraindt <- sbertrainwithoutvardt
finalsbertestdt <- sbertestwithoutvardt
Rejects <- boruta.df[boruta.df$decision == 'Rejected',]
#Remove rejected features from train data.
finalsbertraindt[,row.names(Rejects)] <- NULL
#Remove rejected features from test data
finalsbertestdt[,row.names(Rejects)] <- NULL


#Carve out price_doc from training data/
train_price <- finalsbertraindt$price_doc
#Train data : Remove id and timestamp to avoid overfitting.
sbertraindt <- subset(finalsbertraindt, select = -c(id,timestamp,price_doc))
#Test data : Remove id and timestamp to avoid overfitting.
test_id = finalsbertestdt$id
sbertestdt <- subset(finalsbertestdt, select = -c(id,timestamp,price_doc))

#Get the differences in columns between two df.
difference <- setdiff(names(sbertraindt),names(sbertestdt))

cat("Count of Train Dataset : ", nrow(sbertraindt),"\n" )
cat("Count of Test Dataset : ", nrow(sbertestdt),"\n" )

#Use xGBoost model for training.

dtrain = xgb.DMatrix(as.matrix(sbertraindt), label=log(train_price+1))
dtest = xgb.DMatrix(as.matrix(sbertestdt))

watchlist <- list(train=dtrain, test=dtest)
parameters = list(
  seed = 0,
  colsample_bytree = 1,
  subsample = 1,
  eta = 0.1,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 15.69,
  watchlist=watchlist
)

cv_result = xgb.cv(parameters,
                   dtrain,
                   nrounds=2000,
                   nfold=10,
                   early_stopping_rounds=20,
                   print_every_n = 10,
                   verbose= 2,
                   maximize=F)

cv_nrounds = cv_result$best_iteration

bstmodel = xgb.train(parameters, dtrain, cv_nrounds)

bstprediction <- predict(bstmodel,dtest)
sbersubsampdt$price_doc <- exp(bstprediction)-1

#Show important features used in this model.
importance_matrix <- xgb.importance(model = bstmodel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# Write predictions to the csv file.
write.csv(data.table(id=test_id, price_doc=sbersubsampdt$price_doc), "Amir_XGBPrediction.csv", row.names = F)
