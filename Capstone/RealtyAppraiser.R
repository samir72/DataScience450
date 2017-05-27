# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
library(ggplot2)
library(reshape2)
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)
library(caret)
library(stringr)
Loaddata <- function(file)
{
  #browser()
  ## Read the csv file
  Dataload <- read.csv(file, header = TRUE,stringsAsFactors = FALSE)
  # fread function is more efficent for larger data file and it creates a data table not a data frame in the process.
  #Dataload <- fread(file, stringsAsFactors=TRUE)
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  Dataload[complete.cases(Dataload), ]
  return(Dataload)
}

# Load the file.
testdt <- Loaddata('test.csv')
traindt <- Loaddata('train.csv')

copytraindt <- traindt
str(copytraindt)
##Data Cleansing
traindt <- traindt %>% 
  mutate(max_floor = as.numeric(max_floor), kitch_sq=as.numeric(kitch_sq), num_room=as.numeric(num_room), build_year=as.numeric(build_year), sub_area=as.factor(sub_area))
traindt <- traindt %>% 
  filter(build_year < 2020 | is.na(build_year))
traindt <- traindt %>% mutate(strange_full_sq = ifelse(full_sq <= 1, full_sq+1,0), full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))
traindt <- traindt %>% mutate(strange_life_sq = ifelse(life_sq <= 1, life_sq+1,0), strange_life_sq= ifelse(is.na(strange_life_sq),0,strange_life_sq), life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))
traindt <- traindt %>% mutate(kitch_sq = as.numeric(kitch_sq),strange_kitch_sq = ifelse(kitch_sq <= 1, kitch_sq+1,0),kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))
traindt <- traindt %>% mutate(num_room = as.numeric(num_room))
traindt <- traindt %>% mutate(build_year = as.numeric(build_year), strange_build_year = ifelse(build_year <= 1, build_year+1,0), build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))
traindt <- traindt %>% mutate(floor = ifelse(floor > 45, NA, floor))
traindt <- traindt %>% mutate(max_floor = as.numeric(max_floor), strange_max_floor = ifelse(max_floor <= 1, max_floor+1,0), max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))
traindt <- traindt %>% mutate(state = as.numeric(state), state = ifelse(state > 4, NA, state))
traindt <- traindt %>% mutate(material = as.factor(material), material = ifelse(material == 3, NA, material))
traindt <- traindt %>% mutate(product_type = factor(product_type))
traindt <- traindt %>% mutate(sub_area = factor(sub_area))
traindt <- traindt %>% filter(kitch_sq < full_sq | is.na(kitch_sq))
traindt <- traindt %>% filter(kitch_sq < life_sq | is.na(kitch_sq))
traindt <- traindt %>% mutate(num_room = ifelse(num_room==0,NA,num_room))
str(traindt)
## Get Year, Month, Week and day.
# Year of the date
traindt <- traindt %>% 
  mutate(year_of_date = year(traindt$timestamp))

# month of year
traindt <- traindt %>% 
  mutate(month_of_year = month(traindt$timestamp))

# week of year
traindt <- traindt %>% 
  mutate(week_of_year = week(traindt$timestamp))

# day of month
traindt <- traindt %>% 
  mutate(day_of_month = mday(traindt$timestamp))

# weekday
traindt <- traindt %>% 
  mutate(day_of_week = wday(traindt$timestamp))

##Features.
# number of floors to the top of house
traindt <- traindt %>% 
  mutate(floor_from_top = max_floor - floor)

# relative position of floor in house
traindt <- traindt %>% 
  mutate(floor_by_maxfloor = floor/max_floor)

# average room size
traindt <- traindt %>% 
  mutate(roomsize = (life_sq-kitch_sq)/num_room) 

# relative proportion of living area
traindt <- traindt %>% 
  mutate(life_proportion = life_sq/full_sq)

# relative proportion of kitchen area
traindt <- traindt %>% 
  mutate(kitchen_proportion = kitch_sq/full_sq)

# extra area
traindt <- traindt %>% 
  mutate(extra_area = full_sq - life_sq)

# age of house at time of sale
traindt <- traindt %>% 
  mutate(age_at_sale = interval(make_date(year=build_year),timestamp) / years(1))  

#Group Apartment
# assign a common name to them
traindt <- traindt %>% 
  mutate(apartment_name = factor(str_c(sub_area,format(metro_km_avto,digits=3))))
# get the number of apartments in group
traindt <- traindt %>% 
  group_by(apartment_name) %>% 
  tally() %>% 
  right_join(traindt,by="apartment_name") 


#missing data with ggplot
miss_pct <- map_dbl(traindt, function(x) { round((sum(is.na(x)) / length(x)) * 100, digits = 1) })
miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#missing data.
missing <- data.frame(sapply(traindt, function(x) sum(is.na(x))*100/length(x)))
missing$feature <- names(traindt)
missing$num <- c(1:nrow(missing))
colnames(missing) <- c("missing_ratio", "feature", "num")
missing <- missing[, c("num", "feature","missing_ratio")]
print(missing[missing$missing_ratio != 0,], row.names = F)
#Is there a sesonal aspect to the price.
# Months of April and June have the highest price, with November being the lowest.
traindt %>% 
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
traindt %>% 
  mutate(year=year(timestamp)) %>%
  group_by(year) %>% 
  summarize(med_price=median(price_doc)) %>%
  ggplot(aes(x=year, y=med_price)) +
  geom_line(color='red', stat='identity') + 
  geom_point(color='red', size=2) + 
  labs(x='Year', title='Price by year')

#Total features with missing values
nrow(missing[missing$missing_ratio != 0,])

# zero variance variables
insignificant <- nearZeroVar(traindt)
print(names(traindt[ , insignificant]))

### Extracting date, month, year, weekday from timestamp
traindt$date <- as.POSIXct(strptime(traindt$timestamp, format = "%Y-%m-%d"))
traindt$day <- as.integer(format(traindt$date, "%d")) # day
traindt$month <- as.factor(format(traindt$date, "%m")) # month
traindt$year <- as.integer(format(traindt$date, "%Y")) # year
traindt$weekday <- as.factor(format(traindt$date, "%u")) # weekday
traindt$yearmonth <- paste0(traindt$year, traindt$month)
traindt$timestamp <- NULL
traindt$date <- NULL

ggplot(data = traindt, aes(x = as.factor(day), y = price_doc)) + geom_boxplot(fill = "#5C7457") + labs(title = "Date of the month vs Price", x = "Date", y = "Price")
ggplot(data = traindt, aes(x = as.factor(month), y = price_doc)) + geom_boxplot(fill = "#EAC435") + labs(title = "Month vs Price", x = "Month", y = "Price")
ggplot(data = traindt, aes(x = as.factor(year), y = price_doc)) + 
  geom_boxplot(fill = "#345995") +
  coord_cartesian(ylim = c(0,10000000)) + labs(title = "Year vs Price", x = "Year", y = "Price")

ggplot(data = traindt, aes(x = as.factor(weekday), y = price_doc)) + geom_boxplot(fill = "#E40066") + labs(title = "Day of the week vs Price", x = "Day", y = "Price")

# Check the structure of the test data frame.
str(testdt)
testcount <- nrow(testdt)
# Check the structure of the train data frame.
str(traindt)
traincount <-nrow(traindt)

# Check the header data
head(traindt)
#Check the summary of the data frame.
lapply(traindt, summary)

#Get unique values of Sub Area.
unique(traindt$sub_area)
#Data visualization
hist(traindt$num_room)

# Boxplot of price by product type
boxplot(price_doc~product_type,data=traindt, main="Price by product type",
        xlab="Product Type", ylab="Price Doc") 

