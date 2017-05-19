# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
library(ggplot2)
library(reshape2)
Loaddata <- function(file)
{
  #browser()
  ## Read the csv file
  Dataload <- read.csv(file, header = TRUE,stringsAsFactors = FALSE)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  Dataload[complete.cases(Dataload), ]
  return(Dataload)
}
#MinMax Function
MinMax <- function(Column)
{
  x <- Column
  y <- 1000*x
  a <- min(x)
  b <- max(x) - min(x)
  minmaxnormalized <- (x - a) / b
  return(minmaxnormalized)
}
#Discretize Function
Discretize <- function(Column)
{
  #browser()
  x1 <- Column
  range <- max(x1) - min(x1)
  binWidth <- range / 3
  bin1Min <- -Inf
  bin1Max <- min(x1) + binWidth
  bin2Max <- min(x1) + 2*binWidth
  bin3Max <- min(x1) + 3*binWidth
  bin4Max <- Inf

  xDiscretized <- rep(NA, length(x1))
  
  xDiscretized[bin1Min < x1 & x1 <= bin1Max] <- "Low"
  xDiscretized[bin1Max < x1 & x1 <= bin2Max] <- "Medium"
  xDiscretized[bin2Max < x1 & x1 <= bin4Max] <- "High"
  #xDiscretized[bin3Max < x1 & x1 <= bin4Max] <- "Very High"
  
  return(xDiscretized)
}

# Load the file.
videostore <- Loaddata('Video_Store.csv')

# Check the structure of the data frame.
str(videostore)

# Check the header data
head(videostore)
#Check the summary of the data frame.
lapply(videostore, summary)

#Data visualization

ggplot(data = melt(videostore), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#Min-Max Normalization:
#Income
minmaxnormalized<- MinMax(videostore$Income)
videostore$minmaxIncome <- minmaxnormalized
#Age
minmaxnormalized<- MinMax(videostore$Age)
videostore$minmaxAge <- minmaxnormalized
#Rentals
minmaxnormalized<- MinMax(videostore$Rentals)
videostore$minmaxRentals <- minmaxnormalized
#Avg.Per.Visit
minmaxnormalized<- MinMax(videostore$Avg.Per.Visit)
videostore$minmaxAvgPerVisit <- minmaxnormalized

#z-Score Normalizaition
videostore$zScoreRental <- scale(videostore$Rentals)

#Discretization into 3 bins
#Income
xDiscretized <- Discretize(videostore$Income)
videostore$IncomeBin <- xDiscretized
#Age
xDiscretized <- Discretize(videostore$Age)
videostore$AgeBin <- xDiscretized

#Write dataframe to a csv file (A to D).
write.csv(videostore, file = "videostorAtoD.csv")

# Unique Genre values.
unique(videostore$Genre)
# Binarize.
videostore$BinarizedGender <- ifelse(videostore$Gender == 'M',1,0)
videostore$BinarizedGenre <- ifelse(videostore$Genre == 'Action',1,(ifelse(videostore$Genre == 'Drama',2,3)))
# Check the header data after binarizing the categorical variables.
head(videostore)

# Boxplot of Movie Rental By Genre.
boxplot(Rentals~Genre,data=videostore, main="Movie Rental by Genre",
        xlab="Movie Genre", ylab="Rental Count") 
# Action genre is popular in the general population.
# Boxplot of Movie Rental By Income.
boxplot(Rentals~IncomeBin,data=videostore, main="Movie Rental by Income",
        xlab="Income Bin", ylab="Rental Count") 
#Low income group rents the highest number of videos.

table(videostore$Gender,videostore$Genre)
#Females watch more of drama and men watch more of action.

# Boxplot of Gender by Genre
boxplot(BinarizedGender~Genre,data=videostore, main="Gender by Genre",
        xlab="Genre", ylab="Gender")
#Females watch more of drama and men watch more of action.

# Boxplot of Gender by rental
boxplot(Rentals~Gender,data=videostore, main="Rental by Gender",
        xlab="Gender", ylab="Rental")
#In general population Males watch more videos.

#h
#Get good customers.
videostoregoodcust <- videostore[videostore$Rentals >= 30,]
#Check the summary of general customers.
lapply(videostore, summary)
# Boxplot of Movie Rental By Genre.
boxplot(Rentals~Genre,data=videostoregoodcust, main="Movie Rental by Genre for good customers",
        xlab="Movie Genre", ylab="Rental Count") 
#Good customers tend to watch more movies of drama genre followed by action genre.
# Boxplot of Gender by rental
boxplot(Rentals~Gender,data=videostoregoodcust, main="Rental by Gender for good customers",
        xlab="Gender", ylab="Rental")
#Females rent the highest number of vides in the category of good customers.
#Check the summary of good customers.
lapply(videostoregoodcust, summary)
#Mean income of good customers is lower than the general customers.
#Mean age of good customers is lower than the general customers.
#Mean rentals of good customers are higher than general customers.
#Mean Avg.Per.Visit of good customers is slightly higher than general customers, median value however is lower for good customers.


# Boxplot of income by incidentals
boxplot(minmaxIncome~Incidentals,data=videostore, main="Income by incidentals",
        xlab="Incidentals", ylab="Income") 
#Income does not affect the incidentals.
# Boxplot of Genre by incidentals
boxplot(BinarizedGenre~Incidentals,data=videostore, main="Genre by incidentals",
        xlab="Incidentals", ylab="Genre") 
#More movies with action genre should increase the incidentals.

#Write dataframe to a csv file(E to I).
write.csv(videostoregoodcust, file = "videostoreEtoI.csv")




