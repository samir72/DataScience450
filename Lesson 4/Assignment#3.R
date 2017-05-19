# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
library('cluster')
library('fpc')
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

# Load the file.
sessiondata <- Loaddata('Sessions.csv')
#names
names(sessiondata)
# Prepare for clustering
clustersessiondata <- subset(sessiondata, select = -c(Purchase))

head(clustersessiondata)

# Set seed data.
set.seed(23)
#Perform K-Means with 4-10 clusters
for (j in 4:10) {
  #     browser()
  kmeanscluster <- kmeans(clustersessiondata,j)
  cat("\n","This kmean run is for a k value of :", j, "\n" )
  print(kmeanscluster)
  #Compare the clusters with purchase
  cat("\n","Compare the clusters with purchase for a K value of : ", j,"\n")
  print(table(kmeanscluster$cluster, sessiondata$Purchase))
  #Plot the clusters.
  # Create a cluster plot using the first 2 principal components
  clusplot(sessiondata, kmeanscluster$cluster, color=TRUE, shade=TRUE, 
           labels=2, lines=0)
}
#Use Silhouette method
pamkcluster <- pamk(clustersessiondata)
cat("Number of clusters estimated by optimum average silhouette width:", pamkcluster$nc, "\n" )
print(pamkcluster)
plot(pam(sessiondata, pamkcluster$nc))

