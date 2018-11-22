#------------------Question 1-----------------------Question 1---------------------

setwd("C:/Users/Aditya Murali/Desktop")
library(Hmisc)
library(leaps)
library(NbClust)
library(factoextra)

#Read the data file
mydata <- read.csv("dungaree.csv")

#Set seed value = 42
set.seed(42)

#Check if any NAs are present in the data
is.na(mydata$FASHION)
is.na(mydata$LEISURE)
is.na(mydata$STRETCH)
is.na(mydata$ORIGINAL)

#Check for outliers in the data

#Check for outliers in FASHION jeans
outliers_FASH <- boxplot.stats(mydata$FASHION)$out  # outlier values.
outliers_FASH <- sort(outliers_FASH)
boxplot(mydata$FASHION, main="Fashion", boxwex=1)
mtext(paste("Outliers: ", paste(outliers_FASH, collapse=", ")), cex=0.8)

#Check for outliers in LEISURE jeans
outliers_LEIS <- boxplot.stats(mydata$LEISURE)$out  # outlier values.
outliers_LEIS <- sort(outliers_LEIS)
boxplot(mydata$LEISURE, main="Leisure", boxwex=1)
mtext(paste("Outliers: ", paste(outliers_LEIS, collapse=", ")), cex=0.8)

#Check for outliers in STRETCH jeans
outliers_STRE <- boxplot.stats(mydata$STRETCH)$out  # outlier values.
outliers_STRE <- sort(outliers_STRE)
boxplot(mydata$STRETCH, main="Stretch", boxwex=1)
mtext(paste("Outliers: ", paste(outliers_STRE, collapse=", ")), cex=0.8)

#Check for outliers in ORIGINAL jeans
outliers_ORG <- boxplot.stats(mydata$ORIGINAL)$out  # outlier values.
outliers_ORG <- sort(outliers_ORG)
boxplot(mydata$ORIGINAL, main="Original", boxwex=1)
mtext(paste("Outliers: ", paste(outliers_ORG, collapse=", ")), cex=0.8)

#K means is extremely sensitive to outliers, hence I have Winsorized the outliers

#Treat outlier values using Winsorization

val1 <- mydata$FASHION
qnt <- quantile(val1, probs=c(.25, .75), na.rm = T)
caps <- quantile(val1, probs=c(.02, .98), na.rm = T)
H <- 1.5 * IQR(val1, na.rm = T)
val1[val1 < (qnt[1] - H)] <- caps[1]
val1[val1 > (qnt[2] + H)] <- caps[2]

val2 <- mydata$STRETCH
qnt <- quantile(val2, probs=c(.25, .75), na.rm = T)
caps <- quantile(val2, probs=c(.02, .98), na.rm = T)
H <- 1.5 * IQR(val2, na.rm = T)
val2[val2 < (qnt[1] - H)] <- caps[1]
val2[val2 > (qnt[2] + H)] <- caps[2]

val3 <- mydata$LEISURE
qnt <- quantile(val3, probs=c(.25, .75), na.rm = T)
caps <- quantile(val3, probs=c(.02, .98), na.rm = T)
H <- 1.5 * IQR(val3, na.rm = T)
val3[val3 < (qnt[1] - H)] <- caps[1]
val3[val3 > (qnt[2] + H)] <- caps[2]

val4 <- mydata$ORIGINAL
qnt <- quantile(val4, probs=c(.25, .75), na.rm = T)
caps <- quantile(val4, probs=c(.02, .98), na.rm = T)
H <- 1.5 * IQR(val4, na.rm = T)
val4[val4 < (qnt[1] - H)] <- caps[1]
val4[val4 > (qnt[2] + H)] <- caps[2]

#Put values back into original variables
mydata$FASHION <- val1
mydata$STRETCH <- val2
mydata$LEISURE <- val3
mydata$ORIGINAL <- val4

#Replace indices with first column and drop the first column
row.names(mydata) <- mydata[,1]
mydata <- mydata[,-c(1,6)]

#Normalize the data
mydata.norm <- sapply(mydata,scale)

#Set the Seed
set.seed(42)
# Perform k-means cluster analysis
fit.km <- kmeans(mydata.norm,10, nstart=25)
# Calculate number of observations in each cluster
fit.km$size
# calcualte cluster centroids
fit.km$centers
# Calculate sum of squared distances within each cluster
fit.km$withinss

#Is 10 the optimal number of clusters
set.seed(42)
#How many clusters are chosen by criteria
nc <- NbClust(mydata.norm, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Create a WSS plot
wssplot <- function(mydata.norm, nc=10, seed=42) {
  wss <- (nrow(mydata.norm)-1)*sum(apply(mydata.norm, 2, var)) 
  for (i in 2:10) {
    set.seed(42) 
    wss[i] <- sum(kmeans(mydata.norm, centers=i)$withinss)
  } 
  plot(1:10, wss, type="b", xlab="Number of clusters", ylab="within groups sum of squares")
}
wssplot(mydata.norm)

#Run k means with a cluster size of 6
set.seed(42)
fit.km2 <- kmeans(mydata.norm, 6, nstart=25)
# Calculate number of observations in each cluster
fit.km2$size
# calcualte cluster centroids
fit.km2$centers
# calculate within sum of square value for each cluster
fit.km2$withinss

#Compare k = 6 and k = 10 

#Find total withinss for k =1 
fit.km3 <- kmeans(mydata.norm, 1, nstart=25)

#Ratio of total withinss for k = 10 to k = 1
fit.km$tot.withinss/fit.km3$tot.withinss

#Ratio of total withinss for k = 6 to k =1
fit.km2$tot.withinss/fit.km3$tot.withinss

#Intra Cluster sum of squared distances for k = 10
fit.km$withinss
fit.km$tot.withinss

#Intra Cluster sum of squared distances for k = 6
fit.km2$withinss
fit.km2$tot.withinss

#Inter Cluster sum of squared distances for k = 10
fit.km$betweenss

#Inter Cluster sum of squared distances for k = 6
fit.km2$betweenss

#Visualize the clusters after running k means algorithm for k = 10
fviz_cluster(fit.km, data = mydata.norm, geom = "point",stand = FALSE, frame.type = "norm")

#Visualize the clusters after running k means algorithm for k = 6
fviz_cluster(fit.km2, data = mydata.norm, geom = "point",stand = FALSE, frame.type = "norm")

#End of Question 1--------------End of Question 1---------------End of Question 1----------------

#------------------Question 2-----------------------Question 2---------------------

rm(list=ls())
#Set the working directory
setwd("C:/Users/Aditya Murali/Desktop")

#Read data from file
mydata <- read.csv("Pharmaceuticals.csv", header = TRUE)

#Replace index of data set with company symbol
row.names(mydata) <- mydata[,1]

#Drop the non numeric columns
mydata <- mydata[,-c(1:2,12:14)]

#Normalize the data as variables represent different units and scales
mydata.norm <- sapply(mydata, scale)

#Set row names of original data for normalised data 
row.names(mydata.norm) <- row.names(mydata)

#Call NbClust Library
library(NbClust)

#Set the seed
set.seed(42)

#Use NbClust package to find optimum number of clusters
nc <- NbClust(mydata.norm, distance="euclidean", min.nc=2, max.nc=10, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

#Create the dendogram
d <- dist(mydata.norm)
fit.average <- hclust(d, method="average")
plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering")

#Cut the dendogram at the required level to get corresponding cluster size
clusters <- cutree(fit.average, k=3)
table(clusters)
aggregate(mydata.norm, by=list(cluster=clusters), median)
rect.hclust(fit.average, k=3)

#Try another type of clustering: K Means
#Set seed value = 42
set.seed(42)
#How many clusters are chosen by criteria of NbClust package
nc <- NbClust(mydata, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

#Set seed value = 42
set.seed(42)
# Perform k-means cluster analysis for k = 3 as it is the optimal cluster size
fit.km <- kmeans(mydata.norm, 3, nstart=25)
# Calculate number of observations in each cluster
fit.km$size
# calcualte cluster centroids
fit.km$centers
