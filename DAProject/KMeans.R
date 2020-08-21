# Get working directory
getwd()

#import data for analysis
#store in a data frame called Pharmdat
student <-read.csv("student.csv",header=TRUE,sep=",")

#View the first 6 lines of the data frame
#use head() function
head(student)

#Get subset of data (i.e. numeric fields) to use in cluster analysis
#store in another data frame called Pharmdat1
student1 <- student[c(-1,-2,-4,-5,-9:-12,-16:-23)]
student2 <- student[c(-1,-2,-4,-5,-9:-12,-16:-23)]

#View the first 6 lines of the data frame
#use head() function
head(student1)

#Determine how many clusters to use using the Elbow method (scree plot)
#max number of clusters=15
k.max =15
#use sapply() function to run a loop
#Save the wss for each iteration
wss <- sapply(1:k.max, function(k){kmeans(student1, k, iter.max =25)$tot.withinss})

head(wss)

#plot the results
#use plot() function
plot(1:k.max,wss, type="b",xlab="Number of clusters", ylab="Total withinss sum of squares")


#overall assessment using 30 indices to determine optimal number of clusters
#install NBClust() and Factoextra packages
kpgs <-c("factoextra", "NbClust")
install.packages(kpgs)

#load the packages in order to use them
#use library() function
library(factoextra)
library(NbClust)

#use NbClust() package to get cluster solutions for the 30 indices
#guarantee the same starting using set.seed() method
set.seed(10)
resultBNC <-NbClust(student1,distance="euclidean",min.nc=2,max.nc=10,method="kmeans",index="all")

#plot the the frequency histogram
#use fviz_NbClust() function
fviz_nbclust(resultBNC)

#Obtain final cluster solution using 3 clusters
set.seed(10)
#K-means clustering output will stored in the kmResult variable
kmResult <-kmeans(student1,3)

#Show results components
#use attributes() function
attributes(kmResult)

#save the clusters in the dataset
student1$ClustNum <-as.factor(kmResult$cluster)

#view the first 6 lines of the data
head(student1)

#get cluster means using aggregate function
aggregate(student2,by=list(kmResult$cluster),FUN=mean)

#Show the cluster distribution table
table(kmResult$cluster)

#visualize the the cluster solution
#use fviz_cluster() to generate a plot
fviz_cluster(kmResult, student2, ellipse.type = "convex")

#can also plot cluster solution using cluste() package by means
# of Principal Components
#load cluster() package
library(cluster)
#use clusplot() function in the cluster() package
clusplot(student2, kmResult$cluster, main = "2D Re-presentatrion of Cluster Soltuion", color=TRUE,shade=TRUE, labels =2,lines=0)  


#Validate the solution using Silhouette coefficient
#save/store output in a variable called silOut
silOut <- silhouette(kmResult$cluster, dist(student2))

#plot the silOut values
plot(silOut, main="Silhouette Plot - Kmeans Cluster Solution")
























