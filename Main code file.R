
  
  ```{r}
#load relevant libraries
library(dplyr)
library(ggplot2)

#Import data into a new data frame
MasterData<-read.csv("Metacognitive Skills - Clean.csv")

#Shorten column names
colnames(MasterData)<-c("ID","Sex","Background","CA", "CU", "IA", "IU")
```

```{r}
#Examine scatter plots for different variable combination pairs
#Linguistic Background & Correct, Aware
ggplot(MasterData, aes(Background,CA))+ geom_point() + stat_sum(aes(group = 1))

#Linguistic Background & Incorrect, Aware
ggplot(MasterData, aes(Background,IA))+ geom_point() + stat_sum(aes(group = 1))

#Linguistic Background & Incorrect, Unaware
ggplot(MasterData, aes(Background,IU))+ geom_point() + stat_sum(aes(group = 1))
```

```{r}
#Create new data frame for scaled data without non-numeric variables
ScaledData<-as.data.frame(select(MasterData, - ID, - Sex, - Background))

#Calculate Mean and Standard Deviation for Scaled Data
sapply(ScaledData, mean)
sapply(ScaledData, sd)

#Creating the Within Group Sum of Squares function
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#Create the WSS Graph (note that the number of clusters tested (nc) must be less that the number of rows b/c we can't have more clusters than we have observations)
wssplot(ScaledData,nc=15,seed=1234)
```
```{r}
#Create and examine several different possible cluster solutions
twoclusterkmeans<-kmeans(ScaledData, 2, nstart=10)
twoclusterkmeans
threeclusterkmeans<-kmeans(ScaledData, 3, nstart=10)
threeclusterkmeans
fourclusterkmeans<-kmeans(ScaledData, 4, nstart=10)
fourclusterkmeans
```
```{r}
#Assign the clusters for each observation for k=2,3,4 to a new dataframe
Clusters<-data.frame(MasterData, twoclusterkmeans$cluster, threeclusterkmeans$cluster, fourclusterkmeans$cluster)
```

```{r}
#Graph the different solutions - Three Clusters.
ggplot(Clusters, aes(Background,CA, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,CU, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,IA, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,IU, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,CU, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,IA, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,IU, color = factor(threeclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))
```

```{r}
#Graph the different solutions - Four Clusters.
ggplot(Clusters, aes(Background,CA, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,CU, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,IA, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(Background,IU, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,CU, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,IA, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))

ggplot(Clusters, aes(CA,IU, color = factor(fourclusterkmeans.cluster))) + geom_point() + stat_sum(aes(group = 1))
```
