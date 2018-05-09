########### assignment 2

#### problem 1 eurpean data

europe <- read.table("europeanJobs.txt", header = T)

colnames(europe)<-c('Country','Agr','Min','Man','PS','Con','SI','Fin','SPS','TC')
#View(europe)

str(europe)
summary(europe[2:10])

library(ggplot2)

ggplot(europe) + 
  geom_boxplot(aes(x='Agr', y = Agr))+
  geom_boxplot(aes(x='Min', y = Min))+
  geom_boxplot(aes(x='Man', y = Man))+
geom_boxplot(aes(x='PS', y = PS))+
geom_boxplot(aes(x='Con', y = Con))+
geom_boxplot(aes(x='SI', y = SI))+
geom_boxplot(aes(x='Fin', y = Fin))+
geom_boxplot(aes(x='SPS', y = SPS))+
  geom_boxplot(aes(x='TC', y = TC))+
  ggtitle("European employment by sector")+
  xlab("Sector") + ylab("Percentage employed")

boxplot(europe[,c(2:10)])

# splitting the data into train-test
set.seed(123)
index <- sample(nrow(europe),nrow(europe)*0.90)
europe.train <- europe[index,]
europe.test <- europe[-index,] 



#BOT SURE IF WE NEED TO SCALE THE DATA:Scale data to have zero mean and unit variance for each column
#seed <- scale(seed)


####### K means
#Use k-means method for clustering and plot results.

# K-Means Cluster Analysis
fit2 <- kmeans(europe[2:10], 2)
fit3 <- kmeans(europe[2:10], 3) #3 cluster solution
fit4 <- kmeans(europe[2:10], centers = 4)  # just to show that we can select a differnet number of clusters
fit5 <- kmeans(europe[2:10], centers = 5)


#Display number of observations in each cluster
table(fit2$cluster)
table(fit3$cluster)
table(fit4$cluster)
table(fit5$cluster)

## to see what cluster does each observation obelong to...
fit2$cluster
fit3$cluster
fit4$cluster
fit5$cluster

#Plot cluster in kmeans
#install.packages("fpc")
library(fpc)
plotcluster(europe[2:10], fit2$cluster)  # the coordinates x and y stand for dc 1 and dc 2 which is basically a way of showing the data in 2 dimensioonss...but It's very hard to understand them
plotcluster(europe[2:10], fit3$cluster) 
plotcluster(europe[2:10], fit4$cluster) 
plotcluster(europe[2:10], fit5$cluster) 


#See exactly which item are in 1st group
europe[fit5$cluster==1,]  # so all of these observations belong to the first cluster


#get cluster means for scaled data
aggregate(europe[2:10],by=list(fit5$cluster),FUN=mean)



################# determine number of clusters

wss <- (nrow(europe)-1)*sum(apply(europe[2:10],2,var))
for (i in 2:12) wss[i] <- sum(kmeans(europe[2:10],
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")  # we study from 1 up to 12 clusters

##the best thing to select the optimal point is finding the elbow of the graph, in this case for example 3. even if more clusters reduce distance, they add some complexity


# start by selecting k  = 3
fit3 <- kmeans(europe[2:10], 3)

table(fit3$cluster)

fit3$cluster

plotcluster(europe[2:10], fit3$cluster) 

#See exactly which item are in 1st group
europe[fit3$cluster==1,]  # so all of these observations belong to the first cluster
europe[fit3$cluster==2,]
europe[fit3$cluster==3,]

# countries in that group
europe[fit3$cluster==1,1]  # so all of these observations belong to the first cluster
europe[fit3$cluster==2,1]
europe[fit3$cluster==3,1]

#get cluster means for scaled data
aggregate(europe[2:10],by=list(fit3$cluster),FUN=mean)

########## do 2 clusters to see difference of coastal vs central european countries

# start by selecting k  = 2
fit3 <- kmeans(europe[2:10], 2)

table(fit2$cluster)

fit2$cluster

plotcluster(europe[2:10], fit2$cluster) 

#See exactly which item are in 1st group
europe[fit2$cluster==1,]  # so all of these observations belong to the first cluster
europe[fit2$cluster==2,]
europe[fit2$cluster==3,]

# countries in that group
europe[fit2$cluster==1,1]  # so all of these observations belong to the first cluster
europe[fit2$cluster==2,1]
europe[fit2$cluster==3,1]

#get cluster means for scaled data
aggregate(europe[2:10],by=list(fit2$cluster),FUN=mean)



############################
# hierarchical clustering


#Wards Method or Hierarchical clustering
#Calculate the distance matrix
europe.dist=dist(europe[2:10])
#Obtain clusters using the Wards method
europe.hclust=hclust(europe.dist, method="ward")

# plot dendogram
plot(europe.hclust)

#Cut dendrogram at the 3 clusters level and obtain cluster membership
europe.3clust = cutree(europe.hclust,k=3)
europe[europe.3clust==1,]
europe[europe.3clust==2,]
europe[europe.3clust==3,]

#get cluster means for raw data
#Centroid Plot against 1st 2 discriminant functions
#Load the fpc library needed for plotcluster function
library(fpc)
#plotcluster(ZooFood, fit$cluster)
plotcluster(europe[2:10], europe.3clust)


europe.4clust = cutree(europe.hclust,k=4)
europe[europe.4clust==1,]
europe[europe.4clust==2,]
europe[europe.4clust==3,]
europe[europe.4clust==4,]

europe[europe.4clust==1,1]
europe[europe.4clust==2,1]
europe[europe.4clust==3,1]
europe[europe.4clust==4,1]

aggregate(europe[2:10],by=list(europe.4clust),FUN=mean)



############################# problem 2 

## zoo data
# part a) clustering stage

library(readxl)
zoo <- read_excel("qry_Food_by_Month.xls")
View(zoo)
str(zoo)

################# determine number of clusters

wss <- (nrow(zoo)-1)*sum(apply(zoo[2:7],2,var))
for (i in 2:12) wss[i] <- sum(kmeans(zoo[2:7],
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")  # we study from 1 up to 12 clusters

##the best thing to select the optimal point is finding the elbow of the graph, in this case for example 3. even if more clusters reduce distance, they add some complexity

##### 2 clusters


fit2 <- kmeans(zoo[2:7], 2)

table(fit2$cluster)

fit2$cluster

plotcluster(zoo[2:7], fit2$cluster) 

#See exactly which item are in 1st group
zoo[fit2$cluster==1,]  # so all of these observations belong to the first cluster
zoo[fit2$cluster==2,]


# countries in that group
zoo[fit2$cluster==1,1]  # so all of these observations belong to the first cluster
zoo[fit2$cluster==2,1]


#get cluster means for scaled data
aggregate(zoo[2:7],by=list(fit2$cluster),FUN=mean)








## 3 clusters

# start by selecting k  = 3
fit3 <- kmeans(zoo[2:7], 3)

table(fit3$cluster)

fit3$cluster

plotcluster(zoo[2:7], fit3$cluster) 

#See exactly which item are in 1st group
zoo[fit3$cluster==1,]  # so all of these observations belong to the first cluster
zoo[fit3$cluster==2,]
zoo[fit3$cluster==3,]

# countries in that group
zoo[fit3$cluster==1,1]  # so all of these observations belong to the first cluster
zoo[fit3$cluster==2,1]
zoo[fit3$cluster==3,1]

#get cluster means for scaled data
aggregate(zoo[2:7],by=list(fit3$cluster),FUN=mean)



#Think about doing 4 cluster? don't think it's worthy it...
################################################

############## part b ) Association rules
#install.packages('arules')
library(arules)
library(readxl)
food <- read_excel("food_4_association.xls")
head(food)
food <- food[,-1]
food <- as(as.matrix(food), "transactions")


# most frequent items
itemFrequencyPlot(food, support = 0.1, cex.names=0.8)

########## apriori algorithm

# Run the apriori algorithm. In this case minimm support of 3% and 50% for confidence
food_rules <- apriori(food,parameter = list(sup = 0.003, conf = 0.5,target="rules"))
summary(food_rules)
### read the lab notes to get more information on this...


# Check the generated rules using inspect
inspect(head(food_rules))  # realize that this is just the head of the data
inspect(food_rules)

#Basket rules of size greater than 4
inspect(subset(food_rules, size(food_rules)>3))

## basket rules with lift >5
inspect(subset(food_rules, lift>10))


inspect(subset(food_rules, lift>20))
inspect(subset(food_rules, confidence>0.9))
inspect(subset(food_rules, support>0.01))
#Now find the subset rules that has Yogurt in the right hand side. Here we require lift measure exceeds 3.5
#yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt" & lift>3.5)
#Now inspect the subset rules
#inspect(yogurt.rhs)


#Now find the subset rules that has burger in the left hand side.
burger.lhs <- subset(food_rules, subset = lhs %in% "BurgerFood" & lift>1.5)
#Now inspect the subset rules
inspect(burger.lhs)



######## visualize the rules
#We can use the arulesViz package to visualize the rules, for a more complete introduction see (http://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf).

#install.packages('arulesViz')
library('arulesViz')
plot(food_rules)


#The plot function has an interactive mode for you to inspect individual rules:
plot(food_rules, interactive=TRUE)

#Graph-based visualization can be used for very small sets of rules. The vertices are represented by items for the 10 rules with highest lift:
plot(head(sort(food_rules, by="lift"), 7), method = "graph")

plot(food_rules, method="grouped")



###################################