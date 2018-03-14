library(stats)
library(cluster)
 library(NbClust)
library(fpc)
library(MASS)
table<-UScrime		# This is to load UScrime dataset from MASS library


########Determine number of clusters best suitting for ur dataset 
##(either thtat way or do hierarhical first and determine from the tree)
set.seed(1234)
nc <- NbClust(UScrime, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")


###########Hierarhical Clustering
clusters <- hclust(dist(table))
plot(clusters)		#See what is the best n of clusters indicated
clusterCut <- cutree(clusters, 3)

##########Clustering k means
fit <- kmeans(table, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph

clusplot(table, fit$cluster, color=TRUE, shade=TRUE, 
  	labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(table, fit$cluster)


#PCA

pca<-prcomp(table)
View(pca)
biplot(pca, scale = 0)
#compute standard deviation of each principal component
std_dev <- pca$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:16]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:16]

#scree plot
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")
			 
			 #cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")
			  
			  
#second way for PCA
# PCA with function PCA
library(FactoMineR)

# apply PCA
pca3 = PCA(table, graph = FALSE)

# matrix with eigenvalues
pca3$eig