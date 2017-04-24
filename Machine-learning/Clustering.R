# Getting to know the data 
?mtcars
data(mtcars)
View(mtcars)
str(mtcars) # 11 variables 


mtcars1 <- mtcars[, c(1:4, 6:7, 9:11)] # variables selection
str(mtcars1) # 9 variables 

# hierarchical clustering 
# dissmilarity matrix 
dissm <- dist(mtcars1)
dissm

cluster1 <- hclust(dissm)
#visualisation 
plot(cluster1)

# Grouping observations 
# K = groups 
?cutree
Group3 <- cutree(cluster1, k = 3)
Group3

?rect.hclust
# draws rectangles around the 3 clusters 
rect.hclust(cluster1, k = 3, border = 'purple')
# Trying with 4 groups
rect.hclust(cluster1, k = 4, border = 'red')
# Trying with 5 groups
rect.hclust(cluster1, k = 5, border = 'dark green')

# k means clustering method
km1 <- kmeans(mtcars1, 3)
km1

library(cluster)
?clusplot
clusplot(mtcars1,
          km1$cluster, # k mean cluster for cluster data
          color = TRUE, 
          lines = 3, 
          labels = 3 )

rm(list = ls()) # clean up 
