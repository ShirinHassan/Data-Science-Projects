# Regression Decision tree and random forest 

library(MASS)
library(tree)
library(lattice)
library(ggplot2)
library(caret) 
library(randomForest)

head(Boston)
?Boston
summary(Boston)# The dependant variable is medv which is median value of owner-occuped homes in  \$1000s.

# Spliting the dataset into training set and a testing set 
set.seed(1)
Split1 <- createDataPartition(Boston$medv, p = 0.75, list = FALSE, times = 1)
train <- Boston[Split1,]
test <- Boston[-Split1,]
nrow(train) 
nrow(test) 

# Tree 
tree.Boston = tree(medv~., train, method = 'anova')
summary(tree.Boston)
plot(tree.Boston)
text(tree.Boston)

# Prunning 
cv.Boston = cv.tree(tree.Boston)
plot(cv.Boston$size ,cv.Boston$dev ,type='b')
names(cv.Boston)
cv.Boston # best of 9 

prune.Boston=prune.tree(tree.Boston, best = 9)
plot(prune.Boston)
text(prune.Boston, pretty = 0)

# prediction using the test data
yhat=predict (tree.Boston ,test)
plot(yhat, test$medv)
abline (0,1)
mean((yhat -test$medv)^2)
# 22.05

# RandomForest 
set.seed(1)
bag.Boston= randomForest(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax +
                         ptratio + black + lstat , 
                         data=train ,
                         ntree=500,
                         importance =TRUE)
bag.Boston

# How well will the bag model perform on the test set?
yhat=predict (bag.Boston ,test)
plot(yhat, test$medv)
abline (0,1)
mean((yhat -test$medv)^2)
# 8.99 much less than the single tree model 

rm(list = ls()) # clean up 



