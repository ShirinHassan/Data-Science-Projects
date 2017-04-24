# Decision tree to predict infant birth weight 

library(MASS)
library(lattice) # for caret library
library(ggplot2) # for caret library
library(caret) # for training and testing the dataset 
library(tree)
library(rpart) # for decision tree - decided to go with tree function instead 


# view the data 
head(birthwt)

# distributions 
hist(birthwt$bwt)

# before we build the model, the categorical values need 
# to be converted to factors 

columns <- c('low', 'race', 'smoke', 'ht', 'ui')
birthwt[columns] <- lapply(birthwt[columns], as.factor)

# spliting the dataset into training set and a testing set 
set.seed(1)
Createpartition <- createDataPartition(birthwt$low, p = 0.75, list = FALSE, times = 1)
training1 <- birthwt[Createpartition,]
testing <- birthwt[-Createpartition,]
nrow(training) # 143
nrow(testing) # 46

# Building the tree 

#DecTree <- rpart(low ~ . - bwt, data = training1, method = 'class')
DecTree <- tree(low ~ . - bwt, data = training1, method = 'class')
summary(DecTree)

plot(DecTree)
text(DecTree, pretty = 0)

summary(DecTree)

# Examining the model's performance 

lowpredict <- predict(DecTree, testing, type = 'class')
table(lowpredict, testing$low)
# The model is 52% accurate
# Too many splits, the model needs to become prunned 

# Prunning 
PrunTree <- prune.tree(DecTree, best = 4)
plot(PrunTree)
text(PrunTree)
# Testing the performance accuracy 
lowpred <- predict(PrunTree, testing, type = 'class')
table(lowpred, testing$low)
(27+4)/(27+19)
# Accuracy has improved to 67%

#Trying another method for prunning 
# Prunning using cv.tree 
set.seed(2)
CVtree1 =cv.tree(DecTree ,FUN=prune.misclass )
names(CVtree1)
# "size" "dev" "k" "method"
CVtree1
# lowest corss validation rate: tree with 7 terminal nodes
prune.Dectree = prune.tree(DecTree, best=7)
plot(prune.Dectree)
text(prune.Dectree)
Predprune.Dectree <- predict(prune.Dectree, testing, type = 'class')
table(Predprune.Dectree, testing$low)
(31)/(14+32)
# 67% both methods give the same accuracy  

