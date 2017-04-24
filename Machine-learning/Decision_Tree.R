# Decision tree and random forest 

library(tree)
library(ISLR) # for Carseats data set 
attach(Carseats)

# Getting to know the data 

head(Carseats)
# We are specifically interested to view the mean for Sales 
summary(Carseats)
# Mean sales = 7.5

# convert sales from continuous to binary 
# Based on mean, Sales > 8 is high sales 
IsHigh = ifelse(Carseats$Sales >= 8, 'Yes', 'No')

# Merging IsHigh in the dataset 
Carseats=data.frame(Carseats,IsHigh)

head(Carseats) # we can see that IsHigh has been added to the data set 

# Tree function
CarseatsTree <- tree(IsHigh ~ . -Sales, data = Carseats, method = 'class' )
summary(CarseatsTree)
plot(CarseatsTree)
text(CarseatsTree, pretty = 0)
# biggest influence is location 

# Need to evaluate the performance of the model. Therefore: 
# Splitting the data into training and testing
set.seed(1)
Crtsplit <- createDataPartition(Carseats$IsHigh, p = 0.75, list = FALSE, times = 1)
Carseat.train <- Carseats[Crtsplit,]
Carseat.test <- Carseats[-Crtsplit,]
nrow(Carseat.train) # 300
nrow(Carseat.test) # 100

# prediction
CarseatsTree2 <- tree(IsHigh ~ . -Sales, Carseat.train, method = 'class' )
CarseatTree2Predict <- predict(CarseatsTree2, Carseat.test, type = 'class')
table(CarseatTree2Predict, Carseat.test$IsHigh)
(53+27)/(47+53)
# 80% - Not bad! 

plot(CarseatsTree2) 
text(CarseatsTree2)

# Prunning 
# Will use cv.tree() function to perform cross validation
set.seed(2)
CV.carseats =cv.tree(CarseatsTree2 ,FUN=prune.misclass )
names(CV.carseats )
# "size" "dev" "k" "method"
CV.carseats
# lowest corss validation rate: tree with 9 terminal nodes

par(mfrow=c(1,2))
plot(CV.carseats$size, CV.carseats$dev ,type="b")
plot(CV.carseats$k ,CV.carseats$dev ,type="b")

prune.carseats = prune.tree(CarseatsTree2, best=9)
plot(prune.carseats)
text(prune.carseats)
Predprune.carseats <- predict(prune.carseats, Carseat.test, type = 'class')
table(Predprune.carseats, Carseat.test$IsHigh)
(80)/(80+20)
# 80%

# Tried prunning with best 3 and best 5 and best = 9 gives the highest percentage 
# prune.carseats = prune.tree(CarseatsTree2, best=3)
# plot(prune.carseats)
# text(prune.carseats)
# Predprune.carseats <- predict(prune.carseats, Carseat.test, type = 'class')
# table(Predprune.carseats, Carseat.test$IsHigh)
# 74% 

rm(list = ls()) # clean up 


