# Fraud predictive modelling 

  library(ggplot2)
  library(lattice)
  library(caret) 
  library(gplots) 
  library(ROCR) # ROC Curve 
  library(rpart) # Decision tree 
  library(randomForest)


#############################################################################

# Data cleaning and visualisations 

FraudData <- read.csv("C:/Users/sherr/Desktop/R/creditcard.csv")
  head(FraudData)
  str(FraudData)
  summary(FraudData) # check for missing data 
  sum(is.na(FraudData$Class))# 0 
# Class variables represents fraud, 0: No 1: Yes 
# Explore fraud
  # convert to factor 
  FraudData$Class <- as.factor(FraudData$Class)
  class(FraudData$Class) # check 

#Visualise 
ggplot(data=FraudData, aes(x=Class)) +
  geom_bar(stat="count", width=0.5, fill="purple")

# Remove Time column - Not relevant to the analysis 
FraudDat <- subset(FraudData[,-1])
  str(FraudDat)

##################################################################################

# Logistic regression 

# Split data
set.seed(1)
Split <- createDataPartition(FraudDat$Class, p = 0.70, list = FALSE, times = 1)
train <- FraudDat[Split,]
test <- FraudDat[-Split,]
  nrow(train) 
  nrow(test)

# Logistic regression 
model <- glm(Class ~ ., data = train, family=binomial)
  summary(model)
  # Strong predictors: 
    # V4, V8, V10, V14, V20, V21, V22
    # Surprisingly, amount spent is not a very strong predictor on fraud 


# Evaluate the model 
predict1 <- predict.glm(model, newdata = test)
  # Confusion matrix
  table(test$Class, predict1 > 0.5)
  (85277+92)/(85277+92+17+55) # 99.9%

# ROC Curve 
ROC <- prediction(predict1, test$Class)
  ROCRper <- performance(ROC, 'tpr','fpr')
  plot(ROCRper, colorize = TRUE, text.adj = c(-0.2,1.7))
  abline(0, 1)
  
# AUC 
perf <- performance(ROC, measure = "auc")
perf # 98% 
# The curve and AUC show good accuracy 


################################################################################

# Decision Tree 

library(rattle)
library(rpart.plot)
library(RColorBrewer)
  
Tree <- rpart(Class ~ ., data = train, method = "class",
              minsplit = 2, minbucket = 1, cp=-1)


  text(Tree)
  summary(Tree)
  
# plot Tree 
  fancyRpartPlot(Tree)
  
# cross validate 
  printcp(Tree) 
  
# Prune
  mytree <- prune(Tree, cp=.05)
  fancyRpartPlot(mytree)
  # Main predictors are: V17, V14 and V12 
  
# Predict 
  Pred.Tree <- predict(mytree, data = test, type="class") 

# Evaluate using confusion matrix 
  confusionMatrix(Pred.Tree, test$Class) # 99%

##################################################################################

# Random Forest 

RF = randomForest(Class ~ .,  
                    ntree = 100,
                    data = train)
plot(RF)  
print(RF)
# Confusion matrix on the train data set 
(199005+270)/(16+75+199005+270) # 99.9%
# Out of Bag error rate: 0.05% - This shows good model accuracy 


# Variable Importance - Top 10 
varImpPlot(RF,  
           sort = T,
           n.var=10)
          # Top var is V17, 

# Variable Importance
var.imp = data.frame(importance(RF, type=2))
  Variables = row.names(var.imp)  
  print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])
  
# Top 10 predictors in order (Mean decrease Gini)
#  V17: 163.169666  V12: 78.136809  V10: 61.521629  
#  V14: 61.253612   V16: 47.574183  V11: 44.340345  
#  V9 22.906760     V14: 17.793561  V7: 15.936090  V18: 15.327914 
  
# Predict on test data set 
PredRF <- predict(RF, test)
confusionMatrix(PredRF, test$Class) # 99.9 % 


# In progress 



