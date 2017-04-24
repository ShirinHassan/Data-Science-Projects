# HR Analytics 
# Why are people leaving the company?

library(corrplot)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

# Data exploration

HRData <- read.csv("C:/Users/sherr/Desktop/HR.csv")

head(HRData)

summary(HRData)

# satisfaction level is around 62% (mean), employees work on around 3-4 projects, around 200 hours a month
# sales department has the highest number of employees 
# salary is clustered by low (count = 7316), medium(count = 6446) and high( count = 1237)

# How many people have left? 
length1 <- length(which(HRData$left == 1)) # 3571 employees have left 
length2 <- length(which(HRData$left == 0)) 
length2 
lengthtotal <- length1 + length2 # 14999 employees total 
lengthtotal
(length1/lengthtotal)*100 # 24% of employees have left 

# convert categorical variables to factors 
col1 <- c('sales', 'salary')
HRData[col1] <- lapply(HRData[col1], as.factor)

# Lets find the correlations 
cor(HRData[,1:8])
corrplot(cor(HRData[,1:8]))

# We can see a negative correlation between satisfaction level and leaving the company (-0.4)
# This shows that the reason employees leave is because they are not satisfied (as opposed to other reasons such as relocation)

# The next question is therefore: what causes employee dissatisfaction? 
# There is a negative correlation between time spend and satisfaction level - The more time employees spend the more likely they are to leave
# Also experienced workforce are leaving
# Lack of promotion is also another factor for leaving

# visualisation of data 

# filter to see the histogram of people who have left 
hist_hr <- HRData[HRData$left == '1',]
hist(hist_hr$satisfaction_level,col="blue1", main = "Satisfaction level") 
hist(hist_hr$last_evaluation,col="coral3", main = "Last evaluation")
hist(hist_hr$average_montly_hours,col="purple", main = "Average montly hours")

plot(hist_hr$salary,col="red", main = "Salary")
# Majority of people who left were on low salary followed by medium salary 


# We can see that people with low last evaluation and average monthly hours have left 
# However, we can also see that people with high last evaluation and average monthly hours have left 
# Showing that good workforce are also leaving and not satisfied 


# Logistic Regression modelling - simple models to test 
model <- glm(HRData$left~HRData$satisfaction_level, binomial)
summary(model)
# We reject the null hypothesis - that the satisfaction level has no impact on employees leaving 

model1 <- glm(HRData$left~HRData$average_montly_hours, binomial)
summary(model1)
# We reject the null hypothesis - that the average monthly hours has no impact on employees leaving 


# Predictive modeling 


# Training the data 
set.seed(1234)
splitIndex <- createDataPartition(HRData$left, p = .80,list = FALSE, times = 1)
trainData <- HRData[ splitIndex,]
testData <- HRData[-splitIndex,]
print(table(trainData$left))

# Logistic regression model 
TrainContrl <- trainControl(method = "cv", number = 5)
glm1 <- train(as.factor(left) ~. , data = trainData, method = "glm", trControl = TrainContrl)
summary(glm1)

# predict 
predict1 <- predict(glm1, testData)
summary(predict1)

# summary(predict1)
# 0    1 
# 2617  382 
