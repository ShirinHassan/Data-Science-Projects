# import csv file 
RegressionData <- read.csv("C:/Users/sherr/Desktop/Files/03_02/regression-data.csv")

head(RegressionData)
str(RegressionData)

# plot data for correlations 
plot(RegressionData$BROADCAST,RegressionData$NET.SALES)
# visually we can see there is a positive correlation 

Lm1 <- lm(RegressionData$NET.SALES ~ RegressionData$BROADCAST)
#visualise 
lines(RegressionData$BROADCAST,Lm1$fitted)

# coefficients 
Lm1$coeff

# we can see a positive correlation, for each broadcast increase, sales increase by 12142 

# correlation coefficient 
cor(RegressionData$BROADCAST,RegressionData$NET.SALES)
# 95% very strong positive correlation 