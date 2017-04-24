# Analysis of effects of smoking and income on infant birth weight 
# Data source: http://fmwww.bc.edu/ec-p/data/wooldridge/bwght.dta

library(foreign)
library(corrplot)

infntData <- read.dta("C:/Users/sherr/Desktop/R/bwght.dta")
head(infntData)
str(infntData)
summary(infntData)

# faminc - family income 
# We want to see if our data captures a range of incomes
hist(infntData$faminc, 
     col = "purple",
     main = "Family income",
     xlab = "Income")
  # we have data from a range of incomes (rather than focusing on a specific income group)
  # min income = 0.5 max = 65, shows a wide range
boxplot(infntData$faminc,
        main= "Boxplot of income",
        col = "blue")


# Can smoking and family income be correlated? 

infntData[,c(1:4,10)]
  cor(infntData[,c(1:4,10)])
  corrplot(cor(infntData[,c(1:4,10)]))

# Family income and cigarettes are negatively correlated (-17%)
  # This is interesting as higher income group can afford more cigs
# Not a strong correlation, we can ignore Multicollinearity
# Negative correlation between birthweight and cigs (-15%) - as expected 

# regression model 
birthwghreg <- lm(bwght ~ faminc + cigs,
                  data = infntData)
summary(birthwghreg)
# without faminc 
birthwghreg1 <- lm(bwght ~ cigs,
                  data = infntData)
summary(birthwghreg1)
# by omitting faminc, effect of cigs is stronger for our model 


# Overall conclusion: 
# Increase in family income leads to increase in birthweight 
  # One could argue that this is because people with high income have access to better care
  # However, people with higher income also smoke less (negative correlation between smoking and income)
# Smoking is a stronger predictor for infant birth weight 
  # Increase in smoking leads to lower infant birth weight 

# explorting 
write.csv(infntData, file = "birthweightData.csv")
