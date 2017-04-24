# Linear regression  
# Is there a relationship between paritipating in the 401K pension plan
# and the generosity of the plan?

library(foreign) # to read .dta file 
PensionData <- read.dta("C:/Users/sherr/Desktop/R/401k.dta")
# Datasource: https://ideas.repec.org/p/boc/bocins/401k.html


# getting to know the data
head(PensionData)
summary(PensionData)
  # prate = % of eligible workers on the plan - y 
  # mrate = plan's generosity measure - x


# Shape of the data 
hist(PensionData$prate,col="blue1", main = "% of workers on the plan") 
  # skewed to the right, makes sense since mean prate = 87% 
hist(PensionData$mrate,col="purple", main = "Pension plan's generosity")
  # skewed to the left 
  

# Linear regression model 
Pensionreg <- lm(PensionData$prate ~ PensionData$mrate)
              summary(Pensionreg)
              # p-value < 0.05 means mrate is statistically significant 
              # R-squared is quite low, the model is therefore not very good 
              # Perhaps we should try adding more x variables to the model to improve the model 
    
# predict when mrate = 3.5 
predict(Pensionreg, data.frame(mrate="3.5"))

# The result doesnt look right so i'll manually perform the prediction
# prate = 83.07 + 5.86 (3.5) = 103.58 
# Given that prate is in percentage, we can't have more than 100% 
# This model of prediction is therefore not good for our analysis 
  
