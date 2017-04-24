# This analysis looks at Fertility rate and 
# its relationship to other socioeconomic indicators (Swiss, 1888)
# The purpose of this analysis is to find out what are the biggest socioeconomic influences on Fertility rate  


install.packages("Hmisc")
library("Hmisc")
# Data 
data(swiss)
?swiss
View(swiss)
str(swiss) # 6 variables 47 observations

# correlation for the 6 variables 
round(cor(swiss), 2)
# Picking the variable with the highest correlation
# Negative correlation of -0.66 between Fertility and Education 

# Hypothesis test 
# H0 = Education has no correlation with Fertility rate 
# H1 = Education and Fertility rate are correlated 
cor.test(swiss$Fertility, swiss$Education) 
# we accept H1 - correlation != 0


#to get a view 
plot(swiss$Fertility, swiss$Education)


# regression 
pairs(swiss)
round(cor(swiss), 2)
# visualisation and correlation show that variables such 
# as Examination and Education are not independant from each other
# hence multiple regression model wont be conducted with all of the variables
# to prevent collinearity  

# to check for significant predictors 
regg <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
           data = swiss)
summary(regg)

# The most significant predictor is Education 
# We conduct a simple linear regression model as other variables -  are not independant from each other 
# Except for Infant.Mortality and Catholic


regg1 <- lm(Fertility ~ Education,
           data = swiss)
summary(regg1)

# We could use infant.Mortality and Catholic as predictors - 
# Infant.Mortality has a weak correlation with Fertility r < 0.50 - we wont use this as predictor 
# But rounding up correlation coefficient is close to 0.50 for Catholic

regg2 <- lm(Fertility ~ Education + Catholic,
            data = swiss)
summary(regg2)

# Example prediction with multiple regression model
predict(regg2, data.frame("Education"=5, "Catholic"=20))
# Fertility rate is 72.5 

#Example prediction with simple regression model 
predict(regg1, data.frame("Education"=5))
# Fertility rate = 75

predict(regg1, data.frame("Education"=70))
# Fertility rate 19 




rm(list = ls()) # clean up 

