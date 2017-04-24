# Student performance and school size hypothesis testing

library(foreign)

perfData <- read.dta("C:/Users/sherr/Desktop/R/meap93.dta")
head(perfData)
str(perfData)
summary(perfData)

perfreg <- lm(math10 ~ totcomp + 
                  staff + enroll,
                  data = perfData)
summary(perfreg)

# Hypothesis testing at 5% level
  # totcomp: statistically significant, reject H0 
  # staff and enroll: statistically insignificant, accept H0

# conclusion: totcomp is a strong predictor and has influence on student performance

######################################################################################

# More hypothesis testing 


wageD <- read.dta("C:/Users/sherr/Desktop/R/twoyear.dta")

head(wageD)
str(wageD)
summary(wageD)

#regression
Wagelm <- reg1 <- lm(lwage ~ jc + univ + exper,
                     data = wageD)
summary(Wagelm)

# p value < 0.05 
# All independant variables are statistically significant (reject null), but 
# jc and uni have more economical infleucen due to the larger coefficients 


