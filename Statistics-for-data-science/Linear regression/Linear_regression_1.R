# Is there a relationship between salary and number of years as CEO?

CeoData <- read.dta("C:/Users/sherr/Desktop/R/CEOSAL2.dta")

head(CeoData)
summary(CeoData)

# salary - y variable 
# ceoten, number of years as company CEO - x variable 

countCEO <- length(which(CeoData$ceoten==0))
countCEO
# 5 CEOs with 0 years of experience as CEO 
max(CeoData$ceoten)
# maximum number as CEO is 37 

cor.test(CeoData$salary, CeoData$ceoten)
CEOreg <- lm(log(CeoData$salary) ~ CeoData$ceoten)
             summary(CEOreg)
             # for every year as CEO, salary increases by 1% 
             
# Overall conclusion: as years of experience increases, salary also increases (r squared = 14%)
# However, years of experience is not statistically significant, meaning its not a strong predictor for salary 