library(ggplot2) 
library(psych)
library(caret) 
library(lattice) 
library(klaR)
library(car) 

# Analysing Payroll Data 

PayrollData <- read.csv("C:/Users/sherr/Desktop/R/data.csv")

head(PayrollData)
summary(PayrollData)
str(PayrollData)
# 285008 obs 35 variables 
summary(PayrollData$Year)
# Years are between 2013-2016

###########################################################################

# Data cleaning 

# Remove $ and convert to numeric 
PayrollData$Projected.Annual.Salary = as.numeric(gsub("\\$", "", PayrollData$Projected.Annual.Salary))
# check result 
class(PayrollData$Projected.Annual.Salary)
# Repeat with the rest of the columns
        PayrollData$Q1.Payments = as.numeric(gsub("\\$", "", PayrollData$Q1.Payments))
        PayrollData$Q2.Payments = as.numeric(gsub("\\$", "", PayrollData$Q2.Payments))
        PayrollData$Q3.Payments = as.numeric(gsub("\\$", "", PayrollData$Q3.Payments))
        PayrollData$Q4.Payments = as.numeric(gsub("\\$", "", PayrollData$Q4.Payments))
        PayrollData$Payments.Over.Base.Pay = as.numeric(gsub("\\$", "", PayrollData$Payments.Over.Base.Pay)) 
        PayrollData$Total.Payments = as.numeric(gsub("\\$", "", PayrollData$Total.Payments))
        PayrollData$Base.Pay = as.numeric(gsub("\\$", "", PayrollData$Base.Pay))
        PayrollData$Permanent.Bonus.Pay = as.numeric(gsub("\\$", "", PayrollData$Permanent.Bonus.Pay))
        PayrollData$Longevity.Bonus.Pay = as.numeric(gsub("\\$", "", PayrollData$Longevity.Bonus.Pay))
        PayrollData$Temporary.Bonus.Pay = as.numeric(gsub("\\$", "", PayrollData$Temporary.Bonus.Pay))
        PayrollData$Lump.Sum.Pay = as.numeric(gsub("\\$", "", PayrollData$Lump.Sum.Pay))
        PayrollData$Overtime.Pay = as.numeric(gsub("\\$", "", PayrollData$Overtime.Pay))
        PayrollData$Other.Pay...Adjustments = as.numeric(gsub("\\$", "", PayrollData$Other.Pay...Adjustments))
        PayrollData$Other.Pay..Payroll.Explorer. = as.numeric(gsub("\\$", "", PayrollData$Other.Pay..Payroll.Explorer.))
        PayrollData$Average.Health.Cost = as.numeric(gsub("\\$", "", PayrollData$Average.Health.Cost))
        PayrollData$Average.Dental.Cost = as.numeric(gsub("\\$", "", PayrollData$Average.Dental.Cost))
        PayrollData$Average.Basic.Life = as.numeric(gsub("\\$", "", PayrollData$Average.Basic.Life))
        PayrollData$Average.Benefit.Cost = as.numeric(gsub("\\$", "", PayrollData$Average.Benefit.Cost))

###########################################################################

# Lets explore         
summary(PayrollData$Projected.Annual.Salary)
        # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        #   0   56730   78950   79990   99810  360700 
        # Median and mean salary are close and around $80,000, 
        # they show data is well distributed 

# Visualise  
qplot(PayrollData$Projected.Annual.Salary, 
      fill=..count.., 
      geom="histogram", 
      xlab = "Salary", 
      main = "Histogram for Salary") 


# Boxplot
plot <- ggplot(PayrollData, aes(Department.Title, Projected.Annual.Salary/1000)) 
plot +  geom_boxplot(fill='white', color="darkblue") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Department") + ylab("Annual Salary" ) 

# Observations from the plot: 
  # City attorney, City administrative office and Los Angeles Dept of Tourism are the top paying companies 
  # City clerk, Employee relations Board and Public Work have the lowest salaries
  # Many outliers, but mostly on the positive side 
    # The highest salary outlier belongs to Airports LAWA (a couple of people are really at the top!)


###########################################################################

# Hypothesis testing 
# H0 = Mean salaries in 2015 and 2016 are equal
# H1 = 2015 mean salary is higher than 2016

# Creating two samples 
Payroll2016 <- PayrollData[ which(PayrollData$Year == 2016),]
  str(Payroll2016)
Payroll2015 <- PayrollData[ which(PayrollData$Year == 2015),]
  str(Payroll2015)

# Fisher's f-test to compare viariances 
var.test(Payroll2015$Projected.Annual.Salary, Payroll2016$Projected.Annual.Salary)
  # p valuea < 0.05 we can assume that the two variances are not homogeneous (reject H0)

# t-test
t.test(Payroll2015$Projected.Annual.Salary, 
       Payroll2016$Projected.Annual.Salary,
       paired = FALSE, var.equal = FALSE)
       qt(0.95, 119580)
       # P value < 0.05 - reject null 


# Conclusion: 2016 and 2015 annual salaries are different 
            # 2016 annual salary is greater than 2015 annual salary 
       

###########################################################################

# Machine learning: Building a model to predict average health cost

# Regression model

# Look at correlation to 
      # 1. eliminate variables which are not effective
      # 2. x variables which are highly correlated 
cor(PayrollData[,c(9:13, 30:33)])
# visualise the correlation
cor.ci(PayrollData[,c(9:13, 30:33)], method="spearman" )

# Double-Check for missing values 
count(is.na(PayrollData$Average.Health.Cost))# 0 
count(is.na(PayrollData$Projected.Annual.Salary)) # 0
count(is.na(PayrollData$Average.Basic.Life)) # 0 
count(is.na(PayrollData$Average.Dental.Cost)) # 0 
count(which(PayrollData$Average.Health.Cost == 0)) 

# Create subset 
Payroll.subset <- subset(PayrollData,  select=c("Average.Health.Cost",
                         "Average.Basic.Life",  "Projected.Annual.Salary", 
                         "Average.Dental.Cost", "Q1.Payments","Q2.Payments",
                         "Q3.Payments","Q4.Payments")) 
# Notes: 
# Q3 and Q4 are strongly correlated will eliminate one once the model is tested
summary(Payroll.subset)
str(Payroll.subset)


# split into training and testing sets 
set.seed(1)
Split1 <- createDataPartition(Payroll.subset$Average.Health.Cost, 
                              p = 0.75, list = FALSE, times = 1)
train <- Payroll.subset[Split1,]
test <- Payroll.subset[-Split1,]
nrow(train) # 213757
nrow(test) # 71251

# build the regression model on the training sample
reg <- lm(Average.Health.Cost~Average.Basic.Life +
          Projected.Annual.Salary + Average.Dental.Cost +
          Q1.Payments + Q2.Payments + Q3.Payments + Q4.Payments,
          data = train)
summary(reg) # R-squared explains 84% of variations - this is good


# component residual plot to check for linearity 
crPlots(reg) 

# Eliminate outliers to imrove the model 
# Identify outliers
cutoff <- 4/(nrow(train)-length(reg$coefficients)-2)
plot(reg, which=4, cook.levels=cutoff)
plot(reg, which=5, cook.levels=cutoff)
nrow(train)
# Eliminate 
train <- train[-which(rownames(train)
            %in% c("116763","124552","56574")),]
# Check 
nrow(train)
# 213754
summary(reg)
crPlots(reg)

# identify multicolinearity 
vif(reg)
# Remove Q3 and Q4 from the model 
reg <- lm(Average.Health.Cost~Average.Basic.Life +
          Projected.Annual.Salary + Average.Dental.Cost +
          Q1.Payments + Q2.Payments,
          data = train)
summary(reg)
vif(reg) # check 


# Prediction model: 
Train.predict <- predict(reg, newdata= subset(train, select=c("Average.Health.Cost","Average.Basic.Life"
                                                              "Projected.Annual.Salary", "Average.Dental.Cost"
                                                              "Q1.Payments", "Q2.Payments"))
                         
Test.predict < predict(reg, newdata= subset(test, select=c("Average.Health.Cost","Average.Basic.Life"
                                                            "Projected.Annual.Salary", "Average.Dental.Cost"
                                                            "Q1.Payments", "Q2.Payments"))







