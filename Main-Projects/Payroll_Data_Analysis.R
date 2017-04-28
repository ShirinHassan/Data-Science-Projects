library(stringr)
library(plyr)
library(ggplot2)

PayrollData <- read.csv("C:/Users/sherr/Desktop/R/data.csv")

head(PayrollData)
summary(PayrollData)
str(PayrollData)
# 285008 obs 35 variables 
summary(PayrollData$Year)
# Data shows years between 2013-2016

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
        PayrollData$Payments.Over.Base.Pay = as.numeric(gsub("\\$", "", PayrollData$Payments.Over.Base.Pay)) # remove this one 
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


# Lets explore         
summary(PayrollData$Projected.Annual.Salary)
        # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        #   0   56730   78950   79990   99810  360700 
        # Median and mean salary are close and around $80,000, 
        # they show data is well distributed 

# Lets visualise this 
qplot(PayrollData$Projected.Annual.Salary, 
      fill=..count.., 
      geom="histogram", 
      xlab = "Salary", 
      main = "Histogram for Salary") 


# boxplot
plot <- ggplot(PayrollData, aes(Department.Title, Projected.Annual.Salary/1000)) 
plot +  geom_boxplot(fill='white', color="darkblue") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Department") + ylab("Annual Salary" ) 

# Observations from the plot: 
  # City attorney, City administrative office and Los Angeles Dept of Tourism are the top paying companies 
  # City clerk, Employee relations Board and Public Work seem to be on the lower salary side 
  # Many outliers, but mostly on the positive side 
    # The highest salary outlier belongs to Airports LAWA (a couple of people are really at the top!)

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
t.test(Payroll2015$Projected.Annual.Salary, Payroll2016$Projected.Annual.Salary,
      paired = FALSE, var.equal = FALSE)
      qt(0.975, 119580)
      # P value < 0.05 - reject null 


# Conclusion: 2016 and 2015 annual salaries different 
            # Mean of 2016 annual salary is greater than mean of 2015 annual salary 


