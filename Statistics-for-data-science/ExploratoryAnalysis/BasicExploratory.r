# import csv file 
MyData1 <- read.csv("C:/Users/sherr/Desktop/Files/02_02/exploratory-r.csv")

# Quick snapshot of the data 
head(MyData1)

# basic visualisation 
hist(MyData1$impressions)
hist(MyData1$cpa)


# shift the names to each row
row.names(MyData1) <- MyData1$keyword
# review
head(MyData1)

# Create a data matrix 
MyData1Matrix <- data.matrix(MyData1)


# Heatmap
heatmap(MyData1Matrix, x, Rowv = NA, Colv = NA, scale = "column")