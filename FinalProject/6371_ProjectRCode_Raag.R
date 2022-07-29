library(tidyverse)
library(dplyr)
library(car)
library(readr)
library(readxl)
library(knitr)

library(ggplot2)
library(ggpubr)
library(ggcorrplot)

library(stringr)
library(multcomp)
library(pairwiseCI)
library(effectsize)
library(investr)
library(broom)
library(olsrr)

setwd("D:/School/GitHub/MSDS_6371_Stat_Foundations/FinalProject")

FullData = read.csv("Data/train.csv", na.strings = "NA", strip.white = T)

Clean_FullData = FullData

Submission_TestData  = read.csv("Data/test.csv", na.strings = "NA", strip.white = T)

####################################### Data Cleaning

summary(Clean_FullData)

# Test if na.strings worked
# There are 81 NA's in the column BM "GarageCond" according to Excel

sum(is.na(Clean_FullData$GarageCond))

# R counted 81 na's, therefore it worked. 

# This is a loop that will go through the columns and decide 
# whether to delete the column based off of the amount of na's. Then it will 
# go through each row and eleminate the row if it has more than 1 na. 

yCol = 0
yRow = 0

for (i in 1:length(Clean_FullData)){
  x = sum(is.na(Clean_FullData[,i])) / length(Clean_FullData[,i])
  if (x > 0.3150685){ # 1460 - (.3150685 * 1460) = 1000
    yCol = append(yCol,i)
  }
}
yCol
length(yCol)
Clean_FullData = subset(Clean_FullData, select = -yCol)


for (i in 1:nrow(Clean_FullData)){
  x = as.numeric(rowSums(is.na(Clean_FullData[i,])))
  if (x > 0){
    yRow = append(yRow,i)
  }
}
yRow
length(yRow)
Clean_FullData = Clean_FullData[-yRow,]

# Now that the dataset is cleaned

rm(i,x,yCol,yRow)

summary(Clean_FullData)
dim(Clean_FullData)

# We can see that there are 76 variables, and 1094 rows, where each row has
# complete information on a house. This sample is more than plenty to configure
# a model. 

Clean_FullData$Neighborhood = as.factor(Clean_FullData$Neighborhood)
Clean_FullData$YrSold = as.factor(Clean_FullData$YrSold)


####################################### Data Selection // Analysis Question 1

pattern = c("NAmes","Edwards","BrkSide")

AQ1_Data = Clean_FullData %>% dplyr::select(Neighborhood,GrLivArea,SalePrice) %>% 
  filter(grepl(paste(pattern, collapse="|"), Neighborhood)) %>%
  mutate(GrLivArea = round(GrLivArea / 100))

summary(AQ1_Data)

## Plotting the data

ggplot(AQ1_Data, aes(x = GrLivArea, y = log(SalePrice))) + geom_point(aes(color = Neighborhood), alpha = .8) + 
  geom_smooth(method='lm', se = F, color = 'black')


## Model

AQ1Model = lm((log(SalePrice)~GrLivArea+Neighborhood+Neighborhood*GrLivArea),data = AQ1_Data)
summary(AQ1Model)

par(mfrow = c(2,2))

plot(AQ1Model)

par(mfrow = c(1,1))

# Looking at the plots, it's clear to see that there are points that are neccesary
# to remove. Looking at Resids V Lev, we can see that are points that should be 
# looked at. 

# The ADJ r^2 value seems pretty low, at .4206, let see if removing the points mentioned
# will increase it. 

OutRem_AQ1_Data = AQ1_Data

OutRem_AQ1_Data = OutRem_AQ1_Data[-c(70,95,119,136,248,262),]

AQ1Model = lm((log(SalePrice)~GrLivArea+Neighborhood+Neighborhood*GrLivArea),data = OutRem_AQ1_Data)

summary(AQ1Model)

par(mfrow = c(2,2))

plot(AQ1Model)

par(mfrow = c(1,1))

# Okay, these graphs look a lot better, and the ADJ R^2, went up to .49. The model still isn't
# great, but that's also because of the exclusion of so many other variables. 


# delete later
#
# quick note on log(y) transform
# https://www.real-statistics.com/multiple-regression/multiple-regression-log-transformations/
#
# build model here

# Also need to do assumptions formally

par(mfrow = c(2,2))

plot(AQ1Model)

par(mfrow = c(1,1))




####################################### Data Selection // Analysis Question 2

CorrMatData = Clean_FullData %>% dplyr::select(where(is.numeric))

CleanCorrMatrix = round(cor(CorrMatData), 1)

ggcorrplot(CleanCorrMatrix, hc.order = TRUE, type = "lower", outline.col = "white")


ggplot(Clean_FullData, aes(x=YrSold, y=log(SalePrice), fill = YrSold)) + geom_boxplot() + 
  theme(legend.position="none") + 
  scale_fill_brewer(palette="Dark2") + 
  labs(title = "Log(SalePrice) Over the Years)", x = "Year Sold", y = "Log(SalePrice)")
  















































































































































































































































































































































































