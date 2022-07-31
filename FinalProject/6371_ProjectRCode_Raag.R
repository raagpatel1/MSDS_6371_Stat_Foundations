library(tidyverse)
library(dplyr)
library(car)
library(readr)
library(readxl)
library(knitr)

library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(ggthemes)

library(stringr)
library(corrplot)
library(corrr)
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

# Remove ID Column, as we do not need it, since we can access row number. 
# If we do need it, we can use FullData to fill in the blanks. 

Clean_FullData$Id = NULL

# Need to assign certain columns to be categorical/continuous. So we will output
# the dataframe to a csv, and go through the columns, using the kaggle website
# to help us.

write.csv(Clean_FullData,"Data/Clean_FullData.csv", row.names = T)

# Have to do it manually:

sapply(Clean_FullData, class)

Clean_FullData[,c(1,2,5:24,26:32,34,38:41,52,54,56:58,61:63,72:74)] <- lapply(Clean_FullData[,c(1,2,5:24,26:32,34,38:41,52,54,56:58,61:63,72:74)], factor)

sapply(Clean_FullData, class)


####################################### Data Selection // Analysis Question 1

# # Setting the pattern allows us to easily select 3 neighborhood
# 
pattern = c("NAmes","Edwards","BrkSide")
# 
AQ1_Data = Clean_FullData %>% dplyr::select(Neighborhood,GrLivArea,SalePrice) %>%
  filter(grepl(paste(pattern, collapse="|"), Neighborhood)) %>%
  mutate(GrLivArea = round(GrLivArea / 100))
# 
# summary(AQ1_Data)
# 
# ## Plotting the data
# 
ggplot(AQ1_Data, aes(x = GrLivArea, y = SalePrice)) + geom_point(aes(color = Neighborhood), alpha = .8) +
  geom_smooth(method='lm', se = F, color = 'black')
# 
# 
# ## Model
# 
AQ1Model = lm((SalePrice~GrLivArea+Neighborhood+Neighborhood*GrLivArea),data = AQ1_Data)
summary(AQ1Model)
# 
par(mfrow = c(2,2))

plot(AQ1Model)

par(mfrow = c(1,1))
# 
# # Looking at the plots, it's clear to see that there are points that are neccesary
# # to remove. Looking at Resids V Lev, we can see that are points that should be 
# # looked at. 
# 
# # The ADJ r^2 value seems pretty low, at .4206, let see if removing the points mentioned
# # will increase it. 
# 

# Statistically removed outliers
cooksD <- cooks.distance(AQ1Model)
# cooksD

influential <- cooksD[(cooksD >(3 * mean(cooksD,na.rm = TRUE)))]
# influential

names_of_influential <- names(influential)
outliers <- AQ1_Data[names_of_influential,]
OutRem_AQ1_Data <- AQ1_Data %>% anti_join(outliers)


AQ1Model <-lm(SalePrice~GrLivArea+Neighborhood+Neighborhood*GrLivArea,data =OutRem_AQ1_Data)

summary(AQ1Model)

# confint(AQ1Model)
# 
# par(mfrow = c(2,2))
# 
# plot(AQ1Model)
# 
# par(mfrow = c(1,1))
# 
# # Okay, these graphs look a lot better, and the ADJ R^2, went up to .49. The model still isn't
# # great, but that's also because of the exclusion of so many other variables. 
# 
# # Let's remove the outliers from our main dataset now.
# 
# Clean_FullData = Clean_FullData[-c(339,131,190,169,372),]


####################################### Data Selection // Analysis Question 2

# Building a correlation matrix, where we only take data that has high ( > |.5|) correlation
# with SalePrice

CorrMatData = Clean_FullData %>% dplyr::select(where(is.numeric)) %>% correlate() %>% focus(SalePrice) %>% 
  mutate(term = factor(term, levels = term[order(SalePrice)])) %>% filter(SalePrice > abs(.5))

CorrMatData  %>% 
  ggplot(aes(x = term, y = SalePrice)) +
  geom_bar(stat = "identity", color = "black", fill = "turquoise") +
  ylab("Correlation with Sale Price") + xlab("Variable") + theme_tufte() +
  geom_text(aes(label = paste(round((SalePrice*100),2), "%", sep = "")), 
                                parse = F, vjust=1.6, color="black", size=3.5) 
  

# Looking to see if the saleprice has significant changes over the years

ggplot(Clean_FullData, aes(x=YrSold, y=SalePrice, fill = YrSold)) + geom_boxplot() + 
  theme(legend.position="none") + 
  scale_fill_brewer(palette="Dark2") + 
  labs(title = "Price of Home Sales Over the Years", x = "Year Sold", y = "SalePrice")
  

# Let's test the correlation between the categorical variables and the sales price
# to see which variables we should keep there. 

# First we will create a dataframe with just the catgeorical variables and 
# SalePrice

CatModelData =  Clean_FullData %>% dplyr::select(!where(is.numeric), SalePrice, -Utilities)
# We need sale price obviously, and all homes have the same value for utilities.
sapply(CatModelData, class)

Model_Cat_SP = aov(SalePrice ~ YrSold, data = CatModelData)
summary(Model_Cat_SP)

# Where is the p value stored in the AOV model? 
summary(aov(SalePrice ~ YrSold, data = CatModelData))[[1]][["Pr(>F)"]][1]

# Attempting to build a function where I loop all of the cat variables
# that we have, and eliminate those that have little to no effect on the data.
# We can use the AOV since we have met the assumptions, and it will tell us if 
# atleast one of the factor levels in each column have some effect on the mean.

yCol = NULL
x = 0

for (i in 1:length(CatModelData)-1){
  x = x + 1
  # print(x)
  if (summary(aov(SalePrice ~ CatModelData[,x], data = CatModelData))[[1]][["Pr(>F)"]][1] > .05){ # alpha level
    yCol = append(yCol,x)
    # print(summary(aov(SalePrice ~ CatModelData[,x], data = CatModelData))[[1]][["Pr(>F)"]][1])
  }
}

yCol
length(yCol)


CatModelData = subset(CatModelData, select = -yCol)

sapply(CatModelData, class)


summary.aov(SalePrice ~ LotShape, data = CatModelData)





































































































































































































































































































































































