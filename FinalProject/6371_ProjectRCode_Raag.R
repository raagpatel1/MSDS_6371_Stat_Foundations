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
library(forecast)


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

# Need to assign certain columns to be categorical/continuous. So we will output
# the dataframe to a csv, and go through the columns, using the kaggle website
# to help us.

write.csv(Clean_FullData,"Data/Clean_FullData.csv", row.names = T)

# Have to do it manually:

Clean_FullData$Id = as.numeric(Clean_FullData$Id)

sapply(Clean_FullData, class)

Clean_FullData[,c(2,3,6:25,27:33,35,39:42,53,55,57:59,62:64,73:75)] <- lapply(Clean_FullData[,c(2,3,6:25,27:33,35,39:42,53,55,57:59,62:64,73:75)], factor)

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

# # Building a correlation matrix, where we only take data that has high ( > |.5|) correlation
# # with SalePrice
# 
# CorrMatData = Clean_FullData %>% dplyr::select(where(is.numeric)) %>% correlate() %>% focus(SalePrice) %>% 
#   mutate(term = factor(term, levels = term[order(SalePrice)])) %>% filter(SalePrice > abs(.5))
# 
# CorrMatData  %>% 
#   ggplot(aes(x = term, y = SalePrice)) +
#   geom_bar(stat = "identity", color = "black", fill = "turquoise") +
#   ylab("Correlation with Sale Price") + xlab("Variable") + theme_tufte() +
#   geom_text(aes(label = paste(round((SalePrice*100),2), "%", sep = "")), 
#                                 parse = F, vjust=1.6, color="black", size=3.5) 
#   
# 
# # Looking to see if the saleprice has significant changes over the years
# 
# ggplot(Clean_FullData, aes(x=YrSold, y=SalePrice, fill = YrSold)) + geom_boxplot() + 
#   theme(legend.position="none") + 
#   scale_fill_brewer(palette="Dark2") + 
#   labs(title = "Price of Home Sales Over the Years", x = "Year Sold", y = "SalePrice")
#   
# 
# # Let's test the correlation between the categorical variables and the sales price
# # to see which variables we should keep there. 
# 
# # First we will create a dataframe with just the catgeorical variables and 
# # SalePrice
# 
# CatModelData =  Clean_FullData %>% dplyr::select(!where(is.numeric), SalePrice, -Utilities, Id)
# 
# # We need sale price obviously, and all homes have the same value for utilities. 
# # Id is for a later full join
# sapply(CatModelData, class)
# 
# Model_Cat_SP = aov(SalePrice ~ YrSold, data = CatModelData)
# summary(Model_Cat_SP)
# 
# # Where is the p value stored in the AOV model? 
# summary(aov(SalePrice ~ YrSold, data = CatModelData))[[1]][["Pr(>F)"]][1]
# 
# # Attempting to build a function where I loop all of the cat variables
# # that we have, and eliminate those that have little to no effect on the data.
# # We can use the AOV since we have met the assumptions, and it will tell us if 
# # atleast one of the factor levels in each column have some effect on the mean.
# 
# yCol = NULL
# x = 0
# 
# for (i in 1:length(CatModelData[,1:44])){
#   x = x + 1
#   # print(x)
#   if (summary(aov(SalePrice ~ CatModelData[,x], data = CatModelData))[[1]][["Pr(>F)"]][1] > .01){ # alpha level
#     yCol = append(yCol,x)
#     # print(summary(aov(SalePrice ~ CatModelData[,x], data = CatModelData))[[1]][["Pr(>F)"]][1])
#   }
# }
# 
# yCol
# length(yCol)
# 
# CatModelData = subset(CatModelData, select = -yCol)
# 
# sapply(CatModelData, class)
# 
# # I've noticed that some columns have similar data, so I will manually pit them against 
# # each other, to see which column to keep. 
# 
# summary(aov(SalePrice ~ GarageYrBlt  , data = CatModelData))
# 
# summary(aov(SalePrice ~ GarageCond, data = CatModelData))
# 
# TukeyHSD(aov(SalePrice ~ BsmtQual    , data = CatModelData))
# 
# 
# 
# # lets just build a model
# 
# # combine the 2 cat/cont data
# 
# ContData = Clean_FullData %>% dplyr::select(Id,TotalBsmtSF, X1stFlrSF, GrLivArea, FullBath, TotRmsAbvGrd, GarageCars, GarageArea)
# 
# AQ2_CleanData = ContData %>% full_join(CatModelData, by = "Id")
# 
# AQ2_CleanData = AQ2_CleanData %>% dplyr::select(-Id,-YearRemodAdd,-Electrical,-GarageYrBlt,-GarageQual)
# 
# AQ2_CleanData$SalePrice = log(AQ2_CleanData$SalePrice)
# 
# sapply(AQ2_CleanData, class)
# 
# dim(AQ2_CleanData)
# 
# AQ2_Model = lm(SalePrice ~ ., data = AQ2_CleanData)
# 
# summary(AQ2_Model)
# 
# dim(summary(AQ2_Model)$coefficients)
# 
# # # Now let's get rid of everything :)
# # 
# # 
# # xRow = NULL
# # x = 0
# # 
# # for (i in 1:length(summary(AQ2_Model)$coefficients[,4])){
# #   x = x + 1
# #   if (summary(AQ2_Model)$coefficients[x,4] > .01){ # alpha level
# #     xRow = append(xRow,x)
# #   }
# # }
# # 
# # xRow
# # length(xRow)
# 
# # test = AQ2_Model
# # summary(test)
# # test = update(test,~.-YearBuilt2008)
# # summary(test)
# # dim(summary(test)$coefficients)
# 
# # variables <- labels(test)[attr(terms(test), "order") == 1]
# # factors <- sapply(names(test$xlevels), function(x) paste0(x, test$xlevels[[x]])[-1])
# # setdiff(colnames(model.matrix(test)), c("(Intercept)", variables, unlist(factors)))
# 
# 
# # AQ2_Model = update(AQ2_Model,~.-Id)
# # summary(AQ2_Model)
# 
# xtestx = ols_step_both_p(AQ2_Model,pent = .02,prem = .03, progress = T, details = F)
# 
# # 
# # Model Summary                             
# # ---------------------------------------------------------------------
# #   R                     0.952       RMSE                   26548.362 
# # R-Squared               0.907       Coef. Var                 14.194 
# # Adj. R-Squared          0.898       MSE                704815501.583 
# # Pred R-Squared           -Inf       MAE                    15822.379 
# # ---------------------------------------------------------------------
# #
# 
# # apply the model
# 
# 
# # row.number = sample(1:nrow(AQ2_CleanData), 0.8*nrow(AQ2_CleanData))
# # train = AQ2_CleanData[row.number,]
# # test = AQ2_CleanData[-row.number,]
# # dim(train)
# # dim(test)
# # 
# # pred1 <- predict(xtestx$model, newdata = test)
# # rmse <- sqrt(sum((exp(pred1) - test$SalePrice)^2)/length(test$SalePrice))
# # c(RMSE = rmse, R2=summary(xtestx$model)$r.squared)
# # 
# # IdSalePrice = Clean_FullData %>% dplyr::select(Id,SalePrice)
# 
# # new attempt
# 
# SalePrice_train = AQ2_CleanData[2:1094,43]
# 
# Affectors_train = as.matrix(AQ2_CleanData[2:1094,1:42]) 
# 
# # SalePrice_test = as.matrix(Submission_TestData[1461:2919,2:78])
# # 
# # SalePrice_test = as.matrix(Submission_TestData[1461:2919,79])
# 
# full = lm(SalePrice ~ ., data = AQ2_CleanData)
# null = lm(SalePrice ~ 1, data = AQ2_CleanData)
# 
# fullStep = step(null, scope = list(upper=full), direction="both")
# 
# summary(fullStep)
# # 
# # Residual standard error: 0.1101 on 947 degrees of freedom
# # Multiple R-squared:  0.9326,	Adjusted R-squared:  0.9222 
# # F-statistic: 89.72 on 146 and 947 DF,  p-value: < 2.2e-16
# # 
# 
# a = predict(fullStep, newdata = test)
# 
# x = colnames(AQ2_CleanData)
# x = x[1:42]
# Submission_TestData_Filtered = Submission_TestData %>% dplyr::select(x)
# 
# Submission_TestData_Filtered[,c(8:42)] = lapply(Submission_TestData_Filtered[,c(8:42)], factor)
# 
# 
# sapply(AQ2_CleanData, class)
# sapply(Submission_TestData_Filtered, class)
# 
# TestSubmission_1 = predict(fullStep, newdata = Submission_TestData_Filtered)
# 
# ## test
# AQ2_CleanData$ExterQual
# AQ2_CleanData$ExterQual = as.numeric(factor(AQ2_CleanData$ExterQual))
# 

# https://www.kaggle.com/code/ankitnagam/real-estate-in-ames-iowa/report

# 
# I am keeping the above code, as a way to show that this next part is us
# learning from our many mistakes. 
# 

FullData = read.csv("Data/train.csv", na.strings = "NA", strip.white = T)

Submission_TestData  = read.csv("Data/test.csv", na.strings = "NA", strip.white = T)

Submission_TestData$SalePrice = NA
AllData = rbind(FullData,Submission_TestData)

# Starting over, as you can see. Let's just start from the beginning using all
# of the original data. 

# Combining the data so that we can easily modify the columns, and can easily
# split it up earlier.

str(AllData)

# We also know to make SalePrice log transformed to adress normality. 

qqnorm(AllData$SalePrice) 
qqline(AllData$SalePrice)

qqnorm(log(AllData$SalePrice)) 
qqline(log(AllData$SalePrice))

AllData$SalePrice = log(AllData$SalePrice)

# c = c(3,6:8)
# c
# AllData[,c] = as.factor(AllData[,c])
# 
# AllData[,c(3,6:18)] = as.factor(AllData[,c(3,6:18)])
# 
# Warning message:
#   In xtfrm.data.frame(x) : cannot xtfrm data frames

# Will need to change the columns to numeric/categorical. We tried 
# somehow automating it, but to no avail. By hand we go; 

AllData$MSZoning = as.numeric(factor(AllData$MSZoning))
AllData$Street = as.numeric(factor(AllData$Street))
AllData$Alley = as.numeric(factor(AllData$Alley))
AllData$LotShape = as.numeric(factor(AllData$LotShape))
AllData$LandContour = as.numeric(factor(AllData$LandContour))
AllData$Utilities = as.numeric(factor(AllData$Utilities))
AllData$LotConfig = as.numeric(factor(AllData$LotConfig))
AllData$LandSlope = as.numeric(factor(AllData$LandSlope))
AllData$Neighborhood = as.numeric(factor(AllData$Neighborhood))
AllData$Condition1 = as.numeric(factor(AllData$Condition1))
AllData$Condition2 = as.numeric(factor(AllData$Condition2))
AllData$BldgType = as.numeric(factor(AllData$BldgType))
AllData$HouseStyle = as.numeric(factor(AllData$HouseStyle))
AllData$OverallQual = as.numeric(factor(AllData$OverallQual))
AllData$OverallCond = as.numeric(factor(AllData$OverallCond))
AllData$YearBuilt = as.numeric(as.factor(AllData$YearBuilt))
AllData$YearRemodAdd = as.numeric(as.factor(AllData$YearRemodAdd))
AllData$RoofStyle = as.numeric(factor(AllData$RoofStyle))
AllData$RoofMatl = as.numeric(factor(AllData$RoofMatl))
AllData$Exterior1st = as.numeric(factor(AllData$Exterior1st))
AllData$Exterior2nd = as.numeric(factor(AllData$Exterior2nd))
AllData$MasVnrType = as.numeric(factor(AllData$MasVnrType))
AllData$ExterQual = as.numeric(factor(AllData$ExterQual))
AllData$ExterCond = as.numeric(factor(AllData$ExterCond))
AllData$Foundation = as.numeric(factor(AllData$Foundation))
AllData$BsmtQual = as.numeric(factor(AllData$BsmtQual))
AllData$BsmtCond = as.numeric(factor(AllData$BsmtCond))
AllData$BsmtExposure = as.numeric(factor(AllData$BsmtExposure))
AllData$BsmtFinType1 = as.numeric(factor(AllData$BsmtFinType1))
AllData$BsmtFinType2 = as.numeric(factor(AllData$BsmtFinType2))
AllData$Heating = as.numeric(factor(AllData$Heating))
AllData$HeatingQC = as.numeric(factor(AllData$HeatingQC))
AllData$CentralAir = as.numeric(factor(AllData$CentralAir))
AllData$Electrical = as.numeric(factor(AllData$Electrical))
AllData$KitchenQual = as.numeric(factor(AllData$KitchenQual))
AllData$TotRmsAbvGrd = as.numeric(factor(AllData$TotRmsAbvGrd))
AllData$Functional = as.numeric(factor(AllData$Functional))
AllData$FireplaceQu = as.numeric(factor(AllData$FireplaceQu))
AllData$GarageType = as.numeric(factor(AllData$GarageType))
AllData$GarageYrBlt = as.numeric(as.factor(AllData$GarageYrBlt))
AllData$GarageFinish = as.numeric(factor(AllData$GarageFinish))
AllData$GarageQual = as.numeric(factor(AllData$GarageQual))
AllData$GarageCond = as.numeric(factor(AllData$GarageCond))
AllData$PavedDrive = as.numeric(factor(AllData$PavedDrive))
AllData$PoolQC = as.numeric(factor(AllData$PoolQC))
AllData$Fence = as.numeric(factor(AllData$Fence))
AllData$SaleType = as.numeric(factor(AllData$SaleType))
AllData$YrSold = as.numeric(as.factor(AllData$YrSold))
AllData$MiscFeature = as.numeric(factor(AllData$MiscFeature))
AllData$SaleCondition = as.numeric(factor(AllData$SaleCondition))


sapply(AllData, class)

# Instead of deleting NA values, as we did before, we will throw 0's in for the
# categorical variables. Mainly looking at data with a lot of NA's.

summary(AllData)

AllData$Alley[which(is.na(AllData$Alley))] = 0
AllData$PoolQC[which(is.na(AllData$PoolQC))] = 0
AllData$Fence[which(is.na(AllData$Fence))] = 0
AllData$MiscFeature[which(is.na(AllData$MiscFeature))] = 0
AllData$FireplaceQu[which(is.na(AllData$FireplaceQu))] = 0
AllData$LotFrontage[which(is.na(AllData$LotFrontage))] = 0
AllData$GarageType[which(is.na(AllData$GarageType))] = 0
AllData$GarageFinish[which(is.na(AllData$GarageFinish))] = 0
AllData$GarageQual[which(is.na(AllData$GarageQual))] = 0
AllData$GarageCond[which(is.na(AllData$GarageCond))] = 0
AllData$GarageYrBlt[which(is.na(AllData$GarageYrBlt))] = 0
AllData$BsmtFinType2[which(is.na(AllData$BsmtFinType2))] = 0
AllData$BsmtFinType1[which(is.na(AllData$BsmtFinType1))] = 0
AllData$BsmtExposure[which(is.na(AllData$BsmtExposure))] = 0
AllData$BsmtCond[which(is.na(AllData$BsmtCond))] = 0
AllData$BsmtFullBath[which(is.na(AllData$BsmtFullBath))] = 0
AllData$BsmtQual[which(is.na(AllData$BsmtQual))] = 0
AllData$MasVnrType[which(is.na(AllData$MasVnrType))] = 0
AllData$MasVnrArea[which(is.na(AllData$MasVnrArea))] = 0

summary(AllData)

# Unsure what to do about the SalePrice NA's, we have seen methods of using the 
# mean, but that is hard with all the categorical variable differences. We are
# also trying not to delete any data, as that has screwed us in our first 
# attempt of this problem. 

# Time to split the data up. 

FinalTestData = AllData[1461:2919,1:80]

FinalTrainData = AllData[1:1460,1:81]

# Let's create a model with everything in it, then use olss stepwise to 
# fix it. 

CustomModel = lm(SalePrice ~  ., data = FinalTrainData)
summary(CustomModel)
CV(CustomModel)

# Residual standard error: 0.1343 on 1380 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.8931,	Adjusted R-squared:  0.887 
# F-statistic: 147.7 on 78 and 1380 DF,  p-value: < 2.2e-16

# High AdjR^2, probably bc of the overfitting. Though we are surprised that 
# the AdjR^2 is not lower with these many variables in the model. 

StepwiseModel = ols_step_both_p(CustomModel, prem = 0.01, pent = 0.02, details = F, progress = T)
StepwiseModel = StepwiseModel$model
summary(StepwiseModel)
# Residual standard error: 0.1374 on 1435 degrees of freedom
# Multiple R-squared:  0.8837,	Adjusted R-squared:  0.8817 
# F-statistic: 454.2 on 24 and 1435 DF,  p-value: < 2.2e-16
CV(StepwiseModel)
#       CV           AIC          AICc           BIC         AdjR2 
# 2.433418e-02 -5.769467e+03 -5.768487e+03 -5.632026e+03  8.817212e-01 




BackwardsModel = ols_step_backward_p(fit, prem = 0.01, details = F, progress = T)
summary(BackwardsModel)
BackwardsModel = BackwardsModel$model
summary(BackwardsModel)
# Residual standard error: 7.702 on 198 degrees of freedom
# Multiple R-squared:  0.3978,	Adjusted R-squared:  0.3948 
# F-statistic: 130.8 on 1 and 198 DF,  p-value: < 2.2e-16
CV(BackwardsModel)
#       CV        AIC       AICc        BIC      AdjR2 
# 59.920338 820.606174 820.728623 830.501126   0.394783 






ForwardModel = ols_step_forward_p(fit, penter = 0.01, details = F, progress = T)
ForwardModel = ForwardModel$model
summary(ForwardModel)
# Residual standard error: 7.702 on 198 degrees of freedom
# Multiple R-squared:  0.3978,	Adjusted R-squared:  0.3948 
# F-statistic: 130.8 on 1 and 198 DF,  p-value: < 2.2e-16
CV(ForwardModel)
#     CV        AIC       AICc        BIC      AdjR2 
# 59.920338 820.606174 820.728623 830.501126   0.394783 
























































































































































































































































































