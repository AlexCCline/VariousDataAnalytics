#########################CH 2#########################################
#11 The bottom right box are tabs for files, plots, packages, help, and viewer
#12 I am working on Chapter 2 exercises
#13 Ctrl+Enter is the shortcut for Run
#24
adult_ch3_training <- read.csv("~/CSDA 5430/Website Data Sets/adult_ch3_training")
View(adult_ch3_training)
adult <- adult_ch3_training
#25 the dataset name change is to make it simpler for variable use
# 26
install.packages(rpart)
library(rpart)
#27 
table01 <- table(adult$workclass, adult$sex)
##########################CH 3########################################
#11
bank_marketing_training <- read.csv("~/CSDA 5430/Website Data Sets/bank_marketing_training")
View(bank_marketing_training)
bank_train <- bank_marketing_training
n <- dim(bank_train)[1]
bank_train$Index <- c(1:n)
head(bank_train)
#12
bank_train$days_since_previous <- ifelse(test = bank_train$days_since_previous
                                         == 999, yes = NA, 
                                         no = bank_train$days_since_previous)
#13
install.packages(plyr)
library(plyr)

edu.num <- revalue(x= bank_train$education,
 replace = c("illiterate" = 0,
 "basic.4y" = 4, "basic.6y" = 6, "basic.9y" = 9,
 "high.school" = 12, "professional.course" = 12,
 "university.degree" = 16, "unknown" = NA))

bank_train$education_numeric <- as.numeric(levels(edu.num))[edu.num]


#21 use table filter in tab
nutrition_subset <- read.csv("~/CSDA 5430/Website Data Sets/nutrition_subset")
n <- dim(nutrition_subset)[1]
nutrition_subset$Index <- c(1:n)
View(nutrition_subset)
#1 "CHEESECAKE                    1 CAKE  ",
#2 "ICE CREAM; VANLLA; RICH 16% FT1/2 GAL ",
#3 "YELLOWCAKE W/ CHOCFRSTNG;COMML1 CAKE  ",
#4 "CREME PIE                     1 PIE   ",
#5 "	LARD                          1 CUP   "
#it is valid to compare saturated fat with various sizes if they are near equal to serving size

#22 use table filter in tab
nutrition_subset$saturated_fat_per_gram <- nutrition_subset$saturated_fat/nutrition_subset$weight_in_grams
#1 BUTTER; SALTED                1 TBSP  
#2 BUTTER; UNSALTED              1 TBSP  
#3 BUTTER; SALTED                1/2 CUP 
#4 BUTTER; UNSALTED              1/2 CUP 
#5 BUTTER; SALTED                1 PAT   
#Butter; salted 1 tbsp has most saturated fat per gram

#23 use table filter in tab
nutrition_subset$cholesterol_per_gram <- nutrition_subset$cholesterol/nutrition_subset$weight_in_grams
#1 EGGS; RAW; YOLK               1 YOLK  
#2 CHICKEN LIVER; COOKED         1 LIVER 
#3 BEEF LIVER; FRIED             3 OZ    
#4 EGGS; COOKED; FRIED           1 EGG   
#5 EGGS; RAW; WHOLE              1 EGG   
#Eggs raw yolk 1 yolk have most cholesterol

#24
#normalize
nutrition_subset$saturated_fat_per_gram_z <- scale(x = nutrition_subset$saturated_fat_per_gram)
#list outliers z < -3 | z > 3
nutrition_outliers <- nutrition_subset[ which(nutrition_subset$saturated_fat_per_gram_z < -3 | 
                                               nutrition_subset$saturated_fat_per_gram_z > 3),]
nutrition_sort <- nutrition_subset[order(-nutrition_subset$saturated_fat_per_gram_z),]
head(nutrition_sort)
#I set the amount of records listed so high/low is relative to the number I put
nutrition_sort[1:20,]
nutrition_sort[20:1,]

#high ened has 4 (i.e.: 7.106), low end has 4 (i.e.: 3.22)????????
#directions are not clear in chapter how to find outliers in high and low end of z scores
nutrition_outliers




