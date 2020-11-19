###################CSDA 5430 Alex Cline h05####################################
#Chapter 6 Hands-on Analysis
#21 
#load datasets
Loans_Test <- read.csv("~/CSDA 5430/Website Data Sets/Loans_Test")
Loans_Training <- read.csv("~/CSDA 5430/Website Data Sets/Loans_Training")
View(Loans_Training)
#make categorical to factor
Loans_Training$Approval <- factor(Loans_Training$Approval)

#load packages
library(rpart)
library(rpart.plot)
#build and plot tree
cart01 <- rpart(formula = Approval ~ Debt.to.Income.Ratio + FICO.Score + Request.Amount,
                data = Loans_Training, method = "class")
rpart.plot(cart01)
#First few splits include FICO Score < 660 which is 100% of all observations
# and 50% is False and split into:
#31% of all observations are Yes and 4% are False
#69% of all observations are No and 70% are True
#and Debt to Income Ratio >= 0.31 of which:
#9% of all observations are Yes and 13% are False
#61% of all observations are No and 78% are True

#22
#make categorical to factor
Loans_Test$Approval <- factor(Loans_Test$Approval)
#build and plot tree
cart02 <- rpart(formula = Approval ~ Debt.to.Income.Ratio + FICO.Score + Request.Amount,
                data = Loans_Test, method = "class")
rpart.plot(cart02)
#Test and Training are not exactly the same but they are nearly identical

#23
#install and load packages
install.packages("C50")
library(C50)
C5 <- C5.0(formula = Approval ~  Debt.to.Income.Ratio + FICO.Score + Request.Amount,
          data = Loans_Training, control = C5.0Control(minCases = 1000))
plot(C5)
#The first few splits include Debto to Income Ratio is first node
#split into >0.31 and =< 0.31 which is less than 20% are True
#The second node is FICO Score split into =< 655 and > 655
# which is less than 20% are True
#The third node is FICO Score is split into =< 669 and > 669
#There are many more nodes but the plot explains them in detail

#24
#The CART model and C5.0 model are somewhat similar in style but different in content
#The similarities are the percentanges of the split, the node number, and types
# of split. However, these similarities are done in different ways.
#The differences are the C5.0 model does not show percent of all observations,
#it also doesn't show the percentage of the split for both sides of the node
#unless it is a leaf. It also shows the number of cases which is not shown in the
#CART model. The CART model is also much smaller in the training data than the C5.0 model
#in the amount of nodes and splits

#25
C5.test <- C5.0(formula = Approval ~  Debt.to.Income.Ratio + FICO.Score + Request.Amount,
           data = Loans_Test, control = C5.0Control(minCases = 1000))
plot(C5.test)
#The test C5.0 model is much different than the training C5.0 model in node split
#and content but the amount of nodes is similar

############################################################################################
#Chapter 7 Hands-on Analysis
#23
#load datasets
adult_ch6_training <- read.csv("~/CSDA 5430/Website Data Sets/adult_ch6_training")
adult_ch6_test <- read.csv("~/CSDA 5430/Website Data Sets/adult_ch6_test")

#make categorical to factor
adult_ch6_training$Marital.status <- factor(adult_ch6_training$Marital.status)
adult_ch6_training$Income <- factor(adult_ch6_training$Income)
adult_ch6_test$Marital.status <- factor(adult_ch6_test$Marital.status)
adult_ch6_test$Income <- factor(adult_ch6_test$Income)
#create C5.0 model and plot(optional)
C5.adult_train <- C5.0(formula = Income ~  Marital.status + Cap_Gains_Losses,
                data = adult_ch6_training, control = C5.0Control(minCases = 100))
plot(C5.adult_train)
#subset predictor variables from test dataset into own dataframe
test.X <- subset(x = adult_ch6_test, select = c("Marital.status", "Cap_Gains_Losses"))
#run test data through training data model to gather predictions
ypred <- predict(object = C5.adult_train, newdata = test.X)
ypred

#24
#create contingency table
t1 <- table(adult_ch6_test$Income, ypred)
row.names(t1) <- c("Actual: 0", "Actual: 1")
colnames(t1) <- c("Predicted: 0", "Predicted: 1")
t1 <- addmargins(A = t1, FUN = list(Total = sum), quiet = TRUE)
t1

#25
library(caret)
#Model Evaluation Table
#confusionMatrix function might not work. Worked first time no changes with output:
#Confusion Matrix and Statistics

#Reference
#Prediction <=50K >50K
#<=50K  4632 1105
#>50K     42  376

#Accuracy : 0.8136          
#95% CI : (0.8037, 0.8233)
#No Information Rate : 0.7594          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.3244          

#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.9910          
#            Specificity : 0.2539          
#         Pos Pred Value : 0.8074          
#         Neg Pred Value : 0.8995          
#             Prevalence : 0.7594          
#         Detection Rate : 0.7526          
#   Detection Prevalence : 0.9321          
#      Balanced Accuracy : 0.6224          
                                          
#       'Positive' Class : <=50K 
#For some reason now it is only producing this output:      
#[,1] [,2]
#[1,]    0    0
#[2,]    0 5737
#Either way I have combined the previous output above with using t1 to find the rest
confusionMatrix(ypred, adult_ch6_test$Income)
#Recall = 0.2539
#Accuracy = 0.8136
#Error rate = 0.1864
#Sensitivity = 0.9910
#Specificity = 0.2539
#Precision = 0.8995
#F1 = 0.3960
#F2 = 0.2965
#F0.5 = 0.5963

#26
#Accuracy is the number of correct predictors
#Error rate is the rate of errors compared to accuracy of correct predictors
#Sensitivity is the number positives correctly identified
#Specificity is the number or negatives correctly identified
#Precision is the ratio of correctly predicted positive observations to total
#positive observations
#F1 is weighted average of precision and recall where best value is 1 and worst is 0
#F2 weighs recall higher than precision
#F0.5 weighs recall less than precision

#27
#can't name a variable as "3x" so name it costmatrix3x
costmatrix3x <- matrix(c(0,4,1,0), byrow = TRUE, ncol = 2)
dimnames(costmatrix3x) <- list(c("0","1"),c("0","1"))
#add cost matrix to C5.0 model
C5.costs <- C5.0(Income ~ Marital.status + Cap_Gains_Losses, data = adult_ch6_training,
                 costs = costmatrix3x)





