#Chapter 10
#15
cereals <- read.csv("~/CSDA 5430/Website Data Sets/cereals.CSV")
View(cereals)

X <- subset(cereals, select = c("Fat","Sodium"))
Xs <- as.data.frame(scale(X))
colnames(Xs) <- c("Fat_z","Sodium_z")


#16
kmeans01 <- kmeans(Xs, centers = 3)

cluster <- as.factor(kmeans01$cluster)

Cluster1 <- Xs[which(cluster == 1),]
Cluster2 <- Xs[which(cluster == 2),]
Cluster3 <- Xs[which(cluster == 3),]

#17
summary(Cluster1)
summary(Cluster2)
summary(Cluster3)


#a low fat -0.7226 low sodium -1.6491 = Cluster 1
#b low fat -0.3742 high sodium 0.4973 = Cluster 3
#c high fat 1.3990 high sodium 0.063516 = Cluster 2


#____________________________________________________________________________
#Chapter 11
#39
bank_reg_training <- read.csv("~/CSDA 5430/Website Data Sets/bank_reg_training")
View(bank_reg_training)
bank_reg_test <- read.csv("~/CSDA 5430/Website Data Sets/bank_reg_test")
View(bank_reg_test)

bank_reg_training$Approval <- factor(bank_reg_training$Approval)
bank_reg_test$Approval <- factor(bank_reg_test$Approval)

model01_train <- lm(Credit.Score ~ Debt.to.Income.Ratio + Request.Amount, data = bank_reg_training)
summary(model01_train)

model01_test <- lm(Credit.Score ~ Debt.to.Income.Ratio + Request.Amount, data = bank_reg_test)
summary(model01_test)
install.packages("MLmetrics")
library("MLmetrics")
ytrue <- bank_reg_test$Credit.Score
X_test <- data.frame(Request.Amount = bank_reg_test$Request.Amount, Debt.to.Income.Ratio = bank_reg_test$Debt.to.Income.Ratio)
pred <- predict(model01_test, newdata = X_test)
sqrt(MSE(y_pred = pred, y_true = ytrue))
#s = 65.7693 is the size of the prediction error smaller than model2

#40
#R2 = 0.032827 compared to model2 the input variables add value for the test data



MAE(y_pred = pred, y_true = ytrue)
#MAE = 48.01625
library("MASS")
model01_step <- stepAIC(object = model01_test)



#41
#model2 has a smaller MAE so it beats the baseline model1

#model2
model02_test <- lm(Credit.Score ~ Debt.to.Income.Ratio, data = bank_reg_test)
summary(model02_test)

X_test2 <- data.frame(Debt.to.Income.Ratio = bank_reg_test$Debt.to.Income.Ratio)
pred2 <- predict(model02_test, newdata = X_test2)
install.packages("MLmetrics")
library("MLmetrics")
ytrue2 <- bank_reg_test$Credit.Score
MAE(y_pred = pred2, y_true = ytrue2)
#MAE = 48.05053


library("MASS")
model02_step <- stepAIC(object = model02_test)



#42
model3 <- lm(Interest ~ Request.Amount, data = bank_reg_test)
summary(model3)




