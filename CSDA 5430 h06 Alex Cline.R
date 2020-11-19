#31
#load datasets
framingham_nb_test <- read.csv("~/CSDA 5430/Website Data Sets/framingham_nb_test.csv")
framingham_nb_training <- read.csv("~/CSDA 5430/Website Data Sets/framingham_nb_training.csv")
View(framingham_nb_test)
#convert variables to factors
framingham_nb_test$Sex <- factor(framingham_nb_test$Sex)
framingham_nb_test$Educ <- factor(framingham_nb_test$Educ)
framingham_nb_test$Death <- factor(framingham_nb_test$Death)
framingham_nb_training$Sex <- factor(framingham_nb_training$Sex)
framingham_nb_training$Educ <- factor(framingham_nb_training$Educ)
framingham_nb_training$Death <- factor(framingham_nb_training$Death)

#load naiveBayes library
library(e1071)
#naiveBayes
fram.nb <- naiveBayes(Death~Sex+Educ, data = framingham_nb_training)
fram.nb

#32
#predict and create contigency table
frampred <- predict(object = fram.nb, newdata = framingham_nb_test)

t.preds <- table(framingham_nb_test$Death, frampred)
rownames(t.preds) <- c("Actual: 0", "Actual: 1")
colnames(t.preds) <- c("Predicted: 0", "Predicted: 1")
addmargins(A = t.preds, FUN = list(Total = sum), quiet = TRUE)

#33
#load library
library(caret)
#confusionMatrix finds Accuracy and Error Rate is 1-Accuracy Rate
confusionMatrix(framingham_nb_test$Death, frampred)
#Accuracy = 57.3%
#Error Rate = 42.7%

#34
#Many ways can find these answers but I just use Confusion Matrix
#0=alive, 1=dead
#TP/TP+FP=Alive Accuracy, TN/TN+FN=Dead Accuracy
#Alive=38.7%, Dead=77.9%








