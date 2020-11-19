#31 load ggplot2 library and cereals dataset
library(ggplot2)
cereals <- read.csv("~/CSDA 5430/Website Data Sets/cereals.CSV")
# plot bar graph
ggplot(cereals, aes(Manuf)) + geom_bar(aes(fill = Type))
#32 normalized bar graph
ggplot(cereals, aes(Manuf))+ geom_bar(aes(fill = Type), position = "fill")
#33 contigency table
t.v1 <- table(cereals$Manuf, cereals$Type)
t.v2 <- addmargins(A = t.v1, FUN = list(total=sum), quiet = TRUE)
round(prop.table(t.v1, margin = 2)*100,1)
#34 plot histogram
ggplot(cereals, aes(Calories)) + geom_histogram(aes(fill = Manuf), color="black")
                                       
#35 plot normalized histogram
ggplot(cereals, aes(Calories)) + geom_histogram(aes(fill = Manuf), color="black",
                                       position = "fill")
#############################################################################
#21 load Adult dataset
adult <- read.csv("~/CSDA 5430/Website Data Sets/Adult")
View(adult)
#set seed, drop first column, split data 50%
set.seed(7)
n <- dim(adult)[1]
train_ind <- runif(n)<.5
adult_train <- adult[train_ind,]
adult_test <- adult[!train_ind,]
#plot training and test data to see partitions
ggplot(adult_train, aes(education)) + geom_bar(aes(fill = income))
ggplot(adult_test, aes(education)) + geom_bar(aes(fill = income))

#22
#12449 records in training and 3031 are >50k
table(adult_train$income)

#23 calculations for resampling for 35%
#(0.35*12449)-3031/0.65=2040 records

#24 resample data using previous exercise of 2040 records and check proportion
to.resample <- which(adult_train$income == ">50K.")
our.resample <- sample(x = adult$income, size = 2040, replace = TRUE)
our.resample <- adult_train[our.resample,]
train_adult_rebal <- rbind(adult_train, our.resample)
t.v3 <- table(train_adult_rebal$income)
t.v4 <- rbind(t.v3, round(prop.table(t.v3),4))
colnames(t.v4) <- c("Response = <=50K.", "Response = <50K.")
rownames(t.v4) <- c("Count", "Proportion")
t.v4

#25
#We would look for the accuracy of the most frequent class from a predictive classification
#model at 99% accuracy for being >50K. The value is for class >50K. Accuracy is 99%.







