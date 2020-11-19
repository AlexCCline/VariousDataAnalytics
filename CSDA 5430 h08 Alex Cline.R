#Chapter 12
cereals <- read.csv("~/CSDA 5430/Website Data Sets/cereals.CSV")
View(cereals)

#drop some columns
drop <- c("Name","Manuf","Type","Calories","Cold","Nabisco","Quaker","Kelloggs",
          "GeneralMills","Ralston","AHFP")
df = cereals[,!(names(cereals) %in% drop)]

library(dplyr)
glimpse(df)

pairs(x = cereals[,c(10,8,11)], pch = 16)

#normalize and plot correlation matrix
library(BBmisc)
X_z <- normalize(df)
library(psych)
cor(X_z, method = "pearson")
cor.plot(X_z, method = "pearson")
#regression model
library(car)
model1 <- lm(Rating ~ Sugars + Fiber + Potass, data = X_z)
vif(model1)

#PCA1
install.packages("psych")
library(psych)
pca1 <- principal(r = X_z, rotate = "varimax", nfactors = 5)
print(pca1$loadings, cutoff = 0.49)

#plot eigenvalues
ss.load <- c(3.196, 2.230, 1.882, 1.287, 1.235)
par(mar=c(1,1,1,1)) #clear plots first to load plot of eigenvalues
plot(ss.load, type = "b", main = "Plot of Eigenvalues", ylab = "Value", xlab = "Component")
abline(h = 1, lty = 2)
#if margin error enter below code, clear plots, and run again
#par("mar")
#par(mar=c(1,1,1,1))

#26
#You would need at least 4 PCs

#27
#PCA2
pca2 <- principal(r = X_z, rotate = "varimax", nfactors = 2)
print(pca2$loadings, cutoff = 0.49)
#it accounts for at least 50% variability

#28
#Protein, Fiber, Potass, Shelf, Cups in component 1
#Fat, Sugars, Weight, Rating in component 2

#29
#Save components as variables for regression pca model
PC1 <- pca2$scores[,1]
PC2 <- pca2$scores[,2]
model.pca <-lm(X_z$Rating ~ PC1 + PC2)
model.pca
vif(model.pca)
#Regression coefficients PC1=0.358581 and PC2=-0.923041

#30 VIFs
#PC1=1.000039 and PC2=1.000039


#_________________________________________________________________________________
#Chapter 13

Adult <- read.csv("~/CSDA 5430/Website Data Sets/Adult")
View(Adult)
library(dplyr)
glimpse(Adult)

Adult$income <- factor(Adult$income)
#20
adultglm <- glm(income ~ age + education.num + hours.per.week, data=Adult, family = binomial)
summary(adultglm)

#21
#No because they are all significant

#22
#model would stay the same but if I remove one variable it would be:
adultglm2 <- glm(income ~ education.num + age, data=Adult, family = binomial)
summary(adultglm2)

#23
#age coefficient = 0.42848
#effect of one unit change in predictor variable based on age coefficient


#24
#poisson to find impact of probability of high income for 10 years to age
adultglm3 <- glm(age ~ income, data=Adult, family = poisson)
summary(adultglm3)
exp(3.606464 + (0.183074*10))
#229.7988 times more likely to have higher income adding 10 yrs






