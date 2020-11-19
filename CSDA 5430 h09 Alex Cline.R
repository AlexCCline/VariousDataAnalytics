#Chapter 14
Adult <- read.csv("~/CSDA 5430/Website Data Sets/Adult")
View(Adult)
#subset variables into data frame
df <- subset(Adult, select = c("education", "marital.status", "income"))
#factor
Adult$income <- ordered(as.factor(Adult$income))

#apriori algorithms
#21
library(arules)
all.rules <- apriori(data = df, parameter = list(supp = 0.02, target = "rules",
                                                 conf = 0.5, minlen = 2,
                                                 maxlen = 2))
inspect(head(all.rules, by = "lift", n = 10))

#23
#diff
all.rules2 <- apriori(data = df,
                      parameter = list(arem = "diff",
                      aval = TRUE,
                      minval = 0.3, 
                      supp = 0.02, target = "rules",
                      conf = 0.5, minlen = 2,
                      maxlen = 2))
inspect(head(all.rules2, by = "lift", n = 10))

#26
#quot
all.rules3 <- apriori(data = df, parameter = list(arem = "quot",
                                                  aval = TRUE,
                                                  minval = 0.3,
                                                  supp = 0.02,
                                                  target = "rules",
                                                  conf = 0.5,
                                                  minlen = 2,
                                                  maxlen = 2))
inspect(head(all.rules3, by = "lift", n = 10))


























