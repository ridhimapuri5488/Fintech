app = read.csv("appdata.csv")
screen = read.csv("top_screens.csv")

head(app)
head(screen)

summary(app)
summary(screen)

str(app)
options(scipen=999)
library(tidyverse)
library(ggplot2)

# users on a paticular day of week 
ggplot(app, aes(x = dayofweek)) +
  geom_histogram()


ggplot(app, aes(x = used_premium_feature)) +
  geom_histogram()


ggplot(app, aes(x = age)) +
  geom_histogram()

ggplot(data = app, aes(x = "", y = age)) + 
  geom_boxplot()

summary(app)

filter1 = filter(app, enrolled == 0)
nrow(filter1)

filter2 = filter(app, enrolled == 0 & enrolled_date == '')
nrow(filter2)

table(app$used_premium_feature)
table((app$enrolled))
#summary(app)




set.seed(1760)
split = sample.split(app$enrolled, SplitRatio = 0.7)
head(split)
train = app[split == TRUE,]
test = app[split == FALSE,]

nrow(train)/nrow(app)
table(train$enrolled)/nrow(train)
table(test$enrolled)/nrow(test)

str(app)
names(app)

logreg = glm(enrolled ~ as.factor(liked) + as.factor(used_premium_feature) + as.factor(minigame)
             + age + numscreens + as.factor(dayofweek), data=train, family="binomial")

summary(logreg)

#logreg1 = glm(enrolled ~ as.factor(liked) + as.factor(used_premium_feature) + as.factor(minigame)
              #+ age + numscreens + as.factor(dayofweek)+ as.Date(first_open), data=train, family="binomial")
summary(logreg)

test$predProbs = predict(logreg, newdata=test, type="response")
cutoff = 0.70
test$predDefault = ifelse(test$predProbs >= cutoff, 1, 0)
table(test$enrolled, test$predDefault)


str(test)

roc.pred = prediction(test$predProbs, test$enrolled)

perf = performance(roc.pred, "tpr", "fpr")

plot(perf,                      # the data
     main = "ROC Curve",        # the chart's title
     xlab = "1 - Specificity",  # the name of the x-axis
     ylab = "Sensitivity",      # the name of the y-axis
     colorize=TRUE)   
abline(0,1) # adds line at intercept 0, with slope 1
perf_auc = performance(roc.pred, "auc")
perf_auc
as.numeric(perf_auc@y.values)



LiftData <- performance(roc.pred, measure="lift", x.measure="rpp")

plot(LiftData,                # the data
     main="Lift Chart",       # the chart's title
     xlab="% of Population",  # the name of the x-axis
     ylab="Lift",             # the name of the y-axis
     col="blue")              # the color of our lift curve

abline(1,0,col="red") # this adds a straight line at intercept 1 with slope 0

summary(screen)
head(screen)

# users are duplicate
sum(table(app$user) - 1)

names(app)
library(dplyr)
library(tidyr)
library(caTools)
library(ROCR)

modified = read.csv("modified.csv")
str(modified)
names(modified)
modified = modified[ , -c(1,2,7,12,58)]

modified['first_open'] = as.Date(modified$first_open)
modified['dayofweek'] = factor(modified$dayofweek)

set.seed(1760)
split1 = sample.split(modified$enrolled, SplitRatio = 0.7)
head(split1)
train1 = modified[split1 == TRUE,]
test1 = modified[split1 == FALSE,]

nrow(train1)/nrow(modified)
table(train1$enrolled)/nrow(train1)
table(test1$enrolled)/nrow(test1)
str(train1)

logreg2 = glm(enrolled ~. -first_open, data=train1, family="binomial")
summary(logreg2)

pred_new  = predict(logreg2, newdata=test1, type="response")

test1$predProbs = predict(logreg2, newdata=test1, type="response")
cutoff = 0.6981603
#cutoff = 0.80
test1$predDefault = ifelse(test1$predProbs >= cutoff, 1, 0)
table(test1$enrolled, test1$predDefault)
roc.pred1 = prediction(test1$predProbs, test1$enrolled)
perf = performance(roc.pred1, "tpr", "fpr")

plot(perf,                      # the data
     main = "ROC Curve",        # the chart's title
     xlab = "1 - Specificity",  # the name of the x-axis
     ylab = "Sensitivity",      # the name of the y-axis
     colorize=TRUE)   
abline(0,1) # adds line at intercept 0, with slope 1

(4712+6008)/nrow(test1)
4712/(4712+966)
6008/(6008+3314)


perf_auc = performance(roc.pred1, "auc")
perf_auc
as.numeric(perf_auc@y.values)

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
head(subset(cutoffs, fpr > 0.18))

#calculating the loss
#As