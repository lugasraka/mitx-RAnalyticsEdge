polling <- read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

# handle missing data NAs
library(mice)
simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)

# Feedback imputed data to the original
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)

train <- subset(polling, Year == 2004 | Year == 2008)
test <- subset(polling, Year == 2012)
table(train$Republican)

sign(20)

table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

# Finding multicollinearity
cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
mod1 <- glm(Republican ~ PropR, data = train, family = 'binomial')
summary(mod1)

pred1 <- predict(mod1, type = 'response')
table(train$Republican, pred1 >= 0.5)

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = train, family = 'binomial')
summary(mod2)
pred2 <- predict(mod2, type = 'response')
table(train$Republican, pred2 >= 0.5)

##

table(test$Republican, sign(test$Rasmussen))

TestPrediction <- predict(mod2, newdata = test, type = "response")
table(test$Republican, TestPrediction >= 0.5)

subset(test, TestPrediction >= 0.5 & Republican == 0)
