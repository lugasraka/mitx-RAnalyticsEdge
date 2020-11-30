framingham <- read.csv("framingham.csv")
str(framingham)
summary(framingham)

#Splitting the dataset
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train <- subset(framingham, split == TRUE)
train2 <- na.omit(train)
test <- subset(framingham, split == FALSE)
test2 <- na.omit(test)

# Timet to build our log regression
framinghamLog <- glm(TenYearCHD ~ ., data = train2, family = binomial)
summary(framinghamLog)

# predicting
predictTest <- predict(framinghamLog, type = 'response', newdata = test2)
#building confusion matrix
table(test2$TenYearCHD, predictTest > 0.5)
# total accuracy
(1069+11)/ (1069+11+6+187)
(sens3 <- 11/(11+187))
(spec3 <- 1069/(1069+6))

library(ROCR)
# Finding area under curve
# problem solved: if NA is there in the dataset, please omit it so we can use functions from the ROCR, auc package
ROCRpred <- prediction(predictTest, test2$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
