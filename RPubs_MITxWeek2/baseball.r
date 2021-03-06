baseball <- read.csv("baseball.csv")
str(baseball)

moneyball <- subset(baseball, Year < 2002)
str(moneyball)

moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

plot(moneyball$RD, moneyball$W)
WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

# W = 80.88 + 0.105*(RD)
# 80.88 + 0.1058(RD) >= 95, hence RD >= 135 =)

# Quiz