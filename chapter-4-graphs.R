library(ggplot2)
library(lessR)

mydata <- read.csv("~/Work/Study 2 - F2F vs Telephone/approvals.csv")

g2 <- ggplot(mydata, aes(Outcome, ..count..)) + geom_bar(aes(fill = ReferralSource), position = "dodge")
g2 + scale_fill_grey()
g3 <- ggplot(mydata, aes(AdviceType, ..count..)) + geom_bar(aes(fill = ReferralSource), position = "dodge")
g3 + scale_fill_grey()