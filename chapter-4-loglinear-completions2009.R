library(readr)
library(MASS)

#Carrying out a loglinear analysis on likelihood of completion - 2009 sample
#Reading file
completions2009 <- read.csv("C:/Users/Nicole/Documents/Work/Study 2 - F2F vs Telephone/completions2009.csv")
View(completions2009)
attach(completions2009)
mytable <-table(Outcome,AdviceType,ReferralSource)
ftable(mytable)
dimnames(mytable)
margin.table(mytable)
summary(mytable)

#Creating a full model
sat.model = loglm(~Outcome*AdviceType*ReferralSource, data=mytable)
sat.model
step(sat.model, direction="backward")

#Testing two-way interactions
mydata <- completions2009
tbl1 = table(mydata$Outcome, mydata$AdviceType)
tbl1
chisq.test(tbl1)

tbl2 = table(mydata$ReferralSource, mydata$AdviceType)
tbl2
chisq.test(tbl2)

tbl3 = table(mydata$ReferralSource, mydata$Outcome)
tbl3
chisq.test(tbl3)
