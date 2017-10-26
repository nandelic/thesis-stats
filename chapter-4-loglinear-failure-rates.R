library(lessR)
library(MASS)

#Carrying out a loglinear analysis on Failure by 18 months
#Read data
mydata <- read.csv("~/Work/Study 2 - F2F vs Telephone/completions2009.csv")
attach(mydata)
mytable <-table(Fail18months,AdviceType,Referral)
ftable
ftable(mytable)
dimnames(mytable)
summary(mytable)


#Create full model
sat.model = loglm(~Fail18months*AdviceType*Referral, data=mytable)
#Perform stepwise regression
step(sat.model, direction="backward")

#Examine two-way interactions
tbl1 = table(mydata$Outcome, mydata$AdviceType)
tbl1
chisq.test(tbl1)

tbl2 = table(mydata$ReferralSource, mydata$AdviceType)
tbl2
chisq.test(tbl2)
                   
tbl3 = table(mydata$ReferralSource, mydata$Outcome)
tbl3
chisq.test(tbl3)
