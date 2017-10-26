library(lessR)
library(MASS)

#Carrying out a loglinear analysis on approval rates in 2009
approvals <- read.csv("~/Work/Study 2 - F2F vs Telephone/approvals.csv")
attach(approvals)
mytable <- table(Outcome, AdviceType, ReferralSource)
ftable(mytable)

margin.table(mytable)
dimnames(mytable)
margin.table(mytable, c(1,2))

#Created full modle
sat.model = loglm(~AdviceType * Outcome * ReferralSource, data=mytable)
sat.model
#Carried out stepwise regression
step(sat.model, direction="backward") #stepwise elimination
margin.table(mytable, c(1,2,3))
fitted(sat.model)
resid(sat.model) # the standardised residuals

#Examining two-way interactions
mydata <- approvals
tbl1 = table(mydata$Outcome, mydata$AdviceType)
tbl1
chisq.test(tbl1)

tbl2 = table(mydata$ReferralSource, mydata$AdviceType)
tbl2
chisq.test(tbl2)

tbl3 = table(mydata$ReferralSource, mydata$Outcome)
tbl3
chisq.test(tbl3)

#Examining three-way interactions
major <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/major2009.csv")
mytable <- table(major$Outcome,major$AdviceType)
mytable
chisq.test(mytable)
other <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/others2009.csv")
mytable2 <- table(other$Outcome,other$AdviceType)
mytable2
chisq.test(mytable2)
f2f <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/f2f2009.csv")
mytable <- table(f2f$Outcome,f2f$ReferralSource)
mytable
chisq.test(mytable)
tele <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/tele2009.csv")
mytable2 <- table(tele$Outcome,tele$ReferralSource)
mytable2
chisq.test(mytable2)
approved <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/approved2009.csv")
mytable <- table(approved$Outcome,approved$ReferralSource)
mytable
chisq.test(mytable)
dead <- Read("~/Work/Study 2 - F2F vs Telephone/three way interaction/casedead2009.csv")
mytable2 <- table(dead$AdviceType,dead$ReferralSource)
mytable2
chisq.test(mytable2)
