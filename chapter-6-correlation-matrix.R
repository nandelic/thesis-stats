library(lessR)

#Carried out Pearson's correlation

#Reading data
mydata2 <- read.csv("C:/Users/Nicole/Documents/Work/Study 4 - Trust Survey/Analysis in R/sample86.csv")
attach(mydata2)

#Pearson's correlation between each variable
cr(Trust, AccExt)
cr(Trust, AccInt)
cr(Trust, DisclosePer)
cr(Trust, DiscloseFin)
cr(Trust, Attitudes)
cr(Trust, IVARecall)
cr(Trust, MHScore)
cr(DisclosePer, DiscloseFin)
cr(DiscloseFin, AccInt)
cr(DiscloseFin, AccExt)
cr(DiscloseFin, Attitudes)
cr(DiscloseFin, IVARecall)
cr(DiscloseFin, MHScore)
cr(DisclosePer, AccInt)
cr(DisclosePer, AccExt)
cr(DisclosePer, Attitudes)
cr(DisclosePer, IVARecall)
cr(DisclosePer, MHScore)
cr(AccInt, AccExt)
cr(AccInt, Attitudes)
cr(AccInt, IVARecall)
cr(AccInt, MHScore)
cr(AccExt, Attitudes)
cr(AccExt, IVARecall)
cr(AccExt, MHScore)
cr(IVARecall, Attitudes)
cr(Attitudes, MHScore)
cr(IVARecall, MHScore)

#Carried out chi-square test on advice type and outcome
mytable <- table(AdviceMode,Outcome)
chisq.test(mytable)
