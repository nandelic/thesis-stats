library(MASS)
library(aod)

#Carrying out analysis on all participants
#Reading data
mydata2 <- read.csv("C:/Users/Nicole/Documents/Work/Study 4 - Trust Survey/Analysis in R/sample83.csv")

#Creating full model
modelfull2 = glm(Outcome ~ AdviceMode + SurveyMode + Advisor + Appointment + Trust + AccExt +AccInt + DiscloseFin + DisclosePer + Attitudes + MHScore + IVARecall, family=binomial, data=mydata2)
summary(modelfull2)

#Retrieving confidence intervals for full model
confint.default(modelfull2)

#Created model with only two effects
model1 = glm(Outcome ~ AccInt + MHScore, family=binomial, data=mydata)
summary(model1)

G2 = model1$deviance-modelfull2$deviance; G2
1-pchisq(G2,df=1)

#Added Ext acc
model2 = glm(Outcome ~ AccInt + MHScore + AccExt, family=binomial, data=mydata)
summary(model2)

#Compared the residual deviance between model 1 and 2
G2 = model1$deviance-model2$deviance; G2
1-pchisq(G2,df=1)

#Added advisor
model3 = glm(Outcome ~ AccInt + MHScore + AccExt + Advisor, family=binomial, data=mydata)
summary(model3)

G2 = model2$deviance-model3$deviance; G2
1-pchisq(G2,df=1)

#Added recall
model4 = glm(Outcome ~ AccInt + MHScore + AccExt + Advisor + IVARecall, family=binomial, data=mydata)
summary(model4)

G2 = model3$deviance-model4$deviance; G2
1-pchisq(G2,df=1)

#Added Attitudes
model5 = glm(Outcome ~ AccInt + MHScore + AccExt + Advisor + IVARecall + Attitudes, family=binomial, data=mydata)
summary(model5)

G2 = model4$deviance-model5$deviance; G2
1-pchisq(G2,df=1)

#added discloseper
model6 = glm(Outcome ~ AccInt + MHScore + AccExt + Advisor + IVARecall + Attitudes + DisclosePer, family=binomial, data=mydata)
summary(model6)

#added surveymode
model7 = glm(Outcome ~ AccInt + MHScore + AccExt + Advisor + IVARecall + Attitudes + DisclosePer + SurveyMode, family=binomial, data=mydata)
summary(model7)


G2 = model6$deviance-model7$deviance; G2
1-pchisq(G2,df=1)

G2 = model7$deviance-modelfull2$deviance; G2
1-pchisq(G2,df=1)
