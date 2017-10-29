library(MASS)
library(aod)

#Carrying out analysis on all 86 participants
#Reading data
mydata2 <- read.csv("C:/Users/Nicole/Documents/Work/Study 4 - Trust Survey/Analysis in R/sample86.csv")

#Creating full model
modelfull2 = glm(Outcome ~ AdviceMode + SurveyMode + Advisor + Appointment + Trust + AccExt +AccInt + DiscloseFin + DisclosePer + Attitudes + MHScore + IVARecall, family=binomial, data=mydata2)
summary(modelfull2)

#Retrieving confidence intervals for full model
confint.default(modelfull2)

#Created model with only two effects
model1 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccInt + MHScore, family=binomial, data=mydata)
summary(model1)

#Added Attitudes
model2 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccInt + MHScore + Attitudes, family=binomial, data=mydata)
summary(model2)

#Compared the residual deviance between model 1 and 2
G2 = model1$deviance-model2$deviance; G2
1-pchisq(G2,df=1)

#Added External accountability
model3 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccInt + MHScore + Attitudes + AccExt, family=binomial, data=mydata)
summary(model3)

#Compared residual deviance for model 3 and full model/the previous model
G2 = model3$deviance-modelfull2$deviance; G2
1-pchisq(G2,df=1)
G2 = model2$deviance-model3$deviance; G2
1-pchisq(G2,df=1)

#Retrieving confidence intervals for model 3
confint.default(model3)