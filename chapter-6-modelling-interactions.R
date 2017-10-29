library(aod)

#Modelling interactions for the sample of 83 participants
#Reading data
mydata <- read.csv("C:/Users/Nicole/Documents/Work/Study 4 - Trust Survey/Analysis in R/sample83.csv")

#Running main effects model with each possible interaction separately
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + SurveyMode*MHScore, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + SurveyMode*IVARecall, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*AccInt, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*DisclosePer, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*Attitudes, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*MHScore, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*IVARecall, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccInt*DisclosePer, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccInt*Attitudes, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccInt*MHScore, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccInt*IVARecall, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + DisclosePer*Attitudes, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + DisclosePer*MHScore, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + DisclosePer*IVARecall, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + Attitudes*MHScore, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + Attitudes*IVARecall, family=binomial, data=mydata)
summary(model4)
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + MHScore*IVARecall, family=binomial, data=mydata)
summary(model4)

#Creating a main effects only model
model4 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall, family=binomial, data=mydata)

#Creating a model with two interactions
model5 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccExt*MHScore + AccInt*IVARecall, family=binomial, data=mydata)
summary(model5)

#Creating a model with only main effects and the AccInt*IVARecall interaction
model5 = glm(Outcome ~ SurveyMode + Advisor + Appointment + AccExt +AccInt + DisclosePer + Attitudes + MHScore + IVARecall + AccInt*IVARecall, family=binomial, data=mydata)

#Plotting the AccInt*IVARecall interaction
interaction.plot(mydata$AccInt,mydata$Outcome,log(mydata$IVARecall),type="l",xlab="Internal accountability",ylab="(Recall)",trace.label="Outcome")

#Retrieving confidence intervals for Model 5
confint.default(model5)