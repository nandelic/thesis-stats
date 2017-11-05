
#Carrying out simple base-R descriptive statistics
#Read data
mydata <- read.csv("C:/Users/Nicole/Documents/Work/Study 3 - Emotion Recognition/Longform_Emo3.csv")
attach(mydata)

#Descriptive and t-test statistics for Arousal
by(mydata$Arousal,mydata$AdviceType,mean)
by(mydata$Arousal,mydata$AdviceType,sd)
t.test(Arousal~AdviceType)

#Descriptive and t-test statistics for Valence
by(mydata$Valence,mydata$AdviceType,mean)
by(mydata$Valence,mydata$AdviceType,sd)
t.test(Valence~AdviceType)