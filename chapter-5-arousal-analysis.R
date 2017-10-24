library(lessR)
library(lme4)
library(car)

mydata <- Read("C:\\Users\\Nicole\\Documents\\Work\\Chapter 4 Stuff\\Longform_Emo.csv")

#run full model
amdlfull = lmer(Arousal ~ AdviceType + Role + LabourType + InitiationReaction + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Valence), data=mydata)
summary(amdlfull)
Anova(amdlfull)
#run simple model
amdlnull = lmer(Arousal ~ 1 + (1|Speaker), data=mydata)
summary(amdlnull)
anova(amdlfull,amdlnull)
#start removing the least influential variables 
amdl7 = lmer(Arousal ~ AdviceType + LabourType + InitiationReaction + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Valence), data=mydata)
summary(amdl7)
Anova(amdl7)
anova(amdlfull,amdl7)
amdl6 = lmer(Arousal ~ AdviceType + LabourType + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Valence), data=mydata)
summary(amdl6)
anova(amdl7)
