library(lessR)
library(lme4)
library(car)

mydata <- Read("C:\\Users\\Nicole\\Documents\\Work\\Chapter 4 Stuff\\Longform_Emo.csv")

#run full model
vmdlfull = lmer(Valence ~ AdviceType + Role + LabourType + InitiationReaction + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Arousal), data=mydata)
summary(vmdlfull)
Anova(vmdlfull)
#run simple model
vmdlnull = lmer(Valence ~ 1 + (1|Speaker), data=mydata)
summary(vmdlnull)
anova(vmdlfull,vmdlnull)
anova(vmdlfull)
#start removing the least influential variables
vmdl7 = lmer(Valence ~ AdviceType + LabourType + InitiationReaction + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Arousal), data=mydata, REML=FALSE)
summary(vmdl7)
Anova(vmdl7)
anova(vmdlfull,vmdl7)
vmdl6 = lmer(Valence ~ AdviceType + LabourType + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Arousal), data=mydata)
summary(vmdl6)
anova(vmdl6,vmdl7)
anova(vmdl7)
