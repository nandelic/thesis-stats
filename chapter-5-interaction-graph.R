library(lessR)
library(lme4)
library(car)
library(lmerTest)

mydata <- Read("C:\\Users\\Nicole\\Documents\\Work\\Chapter 4 Stuff\\Longform_Emo.csv")
#Change name of the InitiationReaction variable
revalue(InitiationReaction, c("Initiation"="Initiation", "Reaction"="Continuation"))
xmdlx = lmer(Arousal ~ AdviceType + LabourType + InitiationContinuation + AdviceType*TemporalSequence + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Valence), data=mydata, REML=FALSE)
summary(xmdlx)
g3 <- ggplot(mydata, aes(x=TemporalSequence, y=Arousal, group=AdviceType, colour=AdviceType)) + geom_line(stat="summary", fun.y="mean")
g3