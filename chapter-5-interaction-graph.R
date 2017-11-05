library(lessR)
library(lme4)
library(car)
library(lmerTest)
library(plyr)
library(ggplot2)
library(dplyr)
library(grid)

mydata <- read.csv("C:/Users/Nicole/Documents/Work/Study 3 - Emotion Recognition/study3_sample.csv")
#Change name of the InitiationReaction variable
revalue(InitiationReaction, c("Initiation"="Initiation", "Reaction"="Continuation"))

#Run model with interaction
xmdlx = lmer(Arousal ~ AdviceType + LabourType + InitiationContinuation + AdviceType*TemporalSequence + (1|Recording) + (1|Turn) + (1|Speaker) + (1|Valence), data=mydata, REML=FALSE)
summary(xmdlx)

#Creating graphs for Arousal and valence over time
g3 <- ggplot(mydata, aes(x=TemporalSequence, y=Arousal, group=AdviceType, colour=AdviceType)) + geom_line(stat="summary", fun.y="mean")+ labs(title= "Interaction between Temporal Sequence and Advice Type on Arousal")
g3
g5 <- ggplot(mydata, aes(x=TemporalSequence, y=Valence, group=AdviceType, colour=AdviceType))+ geom_line(stat="summary", fun.y="mean")+ labs(title= "Interaction between Temporal Sequence and Advice Type on Valence")
g5

#Creating graphs for functional and emotional data over time
f2fdata <- subset(mydata, AdviceType == "F2F", select=LabourType:TemporalSequence)
g1 <- ggplot(data=f2fdata, aes(x=TemporalSequence, group=LabourType, colour=LabourType)) + geom_line(aes(fill=..count..), stat="bin", binwidth=1) + labs(title= "Temporal sequence and Type of speech in Face to face advice",colour="Type of speech")
g1
teledata <- subset(mydata, AdviceType == "Telephone", select=LabourType:TemporalSequence)
g2 <- ggplot(data=teledata, aes(x=TemporalSequence, group=LabourType, colour=LabourType)) + geom_line(aes(fill=..count..), stat="bin", binwidth=1) + labs(title= "Temporal sequence and Type of speech in Telephone advice",colour="Type of speech")
g2

#Plotting the Telephone arousal data on top of the type of speech graph
teledata$Arousal2 <- teledata$Arousal*500
g4 <- ggplot(data=teledata, aes(x=TemporalSequence)) + geom_line(aes(fill=..count.., group=LabourType, colour=LabourType), stat="bin", binwidth=1) + labs(title= "Temporal sequence and Type of speech in Telephone advice",colour="Type of speech") + geom_line(data=teledata,aes(x=TemporalSequence,y=Arousal2),linetype="dashed", stat="summary",fun.y="mean")
g4


