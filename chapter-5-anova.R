mydata <- Read("C:/Users/Nicole/Documents/Work/Study 3 - Emotion Recognition/study3_sample.csv")
aov_lt_at <- aov(value ~ AdviceType*labourtype + Error(recording/labourtype), data=mydata)
summary(aov_lt_at)