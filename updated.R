sarcasm<-read.csv("sarcasm.csv",sep=";",header=TRUE)
non_sarcasm<-read.csv("non_sarcasm.csv",sep=";")
library(dplyr)
sarcasm<-sarcasm %>% select(tweets,value)
non_sarcasm<-non_sarcasm %>% select(tweets,value)
sarcasm<-distinct(sarcasm)
sarcasm<-distinct(sarcasm)
non_sarcasm<-non_sarcasm[1:1000,]
sarcasm<-sarcasm[1:1000,]
overall<-rbind(sarcasm,non_sarcasm)
overall<-overall[sample(nrow(overall), nrow(overall)), ]
library(caTools)

set.seed(101)           

split1=sample.split(overall$tweets,SplitRatio=2/3)

train=subset(overall,split1==TRUE)

test=subset(overall,split1==FALSE)



library(e1071)
model <- naiveBayes(as.factor(value) ~ ., data = train)
class(model)
summary(model)

preds <- predict(model, newdata =test)

conf_matrix <- table(preds, as.factor(test$value))
conf_matrix
library(caret) 
confusionMatrix(conf_matrix)
