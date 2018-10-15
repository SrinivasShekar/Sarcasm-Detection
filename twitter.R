library (devtools)
library(dplyr)
#install_github("geoffjentry/twitteR")
library(twitteR)

setup_twitter_oauth('8o4cT2QQl1MDoREZvigMSc4gq',
                     'KPmZ4aFkwlG1KrVR6FriYoyOA2ex8FpijSv0CEpQ4bMDeH83Mj',
                     '986221635198832641-bJk6PAYXyPiOe94k6HEy4SE6mNfEnPw',
                     'oF9l7yvik6UbZsBWXKfd0S8Jm6u3u26szDakHhihlK5zW') 
'''
consumer_key <- '8o4cT2QQl1MDoREZvigMSc4gq'
consumer_secret <- 'KPmZ4aFkwlG1KrVR6FriYoyOA2ex8FpijSv0CEpQ4bMDeH83Mj'
access_token <- '986221635198832641-bJk6PAYXyPiOe94k6HEy4SE6mNfEnPw'
access_secret <- 'oF9l7yvik6UbZsBWXKfd0S8Jm6u3u26szDakHhihlK5zW'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
'''
#For a particular search term
tweets <- searchTwitter("#sarcasm", n=10000)# top 300 tweets that contain search term
tweets3<- searchTwitter("#sarcastic",n=10000)
tweets2 <- searchTwitter("#csk", n=10000) 
tweetsDF <- twListToDF(tweets)# more info about tweets.
tweetsDFN <- twListToDF(tweets2)
tweetsDF2 <- twListToDF(tweets3)
tweetsDF<- rbind(tweetsDF,tweetsDF2)

#################For a user
#userTimeline('sarcasm',n=10) # tweets from a user

#homeTimeline (n=15) # get tweets from home timeline

#mentions (n=15) # get your tweets that were retweeted

#favs <- userTimeline('sarcasm',n=100) # tweets a user has favorited

#tweetsDF <- twListToDF(favs)


TextPreprocessing_1 <- lapply(tweetsDF, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
  x = gsub(' +',' ',x) ## Remove extra whitespaces
  x= gsub("<..*","",x) 
})

TextPreprocessing_2 <- lapply(tweetsDFN, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
 x = gsub(' +',' ',x) ## Remove extra whitespaces
 x= gsub("<..*","",x) 
  
#  x = gsub('<\\S+', "", x)
  
})

tweetsDF<-as.data.frame(TextPreprocessing_1)
tweetsDFN<-as.data.frame(TextPreprocessing_2)

#tweetsDF <- tweetsDF[-c(8), ] 
tweetsDF<-subset(tweetsDF,language=='en')
tweetsDFN<-subset(tweetsDFN,language=='en')
tweetsDF<-subset(tweetsDF,text!='')
tweetsDFN<-subset(tweetsDFN,text!='')
#tweetsDF<-subset(tweetsDF,text!=gsub("<\\w+ *", "", text))
sarcasm<-read.csv("sarcasm.csv",sep=";",header=TRUE)
non_sarcasm<-read.csv("non_sarcasm.csv",sep=";")
library(dplyr)
#train_data<-read.csv("sarcasm_v2.csv",sep=",")
#train_data<-subset(train_data) %>% select(Quote.Text,Label)
#train_data$value<-ifelse(train_data$Label=="sarc",1,0)
#train_data <- train_data %>% select(Quote.Text,value)
#colnames(train_data)<-c("tweets","value")

#data<-rbind(sarcasm,non_sarcasm)
sarcasm<-sarcasm %>% select(tweets,value)
non_sarcasm<-non_sarcasm %>% select(tweets,value)
sarcasm<-distinct(sarcasm)
sarcasm<-distinct(sarcasm)
#sarcasm$value<-c(1)

#sarcasm$value<-1
#non_sarcasm$value<-0
#colnames(sarcasm)<-c("tweets","value")
#colnames(non_sarcasm)<-c("tweets","value")
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
#print(model)

'''tbl_list <- sapply(breast_cancer[-10], table, breast_cancer[ , 10])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)
'''
#validate_data<-train[5001:nrow(train),]
#validate_data<-as.data.frame(validate_data)
#colnames(validate_data)<-"value"
preds <- predict(model, newdata =test)

conf_matrix <- table(preds, as.factor(test$value))
conf_matrix
library(caret) 
confusionMatrix(conf_matrix)



#Saving the Datasets
#write.csv2(train, file = "train.csv")
#write.csv2(sarcasm, file = "sarcasm.csv")
#write.csv2(non_sarcasm, file = "non_sarcasm.csv")





####SVM
library(RTextTools)
dtMatrix <- create_matrix(overall["tweets"])

container <- create_container(dtMatrix, overall$value, trainSize=1:11, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)

predictionData <- list("My name is Srinivas","What is your name?","You are extremely awesome","You are nonsense","what are you doing")
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
#trace("create_matrix", edit=T)#--->line 42 change Acronym to acronym

predSize = length(predictionData)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)


results <- classify_model(predictionContainer, model)
results
