setwd("F:/my work/weibo/code")

setwd("D:/competitions/天池大数据竞赛/weibo/code")

library(readr)
library(randomForest)
library(rpart.plot)
library(caTools)
library(kknn)

train <- read_csv("../input/weibo_train_pre.csv")
test <- read_csv("../input/weibo_predict_pre.csv")

train$pub_date = as.Date(train$pub_date,format="%Y-%m-%d")
train$weekdays = as.factor(weekdays(train$pub_date))
#train$months = as.factor(months(train$pub_date))

test$pub_date = as.Date(test$pub_date,format="%Y-%m-%d")
test$weekdays = as.factor(weekdays(test$pub_date))
#test$months = as.factor(months(test$pub_date))

#train$upop_forward = tapply(train$forward,train$uid,sum)
#train$upop_comment = tapply(train$comment,train$uid,sum)
#train$upop_like = tapply(train$like,train$uid,sum)

train$log_forward = log(train$forward+1)
train$log_comment = log(train$comment+1)
train$log_like = log(train$like+1)

spl_forward = sample.split(new_train$forward, SplitRatio = 0.5)
spl_comment = sample.split(new_train$comment, SplitRatio = 0.5)
spl_like = sample.split(new_train$like, SplitRatio = 0.5)

train_a1 = subset(new_train, spl_forward == TRUE)
train_b1 = subset(new_train, spl_forward == FALSE)
train_a2 = subset(train, spl_comment == TRUE)
train_b2 = subset(train, spl_comment == FALSE)
train_a3 = subset(train, spl_like == TRUE)
train_b3 = subset(train, spl_like == FALSE)


#start user features
uid = data.frame(table(train$uid))
forward_mean = tapply(train$forward,train$uid,mean)
comment_mean = tapply(train$comment,train$uid,mean)
like_mean = tapply(train$like,train$uid,mean)
user_info = data.frame(uid =uid$Var1,num = uid$Freq,
					f_mean = forward_mean,
					c_mean = comment_mean,
					l_mean = like_mean)


train$uid = as.factor(train$uid)

new_train = merge(train, user_info, by="uid")

test = merge(test, user_info)

# end user features
submission = data.frame(uid=test$uid,mid = test$mid)
submission$forward = test$f_mean
submission$comment = test$c_mean
submission$like = test$l_mean

submission$forward = exp(pred_forward)-1
submission$comment = exp(pred_comment)-1
submission$like = exp(pred_like)-1

submission$forward[submission$forward<0] = 0
submission$forward = as.integer(submission$forward)

submission$comment[submission$comment<0] = 0
submission$comment = as.integer(submission$comment)

submission$like[submission$like<0] = 0
submission$like = as.integer(submission$like)

#write.csv(submission, file = "baseline.txt",row.names=FALSE,col.names=NULL)
con <- file("../submission.txt", "w")
line = 1
while(line <= nrow(test)) {
	row = submission[line,]
	str1 = paste(row$uid,sep="\t",row$mid)
	str2 = paste(paste(row$forward,sep=",",row$comment),sep=",",row$like)
	str = paste(str1,str2,sep="\t")
	writeLines(str,con)
	line = line + 1
	print (line)
}
close(con)