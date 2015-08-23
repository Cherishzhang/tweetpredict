uid = data.frame(table(train$uid))
user_info = data.frame(uid = uid$Var1,
					f_mean = tapply(train$forward, train$uid, mean),
					c_mean = tapply(train$comment, train$uid, mean),
					l_mean = tapply(train$like, train$uid, mean))
user_info_med = data.frame(uid = uid$Var1,
					f_med = tapply(train$forward, train$uid, median),
					c_med = tapply(train$comment, train$uid, median),
					l_med = tapply(train$like, train$uid, median))

train = merge(train, user_info)
train = merge(train, user_info_med)

test = merge(test, user_info)
test = merge(test, user_info_med)

sqrt(sum((train$forward - train$f_med)^2)/nrow(train))
train$temp = 0
train[is.na(train$title),]$temp =train[is.na(train$title),]$f_mean/2
sqrt(sum((train$forward - train$f_med - train$temp)^2)/nrow(train))

test$temp=0
test[is.na(test$title),]$temp = test[is.na(test$title),]$f_mean/2

submission = data.frame(uid=test$uid,mid = test$mid)
submission$forward = test$f_med+test$temp
submission$comment = test$c_med
submission$like = test$l_med

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