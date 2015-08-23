setwd("F:/my work/weibo/code")

library(readr)

train <- read_csv("../input/weibo_train_pre.csv")
test <- read_csv("../input/weibo_predict_pre.csv")

model1 = lm(forward~len1+len2+verb+noun,data=train)
pred_forward = predict(model1,newdata=test)

model2 = lm(comment~len1+len2+verb+noun, data=train)
pred_comment = predict(model2,newdata=test)

model3 = lm(like~len1+len2+verb+noun, data=train)
pred_like = predict(model3,newdata=test)

submission = data.frame(uid=test$uid,mid = test$mid)
submission$forward = pred_forward
submission$comment = pred_comment
submission$like = pred_like

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