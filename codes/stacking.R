library(Rcpp)
library(RcppArmadillo)
library(tm)
library(randomForest)
library(rpart)

training_data=read.csv("train_bag_of_centroids.csv")
test_data=read.csv("test_bag_of_centroids.csv")
test=as.data.frame(read.table("testData.tsv",stringsAsFactors = F,header=T))

## Decision trees
tree_model1=rpart(as.factor(sentiment)~.,data=training_data[1:(round(0.4*nrow(training_data))),])
pred1=predict(tree_model1,training_data[(round(0.4*nrow(training_data))+1):round(0.8*nrow(training_data)),])
table(pred1[,2]>0.5,training_data$sentiment[(round(0.4*nrow(training_data))+1):round(0.8*nrow(training_data))])


tree_model2=rpart(as.factor(sentiment)~.,data=training_data[(round(0.4*nrow(training_data))+1):round(0.8*nrow(training_data)),])
pred2=predict(tree_model2,training_data[1:(round(0.4*nrow(training_data))),])
table(pred2[,2]>0.5,training_data$sentiment[1:(round(0.4*nrow(training_data)))])

tree_model=rpart(as.factor(sentiment)~.,data=training_data[1:round(0.8*nrow(training_data)),])
pred3=predict(tree_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
table(pred3[,2]>0.5,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])

pred_test=predict(tree_model,test_data)
pred_test=as.data.frame(pred_test)
colnames(pred_test)=c("tree_zero","tree_one")

pred=as.data.frame(rbind(pred2,pred1,pred3))
colnames(pred)=c("tree_zero","tree_one")

training_data$tree_one=pred$tree_one
training_data$tree_zero=pred$tree_zero

test_data$tree_one=pred_test$tree_one
test_data$tree_zero=pred_test$tree_zero

write.csv(training_data,"stacking_training.csv")
write.csv(test_data,"stacking_testing.csv")

####################################################################################

training_data=read.csv("stacking_training.csv")
test_data=read.csv("stacking_testing.csv")


## randomForest
ml_model=randomForest(as.factor(sentiment)~.,data=training_data[1:(round(0.8*nrow(training_data))),],importance=T,ntree=80)
pred=predict(ml_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
table(pred,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])
pred_test=predict(ml_model,test_data)


submit=data.frame(id=test$id,sentiment=pred_test)

write.csv(submit,"rword2vec_boc_stacking_solution.csv",row.names = F)
