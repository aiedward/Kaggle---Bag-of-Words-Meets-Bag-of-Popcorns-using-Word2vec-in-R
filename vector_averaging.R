library(Rcpp)
library(RcppArmadillo)
library(tm)
library(randomForest)
library(rpart)
library(rword2vec)

labeled=as.data.frame(read.table("labeledTrainData.tsv",stringsAsFactors = F,header=T))
test=as.data.frame(read.table("testData.tsv",stringsAsFactors = F,header=T))

train=c(labeled$review,test$review)

### to remove punctuations and to make reviews lowercase 
train=gsub("<br />","",train)
train=tolower(train)
train=gsub("[[:punct:]]", "", train)

labeled$review=train[1:nrow(labeled)]
test$review=train[(nrow(labeled)+1):(length(train))]

### VECTOR AVERAGING
sourceCpp("converter.cpp")
vocab=as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))
colnames(vocab)[1]="word"
print(str(vocab))

### From Words to Paragraphs, Attempt 1:Vector Averaging 
d=NULL
d=strsplit(labeled$review," ")
for(i in 1:nrow(labeled))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}
#training_data=as.data.frame(matrix(rep(0,nrow(labeled)*(ncol(vocab)-1)),nrow = nrow(labeled)))
##
#for(i in 1:nrow(labeled))
#{
#  ### to strip leading and trailing white spaces
#
#  count=0
#  for(j in 1:length(d[[i]]))
#  {
#    if(!(d[[i]][j] %in% stopwords()))
#    {  
#      if(d[[i]][j] %in% vocab$word )
#        {
#          count=count+1
#          index=subset(vocab,word==d[[i]][j])
#          training_data[i,]=training_data[i,]+index[2:length(index)]
#        }
#    }

#  }
#  training_data[i,]=training_data[i,]/count
#}

training_data=get_average_vectors(d,matrix(as.numeric(unlist(vocab[,2:ncol(vocab)])),nrow=nrow(vocab)),vocab$word,stopwords())
training_data=as.data.frame(training_data)
training_data$sentiment=labeled$sentiment
write.csv(training_data,"train_vector_averaging.csv",row.names = F)

d=NULL
d=strsplit(test$review," ")
for(i in 1:nrow(test))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}
#test_data=as.data.frame(matrix(rep(0,nrow(test)*num_clusters),nrow = nrow(test)))

#for(i in 1:nrow(test))
#{
### to strip leading and trailing white spaces
# d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
#  count=0
#  for(j in 1:length(d[[i]]))
#  {
#   if(!(d[[i]][j] %in% stopwords()))
#    {  
#      if(d[[i]][j] %in% vocab$word )
#      {
#        count=count+1
#        index=subset(vocab,word==d[[i]][j])
#       test_data[i,]=test_data[i,]+index[2:length(index)]
#      }
#    }
#  }
#  test_data[i,]=test_data[i,]/count
#}
test_data=get_average_vectors(d,matrix(as.numeric(unlist(vocab[,2:ncol(vocab)])),nrow=nrow(vocab)),vocab$word,stopwords())
test_data=as.data.frame(test_data)
write.csv(test_data,"test_vector_averaging.csv",row.names = F)


#######################################################################################################################################

### train data
training_data=read.csv("train_vector_averaging.csv")
test_data=read.csv("test_vector_averaging.csv")

## Decision trees
#tree_model=rpart(as.factor(sentiment)~.,data=training_data[1:(round(0.8*nrow(training_data))),])
#pred=predict(tree_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
#table(pred[,2]>0.5,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])
#pred_test=predict(tree_model,test_data)

## randomForest
ml_model=randomForest(as.factor(sentiment)~.,data=training_data[1:(round(0.8*nrow(training_data))),],importance=T,ntree=200)
pred=predict(ml_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
table(pred,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])
pred_test=predict(ml_model,test_data)


submit=data.frame(id=test$id,sentiment=pred_test)

write.csv(submit,"rword2vec_va_forest_solution.csv",row.names = F)
