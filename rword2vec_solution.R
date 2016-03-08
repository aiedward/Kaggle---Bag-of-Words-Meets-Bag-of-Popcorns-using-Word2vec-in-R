library(Rcpp)
library(tm)
library(randomForest)
library(rpart)
library(rword2vec)
### library(devtools)
### install_github("mukul13/rword2vec")
### github link: https://github.com/mukul13/rword2vec

#setwd("/home/mukul/Desktop/Mukul Backup/Kaggle/bag of popcorns/")

### to load data
labeled=as.data.frame(read.table("labeledTrainData.tsv",stringsAsFactors = F,header=T))
unlabeled=as.data.frame(read.table("unlabeledTrainData.tsv",stringsAsFactors = F,header=T))
test=as.data.frame(read.table("testData.tsv",stringsAsFactors = F,header=T))

train=c(labeled$review,unlabeled$review,test$review)

### to remove punctuations and to make reviews lowercase 
train=gsub("<br />","",train)
train=tolower(train)
train=gsub("[[:punct:]]", "", train)

labeled$review=train[1:nrow(labeled)]
unlabeled$review=train[(nrow(labeled)+1):(nrow(labeled)+nrow(unlabeled))]
test$review=train[(nrow(labeled)+nrow(unlabeled)+1):(length(train))]

### to write reviews data
write(train,"reviews.txt")
train=NULL
unlabeled=NULL

### word2vec model (output_file must be binary as other rword2vec functions like distance etc use this file for computation)
### However To get txt file as output give txt file name to "output_file" and set parameter "binary"=0 
model=word2vec(train_file = "reviews.txt",output_file = "model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001)

### to convert binary model into txt format
bin_to_txt("model1.bin","model1text.txt")

### to explore model's results
ana1=word_analogy("model1.bin","man woman king")
ana2=word_analogy("model1.bin","paris france berlin")

dist1=distance("model1.bin","man",num = 10)
dist2=distance("model1.bin","queen",num = 10)
dist3=distance("model1.bin","awful",num = 10)


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

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

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

### BAG OF CENTROIDS
sourceCpp("converter.cpp")
vocab=as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))
colnames(vocab)[1]="word"
print(str(vocab))

### From Words to Paragraphs, Attempt 2: Clustering 

## Setting "k" (num_clusters) to be 1/10th of the vocabulary size, or an
## average of 10 words per cluster
num_clusters=floor(nrow(vocab)/10)
kmean_model=kmeans(vocab[,2:ncol(vocab)],centers = num_clusters,iter.max = 150)
write.csv(kmean_model$cluster, file="kmeans_clusters2.csv")
write.csv(kmean_model$centers, file="kmeans_centers2.csv")
kmean_model=NULL

### read cluster data
clusters=read.csv("kmeans_clusters2.csv")
clusters$word=vocab$word
colnames(clusters)=c("word_no","cluster","word")

### bag of centroid
d=NULL
d=strsplit(labeled$review," ")
for(i in 1:nrow(labeled))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}

#training_data=as.data.frame(matrix(rep(0,nrow(labeled)*num_clusters),nrow = nrow(labeled)))
#for(i in 1:nrow(labeled))
#{
#  ### to strip leading and trailing white spaces
#d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
#for(j in 1:length(d[[i]]))
#{
#  if(d[[i]][j] %in% clusters$word)
#  {
#    index=clusters$cluster[clusters$word==d[[i]][j]]
#    training_data[i,index]=training_data[i,index]+1
#  }
#}
#}
training_data=get_bag_of_centroids(d ,vocab$word,clusters$cluster,num_clusters)
training_data=as.data.frame(training_data)
training_data$sentiment=labeled$sentiment

write.csv(training_data,"train_bag_of_centroids.csv",row.names = F)

### test data
d=NULL
d=strsplit(test$review," ")
for(i in 1:nrow(test))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}
test_data=get_bag_of_centroids(d,vocab$word,clusters$cluster,num_clusters)
test_data=as.data.frame(test_data)

#test_data=as.data.frame(matrix(rep(0,nrow(test)*num_clusters),nrow = nrow(test)))

#for(i in 1:nrow(test))
#{
#  ### to strip leading and trailing white spaces
#  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
#  for(j in length(d[[i]]))
#  {
#   if(d[[i]][j] %in% clusters$word)
#    {
#      index=clusters$cluster[clusters$word==d[[i]][j]]
#      test_data[i,index]=test_data[i,index]+1
#    }
#  }
#}
write.csv(test_data,"test_bag_of_centroids.csv",row.names = F)

###################################################################################################################################

### train data
training_data=read.csv("train_bag_of_centroids.csv")
test_data=read.csv("test_bag_of_centroids.csv")

## Decision trees
#tree_model=rpart(as.factor(sentiment)~.,data=training_data[1:(round(0.8*nrow(training_data))),])
#pred=predict(tree_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
#table(pred[,2]>0.5,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])
#pred_test=predict(tree_model,test_data)

## randomForest
ml_model=randomForest(as.factor(sentiment)~.,data=training_data[1:(round(0.8*nrow(training_data))),],importance=T,ntree=50)
pred=predict(ml_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
table(pred,training_data$sentiment[(round(0.8*nrow(training_data))+1):nrow(training_data)])
pred_test=predict(ml_model,test_data)


submit=data.frame(id=test$id,sentiment=pred_test)

write.csv(submit,"rword2vec_boc_forest_solution.csv",row.names = F)
