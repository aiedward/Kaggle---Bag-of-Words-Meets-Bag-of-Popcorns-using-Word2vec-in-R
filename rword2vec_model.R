library(Rcpp)
library(RcppArmadillo)
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
