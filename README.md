## Kaggle Bag of Words Meets Bag of Popcorns using Word2vec in R
Solution to Bag of words meets bag of popcorns using word2vec in R

To get competion data, click [here](https://www.kaggle.com/c/word2vec-nlp-tutorial)

####Packages needed:
* [rword2vec](https://github.com/mukul13/rword2vec)
* Rcpp and RcppArmadillo
* rpart and randomForest
* tm

####Code Explanation:
 * Word vectors are obtained by using rword2vec package. 
 * Binary output file is converted into text file for further processing.
 * To create training dataset for sentiment classification for reviews using word vectors obtained above, two popular methods
 can be used:
  1. Vector Averaging 
  2. Clustering
 * In first methods, we have to do vector averaging for each row of labeled and test dataset. There are many ways to do this 
  but I have done this part using Rcpp and RcppArmadillo (R interface to C++) to avoid these compute intensive operations.
 * In clustering,we are doing bag of centroids instead of bag of words. This part is also done using Rcpp and RcppArmadillo 
  to optimize speed.
 * Finally, classsification is done using random forest.

####Note:
I'd recommend to read [this](https://www.kaggle.com/c/word2vec-nlp-tutorial/details/part-1-for-beginners-bag-of-words) python tutorial series first for better understanding of vector averaging and clustering. 
   
####Test dataset results: 
   
   ![image](https://github.com/mukul13/Kaggle---Bag-of-Words-Meets-Bag-of-Popcorns-using-Word2vec-in-R/blob/master/average%20vectors.png)
   
   <b>Classification using Vector Averaging</b>
   
   
   ![image2](https://github.com/mukul13/Kaggle---Bag-of-Words-Meets-Bag-of-Popcorns-using-Word2vec-in-R/blob/master/bag%20of%20centroids.png)
   
   <b>Classification using Clustering</b>
   
   
