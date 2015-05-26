###  
Predict Search Results Relevance
===================================

This is an entry to [Kaggle](http://www.kaggle.com/)'s
[Search Results Relevance](https://www.kaggle.com/c/crowdflower-search-relevance ompetition.


Problem description
-----------------

Quoting from Kaggle's [description page](https://www.kaggle.com/c/crowdflower-search-relevance):

Given the queries and resulting product descriptions from leading eCommerce sites, this competition asks to evaluate the accuracy of their search algorithms.

To evaluate search relevancy,a total of 261 search terms were generated, and CrowdFlower put together a list of products and their corresponding search terms. Each rater in the crowd was asked to give a product search term a score of 1, 2, 3, 4, with 4 indicating the item completely satisfies the search query, and 1 indicating the item doesn't match the search term.

The goal of this competition is to create an open-source model that can be used to measure the relevance of search results. The competition submission are scored based on the quadratic weighted kappa, which measures the agreement between the scores assigned by the human rater and the predicted scores.


Plan of Attack
------------







Data Overview
-------------
Use the readr library to read in the train and test sets 
```{r}
library(readr)
train = read_csv("../input/train.csv")
test  = read_csv("../input/test.csv")
nrow(train)
nrow(test)
names(train)
names(test)
```
There are 10158 observations in the train set, and 22513 in the test set. The train set has 6 varibles: id, query, product_title, product_description, median_relevance, relevance_variance. The test set has 4 variables: id, query, product_title, product_description.

```{r}
length(unique(train$query))		[1] 261
length(setdiff(unique(train$query), unique(test$query)))		[1] 0
length(intersect(unique(train$product_title), unique(test$product_title)))		[1]1028
```
The problem description mentions that there are 261 search terms. And we can see that, all the queries in the training set are also in the test set. But only about 1028 product titles are both in the train and test sets, which means the product titles are mostly different between train and test sets. 

Data Analysis 
-------------
```{r}
pd_manual = function(input){
  input$product_description = gsub("<.*?>", "", input$product_description)
  input$product_description = gsub("This translation tool is for your convenience only.*?Note: The accuracy and accessibility of the resulting translation is not guaranteed", "", input$product_description)
}
```
Manual adjustments: The problem description says that the product description field is raw and contains information that is irrelevant to the product. So I removed the HTML tags, and some other irrelevant information that I gathered from glancing through the raw data sets.   

```{r}
cleanCorpus = function(documents){
  corpus = Corpus(VectorSource(documents))
  corpus = tm_map(corpus, tolower) 
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stemDocument)
  return (corpus)
}
```
Text Mining: wrote a function to clean the corpus. There will be three clean corpuses, which are containers for query, product title, and product description after we pre-process the data: convert the text to lowercase, remove punctuation, remove English stopwords, stem the words. 

```{r}
DTM = function(clean_Cor,percentage,prefix){
  dtm = DocumentTermMatrix(clean_Cor)
  dtm = removeSparseTerms(dtm,percentage)
  dtm = Matrix(as.matrix(dtm),sparse=T)
  dtm = as.data.frame(as.matrix(dtm))
  colnames(dtm) = make.names(colnames(dtm))
  colnames(dtm) = paste(prefix,colnames(dtm),sep="")
  return (dtm)
}
```
Wrote another function to build a document term matrix from the clean corpus, called dtm, limit dtm to contain terms appearing in at least 1-percentage of documents, and store the result in a data frame, use the make.names function to make the variable names, and distinguish the variables by passing in a prefix with the paste function. 

```{r}
relevance = as.factor(train$median_relevance)
variance = train$relevance_variance
train$median_relevance = NULL
train$relevance_variance = NULL
combi=rbind(train,test)
combi = pd_manual(combi)
dtm_q = DTM(cleanCorpus(combi$query),0.999,"q-")
dtm_pt = DTM(cleanCorpus(combi$product_title),0.997,"pt-")
dtm_pd = DTM(cleanCorpus(combi$product_description),0.995,"pd-")
combi=cbind(dtm_q,dtm_pt,dtm_pd)
sparse_combi = Matrix(as.matrix(combi),sparse=T)
```
Use all the written functions to create three dataframes for the query, product title, product description. Combine all the columns into a single dataframe, but most of the terms are used infrequently, and there are a lot of terms, so create a sparse matrix instead. 

Modeling
-------------
```{r}
sparse_train = sparse_combi[1:10158,]
sparse_test = sparse_combi[10159:32671,]
model = svm(sparse_train,relevance, kernel="linear", cost=0.75)
tune_svm = tune(svm, relevance~., data=as.matrix(train), kernal="linear", ranges=list(gamma = 2^(-1:1), cost = 2^(0:2)))
svm_tune = tune_svm$best.model
summary(svm_tune)
```
Train a linear SVM using the sparse matrix of our selected features, for example, if the query and title/description have similar words then the relevance should be higher. Then use cross validation to pick the cost parameter, store the best model obtained in fit_svm_tune and use it for predictions on the sparse_test. 

```{r}
pred = predict(svm_tune,sparse_test)
Newsubmission = data.frame(id=test$id, prediction = pred)
write.csv(Newsubmission,"model.csv",row.names=F)
```
Results 
------------
```{r}
spl = sample.split(relevance, SplitRatio = 0.7)
Train_relevance = subset(relevance, spl==TRUE)
Test_relevance = subset(relevance, spl==FALSE)
Train_Train = subset(sparse_train, spl==TRUE)
Train_Test = subset(sparse_train, spl==FALSE)
Test_pred = predict(svm_tune,Train_Test)
ScoreQuadraticWeightedKappa(Test_relevance,Test_pred,1,4)  
```
My local sample kappa score is 0.5641 and the score on LeaderBoard is [0.5673](. 

Things that didnt work  
------------
```{r}
percentMatching = function(qString, dString){
  corpus = Corpus(VectorSource(qString))  # Create corpus
  corpus = tm_map(corpus, tolower) # Pre-process data
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stemDocument)
  processedQuery = data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
  processedQueryTerms = apply(processedQuery, 1, function(myString){return(strsplit(myString," "))})
  
  count=rep(0,length(processedQueryTerms))
  for( j in 1:length(processedQueryTerms)){
    terms = processedQueryTerms[[j]]$text
    for( i in 1:length(terms)){
      count[j]= count[j] + ifelse(grepl(terms[i],dString[j],ignore.case=FALSE),1,0)
    }
    count[j]=count[j]/length(terms)
  }
  
  return(count)
  
}
pd_matching = percentMatching(combi$query,combi$product_description)
pt_matching = percentMatching(combi$query,combi$product_title)

#-------------------------------------------------
# Use Radial Kernels 
RB_model = svm(sparse_train,Train_relevance, kernel="radial", gamma = 0.0003783579, cost= 10, cachesize = 200,shrinking = T,probability = F,degree = 3,class.weights = NULL)
summary(RB_model)
pred = predict(RB_model,sparse_test)
ScoreQuadraticWeightedKappa(Test_relevance,pred,1,4)  

#-----------------cross validation on SVM
cvtrain = combi[1:10158,]
spl = sample.split(relevance, SplitRatio = 0.1)
cvt = subset(cvtrain, spl==TRUE)
cvt$relevance = subset(relevance, spl==TRUE)

# With ScoreQuadraticWeightedKappa
asdf = function(A,B){
  return(1-ScoreQuadraticWeightedKappa(A,B,1,4))
}
tuneParam =tune.control(cross = 3, error.fun = asdf)
set.seed(1)
tune.out = tune(svm,relevance~.,data=cvt,kernel = "radial", ranges = list(cost=c(0.5,1,5)), tunecontrol = tuneParam)
```
# ------------------------------------------------------------------------------------
# CART model
library(caret)
#install.packages("e1071")
library(e1071)
# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 20 )
cpGrid = expand.grid( .cp = seq(0.00001,0.00009,0.00001)) 
train( train_relevance ~ ., data=new_train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# The final value used for the model was cp = 8e-05.
Tree = rpart(train_relevance ~ ., data=new_train, method="class", cp=0.00008)
prp(Tree)
PredictCV = predict(Tree, newdata = new_test, type = "class")
Newsubmission = data.frame(id=test$id, prediction = PredictCV)
write.csv(Newsubmission,"Search_Result_CV.csv", row.names=FALSE)
# Random Forest
library(randomForest)
Forest = randomForest(train_relevance ~ ., data=new_train, ntree=1000, mtry=37)
# Make predictions testing set
PredictForest = predict(Forest, newdata = new_test)
Newsubmission = data.frame(id=test$id, prediction = PredictForest)
write.csv(Newsubmission,"Search_Result_CV.csv", row.names=FALSE)
#----------------library(glmnet)
fit = glmnet(new_train,train_relevance)
pred = predict(fit, sparse_test)
cv = cv.glmnet(sparse_train,relevance,nfolds = 3)
pred = predict(fit, sparse_test, type = "response", s= cv$lambda.min)
```
