---
title: "Practical Machine Learning Course Project"
author: "N Casey"
date: "Sunday, July 26, 2015"
output: html_document
---
##Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

##Assignment
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 


##Setup
We load the training and testing sets from the downloaded CSV fies.
```{r}
library(caret)
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")```
```
##Cleaning and Splitting the Data
We clean up the training data by
- removing columns which were 90% full of NAs
- removing near zero predictor columns
- removing the first 7 columns which have identity data such as user-name

Then we split the training data: 60% for training, 40% for testing

```{r}
nasPerColumn<- apply(training,2,function(x) {sum(is.na(x))});
training <- training[,which(nasPerColumn <  nrow(training)*0.9)];  
nearZeroColumns <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, nearZeroColumns$nzv==FALSE]
training<-training[,7:ncol(training)]

inTrain <- createDataPartition(training$classe, p=0.6, list = FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]
```

##Generate model
We train a Random Forest model 
```{r}
model_rf <- train(classe ~ .,  method="rf", data=train)  
```
##Test Accuracy
We perform a prediction on the model with the test partition, and check the confusion matrix
```{r}
rf_predict<- predict(model_rf, test)
print(confusionMatrix(rf_predict, test$classe))
```
The Accuracy given is 0.9907  

#Tuning with Cross Validation
We try to tune the mode using cross validation with 10 folds.
```{r}
cvTrainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
cv_model_rf <- train(classe ~ ., method="rf",  data=train, trControl = cvTrainControl)
```
We perform a prediction on the new model with the test partition, and check the confusion matrix
```{r}
rf_predict_cv<- predict(cv_model_rf, test)
print(confusionMatrix(rf_predict_cv, test$classe))
```

The Accuracy given for the new model is 0.9913, so it is better. 

##Order of importance of the predictors
We check the order of importance of the predictors. We see that roll_belt is the most important.
```{r}
varImport = varImp(cv_model_rf$finalModel)
varImport$var<-rownames(varImport)
varImport = as.data.frame(varImport[with(varImport, order(varImport$Overall, decreasing=TRUE)), ])
rownames(varImport) <- NULL
print(varImport)
```
## Generate files predicting 20 test cases
```{r}
predicttest <- predict(cv_model_rf, testing)
predicttest


answers <- as.vector(predicttest)

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)
```