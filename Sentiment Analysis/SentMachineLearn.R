require(RTextTools)
require(e1071)
# build dtm 
cl.matrix <- create_matrix(clinton.sent, language="english", 
                           removeStopwords=FALSE, removeNumbers=TRUE, 
                           stemWords=FALSE, weighting=tm::weightTfIdf)

# train the model 
cl.mat = as.matrix(cl.matrix) 
classifier = naiveBayes(cl.mat[1:3,], as.factor(cl.mat[1:3,2]) )

# test the validity 
predicted = predict(classifier, cl.mat[4,]); predicted 
table(clinton.sent[11:15, 2], predicted) 
recall_accuracy(tweets[11:15, 2], predicted)

# build the data to specify response variable, training set, testing set. 
cl.container = create_container(cl.matrix, as.numeric(as.factor(clinton.sent)), trainSize=1:2, testSize = 3:4, virgin=FALSE)
cl.models = train_models(cl.container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

# classify the testing models using the training set
dim(as.matrix(cl.models$maxent_results[, -1]))
is.matrix(cl.models$maxent_results[, -1])
is.vector(cl.models$maxent_results[, -1])
class(cl.models$maxent_results[, -1])
cl.results = classify_models(cl.container, cl.models)

# accuracy table 
table(as.numeric(as.factor(cl.matrix[4,])), results[,"FORESTS_LABEL"]) 
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"]) 
# recall accuracy 
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"]) 
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"]) 
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"]) 
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"]) 
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])


