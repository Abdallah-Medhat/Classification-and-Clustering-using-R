#library
#install.packages("corrplot")
#install.packages("plyr")
#install.packages('caTools')
# install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('caret')
#install.packages('xgboost')
#install.packages("neuralnet")

library(plyr)
library(tidyverse)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(readxl)
library(xgboost)
library(neuralnet)
library(pROC)

# Read the Data
Data <- read.csv("Churn Dataset.csv",header=TRUE,stringsAsFactors = TRUE)
str(Data)
glimpse(Data)

#some changes in data
Data$MultipleLines[Data$MultipleLines == "No phone service"]<-'No'
Data$SeniorCitizen <- as.factor(mapvalues(Data$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

collect_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
Data$tenure <- sapply(Data$tenure,collect_tenure)
Data$tenure <- as.factor(Data$tenure)
Data$customerID <- NULL

#missing Data and drop the missing data
summary(is.na(Data))
sum(is.na(Data))
Data=na.omit(Data)

#Convert categorical variable,  scatterplot matrix and correlation matrix 
churn_data <- sapply(Data,unclass)
churn_df <- subset(churn_data, select = -Churn)
correlationMatrix <- cor(churn_df)
corrplot(correlationMatrix, main="Correlation Plot for Numerical Variables", 
         method="number")
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
print(highlyCorrelated)
dataset=as.data.frame(churn_data)
dataset$Churn=as.factor(dataset$Churn)

#other way to scatterplot matrix and correlation matrix
churn__data <- sapply(Data,is.numeric)
corr_matrix <- cor(Data[,churn__data])
corrplot(corr_matrix, main="Correlation Plot for Numerical Variables",
         method="number")

#function confusion
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

#Apply decision tree 
set.seed(42)
split = sample.split(Y =Data$Churn, SplitRatio = 0.8)
train_set = subset(Data, split == TRUE)
test_set = subset(Data, split == FALSE)
model <- rpart(Churn ~ ., data = train_set, method = "class")

#important features
importance <- varImp(model, scale=FALSE)
print(importance)

# Predicting the Test set results
y_pred = predict(model, newdata = test_set[-20], type = 'class')

# Making the Confusion Matrix and calculate accuracy
cm = table(test_set$Churn, y_pred)
accuracy=sum(diag(cm))/sum(cm)

#plot tree
rpart.plot(model,type = 5)

#ROC
roc(test_set$Churn,c(y_pred),plot=TRUE,direction="<",
    percent=TRUE,legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='decision tree')

#improve Decision Tree1
set.seed(43)
split_new = sample.split(Y =Data$Churn, SplitRatio = 0.8)
train_set_new = subset(Data, split_new == TRUE)
test_set_new = subset(Data, split_new == FALSE)
model_new <-rpart(Churn ~ ., data = train_set_new, method = "class", 
              parms=list(split = "gini"), minsplit = 10, minbucket=2)
y_pred_new = predict(model_new, newdata = test_set_new[-20], type = 'class')

# Making the Confusion Matrix and calculate accuracy
cm_new = table(test_set_new$Churn, y_pred_new)
accuracy_new=sum(diag(cm_new))/sum(cm_new)

#plot tree
rpart.plot(model_new,type=5)

#calculate best cp
printcp(model_new)
plotcp(model_new)
best_cp <- model_new$cptable[which.min(model_new$cptable[,"xerror"]),"CP"]

#pruned model
Model_new_pruned <- prune(model_new, cp =best_cp)
y_pred_new_pruned = predict(Model_new_pruned, newdata = test_set_new[-20],
                            type = 'class')

# Making the Confusion Matrix and calculate accuracy
cm_new_pruned = table(test_set_new$Churn, y_pred_new_pruned)
accuracy_new_pruned=sum(diag(cm_new_pruned))/sum(cm_new_pruned)

#plot tree
rpart.plot(Model_new_pruned,type = 5)

#Roc
roc(test_set_new$Churn,c(y_pred_new_pruned),plot=TRUE,direction="<",
    percent=TRUE,legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='decision tree improve1')

#improve2 Decision Tree
set.seed(44)
split_new2 = sample.split(Y =Data$Churn, SplitRatio = 0.8)
train_set_new2 = subset(Data, split_new2 == TRUE)
test_set_new2 = subset(Data, split_new2 == FALSE)
model_new2 <-rpart(Churn ~ ., data = train_set_new2, method = "class", 
                  parms=list(split = "entropay"))
y_pred_new2 = predict(model_new2, newdata = test_set_new2[-20], type = 'class')

# Making the Confusion Matrix and calculate accuracy
cm_new2 = table(test_set_new2$Churn, y_pred_new2)
accuracy_new2=sum(diag(cm_new2))/sum(cm_new2)

#plot tree
rpart.plot(model_new2,type = 5)

#calculate best cp
printcp(model_new2)
plotcp(model_new2)
best_cp2 <- model_new2$cptable[which.min(model_new2$cptable[,"xerror"]),"CP"]

#pruned model
Model_new_pruned2 <- prune(model_new2, cp =best_cp2)
y_pred_new_pruned2 = predict(Model_new_pruned2, newdata = test_set_new2[-20],
                            type = 'class')

# Making the Confusion Matrix and calculate accuracy
cm_new_pruned2 = table(test_set_new2$Churn, y_pred_new_pruned2)
accuracy_new_pruned2=sum(diag(cm_new_pruned2))/sum(cm_new_pruned2)

#plot tree
rpart.plot(Model_new_pruned2,type = 5)

#Roc
roc(test_set_new2$Churn,c(y_pred_new_pruned2),plot=TRUE,direction="<",
    percent=TRUE,legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='decision tree improve2')

#XGboost
set.seed(45)
split_xg = sample.split(Y =dataset$Churn, SplitRatio = 0.8)
train_set_xg = subset(dataset, split_xg == TRUE)
test_set_xg = subset(dataset, split_xg == FALSE)
xg_trcontrol = trainControl(method = "repeatedcv",number = 10, repeats = 3,
                            search = "grid")
xgbGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = c(10,20,5),
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)
xgb_model = train(as.matrix(train_set_xg[-20]),train_set_xg$Churn,
                  trControl = xg_trcontrol,tuneGrid = xgbGrid,method = "xgbTree")
xgb_model$bestTune
xgb_model
y_pred_xgb = predict(xgb_model, newdata = as.matrix(test_set_xg[-20]))
cm_xgb = table(test_set_xg[, 20], y_pred_xgb)
accuracy_xbg=sum(diag(cm_xgb))/sum(cm_xgb)

#Roc
roc(test_set_xg$Churn,c(y_pred_xgb),plot=TRUE,direction="<",
    percent=TRUE,legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='XGboost')

# multilayer perceptron 
# Random sampling
dataset_nn=dataset
dataset_nn <- lapply(dataset_nn, as.numeric)
dataset_nn=as.data.frame(dataset_nn)
samplesize = 0.60 * nrow(dataset_nn)
set.seed(80) 
index = sample(seq_len(nrow(dataset_nn)), size = samplesize )

# Create training and test set
datatrain_per = dataset_nn[ index, ]
datatest_per = dataset_nn[ -index, ]

#Scale data for neural network
max = apply(dataset_nn , 2 , max)
min = apply(dataset_nn, 2 , min)
scaled = as.data.frame(scale(dataset_nn, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(Churn ~ ., trainNN, hidden =5  , linear.output = F )
summary(NN)
plot(NN)
predict_testNN = compute(NN, testNN[-20])
NN_p <- predict_testNN$net.result
predict_testNN = (predict_testNN$net.result * (max(dataset_nn$Churn) - min(dataset_nn$Churn))) + min(dataset_nn$Churn)

# calculate values   
pred_NN <- ifelse(NN_p>0.5, 1, 0)
cm_NN<- table(pred_NN, testNN$Churn)
accuracy_NN=sum(diag(cm_NN))/sum(cm_NN)
confusionMatrix_NN=confusionMatrix(cm_NN)
draw_confusion_matrix(confusionMatrix_NN)

#ROC
roc(testNN$Churn,c(pred_NN),plot=TRUE,direction="<",percent=TRUE,
    legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='neural network')

#change in neural network
# fit neural network
set.seed(5)
NN_a = neuralnet(Churn ~ ., trainNN, hidden =3  , linear.output = F,
               act.fct = "tanh",stepmax = 1e+08)
summary(NN_a)
plot(NN_a)
predict_testNN_a = compute(NN_a, testNN[-20])
NN_pa <- predict_testNN_a$net.result
predict_testNN = (predict_testNN_a$net.result * (max(dataset_nn$Churn) - min(dataset_nn$Churn))) + min(dataset_nn$Churn)

# calculate values   
pred_NN_a <- ifelse(NN_pa>0.5, 1, 0)
cm_NN_a<- table(pred_NN_a, testNN$Churn)
accuracy_NN_a=sum(diag(cm_NN_a))/sum(cm_NN_a)
confusionMatrix_NN_a=confusionMatrix(cm_NN_a)
draw_confusion_matrix(confusionMatrix_NN_a)

#Roc
roc(testNN$Churn,c(pred_NN_a),plot=TRUE,direction="<",percent=TRUE,
    legacy.axes=TRUE,xlab="false positive",ylab='True positive',
    main='neural network active')
