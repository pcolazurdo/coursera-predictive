
# Load needed libraries
library(caret)
library(rpart)
library(randomForest)

#Remove DIV/0! from the source file
xx <- read.table("/Users/IBM_ADMIN/Dropbox/Personal/Courses/2015/Predictive/Course Project/pml-training.csv", header=TRUE, sep=",", quote="\"", na.strings = "NA", stringsAsFactors = FALSE)
yy <- read.table("/Users/IBM_ADMIN/Dropbox/Personal/Courses/2015/Predictive/Course Project/pml-testing.csv", header=TRUE, sep=",", quote="\"", na.strings = "NA", stringsAsFactors = FALSE)

#Set to 0 every NA value
xx[is.na(xx)] = 0.00 
yy[is.na(yy)] = 0.00 
# remove text columns
training <- xx[,-c(6,5,2)]
testing <- yy[,-c(6,5,2)]


#remove empty columns
drops <- c("kurtosis_yaw_belt", "skewness_yaw_belt", "amplitude_yaw_belt", "kurtosis_yaw_dumbbell", "skewness_yaw_dumbbell", "amplitude_yaw_dumbbell", "kurtosis_yaw_forearm", "skewness_yaw_forearm", "amplitude_yaw_forearm") 
fit <- training[,! names(training) %in% drops]
fit <- fit[-c(1,2,3,4)]
fit$classe <- as.factor(fit$classe)

test <- testing[,! names(testing) %in% drops]
test <- test[-c(1,2,3,4)]
#test$classe <- as.factor(test$classe)

inTrain <- createDataPartition(y=fit$classe, p=0.85, list=FALSE)
fitTraining = fit[inTrain,]
fitTesting = fit[-inTrain,]

# This was used to create a prediction model and verify it against a testing set ... I've used several differente models until I've found a table with very low error.


#First Try
#Run RPART to create Tree Model
rp <- rpart(classe ~ ., fitTraining)
pred <- predict(rp,fitTesting, type="class")
table(pred, fitTesting[,"classe"])

# Manually choose most representative variables on the decision tree to create a new dataset
trainPlot1 <- fitTraining[,names(fitTraining) %in% c("roll_belt","pitch_forearm","roll_forearm","yaw_belt","pitch_belt", "yaw_forearm" ,"classe")]
testPlot1 <- fitTesting[,names(fitTraining) %in% c("roll_belt","pitch_forearm","roll_forearm","yaw_belt","pitch_belt", "yaw_forearm" ,"classe")]



#Run a new decision tree with these new vars
rp <- rpart(classe ~ ., data=trainPlot1)
pred <- predict(rp, testPlot1, type="class")
table(pred, testPlot1[,"classe"])


#Another Try
rf <- randomForest(classe ~ ., data=trainPlot1, ntree=20, mtry=5, importance=TRUE)
testPlot1$pred <- predict(rf,testPlot1, type="response")
table(testPlot1$pred, testPlot1$classe)


#Best Try
inTrain <- createDataPartition(y=fit$classe, p=0.85, list=FALSE)
fitTraining = fit[inTrain,]
fitTesting = fit[-inTrain,]
rf <- randomForest(classe ~ ., data=fitTraining, ntree=100, mtry=15, importance=TRUE)
fitTesting$pred <- predict(rf,fitTesting, type="response")
table(fitTesting$pred, fitTesting$classe)


#So this work fines ---
#Retrain the algorithm with the 100% of training cases and run the algorithm against the testing set
inTrain <- createDataPartition(y=fit$classe, p=1, list=FALSE)
fitTraining = fit[inTrain,]
rf <- randomForest(classe ~ ., data=fitTraining, ntree=100, mtry=15, importance=TRUE)
test$pred <- predict(rf,test, type="response")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("c:\\temp\\problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(test$pred)


