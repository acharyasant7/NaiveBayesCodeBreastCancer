library(caret)

#reading the data

breastdata <- read.csv("E:/Bioinformatics/R/Breast Cancer/breastcancer.data")

#Changing the designation for Malignant and Benign Tumour
View(breastdata)
for (i in 1: nrow(breastdata))
{
  if (breastdata$X2.1[i] == 2)
    breastdata$X2.1[i] <- 'B'
  else
    breastdata$X2.1[i] <- 'M'
}

View(breastdata)
str(breastdata) 
dim(breastdata)
summary(breastdata)

#Partitioning data into Train and Test Set

ind <- createDataPartition(breastdata$X2.1, p=4/5, list = FALSE)
trainD <- breastdata[ind,]
testD <- breastdata[-ind,]

#Doing k-fold Cross Validation and Training the Data using Naive Bayes Algorithm

ControlParameters <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE)
parameterGrid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1))
model <- train(X2.1~., data = trainD, method = "nb", trControl = ControlParameters, tuneGrid= parameterGrid)
model

#Plotting the model
plot(model)

#Predicting for the Test Set Using the Model

pred <- predict(model, testD)
pred
#Comparision of Prediction with Actual

t <- table(pred=pred, actual= testD$X2.1)
t

#Plotting a ROC Curve for the Analysis

library(ROCR)
predvec <- ifelse(pred=="B", 1, 0)
realvec <- ifelse(testD$X2.1=="B", 1, 0)

pr <- prediction(predvec, realvec) #transform the input data into a standardized format.
prf <- performance(pr, "tpr", "fpr")
plot(prf)
