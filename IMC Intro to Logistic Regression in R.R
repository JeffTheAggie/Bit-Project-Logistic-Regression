#Logistic Regression in R

#Load Dataset
library(dplyr)
chest = heart

#Looks at first 6 rows of dataset
head(chest)

#Summary Statistics
summary(chest)

#Names of the features
names(chest)

#Train-Test Split by 70%
library(caTools)
sample_size = sample.split(chest$target,SplitRatio=0.70)
train=subset(chest,sample_size==TRUE)
test=subset(chest,sample_size==FALSE)

#Logistic Regression Model
Diagnosis_fit=glm(target~sex+cp+thalach+oldpeak+ca,train,family=binomial())
summary(Diagnosis_fit)

#Creating the prediction variable
Diagnosis_pred = predict(Diagnosis_fit,test,type="response")
prediction = as.data.frame(Diagnosis_pred)
categorise=function(x){
  return(ifelse(x>0.5,1,0))
}

prediction = apply(prediction,2,categorise)
head(prediction,10)

#Creating the confusion matrix that will also give the accuracy level
library(caret)
confusionMatrix(as.factor(test$target),as.factor(prediction))

#K-fold Cross Validation to estimate out-of-sample performance
Diagnosis_control = trainControl(method = "cv", number = 10)
Diagnosis_model = train(target~., train, method = "glm", trControl = Diagnosis_control)
print(Diagnosis_model)