

#Load Packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)
pacman::p_load(caret, caTools,knitr,car,ROCR,IRdisplay, e1071, earth)

#Set the directory
setwd("C:\\Dino_C\\Datasets")

#Load data
cc_data = fread("creditcard.csv", sep = ",")

#How is the data
head(cc_data,4)
dim(cc_data)
str(cc_data)
summary(cc_data)

#almost 50% of variables have either +ve or -ve relation in determining the anamoly
corrplot::corrplot(cor(cc_data), method = "number", type = 'lower')


#How is Amount distributed
hist(cc_data$Amount)

#How is data across various Times
hist(cc_data$Time)

#Data is highly uneven
barplot(table(cc_data$Class),col = 'darkgreen')


#Split the data
#pacman::p_load(ROSE)
set.seed(1000)
split = sample.split(cc_data$Class, SplitRatio = 0.7)
trainset = subset(cc_data, split == TRUE)
testset = subset(cc_data, split == FALSE)
table(trainset$Class); table(testset$Class)

#Models
cc_model1 = glm(Class ~ . , family = binomial, data = trainset)


#Model Evaluation
summary(cc_model1)
anova(cc_model1)
probTrainset = predict(cc_model1, type = 'response')
thresholdTrainset = sum(trainset$Class == 1 )/length(trainset$Class)
predictTrainset = ifelse(probTrainset>thresholdTrainset, 1, 0)
#table(trainset$Class, predictTrainset)
AccuracyTrain = mean(predictTrainset == trainset$Class)
print(paste('Trainset Accuracy = ',  round(AccuracyTrain *100, 2), '%' ))

probTestset = predict(cc_model1, newdata = testset, type = 'response')
predictTestset = ifelse(probTestset>thresholdTrainset, 1, 0)
#table(testset$Class, predictTestset)
AccuracyTest = mean(testset$Class == predictTestset)
print(paste('Testset Accuracy = ',  round(AccuracyTest *100, 2), '%' ))


# confusion matrix
confusionMatrix(data = as.factor(predictTrainset), reference = as.factor(trainset$Class))
confusionMatrix(data = as.factor(predictTestset), reference = as.factor(testset$Class))

# more detailed importance values
imp = as.data.frame(varImp(cc_model1))
imp = data.frame(overall = imp$Overall, names = rownames(imp))
imp[order(imp$overall,decreasing = T),]
barplot(sort(imp$overall, decreasing = TRUE))


#Model2 with important variables 
cc_model2 = glm(Class ~ +V1 +V4 +V6 +V8 +V10 +V13 +V14 +V21 +V22 +V20 +V27 +V28 +Amount, family = binomial, data = trainset)


#Model Evaluation
summary(cc_model2)
probTrainset = predict(cc_model2, type = 'response')
thresholdTrainset = sum(trainset$Class == 1 )/length(trainset$Class)
predictTrainset = ifelse(probTrainset>thresholdTrainset, 1, 0)
#table(trainset$Class, predictTrainset)
AccuracyTrain = mean(predictTrainset == trainset$Class)
print(paste('Trainset Accuracy = ',  round(AccuracyTrain *100, 2), '%' ))

probTestset = predict(cc_model2, newdata = testset, type = 'response')
predictTestset = ifelse(probTestset>thresholdTrainset, 1, 0)
#table(testset$Class, predictTestset)
AccuracyTest = mean(testset$Class == predictTestset)
print(paste('Testset Accuracy = ',  round(AccuracyTest *100, 2), '%' ))


# confusion matrix
confusionMatrix(data = as.factor(predictTrainset), reference = as.factor(trainset$Class))
confusionMatrix(data = as.factor(predictTestset), reference = as.factor(testset$Class))

?confusionMatrix

#lift chart
detec_anam = prediction(probTrainset, trainset$Class)
perf = performance( detec_anam, "lift", "rpp" )
plot(perf, main="lift curve", xlab = 'Proportion of Transactions (sorted prob)')



#Model3 with balanced data
# load ROSE pakage for sample correction (over + under)
pacman::p_load(ROSE)
cc_data_ovun = ovun.sample(Class~., data = cc_data, N=nrow(cc_data), p = 0.5, seed = 1, method = "both")$data
head(cc_data_ovun,4)
table(cc_data_ovun$Class)

#Split the data
split = sample.split(cc_data_ovun$Class, SplitRatio = 0.7)
trainset = subset(cc_data_ovun, split == TRUE)
testset = subset(cc_data_ovun, split == FALSE)


cc_model3 = glm(Class ~ ., family = binomial, data = trainset)


#Model Evaluation
summary(cc_model3)
probTrainset = predict(cc_model2, type = 'response')
thresholdTrainset = sum(trainset$Class == 1 )/length(trainset$Class)
predictTrainset = ifelse(probTrainset>thresholdTrainset, 1, 0)
#table(trainset$Class, predictTrainset)
AccuracyTrain = mean(predictTrainset == trainset$Class)
print(paste('Trainset Accuracy = ',  round(AccuracyTrain *100, 2), '%' ))

probTestset = predict(cc_model2, newdata = testset, type = 'response')
predictTestset = ifelse(probTestset>thresholdTrainset, 1, 0)
#table(testset$Class, predictTestset)
AccuracyTest = mean(testset$Class == predictTestset)
print(paste('Testset Accuracy = ',  round(AccuracyTest *100, 2), '%' ))


# confusion matrix
confusionMatrix(data = as.factor(predictTrainset), reference = as.factor(trainset$Class))
confusionMatrix(data = as.factor(predictTestset), reference = as.factor(testset$Class))

?confusionMatrix

#lift chart
detec_anam = prediction(probTrainset, trainset$Class)
perf = performance( detec_anam, "lift", "rpp" )
plot(perf, main="lift curve", xlab = 'Proportion of Transactions (sorted prob)')



