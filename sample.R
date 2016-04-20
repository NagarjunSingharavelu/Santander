
#####################################################Package installation###################################################
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
#####################################################Package installation###################################################



#####################################################Modelling without feature selection#####################################
actualData=read.delim("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",sep = ",",header = F)
names(actualData)=c("age","workclass","fnlwgt","education","educationnum","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","nativecountry","y")
samVal=sample(32560,20000)
trtrain=actualData[samVal,]
trtest=actualData[-samVal,]
rf=randomForest(y~.,data=trtrain)
testPred=predict(rf,trtest)
confusionMatrix(table(testPred,trtest$y))
#####################################################Modelling without feature selection#####################################

#####################################################Modelling with feature selection########################################
featuresSelected=c("age","workclass","fnlwgt","education","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","y")
newrf=randomForest(y~age + workclass + fnlwgt + education + maritalstatus + occupation +
relationship + race + sex + capitalgain + capitalloss + hoursperweek,data=trtrain[,featuresSelected])
newtestPred=predict(newrf,trtest[,featuresSelected])
confusionMatrix(table(newtestPred,trtest$y))
#####################################################Modelling with feature selection########################################


#####################################################AUC calculation & ROC plotting##########################################
PredTrainOld = predict(rf,type="prob")[, 2]
ROCROld = prediction(PredTrainOld ,trtrain$y)
aucValueOld <- performance(ROCROld, measure = "auc")
aucValueOld <- aucValueOld@y.values[[1]]
aucValueOld

PredTrain = predict(newrf,type="prob")[, 2]
ROCR = prediction(PredTrain ,trtrain$y)
aucValue <- performance(ROCR, measure = "auc")
aucValue <- aucValue@y.values[[1]]
aucValue

plot(rocVal, main="ROC",col="red")
plot(rocValOld, add=TRUE,main="ROC",col="green")

#####################################################AUC calculation & ROC plotting##########################################


#####################################################Feature Selection based on FLM##########################################
trtrain_flm=trtrain
levels(trtrain_flm$y)<-c(1,2)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
trtrain_flm$y=as.numeric.factor(trtrain_flm$y)
step(lm(y~.,data=trtrain_flm),direction = "backward")
trtrain_flm=trtrain
levels(trtrain_flm$y)<-c(1,2)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
trtrain_flm$y=as.numeric.factor(trtrain_flm$y)
step(lm(y~.,data=trtrain_flm),direction = "backward")
#####################################################Feature Selection based on FLM###########################################
