

#Package for ROC Plotting
install.packages("ROCR")
library("ROCR")
#---------------------------------------------------------------------------------

################################Main Program######################################

#Data loading 
trainDataSet <- read.csv("https://web.njit.edu/~ts336/train.csv")

#Data Cleansing - Search for NA and is Present we wont take that column 
##Not present in our database
sapply(trainDataSet,function(x) sum(is.na(x)))

#Feature Selection is done below
filterData <- subset(trainDataSet,select=c('var3','var15','imp_op_var39_comer_ult3', 'imp_op_var40_ult1', 'ind_var1_0', 'ind_var8', 'ind_var26_0', 'ind_var30_0', 'ind_var30', 'ind_var40_0', 'num_var1_0','num_var5', 'num_var42', 'saldo_var1', 'saldo_var5', 'var36', 'delta_imp_reemb_var17_1y3','ind_var43_recib_ult1', 'num_ent_var16_ult1', 'num_meses_var5_ult3', 'num_meses_var8_ult3','num_meses_var39_vig_ult3', 'num_reemb_var17_ult1', 'num_var43_recib_ult1', 'var38', 'TARGET'))

#Split Dataset into 2 (First - Train; Second - Test)
midValue <- nrow(filterData)/2
firstHalfDS <- filterData[1:midValue,]
secondHalfDS <- filterData[(midValue+1):nrow(filterData),]

#Model Creation by Logistic Regression
model <- glm(TARGET~.,family=binomial(link='logit'),data=firstHalfDS)
summary(model)

#To Validate Feature selection-----------------------------------------------------
anova(model, test="Chisq")

#Prediction for Test data based on Model & Data Correction
fitted.results <- predict(model,newdata=subset(secondHalfDS,select=c(1:25)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Calculate Error Rate 
misClasificError <- mean(fitted.results != secondHalfDS$TARGET)
print(paste('Accuracy in % : ',(1-misClasificError)*100))

#To find the Area Under Curve------------------------------------------------------
predValue <- predict(model, newdata=subset(secondHalfDS,select=c(1:25)), type="response")
compValue <- prediction(predValue, secondHalfDS$TARGET)
perfValue <- performance(compValue, measure = "tpr", x.measure = "fpr")
plot(perfValue)
aucValue <- performance(compValue, measure = "auc")
aucValue <- aucValue@y.values[[1]]
aucValue

#To find the ROC - ROC for Test Data
rocPlotDataTest = roc(TARGET~fitted.results,data=secondHalfDS)
plot(rocPlotDataTest)

#To find the ROC - ROC for Train Data
predRocValue = predict(model,newdata = firstHalfDS , type='response')
rocPlotDataTrain = roc(TARGET~predRocValue,data=firstHalfDS)
plot(rocPlotDataTrain)

#For Future reference--------------------------------------------------------------
##install.packages('pscl')
##library(pscl)
##pR2(model)
#----------------------------------------------------------------------------------

################ To find k - fold cross validation ################
install.packages("caret")
library("caret")

# number=5 in the below comment represents the k value
train_control = trainControl(method="cv",number=5)
new.model = train(TARGET~.,data=firstHalfDS,trControl=train_control,method="glm")

cvpred = predict(new.model,newdata=secondHalfDS)
cvpred <- ifelse(cvpred > 0.5,1,0);
new.misClasificError <- mean(cvpred != secondHalfDS$TARGET)
print(paste('5-fold-Accuracy',1-new.misClasificError) )
