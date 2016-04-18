train <- read.csv("https://web.njit.edu/~ts336/train.csv")
sapply(train,function(x) sum(is.na(x)))
sapply(train, function(x) length(unique(x)))

install.packages("Amelia")
library("Amelia")
missmap(train, main = "Missing values vs Observed")
data <- subset(train,select=c(2:371))

nrow(train)/2
data.train <- data[0:38010, ]
data.test <- data[38011:76020, ]

model <- glm(TARGET ~.,family=binomial(link='logit'),data=data.train)
summary(model)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(data.test,select=c(1:369)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != data.test$TARGET)
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, data.test$TARGET)

install.packages("ROCR")
library("ROCR")
pr <- prediction(fitted.results, data.test$TARGET)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

g = roc(TARGET~fitted.results,data=data.test)
plot(g)

pt = predict(model,newdata = data.train , type='response')
g = roc(TARGET~pt,data=data.train)
plot(g)

############ k fold cross validation ###########

install.packages("cvTools")
library("cvTools")
install.packages("caret")
library("caret")

t.control = trainControl(method="cv",number=5) 
grid = expand.grid(.fL=c(0), .usekernel=c(FALSE))

model = train(TARGET~.,data=data1,trControl=t.control,method="nb",tuneGrid=grid)

print(model)

cvpred = predict(model,newdata=train, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0);
misClasificError <- mean(fitted.results != test$TARGET)
paste('Accuracy',1-misClasificError) 


glmFit <- glm(TARGET ~., data=data.test,family=gaussian(link="identity"))
library(boot)                         
k <- 5
kfCV <- cv.glm(data=data.test, glmfit=glmFit, K=k)


