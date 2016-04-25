# load the library
options( java.parameters = "-Xmx6g")
library(FSelector)
library(nnet)
library(ROCR)
library(randomForest)
# load the data
trainDataset = read.csv("https://web.njit.edu/~ts336/train.csv")
testDataset = read.csv("https://web.njit.edu/~ts336/test.csv")
n <- names(trainDataset)
f <- as.formula(paste("TARGET ~", paste(n[!n %in% "TARGET"], collapse = " + ")))
c1 = chi.squared(f, trainDataset)
c1[c1 == 0] <- NA

fselect = na.omit(c1)
n = rownames(fselect)
n[length(n)+1]="TARGET"
filterData <- subset(trainDataset,select=n)
scaled = filterData
index <- sample(1:nrow(filterData),nrow(filterData)/2)
train_ <- scaled[index,]
test_ <- scaled[-index,]


### Random Forest Train 
fit <- randomForest(TARGET ~ ., train_ )
summary(fit)

predicted= predict(fit,test_)

check1 = table(test_$TARGET,predicted)
fitted.results <- ifelse(predicted > 0.5,1,0)
misClasificError <- mean(fitted.results != test_$TARGET)
print(paste('Accuracy in % : ',(1-misClasificError)*100))
compValue <- prediction(predicted, test_$TARGET)
aucValue <- performance(compValue, measure = "auc")
aucValue <- aucValue@y.values[[1]]
aucValue

# ##################on testDataset


train_ <- subset(trainDataset,select=n)
test_ <- subset(testDataset,select=n2)



fit <- randomForest(TARGET ~ ., train_)
summary(fit)

predicted= predict(fit,test_)
#fitted.results <- ifelse(test.nnet > 0.5,1,0)
fitted.results = predicted


outVal=data.frame(ID=testDataset$ID,TARGET=fitted.results)
write.csv(outVal,"RF_Submission.csv")

