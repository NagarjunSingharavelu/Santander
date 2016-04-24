
# load the library
options( java.parameters = "-Xmx6g") # for chi-square evaluation 
library(FSelector)
library(nnet)
library(ROCR)

# load the data
trainDataset = read.csv("https://web.njit.edu/~ts336/train.csv")
testDataset = read.csv("https://web.njit.edu/~ts336/test.csv")


# Feature Selection using Chi-Square Method

n <- names(trainDataset)
f <- as.formula(paste("TARGET ~", paste(n[!n %in% "TARGET"], collapse = " + ")))
c1 = chi.squared(f, trainDataset)
c1[c1 == 0] <- NA

fselect = na.omit(c1)
n = rownames(fselect)
n[length(n)+1]="TARGET"
n2 = rownames(fselect)

filterData <- subset(trainDataset,select=n)
scaled = filterData

# Neural Networks with 50-50 cross validation

index <- sample(1:nrow(filterData),nrow(filterData)/2)
train_ <- scaled[index,]
test_ <- scaled[-index,]
train.nnet<-nnet(TARGET~.,train_,size=5,rang=0.07,Hess=FALSE,decay=15e-4,maxit=500)
test.nnet<-predict(train.nnet,test_)
check1 = table(test_$TARGET,test.nnet)
fitted.results <- ifelse(test.nnet > 0.5,1,0)
misClasificError <- mean(fitted.results != test_$TARGET)
print(paste('Accuracy in % : ',(1-misClasificError)*100))
compValue <- prediction(test.nnet, test_$TARGET)
aucValue <- performance(compValue, measure = "auc")
aucValue <- aucValue@y.values[[1]]
aucValue


# Neural Netowrks on testDataset


train_ <- subset(trainDataset,select=n)
test_ <- subset(testDataset,select=n2)

train.nnet<-nnet(TARGET~.,train_,size=5,rang=0.07,Hess=FALSE,decay=15e-4,maxit=500)
test.nnet<-predict(train.nnet,test_)
fitted.results = test.nnet
outVal=data.frame(ID=testDataset$ID,TARGET=fitted.results)
write.csv(outVal,"NN_Submission.csv")
