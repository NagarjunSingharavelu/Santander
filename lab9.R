library(pROC)
data1 = read.csv("/Users/Sundu/Desktop/R/Work/train.csv")


rw = nrow(data1)
hf = rw/2

train = data1[1:hf,]
hf = hf+1
test = data1[hf:rw,]

model <- glm(TARGET ~.,family=binomial(link='logit'),data=train)
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$TARGET)
paste('Accuracy',1-misClasificError)

g = roc(TARGET~fitted.results,data=test)
plot(g)

pt = predict(model,newdata = train , type='response')
g = roc(TARGET~pt,data=train)
plot(g)