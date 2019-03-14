library(caTools)
library(ROCR)
data = read.csv("customer.csv", header = TRUE)

data<- sapply(data, as.character)    #Removing the "NA" values as these are not computed by function
data[is.na(data)] <- " "

data=as.data.frame(data)
#                 #putting blank values in place of "NA" 
for (i in 1:11)   #we will start from a particular column (column from where we have to give it in the function,it wpuld be defined from which index we have to input columns)
  data[,i]<- scale(as.numeric(data[,i]))   
data         #scaled dataset (scaling only for the columns that are to be passed in the function)

split <- sample.split(data, SplitRatio = 0.70)

train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

#train
#test
#length(train)
#length(test)

model <- glm ( boolean~.,train,family="binomial")
summary(model)


      #predicting the demands for the month whose values are not given            
# prob1
# 
# 
# length(prob1)

test
prob1 <- predict(model,test,type="response")
table(ActualValue=test$boolean, PredictedValue=prob1>0.5)

res <- predict(model,train,type='response')

ROCRPred = prediction(res,train$boolean)
ROCRPref <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize= TRUE,print.cutoffs.at=seq(0.1,by=.1))


prob1 <- predict(model,test,type="response")
table(ActualValue=test$boolean, PredictedValue=prob1>0.8)


data2=read.csv("logisticoutput.csv", header = TRUE)
data2
data2<- sapply(data2, as.character)    #Removing the "NA" values as these are not computed by function
data2[is.na(data2)] <- " "

data2=as.data.frame(data2)
#                                 #putting blank values in place of "NA" 
for (i in 1:11)   #we will start from a particular column (column from where we have to give it in the function,it wpuld be defined from which index we have to input columns)
  data2[,i]<- scale(as.numeric(data2[,i]))   
data2     #scaled dataset (scaling only for the columns that are to be passed in the function)

split <- sample.split(data2, SplitRatio = 0)

test <- subset(data2, split == FALSE)
prob2 <- predict(model,test,type="response")
prob2
