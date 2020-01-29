#script utilizzato per testare la classificazione tramite SVM
setwd("~/Documents/ProgettiUniversità/ProgettoDT_ML /codice/R")

library(e1071)
library(pROC)
library(ggplot2)
library(ROSE)
library(caret)

#setwd("~/progetto")
dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)

#preprocessing

#shuffle
set.seed(42)
dataset = dataset[sample(nrow(dataset), nrow(dataset)), ]

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

dataset$y = factor(dataset$y)

#feature selection
selectd_features = c(7, 9, 12, 16)
selectd_features_names = c("housing", "contact", "duration", "poutcome")

train.rows<- createDataPartition(y= dataset$y, p=0.75, list = FALSE)
trainset<- dataset[train.rows,] # 70% data goes in here
trainset$y = factor(trainset$y)

table(trainset$y)

#undersampling con classe minoritaria y = 1, 5289 campioni
trainset <- ovun.sample(y ~ ., data = trainset, method = "under", N = 7934, seed = 1)$data
#trainset <- ovun.sample(y ~ ., data = trainset, method = "both", N = mean(c(29942, 3967)), seed = 1)$data
table(trainset$y)

testset <- dataset[-train.rows,]
testset$y = factor(testset$y)
#age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome
svm.model = svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + duration + campaign + pdays + previous + poutcome, 
                data=trainset,
                kernel='polynomial',
                cost=1)

#pesi per caso sbilanciato
#class.weights = c("0" = 0.117, "1" = 0.883)
print("Accuracy train")
svm.pred_train = predict(svm.model, trainset)
svm.table_train=table(trainset$y, svm.pred_train)
svm.table_train
sum(diag(svm.table_train))/sum(svm.table_train)

print("Accuracy test")
svm.pred = predict(svm.model, testset)
svm.table=table(testset$y, svm.pred)
svm.table
sum(diag(svm.table))/sum(svm.table)

roc_obj = roc(testset$y, as.numeric(svm.pred))
plot(ggroc(roc_obj))

#ggplot(data =  dataset, mapping = aes(x = svm.pred, y = testset$y)) +
  #geom_tile(aes(fill = value), colour = "white") +
  #geom_text(aes(label = sprintf("%1.0f",value)), vjust = 1) +
  #scale_fill_gradient(low = "white", high = "steelblue")

#tuning dei parametri
#tuning = tune.svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome, data =trainset ,kernel='radial',
#                 cost=c(0.1, 1,5,10,100,200))

