#script utilizzato per testare la classificazione tramite decision tree
setwd("~/Documents/ProgettiUniversità/ProgettoDT_ML /codice/R")
library(DataExplorer)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(pROC)
library(party) 
library(ROSE)
library(caret)


dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)
dataset$y = factor(dataset$y)

#shuffle
set.seed(42)
dataset = dataset[sample(nrow(dataset), nrow(dataset)), ]

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

#feature selection
selectd_features = c(7, 9, 12, 16)
selectd_features_names = c("housing", "contact", "duration", "poutcome")

train.rows<- createDataPartition(y= dataset$y, p=0.75, list = FALSE)
trainset<- dataset[train.rows,] # 70% data goes in here
trainset$y = factor(trainset$y)
testset <- dataset[-train.rows,]
testset$y = factor(testset$y)

#undersampling con classe minoritaria y = 1, 5289 campioni
trainset <- ovun.sample(y ~ ., 
                        data = trainset, 
                        method = "under", 
                        N = table(trainset$y)['1'] * 2, 
                        seed = 1)$data
table(trainset$y)

prop = table(trainset$y)

weight<-c(rep(0, prop['0']), rep(1, prop['1']))

decisionTree = rpart(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome, 
                     data = trainset,
                     weights = weight,
                     method = "class")

printcp(decisionTree)
fancyRpartPlot(decisionTree)
summary(decisionTree)
testset$prediction = predict(decisionTree, testset, type = "class")
confusion.matrix = table(testset$y, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)

roc_obj = roc(testset$y, as.numeric(testset$prediction))
plot(ggroc(roc_obj))