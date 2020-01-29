#questo script viene eseguito dopo lo script 'corss_weighted.R', così da mantenere gli stessi fold.
#script che esegue la 10-fold cross validation utilizzando i 3 modelli finali eseguendo undersampling sul dataset.
#vengono calcolate le misure di accuracy, precision, sensitivity(recall), f-measure e AUC per ogni modello eseguito su ogni fold
#vengono inoltre visualizzate tutte le curve ROC dei modelli per ogni fold

setwd("~/Documents/ProgettiUniversità/ProgettoDT_ML /codice/R")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(e1071)
library(pROC)
library(caret)
library(ROSE)

#importazione del dataset
#dataset = read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)
#dataset$y = factor(dataset$y)
#dataset = dataset[sample(nrow(dataset)),]
#dataset$X = NULL

selectd_features = c(7, 9, 12, 16)
selectd_features_names = c("housing", "contact", "duration", "poutcome")

#Creazione di 10 fold
#folds <- cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)
#folds <- createFolds(factor(dataset$y), k = 10, list = FALSE)

precisioniTrueAlberi = c()
precisioniTrueSvm = c()
precisioniFalseAlberi = c()
precisioniFalseSvm = c()

sensitivitaTrueAlberi = c()
sensitivitaTrueSvm = c()
sensitivitaFalseAlberi = c()
sensitivitaFalseSvm = c()

fmeasureTrueAlberi = c()
fmeasureTrueaSvm = c()
fmeasureFalseAlberi = c()
fmeasureFalseSvm = c()

accuratezzaAlberi = c()
accuratezzaSvm = c()

areaAlberi = c()
areaSvm = c()

matriciAlberi = list()
matriciSvm = list()

nomi = c("fold1.pdf","fold2.pdf","fold3.pdf","fold4.pdf","fold5.pdf","fold6.pdf","fold7.pdf","fold8.pdf","fold9.pdf","fold10.pdf")

dt.table_1u = c()
dt.table_2u = c()
dt.table_3u = c()
dt.table_4u = c()

svm.table_1u = c()
svm.table_2u = c()
svm.table_3u = c()
svm.table_4u = c()

for(i in 1:10){
  #Creazione di trainset e testset utilizzando il fold i-esimo
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testset <- dataset[testIndexes, ]
  trainset <- dataset[-testIndexes, ]
  testset$y = factor(testset$y)
  trainset$y = factor(trainset$y)
  
  print(table(trainset$y))
  
  #undersampling con classe minoritaria y = 1, 5289 campioni
  trainset <- ovun.sample(y ~ .,
                          data = trainset,
                          method = "under", 
                          N = table(trainset$y)['1'] * 2, 
                          seed = 1)$data
  
  #trainset <- ovun.sample(y ~.,
  #                        data = trainset,
  #                        method = "both", 
  #                        N = mean(c(35930, 4760)), 
  #                        seed = 1)$data
  
  print(table(trainset$y))
  
  #decision tree
  decisionTree = rpart(y ~ duration + poutcome + contact + housing + loan + campaign + marital + education + age + month + job,
                       trainset, 
                       method = "class")
  dt_Prediction = predict(decisionTree, testset, type = "class")
  roc_dt = roc(testset$y, as.numeric(dt_Prediction))
  dt.table=table(testset$y, dt_Prediction)
  
  dt.table_1u = c(dt.table_1u, dt.table[1])
  dt.table_2u = c(dt.table_2u, dt.table[2])
  dt.table_3u = c(dt.table_3u, dt.table[3])
  dt.table_4u = c(dt.table_4u, dt.table[4])
  
  #calcolo misure decision tree
  accuratezzaAlberi[i] = dt.accuracy = sum(diag(dt.table))/sum(dt.table)
  dt.precisionTrue= dt.table[4]/(dt.table[4]+dt.table[3])
  dt.precisionFalse= dt.table[1]/(dt.table[1]+dt.table[2])
  dt.sensitivityTrue = dt.table[4]/(dt.table[4]+dt.table[2])
  dt.sensitivityFalse = dt.table[1]/(dt.table[1]+dt.table[3])
  precisioniTrueAlberi[i] =  dt.precisionTrue
  precisioniFalseAlberi[i] =  dt.precisionFalse
  sensitivitaTrueAlberi[i] = dt.sensitivityTrue
  sensitivitaFalseAlberi[i] = dt.sensitivityFalse
  fmeasureTrueAlberi = 2*((dt.precisionTrue*dt.sensitivityTrue)/(dt.precisionTrue+dt.sensitivityTrue))
  fmeasureFalseAlberi = 2*((dt.precisionFalse*dt.sensitivityFalse)/(dt.precisionFalse+dt.sensitivityFalse))
  areaAlberi[i] =auc(roc_dt)
  matriciAlberi[[i]] = dt.table
  
  #svm
  svm.model = svm(y ~ duration + poutcome + contact + housing + loan + campaign, 
                  data=trainset, 
                  kernel='polynomial', 
                  cost=1)
  svm_Prediction = predict(svm.model, testset)
  roc_svm = roc(testset$y, as.numeric(svm_Prediction))
  svm.table=table(testset$y, svm_Prediction)
  
  svm.table_1u = c(svm.table_1u, svm.table[1])
  svm.table_2u = c(svm.table_2u, svm.table[2])
  svm.table_3u = c(svm.table_3u, svm.table[3])
  svm.table_4u = c(svm.table_4u, svm.table[4])
  
  #calcolo misure svm
  accuratezzaSvm[i] = svm.accuracy = sum(diag(svm.table))/sum(svm.table)
  svm.precisionTrue= svm.table[4]/(svm.table[4]+svm.table[3])
  svm.precisionFalse= svm.table[1]/(svm.table[1]+svm.table[2])
  svm.sensitivityTrue = svm.table[4]/(svm.table[4]+svm.table[2])
  svm.sensitivityFalse = svm.table[1]/(svm.table[1]+svm.table[3])
  precisioniTrueSvm[i] =  svm.precisionTrue
  precisioniFalseSvm[i] =  svm.precisionFalse
  sensitivitaTrueSvm[i] = svm.sensitivityTrue
  sensitivitaFalseSvm[i] = svm.sensitivityFalse
  fmeasureTrueSvm = 2*((svm.precisionTrue*svm.sensitivityTrue)/(svm.precisionTrue+svm.sensitivityTrue))
  fmeasureFalseSvm = 2*((svm.precisionFalse*svm.sensitivityFalse)/(svm.precisionFalse+svm.sensitivityFalse))
  areaSvm[i] =auc(roc_svm)
  matriciSvm[[i]] = svm.table
  
  #stampa su pdf le curve ROC dei tre modelli
  #pdf(nomi[i])
  #plot delle curve ROC dei tre modelli
  plot(ggroc(list(SVM = roc_svm, DecisionTree = roc_dt), aes="colour", size = 1.5, legacy.axes = TRUE))
  
  #dev.off()
}

print(dt.table)
print(svm.table)

#stampa accuratezze medie dei tre modelli
#acc_alberi_under = mean(accuratezzaAlberi)
#acc_svm_under = mean(accuratezzaSvm)

prec_alberi_under = mean(precisioniTrueAlberi)
prec_svm_under = mean(precisioniTrueSvm)

rec_alberi_under = mean(sensitivitaTrueAlberi)
rec_svm_under = mean(sensitivitaTrueSvm)

f1_alberi_under = mean(fmeasureTrueAlberi)
f1_svm_under = mean(fmeasureTrueSvm)

area_alberi_under = mean(areaAlberi)
area_svm_under = mean(areaSvm)

print(sum(dt.table_1u))
print(sum(dt.table_2u))
print(sum(dt.table_3u))
print(sum(dt.table_4u))

print(sum(svm.table_1u))
print(sum(svm.table_2u))
print(sum(svm.table_3u))
print(sum(svm.table_4u))