#questo script viene eseguito dopo lo script 'corss_base.R', così da mantenere gli stessi fold.
#script che esegue la 10-fold cross validation utilizzando i 3 modelli finali, attribuendo un peso ai valori di y.
#vengono calcolate le misure di accuracy, precision, sensitivity(recall), f-measure e AUC per ogni modello eseguito su ogni fold
#vengono inoltre visualizzate tutte le curve ROC dei modelli per ogni fold

setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(e1071)
library(pROC)
library(caret)
library(ROSE)

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

dt.table_1w = c()
dt.table_2w = c()
dt.table_3w = c()
dt.table_4w = c()

svm.table_1w = c()
svm.table_2w = c()
svm.table_3w = c()
svm.table_4w = c()

for(i in 1:10){
  #Creazione di trainset e testset utilizzando il fold i-esimo
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testset <- dataset[testIndexes, ]
  trainset <- dataset[-testIndexes, ]
  testset$y = factor(testset$y)
  trainset$y = factor(trainset$y)
  
  print(table(trainset$y))
  
  prop = table(trainset$y)
  
  weight<-c(rep(0, prop['0']), rep(1, prop['1']))
  
  #decision tree
  decisionTree = rpart(y ~ duration + poutcome + contact + housing + loan + campaign + marital + education + age + month + job,
                       data = trainset,
                       weights = weight)
  dt_Prediction = predict(decisionTree, testset, type = "class")
  roc_dt = roc(testset$y, as.numeric(dt_Prediction))
  dt.table=table(testset$y, dt_Prediction)
  
  dt.table_1w = c(dt.table_1w, dt.table[1])
  dt.table_2w = c(dt.table_2w, dt.table[2])
  dt.table_3w = c(dt.table_3w, dt.table[3])
  dt.table_4w = c(dt.table_4w, dt.table[4])
  
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
                  cost=1,
                  class.weights = c("0" = 0.115, "1" = 0.885))
  svm_Prediction = predict(svm.model, testset)
  roc_svm = roc(testset$y, as.numeric(svm_Prediction))
  svm.table=table(testset$y, svm_Prediction)
  
  svm.table_1w = c(svm.table_1w, svm.table[1])
  svm.table_2w = c(svm.table_2w, svm.table[2])
  svm.table_3w = c(svm.table_3w, svm.table[3])
  svm.table_4w = c(svm.table_4w, svm.table[4])
  
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
#acc_alberi_weight = mean(accuratezzaAlberi)
#acc_svm_weight = mean(accuratezzaSvm)

prec_alberi_weight = mean(precisioniTrueAlberi)
prec_svm_weight = mean(precisioniTrueSvm)

rec_alberi_weight = mean(sensitivitaTrueAlberi)
rec_svm_weight = mean(sensitivitaTrueSvm)

f1_alberi_weight = mean(fmeasureTrueAlberi)
f1_svm_weight = mean(fmeasureTrueSvm)

area_alberi_weight = mean(areaAlberi)
area_svm_weight = mean(areaSvm)

print(sum(dt.table_1w))
print(sum(dt.table_2w))
print(sum(dt.table_3w))
print(sum(dt.table_4w))

print(sum(svm.table_1w))
print(sum(svm.table_2w))
print(sum(svm.table_3w))
print(sum(svm.table_4w))