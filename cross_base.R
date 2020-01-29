#script che esegue la 10-fold cross validation utilizzando i 3 modelli finali con la configurazione baseline.
#vengono calcolate le misure di precision, sensitivity(recall), f-measure e AUC per ogni modello eseguito su ogni fold
#vengono inoltre visualizzate tutte le curve ROC dei modelli per ogni fold.

setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(e1071)
library(pROC)
library(caret)
library(ROSE)

#importazione del dataset
dataset = read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)
dataset$y = factor(dataset$y)
dataset = dataset[sample(nrow(dataset)),]
dataset$X = NULL

#Creazione di 10 fold
folds <- createFolds(factor(dataset$y), k = 10, list = FALSE)

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

dt.table_1 = c()
dt.table_2 = c()
dt.table_3 = c()
dt.table_4 = c()

svm.table_1 = c()
svm.table_2 = c()
svm.table_3 = c()
svm.table_4 = c()

nomi = c("fold1.pdf","fold2.pdf","fold3.pdf","fold4.pdf","fold5.pdf","fold6.pdf","fold7.pdf","fold8.pdf","fold9.pdf","fold10.pdf")

for(i in 1:10){
  #Creazione di trainset e testset utilizzando il fold i-esimo
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testset <- dataset[testIndexes, ]
  trainset <- dataset[-testIndexes, ]
  testset$y = factor(testset$y)
  trainset$y = factor(trainset$y)
  
  #decision tree
  decisionTree = rpart(y ~ duration + poutcome + contact + housing + loan + campaign + marital + education + age + month + job,
                       trainset, 
                       method = "class")
  dt_Prediction = predict(decisionTree, testset, type = "class")
  roc_dt = roc(testset$y, as.numeric(dt_Prediction))
  dt.table=table(testset$y, dt_Prediction)
  
  #calcolo misure decision tree
  accuratezzaAlberi[i] = dt.accuracy = sum(diag(dt.table))/sum(dt.table)
  dt.precisionTrue= dt.table[4]/(dt.table[4]+dt.table[3])
  print(dt.table)
  print(dt.table[1])
  print(dt.table[2])
  print(dt.table[3])
  print(dt.table[4])
  dt.table_1 = c(dt.table_1, dt.table[1])
  dt.table_2 = c(dt.table_2, dt.table[2])
  dt.table_3 = c(dt.table_3, dt.table[3])
  dt.table_4 = c(dt.table_4, dt.table[4])
  
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
  print(svm.table)
  svm.table_1 = c(svm.table_1, svm.table[1])
  svm.table_2 = c(svm.table_2, svm.table[2])
  svm.table_3 = c(svm.table_3, svm.table[3])
  svm.table_4 = c(svm.table_4, svm.table[4])
  
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
#acc_alberi_base = mean(accuratezzaAlberi)
#acc_svm_base = mean(accuratezzaSvm)

prec_alberi_base = mean(precisioniTrueAlberi)
prec_svm_base = mean(precisioniTrueSvm)

rec_alberi_base = mean(sensitivitaTrueAlberi)
rec_svm_base = mean(sensitivitaTrueSvm)

f1_alberi_base = mean(fmeasureTrueAlberi)
f1_svm_base = mean(fmeasureTrueSvm)

area_alberi_base = mean(areaAlberi)
area_svm_base = mean(areaSvm)

print(sum(dt.table_1))
print(sum(dt.table_2))
print(sum(dt.table_3))
print(sum(dt.table_4))

print(sum(svm.table_1))
print(sum(svm.table_2))
print(sum(svm.table_3))
print(sum(svm.table_4))