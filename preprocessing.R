#script utilizzato per calcolare la matrice di covarianza.
#Successivamente, vengono visualizzati i valori di covarianza tra gli attributi e la classe y.
#Infine, vengono calcolate le coppie di attributi con covarianza maggiore (almeno 0.4).
setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")
library(corrplot)
library(RColorBrewer)
library(caret)
library(e1071)
library(pROC)

#caricamento dataset
dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

#matrice di correlazione e plot
M <-cor(dataset)
corrplot(M, method = 'square', type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#estraggo dalla matice di correlazione il vettore delle correlazioni con la classe
y_corr = tail(M, 1)
#rimuovo la correlazione con se stessa
y_corr = subset(y_corr, select = -c(0,17) )

#estraggo i nomi degli attributi
names = c(rownames(M))

#plot correlazione attributi con la classe 
barplot(y_corr, main = "Correlazione con la classe", ylim=c(-1, 1), las = 2)

#salvo le coppie di attributi più correlati (almeno 0.4) 
for (i in 1:nrow(M)) {
  for(j in 1:ncol(M)){
    if(abs(M[i, j]) > 0.4 && M[i, j] < 1){
      print(i)
      print(j)
      print(names[i])
      print(names[j])
      print(M[i, j])
      print("--")
    }
  }
}

#more correlated
#"poutcome" "previous" 0.4813412
#"poutcome" "pdays" 0.790806
#"previous" "pdays" 0.4548196
