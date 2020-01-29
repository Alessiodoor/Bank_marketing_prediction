#script utilizzato per individuare gli outliers presenti nel dataset, viene inoltre visualizzato un barplot delle percentuali
setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")

library(corrplot)
library(RColorBrewer)
library(e1071)
library(pROC)
library(ggplot2)
library(ROSE)
library(caret)

get_n_outlieras <- function(col, min, max){
  n = 0
  for(i in 1:nrow(dataset)){
    if(dataset[i, col] < min || dataset[i, col] > max){
      n = n+1
    }
  }
  return(n)
}

#caricamento dataset
dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)

dataset$y = factor(dataset$y)
#shuffle
set.seed(42)
dataset = dataset[sample(nrow(dataset), nrow(dataset)), ]

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

df_copy = data.frame(dataset)

print(nrow(df_copy))

range = 3
names(dataset)

n_out = c()

max = quantile(dataset$age, 0.75) + (IQR(dataset$age) * range)
min = quantile(dataset$age, 0.25) - (IQR(dataset$age) * range)

n = get_n_outlieras('age', min, max)
n_out = c(n_out, n/nrow(dataset))

max = quantile(dataset$balance, 0.75) + (IQR(dataset$balance) * range)
min = quantile(dataset$balance, 0.25) - (IQR(dataset$balance) * range)

n = get_n_outlieras('balance', min, max)
n_out = c(n_out, n/nrow(dataset))

max = quantile(dataset$duration, 0.75) + (IQR(dataset$duration) * range)
min = quantile(dataset$duration, 0.25) - (IQR(dataset$duration) * range)

n = get_n_outlieras('duration', min, max)
n_out = c(n_out, n/nrow(dataset))

max = quantile(dataset$campaign, 0.75) + (IQR(dataset$campaign) * range)
min = quantile(dataset$campaign, 0.25) - (IQR(dataset$campaign) * range)

n = get_n_outlieras('campaign', min, max)
n_out = c(n_out, n/nrow(dataset))

max = quantile(dataset$pdays, 0.75) + (IQR(dataset$pdays) * range)
min = quantile(dataset$pdays, 0.25) - (IQR(dataset$pdays) * range)

n = get_n_outlieras('pdays', min, max)
n_out = c(n_out, n/nrow(dataset))

max = quantile(dataset$previous, 0.75) + (IQR(dataset$previous) * range)
min = quantile(dataset$previous, 0.25) - (IQR(dataset$previous) * range)

n = get_n_outlieras('previous', min, max)
n_out = c(n_out, n/nrow(dataset))

nrow(dataset)
nrow(df_copy)

names(n_out) = c('age', 'balance', 'duration', 'campaign', 'pdays', 'previous')
n_out

barplot(n_out,
        main = 'Nuemro di outliers negli attributi numerici',
        ylim = c(0, 0.2),
        ylab = 'Numero di clienti',
        xlab = 'Attributi',
        col = c('blue'))




