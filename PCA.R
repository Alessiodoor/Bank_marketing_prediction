#script utilizzzato per calcolare la PCA a due dimensioni.
setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")

library(devtools)
library(ggplot2)
library(ggfortify)
#caricamento dataset
dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

autoplot(prcomp(dataset), data = dataset, colour = 'y')

