#script utilizzato per estrarre il sottoinsieme di attributi più performante per i modelli valutati
#vengono ordinati gli attributi in base alla correlazione con y, successivamente, partendo dall'insieme 
#costitutito dai primi due attributi, viene calcolata l'AUC di ogni modello.
#Iterativamente, viene aggiunto l'attributi successivo al sottoinsieme e ricalcolata l'AUC.
#Infine, viene mostrato un plot dei valori di AUC di ogni modello con ogni sottoinsieme.
setwd("~/Documents/ProgettiUniversità/Progetto_ML_Porta/codice/R")

library(corrplot)
library(RColorBrewer)
library(e1071)
library(pROC)
library(ggplot2)
library(ROSE)
library(caret)
library(rpart)

#caricamento dataset
dataset= read.csv("bank-int.csv", header = TRUE, sep=",", row.names=NULL)

#shuffle
set.seed(42)
dataset = dataset[sample(nrow(dataset), nrow(dataset)), ]

#rimuovo la prima colonna, ovvero l'id dei campioni
dataset$X = NULL

areas_w_no_svm = c()
areas_w_no_dt = c()

#tratified splitting (0.75 => train, 0.25 => test)
train.rows<- createDataPartition(y= dataset$y, p=0.75, list = FALSE)
trainset<- dataset[train.rows,] # 70% data goes in here
trainset$y = factor(trainset$y)
testset <- dataset[-train.rows,]
testset$y = factor(testset$y)

print(table(trainset$y))

#matrice di correlazione e plot
M <-cor(dataset)
corrplot(M, method = 'square', type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#estraggo dalla matice di correlazione il vettore delle correlazioni con la classe
y_corr = tail(M, 1)
#rimuovo la correlazione con se stessa
y_corr = subset(y_corr, select = -c(0,17) )

#plot correlazione attributi con la classe 
barplot(y_corr, main = "Correlazione con la classe", ylim=c(-1, 1), las = 2)

#metto in modulo tutti i valori di correlazione
for(i in 1:ncol(y_corr)){
  y_corr[i] = abs(y_corr[i])  
}

#ordino a partire dall'attributo con correlazione maggiore
y_corr = rbind(y_corr, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
y_corr = y_corr[,order(y_corr[1,], decreasing = T)]
#y_corr = t(y_corr)

y_corr_names = c("duration",
                 "poutcome",
                 "contact",
                 "housing",
                 "loan",
                 "campaign",
                 "marital",
                 "education",
                 "age",
                 "month",
                 "job",
                 "balance",
                 "day",
                 "default")

indexs = c()
for( i in 1:ncol(y_corr)){
  if(y_corr[2, i]  != 14 && y_corr[2, i] != 15){
    indexs = c(indexs, y_corr[2,i])
  }
}

print(length(indexs))

for(limit in 2:14){
  #estraggo i primi n attributi
  features = c()
  features_names = c("y")
  #limit = 1
  for(i in 1:length(indexs)){
    if(i <= limit){
      features = c(features, indexs[i])
      features_names = c(features_names, y_corr_names[i])
    }
  }
  
  print(features)
  print(features_names)
  
  part_train = trainset[features_names]
  part_test = testset[features_names]
  
  prop = table(trainset$y)
  
  weight<-c(rep(0, prop['0']), rep(1, prop['1']))
  
  #------------svm------------
  model = svm(y ~ ., 
              data=part_train, 
              kernel='polynomial', 
              cost=1,
              class.weights = c("0" = 0.115, "1" = 0.885))
  
  predicted_test = predict(model, testset, type = "class") 
  
  #roc curve
  roc_obj = roc(testset$y, as.numeric(predicted_test))
  # roc_objs = c(roc_objs, roc_obj)
  plot(ggroc(roc_obj))
  
  area = auc(roc_obj)
  
  areas_w_no_svm = c(areas_w_no_svm, area)
  
  #--------------dt----------------
  decisionTree = rpart(y ~ ., 
                       data = part_train,
                       weights = weight,
                       method = "class")
  
  predicted_test = predict(decisionTree, testset, type = "class") 
  
  #roc curve
  roc_obj = roc(testset$y, as.numeric(predicted_test))
  # roc_objs = c(roc_objs, roc_obj)
  plot(ggroc(roc_obj))
  
  area = auc(roc_obj)
  
  areas_w_no_dt = c(areas_w_no_dt, area)
}

print(areas_w_no_svm)
print(max(areas_w_no_svm))

print(areas_w_no_dt)
print(max(areas_w_no_dt))

plotmeasure_2 <- function(y1, y2, title){
  x = c(2,3,4,5,6,7,8,9,10,11,12,13,14)
  miny = min(c(min(y1), min(y2)))
  maxy = max(c(max(y1), max(y2)))
  plot(x, y1, type = "b", frame = FALSE, pch = 19, 
       col = "red", xlab = "Numero di features", ylab = "Misura di performance", 
       ylim= c(0.6, 0.85), xlim = c(1, 17))
  axis(1, at = x)
  # Add a second line
  lines(x, y2, pch = 18, col = "blue", type = "b", lty = 2)
  title(title)
  # Add a legend to the plot
  legend("topleft", legend=c("SVM", "Decision Tree"),
         col=c("red", "blue", "black"), lty = 1:2, cex=0.8)
}

plotmeasure_2(areas_w_no_svm, areas_w_no_dt, "Andamento AUC modelli pesati")
