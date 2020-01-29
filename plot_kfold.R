#script utilizzato per confrontare i risultati ottenuti durante le tre diverse crosso validation (normale, weighted e undersampled)
#questo scritp deve essere eseguito dopo i tre script di cross validation (cross_base.R, cross_weighted.R e cross_under.R)

plotconf_1 <- function(base, weighted, title){
  color.names = c("lightblue", "red")
  classifier = c("DecisionTree", "SVM")
  data_acc <- data.frame(base = base, weighted = weighted)
  row.names(data_acc) = classifier
  barplot(t(data_acc), beside=T, ylim=c(0,1), 
          main = title,
          col = color.names)
  legend("bottomleft",rownames(t(data_acc)),cex =0.8,fill=color.names)
}

plotconf_2 <- function(weighted, under, title){
  color.names = c("lightblue", "red")
  classifier = c("DecisionTree", "SVM")
  data_acc <- data.frame(weighted = weighted, undernsampling = under)
  row.names(data_acc) = classifier
  barplot(t(data_acc), beside=T, ylim=c(0,1), 
          main = title,
          col = color.names)
  legend("bottomleft",rownames(t(data_acc)),cex =0.8,fill=color.names)
}

prec_base = c(prec_alberi_base, prec_svm_base)
rec_base = c(rec_alberi_base, rec_svm_base)
f1_base = c(f1_alberi_base, f1_svm_base)
area_base = c(area_alberi_base, area_svm_base)

prec_w = c(prec_alberi_weight, prec_svm_weight)
rec_w = c(rec_alberi_weight, rec_svm_weight)
f1_w = c(f1_alberi_weight, f1_svm_weight)
area_w = c(area_alberi_weight, area_svm_weight)

prec_u = c(prec_alberi_under, prec_svm_under)
rec_u = c(rec_alberi_under, rec_svm_under)
f1_u = c(f1_alberi_under, f1_svm_under)
area_u = c(area_alberi_under, area_svm_under)

names = c("DecisionTree", "SVM")

#confronti tra base a pesato
plotconf_1(prec_base, prec_w, "Precision")
plotconf_1(rec_base, rec_w, "Recall")
plotconf_1(f1_base, f1_w, "F-measure")
plotconf_1(area_base, area_w, "AUC")

#confronti tra pesato e under
plotconf_2(prec_w, prec_u, "Precision")
plotconf_2(rec_w, rec_u, "Recall")
plotconf_2(f1_w, f1_u, "F-measure")
plotconf_2(area_w, area_u, "AUC")

acc_base
acc_w
acc_u
prec_base
prec_w
prec_u
rec_base
rec_w
rec_u
f1_base
f1_w
f1_u
area_base
area_w
area_u

