
library(class)
library(gmodels)
library(tm)
library(e1071)
library(C50)
library(RWeka)
library(rpart)
library(rpart.plot)
library(neuralnet)

#read in data files:
BRSP_totab<-read.csv("BRSP_totab.csv")
BRSP_totab<-BRSP_totab[,-1:-2]
BRSP_CoV<-read.csv("BRSP_CoV.csv")
BRSP_CoV<-BRSP_CoV[,-1:-2]
BRSP_pres<-read.csv("BRSP_pres.csv")  
BRSP_pres<-BRSP_pres[,-1:-2]


#Nearest Neighbor Classification: ------
#classification so using presence/absence as dependent variable
#Normalize variables:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
pres_n <- as.data.frame(lapply(BRSP_pres[c(2:30)], normalize))
#partition training and test data 
set.seed(123)
train_sample<-sample(356, 267)#75% data for training

#pres - using normalized variables per text suggestion
pres_train<-pres_n[train_sample,]
pres_train_labels<-BRSP_pres[train_sample,1]
pres_test<-pres_n[-train_sample,]
pres_test_labels<-BRSP_pres[-train_sample,1]

pres_test_pred16<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 16)
pres_ct16<-CrossTable(x = pres_test_labels, y = pres_test_pred16, prop.chisq = FALSE)
## error rate: 19%, (16 false negatives, 1 false positive)

#test different k values: (1, 5, 10, 16, 20, 25)
pres_test_pred1<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 1)
pres_ct1<-CrossTable(x = pres_test_labels, y = pres_test_pred1, prop.chisq = FALSE)
# error rate: 22% (13 false negatives, 7 false positives)

pres_test_pred5<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 5)
pres_ct5<-CrossTable(x = pres_test_labels, y = pres_test_pred5, prop.chisq = FALSE)
# error rate: 22% (17 false negatives, 3 false positives)

pres_test_pred10<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 10)
pres_ct10<-CrossTable(x = pres_test_labels, y = pres_test_pred10, prop.chisq = FALSE)
# error rate: 21% (17 false negatives, 2 false positives)

pres_test_pred20<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 20)
pres_ct20<-CrossTable(x = pres_test_labels, y = pres_test_pred20, prop.chisq = FALSE)
# error rate: 20% (17 false negatives, 1 false positives)

pres_test_pred25<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 25)
pres_ct25<-CrossTable(x = pres_test_labels, y = pres_test_pred25, prop.chisq = FALSE)
# error rate: 21% (18 false negatives, 2 false positives)

#Naive Bayes: -------

#presence/absence
pres_classifier<-naiveBayes(pres_train, pres_train_labels)
pres_pred<-predict(pres_classifier, pres_test)
CrossTable(pres_pred, pres_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#Decison Trees: ------
#Classification so using presence/absence as dependent variable:
set.seed(123)
train_sample<-sample(356, 320)#90% data for training

#pres
pres_train<-BRSP_pres[train_sample,]
pres_train_labels<-BRSP_pres[train_sample,3]
pres_test<-BRSP_pres[-train_sample,]
pres_test_labels<-BRSP_pres[-train_sample,3]

pres_model<-C5.0(pres_train[-1], as.factor(pres_train$pres))
pres_model
summary(pres_model)
pres_pred<- predict(pres_model, pres_test)
CrossTable(pres_test$pres, pres_pred, 
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#error rate: 28% (5 false negatives, 5 false positives)

#Boosting:
pres_modelb10<-C5.0(pres_train[-1], as.factor(pres_train$pres), 
                 trials = 10)
pres_modelb10
summary(pres_modelb10)
pres_predb10<- predict(pres_modelb10, pres_test)
CrossTable(pres_test$pres, pres_predb10, 
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#error rate: 8% (2 false negatives, 1 false positives)

#rule learners: 
pres_1R<-OneR(as.factor(pres)~., data = BRSP_pres)
pres_1R
summary(pres_1R)
pres_jrip<-JRip(as.factor(pres)~., data = BRSP_pres)
pres_jrip

#Regression Methods: -----
# method does numeric prediction so using total abundance and CoV dependent variables:
#Regression Trees:
set.seed(123)
train_sample<-sample(356, 267)#75% data for training
#total abundance:
totab_train<-BRSP_totab[train_sample, ]
totab_test<-BRSP_totab[-train_sample, ]

totab_rpart<-rpart(Totalab ~ ., data = totab_train)
totab_rpart
summary(totab_rpart)
#plot
rpart.plot(totab_rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
#model performance:
totab_pRpart<-predict(totab_rpart, totab_test)
summary(totab_pRpart)
summary(totab_test$Totalab)
cor(totab_pRpart, totab_test$Totalab)
#0.6751305
##mean absolute error:
MAE<- function(actual, predicted){
  mean(abs(actual - predicted))
}
MAE(totab_pRpart, totab_test$Totalab)
#39.46586
mean(totab_test$Totalab)
MAE(47.83333, totab_test$Totalab)
#60.31481
##Improving model performance:
totab_m5p<-M5P(Totalab ~ ., data = totab_train)
totab_m5p
summary(totab_m5p)
totab_pm5p<-predict(totab_m5p, totab_test)
summary(totab_pm5p)
cor(totab_pm5p, totab_test$Totalab)
#0.767422
MAE(totab_pm5p, totab_test$Totalab)
#32.9703

#CoV
CoV_train<-BRSP_CoV[train_sample, ]
CoV_test<-BRSP_CoV[-train_sample, ]

CoV_rpart<-rpart(CoV ~ ., data = CoV_train)
CoV_rpart
summary(CoV_rpart)
#plot
rpart.plot(CoV_rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
#model performance:
CoV_pRpart<-predict(CoV_rpart, CoV_test)
summary(CoV_pRpart)
summary(CoV_test$CoV)
cor(CoV_pRpart, CoV_test$CoV, method = "spearman")
#0.4549783
##mean absolute error:
MAE(CoV_pRpart, CoV_test$CoV)
#0.2838972
mean(CoV_test$CoV)
MAE(47.83333, CoV_test$CoV)
#47.53104
##Improving model performance:
CoV_m5p<-M5P(CoV ~ ., data = CoV_train)
CoV_m5p
summary(CoV_m5p)
CoV_pm5p<-predict(CoV_m5p, CoV_test)
summary(CoV_pm5p)
cor(CoV_pm5p, CoV_test$CoV, method = "spearman")
#0.4720839
MAE(CoV_pm5p, CoV_test$CoV)
#0.278617

#Neural Networks: -----
# method does numeric prediction so using total abundance and CoV dependent variables:
#Normalize variables:
totab_n <- as.data.frame(lapply(BRSP_totab, normalize))
CoV_n <- as.data.frame(lapply(BRSP_CoV, normalize))

#Partition training/testing data
set.seed(123)
train_sample<-sample(356, 267)#75% data for training

#Total abundance
totab_n_train<-totab_n[train_sample, ]
totab_n_test<-totab_n[-train_sample, ]
totab_nnm<- neuralnet(Totalab ~ ., data = totab_n_train, hidden = 1)
plot(totab_nnm)
totab_nnm$result.matrix
#error (SSE) = 1.160632065, steps: 582
#evalute performance:
totab_model_results<-compute(totab_nnm, totab_n_test[2:30])
totab_predicted_strength<-totab_model_results$net.result
cor(totab_predicted_strength, totab_n_test$Totalab, method = "spearman")
#0.6948018
##improving model performance:
totab_nnm2<- neuralnet(Totalab ~ ., data = totab_n_train, hidden = 5)
plot(totab_nnm2)
totab_nnm2$result.matrix
#error = 0.07339928, steps = 5610
totab_model_results2<-compute(totab_nnm2, totab_n_test[2:30])
totab_predicted_strength2<-totab_model_results2$net.result
cor(totab_predicted_strength2, totab_n_test$Totalab, method = "spearman")
#0.6572663
#CoV
CoV_n_train<-CoV_n[train_sample, ]
CoV_n_test<-CoV_n[-train_sample, ]
CoV_nnm<- neuralnet(CoV ~ ., data = CoV_n_train, hidden = 1)
plot(CoV_nnm)
CoV_nnm$result.matrix
#error (SSE) = 2.597393, steps: 1489
#evalute performance:
CoV_model_results<-compute(CoV_nnm, CoV_n_test[2:30])
CoV_predicted_strength<-CoV_model_results$net.result
cor(CoV_predicted_strength, CoV_n_test$CoV,method = "spearman")
#0.5691191
##improving model performance:
CoV_nnm2<- neuralnet(CoV ~ ., data = CoV_n_train, hidden = 5)
plot(CoV_nnm2)
CoV_nnm2$result.matrix
#error = 0.3088569, steps = 18947
CoV_model_results2<-compute(CoV_nnm2, CoV_n_test[2:30])
CoV_predicted_strength2<-CoV_model_results2$net.result
cor(CoV_predicted_strength2, CoV_n_test$CoV, method = "spearman")
#-0.01691676

#Support Vector Machines: -------






