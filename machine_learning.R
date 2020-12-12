
library(class)
library(gmodels)
library(tm)
library(e1071)
library(C50)
library(RWeka)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(kernlab)
library(irr)
library(caret)
library(ipred)
library(kernlab)
library(adabag)
library(randomForest)

#read in data files:
BRSP_totab<-read.csv("BRSP_totab.csv")
BRSP_totab<-BRSP_totab[,-1:-2]#remove first 2 rows as they identify the observations and not needed in the analysis 
BRSP_CoV<-read.csv("BRSP_CoV.csv")
BRSP_CoV<-BRSP_CoV[,-1:-2]
BRSP_pres<-read.csv("BRSP_pres.csv")  
BRSP_pres<-BRSP_pres[,-1:-2]

RNGkind(sample.kind = "Rejection")#set.seed to match previous results
#Nearest Neighbor Classification: ------
#classification so using presence/absence as dependent variable
#Normalize variables:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
pres_n <- as.data.frame(lapply(BRSP_pres[c(2:30)], normalize))#book suggests using normalized variables for nearest neighbor
#partition training and test data 
set.seed(123)
train_sample<-sample(356, 320)#90% data for training

#pres - using normalized variables per text suggestion
pres_train<-pres_n[train_sample,]
pres_train_labels<-BRSP_pres[train_sample,1]
pres_test<-pres_n[-train_sample,]
pres_test_labels<-BRSP_pres[-train_sample,1]

set.seed(123)#makes results reproducible
pres_test_pred16<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 16)
CrossTable(x = pres_test_labels, y = pres_test_pred16, prop.chisq = FALSE)
## error rate: 14%, (3 false negatives, 2 false positive)
#kappa statistic:
pr_a<-0.528+0.333
pr_e<-0.583*0.611 + 0.417*0.389
k = (pr_a - pr_e)/(1-pr_e)
k #0.7113632 good agreement

kappa2(pres_test_labels, pres_test_pred16)
#test different k values: (1, 5, 10, 16, 20, 25)
set.seed(123)
pres_test_pred1<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 1)
CrossTable(x = pres_test_labels, y = pres_test_pred1, prop.chisq = FALSE)
# error rate: 19% (2 false negatives, 5 false positives)
#kappa statistic:
pr_a<-0.444+0.361
pr_e<-0.583*0.500 + 0.417*0.500
k = (pr_a - pr_e)/(1-pr_e)
k #0.61 good agreement

set.seed(123)
pres_test_pred5<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 5)
CrossTable(x = pres_test_labels, y = pres_test_pred5, prop.chisq = FALSE)
# error rate: 14% (4 false negatives, 1 false positives)
#kappa statistic:
pr_a<-0.556+0.306
pr_e<-0.583*0.667 + 0.417*0.333
k = (pr_a - pr_e)/(1-pr_e)
k #0.7077992 good agreement


set.seed(123)
pres_test_pred10<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 10)
CrossTable(x = pres_test_labels, y = pres_test_pred10, prop.chisq = FALSE)
# error rate: 17% (4 false negatives, 2 false positives)
#kappa statistic:
pr_a<-0.528+0.306
pr_e<-0.583*0.639 + 0.417*0.361
k = (pr_a - pr_e)/(1-pr_e)
k #0.6519376 good agreement


set.seed(123)
pres_test_pred20<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 20)
CrossTable(x = pres_test_labels, y = pres_test_pred20, prop.chisq = FALSE)
# error rate: 14% (3 false negatives, 2 false positives)
#kappa statistic:
pr_a<-0.528+0.333
pr_e<-0.583*0.611 + 0.417*0.389
k = (pr_a - pr_e)/(1-pr_e)
k #0.7113632 good agreement

set.seed(123)
pres_test_pred25<-knn(train = pres_train, test = pres_test, cl = pres_train_labels, k = 25)
CrossTable(x = pres_test_labels, y = pres_test_pred25, prop.chisq = FALSE)
# error rate: 14% (3 false negatives, 2 false positives)
#kappa statistic:
pr_a<-0.528+0.333
pr_e<-0.583*0.611 + 0.417*0.389
k = (pr_a - pr_e)/(1-pr_e)
k #0.7113632 good agreement

#Decison Trees: ------
#Classification so using presence/absence as dependent variable:
set.seed(123)
train_sample<-sample(356, 320)#90% data for training - 

#pres
pres_train<-BRSP_pres[train_sample,]
pres_train_labels<-BRSP_pres[train_sample,3]
pres_test<-BRSP_pres[-train_sample,]
pres_test_labels<-BRSP_pres[-train_sample,3]

set.seed(123)
pres_model<-C5.0(pres_train[-1], as.factor(pres_train$pres))
pres_model
summary(pres_model)
pres_pred<- predict(pres_model, pres_test)
CrossTable(pres_test$pres, pres_pred, prop.chisq = FALSE)
#error rate: 28% (5 false negatives, 5 false positives)
#kappa statistic:
pr_a<-0.444+0.278
pr_e<-0.583*0.583 + 0.417*0.417
k = (pr_a - pr_e)/(1-pr_e)
k #0.4282447 moderate agreement

#Boosting:
set.seed(123)
pres_modelb10<-C5.0(pres_train[-1], as.factor(pres_train$pres), 
                 trials = 10)
pres_modelb10
summary(pres_modelb10)
pres_predb10<- predict(pres_modelb10, pres_test)
CrossTable(pres_test$pres, pres_predb10,prop.chisq = FALSE)
#error rate: 8% (2 false negatives, 1 false positives)
#kappa statistic:
pr_a<-0.556+0.361
pr_e<-0.583*0.611 + 0.417*0.389
k = (pr_a - pr_e)/(1-pr_e)
k #0.8276485 Very good agreement

#rule learners: 
set.seed(123)
pres_1R<-OneR(as.factor(pres)~., data = BRSP_pres)
pres_1R
summary(pres_1R)
set.seed(123)
pres_jrip<-JRip(as.factor(pres)~., data = BRSP_pres)
pres_jrip

###Cross Validation: (10-fold)
set.seed(123)
folds<-createFolds(BRSP_pres$pres, k = 10)
set.seed(123)
cv_results <- lapply(folds, function(x){
  pres_train<- BRSP_pres[-x, ]
  pres_test<- BRSP_pres[x, ]
  pres_model<- C5.0(as.factor(pres) ~ ., data = pres_train)
  pres_pred<-predict(pres_model, pres_test)
  pres_actual<-pres_test$pres
  kappa<-kappa2(data.frame(pres_actual, pres_pred))$value
  return(kappa)
})
cv_results
mean(unlist(cv_results))
#0.4374702 moderate agreement

#tune:
set.seed(123)
m<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "C5.0")
m
summary(m$finalModel)
pred<-predict(m, BRSP_pres)
table(pred, BRSP_pres$pres)
#error rate = 0%
#control object:
ctrl<-trainControl(method = "cv", number = 25, selectionFunction = "oneSE")
grid<-expand.grid(.model = c("rules", "tree"),
                  .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                  .winnow = "FALSE")
set.seed(123)
m<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_pres)
table(pred, BRSP_pres$pres)
#error - 0%

ctrl<-trainControl(method = "cv", number = 25, selectionFunction = "best")
grid<-expand.grid(.model = c("rules", "tree"),
                  .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                  .winnow = "FALSE")

set.seed(123)
m<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_pres)
table(pred, BRSP_pres$pres)
#error - 0%

#bootstrapping:
ctrl<-trainControl(method = "boot", number = 10, selectionFunction = "best")
grid<-expand.grid(.model = c("rules", "tree"),
                  .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                  .winnow = "FALSE")
set.seed(123)
m<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_pres)
table(pred, BRSP_pres$pres)
#error = 0%

ctrl<-trainControl(method = "boot", number = 25, selectionFunction = "oneSE")
grid<-expand.grid(.model = c("rules", "tree"),
                  .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                  .winnow = "FALSE")
set.seed(123)
m<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_pres)
table(pred, BRSP_pres$pres)
#error - 0%

#bagging:
set.seed(123)
BRSP_pres$pres<-factor(BRSP_pres$pres)
mybag<-bagging(pres ~ ., data = BRSP_pres, nbag = 25)
pres_pred<-predict(mybag, BRSP_pres)
table(pres_pred, as.numeric(BRSP_pres$pres))
#error: 0%


set.seed(123)
ctrl<- trainControl(method = "cv", number = 10)
bagtrain<-train(as.factor(pres) ~ ., data = BRSP_pres, method = "treebag", trControl = ctrl)
bagtrain

#boosting:
set.seed(123)
BRSP_pres$pres<-factor(BRSP_pres$pres)
m_adaboost<-boosting(pres ~ . , data = BRSP_pres)
p_adaboost<-predict(m_adaboost, BRSP_pres)
p_adaboost$confusion
##10 fold cv with boosting:
set.seed(123)
adaboost_cv<-boosting.cv(pres ~ . , data = BRSP_pres)
adaboost_cv$confusion
#error: 19%
#Regression Methods: -----
# method does numeric prediction so using total abundance and CoV dependent variables:
#Regression Trees:
set.seed(123)
train_sample<-sample(356, 320)#90% data for training
#total abundance:
totab_train<-BRSP_totab[train_sample, ]
totab_test<-BRSP_totab[-train_sample, ]

set.seed(123)
totab_rpart<-rpart(Totalab ~ ., data = totab_train)
totab_rpart
summary(totab_rpart)
#plot
rpart.plot(totab_rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
#model performance:
totab_pRpart<-predict(totab_rpart, totab_test, type = "vector")
summary(totab_pRpart)
summary(totab_test$Totalab)
cor(totab_pRpart, totab_test$Totalab)
#0.6751305
##mean absolute error:
MAE<- function(actual, predicted){
  mean(abs(actual - predicted))
}
MAE(totab_test$Totalab, totab_pRpart)
#39.46586
mean(totab_test$Totalab)
MAE(totab_test$Totalab,47.83333)
#60.31481 - MAE of model is better than if we predicted everything was mean of total abundance
##Improving model performance:
set.seed(123)
totab_m5p<-M5P(Totalab ~ ., data = totab_train)
totab_m5p
summary(totab_m5p)
totab_pm5p<-predict(totab_m5p, totab_test)
summary(totab_pm5p)
cor(totab_pm5p, totab_test$Totalab)
#0.7498819 - correlation imporved
MAE(totab_pm5p, totab_test$Totalab)
#37.25361 - MAE improved

# MAE(pred, totab_test$Totalab)

#cross validation:
set.seed(123)
m<-train(Totalab ~ ., data = BRSP_totab, method = "rpart")
m
summary(m$finalModel)
pred<-predict(m, BRSP_totab)
cor(pred, BRSP_totab$Totalab)
#0.6474717
MAE(pred, BRSP_totab$Totalab)
#36.1603

#control object:
ctrl<-trainControl(method = "cv", number = 10, selectionFunction = "best")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(Totalab ~ ., data = BRSP_totab, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_totab)
cor(pred, BRSP_totab$Totalab)
#0.7216157
MAE(pred, BRSP_totab$Totalab)
#33.38537
m$finalModel
rpart.plot(m$finalModel, type = 3, extra = 101)

ctrl<-trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(Totalab ~ ., data = BRSP_totab, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_totab)
cor(pred, BRSP_totab$Totalab)
#0.7216157
MAE(pred, BRSP_totab$Totalab)
#33.38537

#bootstrapping:
ctrl<-trainControl(method = "boot", number = 25, selectionFunction = "best")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
bsbm<-train(Totalab ~ ., data = BRSP_totab, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
bsbm
summary(bsbm$finalModel)
pred<-predict(bsbm, BRSP_totab)
cor(pred, BRSP_totab$Totalab)
#0.8167321
MAE(pred, BRSP_totab$Totalab)
#27.54754
rpart.plot(bsbm$finalModel, type = 3, extra = 101)

ctrl<-trainControl(method = "boot", number = 25, selectionFunction = "oneSE")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(Totalab ~ ., data = BRSP_totab, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_totab)
cor(pred, BRSP_totab$Totalab)
#0.8167321
MAE(pred, BRSP_totab$Totalab)
#27.54754

#CoV
CoV_train<-BRSP_CoV[train_sample, ]
CoV_test<-BRSP_CoV[-train_sample, ]

set.seed(123)
CoV_rpart<-rpart(CoV ~ ., data = CoV_train)
CoV_rpart
summary(CoV_rpart)
#plot
rpart.plot(CoV_rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
#model performance:
CoV_pRpart<-predict(CoV_rpart, CoV_test)
summary(CoV_pRpart)
summary(CoV_test$CoV)
cor(CoV_pRpart, CoV_test$CoV)
#0.248373
##mean absolute error:
MAE(CoV_pRpart, CoV_test$CoV)
#0.2608984
mean(CoV_test$CoV)
MAE(0.2134344, CoV_test$CoV)
#0.2509877 MAE is worse than if predicting every value was mean CoV
##Improving model performance:
set.seed(123)
CoV_m5p<-M5P(CoV ~ ., data = CoV_train)
CoV_m5p
summary(CoV_m5p)
CoV_pm5p<-predict(CoV_m5p, CoV_test)
summary(CoV_pm5p)
cor(CoV_pm5p, CoV_test$CoV)
#0.4040384 #Correlation has improved
MAE(CoV_pm5p, CoV_test$CoV)
#0.2232957 # MAE has improved and is better than predicting all values are mean CoV

#cross validation:
set.seed(123)
m<-train(CoV ~ ., data = BRSP_CoV, method = "rpart")
m
summary(m$finalModel)
pred<-predict(m, BRSP_CoV)
cor(pred, BRSP_CoV$CoV)
#NA
MAE(pred, BRSP_CoV$CoV)
#0.3372078

#control object:
ctrl<-trainControl(method = "cv", number = 10, selectionFunction = "best")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1))
set.seed(123)
m<-train(CoV ~ ., data = BRSP_CoV, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_CoV)
cor(pred, BRSP_CoV$CoV)
#0.3535036
MAE(pred, BRSP_CoV$CoV)
#0.2756712
m$finalModel
rpart.plot(m$finalModel, type = 3, extra = 101)

ctrl<-trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(CoV ~ ., data = BRSP_CoV, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_CoV)
cor(pred, BRSP_CoV$CoV)
#0.3535036
MAE(pred, BRSP_CoV$CoV)
#0.2756712

#bootstrapping:
ctrl<-trainControl(method = "boot", number = 25, selectionFunction = "best")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(CoV ~ ., data = BRSP_CoV, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_CoV)
cor(pred, BRSP_CoV$CoV)
#0.3535036
MAE(pred, BRSP_CoV$CoV)
#0.2756712
rpart.plot(m$finalModel, type = 3, extra = 101)

ctrl<-trainControl(method = "boot", number = 25, selectionFunction = "oneSE")
grid<-expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))
set.seed(123)
m<-train(CoV ~ ., data = BRSP_CoV, method = "rpart", metric = "MAE", trControl = ctrl, tuneGrid = grid)
m
summary(m$finalModel)
pred<-predict(m, BRSP_CoV)
cor(pred, BRSP_CoV$CoV)
#NA
MAE(pred, BRSP_CoV$CoV)
#0.3372078


#Neural Networks: -----
# method does numeric prediction so using total abundance and CoV dependent variables:
#Normalize variables:
totab_n <- as.data.frame(lapply(BRSP_totab, normalize))
CoV_n <- as.data.frame(lapply(BRSP_CoV, normalize))

#Partition training/testing data
set.seed(123)
train_sample<-sample(356, 320)#90% data for training

#Total abundance
totab_n_train<-totab_n[train_sample, ]
totab_n_test<-totab_n[-train_sample, ]

set.seed(123)
totab_nnm<- neuralnet(Totalab ~ ., data = totab_n_train, hidden = 1)
plot(totab_nnm)
totab_nnm$result.matrix
#error (SSE) =1.359655, steps: 1017
#evalute performance:
totab_model_results<-neuralnet::compute(totab_nnm, totab_n_test[2:30])
totab_predicted_strength<-totab_model_results$net.result
cor(totab_predicted_strength, totab_n_test$Totalab)
#0.7580125
MAE(totab_predicted_strength, totab_n_test$Totalab)
#0.06552385 
mean(totab_n_test$Totalab)
MAE(0.09566667, totab_n_test$Totalab)
#0.0.1206296
##improving model performance:
set.seed(123)
totab_nnm2<- neuralnet(Totalab ~ ., data = totab_n_train, hidden = 5)
plot(totab_nnm2)
totab_nnm2$result.matrix
#error = 0.1729141, steps = 5828
totab_model_results2<-neuralnet::compute(totab_nnm2, totab_n_test[2:30])
totab_predicted_strength2<-totab_model_results2$net.result
cor(totab_predicted_strength2, totab_n_test$Totalab)
#0.702901 # correlation decreased
MAE(totab_predicted_strength2, totab_n_test$Totalab)
#0.09836915 #MAE a bit higher but still lower than predicting every value is mean totab
#try 3 layers
set.seed(123)
totab_nnm3<- neuralnet(Totalab ~ ., data = totab_n_train, hidden = 3)
plot(totab_nnm3)
totab_nnm3$result.matrix
#error = 0.4899430, steps = 3697
totab_model_results3<-neuralnet::compute(totab_nnm3, totab_n_test[2:30])
totab_predicted_strength3<-totab_model_results3$net.result
cor(totab_predicted_strength3, totab_n_test$Totalab)
#1
MAE(totab_predicted_strength3, totab_n_test$Totalab)
#0.0

#CoV
CoV_n_train<-CoV_n[train_sample, ]
CoV_n_test<-CoV_n[-train_sample, ]
set.seed(123)
CoV_nnm<- neuralnet(CoV ~ ., data = CoV_n_train, hidden = 1)
plot(CoV_nnm)
CoV_nnm$result.matrix
#error (SSE) = 4.020312852, steps: 759
#evalute performance:
CoV_model_results<-neuralnet::compute(CoV_nnm, CoV_n_test[2:30])
CoV_predicted_strength<-CoV_model_results$net.result
cor(CoV_predicted_strength, CoV_n_test$CoV)
#0.3061089
MAE(CoV_predicted_strength, CoV_n_test$CoV)
#0.08764893
mean(CoV_n_test$CoV)
MAE(0.09545076, CoV_n_test$CoV)
#0.1122451
##improving model performance:
set.seed(123)
CoV_nnm2<- neuralnet(CoV ~ ., data = CoV_n_train, hidden = 5)
plot(CoV_nnm2)
CoV_nnm2$result.matrix
#error = 0.81749384, steps = 9985
CoV_model_results2<-neuralnet::compute(CoV_nnm2, CoV_n_test[2:30])
CoV_predicted_strength2<-CoV_model_results2$net.result
cor(CoV_predicted_strength2, CoV_n_test$CoV)
#0.05161386 - correlation has gotten worse
MAE(CoV_predicted_strength2, CoV_n_test$CoV)
#0.19431 - MAE has increased and is higher than if all predicted values = mean CoV
#try 3 layers
set.seed(123)
CoV_nnm3<- neuralnet(CoV ~ ., data = CoV_n_train, hidden = 3)
plot(CoV_nnm3)
CoV_nnm3$result.matrix
#error = 1.741966, steps = 8310
CoV_model_results3<-neuralnet::compute(CoV_nnm3, CoV_n_test[2:30])
CoV_predicted_strength3<-CoV_model_results3$net.result
cor(CoV_predicted_strength3, CoV_n_test$CoV)
#0.1543845 - correlation has gotten worse
MAE(CoV_predicted_strength3, CoV_n_test$CoV)
#0.1240324 - MAE has increased and is higher than if all predicted values = mean CoV

#Support Vector Machines: -------
#can use for classification and numeric prediction so will use all 3 dependent variables:
set.seed(123)
train_sample<-sample(356, 320)#90% data for training
#pres
pres_train<-BRSP_pres[train_sample,]
pres_test<-BRSP_pres[-train_sample,]

set.seed(123)
pres_msvm<-ksvm(as.factor(pres) ~ ., data = pres_train, kernel = "vanilladot")
pres_msvm
pres_psvm<-predict(pres_msvm, pres_test)
table(pres_psvm, as.factor(pres_test$pres))
# error = 14% 3 false negatives, 2 false positives
# improve model performance:
#"rbf dot" kernel:
set.seed(123)
pres_msvm_rbf<-ksvm(as.factor(pres) ~ ., data = pres_train, kernel = "rbfdot")
pres_msvm_rbf
pres_psvm_rbf<-predict(pres_msvm_rbf, pres_test)
table(pres_psvm_rbf, as.factor(pres_test$pres))
# error = 14% 3 false negatives, 2 false positives
#"polydot" kernel:
set.seed(123)
pres_msvm_poly<-ksvm(as.factor(pres) ~ ., data = pres_train, kernel = "polydot")
pres_msvm_poly
pres_psvm_poly<-predict(pres_msvm_poly, pres_test)
table(pres_psvm_poly, as.factor(pres_test$pres))
# error = 14% 3 false negatives, 2 false positives
#tanhdot
set.seed(123)
pres_msvm_tan<-ksvm(as.factor(pres) ~ ., data = pres_train, kernel = "tanhdot")
pres_msvm_tan
pres_psvm_tan<-predict(pres_msvm_tan, pres_test)
table(pres_psvm_tan, as.factor(pres_test$pres))
# error = 28% 8 false negatives, 2 false positives

#totab
totab_train<-BRSP_totab[train_sample,]
totab_test<-BRSP_totab[-train_sample,]

set.seed(123)
totab_msvm<-ksvm(Totalab ~ ., data = totab_train, kernel = "vanilladot")
totab_msvm
totab_psvm<-predict(totab_msvm, totab_test)
cor(totab_psvm, totab_test$Totalab)
#0.8342604
MAE(totab_psvm, totab_test$Totalab)
#33.62631
mean(totab_test$Totalab)
MAE(47.83333, totab_test$Totalab)
#60.31481

# improve model performance:
#"rbf dot" kernel:
set.seed(123)
totab_msvm_rbf<-ksvm(Totalab ~ ., data = totab_train, kernel = "rbfdot")
totab_msvm_rbf
totab_psvm_rbf<-predict(totab_msvm_rbf, totab_test)
cor(totab_psvm_rbf, totab_test$Totalab)
#0.8342604 improved
MAE(totab_psvm_rbf, totab_test$Totalab)
#31.06874 decreased
#"polydot" kernel:
set.seed(123)
totab_msvm_poly<-ksvm(Totalab ~ ., data = totab_train, kernel = "polydot")
totab_msvm_poly
totab_psvm_poly<-predict(totab_msvm_poly, totab_test)
cor(totab_psvm_poly, totab_test$Totalab)
#0.802532
MAE(totab_psvm_poly, totab_test$Totalab)
#33.64616 - slightly higher

#tanhdot
set.seed(123)
totab_msvm_tan<-ksvm(Totalab ~ ., data = totab_train, kernel = "tanhdot")
totab_msvm_tan
totab_psvm_tan<-predict(totab_msvm_tan, totab_test)
cor(totab_psvm_tan, totab_test$Totalab)
#0.3427584 worse
MAE(totab_psvm_tan, totab_test$Totalab)
#1233.822 much worse

#CoV
CoV_train<-BRSP_CoV[train_sample,]
CoV_test<-BRSP_CoV[-train_sample,]

set.seed(123)
CoV_msvm<-ksvm(CoV ~ ., data = CoV_train, kernel = "vanilladot")
CoV_msvm
CoV_psvm<-predict(CoV_msvm, CoV_test)
cor(CoV_psvm, CoV_test$CoV)
#0.3354546
MAE(CoV_psvm, CoV_test$CoV)
#0.1761476
mean(CoV_test$CoV)
MAE(0.2134344, CoV_test$CoV)
#0.2509877 - predicted is better than mean

# improve model performance:
#"rbf dot" kernel:
set.seed(123)
CoV_msvm_rbf<-ksvm(CoV ~ ., data = CoV_train, kernel = "rbfdot")
CoV_msvm_rbf
CoV_psvm_rbf<-predict(CoV_msvm_rbf, CoV_test)
cor(CoV_psvm_rbf, CoV_test$CoV)
#0.292347 improved
MAE(CoV_psvm_rbf, CoV_test$CoV)
#0.2001168 decreased
#"polydot" kernel:
set.seed(123)
CoV_msvm_poly<-ksvm(CoV ~ ., data = CoV_train, kernel = "polydot")
CoV_msvm_poly
CoV_psvm_poly<-predict(CoV_msvm_poly, CoV_test)
cor(CoV_psvm_poly, CoV_test$CoV)
#0.335469 - slightly less
MAE(CoV_psvm_poly, CoV_test$CoV)
# 0.1761053 - decreased

#tanhdot
set.seed(123)
CoV_msvm_tan<-ksvm(CoV ~ ., data = CoV_train, kernel = "tanhdot")
CoV_msvm_tan
CoV_psvm_tan<-predict(CoV_msvm_tan, CoV_test)
cor(CoV_psvm_tan, CoV_test$CoV)
#-0.3222307 worse
MAE(CoV_psvm_tan, CoV_test$CoV)
#6.571009 much worse








#Random Forests: -----
#pres:
set.seed(123)
pres_rf<-randomForest(pres ~., data = BRSP_pres, importance = TRUE, proximity = TRUE)#mtry defaults to sqrt(#variables) which is rounded to 5 here:
pres_rf
#error - 18% (out-of-bag)
summary(pres_rf)
importance(pres_rf)
varImpPlot(pres_rf)

#evaluate performance/improve model:
BRSP_pres$pres<-as.factor(BRSP_pres$pres)
ctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf<-expand.grid(.mtry = c(2,5,10,29))
set.seed(123)
presm_rf<-train(pres ~., data = BRSP_pres, method = "rf", metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
presm_rf
varImpPlot(presm_rf$finalModel)

#totab:
set.seed(123)
totab_rf<-randomForest(Totalab ~., data = BRSP_totab, importance = TRUE, proximity = TRUE)
totab_rf

summary(totab_rf)
importance(totab_rf)
varImpPlot(totab_rf)

#evaluate performance:
ctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf<-expand.grid(.mtry = c(2,5,10,29))
set.seed(123)
totabm_rf<-train(Totalab ~., data = BRSP_totab, method = "rf", metric = "MAE", trControl = ctrl, importance=TRUE, tuneGrid = grid_rf)
totabm_rf
varImpPlot(totabm_rf$finalModel)



#CoV:
set.seed(123)
CoV_rf<-randomForest(CoV ~., data = BRSP_CoV, importance = TRUE, proximity = TRUE)
CoV_rf

summary(CoV_rf)
importance(CoV_rf)
varImpPlot(CoV_rf)

#evaluate performance:
ctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf<-expand.grid(.mtry = c(2,5,10,29))
set.seed(123)
CoVm_rf<-train(CoV ~., data = BRSP_CoV, method = "rf", metric = "MAE", trControl = ctrl, importance=TRUE, tuneGrid = grid_rf)
CoVm_rf
varImpPlot(CoVm_rf$finalModel)




