
#variable importance for Support Vector Machine
#Take out one variable at a time and see how it changes resulting model MAE
options(scipen = 999)
set.seed(123)
train_sample<-sample(356, 320)#90% data for training

#total abunbance: -----
totab_train<-BRSP_totab[train_sample,]
totab_test<-BRSP_totab[-train_sample,]

#"rbfdot" kernel:
set.seed(123)
totab_msvm_rbf<-ksvm(Totalab ~ ., data = totab_train, kernel = "rbfdot")
totab_msvm_rbf
totab_psvm_rbf<-predict(totab_msvm_rbf, totab_test)
cor(totab_psvm_rbf, totab_test$Totalab)
#0.8342604 
MAE1<-MAE(totab_psvm_rbf, totab_test$Totalab)
#31.06874 

#-p312_400m
set.seed(123)
totab_msvm_rbf2<-ksvm(Totalab ~ ., data = totab_train[,-2], kernel = "rbfdot")
totab_msvm_rbf2
totab_psvm_rbf2<-predict(totab_msvm_rbf2, totab_test[,-2])
cor(totab_psvm_rbf2, totab_test$Totalab)
MAE2<-MAE(totab_psvm_rbf2, totab_test$Totalab)
diff_mae2<-(MAE1-MAE2)
diff_mae2
# 0.4923805

#-p316_8km
set.seed(123)
totab_msvm_rbf3<-ksvm(Totalab ~ ., data = totab_train[,-3], kernel = "rbfdot")
totab_msvm_rbf3
totab_psvm_rbf3<-predict(totab_msvm_rbf3, totab_test[,-3])
cor(totab_psvm_rbf3, totab_test$Totalab)
MAE3<-MAE(totab_psvm_rbf3, totab_test$Totalab)
diff_mae3<-(MAE1-MAE3)
diff_mae3
# 0.7151175

#-p437_5km
set.seed(123)
totab_msvm_rbf4<-ksvm(Totalab ~ ., data = totab_train[,-4], kernel = "rbfdot")
totab_msvm_rbf4
totab_psvm_rbf4<-predict(totab_msvm_rbf4, totab_test[,-4])
cor(totab_psvm_rbf4, totab_test$Totalab)
MAE4<-MAE(totab_psvm_rbf4, totab_test$Totalab)
diff_mae4<-(MAE1-MAE4)
diff_mae4
# 0.5608195

#-p484_1km
set.seed(123)
totab_msvm_rbf5<-ksvm(Totalab ~ ., data = totab_train[,-5], kernel = "rbfdot")
totab_msvm_rbf5
totab_psvm_rbf5<-predict(totab_msvm_rbf5, totab_test[,-5])
cor(totab_psvm_rbf5, totab_test$Totalab)
MAE5<-MAE(totab_psvm_rbf5, totab_test$Totalab)
diff_mae5<-(MAE1-MAE5)
diff_mae5
# 0.4232326

#-p485_20km
set.seed(123)
totab_msvm_rbf6<-ksvm(Totalab ~ ., data = totab_train[,-6], kernel = "rbfdot")
totab_msvm_rbf6
totab_psvm_rbf6<-predict(totab_msvm_rbf6, totab_test[,-6])
cor(totab_psvm_rbf6, totab_test$Totalab)
MAE6<-MAE(totab_psvm_rbf6, totab_test$Totalab)
diff_mae6<-(MAE1-MAE6)
diff_mae6
# -0.4377896

#-p488_20km
set.seed(123)
totab_msvm_rbf7<-ksvm(Totalab ~ ., data = totab_train[,-7], kernel = "rbfdot")
totab_msvm_rbf7
totab_psvm_rbf7<-predict(totab_msvm_rbf7, totab_test[,-7])
cor(totab_psvm_rbf7, totab_test$Totalab)
MAE7<-MAE(totab_psvm_rbf7, totab_test$Totalab)
diff_mae7<-(MAE1-MAE7)
diff_mae7
# -0.2091821

#-p489_400m
set.seed(123)
totab_msvm_rbf8<-ksvm(Totalab ~ ., data = totab_train[,-8], kernel = "rbfdot")
totab_msvm_rbf8
totab_psvm_rbf8<-predict(totab_msvm_rbf8, totab_test[,-8])
cor(totab_psvm_rbf8, totab_test$Totalab)
MAE8<-MAE(totab_psvm_rbf8, totab_test$Totalab)
diff_mae8<-(MAE1-MAE8)
diff_mae8
# 2.661991

#-p490_400m
set.seed(123)
totab_msvm_rbf9<-ksvm(Totalab ~ ., data = totab_train[,-9], kernel = "rbfdot")
totab_msvm_rbf9
totab_psvm_rbf9<-predict(totab_msvm_rbf9, totab_test[,-9])
cor(totab_psvm_rbf9, totab_test$Totalab)
MAE9<-MAE(totab_psvm_rbf9, totab_test$Totalab)
diff_mae9<-(MAE1-MAE9)
diff_mae9
# -1.442331

#-p491_20km
set.seed(123)
totab_msvm_rbf10<-ksvm(Totalab ~ ., data = totab_train[,-10], kernel = "rbfdot")
totab_msvm_rbf10
totab_psvm_rbf10<-predict(totab_msvm_rbf10, totab_test[,-10])
cor(totab_psvm_rbf10, totab_test$Totalab)
MAE10<-MAE(totab_psvm_rbf10, totab_test$Totalab)
diff_mae10<-(MAE1-MAE10)
diff_mae10
# -1.386826

#-p492_5km
set.seed(123)
totab_msvm_rbf11<-ksvm(Totalab ~ ., data = totab_train[,-11], kernel = "rbfdot")
totab_msvm_rbf11
totab_psvm_rbf11<-predict(totab_msvm_rbf11, totab_test[,-11])
cor(totab_psvm_rbf11, totab_test$Totalab)
MAE11<-MAE(totab_psvm_rbf11, totab_test$Totalab)
diff_mae11<-(MAE1-MAE11)
diff_mae11
# -0.1105037

#-p493_4km
set.seed(123)
totab_msvm_rbf12<-ksvm(Totalab ~ ., data = totab_train[,-12], kernel = "rbfdot")
totab_msvm_rbf12
totab_psvm_rbf12<-predict(totab_msvm_rbf12, totab_test[,-12])
cor(totab_psvm_rbf12, totab_test$Totalab)
MAE12<-MAE(totab_psvm_rbf12, totab_test$Totalab)
diff_mae12<-(MAE1-MAE12)
diff_mae12
# -0.2390012

#-p495_1km
set.seed(123)
totab_msvm_rbf13<-ksvm(Totalab ~ ., data = totab_train[,-13], kernel = "rbfdot")
totab_msvm_rbf13
totab_psvm_rbf13<-predict(totab_msvm_rbf13, totab_test[,-13])
cor(totab_psvm_rbf13, totab_test$Totalab)
MAE13<-MAE(totab_psvm_rbf13, totab_test$Totalab)
diff_mae13<-(MAE1-MAE13)
diff_mae13
# -1.729571

#-p498_20km
set.seed(123)
totab_msvm_rbf14<-ksvm(Totalab ~ ., data = totab_train[,-14], kernel = "rbfdot")
totab_msvm_rbf14
totab_psvm_rbf14<-predict(totab_msvm_rbf14, totab_test[,-14])
cor(totab_psvm_rbf14, totab_test$Totalab)
MAE14<-MAE(totab_psvm_rbf14, totab_test$Totalab)
diff_mae14<-(MAE1-MAE14)
diff_mae14
# 0.2505874

#-p556_8km
set.seed(123)
totab_msvm_rbf15<-ksvm(Totalab ~ ., data = totab_train[,-15], kernel = "rbfdot")
totab_msvm_rbf15
totab_psvm_rbf15<-predict(totab_msvm_rbf15, totab_test[,-15])
cor(totab_psvm_rbf15, totab_test$Totalab)
MAE15<-MAE(totab_psvm_rbf15, totab_test$Totalab)
diff_mae15<-(MAE1-MAE15)
diff_mae15
# 0.04935138

#-p557_8km
set.seed(123)
totab_msvm_rbf16<-ksvm(Totalab ~ ., data = totab_train[,-16], kernel = "rbfdot")
totab_msvm_rbf16
totab_psvm_rbf16<-predict(totab_msvm_rbf16, totab_test[,-16])
cor(totab_psvm_rbf16, totab_test$Totalab)
MAE16<-MAE(totab_psvm_rbf16, totab_test$Totalab)
diff_mae16<-(MAE1-MAE16)
diff_mae16
# 0.1132377

#-p558_4km
set.seed(123)
totab_msvm_rbf17<-ksvm(Totalab ~ ., data = totab_train[,-17], kernel = "rbfdot")
totab_msvm_rbf17
totab_psvm_rbf17<-predict(totab_msvm_rbf17, totab_test[,-17])
cor(totab_psvm_rbf17, totab_test$Totalab)
MAE17<-MAE(totab_psvm_rbf17, totab_test$Totalab)
diff_mae17<-(MAE1-MAE17)
diff_mae17
# 0.4453318

#-pADV_400m
set.seed(123)
totab_msvm_rbf18<-ksvm(Totalab ~ ., data = totab_train[,-18], kernel = "rbfdot")
totab_msvm_rbf18
totab_psvm_rbf18<-predict(totab_msvm_rbf18, totab_test[,-18])
cor(totab_psvm_rbf18, totab_test$Totalab)
MAE18<-MAE(totab_psvm_rbf18, totab_test$Totalab)
diff_mae18<-(MAE1-MAE18)
diff_mae18
# -0.2800854

#-pDSD_400m
set.seed(123)
totab_msvm_rbf19<-ksvm(Totalab ~ ., data = totab_train[,-19], kernel = "rbfdot")
totab_msvm_rbf19
totab_psvm_rbf19<-predict(totab_msvm_rbf19, totab_test[,-19])
cor(totab_psvm_rbf19, totab_test$Totalab)
MAE19<-MAE(totab_psvm_rbf19, totab_test$Totalab)
diff_mae19<-(MAE1-MAE19)
diff_mae19
# -1.226612

#-pISNV_20km
set.seed(123)
totab_msvm_rbf20<-ksvm(Totalab ~ ., data = totab_train[,-20], kernel = "rbfdot")
totab_msvm_rbf20
totab_psvm_rbf20<-predict(totab_msvm_rbf20, totab_test[,-20])
cor(totab_psvm_rbf20, totab_test$Totalab)
MAE20<-MAE(totab_psvm_rbf20, totab_test$Totalab)
diff_mae20<-(MAE1-MAE20)
diff_mae20
# -0.0230471

#-pSHV_400m
set.seed(123)
totab_msvm_rbf21<-ksvm(Totalab ~ ., data = totab_train[,-21], kernel = "rbfdot")
totab_msvm_rbf21
totab_psvm_rbf21<-predict(totab_msvm_rbf21, totab_test[,-21])
cor(totab_psvm_rbf21, totab_test$Totalab)
MAE21<-MAE(totab_psvm_rbf21, totab_test$Totalab)
diff_mae21<-(MAE1-MAE21)
diff_mae21
# -0.05214814

#-evd20km
set.seed(123)
totab_msvm_rbf22<-ksvm(Totalab ~ ., data = totab_train[,-22], kernel = "rbfdot")
totab_msvm_rbf22
totab_psvm_rbf22<-predict(totab_msvm_rbf22, totab_test[,-22])
cor(totab_psvm_rbf22, totab_test$Totalab)
MAE22<-MAE(totab_psvm_rbf22, totab_test$Totalab)
diff_mae22<-(MAE1-MAE22)
diff_mae22
# -0.08837876

#-lvd8km
set.seed(123)
totab_msvm_rbf23<-ksvm(Totalab ~ ., data = totab_train[,-23], kernel = "rbfdot")
totab_msvm_rbf23
totab_psvm_rbf23<-predict(totab_msvm_rbf23, totab_test[,-23])
cor(totab_psvm_rbf23, totab_test$Totalab)
MAE23<-MAE(totab_psvm_rbf23, totab_test$Totalab)
diff_mae23<-(MAE1-MAE23)
diff_mae23
# -0.5899033

#-pwm400m
set.seed(123)
totab_msvm_rbf24<-ksvm(Totalab ~ ., data = totab_train[,-24], kernel = "rbfdot")
totab_msvm_rbf24
totab_psvm_rbf24<-predict(totab_msvm_rbf24, totab_test[,-24])
cor(totab_psvm_rbf24, totab_test$Totalab)
MAE24<-MAE(totab_psvm_rbf24, totab_test$Totalab)
diff_mae24<-(MAE1-MAE24)
diff_mae24
# 0.005402787

#-pwt400m
set.seed(123)
totab_msvm_rbf25<-ksvm(Totalab ~ ., data = totab_train[,-25], kernel = "rbfdot")
totab_msvm_rbf25
totab_psvm_rbf25<-predict(totab_msvm_rbf25, totab_test[,-25])
cor(totab_psvm_rbf25, totab_test$Totalab)
MAE25<-MAE(totab_psvm_rbf25, totab_test$Totalab)
diff_mae25<-(MAE1-MAE25)
diff_mae25
# 0.005402787

#-countobs
set.seed(123)
totab_msvm_rbf26<-ksvm(Totalab ~ ., data = totab_train[,-26], kernel = "rbfdot")
totab_msvm_rbf26
totab_psvm_rbf26<-predict(totab_msvm_rbf26, totab_test[,-26])
cor(totab_psvm_rbf26, totab_test$Totalab)
MAE26<-MAE(totab_psvm_rbf26, totab_test$Totalab)
diff_mae26<-(MAE1-MAE26)
diff_mae26
# -0.9145969

#-mean_temp
set.seed(123)
totab_msvm_rbf27<-ksvm(Totalab ~ ., data = totab_train[,-27], kernel = "rbfdot")
totab_msvm_rbf27
totab_psvm_rbf27<-predict(totab_msvm_rbf27, totab_test[,-27])
cor(totab_psvm_rbf27, totab_test$Totalab)
MAE27<-MAE(totab_psvm_rbf27, totab_test$Totalab)
diff_mae27<-(MAE1-MAE27)
diff_mae27
# -0.4148494

#-prop_quality
set.seed(123)
totab_msvm_rbf28<-ksvm(Totalab ~ ., data = totab_train[,-28], kernel = "rbfdot")
totab_msvm_rbf28
totab_psvm_rbf28<-predict(totab_msvm_rbf28, totab_test[,-28])
cor(totab_psvm_rbf28, totab_test$Totalab)
MAE28<-MAE(totab_psvm_rbf28, totab_test$Totalab)
diff_mae28<-(MAE1-MAE28)
diff_mae28
# 1.123591

#-msday
set.seed(123)
totab_msvm_rbf29<-ksvm(Totalab ~ ., data = totab_train[,-29], kernel = "rbfdot")
totab_msvm_rbf29
totab_psvm_rbf29<-predict(totab_msvm_rbf29, totab_test[,-29])
cor(totab_psvm_rbf29, totab_test$Totalab)
MAE29<-MAE(totab_psvm_rbf29, totab_test$Totalab)
diff_mae29<-(MAE1-MAE29)
diff_mae29
# 0.3646448

#-mean_noise
set.seed(123)
totab_msvm_rbf30<-ksvm(Totalab ~ ., data = totab_train[,-30], kernel = "rbfdot")
totab_msvm_rbf30
totab_psvm_rbf30<-predict(totab_msvm_rbf30, totab_test[,-30])
cor(totab_psvm_rbf30, totab_test$Totalab)
MAE30<-MAE(totab_psvm_rbf30, totab_test$Totalab)
diff_mae30<-(MAE1-MAE30)
diff_mae30
# 0.9213096






###CoV ------
CoV_train<-BRSP_CoV[train_sample,]
CoV_test<-BRSP_CoV[-train_sample,]

#"polydot" kernel:
set.seed(123)
CoV_msvm_poly<-ksvm(CoV ~ ., data = CoV_train, kernel = "polydot")
CoV_msvm_poly
CoV_psvm_poly<-predict(CoV_msvm_poly, CoV_test)
cor(CoV_psvm_poly, CoV_test$CoV)
#0.335469 
MAE1<-MAE(CoV_psvm_poly, CoV_test$CoV)
# 0.1761053 

#-p312_400m
set.seed(123)
CoV_msvm_poly2<-ksvm(CoV ~ ., data = CoV_train[,-2], kernel = "polydot")
CoV_msvm_poly2
CoV_psvm_poly2<-predict(CoV_msvm_poly2, CoV_test[,-2])
cor(CoV_psvm_poly2, CoV_test$CoV)
MAE2<-MAE(CoV_psvm_poly2, CoV_test$CoV)
diff_mae2<-(MAE1-MAE2)
diff_mae2
#-0.00002960851

#-p316_8km
set.seed(123)
CoV_msvm_poly3<-ksvm(CoV ~ ., data = CoV_train[,-3], kernel = "polydot")
CoV_msvm_poly3
CoV_psvm_poly3<-predict(CoV_msvm_poly3, CoV_test[,-3])
cor(CoV_psvm_poly3, CoV_test$CoV)
MAE3<-MAE(CoV_psvm_poly3, CoV_test$CoV)
diff_mae3<-(MAE1-MAE3)
diff_mae3
#-0.003341131

#-p437_5km
set.seed(123)
CoV_msvm_poly4<-ksvm(CoV ~ ., data = CoV_train[,-4], kernel = "polydot")
CoV_msvm_poly4
CoV_psvm_poly4<-predict(CoV_msvm_poly4, CoV_test[,-4])
cor(CoV_psvm_poly4, CoV_test$CoV)
MAE4<-MAE(CoV_psvm_poly4, CoV_test$CoV)
diff_mae4<-(MAE1-MAE4)
diff_mae4
#0.0008929581

#-p484_1km
set.seed(123)
CoV_msvm_poly5<-ksvm(CoV ~ ., data = CoV_train[,-5], kernel = "polydot")
CoV_msvm_poly5
CoV_psvm_poly5<-predict(CoV_msvm_poly5, CoV_test[,-5])
cor(CoV_psvm_poly5, CoV_test$CoV)
MAE5<-MAE(CoV_psvm_poly5, CoV_test$CoV)
diff_mae5<-(MAE1-MAE5)
diff_mae5
# -0.005964356

#-p485_20km
set.seed(123)
CoV_msvm_poly6<-ksvm(CoV ~ ., data = CoV_train[,-6], kernel = "polydot")
CoV_msvm_poly6
CoV_psvm_poly6<-predict(CoV_msvm_poly6, CoV_test[,-6])
cor(CoV_psvm_poly6, CoV_test$CoV)
MAE6<-MAE(CoV_psvm_poly6, CoV_test$CoV)
diff_mae6<-(MAE1-MAE6)
diff_mae6
# -0.002039929

#-p488_20km
set.seed(123)
CoV_msvm_poly7<-ksvm(CoV ~ ., data = CoV_train[,-7], kernel = "polydot")
CoV_msvm_poly7
CoV_psvm_poly7<-predict(CoV_msvm_poly7, CoV_test[,-7])
cor(CoV_psvm_poly7, CoV_test$CoV)
MAE7<-MAE(CoV_psvm_poly7, CoV_test$CoV)
diff_mae7<-(MAE1-MAE7)
diff_mae7
# 0.002325602

#-p489_400m
set.seed(123)
CoV_msvm_poly8<-ksvm(CoV ~ ., data = CoV_train[,-8], kernel = "polydot")
CoV_msvm_poly8
CoV_psvm_poly8<-predict(CoV_msvm_poly8, CoV_test[,-8])
cor(CoV_psvm_poly8, CoV_test$CoV)
MAE8<-MAE(CoV_psvm_poly8, CoV_test$CoV)
diff_mae8<-(MAE1-MAE8)
diff_mae8
# -0.009294738

#-p490_400m
set.seed(123)
CoV_msvm_poly9<-ksvm(CoV ~ ., data = CoV_train[,-9], kernel = "polydot")
CoV_msvm_poly9
CoV_psvm_poly9<-predict(CoV_msvm_poly9, CoV_test[,-9])
cor(CoV_psvm_poly9, CoV_test$CoV)
MAE9<-MAE(CoV_psvm_poly9, CoV_test$CoV)
diff_mae9<-(MAE1-MAE9)
diff_mae9
# -0.00436172

#-p491_20km
set.seed(123)
CoV_msvm_poly10<-ksvm(CoV ~ ., data = CoV_train[,-10], kernel = "polydot")
CoV_msvm_poly10
CoV_psvm_poly10<-predict(CoV_msvm_poly10, CoV_test[,-10])
cor(CoV_psvm_poly10, CoV_test$CoV)
MAE10<-MAE(CoV_psvm_poly10, CoV_test$CoV)
diff_mae10<-(MAE1-MAE10)
diff_mae10
# -0.01599372

#-p492_5km
set.seed(123)
CoV_msvm_poly11<-ksvm(CoV ~ ., data = CoV_train[,-11], kernel = "polydot")
CoV_msvm_poly11
CoV_psvm_poly11<-predict(CoV_msvm_poly11, CoV_test[,-11])
cor(CoV_psvm_poly11, CoV_test$CoV)
MAE11<-MAE(CoV_psvm_poly11, CoV_test$CoV)
diff_mae11<-(MAE1-MAE11)
diff_mae11
# 0.001847503

#-p493_4km
set.seed(123)
CoV_msvm_poly12<-ksvm(CoV ~ ., data = CoV_train[,-12], kernel = "polydot")
CoV_msvm_poly12
CoV_psvm_poly12<-predict(CoV_msvm_poly12, CoV_test[,-12])
cor(CoV_psvm_poly12, CoV_test$CoV)
MAE12<-MAE(CoV_psvm_poly12, CoV_test$CoV)
diff_mae12<-(MAE1-MAE12)
diff_mae12
# -0.002020721

#-p495_1km
set.seed(123)
CoV_msvm_poly13<-ksvm(CoV ~ ., data = CoV_train[,-13], kernel = "polydot")
CoV_msvm_poly13
CoV_psvm_poly13<-predict(CoV_msvm_poly13, CoV_test[,-13])
cor(CoV_psvm_poly13, CoV_test$CoV)
MAE13<-MAE(CoV_psvm_poly13, CoV_test$CoV)
diff_mae13<-(MAE1-MAE13)
diff_mae13
# -0.002115517

#-p498_20km
set.seed(123)
CoV_msvm_poly14<-ksvm(CoV ~ ., data = CoV_train[,-14], kernel = "polydot")
CoV_msvm_poly14
CoV_psvm_poly14<-predict(CoV_msvm_poly14, CoV_test[,-14])
cor(CoV_psvm_poly14, CoV_test$CoV)
MAE14<-MAE(CoV_psvm_poly14, CoV_test$CoV)
diff_mae14<-(MAE1-MAE14)
diff_mae14
#-0.00388595

#-p556_8km
set.seed(123)
CoV_msvm_poly15<-ksvm(CoV ~ ., data = CoV_train[,-15], kernel = "polydot")
CoV_msvm_poly15
CoV_psvm_poly15<-predict(CoV_msvm_poly15, CoV_test[,-15])
cor(CoV_psvm_poly15, CoV_test$CoV)
MAE15<-MAE(CoV_psvm_poly15, CoV_test$CoV)
diff_mae15<-(MAE1-MAE15)
diff_mae15
# 0.0004627835

#-p557_8km
set.seed(123)
CoV_msvm_poly16<-ksvm(CoV ~ ., data = CoV_train[,-16], kernel = "polydot")
CoV_msvm_poly16
CoV_psvm_poly16<-predict(CoV_msvm_poly16, CoV_test[,-16])
cor(CoV_psvm_poly16, CoV_test$CoV)
MAE16<-MAE(CoV_psvm_poly16, CoV_test$CoV)
diff_mae16<-(MAE1-MAE16)
diff_mae16
# -0.0002971532

#-p558_4km
set.seed(123)
CoV_msvm_poly17<-ksvm(CoV ~ ., data = CoV_train[,-17], kernel = "polydot")
CoV_msvm_poly17
CoV_psvm_poly17<-predict(CoV_msvm_poly17, CoV_test[,-17])
cor(CoV_psvm_poly17, CoV_test$CoV)
MAE17<-MAE(CoV_psvm_poly17, CoV_test$CoV)
diff_mae17<-(MAE1-MAE17)
diff_mae17
# 0.0003144677

#-pADV_400m
set.seed(123)
CoV_msvm_poly18<-ksvm(CoV ~ ., data = CoV_train[,-18], kernel = "polydot")
CoV_msvm_poly18
CoV_psvm_poly18<-predict(CoV_msvm_poly18, CoV_test[,-18])
cor(CoV_psvm_poly18, CoV_test$CoV)
MAE18<-MAE(CoV_psvm_poly18, CoV_test$CoV)
diff_mae18<-(MAE1-MAE18)
diff_mae18
# 0.0008221541

#-pDSD_400m
set.seed(123)
CoV_msvm_poly19<-ksvm(CoV ~ ., data = CoV_train[,-19], kernel = "polydot")
CoV_msvm_poly19
CoV_psvm_poly19<-predict(CoV_msvm_poly19, CoV_test[,-19])
cor(CoV_psvm_poly19, CoV_test$CoV)
MAE19<-MAE(CoV_psvm_poly19, CoV_test$CoV)
diff_mae19<-(MAE1-MAE19)
diff_mae19
# 0.002848715

#-pISNV_20km
set.seed(123)
CoV_msvm_poly20<-ksvm(CoV ~ ., data = CoV_train[,-20], kernel = "polydot")
CoV_msvm_poly20
CoV_psvm_poly20<-predict(CoV_msvm_poly20, CoV_test[,-20])
cor(CoV_psvm_poly20, CoV_test$CoV)
MAE20<-MAE(CoV_psvm_poly20, CoV_test$CoV)
diff_mae20<-(MAE1-MAE20)
diff_mae20
# 0.0001690299

#-pSHV_400m
set.seed(123)
CoV_msvm_poly21<-ksvm(CoV ~ ., data = CoV_train[,-21], kernel = "polydot")
CoV_msvm_poly21
CoV_psvm_poly21<-predict(CoV_msvm_poly21, CoV_test[,-21])
cor(CoV_psvm_poly21, CoV_test$CoV)
MAE21<-MAE(CoV_psvm_poly21, CoV_test$CoV)
diff_mae21<-(MAE1-MAE21)
diff_mae21
# -0.0009736455

#-evd20km
set.seed(123)
CoV_msvm_poly22<-ksvm(CoV ~ ., data = CoV_train[,-22], kernel = "polydot")
CoV_msvm_poly22
CoV_psvm_poly22<-predict(CoV_msvm_poly22, CoV_test[,-22])
cor(CoV_psvm_poly22, CoV_test$CoV)
MAE22<-MAE(CoV_psvm_poly22, CoV_test$CoV)
diff_mae22<-(MAE1-MAE22)
diff_mae22
# 0.0006697302

#-lvd8km
set.seed(123)
CoV_msvm_poly23<-ksvm(CoV ~ ., data = CoV_train[,-23], kernel = "polydot")
CoV_msvm_poly23
CoV_psvm_poly23<-predict(CoV_msvm_poly23, CoV_test[,-23])
cor(CoV_psvm_poly23, CoV_test$CoV)
MAE23<-MAE(CoV_psvm_poly23, CoV_test$CoV)
diff_mae23<-(MAE1-MAE23)
diff_mae23
# 0.00148726

#-pwm400m
set.seed(123)
CoV_msvm_poly24<-ksvm(CoV ~ ., data = CoV_train[,-24], kernel = "polydot")
CoV_msvm_poly24
CoV_psvm_poly24<-predict(CoV_msvm_poly24, CoV_test[,-24])
cor(CoV_psvm_poly24, CoV_test$CoV)
MAE24<-MAE(CoV_psvm_poly24, CoV_test$CoV)
diff_mae24<-(MAE1-MAE24)
diff_mae24
# 0.0000941859

#-pwt400m
set.seed(123)
CoV_msvm_poly25<-ksvm(CoV ~ ., data = CoV_train[,-25], kernel = "polydot")
CoV_msvm_poly25
CoV_psvm_poly25<-predict(CoV_msvm_poly25, CoV_test[,-25])
cor(CoV_psvm_poly25, CoV_test$CoV)
MAE25<-MAE(CoV_psvm_poly25, CoV_test$CoV)
diff_mae25<-(MAE1-MAE25)
diff_mae25
# 0.0000941859

#-countobs
set.seed(123)
CoV_msvm_poly26<-ksvm(CoV ~ ., data = CoV_train[,-26], kernel = "polydot")
CoV_msvm_poly26
CoV_psvm_poly26<-predict(CoV_msvm_poly26, CoV_test[,-26])
cor(CoV_psvm_poly26, CoV_test$CoV)
MAE26<-MAE(CoV_psvm_poly26, CoV_test$CoV)
diff_mae26<-(MAE1-MAE26)
diff_mae26
# -0.001681674

#-mean_temp
set.seed(123)
CoV_msvm_poly27<-ksvm(CoV ~ ., data = CoV_train[,-27], kernel = "polydot")
CoV_msvm_poly27
CoV_psvm_poly27<-predict(CoV_msvm_poly27, CoV_test[,-27])
cor(CoV_psvm_poly27, CoV_test$CoV)
MAE27<-MAE(CoV_psvm_poly27, CoV_test$CoV)
diff_mae27<-(MAE1-MAE27)
diff_mae27
# -0.005723867

#-prop_quality
set.seed(123)
CoV_msvm_poly28<-ksvm(CoV ~ ., data = CoV_train[,-28], kernel = "polydot")
CoV_msvm_poly28
CoV_psvm_poly28<-predict(CoV_msvm_poly28, CoV_test[,-28])
cor(CoV_psvm_poly28, CoV_test$CoV)
MAE28<-MAE(CoV_psvm_poly28, CoV_test$CoV)
diff_mae28<-(MAE1-MAE28)
diff_mae28
# 0.0009213077

#-msday
set.seed(123)
CoV_msvm_poly29<-ksvm(CoV ~ ., data = CoV_train[,-29], kernel = "polydot")
CoV_msvm_poly29
CoV_psvm_poly29<-predict(CoV_msvm_poly29, CoV_test[,-29])
cor(CoV_psvm_poly29, CoV_test$CoV)
MAE29<-MAE(CoV_psvm_poly29, CoV_test$CoV)
diff_mae29<-(MAE1-MAE29)
diff_mae29
# 0.006346428

#-mean_noise
set.seed(123)
CoV_msvm_poly30<-ksvm(CoV ~ ., data = CoV_train[,-30], kernel = "polydot")
CoV_msvm_poly30
CoV_psvm_poly30<-predict(CoV_msvm_poly30, CoV_test[,-30])
cor(CoV_psvm_poly30, CoV_test$CoV)
MAE30<-MAE(CoV_psvm_poly30, CoV_test$CoV)
diff_mae30<-(MAE1-MAE30)
diff_mae30
# -0.001902978

