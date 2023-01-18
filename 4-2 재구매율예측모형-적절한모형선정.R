#모형 구축
##인공신경망모형
library(nnet)
md<-nnet(re_charge~., size=2, decay=5e-04,rang=0.1, maxit=1000, data=train_pca)
#save(md, file="../../3. DeepD_데이터 및 모델세이브파일/r_model/nnet_md.RData")
#load('../../3. DeepD_데이터 및 모델세이브파일/r_model/nnet_md.RData')
pred<-predict(md, test_pca, type="class")
pred<-as.factor(pred)

#과적합검증
cv_results <- lapply(folds, function(x) {
  training <- data.frame(df3[x, ])
  testing <- data.frame(df3[-x, ])
  
  sms_model1 <- nnet(re_charge~., size=2, decay=5e-04,rang=0.1, maxit=200, data=training)
  
  credit_pred <- predict(sms_model1, testing, type="raw")
  df_pred<-as.data.frame(credit_pred)
  
  df_pred$default<-ifelse(df_pred[,1]>=0.5,
                          df_pred$default<-1, df_pred$default<-0)
  df_pred$default<-as.factor(df_pred$default)
  testing$re_charge<-as.factor(testing$re_charge)
  
  cm1 <- caret::confusionMatrix(df_pred$default, testing[,7], positive="1")
  
  #a<-append(auc(actual=testing[,7], predicted=df_pred$default))
  return(cm1$overall[[1]])
})
str(cv_results) #정확도 비슷함=> 과적합 아님
mean(unlist(cv_results)) 

#모형검증-counfisionMatrix
cm<-caret::confusionMatrix(data=pred, reference=test_pca[,7], positive="1")
cm
#모형검증-HSS
PCM<-cm$overall[[1]]
table<-cm$table
PCR<-(sum(table[1,])*sum(table[,1])+sum(table[2,])*sum(table[,2]))/(sum(table))^2
HSS<-(PCM-PCR)/(1-PCR)
HSS
#모형검증-AUC
ModelMetrics::auc(actual=test_pca[,7], predicted=pred)



##부스팅모형

#라이브러리
library(adabag)

#적합
#load('../../3. DeepD_데이터 및 모델세이브파일/r_model/boosting_md.RData')
boo.fit<-boosting(re_charge~., data=train_pca, boos=TRUE, mfinal=100)
#save(boo.fit, file="../../3. DeepD_데이터 및 모델세이브파일/r_model/boosting_md.RData")
#load('../../3. DeepD_데이터 및 모델세이브파일/r_model/boosting_md.RData')

#트리 시각화
plot(boo.fit$trees[[1000]])
text(boo.fit$trees[[1000]])


#변수중요도 시각화
boo.fit$importance
barplot(boo.fit$importance[order(boo.fit$importance, decreasing = TRUE)],
        ylim=c(1,100),
        main="Variables Relative Importance")


#test data로 예측
xgb.pred<-predict(boo.fit, newdata = test_pca)


#예측값 형태 변형(문자형->팩터형)
xgb.pred$class<-as.factor(xgb.pred$class)


#모형검증-counfisionMatrix
cm<-caret::confusionMatrix(xgb.pred$class, reference=test_pca[,7], positive="1")
cm

#모형검증-HSS
PCM<-cm$overall[[1]]
table<-cm$table
PCR<-(sum(table[1,])*sum(table[,1])+sum(table[2,])*sum(table[,2]))/(sum(table))^2
HSS<-(PCM-PCR)/(1-PCR)
HSS
#모형검증-auc
auc(actual=test_pca[,7], predicted=xgb.pred$class)

####앙상블 배깅모형
library(adabag)
md.bagging<-bagging(re_charge~., data=train_pca, mfinal=1000)
#save(md.bagging, file='../../3. DeepD_데이터 및 모델세이브파일/r_model/bagging_md.RData')
#load('../../3. DeepD_데이터 및 모델세이브파일/r_data/bagging_md.RData')
ls(md.bagging)

#시각화
md.bagging$trees[[100]]
plot(md.bagging$trees[[1]])
text(md.bagging$trees[[1]])
md.bagging$importance
barplot(md.bagging$importance[order(md.bagging$importance, decreasing = TRUE)],
        ylim=c(1,100),
        main="Variables Relative Importance")

#test data로 예측
pred<-predict(md.bagging, test_pca, type="class")

#예측값 형태 변형(문자형->팩터형)
pred$class<-as.factor(pred$class)

#모형검증-counfisionMatrix
cm<-caret::confusionMatrix(pred$class, reference=test_pca[,7], positive="1")
cm

#모형검증-HSS
PCM<-cm$overall[[1]]
table<-cm$table
PCR<-(sum(table[1,])*sum(table[,1])+sum(table[2,])*sum(table[,2]))/(sum(table))^2
HSS<-(PCM-PCR)/(1-PCR)
HSS

#모형검증-auc
ModelMetrics::auc(actual=test_pca[,7], predicted=pred)