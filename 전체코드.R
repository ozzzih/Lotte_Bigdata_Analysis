#####데이터 불러오기######
library(data.table)
d_01<-fread("LPOINT_BIG_COMP_01_DEMO.csv")
d_02<-fread("LPOINT_BIG_COMP_02_PDDE.csv")
d_03<-fread("LPOINT_BIG_COMP_03_COP_U.csv")
d_04<-fread("LPOINT_BIG_COMP_04_PD_CLAC.csv")
d_05<-fread("LPOINT_BIG_COMP_05_BR.csv")


########################################################################
#외부데이터#############################################################
########################################################################
#불러오기
f_20<-readxl::read_excel('f_20.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
f_30<-readxl::read_excel('f_30.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
f_40<-readxl::read_excel('f_40.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
f_50<-readxl::read_excel('f_50.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
f_6070<-readxl::read_excel('f_6070.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
m_20<-readxl::read_excel('m_20.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
m_30<-readxl::read_excel('m_30.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
m_40<-readxl::read_excel('m_40.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
m_50<-readxl::read_excel('m_50.xlsx', range=cellranger::cell_rows(7:16), col_names = T)
m_6070<-readxl::read_excel('m_6070.xlsx', range=cellranger::cell_rows(7:16), col_names = T)

f_20<-sum(as.numeric(f_20$와인), as.numeric(f_20$맥주),
          as.numeric(f_20$소주), as.numeric(f_20$양주),
          as.numeric(f_20$전통주))
f_30<-sum(as.numeric(f_30$와인), as.numeric(f_30$맥주),
          as.numeric(f_30$소주), as.numeric(f_30$양주),
          as.numeric(f_30$전통주))
f_40<-sum(as.numeric(f_40$와인), as.numeric(f_40$맥주),
          as.numeric(f_40$소주), as.numeric(f_40$양주),
          as.numeric(f_40$전통주))
f_50<-sum(as.numeric(f_50$와인), as.numeric(f_50$맥주),
          as.numeric(f_50$소주), as.numeric(f_50$양주),
          as.numeric(f_50$전통주))
f_6070<-sum(as.numeric(f_6070$와인), as.numeric(f_6070$맥주),
            as.numeric(f_6070$소주), as.numeric(f_6070$양주),
            as.numeric(f_6070$전통주))
m_20<-sum(as.numeric(m_20$와인), as.numeric(m_20$맥주),
          as.numeric(m_20$소주), as.numeric(m_20$양주),
          as.numeric(m_20$전통주))
m_30<-sum(as.numeric(m_30$와인), as.numeric(m_30$맥주),
          as.numeric(m_30$소주), as.numeric(m_30$양주),
          as.numeric(m_30$전통주))
m_40<-sum(as.numeric(m_40$와인), as.numeric(m_40$맥주),
          as.numeric(m_40$소주), as.numeric(m_40$양주),
          as.numeric(m_40$전통주))
m_50<-sum(as.numeric(m_50$와인), as.numeric(m_50$맥주),
          as.numeric(m_50$소주), as.numeric(m_50$양주),
          as.numeric(m_50$전통주))
m_6070<-sum(as.numeric(m_6070$와인), as.numeric(m_6070$맥주),
            as.numeric(m_6070$소주), as.numeric(m_6070$양주),
            as.numeric(m_6070$전통주))

ma_fem_dv<-c(rep("여성",6),rep("남성",6))
ages<-c(rep(c("20대","30대","40대","50대","60대","70대"),2))
liked<-c(f_20,f_30,f_40,f_50,f_6070,f_6070,m_20,m_30,m_40,m_50,m_6070,m_6070)

#활용할 외부데이터 형식
df_liked<-data.frame(ages, ma_fem_dv, liked)

########################################################################
#데이터 전처리###################################
########################################################################
#고객정보, 상품정보 병합하기
library(dplyr)
#d_02데이터
df2<-left_join(d_02, d_01, by="cust")#고객정보조인
df2<-left_join(df2, d_04, by='pd_c')#상품정보조인
#d_03데이터
df3<-left_join(d_03, d_01, by="cust")

########Data check############
#missing value check
sum(!complete.cases(d_01))
sum(!complete.cases(d_02))
sum(!complete.cases(d_03))
sum(!complete.cases(d_04))
sum(!complete.cases(d_05))
sum(!complete.cases(d_06))

#Outlier check
summary(d_01)
summary(d_02)
summary(d_03)
summary(d_04)
summary(d_05)
summary(d_06)

#as.Date
df2$de_dt<- as.Date(as.character(df2$de_dt), format = '%Y%m%d')

########################################################################
#EDA
########################################################################


#지역코드 추론: 지역코드 대분류별 중분류개수 확인
a<- d_05 %>% group_by(zon_hlv) %>% summarize(count=n_distinct(zon_mcls))

#유통사코드 추론
a<- d_05 %>% group_by(zon_hlv, cop_c ) %>% summarize(count=n_distinct(br_c))

#18p-EDA

#상품구매 데이터 고객수 확인
length(unique(df2$cust)) #1~12월: 26,917명
nrow(unique(df2[df2$de_dt<=as.Date("2021-09-30"),1])) #1~9월: 26,150명

#주류 구매 고객 추출(14,266명)
drink<-df2 %>% filter(clac_hlv_nm=="주류")
#load("../../3. DeepD_데이터 및 모델세이브파일/r_data/주류구매데이터.RData")
length(unique(drink$cust))
(length(unique(drink$cust))/length(unique(df2$cust)))*100 #53%


#기준시점(2021-10-01)전에 주류구매가 일어난 사람:13,019명
drink_bf<-drink %>% filter(de_dt<=as.Date("2021-09-31"))
length(unique(drink_bf$cust)) #13,019명
c_u<-unique(drink_bf$cust)
df<-as.data.frame(c_u)
colnames(df)<-"cust"

#기준시점(2021-10-01)이후에 주류 재구매가 일어난 사람:6,389명
re<-drink_af[cust %in% c_u,]
length(unique(re$cust))
c_re<-unique(re$cust)



#전체 영수증 중 주류가 포함된 비율

a<-unique(drink$rct_no)
length(a)
(length(a)/length(unique(df2$rct_no)))*100


#성별 재구매율 확인
## 여성이 재구매한 비율
drink_bf<-drink %>% filter(de_dt<=20210931 & ma_fem_dv=="여성")
length(unique(drink_bf$cust)) #8829
c_u<-unique(drink_bf$cust)
drink_af<-drink %>% filter(de_dt>20210931)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re$cust)) #4249
4249/8829 #0.481
## 남성이 재구매한 비율
drink_bf<-drink %>% filter(de_dt<=20210931 & ma_fem_dv=="남성")
length(unique(drink_bf$cust)) #4190
c_u<-unique(drink_bf$cust)
drink_af<-drink %>% filter(de_dt>20210931)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re$cust)) #2140
2140/4190 #0.511

#유통사별 재구매율
##A01
drink_bf<-drink %>% filter(de_dt<=20210931& cop_c=="A01")
length(unique(drink_bf$cust)) #1459
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$cop_c=="A01",1]))#338
338/1459 #0.232

##A02
drink_bf<-drink %>% filter(de_dt<=20210931& cop_c=="A02")
length(unique(drink_bf$cust)) #8524
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$cop_c=="A02",1]))#3653
3653/8524 #0.429


##A03
drink_bf<-drink %>% filter(de_dt<=20210931& cop_c=="A03")
length(unique(drink_bf$cust)) #3936
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$cop_c=="A03",1]))#1678
1678/3936 #0.426


##A04
drink_bf<-drink %>% filter(de_dt<=20210931& cop_c=="A04")
length(unique(drink_bf$cust)) #2591
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$cop_c=="A04",1]))#861
861/2591 #0.332

#주류 중분류별 재구매율
##소주 재구매율
drink_bf<-drink %>% filter(de_dt<=20210931& clac_mcls_nm=="소주")
length(unique(drink_bf$cust)) #6489
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$clac_mcls_nm=="소주",1]))#2485
2485/6489 #0.383

##맥주 재구매율
drink_bf<-drink %>% filter(de_dt<=20210931& clac_mcls_nm=="맥주")
length(unique(drink_bf$cust)) #9744
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$clac_mcls_nm=="맥주",1]))#3968
3968/9744 #0.407

##양주 재구매율
drink_bf<-drink %>% filter(de_dt<=20210931& clac_mcls_nm=="양주")
length(unique(drink_bf$cust)) #1174
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$clac_mcls_nm=="양주",1]))#175
175/1174 #0.149


##와인 재구매율
drink_bf<-drink %>% filter(de_dt<=20210931& clac_mcls_nm=="와인")
length(unique(drink_bf$cust)) #3602
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$clac_mcls_nm=="와인",1]))#1043
1043/3602 #0.29


##전통주 재구매율
drink_bf<-drink %>% filter(de_dt<=20210931& clac_mcls_nm=="전통주")
length(unique(drink_bf$cust)) #4818
c_u<-unique(drink_bf$cust)
re<-drink_af[drink_af$cust %in% c_u,]
length(unique(re[re$clac_mcls_nm=="전통주",1]))#1289
1289/4818 #0.27





########################################################################
#재구매지수 예측모형###################################
########################################################################
#파생변수 생성

###re_charge변수만들기
x<-as.data.frame(c_re)
x$re<-1
colnames(x)<-c("cust","re_charge")
df<-left_join(df, x, by="cust")
df[is.na(df)]<-0
table(df$re)

###amount변수만들기
amount<-drink_bf %>% group_by(cust) %>% summarize(amount=sum(buy_am))
df<-left_join(df, amount, by='cust')

###m_amount변수 만들기
m_amount<-drink_bf %>% group_by(cust) %>% summarize(m_amount=sum(buy_am)/n_distinct(rct_no))
df<-left_join(df, m_amount, by='cust')

###bk변수 만들기
bk<-drink_bf %>% group_by(cust) %>% summarize(bk=difftime(as.Date('2021-10-01'), max(de_dt), units = "days"))
df<-left_join(df, bk, by='cust')

###freq변수 만들기
freq<-drink_bf %>% group_by(cust) %>% summarize(freq=n_distinct(rct_no))
df<-left_join(df, freq, by='cust')

###break_freq변수만들기
df$bk<-as.numeric(df$bk)
df$break_freq<-df$bk/df$freq

###rec_trd변수 만들기
df<-arrange(df, cust)
cust<-NULL
rec_trd<-NULL
for (i in 1:nrow(df)){
  dts_r<-unique(drink_bf[cust==df$cust[i],c(2,7)])
  dts<-dts_r$de_dt
  mdt<-min(dts)
  rec<-NULL
  for (j in 1:length(dts)){
    rec<-append(rec, difftime(dts[j], as.Date('2021-01-01'), units = "days")+1)
  }
  cust<-append(cust, df$cust[i])
  rec_trd<-append(rec_trd, sum(as.numeric(rec)))
}
r<-cbind(cust,rec_trd)
r<-as.data.frame(r)
r$rec_trd<-as.numeric(r$rec_trd)
str(r)
df<-left_join(df, r, by='cust')

###m_rec_trd변수만들기
df$m_rec_trd<-df$rec_trd/df$freq

###trd_skdw변수 만들기
cust<-NULL
trd_skdw<-NULL
for (i in 1:nrow(df)){
  dts_r<-unique(drink_bf[cust==df$cust[i],c(2,7)])
  dts<-dts_r$de_dt
  mdt<-min(dts)
  x<-as.numeric(difftime(as.Date('2021-10-01'),mdt, units = "days"))/2
  stand<-mdt+x
  trd<-NULL
  for (j in 1:length(dts)){
    trd<-append(trd, abs(difftime(dts[j],stand, units = "days")))
  }
  cust<-append(cust, df$cust[i])
  trd_skdw<-append(trd_skdw, sum(as.numeric(trd)))
}
r<-cbind(cust,trd_skdw)
r<-as.data.frame(r)
r$trd_skdw<-as.numeric(r$trd_skdw)
df<-left_join(df, r, by='cust')

###m_trd_skdw변수만들기
df$m_trd_skdw<-df$trd_skdw/df$freq


###age조인 시키기
df<-left_join(df, d_01[,c(1,3)], by='cust')

###liked변수 만들기
df<-left_join(df, d_01[,c(1,2)], by='cust')
df<-left_join(df, df_liked, by=c('ages','ma_fem_dv'))
df<-df[,-14]
#write.csv(df, "df.csv") #재구매지수 예측모델 데이터 완성




#재구매 여부에 따른 변수들의 평균 비교
#df<-read.csv("df.csv")
#df<-df[,-1]
cu<-df[,1]#고객명을 따로 저장
df<-df[,-1]#고객명을 기존 데이터에서 삭제

## age변수(범주형변수) 확인(카이스퀘어 검정)
library(dplyr)
df %>% group_by(ages, re_charge) %>% summarise(cnt=n())
x<-matrix(c(696, 489,
            1739, 1752,
            2304, 2506,
            1317, 1199,
            443, 340,
            131, 103), nrow=2, ncol=6)
x<-as.data.frame(x)
colnames(x)<-c("20대","30대","40대","50대","60대","70대")
rownames(x)<-c("0","1")

chisq.test(x) #재구매 여부에 따라 연령대의 비율이 유의한 차이가 있다.

## 나머지 변수(연속형변수) 확인(t 검정)
t.test(df[df$re_charge=="0","amount"], df[df$re_charge=="1","amount"], var.equal=T)
t.test(df[df$re_charge=="0","m_amount"], df[df$re_charge=="1","m_amount"], var.equal=T)
t.test(df[df$re_charge=="0","bk"], df[df$re_charge=="1","bk"], var.equal=T)
t.test(df[df$re_charge=="0","freq"], df[df$re_charge=="1","freq"], var.equal=T)
t.test(df[df$re_charge=="0","break_freq"], df[df$re_charge=="1","break_freq"], var.equal=T)
t.test(df[df$re_charge=="0","rec_trd"], df[df$re_charge=="1","rec_trd"], var.equal=T)
t.test(df[df$re_charge=="0","m_rec_trd"], df[df$re_charge=="1","m_rec_trd"], var.equal=T)
t.test(df[df$re_charge=="0","trd_skdw"], df[df$re_charge=="1","trd_skdw"], var.equal=T)
t.test(df[df$re_charge=="0","m_trd_skdw"], df[df$re_charge=="1","m_trd_skdw"], var.equal=T)
t.test(df[df$re_charge=="0","liked"], df[df$re_charge=="1","liked"], var.equal=T)
#m_amount변수 제외
df_new<- df[,-3]

#표준화작업
df_new[,c(2:9,11)]<-scale(df_new[,c(2:9,11)])

#원핫인코딩
library(caret)
dmy <- dummyVars(~., data = df_new)
df_new <- data.frame(predict(dmy, newdata = df_new))

#다중공선성확인
library(fmsb)
X<-df_new[,c(2:9,16)]
VIF(lm(amount~., data=X))
VIF(lm(bk~., data=X))
VIF(lm(freq~., data=X))
VIF(lm(break_freq~., data=X))
VIF(lm(rec_trd~., data=X))
VIF(lm(m_rec_trd~., data=X))
VIF(lm(trd_skdw~., data=X))
VIF(lm(m_trd_skdw~., data=X))
VIF(lm(liked~., data=X))

#주성분분석
pca_df<- prcomp(df_new[,-1],center=T, scale=T )
pca_df #주성분별 변수 영향도 확인
summary(pca_df) #6개까지:80%까지 설명


df2<-as.matrix(df_new[,-1]) %*% pca_df$rotation[,1:6]
df3<-cbind(df2, as.data.frame(df_new$re_charge))
colnames(df3)[7]<-'re_charge'
summary(df3) #이상치 확인
#save(df3, file="../../3. DeepD_데이터 및 모델세이브파일/r_data/r_input.RData")
#load('../../3. DeepD_데이터 및 모델세이브파일/r_data/r_input.RData')


#모델학습에 있어서 이상치는 민감하기 때문에 이상치를 제거 후 학습
#모델 구축 후, 예측 대상에는 포함.
df3_1<-df3[-12018,] 
df3_1<-df3_1[-789,]

#데이터 분리
set.seed(123)

idx_pca<-createDataPartition(df3_1$re_charge, p=0.7)
test_pca<-data.frame(df3_1[-idx_pca$Resample1,])
train_pca<-data.frame(df3_1[idx_pca$Resample1,])

train_pca$re_charge<-as.factor(train_pca$re_charge)
test_pca$re_charge<-as.factor(test_pca$re_charge)

#데이터 전처리 끝
###################################################################

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




########################################################################
####앙상블 부스팅 모형으로 전체 예측####################################
########################################################################

getwd()
load('../../3. DeepD_데이터 및 모델세이브파일/r_data/r_input.RData')
load('../../3. DeepD_데이터 및 모델세이브파일/r_model/boosting_md.RData')
all_pred<-predict(boo.fit, newdata = df3)
#save(all_pred, file="../../3. DeepD_데이터 및 모델세이브파일/r_data/r_output.RData")
prob<-all_pred$prob
prob<-as.data.frame(prob[,2])
t_df<-cbind(cu,df3[,7], prob)
colnames(t_df)<-c('cust','realorder','prob')


########################################################################
####분포확인############################################################
########################################################################
cut_value <- cut(t_df$prob, breaks= 10) #계급:10개
table(cut_value)
#실제재구매율 확인
table(t_df[t_df$prob<=0.376&t_df$prob>0.301,2])


########################################################################
####타겟 추출###########################################################
########################################################################
#0.301~0.899
library(dplyr)
t_df<-arrange(t_df, prob)
target<-t_df[1775:11860,]
target<-unique(target$cust)
#총 10,086명

########################################################################
####타겟고객의 구매이력 데이터 추출#####################################
########################################################################
#load("../../3. DeepD_데이터 및 모델세이브파일/r_data/주류구매데이터.RData")
data<-drink[drink$cust %in% target,] #89,219건
write.csv(data, "data.csv")

########################################################################
####파이썬분석 시작#####################################
########################################################################

#######################################################################
####연관분석###########################################################
#######################################################################



a<-unique(drink$rct_no)
anju<-df2[df2$rct_no %in% a,]
li<-c("주류","냉장식품", "채소", "축산물","과자","음료","유제품","과일",
     "커피/차", "냉동식품", "대용식", "수산물",
     "병통조림", '조리식품', "양곡", "건해산물", "건강식품", "유아식품")
anju_c<-anju[anju$clac_hlv_nm %in% li,]
write.csv(anju_c,"anju_c.csv")
anju_c<-read.csv("anju_c.csv")
anju_c<-anju_c[,-1]
anju_c[anju_c$pd_nm=="와인세트",13]<-"와인"
anju_c[anju_c$pd_nm=="양주세트",13]<-"양주"
anju_c[anju_c$pd_nm=="전통주세트",13]<-"전통주"

A1<-anju_c[anju_c$cop_c=="A01",]
A2<-anju_c[anju_c$cop_c=="A02",]
A3<-anju_c[anju_c$cop_c=="A03",]
A4<-anju_c[anju_c$cop_c=="A04",]
A6<-anju_c[anju_c$cop_c=="A06",]


write.csv(A1, "A1.csv")
write.csv(A2, "A2.csv")
write.csv(A3, "A3.csv")
write.csv(A4, "A4.csv")
write.csv(A6, "A6.csv")

library(arules)
trans1<-read.transactions("A1.csv", format="single", cols=c(3,14), sep=",", rm.duplicates=T)
trans2<-read.transactions("A2.csv", format="single", cols=c(3,14), sep=",", rm.duplicates=T)
trans3<-read.transactions("A3.csv", format="single", cols=c(3,14), sep=",", rm.duplicates=T)
trans4<-read.transactions("A4.csv", format="single", cols=c(3,14), sep=",", rm.duplicates=T)

rule_s <- apriori(trans1, parameter=list(support=10/4280,confidence=0.1, minlen=2, maxlen=10),
                  appearance=list(rhs="맥주",default='lhs'))

rule_s <- sort(rule_s, by='confidence')

r<-as.data.frame(inspect(rule_s))

###숙박제휴사랑 술구매랑 어떤 관련이 있는지
#a<-drink[drink$clac_mcls_nm=="와인",]
a_w<-unique(drink$cust)

#숙박업체 이용 데이터(고객수: 2,199명)
hotel<-d_03[cop_c=='B01',]
hotel$vst_dt <-as.Date(as.character(hotel$vst_dt), format = '%Y%m%d')
length(unique(hotel$cust))

#주류와 숙박을 구매한 이력이 있는 고객(고객수: 1437명)
h<-hotel[cust %in% a_w,]
h_nw<-hotel[!(cust %in% a_w),]
b<-unique(h$cust)

#1,437명의 숙박이용 데이터 
hotel_w<-hotel[hotel$cust %in% b,]

##숙박이용 하루전 주류 구매 여부확인
result<-NULL
for (i in 1:length(b)){
  w<-a[a$cust==b[i],]
  h<-hotel_w[hotel_w$cust==b[i],]
  for (j in 1:nrow(h)){
    if (any((h$vst_dt[j] - 1)<=w$de_dt & w$de_dt<=h$vst_dt)) {
      result=append(result,1)
    }
    else{
      result=append(result,0)
    }
  }
}
a<-a_w[a_w$cust==b[1],]
h<-hotel_w[hotel_w$cust==b[1],]
any(h$vst_dt[5] - 5>=a$de_dt & a$de_dt>=h$vst_dt)
h$vst_dt[5] - 5>=a$de_dt & a$de_dt>=h$vst_dt
resu<-cbind(hotel_w, result)
table(resu$result)
