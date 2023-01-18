########################################################################
#재구매지수 예측모형
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
#write.csv(df, "df.csv") 

#############파생변수 완성################3

#변수선택
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