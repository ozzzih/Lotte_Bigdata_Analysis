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