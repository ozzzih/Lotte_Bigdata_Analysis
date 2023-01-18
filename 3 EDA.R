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


