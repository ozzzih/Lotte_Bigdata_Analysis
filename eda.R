#고객정보, 상품정보 조인
library(dplyr)
#d_02데이터
df2<-left_join(d_02, d_01, by="cust")#고객정보조인
df2<-left_join(df2, d_04, by='pd_c')#상품정보조인
#d_03데이터
df3<-left_join(d_03, d_01, by="cust")
df3<-left_join(df3, d_04, by='pd_c')
#d_06데이터
df6<-left_join(d_06, d_01, by="cust")

#EDA
#df2데이터 건수 정보
########전체#######
df<-df2 %>% group_by(df2$cop_c) %>% summarize(sum=sum(buy_ct))
library(ggplot2)
ggplot(df, aes(x=df$`df2$cop_c`,y=df$sum))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           fill="steelblue")
df<-t(df)
########성별########
df<-df2 %>% group_by(df2$cop_c, df2$ma_fem_dv) %>% summarize(sum=sum(buy_ct))

d_m<-data.frame(table(df2$cop_c[df2$ma_fem_dv=="남성"]))
d_f<-data.frame(table(df2$cop_c[df2$ma_fem_dv=="여성"]))
d_m[,3]<-"남자"
d_f[,3]<-"여자"
x<-rbind(d_m, d_f)
df<-transform(df, V3=factor(df$`df2$ma_fem_dv`, levels = c("여성", "남성")))
ggplot(df, aes(x=df$df2.cop_c,y=sum))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           aes(fill=V3), position = "dodge", colour="black")+
  scale_fill_brewer(palette="Pastel1")
d_f<-df[df$`df2$ma_fem_dv`=="여성",]
d_f<-t(d_f)
d_m<-d_m[-2,]
df<-t(df)
##########연령별#########
df<-df2 %>% group_by(df2$cop_c, df2$ages) %>% summarize(count=sum(buy_ct))
ggplot(df, aes(x=df$`df2$cop_c`,y=df$count))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           aes(fill=df$`df2$ages`), position = "dodge", colour="black")+
  scale_fill_brewer(palette="Pastel1")
d<-t(df[,c(2,3)])


#df2데이터 금액 정보
########전체#######
df<-aggregate(df2$buy_am,by=list(df2$cop_c, df2$ages, df2$ma_fem_dv), FUN=sum)
ggplot(df, aes(x=Group.1,y=x))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           fill="steelblue")
df1<-df %>% group_by(df$Group.1) %>% summarise(sum=sum(x))
df1<-t(df1)
####성별#####
df<-aggregate(df2$buy_am,by=list(df2$cop_c, df2$ma_fem_dv), FUN=sum)
df<-transform(df, Group.2=factor(Group.2, levels = c("여성", "남성")))
ggplot(df, aes(x=Group.1,y=x))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           aes(fill=Group.2), position = "dodge", colour="black")+
  scale_fill_brewer(palette="Pastel1")
df<-t(df[,-2])
#####연령별#####
df<-aggregate(df2$buy_am,by=list(df2$cop_c, df2$ages), FUN=sum)
ggplot(df, aes(x=Group.2,y=x))+ # 데이터 및 축 입력
  geom_bar(stat="identity", width=0.7, # 그래프 종류 및 서식 입력
           aes(fill=Group.1), position = "dodge", colour="black")+
  scale_fill_brewer(palette="Pastel1")

#####대분류별######

df<-df2[df2$cop_c=="A06",]
x<-df %>% group_by(df$clac_hlv_nm, df$ages) %>% summarize(num=sum(buy_ct))
x<-df %>% group_by(df$clac_hlv_nm, df$ma_fem_dv) %>% summarize(num=sum(buy_ct))

y<-df %>% group_by(df$clac_hlv_nm, df$ages) %>% summarize(num=sum(buy_am))
x<-df %>% group_by(df$clac_hlv_nm, df$ma_fem_dv) %>% summarize(num=sum(buy_am))
table(a)


#여성이 남성의 옷을 구매하는 경향
x<-df2 %>% filter(grepl('남성', df2$clac_hlv_nm))
y<-df2 %>% filter(grepl('남성', df2$clac_mcls_nm))
z<-df2 %>% filter(grepl('남성', df2$pd_nm))
x<-rbind(x,y,z)
a<-unique(x$cust)
b<-unique(x$cust)
y<-x[!duplicated(x), ]
y<-y[y$ma_fem_dv=="여성",]
write.csv(y, "여성이 남성제품 구매.csv")
z<-y %>% group_by(y$ages, y$clac_hlv_nm) %>% summarize(sum=sum(buy_ct))
write.csv(z, "여성구매.csv")
x<-df2[df2$ma_fem_dv=="여성",]
a<-as.data.frame(a)
b<-as.data.frame(b)
a<-left_join(a, d_01, by=c('a'='cust'))
b<-left_join(b, d_01, by=c('b'='cust'))
table(b$ages)
