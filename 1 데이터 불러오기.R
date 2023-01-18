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