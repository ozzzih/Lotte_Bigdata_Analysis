library(data.table)
R_LIBS_SITE="C:\\Program Files\\R\\R-4.2.1\\library"
d_01<-fread("LPOINT_BIG_COMP_01_DEMO.csv")
d_02<-fread("LPOINT_BIG_COMP_02_PDDE.csv")
d_03<-fread("LPOINT_BIG_COMP_03_COP_U.csv")
d_04<-fread("LPOINT_BIG_COMP_04_PD_CLAC.csv")
d_05<-fread("LPOINT_BIG_COMP_05_BR.csv")
d_06<-fread("LPOINT_BIG_COMP_06_LPAY.csv")

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

