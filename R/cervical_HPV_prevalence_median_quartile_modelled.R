rm(list=ls(all=TRUE))  

library(SASxport)
library(dplyr)
library(survey)
library(haven)
library(forcats)
library(questionr)
library(weights)
library(data.table)

setwd("H:/Oropharyngeal/NHANES data/")
out.dir<-("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")

demo_d<-read.xport("DEMO_D.xpt")
hpvswr_d<-read.xport("HPVSWR_D.xpt")
sxq_d<-read.xport("SXQ_D.xpt")

demo_c<-read.xport("DEMO_C.xpt")
hpvswr_c<-read.xport("L37SWR_C.xpt")
sxq_c<-read.xport("SXQ_C.xpt")

hpvswa_c<-read.xport("L37SWA_C.xpt")


cervix<-merge(demo_d, hpvswr_d, by="SEQN", sort = TRUE, all.x = TRUE)
cervix<-merge(cervix, sxq_d, by="SEQN", sort = TRUE, all.x = TRUE)
cervix<-filter(cervix, RIAGENDR==2)
cervix$hpv16<-ifelse(cervix$LBDR16==2,0, 
                     ifelse(cervix$LBDR16==1,1, NA))
table(cervix$hpv16, cervix$LBDR16, useNA="always")

cervix$hpv18<-ifelse(cervix$LBDR18==2,0, 
                     ifelse(cervix$LBDR18==1,1, NA))

cervix$age<-cervix$RIDAGEYR

##############
T1_3=19
T2_3=38
T3_3=57

cervix$SPL3AGE<-ifelse(cervix$age<=T1_3,0,
                       ifelse(cervix$age<=T2_3, (cervix$age-T1_3)^3,
                              ifelse(cervix$age<=T3_3, (cervix$age-T1_3)^3 + -((T3_3-T1_3)/(T3_3-T2_3))*(cervix$age-T2_3)^3,
                                     ifelse(cervix$age>T3_3,(cervix$age-T1_3)^3 + -((T3_3-T1_3)/(T3_3-T2_3))*(cervix$age-T2_3)^3
                                            +((T2_3-T1_3)/(T3_3-T2_3))*(cervix$age-T3_3)^3,NA))))




# lifetime any partners, categorical
#Eversex
cervix$eversex<-ifelse(cervix$SXQ021==1,1,
                       ifelse(cervix$SXQ021==2, 0, NA))

cervix$SXQ101<-ifelse(cervix$SXQ101>1000,NA,cervix$SXQ101)
cervix$lifetimeany<-cervix$SXQ101
cervix$lifetimeany<-ifelse(cervix$eversex==0,0,cervix$lifetimeany)

cervix$agecat<-ifelse(cervix$age>=18 & cervix$age<=24,1,
                      ifelse(cervix$age>=25 & cervix$age<=29,2,
                             ifelse(cervix$age>=30 & cervix$age<=34,3,
                                    ifelse(cervix$age>=35 & cervix$age<=39,4,
                                           ifelse(cervix$age>=40 & cervix$age<=44,5,
                                                  ifelse(cervix$age>=45 & cervix$age<=49,6,
                                                         ifelse(cervix$age>=50 & cervix$age<=54,7,
                                                                ifelse(cervix$age>=55 & cervix$age<=59,8,NA))))))))



# age 18-59
for (x in 1:8) {
  assign(paste0("median.age",x),eval(parse(text =paste0("wtd.quantile(cervix$lifetimeany[cervix$agecat==",x,"], weights=cervix$WTMEC2YR[cervix$agecat==",x,"], probs=c(0.5), na.rm = TRUE)"))))
}  


cervix$lifetimeanymedian<-rep(NA,dim(cervix)[1])
for (x in 1:8) {
  temp<-eval(parse(text =paste0("ifelse(cervix$agecat==",x," & cervix$lifetimeany<median.age",x,"[1],0,
                     ifelse(cervix$agecat==",x," & cervix$lifetimeany>=median.age",x,"[1] & is.na(cervix$lifetimeany)==FALSE,1,NA))")))
  cervix$lifetimeanymedian<-ifelse(is.na(temp)==TRUE,cervix$lifetimeanymedian,temp)
}

cervix$lifetimeanymedian<-as.factor(cervix$lifetimeanymedian)

### quartiles
for (x in 1:8) {
  assign(paste0("quartile.age",x),eval(parse(text =paste0("wtd.quantile(cervix$lifetimeany[cervix$agecat==",x,"], weights=cervix$WTMEC2YR[cervix$agecat==",x,"], probs=c(0.25, 0.5, 0.75), na.rm = TRUE)"))))
}  


cervix$lifetimeanyquartile<-rep(NA,dim(cervix)[1])
for (x in 1:8) {
  temp<-eval(parse(text =paste0("ifelse(cervix$agecat==",x," & cervix$lifetimeany<quartile.age",x,"[1],0,
                                ifelse(cervix$agecat==",x," & cervix$lifetimeany<quartile.age",x,"[2],1,
                                ifelse(cervix$agecat==",x," & cervix$lifetimeany<quartile.age",x,"[3],2,
                                ifelse(cervix$agecat==",x," & cervix$lifetimeany>=quartile.age",x,"[3] & is.na(cervix$lifetimeany)==FALSE,3,NA))))")))
  cervix$lifetimeanyquartile<-ifelse(is.na(temp)==TRUE,cervix$lifetimeanyquartile,temp)
}

cervix$lifetimeanyquartile<-as.factor(cervix$lifetimeanyquartile)


# modelled cervical HPV16

cervix_belowmed<-filter(cervix, lifetimeanymedian==0)
cervix_abovemed<-filter(cervix, lifetimeanymedian==1)

cervix_q0<-filter(cervix, lifetimeanyquartile==0)
cervix_q1<-filter(cervix, lifetimeanyquartile==1)
cervix_q2<-filter(cervix, lifetimeanyquartile==2)
cervix_q3<-filter(cervix, lifetimeanyquartile==3)


nhanesDesign <- svydesign(id      = ~SDMVPSU,
                          strata  = ~SDMVSTRA,
                          weights = ~WTMEC2YR,
                          nest    = TRUE,
                          data    = cervix)
design_belowmed <- subset(nhanesDesign,lifetimeanymedian==0 & age>=18 & age<60)
design_abovemed <- subset(nhanesDesign,lifetimeanymedian==1 & age>=18 & age<60)

design_q0 <- subset(nhanesDesign,lifetimeanyquartile==0 & age>=18 & age<60)
design_q1 <- subset(nhanesDesign,lifetimeanyquartile==1 & age>=18 & age<60)
design_q2 <- subset(nhanesDesign,lifetimeanyquartile==2 & age>=18 & age<60)
design_q3 <- subset(nhanesDesign,lifetimeanyquartile==3 & age>=18 & age<60)


below<-svyglm(hpv16 ~ age+SPL3AGE, design=design_belowmed, family=binomial)
above<-svyglm(hpv16 ~ age+SPL3AGE, design=design_abovemed, family=binomial)

q0<-svyglm(hpv16 ~ age+SPL3AGE, design=design_q0, family=binomial)
q1<-svyglm(hpv16 ~ age+SPL3AGE, design=design_q1, family=binomial)
q2<-svyglm(hpv16 ~ age+SPL3AGE, design=design_q2, family=binomial)
q3<-svyglm(hpv16 ~ age+SPL3AGE, design=design_q3, family=binomial)

cervix_belowmed$predbelow<-predict(below, cervix_belowmed, type="response" )
cervix_abovemed$predabove<-predict(above, cervix_abovemed, type="response" )


cervix_q0$predq0<-predict(q0, cervix_q0, type="response" )
cervix_q1$predq1<-predict(q1, cervix_q1, type="response" )
cervix_q2$predq2<-predict(q2, cervix_q2, type="response" )
cervix_q3$predq3<-predict(q3, cervix_q3, type="response" )


cervix_belowmed_small<-select(cervix_belowmed, age,predbelow )
cervix_belowmed_small<-unique(cervix_belowmed_small)
sort(cervix_belowmed_small$age)

cervix_abovemed_small<-select(cervix_abovemed, age,predabove )
cervix_abovemed_small<-unique(cervix_abovemed_small)
sort(cervix_abovemed_small$age)
temp<-merge(cervix_abovemed_small,cervix_belowmed_small, by="age")
fwrite(temp,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/pred_cervical_hpv.csv")


cervix_q0_small<-select(cervix_q0, age,predq0 )
cervix_q0_small<-unique(cervix_q0_small)
sort(cervix_q0_small$age)

cervix_q1_small<-select(cervix_q1, age,predq1 )
cervix_q1_small<-unique(cervix_q1_small)
sort(cervix_q1_small$age)

cervix_q2_small<-select(cervix_q2, age,predq2 )
cervix_q2_small<-unique(cervix_q2_small)
sort(cervix_q2_small$age)

cervix_q3_small<-select(cervix_q3, age,predq3 )
cervix_q3_small<-unique(cervix_q3_small)
sort(cervix_q3_small$age)

temp<-merge(cervix_q0_small,cervix_q1_small, by="age")
temp<-merge(temp,cervix_q2_small, by="age")
temp<-merge(temp,cervix_q3_small, by="age")
fwrite(temp,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/pred_cervical_hpv_quartiles.csv")


### creating input file

## provide estimates for ages 15-17 and 60+
female.pop<-fread("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/female.pop.size2009_2016.csv")

###m0:
age<-c(15:79)
prev.18<-weighted.mean(cervix$hpv16[cervix$RIDAGEYR==18],cervix$WTMEC2YR[cervix$RIDAGEYR==18], na.rm=T)
prev.19<-weighted.mean(cervix$hpv16[cervix$RIDAGEYR==19],cervix$WTMEC2YR[cervix$RIDAGEYR==19], na.rm=T)
prev18.19<-((prev.18*female.pop[4])+(prev.19*female.pop[5]))/sum(female.pop[4:5])
prev15<-prev18.19*(9.4/26.7)
prev16.17<-prev18.19*(18.6/26.7)
prev55.59<-((temp$predbelow[36]*female.pop[41])+(temp$predbelow[37]*female.pop[42])+(temp$predbelow[38]*female.pop[43])+(temp$predbelow[39]*female.pop[44])+(temp$predbelow[40]*female.pop[45]))/(sum(female.pop[41:45]))
prev20.24<-((temp$predbelow[1]*female.pop[6])+(temp$predbelow[2]*female.pop[7])+(temp$predbelow[3]*female.pop[8])+(temp$predbelow[4]*female.pop[9])+(temp$predbelow[5]*female.pop[10]))/(sum(female.pop[6:10]))
prev20.24a<-((temp$predabove[1]*female.pop[6])+(temp$predabove[2]*female.pop[7])+(temp$predabove[3]*female.pop[8])+(temp$predabove[4]*female.pop[9])+(temp$predabove[5]*female.pop[10]))/(sum(female.pop[6:10]))



for (h in 20:59) {
  assign((text=paste0("prev.below.",h)),eval(parse(text =paste0("weighted.mean(cervix$hpv16[cervix$lifetimeanymedian==0 & cervix$RIDAGEYR==",h,"],cervix$WTMEC2YR[cervix$lifetimeanymedian==0 &cervix$RIDAGEYR==",h,"], na.rm=T)"))))
  assign((text=paste0("prev.above.",h)),eval(parse(text =paste0("weighted.mean(cervix$hpv16[cervix$lifetimeanymedian==1 & cervix$RIDAGEYR==",h,"],cervix$WTMEC2YR[cervix$lifetimeanymedian==1 &cervix$RIDAGEYR==",h,"], na.rm=T)"))))
}

prev20.24.below<-((temp$predbelow[1]*female.pop[6])+(temp$predbelow[2]*female.pop[7])+(temp$predbelow[3]*female.pop[8])+(temp$predbelow[4]*female.pop[9])+(temp$predbelow[5]*female.pop[10]))/(sum(female.pop[6:10]))
prev20.24.above<-((temp$predabove[1]*female.pop[6])+(temp$predabove[2]*female.pop[7])+(temp$predabove[3]*female.pop[8])+(temp$predabove[4]*female.pop[9])+(temp$predabove[5]*female.pop[10]))/(sum(female.pop[6:10]))


#no lifetime quartile data for women aged 18-19, so just use full pop divided by 4
prev.18<-weighted.mean(cervix$hpv16[cervix$RIDAGEYR==18],cervix$WTMEC2YR[cervix$RIDAGEYR==18], na.rm=T)
prev.19<-weighted.mean(cervix$hpv16[cervix$RIDAGEYR==19],cervix$WTMEC2YR[cervix$RIDAGEYR==19], na.rm=T)
prev18.19<-weighted.mean(cervix$hpv16[cervix$RIDAGEYR>=18 & cervix$RIDAGEYR<=19],cervix$WTMEC2YR[cervix$RIDAGEYR>=18 & cervix$RIDAGEYR<=19], na.rm=T)
prev15<-prev18.19*(9.4/26.7)
prev16.17<-prev18.19*(18.6/26.7)
prev55.59<-weighted.mean(cervix$hpv16[cervix$lifetimeanyquart==0 & cervix$RIDAGEYR>=55 & cervix$RIDAGEYR<=59],cervix$WTMEC2YR[cervix$lifetimeanyquart==0 & cervix$RIDAGEYR>=55 & cervix$RIDAGEYR<=59], na.rm=T)


##### creating prev column

prev<-c(prev15,prev16.17,prev16.17,prev.18,prev.19,prev.20,prev.21,prev.22,prev.23,prev.24,prev.25,prev.26,prev.27,prev.28,
        prev.29,prev.30,prev.31,prev.32,prev.33,prev.34,prev.35,prev.36,prev.37,prev.38,prev.39,prev.40,prev.41,prev.42,prev.43,
        prev.44,prev.45,prev.46,prev.47,prev.48,prev.49,prev.50,prev.51,prev.52,prev.53,prev.54,prev.55,prev.56,prev.57,prev.58,prev.59)
prev60.79<-rep(prev55.59,20)
prev<-c(prev,prev60.79)

cervix.prev<-as.data.frame(cbind(age,popq0,prev))
cervix.prev <- cervix.prev %>% mutate(N.HPV16 = popq0 * prev)


