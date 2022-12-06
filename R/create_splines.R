rm(list=ls(all=TRUE)) 
age<-c(15:84)


T1_5=18.6
T2_5=27.7
T3_5=41.6
T4_5=53.9 
T5_5=64.7
#Location of knots 5 25 50 75 95*/
SPL5AGE1<-ifelse(age<= T1_5,0,
                            ifelse(T1_5<age & age<=T4_5,(age-T1_5)^3,
                                   ifelse( T4_5<age & age<=T5_5,((age-T1_5)^3) + -((T5_5-T1_5)/(T5_5-T4_5))*(age-T4_5)^3,
                                           ifelse(T5_5<age, (age-T1_5)^3 + -((T5_5-T1_5)/(T5_5-T4_5))*(age-T4_5)^3 +
                                                    ((T4_5-T1_5)/(T5_5-T4_5))*(age-T5_5)^3,NA))))

data<-as.data.frame(cbind(age,SPL5AGE1))

data$SPL5AGE2<-ifelse(data$age<= T2_5,0,
                            ifelse(data$age>T2_5 & data$age<=T4_5,(data$age-T2_5)^3,
                                   ifelse(T4_5<data$age & data$age<=T5_5,(data$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(data$age-T4_5)^3,
                                          ifelse(T5_5<data$age,(data$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(data$age-T4_5)^3
                                                 + ((T4_5-T2_5)/(T5_5-T4_5))*(data$age-T5_5)^3, NA))))  


data$SPL5AGE3<-ifelse(data$age<= T3_5,0,
                            ifelse(data$age>T3_5 & data$age<=T4_5,(data$age-T3_5)**3, 
                                   ifelse(T4_5<data$age & data$age<=T5_5,(data$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(data$age-T4_5)^3,
                                          ifelse(T5_5<data$age,(data$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(data$age-T4_5)^3
                                                 + ((T4_5-T3_5)/(T5_5-T4_5))*(data$age-T5_5)^3,NA))))

fwrite(data, "L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/splines.csv")