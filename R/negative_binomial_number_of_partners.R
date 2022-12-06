rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R

library(SASxport)
library(dplyr)
library(survey)
library(haven)
library(forcats)
library(questionr)
library(weights)
library(pscl)
library(MASS)
library(data.table)
library(sjstats)
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")
out.dir<-("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")



#####

NHANES0916<-read_sas("C:/Users/landyrm/Desktop/microsim/Data/Final/jan2020c4.sas7bdat")
NHANES0916<-filter(NHANES0916, RIAGENDR==1)
#09-16: current smoking: SMQ040==1 or 2. not current smoker==3. weight for this variable is int weight
# SMQ020 - ever smoked 100 cigarettes : 2 = no
NHANES0916$currentsmoking=ifelse(NHANES0916$smoking==3,1,
                                 ifelse(NHANES0916$smoking>=1 & NHANES0916$smoking<=2,0,NA))

NHANES0916$age<-NHANES0916$RIDAGEYR

# race: 
# * ridreth1=1: Mexican-American
# * ridreth1=2: other Hispanic-American
# * ridreth1=3: Non-Hispanic White
# * ridreth1=4: Hon-Hispanic Black
# * ridreth1=5: other including multiracial;

NHANES0916$racecat<-as.factor(ifelse(NHANES0916$RIDRETH1>=1 & NHANES0916$RIDRETH1<=2, 1,
                          ifelse(NHANES0916$RIDRETH1==3,2,
                                 ifelse(NHANES0916$RIDRETH1==4, 3, 4))))

# racecat=1: Hispanic-American
# racecat=2: Non-Hispanic White
# racecat=3: Hon-Hispanic Black
# racecat=4: other;


#for ages <18 use MEC weights, for ages >69 use INT weights
NHANES0916$WTINT8YR<-NHANES0916$WTINT2YR/4
NHANES0916$WTMEC8YR<-NHANES0916$WTMEC2YR/4

# splines

T1_5=18.6
T2_5=27.7
T3_5=41.6
T4_5=53.9 
T5_5=64.7
#Location of knots 5 25 50 75 95*/
NHANES0916$SPL5AGE1<-ifelse(NHANES0916$age<= T1_5,0,
                            ifelse(T1_5<NHANES0916$age & NHANES0916$age<=T4_5,(NHANES0916$age-T1_5)^3,
                                   ifelse( T4_5<NHANES0916$age & NHANES0916$age<=T5_5,((NHANES0916$age-T1_5)^3) + -((T5_5-T1_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3,
                                           ifelse(T5_5<NHANES0916$age, (NHANES0916$age-T1_5)^3 + -((T5_5-T1_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3 +
                                                   ((T4_5-T1_5)/(T5_5-T4_5))*(NHANES0916$age-T5_5)^3,NA))))


NHANES0916$SPL5AGE2<-ifelse(NHANES0916$age<= T2_5,0,
                            ifelse(NHANES0916$age>T2_5 & NHANES0916$age<=T4_5,(NHANES0916$age-T2_5)^3,
                                   ifelse(T4_5<NHANES0916$age & NHANES0916$age<=T5_5,(NHANES0916$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3,
                                           ifelse(T5_5<NHANES0916$age,(NHANES0916$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3
                                                  + ((T4_5-T2_5)/(T5_5-T4_5))*(NHANES0916$age-T5_5)^3, NA))))  
                            

NHANES0916$SPL5AGE3<-ifelse(NHANES0916$age<= T3_5,0,
                          ifelse(NHANES0916$age>T3_5 & NHANES0916$age<=T4_5,(NHANES0916$age-T3_5)**3, 
                                 ifelse(T4_5<NHANES0916$age & NHANES0916$age<=T5_5,(NHANES0916$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3,
                                        ifelse(T5_5<NHANES0916$age,(NHANES0916$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(NHANES0916$age-T4_5)^3
                                               + ((T4_5-T3_5)/(T5_5-T4_5))*(NHANES0916$age-T5_5)^3,NA))))
                          

NHANES0916$agecat<-ifelse(NHANES0916$age<18,1,
                          ifelse(NHANES0916$age>=70 & NHANES0916$age<75,12,
                                 ifelse(NHANES0916$age>=75 & NHANES0916$age<80,13,
                                        ifelse(NHANES0916$age>=80 & NHANES0916$age<85,14,NHANES0916$agecat))))


NHANES0916$recentoralcorrected<-ifelse(NHANES0916$eversex==0 & is.na(NHANES0916$eversex)==F,0,
                                       ifelse(NHANES0916$lifetimeoral==0 & is.na(NHANES0916$lifetimeoral)==F,0,
                                              ifelse(NHANES0916$recentany==0 & is.na(NHANES0916$recentany)==F,0,
                                                     ifelse(is.na(NHANES0916$SXQ627)==F & is.na(NHANES0916$SXQ639)==T,0,NHANES0916$SXQ639))))


NHANES0916$recentnew<-ifelse(NHANES0916$eversex==0,0,
                                    ifelse(NHANES0916$recentany==0,0,
                                          ifelse(NHANES0916$SXQ648==2,0,
                                              ifelse(NHANES0916$SXQ648==1,1,NA))))

table(NHANES0916$SXQ648[NHANES0916$age>=18 & NHANES0916$age<70],NHANES0916$recentnew[NHANES0916$age>=18 & NHANES0916$age<70], useNA="always")                                           
table(NHANES0916$recentany[NHANES0916$age>=18 & NHANES0916$age<70],NHANES0916$recentnew[NHANES0916$age>=18 & NHANES0916$age<70], useNA="always")                                           
table(NHANES0916$recentnew[NHANES0916$age>=18 & NHANES0916$age<70], useNA="always")                                           

table(NHANES0916$age[NHANES0916$age>50 & NHANES0916$age<70 & NHANES0916$SDDSRVYR==9],NHANES0916$lifetimeoralcat[NHANES0916$age>50 & NHANES0916$age<70& NHANES0916$SDDSRVYR==9], useNA="always")


# create age-specific lifetimeoral quartiles
# age 18-59
for (x in 2:9) {
    assign(paste0("quart.oral.age",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeoral[NHANES0916$agecat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$agecat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
}

for (x in 2:11) {
    assign(paste0("quart.any.age",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeany[NHANES0916$agecat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$agecat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
}


NHANES0916$quartile.lifetimeoral<-rep(NA,dim(NHANES0916)[1])
for (x in 2:9) {
temp<-eval(parse(text =paste0("ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeoral<=quart.oral.age",x,"[1],1,
       ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeoral<=quart.oral.age",x,"[2],2,
              ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeoral<=quart.oral.age",x,"[3],3,
                     ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeoral>quart.oral.age",x,"[3] & is.na(NHANES0916$lifetimeoral)==FALSE,4,NA))))")))
NHANES0916$quartile.lifetimeoral<-ifelse(is.na(temp)==TRUE,NHANES0916$quartile.lifetimeoral,temp)
}

NHANES0916$quartile.lifetimeoral<-as.factor(NHANES0916$quartile.lifetimeoral)

NHANES0916$quartile.lifetimeany<-rep(NA,dim(NHANES0916)[1])
for (x in 2:11) {
temp<-eval(parse(text =paste0("ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany<=quart.any.age",x,"[1],1,
       ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany<=quart.any.age",x,"[2],2,
              ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany<=quart.any.age",x,"[3],3,
                     ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany>quart.any.age",x,"[3] & is.na(NHANES0916$lifetimeany)==FALSE,4,NA))))")))
NHANES0916$quartile.lifetimeany<-ifelse(is.na(temp)==TRUE,NHANES0916$quartile.lifetimeany,temp)
}

NHANES0916$quartile.lifetimeany<-as.factor(NHANES0916$quartile.lifetimeany)

#use lifetime oral quartile if recorded, otherwise lifetimeany
NHANES0916$quartile.lifetime<-ifelse(is.na(NHANES0916$quartile.lifetimeoral)==FALSE,NHANES0916$quartile.lifetimeoral,NHANES0916$quartile.lifetimeany)

table(NHANES0916$quartile.lifetime, NHANES0916$RIDAGEYR)

# negative binomial model
NHANES0916 <- within(NHANES0916, racecat <- relevel(racecat, ref = 4))
NHANES0916$quartile.lifetime<-as.factor(NHANES0916$quartile.lifetime)
NHANES0916 <- within(NHANES0916, quartile.lifetime <- relevel(quartile.lifetime, ref = 1))

NHANES0916.1859<-filter(NHANES0916, age>=18 & age<=59 & is.na(SDMVPSU)==F & is.na(recentoralcorrected)==F & is.na(recentnew)==F & is.na(currentsmoking)==F)
summary(NHANES0916.1859$behavweight8yr)
NHANES0916.1859$newweight<-NHANES0916.1859$behavweight8yr/12667

des <- svydesign(
  id = ~ SDMVPSU,
  strat = ~ SDMVSTRA,
  weights = ~behavweight8yr ,
  nest = TRUE,
  data = NHANES0916.1859
)

wtd.table(NHANES0916.1859$eversex[NHANES0916.1859$age==18], weights=NHANES0916.1859$behavweight8yr[NHANES0916.1859$age==18])

wtd.table(NHANES0916.1859$debut[NHANES0916.1859$age==18], weights=NHANES0916.1859$behavweight8yr[NHANES0916.1859$age==18])

table(is.na(NHANES0916.1859$currentsmoking))

summary(m1 <-glm.nb(recentoralcorrected ~ age + SPL5AGE1+ SPL5AGE2+SPL5AGE3+ racecat+ as.factor(quartile.lifetime)+
                         currentsmoking+ recentnew, weights=behavweight8yr, data=NHANES0916.1859))

allcomb1<-fread("allcombinations.csv")
allcomb2<-fread("allcombinations.csv")
allcomb1$recentnew<-0
allcomb2$recentnew<-1
allcomb<-as.data.frame(rbind(allcomb1,allcomb2))
allcomb$currentsmoking<-allcomb$smoking
allcomb$racecat<-as.factor(allcomb$racecat)
allcomb$quartile.lifetime<-allcomb$lifetimequart
allcomb$SPL5AGE1<-ifelse(allcomb$age<= T1_5,0,
                            ifelse(T1_5<allcomb$age & allcomb$age<=T4_5,(allcomb$age-T1_5)^3,
                                   ifelse( T4_5<allcomb$age & allcomb$age<=T5_5,((allcomb$age-T1_5)^3) + -((T5_5-T1_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3,
                                           ifelse(T5_5<allcomb$age, (allcomb$age-T1_5)^3 + -((T5_5-T1_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3 +
                                                    ((T4_5-T1_5)/(T5_5-T4_5))*(allcomb$age-T5_5)^3,NA))))


allcomb$SPL5AGE2<-ifelse(allcomb$age<= T2_5,0,
                            ifelse(allcomb$age>T2_5 & allcomb$age<=T4_5,(allcomb$age-T2_5)^3,
                                   ifelse(T4_5<allcomb$age & allcomb$age<=T5_5,(allcomb$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3,
                                          ifelse(T5_5<allcomb$age,(allcomb$age-T2_5)^3 + -((T5_5-T2_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3
                                                 + ((T4_5-T2_5)/(T5_5-T4_5))*(allcomb$age-T5_5)^3, NA))))  


allcomb$SPL5AGE3<-ifelse(allcomb$age<= T3_5,0,
                            ifelse(allcomb$age>T3_5 & allcomb$age<=T4_5,(allcomb$age-T3_5)**3, 
                                   ifelse(T4_5<allcomb$age & allcomb$age<=T5_5,(allcomb$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3,
                                          ifelse(T5_5<allcomb$age,(allcomb$age-T3_5)^3 + -((T5_5-T3_5)/(T5_5-T4_5))*(allcomb$age-T4_5)^3
                                                 + ((T4_5-T3_5)/(T5_5-T4_5))*(allcomb$age-T5_5)^3,NA))))
allcomb$id<-as.numeric(rownames(allcomb))

pred.int <- as.data.frame(predict.glm(m1, newdata=allcomb, se.fit=T,type = "response" ))
pred.int$id<-as.numeric(rownames(pred.int))

allcomb.pred<-merge(allcomb,pred.int, by="id")
fwrite(allcomb.pred, "L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/negbin/predictions.csv")


keepvar <- c("fit")
theta<-4.060326051
size=1/theta
# 4 columns: recentnew=1 SIZE, recentnew=1 mean, recentnew=0 SIZE, recentnew=0 mean, 
r1s0q0n0<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==1 & recentnew==0)
r1s0q0n1<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==1 & recentnew==1)
r1s0q0<-cbind(rep(size,70),r1s0q0n1[keepvar],rep(size,70),r1s0q0n0[keepvar])
r2s0q0n0<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==1 & recentnew==0)
r2s0q0n1<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==1 & recentnew==1)
r2s0q0<-cbind(rep(size,70),r2s0q0n1[keepvar],rep(size,70),r2s0q0n0[keepvar])
r3s0q0n0<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==1 & recentnew==0)
r3s0q0n1<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==1 & recentnew==1)
r3s0q0<-cbind(rep(size,70),r3s0q0n1[keepvar],rep(size,70),r3s0q0n0[keepvar])
r4s0q0n0<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==1 & recentnew==0)
r4s0q0n1<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==1 & recentnew==1)
r4s0q0<-cbind(rep(size,70),r4s0q0n1[keepvar],rep(size,70),r4s0q0n0[keepvar])

r1s1q0n0<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==1 & recentnew==0)
r1s1q0n1<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==1 & recentnew==1)
r1s1q0<-cbind(rep(size,70),r1s1q0n1[keepvar],rep(size,70),r1s1q0n0[keepvar])
r2s1q0n0<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==1 & recentnew==0)
r2s1q0n1<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==1 & recentnew==1)
r2s1q0<-cbind(rep(size,70),r2s1q0n1[keepvar],rep(size,70),r2s1q0n0[keepvar])
r3s1q0n0<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==1 & recentnew==0)
r3s1q0n1<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==1 & recentnew==1)
r3s1q0<-cbind(rep(size,70),r3s1q0n1[keepvar],rep(size,70),r3s1q0n0[keepvar])
r4s1q0n0<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==1 & recentnew==0)
r4s1q0n1<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==1 & recentnew==1)
r4s1q0<-cbind(rep(size,70),r4s1q0n1[keepvar],rep(size,70),r4s1q0n0[keepvar])

r1s0q1n0<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==2 & recentnew==0)
r1s0q1n1<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==2 & recentnew==1)
r1s0q1<-cbind(rep(size,70),r1s0q1n1[keepvar],rep(size,70),r1s0q1n0[keepvar])
r2s0q1n0<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==2 & recentnew==0)
r2s0q1n1<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==2 & recentnew==1)
r2s0q1<-cbind(rep(size,70),r2s0q1n1[keepvar],rep(size,70),r2s0q1n0[keepvar])
r3s0q1n0<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==2 & recentnew==0)
r3s0q1n1<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==2 & recentnew==1)
r3s0q1<-cbind(rep(size,70),r3s0q1n1[keepvar],rep(size,70),r3s0q1n0[keepvar])
r4s0q1n0<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==2 & recentnew==0)
r4s0q1n1<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==2 & recentnew==1)
r4s0q1<-cbind(rep(size,70),r4s0q1n1[keepvar],rep(size,70),r4s0q1n0[keepvar])

r1s1q1n0<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==2 & recentnew==0)
r1s1q1n1<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==2 & recentnew==1)
r1s1q1<-cbind(rep(size,70),r1s1q1n1[keepvar],rep(size,70),r1s1q1n0[keepvar])
r2s1q1n0<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==2 & recentnew==0)
r2s1q1n1<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==2 & recentnew==1)
r2s1q1<-cbind(rep(size,70),r2s1q1n1[keepvar],rep(size,70),r2s1q1n0[keepvar])
r3s1q1n0<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==2 & recentnew==0)
r3s1q1n1<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==2 & recentnew==1)
r3s1q1<-cbind(rep(size,70),r3s1q1n1[keepvar],rep(size,70),r3s1q1n0[keepvar])
r4s1q1n0<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==2 & recentnew==0)
r4s1q1n1<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==2 & recentnew==1)
r4s1q1<-cbind(rep(size,70),r4s1q1n1[keepvar],rep(size,70),r4s1q1n0[keepvar])

r1s0q2n0<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==3 & recentnew==0)
r1s0q2n1<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==3 & recentnew==1)
r1s0q2<-cbind(rep(size,70),r1s0q2n1[keepvar],rep(size,70),r1s0q2n0[keepvar])
r2s0q2n0<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==3 & recentnew==0)
r2s0q2n1<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==3 & recentnew==1)
r2s0q2<-cbind(rep(size,70),r2s0q2n1[keepvar],rep(size,70),r2s0q2n0[keepvar])
r3s0q2n0<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==3 & recentnew==0)
r3s0q2n1<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==3 & recentnew==1)
r3s0q2<-cbind(rep(size,70),r3s0q2n1[keepvar],rep(size,70),r3s0q2n0[keepvar])
r4s0q2n0<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==3 & recentnew==0)
r4s0q2n1<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==3 & recentnew==1)
r4s0q2<-cbind(rep(size,70),r4s0q2n1[keepvar],rep(size,70),r4s0q2n0[keepvar])

r1s1q2n0<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==3 & recentnew==0)
r1s1q2n1<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==3 & recentnew==1)
r1s1q2<-cbind(rep(size,70),r1s1q2n1[keepvar],rep(size,70),r1s1q2n0[keepvar])
r2s1q2n0<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==3 & recentnew==0)
r2s1q2n1<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==3 & recentnew==1)
r2s1q2<-cbind(rep(size,70),r2s1q2n1[keepvar],rep(size,70),r2s1q2n0[keepvar])
r3s1q2n0<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==3 & recentnew==0)
r3s1q2n1<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==3 & recentnew==1)
r3s1q2<-cbind(rep(size,70),r3s1q2n1[keepvar],rep(size,70),r3s1q2n0[keepvar])
r4s1q2n0<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==3 & recentnew==0)
r4s1q2n1<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==3 & recentnew==1)
r4s1q2<-cbind(rep(size,70),r4s1q2n1[keepvar],rep(size,70),r4s1q2n0[keepvar])

r1s0q3n0<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==4 & recentnew==0)
r1s0q3n1<-filter(allcomb.pred, racecat==1 & smoking==0 & quartile.lifetime==4 & recentnew==1)
r1s0q3<-cbind(rep(size,70),r1s0q3n1[keepvar],rep(size,70),r1s0q3n0[keepvar])
r2s0q3n0<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==4 & recentnew==0)
r2s0q3n1<-filter(allcomb.pred, racecat==2 & smoking==0 & quartile.lifetime==4 & recentnew==1)
r2s0q3<-cbind(rep(size,70),r2s0q3n1[keepvar],rep(size,70),r2s0q3n0[keepvar])
r3s0q3n0<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==4 & recentnew==0)
r3s0q3n1<-filter(allcomb.pred, racecat==3 & smoking==0 & quartile.lifetime==4 & recentnew==1)
r3s0q3<-cbind(rep(size,70),r3s0q3n1[keepvar],rep(size,70),r3s0q3n0[keepvar])
r4s0q3n0<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==4 & recentnew==0)
r4s0q3n1<-filter(allcomb.pred, racecat==4 & smoking==0 & quartile.lifetime==4 & recentnew==1)
r4s0q3<-cbind(rep(size,70),r4s0q3n1[keepvar],rep(size,70),r4s0q3n0[keepvar])

r1s1q3n0<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==4 & recentnew==0)
r1s1q3n1<-filter(allcomb.pred, racecat==1 & smoking==1 & quartile.lifetime==4 & recentnew==1)
r1s1q3<-cbind(rep(size,70),r1s1q3n1[keepvar],rep(size,70),r1s1q3n0[keepvar])
r2s1q3n0<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==4 & recentnew==0)
r2s1q3n1<-filter(allcomb.pred, racecat==2 & smoking==1 & quartile.lifetime==4 & recentnew==1)
r2s1q3<-cbind(rep(size,70),r2s1q3n1[keepvar],rep(size,70),r2s1q3n0[keepvar])
r3s1q3n0<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==4 & recentnew==0)
r3s1q3n1<-filter(allcomb.pred, racecat==3 & smoking==1 & quartile.lifetime==4 & recentnew==1)
r3s1q3<-cbind(rep(size,70),r3s1q3n1[keepvar],rep(size,70),r3s1q3n0[keepvar])
r4s1q3n0<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==4 & recentnew==0)
r4s1q3n1<-filter(allcomb.pred, racecat==4 & smoking==1 & quartile.lifetime==4 & recentnew==1)
r4s1q3<-cbind(rep(size,70),r4s1q3n1[keepvar],rep(size,70),r4s1q3n0[keepvar])

out.dir2<-("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")
library(data.table)
#q0
fwrite(r1s0q0, file=paste0(out.dir2,"recentoralr1s0q0.csv"))
fwrite(r2s0q0, file=paste0(out.dir2,"recentoralr2s0q0.csv"))
fwrite(r3s0q0, file=paste0(out.dir2,"recentoralr3s0q0.csv"))
fwrite(r4s0q0, file=paste0(out.dir2,"recentoralr4s0q0.csv"))
fwrite(r1s1q0, file=paste0(out.dir2,"recentoralr1s1q0.csv"))
fwrite(r2s1q0, file=paste0(out.dir2,"recentoralr2s1q0.csv"))
fwrite(r3s1q0, file=paste0(out.dir2,"recentoralr3s1q0.csv"))
fwrite(r4s1q0, file=paste0(out.dir2,"recentoralr4s1q0.csv"))
#q1
fwrite(r1s0q1, file=paste0(out.dir2,"recentoralr1s0q1.csv"))
fwrite(r2s0q1, file=paste0(out.dir2,"recentoralr2s0q1.csv"))
fwrite(r3s0q1, file=paste0(out.dir2,"recentoralr3s0q1.csv"))
fwrite(r4s0q1, file=paste0(out.dir2,"recentoralr4s0q1.csv"))
fwrite(r1s1q1, file=paste0(out.dir2,"recentoralr1s1q1.csv"))
fwrite(r2s1q1, file=paste0(out.dir2,"recentoralr2s1q1.csv"))
fwrite(r3s1q1, file=paste0(out.dir2,"recentoralr3s1q1.csv"))
fwrite(r4s1q1, file=paste0(out.dir2,"recentoralr4s1q1.csv"))
#q2
fwrite(r1s0q2, file=paste0(out.dir2,"recentoralr1s0q2.csv"))
fwrite(r2s0q2, file=paste0(out.dir2,"recentoralr2s0q2.csv"))
fwrite(r3s0q2, file=paste0(out.dir2,"recentoralr3s0q2.csv"))
fwrite(r4s0q2, file=paste0(out.dir2,"recentoralr4s0q2.csv"))
fwrite(r1s1q2, file=paste0(out.dir2,"recentoralr1s1q2.csv"))
fwrite(r2s1q2, file=paste0(out.dir2,"recentoralr2s1q2.csv"))
fwrite(r3s1q2, file=paste0(out.dir2,"recentoralr3s1q2.csv"))
fwrite(r4s1q2, file=paste0(out.dir2,"recentoralr4s1q2.csv"))
#q3
fwrite(r1s0q3, file=paste0(out.dir2,"recentoralr1s0q3.csv"))
fwrite(r2s0q3, file=paste0(out.dir2,"recentoralr2s0q3.csv"))
fwrite(r3s0q3, file=paste0(out.dir2,"recentoralr3s0q3.csv"))
fwrite(r4s0q3, file=paste0(out.dir2,"recentoralr4s0q3.csv"))
fwrite(r1s1q3, file=paste0(out.dir2,"recentoralr1s1q3.csv"))
fwrite(r2s1q3, file=paste0(out.dir2,"recentoralr2s1q3.csv"))
fwrite(r3s1q3, file=paste0(out.dir2,"recentoralr3s1q3.csv"))
fwrite(r4s1q3, file=paste0(out.dir2,"recentoralr4s1q3.csv"))


##### simulating to see what get

mutemp<-filter(allcomb.pred,racecat==2 & smoking==1 & lifetimequart==3 & recentnew==1 & age==30)
theta<-4.060326051
temp<-rnbinom(100000,mu=as.numeric(mutemp[12]), size=1/theta)
summary(temp)
table(temp)
hist(temp,probability=T)
se.temp<- sqrt(var(temp))/sqrt(100000)
temp2<-rnbinom(100000,mu=0.2478460, size=3.63)
summary(temp2)
temp3<-rnbinom(100000,mu=exp(0.2478460), size=3.63)
summary(temp3)


# want to use dispersion as size
log_temp<-log(temp)
var(log_temp)


######### reading recentnew and creating 32 files
recentnew<-as.data.table(read_sas("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/newrecent/newrecentpredictions.sas7bdat"))

#q0
r1s0q0<-filter(recentnew, racecat==1 & currentsmoking==0 & lifetimeoral_quart==0  & allcomb==1)
r1s0q0<-dplyr::select(r1s0q0, c("age", "prediction"))
r1s0q0 <- r1s0q0[order(age),]
r1s0q0<-as.data.frame(r1s0q0[,2])
r2s0q0<-filter(recentnew, racecat==2 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r2s0q0<-dplyr::select(r2s0q0, c("age", "prediction"))
r2s0q0 <- r2s0q0[order(age),]
r2s0q0<-as.data.frame(r2s0q0[,2])
r3s0q0<-filter(recentnew, racecat==3 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r3s0q0<-dplyr::select(r3s0q0, c("age", "prediction"))
r3s0q0 <- r3s0q0[order(age),]
r3s0q0<-as.data.frame(r3s0q0[,2])
r4s0q0<-filter(recentnew, racecat==4 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r4s0q0<-dplyr::select(r4s0q0, c("age", "prediction"))
r4s0q0 <- r4s0q0[order(age),]
r4s0q0<-as.data.frame(r4s0q0[,2])
r1s1q0<-filter(recentnew, racecat==1 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r1s1q0<-dplyr::select(r1s1q0, c("age", "prediction"))
r1s1q0 <- r1s1q0[order(age),]
r1s1q0<-as.data.frame(r1s1q0[,2])
r2s1q0<-filter(recentnew, racecat==2 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r2s1q0<-dplyr::select(r2s1q0, c("age", "prediction"))
r2s1q0 <- r2s1q0[order(age),]
r2s1q0<-as.data.frame(r2s1q0[,2])
r3s1q0<-filter(recentnew, racecat==3 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r3s1q0<-dplyr::select(r3s1q0, c("age", "prediction"))
r3s1q0 <- r3s1q0[order(age),]
r3s1q0<-as.data.frame(r3s1q0[,2])
r4s1q0<-filter(recentnew, racecat==4 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r4s1q0<-dplyr::select(r4s1q0, c("age", "prediction"))
r4s1q0 <- r4s1q0[order(age),]
r4s1q0<-as.data.frame(r4s1q0[,2])

#q1
r1s0q1<-filter(recentnew, racecat==1 & currentsmoking==0 & lifetimeoral_quart==0  & allcomb==1)
r1s0q1<-dplyr::select(r1s0q1, c("age", "prediction"))
r1s0q1 <- r1s0q1[order(age),]
r1s0q1<-as.data.frame(r1s0q1[,2])
r2s0q1<-filter(recentnew, racecat==2 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r2s0q1<-dplyr::select(r2s0q1, c("age", "prediction"))
r2s0q1 <- r2s0q1[order(age),]
r2s0q1<-as.data.frame(r2s0q1[,2])
r3s0q1<-filter(recentnew, racecat==3 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r3s0q1<-dplyr::select(r3s0q1, c("age", "prediction"))
r3s0q1 <- r3s0q1[order(age),]
r3s0q1<-as.data.frame(r3s0q1[,2])
r4s0q1<-filter(recentnew, racecat==4 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r4s0q1<-dplyr::select(r4s0q1, c("age", "prediction"))
r4s0q1 <- r4s0q1[order(age),]
r4s0q1<-as.data.frame(r4s0q1[,2])
r1s1q1<-filter(recentnew, racecat==1 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r1s1q1<-dplyr::select(r1s1q1, c("age", "prediction"))
r1s1q1 <- r1s1q1[order(age),]
r1s1q1<-as.data.frame(r1s1q1[,2])
r2s1q1<-filter(recentnew, racecat==2 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r2s1q1<-dplyr::select(r2s1q1, c("age", "prediction"))
r2s1q1 <- r2s1q1[order(age),]
r2s1q1<-as.data.frame(r2s1q1[,2])
r3s1q1<-filter(recentnew, racecat==3 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r3s1q1<-dplyr::select(r3s1q1, c("age", "prediction"))
r3s1q1 <- r3s1q1[order(age),]
r3s1q1<-as.data.frame(r3s1q1[,2])
r4s1q1<-filter(recentnew, racecat==4 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r4s1q1<-dplyr::select(r4s1q1, c("age", "prediction"))
r4s1q1 <- r4s1q1[order(age),]
r4s1q1<-as.data.frame(r4s1q1[,2])
#q2
r1s0q2<-filter(recentnew, racecat==1 & currentsmoking==0 & lifetimeoral_quart==0  & allcomb==1)
r1s0q2<-dplyr::select(r1s0q2, c("age", "prediction"))
r1s0q2 <- r1s0q2[order(age),]
r1s0q2<-as.data.frame(r1s0q2[,2])
r2s0q2<-filter(recentnew, racecat==2 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r2s0q2<-dplyr::select(r2s0q2, c("age", "prediction"))
r2s0q2 <- r2s0q2[order(age),]
r2s0q2<-as.data.frame(r2s0q2[,2])
r3s0q2<-filter(recentnew, racecat==3 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r3s0q2<-dplyr::select(r3s0q2, c("age", "prediction"))
r3s0q2 <- r3s0q2[order(age),]
r3s0q2<-as.data.frame(r3s0q2[,2])
r4s0q2<-filter(recentnew, racecat==4 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r4s0q2<-dplyr::select(r4s0q2, c("age", "prediction"))
r4s0q2 <- r4s0q2[order(age),]
r4s0q2<-as.data.frame(r4s0q2[,2])
r1s1q2<-filter(recentnew, racecat==1 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r1s1q2<-dplyr::select(r1s1q2, c("age", "prediction"))
r1s1q2 <- r1s1q2[order(age),]
r1s1q2<-as.data.frame(r1s1q2[,2])
r2s1q2<-filter(recentnew, racecat==2 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r2s1q2<-dplyr::select(r2s1q2, c("age", "prediction"))
r2s1q2 <- r2s1q2[order(age),]
r2s1q2<-as.data.frame(r2s1q2[,2])
r3s1q2<-filter(recentnew, racecat==3 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r3s1q2<-dplyr::select(r3s1q2, c("age", "prediction"))
r3s1q2 <- r3s1q2[order(age),]
r3s1q2<-as.data.frame(r3s1q2[,2])
r4s1q2<-filter(recentnew, racecat==4 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r4s1q2<-dplyr::select(r4s1q2, c("age", "prediction"))
r4s1q2 <- r4s1q2[order(age),]
r4s1q2<-as.data.frame(r4s1q2[,2])
#q3
r1s0q3<-filter(recentnew, racecat==1 & currentsmoking==0 & lifetimeoral_quart==0  & allcomb==1)
r1s0q3<-dplyr::select(r1s0q3, c("age", "prediction"))
r1s0q3 <- r1s0q3[order(age),]
r1s0q3<-as.data.frame(r1s0q3[,2])
r2s0q3<-filter(recentnew, racecat==2 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r2s0q3<-dplyr::select(r2s0q3, c("age", "prediction"))
r2s0q3 <- r2s0q3[order(age),]
r2s0q3<-as.data.frame(r2s0q3[,2])
r3s0q3<-filter(recentnew, racecat==3 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r3s0q3<-dplyr::select(r3s0q3, c("age", "prediction"))
r3s0q3 <- r3s0q3[order(age),]
r3s0q3<-as.data.frame(r3s0q3[,2])
r4s0q3<-filter(recentnew, racecat==4 & currentsmoking==0 & lifetimeoral_quart==0 & allcomb==1)
r4s0q3<-dplyr::select(r4s0q3, c("age", "prediction"))
r4s0q3 <- r4s0q3[order(age),]
r4s0q3<-as.data.frame(r4s0q3[,2])
r1s1q3<-filter(recentnew, racecat==1 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r1s1q3<-dplyr::select(r1s1q3, c("age", "prediction"))
r1s1q3 <- r1s1q3[order(age),]
r1s1q3<-as.data.frame(r1s1q3[,2])
r2s1q3<-filter(recentnew, racecat==2 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r2s1q3<-dplyr::select(r2s1q3, c("age", "prediction"))
r2s1q3 <- r2s1q3[order(age),]
r2s1q3<-as.data.frame(r2s1q3[,2])
r3s1q3<-filter(recentnew, racecat==3 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r3s1q3<-dplyr::select(r3s1q3, c("age", "prediction"))
r3s1q3 <- r3s1q3[order(age),]
r3s1q3<-as.data.frame(r3s1q3[,2])
r4s1q3<-filter(recentnew, racecat==4 & currentsmoking==1 & lifetimeoral_quart==0 & allcomb==1)
r4s1q3<-dplyr::select(r4s1q3, c("age", "prediction"))
r4s1q3 <- r4s1q3[order(age),]
r4s1q3<-as.data.frame(r4s1q3[,2])

out.dir2<-("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")
library(data.table)
#q0
fwrite(r1s0q0, file=paste0(out.dir2,"recentnewr1s0q0.csv"),col.names=F)
fwrite(r2s0q0, file=paste0(out.dir2,"recentnewr2s0q0.csv"),col.names=F)
fwrite(r3s0q0, file=paste0(out.dir2,"recentnewr3s0q0.csv"),col.names=F)
fwrite(r4s0q0, file=paste0(out.dir2,"recentnewr4s0q0.csv"),col.names=F)
fwrite(r1s1q0, file=paste0(out.dir2,"recentnewr1s1q0.csv"),col.names=F)
fwrite(r2s1q0, file=paste0(out.dir2,"recentnewr2s1q0.csv"),col.names=F)
fwrite(r3s1q0, file=paste0(out.dir2,"recentnewr3s1q0.csv"),col.names=F)
fwrite(r4s1q0, file=paste0(out.dir2,"recentnewr4s1q0.csv"),col.names=F)
#q1
fwrite(r1s0q1, file=paste0(out.dir2,"recentnewr1s0q1.csv"),col.names=F)
fwrite(r2s0q1, file=paste0(out.dir2,"recentnewr2s0q1.csv"),col.names=F)
fwrite(r3s0q1, file=paste0(out.dir2,"recentnewr3s0q1.csv"),col.names=F)
fwrite(r4s0q1, file=paste0(out.dir2,"recentnewr4s0q1.csv"),col.names=F)
fwrite(r1s1q1, file=paste0(out.dir2,"recentnewr1s1q1.csv"),col.names=F)
fwrite(r2s1q1, file=paste0(out.dir2,"recentnewr2s1q1.csv"),col.names=F)
fwrite(r3s1q1, file=paste0(out.dir2,"recentnewr3s1q1.csv"),col.names=F)
fwrite(r4s1q1, file=paste0(out.dir2,"recentnewr4s1q1.csv"),col.names=F)
#q2
fwrite(r1s0q2, file=paste0(out.dir2,"recentnewr1s0q2.csv"),col.names=F)
fwrite(r2s0q2, file=paste0(out.dir2,"recentnewr2s0q2.csv"),col.names=F)
fwrite(r3s0q2, file=paste0(out.dir2,"recentnewr3s0q2.csv"),col.names=F)
fwrite(r4s0q2, file=paste0(out.dir2,"recentnewr4s0q2.csv"),col.names=F)
fwrite(r1s1q2, file=paste0(out.dir2,"recentnewr1s1q2.csv"),col.names=F)
fwrite(r2s1q2, file=paste0(out.dir2,"recentnewr2s1q2.csv"),col.names=F)
fwrite(r3s1q2, file=paste0(out.dir2,"recentnewr3s1q2.csv"),col.names=F)
fwrite(r4s1q2, file=paste0(out.dir2,"recentnewr4s1q2.csv"),col.names=F)
#q3
fwrite(r1s0q3, file=paste0(out.dir2,"recentnewr1s0q3.csv"),col.names=F)
fwrite(r2s0q3, file=paste0(out.dir2,"recentnewr2s0q3.csv"),col.names=F)
fwrite(r3s0q3, file=paste0(out.dir2,"recentnewr3s0q3.csv"),col.names=F)
fwrite(r4s0q3, file=paste0(out.dir2,"recentnewr4s0q3.csv"),col.names=F)
fwrite(r1s1q3, file=paste0(out.dir2,"recentnewr1s1q3.csv"),col.names=F)
fwrite(r2s1q3, file=paste0(out.dir2,"recentnewr2s1q3.csv"),col.names=F)
fwrite(r3s1q3, file=paste0(out.dir2,"recentnewr3s1q3.csv"),col.names=F)
fwrite(r4s1q3, file=paste0(out.dir2,"recentnewr4s1q3.csv"),col.names=F)

