rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R

library(SASxport)
library(dplyr)
library(haven)
library(questionr)
library(weights)
library(pollster)


out.dir<-("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/")

source("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/R/nhis2009_16_for_weights_age_80up.R")

### weights from 2009/2016 so have sufficient N,
### full population of men, not only those who aren't vaccinated
### behav weights where available, otherwise MEC weights (15-17 and 70+)

#####

NHANES0916<-read_sas("C:/Users/landyrm/Desktop/microsim/Data/Final/jan2020c4.sas7bdat")
NHANES0916<-filter(NHANES0916, RIAGENDR==1)

#09-16: current smoking: SMQ040==1 or 2. not current smoker==3. weight for this variable is int weight
# SMQ020 - ever smoked 100 cigarettes : 2 = no
NHANES0916$currentsmoking=ifelse(NHANES0916$smoking==3,1,
                                 ifelse(NHANES0916$smoking>=1 & NHANES0916$smoking<=2,0,NA))

# race: 
# * ridreth1=1: Mexican American
# * ridreth1=2: other Hispanic
# * ridreth1=3: Non-Hispanic White
# * ridreth1=4: Non-Hispanic Black
# * ridreth1=5: other including multiracial;

NHANES0916$racecat<-as.factor(ifelse(NHANES0916$RIDRETH1>=1 & NHANES0916$RIDRETH1<=2, 1,
                          ifelse(NHANES0916$RIDRETH1==3,2,
                                 ifelse(NHANES0916$RIDRETH1==4, 3, 4))))

# racecat=1: Hispanic
# racecat=2: Non-Hispanic White
# racecat=3: Non-Hispanic Black
# racecat=4: other;

NHANES0916$race.smoking<-ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0,1,
                                ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0,2,
                                ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0,3,
                                ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0,4,
                                ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1,5,
                                ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1,6,
                                ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1,7,
                                ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1,8,NA))))))))


#for ages <18 use MEC weights, for ages >69 use MEC weights
NHANES0916$WTMEC8YR<-NHANES0916$WTMEC2YR/4

NHANES0916$weights<-ifelse(NHANES0916$RIDAGEYR<18, NHANES0916$WTMEC8YR,
                           ifelse(NHANES0916$RIDAGEYR<70, NHANES0916$behavweight8yr, NHANES0916$WTMEC8YR))


# create age-specific lifetimeoral quartiles
# age 18-59
for (x in 2:9) {
    assign(paste0("quart.oral.age",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeoral[NHANES0916$agecat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$agecat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
}

for (x in 2:11) {
    assign(paste0("quart.any.age",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeany2[NHANES0916$agecat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$agecat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
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
temp<-eval(parse(text =paste0("ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany2<=quart.any.age",x,"[1],1,
       ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany2<=quart.any.age",x,"[2],2,
              ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany2<=quart.any.age",x,"[3],3,
                     ifelse(NHANES0916$agecat==",x," & NHANES0916$lifetimeany2>quart.any.age",x,"[3] & is.na(NHANES0916$lifetimeany2)==FALSE,4,NA))))")))
NHANES0916$quartile.lifetimeany<-ifelse(is.na(temp)==TRUE,NHANES0916$quartile.lifetimeany,temp)
}

NHANES0916$quartile.lifetimeany<-as.factor(NHANES0916$quartile.lifetimeany)

#use lifetime oral quartile if recorded, otherwise lifetimeany
NHANES0916$quartile.lifetime<-ifelse(is.na(NHANES0916$quartile.lifetimeoral)==FALSE,NHANES0916$quartile.lifetimeoral,NHANES0916$quartile.lifetimeany)

table(NHANES0916$quartile.lifetime, NHANES0916$RIDAGEYR)




# use same race- and smoking-specific proportions for lifetime quartiles

#for ages 15-17, use data from men aged 18-20
NHANES0916_age1820<-filter(NHANES0916, age>=18 & age<=20)
temp<-crosstab(df=NHANES0916_age1820, x=race.smoking, y=quartile.lifetime, weight=weights, pct_type = "row", n=F)

q1.race1.smok0.1820<-temp[1,2]/100
q2.race1.smok0.1820<-temp[1,3]/100
q3.race1.smok0.1820<-temp[1,4]/100
q4.race1.smok0.1820<-temp[1,5]/100
q1.race2.smok0.1820<-temp[2,2]/100
q2.race2.smok0.1820<-temp[2,3]/100
q3.race2.smok0.1820<-temp[2,4]/100
q4.race2.smok0.1820<-temp[2,5]/100
q1.race3.smok0.1820<-temp[3,2]/100
q2.race3.smok0.1820<-temp[3,3]/100
q3.race3.smok0.1820<-temp[3,4]/100
q4.race3.smok0.1820<-temp[3,5]/100
q1.race4.smok0.1820<-temp[4,2]/100
q2.race4.smok0.1820<-temp[4,3]/100
q3.race4.smok0.1820<-temp[4,4]/100
q4.race4.smok0.1820<-temp[4,5]/100

q1.race1.smok1.1820<-temp[5,2]/100
q2.race1.smok1.1820<-temp[5,3]/100
q3.race1.smok1.1820<-temp[5,4]/100
q4.race1.smok1.1820<-temp[5,5]/100
q1.race2.smok1.1820<-temp[6,2]/100
q2.race2.smok1.1820<-temp[6,3]/100
q3.race2.smok1.1820<-temp[6,4]/100
q4.race2.smok1.1820<-temp[6,5]/100
q1.race3.smok1.1820<-temp[7,2]/100
q2.race3.smok1.1820<-temp[7,3]/100
q3.race3.smok1.1820<-temp[7,4]/100
q4.race3.smok1.1820<-temp[7,5]/100
q1.race4.smok1.1820<-temp[8,2]/100
q2.race4.smok1.1820<-temp[8,3]/100
q3.race4.smok1.1820<-temp[8,4]/100
q4.race4.smok1.1820<-temp[8,5]/100

#for ages 70-79, use data from men aged 65-69
NHANES0916_age6569<-filter(NHANES0916, age>=65 & age<=69)
temp<-crosstab(df=NHANES0916_age6569, x=race.smoking, y=quartile.lifetime, weight=weights, pct_type = "row", n=F)

q1.race1.smok0.6569<-temp[1,2]/100
q2.race1.smok0.6569<-temp[1,3]/100
q3.race1.smok0.6569<-temp[1,4]/100
q4.race1.smok0.6569<-temp[1,5]/100
q1.race2.smok0.6569<-temp[2,2]/100
q2.race2.smok0.6569<-temp[2,3]/100
q3.race2.smok0.6569<-temp[2,4]/100
q4.race2.smok0.6569<-temp[2,5]/100
q1.race3.smok0.6569<-temp[3,2]/100
q2.race3.smok0.6569<-temp[3,3]/100
q3.race3.smok0.6569<-temp[3,4]/100
q4.race3.smok0.6569<-temp[3,5]/100
q1.race4.smok0.6569<-temp[4,2]/100
q2.race4.smok0.6569<-temp[4,3]/100
q3.race4.smok0.6569<-temp[4,4]/100
q4.race4.smok0.6569<-temp[4,5]/100

q1.race1.smok1.6569<-temp[5,2]/100
q2.race1.smok1.6569<-temp[5,3]/100
q3.race1.smok1.6569<-temp[5,4]/100
q4.race1.smok1.6569<-temp[5,5]/100
q1.race2.smok1.6569<-temp[6,2]/100
q2.race2.smok1.6569<-temp[6,3]/100
q3.race2.smok1.6569<-temp[6,4]/100
q4.race2.smok1.6569<-temp[6,5]/100
q1.race3.smok1.6569<-temp[7,2]/100
q2.race3.smok1.6569<-temp[7,3]/100
q3.race3.smok1.6569<-temp[7,4]/100
q4.race3.smok1.6569<-temp[7,5]/100
q1.race4.smok1.6569<-temp[8,2]/100
q2.race4.smok1.6569<-temp[8,3]/100
q3.race4.smok1.6569<-temp[8,4]/100
q4.race4.smok1.6569<-temp[8,5]/100



NHANES0916$race.smoking.quartile<-ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,1,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,2,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,3,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,4,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,5,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,6,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,7,
                                  ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,8,

                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,9,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,10,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,11,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,12,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,13,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,14,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,15,
                                  ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,16,       
                                  
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,17,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,18,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,19,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,20,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,21,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,22,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,23,
                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,24,                                           
                                  
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,25,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,26,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,27,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,28,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,29,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,30,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,31,
                                  ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,32,NA))))))))))))))))))))))))))))))))

table(NHANES0916$race.smoking.quartile, NHANES0916$RIDAGEYR)

library(data.table)
temp<-as.data.table(crosstab(NHANES0916, RIDAGEYR,race.smoking.quartile, weights, pct_type = "row", n=F))

N15<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==15],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==15])
N16<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==16],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==16])
N17<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==17],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==17])
N70<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==70],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==70])
N71<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==71],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==71])
N72<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==72],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==72])
N73<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==73],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==73])
N74<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==74],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==74])
N75<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==75],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==75])
N76<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==76],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==76])
N77<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==77],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==77])
N78<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==78],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==78])
N79<-wtd.table(NHANES0916$race.smoking[NHANES0916$RIDAGEYR==79],weights = NHANES0916$weights[NHANES0916$RIDAGEYR==79])

N80<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$age_p==80],weights = nhis2009_16$wtfa_sa[nhis2009_16$age_p==80])
N81<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$age_p==81],weights = nhis2009_16$wtfa_sa[nhis2009_16$age_p==81])
N82<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$age_p==82],weights = nhis2009_16$wtfa_sa[nhis2009_16$age_p==82])
N83<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$age_p==83],weights = nhis2009_16$wtfa_sa[nhis2009_16$age_p==83])
N84<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$age_p==84],weights = nhis2009_16$wtfa_sa[nhis2009_16$age_p==84])



#####
results.15<-cbind(N15$sum.of.weights[1]*q1.race1.smok0.1820,N15$sum.of.weights[1]*q2.race1.smok0.1820,N15$sum.of.weights[1]*q3.race1.smok0.1820,N15$sum.of.weights[1]*q4.race1.smok0.1820,
              N15$sum.of.weights[5]*q1.race1.smok1.1820,N15$sum.of.weights[5]*q2.race1.smok1.1820,N15$sum.of.weights[5]*q3.race1.smok1.1820,N15$sum.of.weights[5]*q4.race1.smok1.1820,
              N15$sum.of.weights[2]*q1.race2.smok0.1820,N15$sum.of.weights[2]*q2.race2.smok0.1820,N15$sum.of.weights[2]*q3.race2.smok0.1820,N15$sum.of.weights[2]*q4.race2.smok0.1820,
              N15$sum.of.weights[6]*q1.race2.smok1.1820,N15$sum.of.weights[6]*q2.race2.smok1.1820,N15$sum.of.weights[6]*q3.race2.smok1.1820,N15$sum.of.weights[6]*q4.race2.smok1.1820,
              N15$sum.of.weights[3]*q1.race3.smok0.1820,N15$sum.of.weights[3]*q2.race3.smok0.1820,N15$sum.of.weights[3]*q3.race3.smok0.1820,N15$sum.of.weights[3]*q4.race3.smok0.1820,
              N15$sum.of.weights[7]*q1.race3.smok1.1820,N15$sum.of.weights[7]*q2.race3.smok1.1820,N15$sum.of.weights[7]*q3.race3.smok1.1820,N15$sum.of.weights[7]*q4.race3.smok1.1820,
              N15$sum.of.weights[4]*q1.race4.smok0.1820,N15$sum.of.weights[4]*q2.race4.smok0.1820,N15$sum.of.weights[4]*q3.race4.smok0.1820,N15$sum.of.weights[4]*q4.race4.smok0.1820,
              N15$sum.of.weights[8]*q1.race4.smok1.1820,N15$sum.of.weights[8]*q2.race4.smok1.1820,N15$sum.of.weights[8]*q3.race4.smok1.1820,N15$sum.of.weights[8]*q4.race4.smok1.1820)
results.16<-cbind(N16$sum.of.weights[1]*q1.race1.smok0.1820,N16$sum.of.weights[1]*q2.race1.smok0.1820,N16$sum.of.weights[1]*q3.race1.smok0.1820,N16$sum.of.weights[1]*q4.race1.smok0.1820,
                  N16$sum.of.weights[5]*q1.race1.smok1.1820,N16$sum.of.weights[5]*q2.race1.smok1.1820,N16$sum.of.weights[5]*q3.race1.smok1.1820,N16$sum.of.weights[5]*q4.race1.smok1.1820,
                  N16$sum.of.weights[2]*q1.race2.smok0.1820,N16$sum.of.weights[2]*q2.race2.smok0.1820,N16$sum.of.weights[2]*q3.race2.smok0.1820,N16$sum.of.weights[2]*q4.race2.smok0.1820,
                  N16$sum.of.weights[6]*q1.race2.smok1.1820,N16$sum.of.weights[6]*q2.race2.smok1.1820,N16$sum.of.weights[6]*q3.race2.smok1.1820,N16$sum.of.weights[6]*q4.race2.smok1.1820,
                  N16$sum.of.weights[3]*q1.race3.smok0.1820,N16$sum.of.weights[3]*q2.race3.smok0.1820,N16$sum.of.weights[3]*q3.race3.smok0.1820,N16$sum.of.weights[3]*q4.race3.smok0.1820,
                  N16$sum.of.weights[7]*q1.race3.smok1.1820,N16$sum.of.weights[7]*q2.race3.smok1.1820,N16$sum.of.weights[7]*q3.race3.smok1.1820,N16$sum.of.weights[7]*q4.race3.smok1.1820,
                  N16$sum.of.weights[4]*q1.race4.smok0.1820,N16$sum.of.weights[4]*q2.race4.smok0.1820,N16$sum.of.weights[4]*q3.race4.smok0.1820,N16$sum.of.weights[4]*q4.race4.smok0.1820,
                  N16$sum.of.weights[8]*q1.race4.smok1.1820,N16$sum.of.weights[8]*q2.race4.smok1.1820,N16$sum.of.weights[8]*q3.race4.smok1.1820,N16$sum.of.weights[8]*q4.race4.smok1.1820)
results.17<-cbind(N17$sum.of.weights[1]*q1.race1.smok0.1820,N17$sum.of.weights[1]*q2.race1.smok0.1820,N17$sum.of.weights[1]*q3.race1.smok0.1820,N17$sum.of.weights[1]*q4.race1.smok0.1820,
                  N17$sum.of.weights[5]*q1.race1.smok1.1820,N17$sum.of.weights[5]*q2.race1.smok1.1820,N17$sum.of.weights[5]*q3.race1.smok1.1820,N17$sum.of.weights[5]*q4.race1.smok1.1820,
                  N17$sum.of.weights[2]*q1.race2.smok0.1820,N17$sum.of.weights[2]*q2.race2.smok0.1820,N17$sum.of.weights[2]*q3.race2.smok0.1820,N17$sum.of.weights[2]*q4.race2.smok0.1820,
                  N17$sum.of.weights[6]*q1.race2.smok1.1820,N17$sum.of.weights[6]*q2.race2.smok1.1820,N17$sum.of.weights[6]*q3.race2.smok1.1820,N17$sum.of.weights[6]*q4.race2.smok1.1820,
                  N17$sum.of.weights[3]*q1.race3.smok0.1820,N17$sum.of.weights[3]*q2.race3.smok0.1820,N17$sum.of.weights[3]*q3.race3.smok0.1820,N17$sum.of.weights[3]*q4.race3.smok0.1820,
                  N17$sum.of.weights[7]*q1.race3.smok1.1820,N17$sum.of.weights[7]*q2.race3.smok1.1820,N17$sum.of.weights[7]*q3.race3.smok1.1820,N17$sum.of.weights[7]*q4.race3.smok1.1820,
                  N17$sum.of.weights[4]*q1.race4.smok0.1820,N17$sum.of.weights[4]*q2.race4.smok0.1820,N17$sum.of.weights[4]*q3.race4.smok0.1820,N17$sum.of.weights[4]*q4.race4.smok0.1820,
                  N17$sum.of.weights[8]*q1.race4.smok1.1820,N17$sum.of.weights[8]*q2.race4.smok1.1820,N17$sum.of.weights[8]*q3.race4.smok1.1820,N17$sum.of.weights[8]*q4.race4.smok1.1820)
results.70<-cbind(N70$sum.of.weights[1]*q1.race1.smok0.6569,N70$sum.of.weights[1]*q2.race1.smok0.6569,N70$sum.of.weights[1]*q3.race1.smok0.6569,N70$sum.of.weights[1]*q4.race1.smok0.6569,
                  N70$sum.of.weights[5]*q1.race1.smok1.6569,N70$sum.of.weights[5]*q2.race1.smok1.6569,N70$sum.of.weights[5]*q3.race1.smok1.6569,N70$sum.of.weights[5]*q4.race1.smok1.6569,
                  N70$sum.of.weights[2]*q1.race2.smok0.6569,N70$sum.of.weights[2]*q2.race2.smok0.6569,N70$sum.of.weights[2]*q3.race2.smok0.6569,N70$sum.of.weights[2]*q4.race2.smok0.6569,
                  N70$sum.of.weights[6]*q1.race2.smok1.6569,N70$sum.of.weights[6]*q2.race2.smok1.6569,N70$sum.of.weights[6]*q3.race2.smok1.6569,N70$sum.of.weights[6]*q4.race2.smok1.6569,
                  N70$sum.of.weights[3]*q1.race3.smok0.6569,N70$sum.of.weights[3]*q2.race3.smok0.6569,N70$sum.of.weights[3]*q3.race3.smok0.6569,N70$sum.of.weights[3]*q4.race3.smok0.6569,
                  N70$sum.of.weights[7]*q1.race3.smok1.6569,N70$sum.of.weights[7]*q2.race3.smok1.6569,N70$sum.of.weights[7]*q3.race3.smok1.6569,N70$sum.of.weights[7]*q4.race3.smok1.6569,
                  N70$sum.of.weights[4]*q1.race4.smok0.6569,N70$sum.of.weights[4]*q2.race4.smok0.6569,N70$sum.of.weights[4]*q3.race4.smok0.6569,N70$sum.of.weights[4]*q4.race4.smok0.6569,
                  N70$sum.of.weights[8]*q1.race4.smok1.6569,N70$sum.of.weights[8]*q2.race4.smok1.6569,N70$sum.of.weights[8]*q3.race4.smok1.6569,N70$sum.of.weights[8]*q4.race4.smok1.6569)
results.71<-cbind(N71$sum.of.weights[1]*q1.race1.smok0.6569,N71$sum.of.weights[1]*q2.race1.smok0.6569,N71$sum.of.weights[1]*q3.race1.smok0.6569,N71$sum.of.weights[1]*q4.race1.smok0.6569,
                  N71$sum.of.weights[5]*q1.race1.smok1.6569,N71$sum.of.weights[5]*q2.race1.smok1.6569,N71$sum.of.weights[5]*q3.race1.smok1.6569,N71$sum.of.weights[5]*q4.race1.smok1.6569,
                  N71$sum.of.weights[2]*q1.race2.smok0.6569,N71$sum.of.weights[2]*q2.race2.smok0.6569,N71$sum.of.weights[2]*q3.race2.smok0.6569,N71$sum.of.weights[2]*q4.race2.smok0.6569,
                  N71$sum.of.weights[6]*q1.race2.smok1.6569,N71$sum.of.weights[6]*q2.race2.smok1.6569,N71$sum.of.weights[6]*q3.race2.smok1.6569,N71$sum.of.weights[6]*q4.race2.smok1.6569,
                  N71$sum.of.weights[3]*q1.race3.smok0.6569,N71$sum.of.weights[3]*q2.race3.smok0.6569,N71$sum.of.weights[3]*q3.race3.smok0.6569,N71$sum.of.weights[3]*q4.race3.smok0.6569,
                  N71$sum.of.weights[7]*q1.race3.smok1.6569,N71$sum.of.weights[7]*q2.race3.smok1.6569,N71$sum.of.weights[7]*q3.race3.smok1.6569,N71$sum.of.weights[7]*q4.race3.smok1.6569,
                  N71$sum.of.weights[4]*q1.race4.smok0.6569,N71$sum.of.weights[4]*q2.race4.smok0.6569,N71$sum.of.weights[4]*q3.race4.smok0.6569,N71$sum.of.weights[4]*q4.race4.smok0.6569,
                  N71$sum.of.weights[8]*q1.race4.smok1.6569,N71$sum.of.weights[8]*q2.race4.smok1.6569,N71$sum.of.weights[8]*q3.race4.smok1.6569,N71$sum.of.weights[8]*q4.race4.smok1.6569)
results.72<-cbind(N72$sum.of.weights[1]*q1.race1.smok0.6569,N72$sum.of.weights[1]*q2.race1.smok0.6569,N72$sum.of.weights[1]*q3.race1.smok0.6569,N72$sum.of.weights[1]*q4.race1.smok0.6569,
                  N72$sum.of.weights[5]*q1.race1.smok1.6569,N72$sum.of.weights[5]*q2.race1.smok1.6569,N72$sum.of.weights[5]*q3.race1.smok1.6569,N72$sum.of.weights[5]*q4.race1.smok1.6569,
                  N72$sum.of.weights[2]*q1.race2.smok0.6569,N72$sum.of.weights[2]*q2.race2.smok0.6569,N72$sum.of.weights[2]*q3.race2.smok0.6569,N72$sum.of.weights[2]*q4.race2.smok0.6569,
                  N72$sum.of.weights[6]*q1.race2.smok1.6569,N72$sum.of.weights[6]*q2.race2.smok1.6569,N72$sum.of.weights[6]*q3.race2.smok1.6569,N72$sum.of.weights[6]*q4.race2.smok1.6569,
                  N72$sum.of.weights[3]*q1.race3.smok0.6569,N72$sum.of.weights[3]*q2.race3.smok0.6569,N72$sum.of.weights[3]*q3.race3.smok0.6569,N72$sum.of.weights[3]*q4.race3.smok0.6569,
                  N72$sum.of.weights[7]*q1.race3.smok1.6569,N72$sum.of.weights[7]*q2.race3.smok1.6569,N72$sum.of.weights[7]*q3.race3.smok1.6569,N72$sum.of.weights[7]*q4.race3.smok1.6569,
                  N72$sum.of.weights[4]*q1.race4.smok0.6569,N72$sum.of.weights[4]*q2.race4.smok0.6569,N72$sum.of.weights[4]*q3.race4.smok0.6569,N72$sum.of.weights[4]*q4.race4.smok0.6569,
                  N72$sum.of.weights[8]*q1.race4.smok1.6569,N72$sum.of.weights[8]*q2.race4.smok1.6569,N72$sum.of.weights[8]*q3.race4.smok1.6569,N72$sum.of.weights[8]*q4.race4.smok1.6569)
results.73<-cbind(N73$sum.of.weights[1]*q1.race1.smok0.6569,N73$sum.of.weights[1]*q2.race1.smok0.6569,N73$sum.of.weights[1]*q3.race1.smok0.6569,N73$sum.of.weights[1]*q4.race1.smok0.6569,
                  N73$sum.of.weights[5]*q1.race1.smok1.6569,N73$sum.of.weights[5]*q2.race1.smok1.6569,N73$sum.of.weights[5]*q3.race1.smok1.6569,N73$sum.of.weights[5]*q4.race1.smok1.6569,
                  N73$sum.of.weights[2]*q1.race2.smok0.6569,N73$sum.of.weights[2]*q2.race2.smok0.6569,N73$sum.of.weights[2]*q3.race2.smok0.6569,N73$sum.of.weights[2]*q4.race2.smok0.6569,
                  N73$sum.of.weights[6]*q1.race2.smok1.6569,N73$sum.of.weights[6]*q2.race2.smok1.6569,N73$sum.of.weights[6]*q3.race2.smok1.6569,N73$sum.of.weights[6]*q4.race2.smok1.6569,
                  N73$sum.of.weights[3]*q1.race3.smok0.6569,N73$sum.of.weights[3]*q2.race3.smok0.6569,N73$sum.of.weights[3]*q3.race3.smok0.6569,N73$sum.of.weights[3]*q4.race3.smok0.6569,
                  N73$sum.of.weights[7]*q1.race3.smok1.6569,N73$sum.of.weights[7]*q2.race3.smok1.6569,N73$sum.of.weights[7]*q3.race3.smok1.6569,N73$sum.of.weights[7]*q4.race3.smok1.6569,
                  N73$sum.of.weights[4]*q1.race4.smok0.6569,N73$sum.of.weights[4]*q2.race4.smok0.6569,N73$sum.of.weights[4]*q3.race4.smok0.6569,N73$sum.of.weights[4]*q4.race4.smok0.6569,
                  N73$sum.of.weights[8]*q1.race4.smok1.6569,N73$sum.of.weights[8]*q2.race4.smok1.6569,N73$sum.of.weights[8]*q3.race4.smok1.6569,N73$sum.of.weights[8]*q4.race4.smok1.6569)
results.74<-cbind(N74$sum.of.weights[1]*q1.race1.smok0.6569,N74$sum.of.weights[1]*q2.race1.smok0.6569,N74$sum.of.weights[1]*q3.race1.smok0.6569,N74$sum.of.weights[1]*q4.race1.smok0.6569,
                  N74$sum.of.weights[5]*q1.race1.smok1.6569,N74$sum.of.weights[5]*q2.race1.smok1.6569,N74$sum.of.weights[5]*q3.race1.smok1.6569,N74$sum.of.weights[5]*q4.race1.smok1.6569,
                  N74$sum.of.weights[2]*q1.race2.smok0.6569,N74$sum.of.weights[2]*q2.race2.smok0.6569,N74$sum.of.weights[2]*q3.race2.smok0.6569,N74$sum.of.weights[2]*q4.race2.smok0.6569,
                  N74$sum.of.weights[6]*q1.race2.smok1.6569,N74$sum.of.weights[6]*q2.race2.smok1.6569,N74$sum.of.weights[6]*q3.race2.smok1.6569,N74$sum.of.weights[6]*q4.race2.smok1.6569,
                  N74$sum.of.weights[3]*q1.race3.smok0.6569,N74$sum.of.weights[3]*q2.race3.smok0.6569,N74$sum.of.weights[3]*q3.race3.smok0.6569,N74$sum.of.weights[3]*q4.race3.smok0.6569,
                  N74$sum.of.weights[7]*q1.race3.smok1.6569,N74$sum.of.weights[7]*q2.race3.smok1.6569,N74$sum.of.weights[7]*q3.race3.smok1.6569,N74$sum.of.weights[7]*q4.race3.smok1.6569,
                  N74$sum.of.weights[4]*q1.race4.smok0.6569,N74$sum.of.weights[4]*q2.race4.smok0.6569,N74$sum.of.weights[4]*q3.race4.smok0.6569,N74$sum.of.weights[4]*q4.race4.smok0.6569,
                  N74$sum.of.weights[8]*q1.race4.smok1.6569,N74$sum.of.weights[8]*q2.race4.smok1.6569,N74$sum.of.weights[8]*q3.race4.smok1.6569,N74$sum.of.weights[8]*q4.race4.smok1.6569)
results.75<-cbind(N75$sum.of.weights[1]*q1.race1.smok0.6569,N75$sum.of.weights[1]*q2.race1.smok0.6569,N75$sum.of.weights[1]*q3.race1.smok0.6569,N75$sum.of.weights[1]*q4.race1.smok0.6569,
                  N75$sum.of.weights[5]*q1.race1.smok1.6569,N75$sum.of.weights[5]*q2.race1.smok1.6569,N75$sum.of.weights[5]*q3.race1.smok1.6569,N75$sum.of.weights[5]*q4.race1.smok1.6569,
                  N75$sum.of.weights[2]*q1.race2.smok0.6569,N75$sum.of.weights[2]*q2.race2.smok0.6569,N75$sum.of.weights[2]*q3.race2.smok0.6569,N75$sum.of.weights[2]*q4.race2.smok0.6569,
                  N75$sum.of.weights[6]*q1.race2.smok1.6569,N75$sum.of.weights[6]*q2.race2.smok1.6569,N75$sum.of.weights[6]*q3.race2.smok1.6569,N75$sum.of.weights[6]*q4.race2.smok1.6569,
                  N75$sum.of.weights[3]*q1.race3.smok0.6569,N75$sum.of.weights[3]*q2.race3.smok0.6569,N75$sum.of.weights[3]*q3.race3.smok0.6569,N75$sum.of.weights[3]*q4.race3.smok0.6569,
                  N75$sum.of.weights[7]*q1.race3.smok1.6569,N75$sum.of.weights[7]*q2.race3.smok1.6569,N75$sum.of.weights[7]*q3.race3.smok1.6569,N75$sum.of.weights[7]*q4.race3.smok1.6569,
                  N75$sum.of.weights[4]*q1.race4.smok0.6569,N75$sum.of.weights[4]*q2.race4.smok0.6569,N75$sum.of.weights[4]*q3.race4.smok0.6569,N75$sum.of.weights[4]*q4.race4.smok0.6569,
                  N75$sum.of.weights[8]*q1.race4.smok1.6569,N75$sum.of.weights[8]*q2.race4.smok1.6569,N75$sum.of.weights[8]*q3.race4.smok1.6569,N75$sum.of.weights[8]*q4.race4.smok1.6569)
results.76<-cbind(N76$sum.of.weights[1]*q1.race1.smok0.6569,N76$sum.of.weights[1]*q2.race1.smok0.6569,N76$sum.of.weights[1]*q3.race1.smok0.6569,N76$sum.of.weights[1]*q4.race1.smok0.6569,
                  N76$sum.of.weights[5]*q1.race1.smok1.6569,N76$sum.of.weights[5]*q2.race1.smok1.6569,N76$sum.of.weights[5]*q3.race1.smok1.6569,N76$sum.of.weights[5]*q4.race1.smok1.6569,
                  N76$sum.of.weights[2]*q1.race2.smok0.6569,N76$sum.of.weights[2]*q2.race2.smok0.6569,N76$sum.of.weights[2]*q3.race2.smok0.6569,N76$sum.of.weights[2]*q4.race2.smok0.6569,
                  N76$sum.of.weights[6]*q1.race2.smok1.6569,N76$sum.of.weights[6]*q2.race2.smok1.6569,N76$sum.of.weights[6]*q3.race2.smok1.6569,N76$sum.of.weights[6]*q4.race2.smok1.6569,
                  N76$sum.of.weights[3]*q1.race3.smok0.6569,N76$sum.of.weights[3]*q2.race3.smok0.6569,N76$sum.of.weights[3]*q3.race3.smok0.6569,N76$sum.of.weights[3]*q4.race3.smok0.6569,
                  N76$sum.of.weights[7]*q1.race3.smok1.6569,N76$sum.of.weights[7]*q2.race3.smok1.6569,N76$sum.of.weights[7]*q3.race3.smok1.6569,N76$sum.of.weights[7]*q4.race3.smok1.6569,
                  N76$sum.of.weights[4]*q1.race4.smok0.6569,N76$sum.of.weights[4]*q2.race4.smok0.6569,N76$sum.of.weights[4]*q3.race4.smok0.6569,N76$sum.of.weights[4]*q4.race4.smok0.6569,
                  N76$sum.of.weights[8]*q1.race4.smok1.6569,N76$sum.of.weights[8]*q2.race4.smok1.6569,N76$sum.of.weights[8]*q3.race4.smok1.6569,N76$sum.of.weights[8]*q4.race4.smok1.6569)
results.77<-cbind(N77$sum.of.weights[1]*q1.race1.smok0.6569,N77$sum.of.weights[1]*q2.race1.smok0.6569,N77$sum.of.weights[1]*q3.race1.smok0.6569,N77$sum.of.weights[1]*q4.race1.smok0.6569,
                  N77$sum.of.weights[5]*q1.race1.smok1.6569,N77$sum.of.weights[5]*q2.race1.smok1.6569,N77$sum.of.weights[5]*q3.race1.smok1.6569,N77$sum.of.weights[5]*q4.race1.smok1.6569,
                  N77$sum.of.weights[2]*q1.race2.smok0.6569,N77$sum.of.weights[2]*q2.race2.smok0.6569,N77$sum.of.weights[2]*q3.race2.smok0.6569,N77$sum.of.weights[2]*q4.race2.smok0.6569,
                  N77$sum.of.weights[6]*q1.race2.smok1.6569,N77$sum.of.weights[6]*q2.race2.smok1.6569,N77$sum.of.weights[6]*q3.race2.smok1.6569,N77$sum.of.weights[6]*q4.race2.smok1.6569,
                  N77$sum.of.weights[3]*q1.race3.smok0.6569,N77$sum.of.weights[3]*q2.race3.smok0.6569,N77$sum.of.weights[3]*q3.race3.smok0.6569,N77$sum.of.weights[3]*q4.race3.smok0.6569,
                  N77$sum.of.weights[7]*q1.race3.smok1.6569,N77$sum.of.weights[7]*q2.race3.smok1.6569,N77$sum.of.weights[7]*q3.race3.smok1.6569,N77$sum.of.weights[7]*q4.race3.smok1.6569,
                  N77$sum.of.weights[4]*q1.race4.smok0.6569,N77$sum.of.weights[4]*q2.race4.smok0.6569,N77$sum.of.weights[4]*q3.race4.smok0.6569,N77$sum.of.weights[4]*q4.race4.smok0.6569,
                  N77$sum.of.weights[8]*q1.race4.smok1.6569,N77$sum.of.weights[8]*q2.race4.smok1.6569,N77$sum.of.weights[8]*q3.race4.smok1.6569,N77$sum.of.weights[8]*q4.race4.smok1.6569)
results.78<-cbind(N78$sum.of.weights[1]*q1.race1.smok0.6569,N78$sum.of.weights[1]*q2.race1.smok0.6569,N78$sum.of.weights[1]*q3.race1.smok0.6569,N78$sum.of.weights[1]*q4.race1.smok0.6569,
                  N78$sum.of.weights[5]*q1.race1.smok1.6569,N78$sum.of.weights[5]*q2.race1.smok1.6569,N78$sum.of.weights[5]*q3.race1.smok1.6569,N78$sum.of.weights[5]*q4.race1.smok1.6569,
                  N78$sum.of.weights[2]*q1.race2.smok0.6569,N78$sum.of.weights[2]*q2.race2.smok0.6569,N78$sum.of.weights[2]*q3.race2.smok0.6569,N78$sum.of.weights[2]*q4.race2.smok0.6569,
                  N78$sum.of.weights[6]*q1.race2.smok1.6569,N78$sum.of.weights[6]*q2.race2.smok1.6569,N78$sum.of.weights[6]*q3.race2.smok1.6569,N78$sum.of.weights[6]*q4.race2.smok1.6569,
                  N78$sum.of.weights[3]*q1.race3.smok0.6569,N78$sum.of.weights[3]*q2.race3.smok0.6569,N78$sum.of.weights[3]*q3.race3.smok0.6569,N78$sum.of.weights[3]*q4.race3.smok0.6569,
                  N78$sum.of.weights[7]*q1.race3.smok1.6569,N78$sum.of.weights[7]*q2.race3.smok1.6569,N78$sum.of.weights[7]*q3.race3.smok1.6569,N78$sum.of.weights[7]*q4.race3.smok1.6569,
                  N78$sum.of.weights[4]*q1.race4.smok0.6569,N78$sum.of.weights[4]*q2.race4.smok0.6569,N78$sum.of.weights[4]*q3.race4.smok0.6569,N78$sum.of.weights[4]*q4.race4.smok0.6569,
                  N78$sum.of.weights[8]*q1.race4.smok1.6569,N78$sum.of.weights[8]*q2.race4.smok1.6569,N78$sum.of.weights[8]*q3.race4.smok1.6569,N78$sum.of.weights[8]*q4.race4.smok1.6569)
results.79<-cbind(N79$sum.of.weights[1]*q1.race1.smok0.6569,N79$sum.of.weights[1]*q2.race1.smok0.6569,N79$sum.of.weights[1]*q3.race1.smok0.6569,N79$sum.of.weights[1]*q4.race1.smok0.6569,
                  N79$sum.of.weights[5]*q1.race1.smok1.6569,N79$sum.of.weights[5]*q2.race1.smok1.6569,N79$sum.of.weights[5]*q3.race1.smok1.6569,N79$sum.of.weights[5]*q4.race1.smok1.6569,
                  N79$sum.of.weights[2]*q1.race2.smok0.6569,N79$sum.of.weights[2]*q2.race2.smok0.6569,N79$sum.of.weights[2]*q3.race2.smok0.6569,N79$sum.of.weights[2]*q4.race2.smok0.6569,
                  N79$sum.of.weights[6]*q1.race2.smok1.6569,N79$sum.of.weights[6]*q2.race2.smok1.6569,N79$sum.of.weights[6]*q3.race2.smok1.6569,N79$sum.of.weights[6]*q4.race2.smok1.6569,
                  N79$sum.of.weights[3]*q1.race3.smok0.6569,N79$sum.of.weights[3]*q2.race3.smok0.6569,N79$sum.of.weights[3]*q3.race3.smok0.6569,N79$sum.of.weights[3]*q4.race3.smok0.6569,
                  N79$sum.of.weights[7]*q1.race3.smok1.6569,N79$sum.of.weights[7]*q2.race3.smok1.6569,N79$sum.of.weights[7]*q3.race3.smok1.6569,N79$sum.of.weights[7]*q4.race3.smok1.6569,
                  N79$sum.of.weights[4]*q1.race4.smok0.6569,N79$sum.of.weights[4]*q2.race4.smok0.6569,N79$sum.of.weights[4]*q3.race4.smok0.6569,N79$sum.of.weights[4]*q4.race4.smok0.6569,
                  N79$sum.of.weights[8]*q1.race4.smok1.6569,N79$sum.of.weights[8]*q2.race4.smok1.6569,N79$sum.of.weights[8]*q3.race4.smok1.6569,N79$sum.of.weights[8]*q4.race4.smok1.6569)
results.80<-cbind(N80$sum.of.weights[1]*q1.race1.smok0.6569,N80$sum.of.weights[1]*q2.race1.smok0.6569,N80$sum.of.weights[1]*q3.race1.smok0.6569,N80$sum.of.weights[1]*q4.race1.smok0.6569,
                  N80$sum.of.weights[5]*q1.race1.smok1.6569,N80$sum.of.weights[5]*q2.race1.smok1.6569,N80$sum.of.weights[5]*q3.race1.smok1.6569,N80$sum.of.weights[5]*q4.race1.smok1.6569,
                  N80$sum.of.weights[2]*q1.race2.smok0.6569,N80$sum.of.weights[2]*q2.race2.smok0.6569,N80$sum.of.weights[2]*q3.race2.smok0.6569,N80$sum.of.weights[2]*q4.race2.smok0.6569,
                  N80$sum.of.weights[6]*q1.race2.smok1.6569,N80$sum.of.weights[6]*q2.race2.smok1.6569,N80$sum.of.weights[6]*q3.race2.smok1.6569,N80$sum.of.weights[6]*q4.race2.smok1.6569,
                  N80$sum.of.weights[3]*q1.race3.smok0.6569,N80$sum.of.weights[3]*q2.race3.smok0.6569,N80$sum.of.weights[3]*q3.race3.smok0.6569,N80$sum.of.weights[3]*q4.race3.smok0.6569,
                  N80$sum.of.weights[7]*q1.race3.smok1.6569,N80$sum.of.weights[7]*q2.race3.smok1.6569,N80$sum.of.weights[7]*q3.race3.smok1.6569,N80$sum.of.weights[7]*q4.race3.smok1.6569,
                  N80$sum.of.weights[4]*q1.race4.smok0.6569,N80$sum.of.weights[4]*q2.race4.smok0.6569,N80$sum.of.weights[4]*q3.race4.smok0.6569,N80$sum.of.weights[4]*q4.race4.smok0.6569,
                  N80$sum.of.weights[8]*q1.race4.smok1.6569,N80$sum.of.weights[8]*q2.race4.smok1.6569,N80$sum.of.weights[8]*q3.race4.smok1.6569,N80$sum.of.weights[8]*q4.race4.smok1.6569)
results.81<-cbind(N81$sum.of.weights[1]*q1.race1.smok0.6569,N81$sum.of.weights[1]*q2.race1.smok0.6569,N81$sum.of.weights[1]*q3.race1.smok0.6569,N81$sum.of.weights[1]*q4.race1.smok0.6569,
                  N81$sum.of.weights[5]*q1.race1.smok1.6569,N81$sum.of.weights[5]*q2.race1.smok1.6569,N81$sum.of.weights[5]*q3.race1.smok1.6569,N81$sum.of.weights[5]*q4.race1.smok1.6569,
                  N81$sum.of.weights[2]*q1.race2.smok0.6569,N81$sum.of.weights[2]*q2.race2.smok0.6569,N81$sum.of.weights[2]*q3.race2.smok0.6569,N81$sum.of.weights[2]*q4.race2.smok0.6569,
                  N81$sum.of.weights[6]*q1.race2.smok1.6569,N81$sum.of.weights[6]*q2.race2.smok1.6569,N81$sum.of.weights[6]*q3.race2.smok1.6569,N81$sum.of.weights[6]*q4.race2.smok1.6569,
                  N81$sum.of.weights[3]*q1.race3.smok0.6569,N81$sum.of.weights[3]*q2.race3.smok0.6569,N81$sum.of.weights[3]*q3.race3.smok0.6569,N81$sum.of.weights[3]*q4.race3.smok0.6569,
                  N81$sum.of.weights[7]*q1.race3.smok1.6569,N81$sum.of.weights[7]*q2.race3.smok1.6569,N81$sum.of.weights[7]*q3.race3.smok1.6569,N81$sum.of.weights[7]*q4.race3.smok1.6569,
                  N81$sum.of.weights[4]*q1.race4.smok0.6569,N81$sum.of.weights[4]*q2.race4.smok0.6569,N81$sum.of.weights[4]*q3.race4.smok0.6569,N81$sum.of.weights[4]*q4.race4.smok0.6569,
                  N81$sum.of.weights[8]*q1.race4.smok1.6569,N81$sum.of.weights[8]*q2.race4.smok1.6569,N81$sum.of.weights[8]*q3.race4.smok1.6569,N81$sum.of.weights[8]*q4.race4.smok1.6569)
results.82<-cbind(N82$sum.of.weights[1]*q1.race1.smok0.6569,N82$sum.of.weights[1]*q2.race1.smok0.6569,N82$sum.of.weights[1]*q3.race1.smok0.6569,N82$sum.of.weights[1]*q4.race1.smok0.6569,
                  N82$sum.of.weights[5]*q1.race1.smok1.6569,N82$sum.of.weights[5]*q2.race1.smok1.6569,N82$sum.of.weights[5]*q3.race1.smok1.6569,N82$sum.of.weights[5]*q4.race1.smok1.6569,
                  N82$sum.of.weights[2]*q1.race2.smok0.6569,N82$sum.of.weights[2]*q2.race2.smok0.6569,N82$sum.of.weights[2]*q3.race2.smok0.6569,N82$sum.of.weights[2]*q4.race2.smok0.6569,
                  N82$sum.of.weights[6]*q1.race2.smok1.6569,N82$sum.of.weights[6]*q2.race2.smok1.6569,N82$sum.of.weights[6]*q3.race2.smok1.6569,N82$sum.of.weights[6]*q4.race2.smok1.6569,
                  N82$sum.of.weights[3]*q1.race3.smok0.6569,N82$sum.of.weights[3]*q2.race3.smok0.6569,N82$sum.of.weights[3]*q3.race3.smok0.6569,N82$sum.of.weights[3]*q4.race3.smok0.6569,
                  N82$sum.of.weights[7]*q1.race3.smok1.6569,N82$sum.of.weights[7]*q2.race3.smok1.6569,N82$sum.of.weights[7]*q3.race3.smok1.6569,N82$sum.of.weights[7]*q4.race3.smok1.6569,
                  N82$sum.of.weights[4]*q1.race4.smok0.6569,N82$sum.of.weights[4]*q2.race4.smok0.6569,N82$sum.of.weights[4]*q3.race4.smok0.6569,N82$sum.of.weights[4]*q4.race4.smok0.6569,
                  N82$sum.of.weights[8]*q1.race4.smok1.6569,N82$sum.of.weights[8]*q2.race4.smok1.6569,N82$sum.of.weights[8]*q3.race4.smok1.6569,N82$sum.of.weights[8]*q4.race4.smok1.6569)
results.83<-cbind(N83$sum.of.weights[1]*q1.race1.smok0.6569,N83$sum.of.weights[1]*q2.race1.smok0.6569,N83$sum.of.weights[1]*q3.race1.smok0.6569,N83$sum.of.weights[1]*q4.race1.smok0.6569,
                  N83$sum.of.weights[5]*q1.race1.smok1.6569,N83$sum.of.weights[5]*q2.race1.smok1.6569,N83$sum.of.weights[5]*q3.race1.smok1.6569,N83$sum.of.weights[5]*q4.race1.smok1.6569,
                  N83$sum.of.weights[2]*q1.race2.smok0.6569,N83$sum.of.weights[2]*q2.race2.smok0.6569,N83$sum.of.weights[2]*q3.race2.smok0.6569,N83$sum.of.weights[2]*q4.race2.smok0.6569,
                  N83$sum.of.weights[6]*q1.race2.smok1.6569,N83$sum.of.weights[6]*q2.race2.smok1.6569,N83$sum.of.weights[6]*q3.race2.smok1.6569,N83$sum.of.weights[6]*q4.race2.smok1.6569,
                  N83$sum.of.weights[3]*q1.race3.smok0.6569,N83$sum.of.weights[3]*q2.race3.smok0.6569,N83$sum.of.weights[3]*q3.race3.smok0.6569,N83$sum.of.weights[3]*q4.race3.smok0.6569,
                  N83$sum.of.weights[7]*q1.race3.smok1.6569,N83$sum.of.weights[7]*q2.race3.smok1.6569,N83$sum.of.weights[7]*q3.race3.smok1.6569,N83$sum.of.weights[7]*q4.race3.smok1.6569,
                  N83$sum.of.weights[4]*q1.race4.smok0.6569,N83$sum.of.weights[4]*q2.race4.smok0.6569,N83$sum.of.weights[4]*q3.race4.smok0.6569,N83$sum.of.weights[4]*q4.race4.smok0.6569,
                  N83$sum.of.weights[8]*q1.race4.smok1.6569,N83$sum.of.weights[8]*q2.race4.smok1.6569,N83$sum.of.weights[8]*q3.race4.smok1.6569,N83$sum.of.weights[8]*q4.race4.smok1.6569)
results.84<-cbind(N84$sum.of.weights[1]*q1.race1.smok0.6569,N84$sum.of.weights[1]*q2.race1.smok0.6569,N84$sum.of.weights[1]*q3.race1.smok0.6569,N84$sum.of.weights[1]*q4.race1.smok0.6569,
                  N84$sum.of.weights[5]*q1.race1.smok1.6569,N84$sum.of.weights[5]*q2.race1.smok1.6569,N84$sum.of.weights[5]*q3.race1.smok1.6569,N84$sum.of.weights[5]*q4.race1.smok1.6569,
                  N84$sum.of.weights[2]*q1.race2.smok0.6569,N84$sum.of.weights[2]*q2.race2.smok0.6569,N84$sum.of.weights[2]*q3.race2.smok0.6569,N84$sum.of.weights[2]*q4.race2.smok0.6569,
                  N84$sum.of.weights[6]*q1.race2.smok1.6569,N84$sum.of.weights[6]*q2.race2.smok1.6569,N84$sum.of.weights[6]*q3.race2.smok1.6569,N84$sum.of.weights[6]*q4.race2.smok1.6569,
                  N84$sum.of.weights[3]*q1.race3.smok0.6569,N84$sum.of.weights[3]*q2.race3.smok0.6569,N84$sum.of.weights[3]*q3.race3.smok0.6569,N84$sum.of.weights[3]*q4.race3.smok0.6569,
                  N84$sum.of.weights[7]*q1.race3.smok1.6569,N84$sum.of.weights[7]*q2.race3.smok1.6569,N84$sum.of.weights[7]*q3.race3.smok1.6569,N84$sum.of.weights[7]*q4.race3.smok1.6569,
                  N84$sum.of.weights[4]*q1.race4.smok0.6569,N84$sum.of.weights[4]*q2.race4.smok0.6569,N84$sum.of.weights[4]*q3.race4.smok0.6569,N84$sum.of.weights[4]*q4.race4.smok0.6569,
                  N84$sum.of.weights[8]*q1.race4.smok1.6569,N84$sum.of.weights[8]*q2.race4.smok1.6569,N84$sum.of.weights[8]*q3.race4.smok1.6569,N84$sum.of.weights[8]*q4.race4.smok1.6569)

r15<-cbind(15, results.15/sum(results.15, na.rm=TRUE))
r16<-cbind(16, results.16/sum(results.16, na.rm=TRUE))
r16[is.na(r16)] <- 0
r17<-cbind(17, results.17/sum(results.17, na.rm=TRUE))
r70<-cbind(70, results.70/sum(results.70, na.rm=TRUE))
r71<-cbind(71, results.71/sum(results.71, na.rm=TRUE))
r72<-cbind(72, results.72/sum(results.72, na.rm=TRUE))
r73<-cbind(73, results.73/sum(results.73, na.rm=TRUE))
r74<-cbind(74, results.74/sum(results.74, na.rm=TRUE))
r75<-cbind(75, results.75/sum(results.75, na.rm=TRUE))
r76<-cbind(76, results.76/sum(results.76, na.rm=TRUE))
r77<-cbind(77, results.77/sum(results.77, na.rm=TRUE))
r78<-cbind(78, results.78/sum(results.78, na.rm=TRUE))
r79<-cbind(79, results.79/sum(results.79, na.rm=TRUE))
r80<-cbind(80, results.80/sum(results.80, na.rm=TRUE))
r81<-cbind(81, results.81/sum(results.81, na.rm=TRUE))
r82<-cbind(82, results.82/sum(results.82, na.rm=TRUE))
r83<-cbind(83, results.83/sum(results.83, na.rm=TRUE))
r84<-cbind(84, results.84/sum(results.84, na.rm=TRUE))

r70[is.na(r70)] <- 0
r77[is.na(r77)] <- 0
r79[is.na(r79)] <- 0
r82[is.na(r82)] <- 0
r84[is.na(r84)] <- 0

#####
temp2<-cbind(temp[,1],temp[,2:33]/100)
temp3<-mapply(c,r15,r16,r17,temp2,r70,r71,r72,r73,r74,r75,r76,r77,r78,r79,r80,r81,r82,r83,r84)
View(temp3)

fwrite(temp3, paste0(out.dir,"weights.csv"), append=F)







###### creating weights for birth cohort analyses
rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R

library(SASxport)
library(dplyr)
# library(survey)
library(haven)
# library(forcats)
library(questionr)
library(weights)
# library(pscl)
# library(descr)
library(pollster)

source("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/R/nhis2009_16 for weights age 80up.R")
nhis2009_16$birth_cohort=nhis2009_16$srvy_yr-nhis2009_16$age_p

NHANES0916<-read_sas("C:/Users/landyrm/Desktop/microsim/Data/Final/jan2020c4.sas7bdat")
NHANES0916<-filter(NHANES0916, RIAGENDR==1)

table(NHANES0916$RIDAGEYR)
table(NHANES0916$age)



NHANES0916$age<-NHANES0916$RIDAGEYR
# assign birth cohort as second year of survey cycle minus age
NHANES0916$birth_cohort<-ifelse(NHANES0916$SDDSRVYR==6, 2010-NHANES0916$age,
                                ifelse(NHANES0916$SDDSRVYR==7, 2012-NHANES0916$age,
                                       ifelse(NHANES0916$SDDSRVYR==8, 2014-NHANES0916$age,
                                              ifelse(NHANES0916$SDDSRVYR==9, 2016-NHANES0916$age,NA))))


#09-16: current smoking: SMQ040==1 or 2. not current smoker==3. weight for this variable is int weight
# SMQ020 - ever smoked 100 cigarettes : 2 = no
NHANES0916$currentsmoking=ifelse(NHANES0916$smoking==3,1,
                                 ifelse(NHANES0916$smoking>=1 & NHANES0916$smoking<=2,0,NA))

# race: 
# * ridreth1=1: mexican american
# * ridreth1=2: other hispanic
# * ridreth1=3: non-hispanic white
# * ridreth1=4: non-hispanic black
# * ridreth1=5: other including multiracial;

NHANES0916$racecat<-as.factor(ifelse(NHANES0916$RIDRETH1>=1 & NHANES0916$RIDRETH1<=2, 1,
                                     ifelse(NHANES0916$RIDRETH1==3,2,
                                            ifelse(NHANES0916$RIDRETH1==4, 3, 4))))

# racecat=1: hispanic
# racecat=2: white
# racecat=3: black
# racecat=4: other;

NHANES0916$race.smoking<-ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0,1,
                                ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0,2,
                                       ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0,3,
                                              ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0,4,
                                                     ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1,5,
                                                            ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1,6,
                                                                   ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1,7,
                                                                          ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1,8,NA))))))))


#for ages <18 use MEC weights, for ages >69 use MEC weights
NHANES0916$WTMEC8YR<-NHANES0916$WTMEC2YR/4

NHANES0916$weights<-ifelse(NHANES0916$RIDAGEYR<18, NHANES0916$WTMEC8YR,
                           ifelse(NHANES0916$RIDAGEYR<70, NHANES0916$behavweight8yr, NHANES0916$WTMEC8YR))


# for youngest birth cohorts (1997 onwards), need to assign race-specific smoking from older ages - use birth cohorts 1992-1996, and take race-specific proportions
NHANES0916_bc9296<-as.data.frame(filter(NHANES0916, birth_cohort>=1992 & birth_cohort<=1996 & weights>0 & is.na(weights)==F))
temp<-crosstab(df=NHANES0916_bc9296, x=racecat, y=currentsmoking, weight=weights, pct_type = "row", n=F)
racecat1.smoking.bc9296<-temp[1,3]/100
racecat2.smoking.bc9296<-temp[2,3]/100
racecat3.smoking.bc9296<-temp[3,3]/100
racecat4.smoking.bc9296<-temp[4,3]/100

for (y in 1997:2006) {
  x<-wtd.table(NHANES0916$racecat[NHANES0916$birth_cohort==y], weights=NHANES0916$weights[NHANES0916$birth_cohort==y])
  assign(paste0("racecat1.bc",y),as.numeric(x[1])/sum(x))
  assign(paste0("racecat2.bc",y),as.numeric(x[2])/sum(x))
  assign(paste0("racecat3.bc",y),as.numeric(x[3])/sum(x))
  assign(paste0("racecat4.bc",y),as.numeric(x[4])/sum(x))
}

# create birth-cohort-specific lifetimeoral quartiles
NHANES0916$birthcohort_cat<-ifelse(NHANES0916$birth_cohort>=1947 & NHANES0916$birth_cohort<1952,1,
                                   ifelse(NHANES0916$birth_cohort>=1952 & NHANES0916$birth_cohort<1957,2,
                                          ifelse(NHANES0916$birth_cohort>=1957 & NHANES0916$birth_cohort<1962,3,
                                                 ifelse(NHANES0916$birth_cohort>=1962 & NHANES0916$birth_cohort<1967,4,
                                                        ifelse(NHANES0916$birth_cohort>=1967 & NHANES0916$birth_cohort<1972,5,
                                                               ifelse(NHANES0916$birth_cohort>=1972 & NHANES0916$birth_cohort<1977,6,
                                                                      ifelse(NHANES0916$birth_cohort>=1977 & NHANES0916$birth_cohort<1982,7,
                                                                             ifelse(NHANES0916$birth_cohort>=1982 & NHANES0916$birth_cohort<1987,8,
                                                                                    ifelse(NHANES0916$birth_cohort>=1987 & NHANES0916$birth_cohort<1992,9,
                                                                                           ifelse(NHANES0916$birth_cohort>=1992 & NHANES0916$birth_cohort<1997,10,
                                                                                                  ifelse(NHANES0916$birth_cohort>=1997 & NHANES0916$birth_cohort<2002,11,
                                                                                                         ifelse(NHANES0916$birth_cohort>=2002 & NHANES0916$birth_cohort<2007,12,NA))))))))))))

table(NHANES0916$birthcohort_cat)

# birth cohorts 1947-2006
for (x in 1:11) {
  assign(paste0("quart.oral.bc",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeoral[NHANES0916$birthcohort_cat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$birthcohort_cat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
}

for (x in 1:11) {
  assign(paste0("quart.any.bc",x),eval(parse(text =paste0("wtd.quantile(NHANES0916$lifetimeany2[NHANES0916$birthcohort_cat==",x,"], weights=NHANES0916$behavweight8yr[NHANES0916$birthcohort_cat==",x,"], probs=c(0.25,0.5,0.75), na.rm = TRUE)"))))
}

#birthcohort_cat==12 is too young so has no data - apply proportions from birthcohort_cat==11

NHANES0916$quartile.lifetimeoral<-rep(NA,dim(NHANES0916)[1])
for (x in 1:11) {
  temp<-eval(parse(text =paste0("ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeoral<=quart.oral.bc",x,"[1],1,
       ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeoral<=quart.oral.bc",x,"[2],2,
              ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeoral<=quart.oral.bc",x,"[3],3,
                     ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeoral>quart.oral.bc",x,"[3] & is.na(NHANES0916$lifetimeoral)==FALSE,4,NA))))")))
  NHANES0916$quartile.lifetimeoral<-ifelse(is.na(temp)==TRUE,NHANES0916$quartile.lifetimeoral,temp)
}

NHANES0916$quartile.lifetimeoral<-as.factor(NHANES0916$quartile.lifetimeoral)

NHANES0916$quartile.lifetimeany<-rep(NA,dim(NHANES0916)[1])
for (x in 1:11) {
  temp<-eval(parse(text =paste0("ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeany2<=quart.any.bc",x,"[1],1,
       ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeany2<=quart.any.bc",x,"[2],2,
              ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeany2<=quart.any.bc",x,"[3],3,
                     ifelse(NHANES0916$birthcohort_cat==",x," & NHANES0916$lifetimeany2>quart.any.bc",x,"[3] & is.na(NHANES0916$lifetimeany2)==FALSE,4,NA))))")))
  NHANES0916$quartile.lifetimeany<-ifelse(is.na(temp)==TRUE,NHANES0916$quartile.lifetimeany,temp)
}

NHANES0916$quartile.lifetimeany<-as.factor(NHANES0916$quartile.lifetimeany)

#use lifetime oral quartile if recorded, otherwise lifetimeany
NHANES0916$quartile.lifetime<-ifelse(is.na(NHANES0916$quartile.lifetimeoral)==FALSE,NHANES0916$quartile.lifetimeoral,NHANES0916$quartile.lifetimeany)

table(NHANES0916$quartile.lifetime, NHANES0916$birth_cohort,useNA="always")

x<-crosstab(NHANES0916,quartile.lifetime, birth_cohort,weight=weights, n=F,pct_type = "column")


#for birth cohorts 1994-2006, use data from birth cohorts 1987-1991
NHANES0916_bc8791<-filter(NHANES0916, birth_cohort>=1987 & birth_cohort<=1991)
temp<-crosstab(df=NHANES0916_bc8791, x=race.smoking, y=quartile.lifetime, weight=weights, pct_type = "row", n=F)

q1.race1.smok0.8791<-temp[1,2]/100
q2.race1.smok0.8791<-temp[1,3]/100
q3.race1.smok0.8791<-temp[1,4]/100
q4.race1.smok0.8791<-temp[1,5]/100
q1.race2.smok0.8791<-temp[2,2]/100
q2.race2.smok0.8791<-temp[2,3]/100
q3.race2.smok0.8791<-temp[2,4]/100
q4.race2.smok0.8791<-temp[2,5]/100
q1.race3.smok0.8791<-temp[3,2]/100
q2.race3.smok0.8791<-temp[3,3]/100
q3.race3.smok0.8791<-temp[3,4]/100
q4.race3.smok0.8791<-temp[3,5]/100
q1.race4.smok0.8791<-temp[4,2]/100
q2.race4.smok0.8791<-temp[4,3]/100
q3.race4.smok0.8791<-temp[4,4]/100
q4.race4.smok0.8791<-temp[4,5]/100

q1.race1.smok1.8791<-temp[5,2]/100
q2.race1.smok1.8791<-temp[5,3]/100
q3.race1.smok1.8791<-temp[5,4]/100
q4.race1.smok1.8791<-temp[5,5]/100
q1.race2.smok1.8791<-temp[6,2]/100
q2.race2.smok1.8791<-temp[6,3]/100
q3.race2.smok1.8791<-temp[6,4]/100
q4.race2.smok1.8791<-temp[6,5]/100
q1.race3.smok1.8791<-temp[7,2]/100
q2.race3.smok1.8791<-temp[7,3]/100
q3.race3.smok1.8791<-temp[7,4]/100
q4.race3.smok1.8791<-temp[7,5]/100
q1.race4.smok1.8791<-temp[8,2]/100
q2.race4.smok1.8791<-temp[8,3]/100
q3.race4.smok1.8791<-temp[8,4]/100
q4.race4.smok1.8791<-temp[8,5]/100

#for birth cohorts 1937-1946, use data from birth cohorts 1947-1951
NHANES0916_bc4751<-filter(NHANES0916, birth_cohort>=1947 & birth_cohort<=1951)
temp<-crosstab(df=NHANES0916_bc4751, x=race.smoking, y=quartile.lifetime, weight=weights, pct_type = "row", n=F)

q1.race1.smok0.4751<-temp[1,2]/100
q2.race1.smok0.4751<-temp[1,3]/100
q3.race1.smok0.4751<-temp[1,4]/100
q4.race1.smok0.4751<-temp[1,5]/100
q1.race2.smok0.4751<-temp[2,2]/100
q2.race2.smok0.4751<-temp[2,3]/100
q3.race2.smok0.4751<-temp[2,4]/100
q4.race2.smok0.4751<-temp[2,5]/100
q1.race3.smok0.4751<-temp[3,2]/100
q2.race3.smok0.4751<-temp[3,3]/100
q3.race3.smok0.4751<-temp[3,4]/100
q4.race3.smok0.4751<-temp[3,5]/100
q1.race4.smok0.4751<-temp[4,2]/100
q2.race4.smok0.4751<-temp[4,3]/100
q3.race4.smok0.4751<-temp[4,4]/100
q4.race4.smok0.4751<-temp[4,5]/100

q1.race1.smok1.4751<-temp[5,2]/100
q2.race1.smok1.4751<-temp[5,3]/100
q3.race1.smok1.4751<-temp[5,4]/100
q4.race1.smok1.4751<-temp[5,5]/100
q1.race2.smok1.4751<-temp[6,2]/100
q2.race2.smok1.4751<-temp[6,3]/100
q3.race2.smok1.4751<-temp[6,4]/100
q4.race2.smok1.4751<-temp[6,5]/100
q1.race3.smok1.4751<-temp[7,2]/100
q2.race3.smok1.4751<-temp[7,3]/100
q3.race3.smok1.4751<-temp[7,4]/100
q4.race3.smok1.4751<-temp[7,5]/100
q1.race4.smok1.4751<-temp[8,2]/100
q2.race4.smok1.4751<-temp[8,3]/100
q3.race4.smok1.4751<-temp[8,4]/100
q4.race4.smok1.4751<-temp[8,5]/100


NHANES0916$race.smoking.quartile<-ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,1,
                                         ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,2,
                                                ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,3,
                                                       ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,4,
                                                              ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,5,
                                                                     ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,6,
                                                                            ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,7,
                                                                                   ifelse(NHANES0916$racecat==1 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,8,
                                                                                          
                                                                                          ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,9,
                                                                                                 ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,10,
                                                                                                        ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,11,
                                                                                                               ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,12,
                                                                                                                      ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,13,
                                                                                                                             ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,14,
                                                                                                                                    ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,15,
                                                                                                                                           ifelse(NHANES0916$racecat==2 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,16,       
                                                                                                                                                  
                                                                                                                                                  ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,17,
                                                                                                                                                         ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,18,
                                                                                                                                                                ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,19,
                                                                                                                                                                       ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,20,
                                                                                                                                                                              ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,21,
                                                                                                                                                                                     ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,22,
                                                                                                                                                                                            ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,23,
                                                                                                                                                                                                   ifelse(NHANES0916$racecat==3 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,24,                                           
                                                                                                                                                                                                          
                                                                                                                                                                                                          ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==1,25,
                                                                                                                                                                                                                 ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==2,26,
                                                                                                                                                                                                                        ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==3,27,
                                                                                                                                                                                                                               ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==0 & NHANES0916$quartile.lifetime==4,28,
                                                                                                                                                                                                                                      ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==1,29,
                                                                                                                                                                                                                                             ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==2,30,
                                                                                                                                                                                                                                                    ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==3,31,
                                                                                                                                                                                                                                                           ifelse(NHANES0916$racecat==4 & NHANES0916$currentsmoking==1 & NHANES0916$quartile.lifetime==4,32,NA))))))))))))))))))))))))))))))))
library(data.table)
RESULTS<-as.data.table(crosstab(NHANES0916, birth_cohort,race.smoking.quartile, weights, pct_type = "row", n=F))
# up to 1992, use results from temp

#each element contains 4 values, for q1 to q4
x1993<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1993],weights=NHANES0916$weights[NHANES0916$birth_cohort==1993])
N1993.race1.smoke0<-as.numeric(c((as.numeric(x1993[1])/sum(x1993))*q1.race1.smok0.8791,(as.numeric(x1993[1])/sum(x1993))*q2.race1.smok0.8791,(as.numeric(x1993[1])/sum(x1993))*q3.race1.smok0.8791,(as.numeric(x1993[1])/sum(x1993))*q4.race1.smok0.8791))
N1993.race2.smoke0<-as.numeric(c((as.numeric(x1993[2])/sum(x1993))*q1.race2.smok0.8791,(as.numeric(x1993[2])/sum(x1993))*q2.race2.smok0.8791,(as.numeric(x1993[2])/sum(x1993))*q3.race2.smok0.8791,(as.numeric(x1993[2])/sum(x1993))*q4.race2.smok0.8791))
N1993.race3.smoke0<-as.numeric(c((as.numeric(x1993[3])/sum(x1993))*q1.race3.smok0.8791,(as.numeric(x1993[3])/sum(x1993))*q2.race3.smok0.8791,(as.numeric(x1993[3])/sum(x1993))*q3.race3.smok0.8791,(as.numeric(x1993[3])/sum(x1993))*q4.race3.smok0.8791))
N1993.race4.smoke0<-as.numeric(c((as.numeric(x1993[4])/sum(x1993))*q1.race4.smok0.8791,(as.numeric(x1993[4])/sum(x1993))*q2.race4.smok0.8791,(as.numeric(x1993[4])/sum(x1993))*q3.race4.smok0.8791,(as.numeric(x1993[4])/sum(x1993))*q4.race4.smok0.8791))
N1993.race1.smoke1<-as.numeric(c((as.numeric(x1993[5])/sum(x1993))*q1.race1.smok1.8791,(as.numeric(x1993[5])/sum(x1993))*q2.race1.smok1.8791,(as.numeric(x1993[5])/sum(x1993))*q3.race1.smok1.8791,(as.numeric(x1993[5])/sum(x1993))*q4.race1.smok1.8791))
N1993.race2.smoke1<-as.numeric(c((as.numeric(x1993[6])/sum(x1993))*q1.race2.smok1.8791,(as.numeric(x1993[6])/sum(x1993))*q2.race2.smok1.8791,(as.numeric(x1993[6])/sum(x1993))*q3.race2.smok1.8791,(as.numeric(x1993[6])/sum(x1993))*q4.race2.smok1.8791))
N1993.race3.smoke1<-as.numeric(c((as.numeric(x1993[7])/sum(x1993))*q1.race3.smok1.8791,(as.numeric(x1993[7])/sum(x1993))*q2.race3.smok1.8791,(as.numeric(x1993[7])/sum(x1993))*q3.race3.smok1.8791,(as.numeric(x1993[7])/sum(x1993))*q4.race3.smok1.8791))
N1993.race4.smoke1<-as.numeric(c((as.numeric(x1993[8])/sum(x1993))*q1.race4.smok1.8791,(as.numeric(x1993[8])/sum(x1993))*q2.race4.smok1.8791,(as.numeric(x1993[8])/sum(x1993))*q3.race4.smok1.8791,(as.numeric(x1993[8])/sum(x1993))*q4.race4.smok1.8791))

x1994<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1994],weights=NHANES0916$weights[NHANES0916$birth_cohort==1994])
N1994.race1.smoke0<-as.numeric(c((as.numeric(x1994[1])/sum(x1994))*q1.race1.smok0.8791,(as.numeric(x1994[1])/sum(x1994))*q2.race1.smok0.8791,(as.numeric(x1994[1])/sum(x1994))*q3.race1.smok0.8791,(as.numeric(x1994[1])/sum(x1994))*q4.race1.smok0.8791))
N1994.race2.smoke0<-as.numeric(c((as.numeric(x1994[2])/sum(x1994))*q1.race2.smok0.8791,(as.numeric(x1994[2])/sum(x1994))*q2.race2.smok0.8791,(as.numeric(x1994[2])/sum(x1994))*q3.race2.smok0.8791,(as.numeric(x1994[2])/sum(x1994))*q4.race2.smok0.8791))
N1994.race3.smoke0<-as.numeric(c((as.numeric(x1994[3])/sum(x1994))*q1.race3.smok0.8791,(as.numeric(x1994[3])/sum(x1994))*q2.race3.smok0.8791,(as.numeric(x1994[3])/sum(x1994))*q3.race3.smok0.8791,(as.numeric(x1994[3])/sum(x1994))*q4.race3.smok0.8791))
N1994.race4.smoke0<-as.numeric(c((as.numeric(x1994[4])/sum(x1994))*q1.race4.smok0.8791,(as.numeric(x1994[4])/sum(x1994))*q2.race4.smok0.8791,(as.numeric(x1994[4])/sum(x1994))*q3.race4.smok0.8791,(as.numeric(x1994[4])/sum(x1994))*q4.race4.smok0.8791))
N1994.race1.smoke1<-as.numeric(c((as.numeric(x1994[5])/sum(x1994))*q1.race1.smok1.8791,(as.numeric(x1994[5])/sum(x1994))*q2.race1.smok1.8791,(as.numeric(x1994[5])/sum(x1994))*q3.race1.smok1.8791,(as.numeric(x1994[5])/sum(x1994))*q4.race1.smok1.8791))
N1994.race2.smoke1<-as.numeric(c((as.numeric(x1994[6])/sum(x1994))*q1.race2.smok1.8791,(as.numeric(x1994[6])/sum(x1994))*q2.race2.smok1.8791,(as.numeric(x1994[6])/sum(x1994))*q3.race2.smok1.8791,(as.numeric(x1994[6])/sum(x1994))*q4.race2.smok1.8791))
N1994.race3.smoke1<-as.numeric(c((as.numeric(x1994[7])/sum(x1994))*q1.race3.smok1.8791,(as.numeric(x1994[7])/sum(x1994))*q2.race3.smok1.8791,(as.numeric(x1994[7])/sum(x1994))*q3.race3.smok1.8791,(as.numeric(x1994[7])/sum(x1994))*q4.race3.smok1.8791))
N1994.race4.smoke1<-as.numeric(c((as.numeric(x1994[8])/sum(x1994))*q1.race4.smok1.8791,(as.numeric(x1994[8])/sum(x1994))*q2.race4.smok1.8791,(as.numeric(x1994[8])/sum(x1994))*q3.race4.smok1.8791,(as.numeric(x1994[8])/sum(x1994))*q4.race4.smok1.8791))

x1995<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1995],weights=NHANES0916$weights[NHANES0916$birth_cohort==1995])
N1995.race1.smoke0<-as.numeric(c((as.numeric(x1995[1])/sum(x1995))*q1.race1.smok0.8791,(as.numeric(x1995[1])/sum(x1995))*q2.race1.smok0.8791,(as.numeric(x1995[1])/sum(x1995))*q3.race1.smok0.8791,(as.numeric(x1995[1])/sum(x1995))*q4.race1.smok0.8791))
N1995.race2.smoke0<-as.numeric(c((as.numeric(x1995[2])/sum(x1995))*q1.race2.smok0.8791,(as.numeric(x1995[2])/sum(x1995))*q2.race2.smok0.8791,(as.numeric(x1995[2])/sum(x1995))*q3.race2.smok0.8791,(as.numeric(x1995[2])/sum(x1995))*q4.race2.smok0.8791))
N1995.race3.smoke0<-as.numeric(c((as.numeric(x1995[3])/sum(x1995))*q1.race3.smok0.8791,(as.numeric(x1995[3])/sum(x1995))*q2.race3.smok0.8791,(as.numeric(x1995[3])/sum(x1995))*q3.race3.smok0.8791,(as.numeric(x1995[3])/sum(x1995))*q4.race3.smok0.8791))
N1995.race4.smoke0<-as.numeric(c((as.numeric(x1995[4])/sum(x1995))*q1.race4.smok0.8791,(as.numeric(x1995[4])/sum(x1995))*q2.race4.smok0.8791,(as.numeric(x1995[4])/sum(x1995))*q3.race4.smok0.8791,(as.numeric(x1995[4])/sum(x1995))*q4.race4.smok0.8791))
N1995.race1.smoke1<-as.numeric(c((as.numeric(x1995[5])/sum(x1995))*q1.race1.smok1.8791,(as.numeric(x1995[5])/sum(x1995))*q2.race1.smok1.8791,(as.numeric(x1995[5])/sum(x1995))*q3.race1.smok1.8791,(as.numeric(x1995[5])/sum(x1995))*q4.race1.smok1.8791))
N1995.race2.smoke1<-as.numeric(c((as.numeric(x1995[6])/sum(x1995))*q1.race2.smok1.8791,(as.numeric(x1995[6])/sum(x1995))*q2.race2.smok1.8791,(as.numeric(x1995[6])/sum(x1995))*q3.race2.smok1.8791,(as.numeric(x1995[6])/sum(x1995))*q4.race2.smok1.8791))
N1995.race3.smoke1<-as.numeric(c((as.numeric(x1995[7])/sum(x1995))*q1.race3.smok1.8791,(as.numeric(x1995[7])/sum(x1995))*q2.race3.smok1.8791,(as.numeric(x1995[7])/sum(x1995))*q3.race3.smok1.8791,(as.numeric(x1995[7])/sum(x1995))*q4.race3.smok1.8791))
N1995.race4.smoke1<-as.numeric(c((as.numeric(x1995[8])/sum(x1995))*q1.race4.smok1.8791,(as.numeric(x1995[8])/sum(x1995))*q2.race4.smok1.8791,(as.numeric(x1995[8])/sum(x1995))*q3.race4.smok1.8791,(as.numeric(x1995[8])/sum(x1995))*q4.race4.smok1.8791))

x1996<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1996],weights=NHANES0916$weights[NHANES0916$birth_cohort==1996])
N1996.race1.smoke0<-as.numeric(c((as.numeric(x1996[1])/sum(x1996))*q1.race1.smok0.8791,(as.numeric(x1996[1])/sum(x1996))*q2.race1.smok0.8791,(as.numeric(x1996[1])/sum(x1996))*q3.race1.smok0.8791,(as.numeric(x1996[1])/sum(x1996))*q4.race1.smok0.8791))
N1996.race2.smoke0<-as.numeric(c((as.numeric(x1996[2])/sum(x1996))*q1.race2.smok0.8791,(as.numeric(x1996[2])/sum(x1996))*q2.race2.smok0.8791,(as.numeric(x1996[2])/sum(x1996))*q3.race2.smok0.8791,(as.numeric(x1996[2])/sum(x1996))*q4.race2.smok0.8791))
N1996.race3.smoke0<-as.numeric(c((as.numeric(x1996[3])/sum(x1996))*q1.race3.smok0.8791,(as.numeric(x1996[3])/sum(x1996))*q2.race3.smok0.8791,(as.numeric(x1996[3])/sum(x1996))*q3.race3.smok0.8791,(as.numeric(x1996[3])/sum(x1996))*q4.race3.smok0.8791))
N1996.race4.smoke0<-as.numeric(c((as.numeric(x1996[4])/sum(x1996))*q1.race4.smok0.8791,(as.numeric(x1996[4])/sum(x1996))*q2.race4.smok0.8791,(as.numeric(x1996[4])/sum(x1996))*q3.race4.smok0.8791,(as.numeric(x1996[4])/sum(x1996))*q4.race4.smok0.8791))
N1996.race1.smoke1<-as.numeric(c((as.numeric(x1996[5])/sum(x1996))*q1.race1.smok1.8791,(as.numeric(x1996[5])/sum(x1996))*q2.race1.smok1.8791,(as.numeric(x1996[5])/sum(x1996))*q3.race1.smok1.8791,(as.numeric(x1996[5])/sum(x1996))*q4.race1.smok1.8791))
N1996.race2.smoke1<-as.numeric(c((as.numeric(x1996[6])/sum(x1996))*q1.race2.smok1.8791,(as.numeric(x1996[6])/sum(x1996))*q2.race2.smok1.8791,(as.numeric(x1996[6])/sum(x1996))*q3.race2.smok1.8791,(as.numeric(x1996[6])/sum(x1996))*q4.race2.smok1.8791))
N1996.race3.smoke1<-as.numeric(c((as.numeric(x1996[7])/sum(x1996))*q1.race3.smok1.8791,(as.numeric(x1996[7])/sum(x1996))*q2.race3.smok1.8791,(as.numeric(x1996[7])/sum(x1996))*q3.race3.smok1.8791,(as.numeric(x1996[7])/sum(x1996))*q4.race3.smok1.8791))
N1996.race4.smoke1<-as.numeric(c((as.numeric(x1996[8])/sum(x1996))*q1.race4.smok1.8791,(as.numeric(x1996[8])/sum(x1996))*q2.race4.smok1.8791,(as.numeric(x1996[8])/sum(x1996))*q3.race4.smok1.8791,(as.numeric(x1996[8])/sum(x1996))*q4.race4.smok1.8791))

# for cohorts 1997-2006, first need to assign smoking proportions to each race, then quartile
N1997.race1.smoke0<-c(racecat1.bc1997*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc1997*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc1997*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc1997*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N1997.race2.smoke0<-c(racecat2.bc1997*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc1997*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc1997*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc1997*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N1997.race3.smoke0<-c(racecat3.bc1997*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc1997*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc1997*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc1997*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N1997.race4.smoke0<-c(racecat4.bc1997*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc1997*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc1997*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc1997*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N1997.race1.smoke1<-c(racecat1.bc1997*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc1997*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc1997*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc1997*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N1997.race2.smoke1<-c(racecat2.bc1997*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc1997*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc1997*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc1997*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N1997.race3.smoke1<-c(racecat3.bc1997*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc1997*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc1997*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc1997*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N1997.race4.smoke1<-c(racecat4.bc1997*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc1997*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc1997*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc1997*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N1998.race1.smoke0<-c(racecat1.bc1998*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc1998*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc1998*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc1998*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N1998.race2.smoke0<-c(racecat2.bc1998*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc1998*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc1998*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc1998*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N1998.race3.smoke0<-c(racecat3.bc1998*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc1998*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc1998*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc1998*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N1998.race4.smoke0<-c(racecat4.bc1998*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc1998*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc1998*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc1998*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N1998.race1.smoke1<-c(racecat1.bc1998*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc1998*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc1998*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc1998*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N1998.race2.smoke1<-c(racecat2.bc1998*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc1998*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc1998*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc1998*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N1998.race3.smoke1<-c(racecat3.bc1998*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc1998*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc1998*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc1998*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N1998.race4.smoke1<-c(racecat4.bc1998*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc1998*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc1998*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc1998*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N1999.race1.smoke0<-c(racecat1.bc1999*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc1999*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc1999*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc1999*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N1999.race2.smoke0<-c(racecat2.bc1999*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc1999*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc1999*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc1999*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N1999.race3.smoke0<-c(racecat3.bc1999*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc1999*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc1999*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc1999*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N1999.race4.smoke0<-c(racecat4.bc1999*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc1999*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc1999*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc1999*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N1999.race1.smoke1<-c(racecat1.bc1999*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc1999*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc1999*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc1999*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N1999.race2.smoke1<-c(racecat2.bc1999*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc1999*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc1999*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc1999*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N1999.race3.smoke1<-c(racecat3.bc1999*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc1999*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc1999*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc1999*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N1999.race4.smoke1<-c(racecat4.bc1999*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc1999*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc1999*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc1999*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2000.race1.smoke0<-c(racecat1.bc2000*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2000*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2000*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2000*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2000.race2.smoke0<-c(racecat2.bc2000*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2000*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2000*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2000*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2000.race3.smoke0<-c(racecat3.bc2000*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2000*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2000*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2000*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2000.race4.smoke0<-c(racecat4.bc2000*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2000*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2000*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2000*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2000.race1.smoke1<-c(racecat1.bc2000*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2000*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2000*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2000*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2000.race2.smoke1<-c(racecat2.bc2000*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2000*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2000*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2000*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2000.race3.smoke1<-c(racecat3.bc2000*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2000*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2000*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2000*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2000.race4.smoke1<-c(racecat4.bc2000*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2000*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2000*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2000*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2001.race1.smoke0<-c(racecat1.bc2001*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2001*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2001*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2001*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2001.race2.smoke0<-c(racecat2.bc2001*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2001*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2001*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2001*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2001.race3.smoke0<-c(racecat3.bc2001*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2001*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2001*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2001*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2001.race4.smoke0<-c(racecat4.bc2001*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2001*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2001*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2001*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2001.race1.smoke1<-c(racecat1.bc2001*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2001*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2001*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2001*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2001.race2.smoke1<-c(racecat2.bc2001*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2001*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2001*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2001*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2001.race3.smoke1<-c(racecat3.bc2001*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2001*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2001*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2001*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2001.race4.smoke1<-c(racecat4.bc2001*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2001*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2001*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2001*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2002.race1.smoke0<-c(racecat1.bc2002*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2002*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2002*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2002*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2002.race2.smoke0<-c(racecat2.bc2002*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2002*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2002*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2002*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2002.race3.smoke0<-c(racecat3.bc2002*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2002*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2002*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2002*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2002.race4.smoke0<-c(racecat4.bc2002*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2002*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2002*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2002*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2002.race1.smoke1<-c(racecat1.bc2002*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2002*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2002*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2002*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2002.race2.smoke1<-c(racecat2.bc2002*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2002*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2002*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2002*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2002.race3.smoke1<-c(racecat3.bc2002*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2002*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2002*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2002*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2002.race4.smoke1<-c(racecat4.bc2002*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2002*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2002*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2002*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2003.race1.smoke0<-c(racecat1.bc2003*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2003*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2003*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2003*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2003.race2.smoke0<-c(racecat2.bc2003*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2003*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2003*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2003*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2003.race3.smoke0<-c(racecat3.bc2003*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2003*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2003*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2003*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2003.race4.smoke0<-c(racecat4.bc2003*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2003*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2003*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2003*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2003.race1.smoke1<-c(racecat1.bc2003*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2003*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2003*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2003*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2003.race2.smoke1<-c(racecat2.bc2003*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2003*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2003*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2003*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2003.race3.smoke1<-c(racecat3.bc2003*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2003*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2003*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2003*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2003.race4.smoke1<-c(racecat4.bc2003*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2003*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2003*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2003*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2004.race1.smoke0<-c(racecat1.bc2004*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2004*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2004*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2004*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2004.race2.smoke0<-c(racecat2.bc2004*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2004*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2004*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2004*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2004.race3.smoke0<-c(racecat3.bc2004*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2004*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2004*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2004*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2004.race4.smoke0<-c(racecat4.bc2004*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2004*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2004*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2004*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2004.race1.smoke1<-c(racecat1.bc2004*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2004*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2004*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2004*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2004.race2.smoke1<-c(racecat2.bc2004*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2004*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2004*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2004*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2004.race3.smoke1<-c(racecat3.bc2004*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2004*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2004*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2004*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2004.race4.smoke1<-c(racecat4.bc2004*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2004*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2004*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2004*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2005.race1.smoke0<-c(racecat1.bc2005*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2005*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2005*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2005*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2005.race2.smoke0<-c(racecat2.bc2005*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2005*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2005*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2005*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2005.race3.smoke0<-c(racecat3.bc2005*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2005*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2005*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2005*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2005.race4.smoke0<-c(racecat4.bc2005*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2005*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2005*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2005*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2005.race1.smoke1<-c(racecat1.bc2005*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2005*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2005*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2005*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2005.race2.smoke1<-c(racecat2.bc2005*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2005*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2005*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2005*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2005.race3.smoke1<-c(racecat3.bc2005*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2005*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2005*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2005*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2005.race4.smoke1<-c(racecat4.bc2005*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2005*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2005*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2005*racecat4.smoking.bc9296*q4.race4.smok1.8791)

N2006.race1.smoke0<-c(racecat1.bc2006*(1-racecat1.smoking.bc9296)*q1.race1.smok0.8791,racecat1.bc2006*(1-racecat1.smoking.bc9296)*q2.race1.smok0.8791,
                      racecat1.bc2006*(1-racecat1.smoking.bc9296)*q3.race1.smok0.8791,racecat1.bc2006*(1-racecat1.smoking.bc9296)*q4.race1.smok0.8791)
N2006.race2.smoke0<-c(racecat2.bc2006*(1-racecat2.smoking.bc9296)*q1.race2.smok0.8791,racecat2.bc2006*(1-racecat2.smoking.bc9296)*q2.race2.smok0.8791,
                      racecat2.bc2006*(1-racecat2.smoking.bc9296)*q3.race2.smok0.8791,racecat2.bc2006*(1-racecat2.smoking.bc9296)*q4.race2.smok0.8791)
N2006.race3.smoke0<-c(racecat3.bc2006*(1-racecat3.smoking.bc9296)*q1.race3.smok0.8791,racecat3.bc2006*(1-racecat3.smoking.bc9296)*q2.race3.smok0.8791,
                      racecat3.bc2006*(1-racecat3.smoking.bc9296)*q3.race3.smok0.8791,racecat3.bc2006*(1-racecat3.smoking.bc9296)*q4.race3.smok0.8791)
N2006.race4.smoke0<-c(racecat4.bc2006*(1-racecat4.smoking.bc9296)*q1.race4.smok0.8791,racecat4.bc2006*(1-racecat4.smoking.bc9296)*q2.race4.smok0.8791,
                      racecat4.bc2006*(1-racecat4.smoking.bc9296)*q3.race4.smok0.8791,racecat4.bc2006*(1-racecat4.smoking.bc9296)*q4.race4.smok0.8791)
N2006.race1.smoke1<-c(racecat1.bc2006*racecat1.smoking.bc9296*q1.race1.smok1.8791,racecat1.bc2006*racecat1.smoking.bc9296*q2.race1.smok1.8791,
                      racecat1.bc2006*racecat1.smoking.bc9296*q3.race1.smok1.8791,racecat1.bc2006*racecat1.smoking.bc9296*q4.race1.smok1.8791)
N2006.race2.smoke1<-c(racecat2.bc2006*racecat2.smoking.bc9296*q1.race2.smok1.8791,racecat2.bc2006*racecat2.smoking.bc9296*q2.race2.smok1.8791,
                      racecat2.bc2006*racecat2.smoking.bc9296*q3.race2.smok1.8791,racecat2.bc2006*racecat2.smoking.bc9296*q4.race2.smok1.8791)
N2006.race3.smoke1<-c(racecat3.bc2006*racecat3.smoking.bc9296*q1.race3.smok1.8791,racecat3.bc2006*racecat3.smoking.bc9296*q2.race3.smok1.8791,
                      racecat3.bc2006*racecat3.smoking.bc9296*q3.race3.smok1.8791,racecat3.bc2006*racecat3.smoking.bc9296*q4.race3.smok1.8791)
N2006.race4.smoke1<-c(racecat4.bc2006*racecat4.smoking.bc9296*q1.race4.smok1.8791,racecat4.bc2006*racecat4.smoking.bc9296*q2.race4.smok1.8791,
                      racecat4.bc2006*racecat4.smoking.bc9296*q3.race4.smok1.8791,racecat4.bc2006*racecat4.smoking.bc9296*q4.race4.smok1.8791)

#####
results.1993<-(c(N1993.race1.smoke0,N1993.race1.smoke1,N1993.race2.smoke0,N1993.race2.smoke1,N1993.race3.smoke0,N1993.race3.smoke1,N1993.race4.smoke0,N1993.race4.smoke1)) 
results.1994<-(c(N1994.race1.smoke0,N1994.race1.smoke1,N1994.race2.smoke0,N1994.race2.smoke1,N1994.race3.smoke0,N1994.race3.smoke1,N1994.race4.smoke0,N1994.race4.smoke1)) 
results.1995<-(c(N1995.race1.smoke0,N1995.race1.smoke1,N1995.race2.smoke0,N1995.race2.smoke1,N1995.race3.smoke0,N1995.race3.smoke1,N1995.race4.smoke0,N1995.race4.smoke1)) 
results.1996<-(c(N1996.race1.smoke0,N1996.race1.smoke1,N1996.race2.smoke0,N1996.race2.smoke1,N1996.race3.smoke0,N1996.race3.smoke1,N1996.race4.smoke0,N1996.race4.smoke1)) 
results.1997<-as.numeric(c(N1997.race1.smoke0,N1997.race1.smoke1,N1997.race2.smoke0,N1997.race2.smoke1,N1997.race3.smoke0,N1997.race3.smoke1,N1997.race4.smoke0,N1997.race4.smoke1)) 
results.1998<-as.numeric(c(N1998.race1.smoke0,N1998.race1.smoke1,N1998.race2.smoke0,N1998.race2.smoke1,N1998.race3.smoke0,N1998.race3.smoke1,N1998.race4.smoke0,N1998.race4.smoke1)) 
results.1999<-as.numeric(c(N1999.race1.smoke0,N1999.race1.smoke1,N1999.race2.smoke0,N1999.race2.smoke1,N1999.race3.smoke0,N1999.race3.smoke1,N1999.race4.smoke0,N1999.race4.smoke1)) 
results.2000<-as.numeric(c(N2000.race1.smoke0,N2000.race1.smoke1,N2000.race2.smoke0,N2000.race2.smoke1,N2000.race3.smoke0,N2000.race3.smoke1,N2000.race4.smoke0,N2000.race4.smoke1)) 
results.2001<-as.numeric(c(N2001.race1.smoke0,N2001.race1.smoke1,N2001.race2.smoke0,N2001.race2.smoke1,N2001.race3.smoke0,N2001.race3.smoke1,N2001.race4.smoke0,N2001.race4.smoke1)) 
results.2002<-as.numeric(c(N2002.race1.smoke0,N2002.race1.smoke1,N2002.race2.smoke0,N2002.race2.smoke1,N2002.race3.smoke0,N2002.race3.smoke1,N2002.race4.smoke0,N2002.race4.smoke1)) 
results.2003<-as.numeric(c(N2003.race1.smoke0,N2003.race1.smoke1,N2003.race2.smoke0,N2003.race2.smoke1,N2003.race3.smoke0,N2003.race3.smoke1,N2003.race4.smoke0,N2003.race4.smoke1)) 
results.2004<-as.numeric(c(N2004.race1.smoke0,N2004.race1.smoke1,N2004.race2.smoke0,N2004.race2.smoke1,N2004.race3.smoke0,N2004.race3.smoke1,N2004.race4.smoke0,N2004.race4.smoke1)) 
results.2005<-as.numeric(c(N2005.race1.smoke0,N2005.race1.smoke1,N2005.race2.smoke0,N2005.race2.smoke1,N2005.race3.smoke0,N2005.race3.smoke1,N2005.race4.smoke0,N2005.race4.smoke1)) 
results.2006<-as.numeric(c(N2006.race1.smoke0,N2006.race1.smoke1,N2006.race2.smoke0,N2006.race2.smoke1,N2006.race3.smoke0,N2006.race3.smoke1,N2006.race4.smoke0,N2006.race4.smoke1)) 

### need to add birth cohorts 1937-1946
x1946<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1946],weights=NHANES0916$weights[NHANES0916$birth_cohort==1946])
N1946.race1.smoke0<-as.numeric(c((as.numeric(x1946[1])/sum(x1946))*q1.race1.smok0.4751,(as.numeric(x1946[1])/sum(x1946))*q2.race1.smok0.4751,(as.numeric(x1946[1])/sum(x1946))*q3.race1.smok0.4751,(as.numeric(x1946[1])/sum(x1946))*q4.race1.smok0.4751))
N1946.race2.smoke0<-as.numeric(c((as.numeric(x1946[2])/sum(x1946))*q1.race2.smok0.4751,(as.numeric(x1946[2])/sum(x1946))*q2.race2.smok0.4751,(as.numeric(x1946[2])/sum(x1946))*q3.race2.smok0.4751,(as.numeric(x1946[2])/sum(x1946))*q4.race2.smok0.4751))
N1946.race3.smoke0<-as.numeric(c((as.numeric(x1946[3])/sum(x1946))*q1.race3.smok0.4751,(as.numeric(x1946[3])/sum(x1946))*q2.race3.smok0.4751,(as.numeric(x1946[3])/sum(x1946))*q3.race3.smok0.4751,(as.numeric(x1946[3])/sum(x1946))*q4.race3.smok0.4751))
N1946.race4.smoke0<-as.numeric(c((as.numeric(x1946[4])/sum(x1946))*q1.race4.smok0.4751,(as.numeric(x1946[4])/sum(x1946))*q2.race4.smok0.4751,(as.numeric(x1946[4])/sum(x1946))*q3.race4.smok0.4751,(as.numeric(x1946[4])/sum(x1946))*q4.race4.smok0.4751))
N1946.race1.smoke1<-as.numeric(c((as.numeric(x1946[5])/sum(x1946))*q1.race1.smok1.4751,(as.numeric(x1946[5])/sum(x1946))*q2.race1.smok1.4751,(as.numeric(x1946[5])/sum(x1946))*q3.race1.smok1.4751,(as.numeric(x1946[5])/sum(x1946))*q4.race1.smok1.4751))
N1946.race2.smoke1<-as.numeric(c((as.numeric(x1946[6])/sum(x1946))*q1.race2.smok1.4751,(as.numeric(x1946[6])/sum(x1946))*q2.race2.smok1.4751,(as.numeric(x1946[6])/sum(x1946))*q3.race2.smok1.4751,(as.numeric(x1946[6])/sum(x1946))*q4.race2.smok1.4751))
N1946.race3.smoke1<-as.numeric(c((as.numeric(x1946[7])/sum(x1946))*q1.race3.smok1.4751,(as.numeric(x1946[7])/sum(x1946))*q2.race3.smok1.4751,(as.numeric(x1946[7])/sum(x1946))*q3.race3.smok1.4751,(as.numeric(x1946[7])/sum(x1946))*q4.race3.smok1.4751))
N1946.race4.smoke1<-as.numeric(c((as.numeric(x1946[8])/sum(x1946))*q1.race4.smok1.4751,(as.numeric(x1946[8])/sum(x1946))*q2.race4.smok1.4751,(as.numeric(x1946[8])/sum(x1946))*q3.race4.smok1.4751,(as.numeric(x1946[8])/sum(x1946))*q4.race4.smok1.4751))

x1945<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1945],weights=NHANES0916$weights[NHANES0916$birth_cohort==1945])
N1945.race1.smoke0<-as.numeric(c((as.numeric(x1945[1])/sum(x1945))*q1.race1.smok0.4751,(as.numeric(x1945[1])/sum(x1945))*q2.race1.smok0.4751,(as.numeric(x1945[1])/sum(x1945))*q3.race1.smok0.4751,(as.numeric(x1945[1])/sum(x1945))*q4.race1.smok0.4751))
N1945.race2.smoke0<-as.numeric(c((as.numeric(x1945[2])/sum(x1945))*q1.race2.smok0.4751,(as.numeric(x1945[2])/sum(x1945))*q2.race2.smok0.4751,(as.numeric(x1945[2])/sum(x1945))*q3.race2.smok0.4751,(as.numeric(x1945[2])/sum(x1945))*q4.race2.smok0.4751))
N1945.race3.smoke0<-as.numeric(c((as.numeric(x1945[3])/sum(x1945))*q1.race3.smok0.4751,(as.numeric(x1945[3])/sum(x1945))*q2.race3.smok0.4751,(as.numeric(x1945[3])/sum(x1945))*q3.race3.smok0.4751,(as.numeric(x1945[3])/sum(x1945))*q4.race3.smok0.4751))
N1945.race4.smoke0<-as.numeric(c((as.numeric(x1945[4])/sum(x1945))*q1.race4.smok0.4751,(as.numeric(x1945[4])/sum(x1945))*q2.race4.smok0.4751,(as.numeric(x1945[4])/sum(x1945))*q3.race4.smok0.4751,(as.numeric(x1945[4])/sum(x1945))*q4.race4.smok0.4751))
N1945.race1.smoke1<-as.numeric(c((as.numeric(x1945[5])/sum(x1945))*q1.race1.smok1.4751,(as.numeric(x1945[5])/sum(x1945))*q2.race1.smok1.4751,(as.numeric(x1945[5])/sum(x1945))*q3.race1.smok1.4751,(as.numeric(x1945[5])/sum(x1945))*q4.race1.smok1.4751))
N1945.race2.smoke1<-as.numeric(c((as.numeric(x1945[6])/sum(x1945))*q1.race2.smok1.4751,(as.numeric(x1945[6])/sum(x1945))*q2.race2.smok1.4751,(as.numeric(x1945[6])/sum(x1945))*q3.race2.smok1.4751,(as.numeric(x1945[6])/sum(x1945))*q4.race2.smok1.4751))
N1945.race3.smoke1<-as.numeric(c((as.numeric(x1945[7])/sum(x1945))*q1.race3.smok1.4751,(as.numeric(x1945[7])/sum(x1945))*q2.race3.smok1.4751,(as.numeric(x1945[7])/sum(x1945))*q3.race3.smok1.4751,(as.numeric(x1945[7])/sum(x1945))*q4.race3.smok1.4751))
N1945.race4.smoke1<-as.numeric(c((as.numeric(x1945[8])/sum(x1945))*q1.race4.smok1.4751,(as.numeric(x1945[8])/sum(x1945))*q2.race4.smok1.4751,(as.numeric(x1945[8])/sum(x1945))*q3.race4.smok1.4751,(as.numeric(x1945[8])/sum(x1945))*q4.race4.smok1.4751))

x1944<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1944],weights=NHANES0916$weights[NHANES0916$birth_cohort==1944])
N1944.race1.smoke0<-as.numeric(c((as.numeric(x1944[1])/sum(x1944))*q1.race1.smok0.4751,(as.numeric(x1944[1])/sum(x1944))*q2.race1.smok0.4751,(as.numeric(x1944[1])/sum(x1944))*q3.race1.smok0.4751,(as.numeric(x1944[1])/sum(x1944))*q4.race1.smok0.4751))
N1944.race2.smoke0<-as.numeric(c((as.numeric(x1944[2])/sum(x1944))*q1.race2.smok0.4751,(as.numeric(x1944[2])/sum(x1944))*q2.race2.smok0.4751,(as.numeric(x1944[2])/sum(x1944))*q3.race2.smok0.4751,(as.numeric(x1944[2])/sum(x1944))*q4.race2.smok0.4751))
N1944.race3.smoke0<-as.numeric(c((as.numeric(x1944[3])/sum(x1944))*q1.race3.smok0.4751,(as.numeric(x1944[3])/sum(x1944))*q2.race3.smok0.4751,(as.numeric(x1944[3])/sum(x1944))*q3.race3.smok0.4751,(as.numeric(x1944[3])/sum(x1944))*q4.race3.smok0.4751))
N1944.race4.smoke0<-as.numeric(c((as.numeric(x1944[4])/sum(x1944))*q1.race4.smok0.4751,(as.numeric(x1944[4])/sum(x1944))*q2.race4.smok0.4751,(as.numeric(x1944[4])/sum(x1944))*q3.race4.smok0.4751,(as.numeric(x1944[4])/sum(x1944))*q4.race4.smok0.4751))
N1944.race1.smoke1<-as.numeric(c((as.numeric(x1944[5])/sum(x1944))*q1.race1.smok1.4751,(as.numeric(x1944[5])/sum(x1944))*q2.race1.smok1.4751,(as.numeric(x1944[5])/sum(x1944))*q3.race1.smok1.4751,(as.numeric(x1944[5])/sum(x1944))*q4.race1.smok1.4751))
N1944.race2.smoke1<-as.numeric(c((as.numeric(x1944[6])/sum(x1944))*q1.race2.smok1.4751,(as.numeric(x1944[6])/sum(x1944))*q2.race2.smok1.4751,(as.numeric(x1944[6])/sum(x1944))*q3.race2.smok1.4751,(as.numeric(x1944[6])/sum(x1944))*q4.race2.smok1.4751))
N1944.race3.smoke1<-as.numeric(c((as.numeric(x1944[7])/sum(x1944))*q1.race3.smok1.4751,(as.numeric(x1944[7])/sum(x1944))*q2.race3.smok1.4751,(as.numeric(x1944[7])/sum(x1944))*q3.race3.smok1.4751,(as.numeric(x1944[7])/sum(x1944))*q4.race3.smok1.4751))
N1944.race4.smoke1<-as.numeric(c((as.numeric(x1944[8])/sum(x1944))*q1.race4.smok1.4751,(as.numeric(x1944[8])/sum(x1944))*q2.race4.smok1.4751,(as.numeric(x1944[8])/sum(x1944))*q3.race4.smok1.4751,(as.numeric(x1944[8])/sum(x1944))*q4.race4.smok1.4751))

x1943<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1943],weights=NHANES0916$weights[NHANES0916$birth_cohort==1943])
N1943.race1.smoke0<-as.numeric(c((as.numeric(x1943[1])/sum(x1943))*q1.race1.smok0.4751,(as.numeric(x1943[1])/sum(x1943))*q2.race1.smok0.4751,(as.numeric(x1943[1])/sum(x1943))*q3.race1.smok0.4751,(as.numeric(x1943[1])/sum(x1943))*q4.race1.smok0.4751))
N1943.race2.smoke0<-as.numeric(c((as.numeric(x1943[2])/sum(x1943))*q1.race2.smok0.4751,(as.numeric(x1943[2])/sum(x1943))*q2.race2.smok0.4751,(as.numeric(x1943[2])/sum(x1943))*q3.race2.smok0.4751,(as.numeric(x1943[2])/sum(x1943))*q4.race2.smok0.4751))
N1943.race3.smoke0<-as.numeric(c((as.numeric(x1943[3])/sum(x1943))*q1.race3.smok0.4751,(as.numeric(x1943[3])/sum(x1943))*q2.race3.smok0.4751,(as.numeric(x1943[3])/sum(x1943))*q3.race3.smok0.4751,(as.numeric(x1943[3])/sum(x1943))*q4.race3.smok0.4751))
N1943.race4.smoke0<-as.numeric(c((as.numeric(x1943[4])/sum(x1943))*q1.race4.smok0.4751,(as.numeric(x1943[4])/sum(x1943))*q2.race4.smok0.4751,(as.numeric(x1943[4])/sum(x1943))*q3.race4.smok0.4751,(as.numeric(x1943[4])/sum(x1943))*q4.race4.smok0.4751))
N1943.race1.smoke1<-as.numeric(c((as.numeric(x1943[5])/sum(x1943))*q1.race1.smok1.4751,(as.numeric(x1943[5])/sum(x1943))*q2.race1.smok1.4751,(as.numeric(x1943[5])/sum(x1943))*q3.race1.smok1.4751,(as.numeric(x1943[5])/sum(x1943))*q4.race1.smok1.4751))
N1943.race2.smoke1<-as.numeric(c((as.numeric(x1943[6])/sum(x1943))*q1.race2.smok1.4751,(as.numeric(x1943[6])/sum(x1943))*q2.race2.smok1.4751,(as.numeric(x1943[6])/sum(x1943))*q3.race2.smok1.4751,(as.numeric(x1943[6])/sum(x1943))*q4.race2.smok1.4751))
N1943.race3.smoke1<-as.numeric(c((as.numeric(x1943[7])/sum(x1943))*q1.race3.smok1.4751,(as.numeric(x1943[7])/sum(x1943))*q2.race3.smok1.4751,(as.numeric(x1943[7])/sum(x1943))*q3.race3.smok1.4751,(as.numeric(x1943[7])/sum(x1943))*q4.race3.smok1.4751))
N1943.race4.smoke1<-as.numeric(c((as.numeric(x1943[8])/sum(x1943))*q1.race4.smok1.4751,(as.numeric(x1943[8])/sum(x1943))*q2.race4.smok1.4751,(as.numeric(x1943[8])/sum(x1943))*q3.race4.smok1.4751,(as.numeric(x1943[8])/sum(x1943))*q4.race4.smok1.4751))

x1942<-wtd.table(NHANES0916$race.smoking[NHANES0916$birth_cohort==1942],weights=NHANES0916$weights[NHANES0916$birth_cohort==1942])
N1942.race1.smoke0<-as.numeric(c((as.numeric(x1942[1])/sum(x1942))*q1.race1.smok0.4751,(as.numeric(x1942[1])/sum(x1942))*q2.race1.smok0.4751,(as.numeric(x1942[1])/sum(x1942))*q3.race1.smok0.4751,(as.numeric(x1942[1])/sum(x1942))*q4.race1.smok0.4751))
N1942.race2.smoke0<-as.numeric(c((as.numeric(x1942[2])/sum(x1942))*q1.race2.smok0.4751,(as.numeric(x1942[2])/sum(x1942))*q2.race2.smok0.4751,(as.numeric(x1942[2])/sum(x1942))*q3.race2.smok0.4751,(as.numeric(x1942[2])/sum(x1942))*q4.race2.smok0.4751))
N1942.race3.smoke0<-as.numeric(c((as.numeric(x1942[3])/sum(x1942))*q1.race3.smok0.4751,(as.numeric(x1942[3])/sum(x1942))*q2.race3.smok0.4751,(as.numeric(x1942[3])/sum(x1942))*q3.race3.smok0.4751,(as.numeric(x1942[3])/sum(x1942))*q4.race3.smok0.4751))
N1942.race4.smoke0<-as.numeric(c((as.numeric(x1942[4])/sum(x1942))*q1.race4.smok0.4751,(as.numeric(x1942[4])/sum(x1942))*q2.race4.smok0.4751,(as.numeric(x1942[4])/sum(x1942))*q3.race4.smok0.4751,(as.numeric(x1942[4])/sum(x1942))*q4.race4.smok0.4751))
N1942.race1.smoke1<-as.numeric(c((as.numeric(x1942[5])/sum(x1942))*q1.race1.smok1.4751,(as.numeric(x1942[5])/sum(x1942))*q2.race1.smok1.4751,(as.numeric(x1942[5])/sum(x1942))*q3.race1.smok1.4751,(as.numeric(x1942[5])/sum(x1942))*q4.race1.smok1.4751))
N1942.race2.smoke1<-as.numeric(c((as.numeric(x1942[6])/sum(x1942))*q1.race2.smok1.4751,(as.numeric(x1942[6])/sum(x1942))*q2.race2.smok1.4751,(as.numeric(x1942[6])/sum(x1942))*q3.race2.smok1.4751,(as.numeric(x1942[6])/sum(x1942))*q4.race2.smok1.4751))
N1942.race3.smoke1<-as.numeric(c((as.numeric(x1942[7])/sum(x1942))*q1.race3.smok1.4751,(as.numeric(x1942[7])/sum(x1942))*q2.race3.smok1.4751,(as.numeric(x1942[7])/sum(x1942))*q3.race3.smok1.4751,(as.numeric(x1942[7])/sum(x1942))*q4.race3.smok1.4751))
N1942.race4.smoke1<-as.numeric(c((as.numeric(x1942[8])/sum(x1942))*q1.race4.smok1.4751,(as.numeric(x1942[8])/sum(x1942))*q2.race4.smok1.4751,(as.numeric(x1942[8])/sum(x1942))*q3.race4.smok1.4751,(as.numeric(x1942[8])/sum(x1942))*q4.race4.smok1.4751))

x1941<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$birth_cohort==1941],weights=nhis2009_16$wtfa_sa[nhis2009_16$birth_cohort==1941])
N1941.race1.smoke0<-as.numeric(c((as.numeric(x1941[1])/sum(x1941))*q1.race1.smok0.4751,(as.numeric(x1941[1])/sum(x1941))*q2.race1.smok0.4751,(as.numeric(x1941[1])/sum(x1941))*q3.race1.smok0.4751,(as.numeric(x1941[1])/sum(x1941))*q4.race1.smok0.4751))
N1941.race2.smoke0<-as.numeric(c((as.numeric(x1941[2])/sum(x1941))*q1.race2.smok0.4751,(as.numeric(x1941[2])/sum(x1941))*q2.race2.smok0.4751,(as.numeric(x1941[2])/sum(x1941))*q3.race2.smok0.4751,(as.numeric(x1941[2])/sum(x1941))*q4.race2.smok0.4751))
N1941.race3.smoke0<-as.numeric(c((as.numeric(x1941[3])/sum(x1941))*q1.race3.smok0.4751,(as.numeric(x1941[3])/sum(x1941))*q2.race3.smok0.4751,(as.numeric(x1941[3])/sum(x1941))*q3.race3.smok0.4751,(as.numeric(x1941[3])/sum(x1941))*q4.race3.smok0.4751))
N1941.race4.smoke0<-as.numeric(c((as.numeric(x1941[4])/sum(x1941))*q1.race4.smok0.4751,(as.numeric(x1941[4])/sum(x1941))*q2.race4.smok0.4751,(as.numeric(x1941[4])/sum(x1941))*q3.race4.smok0.4751,(as.numeric(x1941[4])/sum(x1941))*q4.race4.smok0.4751))
N1941.race1.smoke1<-as.numeric(c((as.numeric(x1941[5])/sum(x1941))*q1.race1.smok1.4751,(as.numeric(x1941[5])/sum(x1941))*q2.race1.smok1.4751,(as.numeric(x1941[5])/sum(x1941))*q3.race1.smok1.4751,(as.numeric(x1941[5])/sum(x1941))*q4.race1.smok1.4751))
N1941.race2.smoke1<-as.numeric(c((as.numeric(x1941[6])/sum(x1941))*q1.race2.smok1.4751,(as.numeric(x1941[6])/sum(x1941))*q2.race2.smok1.4751,(as.numeric(x1941[6])/sum(x1941))*q3.race2.smok1.4751,(as.numeric(x1941[6])/sum(x1941))*q4.race2.smok1.4751))
N1941.race3.smoke1<-as.numeric(c((as.numeric(x1941[7])/sum(x1941))*q1.race3.smok1.4751,(as.numeric(x1941[7])/sum(x1941))*q2.race3.smok1.4751,(as.numeric(x1941[7])/sum(x1941))*q3.race3.smok1.4751,(as.numeric(x1941[7])/sum(x1941))*q4.race3.smok1.4751))
N1941.race4.smoke1<-as.numeric(c((as.numeric(x1941[8])/sum(x1941))*q1.race4.smok1.4751,(as.numeric(x1941[8])/sum(x1941))*q2.race4.smok1.4751,(as.numeric(x1941[8])/sum(x1941))*q3.race4.smok1.4751,(as.numeric(x1941[8])/sum(x1941))*q4.race4.smok1.4751))

x1940<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$birth_cohort==1940],weights=nhis2009_16$wtfa_sa[nhis2009_16$birth_cohort==1940])
N1940.race1.smoke0<-as.numeric(c((as.numeric(x1940[1])/sum(x1940))*q1.race1.smok0.4751,(as.numeric(x1940[1])/sum(x1940))*q2.race1.smok0.4751,(as.numeric(x1940[1])/sum(x1940))*q3.race1.smok0.4751,(as.numeric(x1940[1])/sum(x1940))*q4.race1.smok0.4751))
N1940.race2.smoke0<-as.numeric(c((as.numeric(x1940[2])/sum(x1940))*q1.race2.smok0.4751,(as.numeric(x1940[2])/sum(x1940))*q2.race2.smok0.4751,(as.numeric(x1940[2])/sum(x1940))*q3.race2.smok0.4751,(as.numeric(x1940[2])/sum(x1940))*q4.race2.smok0.4751))
N1940.race3.smoke0<-as.numeric(c((as.numeric(x1940[3])/sum(x1940))*q1.race3.smok0.4751,(as.numeric(x1940[3])/sum(x1940))*q2.race3.smok0.4751,(as.numeric(x1940[3])/sum(x1940))*q3.race3.smok0.4751,(as.numeric(x1940[3])/sum(x1940))*q4.race3.smok0.4751))
N1940.race4.smoke0<-as.numeric(c((as.numeric(x1940[4])/sum(x1940))*q1.race4.smok0.4751,(as.numeric(x1940[4])/sum(x1940))*q2.race4.smok0.4751,(as.numeric(x1940[4])/sum(x1940))*q3.race4.smok0.4751,(as.numeric(x1940[4])/sum(x1940))*q4.race4.smok0.4751))
N1940.race1.smoke1<-as.numeric(c((as.numeric(x1940[5])/sum(x1940))*q1.race1.smok1.4751,(as.numeric(x1940[5])/sum(x1940))*q2.race1.smok1.4751,(as.numeric(x1940[5])/sum(x1940))*q3.race1.smok1.4751,(as.numeric(x1940[5])/sum(x1940))*q4.race1.smok1.4751))
N1940.race2.smoke1<-as.numeric(c((as.numeric(x1940[6])/sum(x1940))*q1.race2.smok1.4751,(as.numeric(x1940[6])/sum(x1940))*q2.race2.smok1.4751,(as.numeric(x1940[6])/sum(x1940))*q3.race2.smok1.4751,(as.numeric(x1940[6])/sum(x1940))*q4.race2.smok1.4751))
N1940.race3.smoke1<-as.numeric(c((as.numeric(x1940[7])/sum(x1940))*q1.race3.smok1.4751,(as.numeric(x1940[7])/sum(x1940))*q2.race3.smok1.4751,(as.numeric(x1940[7])/sum(x1940))*q3.race3.smok1.4751,(as.numeric(x1940[7])/sum(x1940))*q4.race3.smok1.4751))
N1940.race4.smoke1<-as.numeric(c((as.numeric(x1940[8])/sum(x1940))*q1.race4.smok1.4751,(as.numeric(x1940[8])/sum(x1940))*q2.race4.smok1.4751,(as.numeric(x1940[8])/sum(x1940))*q3.race4.smok1.4751,(as.numeric(x1940[8])/sum(x1940))*q4.race4.smok1.4751))

x1939<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$birth_cohort==1939],weights=nhis2009_16$wtfa_sa[nhis2009_16$birth_cohort==1939])
N1939.race1.smoke0<-as.numeric(c((as.numeric(x1939[1])/sum(x1939))*q1.race1.smok0.4751,(as.numeric(x1939[1])/sum(x1939))*q2.race1.smok0.4751,(as.numeric(x1939[1])/sum(x1939))*q3.race1.smok0.4751,(as.numeric(x1939[1])/sum(x1939))*q4.race1.smok0.4751))
N1939.race2.smoke0<-as.numeric(c((as.numeric(x1939[2])/sum(x1939))*q1.race2.smok0.4751,(as.numeric(x1939[2])/sum(x1939))*q2.race2.smok0.4751,(as.numeric(x1939[2])/sum(x1939))*q3.race2.smok0.4751,(as.numeric(x1939[2])/sum(x1939))*q4.race2.smok0.4751))
N1939.race3.smoke0<-as.numeric(c((as.numeric(x1939[3])/sum(x1939))*q1.race3.smok0.4751,(as.numeric(x1939[3])/sum(x1939))*q2.race3.smok0.4751,(as.numeric(x1939[3])/sum(x1939))*q3.race3.smok0.4751,(as.numeric(x1939[3])/sum(x1939))*q4.race3.smok0.4751))
N1939.race4.smoke0<-as.numeric(c((as.numeric(x1939[4])/sum(x1939))*q1.race4.smok0.4751,(as.numeric(x1939[4])/sum(x1939))*q2.race4.smok0.4751,(as.numeric(x1939[4])/sum(x1939))*q3.race4.smok0.4751,(as.numeric(x1939[4])/sum(x1939))*q4.race4.smok0.4751))
N1939.race1.smoke1<-as.numeric(c((as.numeric(x1939[5])/sum(x1939))*q1.race1.smok1.4751,(as.numeric(x1939[5])/sum(x1939))*q2.race1.smok1.4751,(as.numeric(x1939[5])/sum(x1939))*q3.race1.smok1.4751,(as.numeric(x1939[5])/sum(x1939))*q4.race1.smok1.4751))
N1939.race2.smoke1<-as.numeric(c((as.numeric(x1939[6])/sum(x1939))*q1.race2.smok1.4751,(as.numeric(x1939[6])/sum(x1939))*q2.race2.smok1.4751,(as.numeric(x1939[6])/sum(x1939))*q3.race2.smok1.4751,(as.numeric(x1939[6])/sum(x1939))*q4.race2.smok1.4751))
N1939.race3.smoke1<-as.numeric(c((as.numeric(x1939[7])/sum(x1939))*q1.race3.smok1.4751,(as.numeric(x1939[7])/sum(x1939))*q2.race3.smok1.4751,(as.numeric(x1939[7])/sum(x1939))*q3.race3.smok1.4751,(as.numeric(x1939[7])/sum(x1939))*q4.race3.smok1.4751))
N1939.race4.smoke1<-as.numeric(c((as.numeric(x1939[8])/sum(x1939))*q1.race4.smok1.4751,(as.numeric(x1939[8])/sum(x1939))*q2.race4.smok1.4751,(as.numeric(x1939[8])/sum(x1939))*q3.race4.smok1.4751,(as.numeric(x1939[8])/sum(x1939))*q4.race4.smok1.4751))

x1938<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$birth_cohort==1938],weights=nhis2009_16$wtfa_sa[nhis2009_16$birth_cohort==1938])
N1938.race1.smoke0<-as.numeric(c((as.numeric(x1938[1])/sum(x1938))*q1.race1.smok0.4751,(as.numeric(x1938[1])/sum(x1938))*q2.race1.smok0.4751,(as.numeric(x1938[1])/sum(x1938))*q3.race1.smok0.4751,(as.numeric(x1938[1])/sum(x1938))*q4.race1.smok0.4751))
N1938.race2.smoke0<-as.numeric(c((as.numeric(x1938[2])/sum(x1938))*q1.race2.smok0.4751,(as.numeric(x1938[2])/sum(x1938))*q2.race2.smok0.4751,(as.numeric(x1938[2])/sum(x1938))*q3.race2.smok0.4751,(as.numeric(x1938[2])/sum(x1938))*q4.race2.smok0.4751))
N1938.race3.smoke0<-as.numeric(c((as.numeric(x1938[3])/sum(x1938))*q1.race3.smok0.4751,(as.numeric(x1938[3])/sum(x1938))*q2.race3.smok0.4751,(as.numeric(x1938[3])/sum(x1938))*q3.race3.smok0.4751,(as.numeric(x1938[3])/sum(x1938))*q4.race3.smok0.4751))
N1938.race4.smoke0<-as.numeric(c((as.numeric(x1938[4])/sum(x1938))*q1.race4.smok0.4751,(as.numeric(x1938[4])/sum(x1938))*q2.race4.smok0.4751,(as.numeric(x1938[4])/sum(x1938))*q3.race4.smok0.4751,(as.numeric(x1938[4])/sum(x1938))*q4.race4.smok0.4751))
N1938.race1.smoke1<-as.numeric(c((as.numeric(x1938[5])/sum(x1938))*q1.race1.smok1.4751,(as.numeric(x1938[5])/sum(x1938))*q2.race1.smok1.4751,(as.numeric(x1938[5])/sum(x1938))*q3.race1.smok1.4751,(as.numeric(x1938[5])/sum(x1938))*q4.race1.smok1.4751))
N1938.race2.smoke1<-as.numeric(c((as.numeric(x1938[6])/sum(x1938))*q1.race2.smok1.4751,(as.numeric(x1938[6])/sum(x1938))*q2.race2.smok1.4751,(as.numeric(x1938[6])/sum(x1938))*q3.race2.smok1.4751,(as.numeric(x1938[6])/sum(x1938))*q4.race2.smok1.4751))
N1938.race3.smoke1<-as.numeric(c((as.numeric(x1938[7])/sum(x1938))*q1.race3.smok1.4751,(as.numeric(x1938[7])/sum(x1938))*q2.race3.smok1.4751,(as.numeric(x1938[7])/sum(x1938))*q3.race3.smok1.4751,(as.numeric(x1938[7])/sum(x1938))*q4.race3.smok1.4751))
N1938.race4.smoke1<-as.numeric(c((as.numeric(x1938[8])/sum(x1938))*q1.race4.smok1.4751,(as.numeric(x1938[8])/sum(x1938))*q2.race4.smok1.4751,(as.numeric(x1938[8])/sum(x1938))*q3.race4.smok1.4751,(as.numeric(x1938[8])/sum(x1938))*q4.race4.smok1.4751))

x1937<-wtd.table(nhis2009_16$race.smoking[nhis2009_16$birth_cohort==1937],weights=nhis2009_16$wtfa_sa[nhis2009_16$birth_cohort==1937])
N1937.race1.smoke0<-as.numeric(c((as.numeric(x1937[1])/sum(x1937))*q1.race1.smok0.4751,(as.numeric(x1937[1])/sum(x1937))*q2.race1.smok0.4751,(as.numeric(x1937[1])/sum(x1937))*q3.race1.smok0.4751,(as.numeric(x1937[1])/sum(x1937))*q4.race1.smok0.4751))
N1937.race2.smoke0<-as.numeric(c((as.numeric(x1937[2])/sum(x1937))*q1.race2.smok0.4751,(as.numeric(x1937[2])/sum(x1937))*q2.race2.smok0.4751,(as.numeric(x1937[2])/sum(x1937))*q3.race2.smok0.4751,(as.numeric(x1937[2])/sum(x1937))*q4.race2.smok0.4751))
N1937.race3.smoke0<-as.numeric(c((as.numeric(x1937[3])/sum(x1937))*q1.race3.smok0.4751,(as.numeric(x1937[3])/sum(x1937))*q2.race3.smok0.4751,(as.numeric(x1937[3])/sum(x1937))*q3.race3.smok0.4751,(as.numeric(x1937[3])/sum(x1937))*q4.race3.smok0.4751))
N1937.race4.smoke0<-as.numeric(c((as.numeric(x1937[4])/sum(x1937))*q1.race4.smok0.4751,(as.numeric(x1937[4])/sum(x1937))*q2.race4.smok0.4751,(as.numeric(x1937[4])/sum(x1937))*q3.race4.smok0.4751,(as.numeric(x1937[4])/sum(x1937))*q4.race4.smok0.4751))
N1937.race1.smoke1<-as.numeric(c((as.numeric(x1937[5])/sum(x1937))*q1.race1.smok1.4751,(as.numeric(x1937[5])/sum(x1937))*q2.race1.smok1.4751,(as.numeric(x1937[5])/sum(x1937))*q3.race1.smok1.4751,(as.numeric(x1937[5])/sum(x1937))*q4.race1.smok1.4751))
N1937.race2.smoke1<-as.numeric(c((as.numeric(x1937[6])/sum(x1937))*q1.race2.smok1.4751,(as.numeric(x1937[6])/sum(x1937))*q2.race2.smok1.4751,(as.numeric(x1937[6])/sum(x1937))*q3.race2.smok1.4751,(as.numeric(x1937[6])/sum(x1937))*q4.race2.smok1.4751))
N1937.race3.smoke1<-as.numeric(c((as.numeric(x1937[7])/sum(x1937))*q1.race3.smok1.4751,(as.numeric(x1937[7])/sum(x1937))*q2.race3.smok1.4751,(as.numeric(x1937[7])/sum(x1937))*q3.race3.smok1.4751,(as.numeric(x1937[7])/sum(x1937))*q4.race3.smok1.4751))
N1937.race4.smoke1<-as.numeric(c((as.numeric(x1937[8])/sum(x1937))*q1.race4.smok1.4751,(as.numeric(x1937[8])/sum(x1937))*q2.race4.smok1.4751,(as.numeric(x1937[8])/sum(x1937))*q3.race4.smok1.4751,(as.numeric(x1937[8])/sum(x1937))*q4.race4.smok1.4751))


results.1937<-(c(N1937.race1.smoke0,N1937.race1.smoke1,N1937.race2.smoke0,N1937.race2.smoke1,N1937.race3.smoke0,N1937.race3.smoke1,N1937.race4.smoke0,N1937.race4.smoke1)) 
results.1938<-(c(N1938.race1.smoke0,N1938.race1.smoke1,N1938.race2.smoke0,N1938.race2.smoke1,N1938.race3.smoke0,N1938.race3.smoke1,N1938.race4.smoke0,N1938.race4.smoke1)) 
results.1939<-(c(N1939.race1.smoke0,N1939.race1.smoke1,N1939.race2.smoke0,N1939.race2.smoke1,N1939.race3.smoke0,N1939.race3.smoke1,N1939.race4.smoke0,N1939.race4.smoke1)) 
results.1940<-(c(N1940.race1.smoke0,N1940.race1.smoke1,N1940.race2.smoke0,N1940.race2.smoke1,N1940.race3.smoke0,N1940.race3.smoke1,N1940.race4.smoke0,N1940.race4.smoke1)) 
results.1941<-(c(N1941.race1.smoke0,N1941.race1.smoke1,N1941.race2.smoke0,N1941.race2.smoke1,N1941.race3.smoke0,N1941.race3.smoke1,N1941.race4.smoke0,N1941.race4.smoke1)) 
results.1942<-(c(N1942.race1.smoke0,N1942.race1.smoke1,N1942.race2.smoke0,N1942.race2.smoke1,N1942.race3.smoke0,N1942.race3.smoke1,N1942.race4.smoke0,N1942.race4.smoke1)) 
results.1943<-(c(N1943.race1.smoke0,N1943.race1.smoke1,N1943.race2.smoke0,N1943.race2.smoke1,N1943.race3.smoke0,N1943.race3.smoke1,N1943.race4.smoke0,N1943.race4.smoke1)) 
results.1944<-(c(N1944.race1.smoke0,N1944.race1.smoke1,N1944.race2.smoke0,N1944.race2.smoke1,N1944.race3.smoke0,N1944.race3.smoke1,N1944.race4.smoke0,N1944.race4.smoke1)) 
results.1945<-(c(N1945.race1.smoke0,N1945.race1.smoke1,N1945.race2.smoke0,N1945.race2.smoke1,N1945.race3.smoke0,N1945.race3.smoke1,N1945.race4.smoke0,N1945.race4.smoke1)) 
results.1946<-(c(N1946.race1.smoke0,N1946.race1.smoke1,N1946.race2.smoke0,N1946.race2.smoke1,N1946.race3.smoke0,N1946.race3.smoke1,N1946.race4.smoke0,N1946.race4.smoke1)) 

#####
#most recent birth cohort at the top
temp2<-RESULTS[order(-birth_cohort),]
temp2a<-temp2[7:52, 2:33]/100

res2006.1993<-rbind(results.2006,results.2005, results.2004, results.2003, results.2002, results.2001, results.2000,
             results.1999, results.1998, results.1997, results.1996, results.1995, results.1994, results.1993)
res1946.37<-rbind(results.1946,results.1945,results.1944,results.1943,results.1942,results.1941,results.1940,
            results.1939,results.1938,results.1937)

a<-rbind(res2006.1993,temp2a,res1946.37, use.names=F)

fwrite(a, "L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/longitudinal_weights.csv")


###### creating weights for all smokers/ all non-smokers sensitivity analyses
rm(list=ls(all=TRUE))
library(data.table)
oldweights<-fread("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/weights.csv")

weights_nosmoking<-as.data.table(oldweights[,1]+oldweights[,5])
weights_nosmoking$V2<-oldweights[,2]+oldweights[,6]
weights_nosmoking$V3<-oldweights[,3]+oldweights[,7]
weights_nosmoking$V4<-oldweights[,4]+oldweights[,8]
weights_nosmoking$V8<-weights_nosmoking$V7<-weights_nosmoking$V6<-weights_nosmoking$V5<-c(rep(0,70))
weights_nosmoking$V9<-oldweights[,9]+oldweights[,13]
weights_nosmoking$V10<-oldweights[,10]+oldweights[,14]
weights_nosmoking$V11<-oldweights[,11]+oldweights[,15]
weights_nosmoking$V12<-oldweights[,12]+oldweights[,16]
weights_nosmoking$V16<-weights_nosmoking$V15<-weights_nosmoking$V14<-weights_nosmoking$V13<-c(rep(0,70))
weights_nosmoking$V17<-oldweights[,17]+oldweights[,21]
weights_nosmoking$V18<-oldweights[,18]+oldweights[,22]
weights_nosmoking$V19<-oldweights[,19]+oldweights[,23]
weights_nosmoking$V20<-oldweights[,20]+oldweights[,24]
weights_nosmoking$V24<-weights_nosmoking$V23<-weights_nosmoking$V22<-weights_nosmoking$V21<-c(rep(0,70))
weights_nosmoking$V25<-oldweights[,25]+oldweights[,29]
weights_nosmoking$V26<-oldweights[,26]+oldweights[,30]
weights_nosmoking$V27<-oldweights[,27]+oldweights[,31]
weights_nosmoking$V28<-oldweights[,28]+oldweights[,32]
weights_nosmoking$V32<-weights_nosmoking$V31<-weights_nosmoking$V30<-weights_nosmoking$V29<-c(rep(0,70))

fwrite(weights_nosmoking,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/weights_allnonsmokers.csv")

weights_allsmoking<-as.data.table(c(rep(0,70)))
weights_allsmoking$V4<-weights_allsmoking$V3<-weights_allsmoking$V2<-c(rep(0,70))
weights_allsmoking$V5<-as.data.table(oldweights[,1]+oldweights[,5])
weights_allsmoking$V6<-oldweights[,2]+oldweights[,6]
weights_allsmoking$V7<-oldweights[,3]+oldweights[,7]
weights_allsmoking$V8<-oldweights[,4]+oldweights[,8]
weights_allsmoking$V12<-weights_allsmoking$V11<-weights_allsmoking$V10<-weights_allsmoking$V9<-c(rep(0,70))
weights_allsmoking$V13<-oldweights[,9]+oldweights[,13]
weights_allsmoking$V14<-oldweights[,10]+oldweights[,14]
weights_allsmoking$V15<-oldweights[,11]+oldweights[,15]
weights_allsmoking$V16<-oldweights[,12]+oldweights[,16]
weights_allsmoking$V20<-weights_allsmoking$V19<-weights_allsmoking$V18<-weights_allsmoking$V17<-c(rep(0,70))
weights_allsmoking$V21<-oldweights[,17]+oldweights[,21]
weights_allsmoking$V22<-oldweights[,18]+oldweights[,22]
weights_allsmoking$V23<-oldweights[,19]+oldweights[,23]
weights_allsmoking$V24<-oldweights[,20]+oldweights[,24]
weights_allsmoking$V28<-weights_allsmoking$V27<-weights_allsmoking$V26<-weights_allsmoking$V25<-c(rep(0,70))
weights_allsmoking$V29<-oldweights[,25]+oldweights[,29]
weights_allsmoking$V30<-oldweights[,26]+oldweights[,30]
weights_allsmoking$V31<-oldweights[,27]+oldweights[,31]
weights_allsmoking$V32<-oldweights[,28]+oldweights[,32]

fwrite(weights_allsmoking,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/weights_allsmokers.csv")

###### creating longitudinal weights for all smokers/ all non-smokers sensitivity analyses
rm(list=ls(all=TRUE))### Clears existing dataset/code from R
library(data.table)
oldweights<-fread("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/longitudinal_weights.csv")

weights_nosmoking<-as.data.table(oldweights[,1]+oldweights[,5])
weights_nosmoking$V2<-oldweights[,2]+oldweights[,6]
weights_nosmoking$V3<-oldweights[,3]+oldweights[,7]
weights_nosmoking$V4<-oldweights[,4]+oldweights[,8]
weights_nosmoking$V8<-weights_nosmoking$V7<-weights_nosmoking$V6<-weights_nosmoking$V5<-c(rep(0,70))
weights_nosmoking$V9<-oldweights[,9]+oldweights[,13]
weights_nosmoking$V10<-oldweights[,10]+oldweights[,14]
weights_nosmoking$V11<-oldweights[,11]+oldweights[,15]
weights_nosmoking$V12<-oldweights[,12]+oldweights[,16]
weights_nosmoking$V16<-weights_nosmoking$V15<-weights_nosmoking$V14<-weights_nosmoking$V13<-c(rep(0,70))
weights_nosmoking$V17<-oldweights[,17]+oldweights[,21]
weights_nosmoking$V18<-oldweights[,18]+oldweights[,22]
weights_nosmoking$V19<-oldweights[,19]+oldweights[,23]
weights_nosmoking$V20<-oldweights[,20]+oldweights[,24]
weights_nosmoking$V24<-weights_nosmoking$V23<-weights_nosmoking$V22<-weights_nosmoking$V21<-c(rep(0,70))
weights_nosmoking$V25<-oldweights[,25]+oldweights[,29]
weights_nosmoking$V26<-oldweights[,26]+oldweights[,30]
weights_nosmoking$V27<-oldweights[,27]+oldweights[,31]
weights_nosmoking$V28<-oldweights[,28]+oldweights[,32]
weights_nosmoking$V32<-weights_nosmoking$V31<-weights_nosmoking$V30<-weights_nosmoking$V29<-c(rep(0,70))

fwrite(weights_nosmoking,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/longitudinal_weights_allnonsmokers.csv")

weights_allsmoking<-as.data.table(c(rep(0,70)))
weights_allsmoking$V4<-weights_allsmoking$V3<-weights_allsmoking$V2<-c(rep(0,70))
weights_allsmoking$V5<-as.data.table(oldweights[,1]+oldweights[,5])
weights_allsmoking$V6<-oldweights[,2]+oldweights[,6]
weights_allsmoking$V7<-oldweights[,3]+oldweights[,7]
weights_allsmoking$V8<-oldweights[,4]+oldweights[,8]
weights_allsmoking$V12<-weights_allsmoking$V11<-weights_allsmoking$V10<-weights_allsmoking$V9<-c(rep(0,70))
weights_allsmoking$V13<-oldweights[,9]+oldweights[,13]
weights_allsmoking$V14<-oldweights[,10]+oldweights[,14]
weights_allsmoking$V15<-oldweights[,11]+oldweights[,15]
weights_allsmoking$V16<-oldweights[,12]+oldweights[,16]
weights_allsmoking$V20<-weights_allsmoking$V19<-weights_allsmoking$V18<-weights_allsmoking$V17<-c(rep(0,70))
weights_allsmoking$V21<-oldweights[,17]+oldweights[,21]
weights_allsmoking$V22<-oldweights[,18]+oldweights[,22]
weights_allsmoking$V23<-oldweights[,19]+oldweights[,23]
weights_allsmoking$V24<-oldweights[,20]+oldweights[,24]
weights_allsmoking$V28<-weights_allsmoking$V27<-weights_allsmoking$V26<-weights_allsmoking$V25<-c(rep(0,70))
weights_allsmoking$V29<-oldweights[,25]+oldweights[,29]
weights_allsmoking$V30<-oldweights[,26]+oldweights[,30]
weights_allsmoking$V31<-oldweights[,27]+oldweights[,31]
weights_allsmoking$V32<-oldweights[,28]+oldweights[,32]

fwrite(weights_allsmoking,"L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Data/longitudinal_weights_allsmokers.csv")