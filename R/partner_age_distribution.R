rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R

library(SASxport)
library(dplyr)
library(survey)
library(haven)
library(forcats)
library(questionr)
library(weights)
library(pscl)
library(data.table)

setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/")

#using NHANES data from 2005-6

sxq_d<-read_sas("H:/Oropharyngeal/NHANES data/datasets to create cx2/sxq_d.sas7bdat")
demo_d<-read_sas("H:/Oropharyngeal/NHANES data/datasets to create cx2/demo_d.sas7bdat")
data<-merge(sxq_d,demo_d, by="SEQN" )
data<-filter(data, RIAGENDR==1)

#total
data$femalerecent<-(data$SXQ510)
data$femalerecent<-ifelse(data$SXQ021==2,0,
                        ifelse(data$SXD171==0,0,
                               ifelse(data$femalerecent<77777,data$femalerecent,NA)))
data$malerecent<-data$SXQ550
data$malerecent<-ifelse(data$SXQ021==2,0,
                        ifelse(data$SXQ410==0,0,
                          ifelse(data$malerecent<77777,data$malerecent,NA)))
data$totalrecent<-data$femalerecent+data$malerecent

# 5-yrs younger
table(data$SXQ600)
data$SXQ600<-ifelse(data$SXQ600>999,NA,data$SXQ600)

# 5-yrs older
table(data$SXQ590)
data$SXQ590<-ifelse(data$SXQ590>999,NA,data$SXQ590)

data$same<-data$totalrecent-data$SXQ590-data$SXQ600 
table(data$same)

data$agecat<-ifelse(data$RIDAGEYR>=20 & data$RIDAGEYR<25,"20-24",
                    ifelse(data$RIDAGEYR>=25 & data$RIDAGEYR<30, "25-29",
                           ifelse(data$RIDAGEYR>=30 & data$RIDAGEYR<35,"30-34",
                                  ifelse(data$RIDAGEYR>=35 & data$RIDAGEYR<40, "35-39",
                                         ifelse(data$RIDAGEYR>=40 & data$RIDAGEYR<45,"40-44",
                                                ifelse(data$RIDAGEYR>=45 & data$RIDAGEYR<50, "45-49",
                                                       ifelse(data$RIDAGEYR>=50 & data$RIDAGEYR<55,"50-54",
                                                              ifelse(data$RIDAGEYR>=55 & data$RIDAGEYR<60, "55-59", NA ))))))))

#exclude anyone with negative same age group number of partners
data<-dplyr::filter(data, same>=0 )
data<-dplyr::filter(data, totalrecent>0 )

t3<-dplyr::filter(data, RIDAGEYR==20)
t4<-dplyr::select(t3, c("SEQN","RIDAGEYR","SXQ600","SXQ590","same","totalrecent","prop.younger","prop.same","prop.older","WTMEC2YR"))

temp<-dplyr::filter(data, is.na(SXQ600)==T | is.na(SXQ590)==T)
temp2<-dplyr::select(temp, c("SEQN","RIDAGEYR","SXQ600","SXQ590","same","totalrecent","prop.younger","prop.same","prop.older","weighted_prop.older","weighted_prop.same","weighted_prop.younger"))
View(temp2)



data$prop.younger<-(data$SXQ600/data$totalrecent)
data$prop.older<-(data$SXQ590/data$totalrecent)
data$prop.same<-(data$same/data$totalrecent)

data<-data %>%
  group_by(RIDAGEYR) %>% 
  mutate(weighted_prop.younger = weighted.mean(prop.younger, WTMEC2YR,na.rm=T))

data<-data %>%
  group_by(RIDAGEYR) %>% 
  mutate(weighted_prop.older = weighted.mean(prop.older, WTMEC2YR,na.rm=T))


data<-data %>%
  group_by(agecat) %>% 
  mutate(weighted_prop.younger.gp = weighted.mean(prop.younger, WTMEC2YR,na.rm=T))

data<-data %>%
  group_by(agecat) %>% 
  mutate(weighted_prop.older.gp = weighted.mean(prop.older, WTMEC2YR,na.rm=T))


newdata<-dplyr::select(data,"RIDAGEYR","weighted_prop.older","weighted_prop.younger","weighted_prop.older.gp", "weighted_prop.younger.gp")
newdata<-distinct(newdata)
newdata$weighted_prop.same=1-newdata$weighted_prop.older-newdata$weighted_prop.younger
newdata$weighted_prop.same.gp=1-newdata$weighted_prop.older.gp-newdata$weighted_prop.younger.gp

View(newdata)
newdata <- newdata[order(newdata$RIDAGEYR),]
fwrite(newdata,"H:/Oropharyngeal/NATSAL/NHANESpartnersagedist.csv")

nhanes55.59.propyounger<-newdata$weighted_prop.younger.gp[newdata$RIDAGEYR==55]
nhanes55.59.propolder<-newdata$weighted_prop.older.gp[newdata$RIDAGEYR==55]
nhanes55.59.propsame=1-nhanes55.59.propolder-nhanes55.59.propyounger

natsal.age.cat<-fread("H:/Oropharyngeal/NATSAL/agediffpartnersinlastyearCat.csv")

names(newdata)[names(newdata) == 'RIDAGEYR'] <- 'age'

merged<-merge(newdata,natsal.age.cat, by="age", all = T)

agecat<-c("15-19","15-19","15-19","80-84","80-84","80-84","80-84","80-84")
age<-c(15:17,80:84)
extendedage<-cbind(age,agecat)
merged<-merge(merged,extendedage, by=c("age","agecat"), all=T)
merged <- merged[order(merged$age),]


merged$weighted_prop.older<-ifelse(merged$age>=60,nhanes55.59.propolder,
                                   ifelse(merged$age<20,0,merged$weighted_prop.older))
merged$weighted_prop.younger<-ifelse(merged$age>=60,nhanes55.59.propyounger,
                                     ifelse(merged$age<20,0,merged$weighted_prop.younger))
merged$weighted_prop.same<-ifelse(merged$age>=60,nhanes55.59.propsame,
                                  ifelse(merged$age<20,1,merged$weighted_prop.same))


natsal7074.c1<-merged$c1[merged$age==75]
natsal7074.c2<-merged$c2[merged$age==75]
natsal7074.c3<-merged$c3[merged$age==75]
natsal7074.c4<-merged$c4[merged$age==75]
natsal7074.c5<-merged$c5[merged$age==75]
natsal7074.c6<-merged$c6[merged$age==75]
natsal7074.c7<-merged$c7[merged$age==75]
natsal7074.c8<-merged$c8[merged$age==75]

merged$c1<-ifelse(merged$age>=80,natsal7074.c1,merged$c1)
merged$c2<-ifelse(merged$age>=80,natsal7074.c2,merged$c2)
merged$c3<-ifelse(merged$age>=80,natsal7074.c3,merged$c3)
merged$c4<-ifelse(merged$age>=80,natsal7074.c4,merged$c4)
merged$c5<-ifelse(merged$age>=80,natsal7074.c5,merged$c5)
merged$c6<-ifelse(merged$age>=80,natsal7074.c6,merged$c6)
merged$c7<-ifelse(merged$age>=80,natsal7074.c7,merged$c7)
merged$c8<-ifelse(merged$age>=80,natsal7074.c8,merged$c8)

merged$total1<-merged$c1+merged$c2+merged$c3
merged$total2<-merged$c4+merged$c5
merged$total3<-merged$c6+merged$c7+merged$c8

merged$o1<-(merged$c1/merged$total1)*merged$weighted_prop.older
merged$o2<-(merged$c2/merged$total1)*merged$weighted_prop.older
merged$o3<-(merged$c3/merged$total1)*merged$weighted_prop.older
merged$o4<-(merged$c4/merged$total2)*merged$weighted_prop.same
merged$o5<-(merged$c5/merged$total2)*merged$weighted_prop.same
merged$o6<-(merged$c6/merged$total3)*merged$weighted_prop.younger
merged$o7<-(merged$c7/merged$total3)*merged$weighted_prop.younger
merged$o8<-(merged$c8/merged$total3)*merged$weighted_prop.younger

merged$o6<-ifelse(merged$total3==0,0,merged$o6)
merged$o7<-ifelse(merged$total3==0,0,merged$o7)
merged$o8<-ifelse(merged$total3==0,0,merged$o8)

merged.o1.18<-merged$o1[merged$age==18]
merged.o2.18<-merged$o2[merged$age==18]
merged.o3.18<-merged$o3[merged$age==18]
merged.o4.18<-merged$o4[merged$age==18]
merged.o5.18<-merged$o5[merged$age==18]
merged.o6.18<-merged$o6[merged$age==18]
merged.o7.18<-merged$o7[merged$age==18]
merged.o8.18<-merged$o8[merged$age==18]

merged$o1<-ifelse(merged$age<18,merged.o1.18,merged$o1)
merged$o2<-ifelse(merged$age<18,merged.o2.18,merged$o2)
merged$o3<-ifelse(merged$age<18,merged.o3.18,merged$o3)
merged$o4<-ifelse(merged$age<18,merged.o4.18,merged$o4)
merged$o5<-ifelse(merged$age<18,merged.o5.18,merged$o5)
merged$o6<-ifelse(merged$age<18,merged.o6.18,merged$o6)
merged$o7<-ifelse(merged$age<18,merged.o7.18,merged$o7)
merged$o8<-ifelse(merged$age<18,merged.o8.18,merged$o8)

merged$cum1<-merged$o8
merged$cum2<-merged$cum1+merged$o7
merged$cum3<-merged$cum2+merged$o6
merged$cum4<-merged$cum3+merged$o5
merged$cum5<-merged$cum4+merged$o4
merged$cum6<-merged$cum5+merged$o3
merged$cum7<-merged$cum6+merged$o2

output<-dplyr::select(merged,c("cum1","cum2","cum3","cum4","cum5","cum6","cum7"))
fwrite(output,"Data/partner_ages.csv",col.names = F)