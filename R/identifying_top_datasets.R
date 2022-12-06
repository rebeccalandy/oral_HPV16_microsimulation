rm(list=ls(all=TRUE))   
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/")

library("readxl")
library("dplyr")
library("tidyr")
library(data.table)
library(survey)


for (i in 1:10000) {
  print(i)
calibration_data<- fread(file=paste0("calibration_results_",i,".csv"), header=TRUE)     

assign(paste0("calibration_data.",i), calibration_data)
}

rm(calibration_data)

z <- as.list(mget(paste("calibration_data.", 1:10000, sep="")))

combined<-bind_rows(z)

combined<- combined[order(combined$"gof_total") , ]
selected<-head(combined,50)
fwrite(selected, file="selected.csv", append=F)

