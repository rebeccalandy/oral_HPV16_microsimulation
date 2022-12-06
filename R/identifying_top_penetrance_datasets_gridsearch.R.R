rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R
library(data.table)
library(dplyr)

setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/")

out.dir<-"Penetrance_results/"

seeds<-fread("calibrated_seeds.csv")[1:50]
calibrated<-list(seeds)

for (i in 1:50) {
  print(i)
  kk<-calibrated[[1]][i]
  data3<- fread(file=paste0("Penetrance_results/cancer3074.",kk,".csv"), header=TRUE)
  data3<-cbind(i, data3)
  assign(paste0("data3.",i), data3)
}

rm(data3)

z3 <- as.list(mget(paste("data3.", 1:50, sep="")))

combined3<-bind_rows(z3)


data<- combined3[order(combined3$"gof_total30.74") , ]
data$can_rank=c(1:50)
fwrite(data, file="Penetrance_results/selected.combined.30.74.csv", append=FALSE)


library(dplyr)
data<-fread("Penetrance_results/selected.combined.30.74.csv")
penetrance<- data[order(data$i) , ]
penetrance<-select(penetrance, starts_with("pen"))
fwrite(penetrance, "Data/penetrance.csv", append=F, col.names=F)
