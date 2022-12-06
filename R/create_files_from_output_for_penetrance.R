rm(list=ls(all=TRUE)) 
library(data.table)
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/")

seeds<-fread("calibrated_seeds.csv")
pop.size<-fread("Data/pop_size.csv")
pop.size<-as.numeric(unlist(pop.size))


for (j in 1:50) {
  data<-fread(paste0("Results/states/states_",seeds[j],".csv"))
  
  #duration distribution from age 30-84
  data2<-data/rowSums(data, na.rm=T)
  data3<-data2[16:70,]
  fwrite(data3,paste0("Data/duration.dist.",seeds[j],".csv"))
  
  #number of HPV infections at each age, for each calibrated seed
  Nhpv<-c(seeds[j], rowSums(data))
  fwrite(Nhpv, "Data/NHPVper100000.csv", append=T)
  
  
  Nhpv2<-rowSums(data)
  NHPVper100000<-as.list(pop.size*mapply('/', Nhpv2, 100000))
  fwrite(NHPVper100000,"Data/NHPV.csv", append=T)
  
  
}
