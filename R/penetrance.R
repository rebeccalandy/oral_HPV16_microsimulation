
rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R
library("readxl")
library("dplyr")
library("tidyr")
#library("xlsx")
library(data.table)

ptm <- proc.time()

seeds<-fread("calibrated_seeds.csv")
calibrated<-list(seeds)
SEER_calibration_targets<-read.csv("Data/SEER.calibration.rates.HPV16.csv", header=TRUE)[4:14, 2]
pop.size<-fread("Data/pop_size.csv")
pop.size<-as.numeric(unlist(pop.size))
NHPVper100000<-fread("Data/NHPVper100000.csv")


# Read initial seed from command line
 args <- commandArgs(trailingOnly = F)
 myarg <-  args[length(args)]
 myarg <- as.numeric(unlist(strsplit(myarg,'-')))

penetrance<-fread("penetrance_data/penetrance_quadratic_valid_halfyears_extendedage74.csv")
names(penetrance)[names(penetrance)=="Var1"] <- "a"
names(penetrance)[names(penetrance)=="Var2"] <- "b"
names(penetrance)[names(penetrance)=="Var3"] <- "c"
penetranceabc<-penetrance

 penfileno<-as.numeric(myarg[2])  ###part of penetrance simulation
 jj<-as.numeric(myarg[3])  #calibration number

nsim<-dim(penetrance)[1]
pennumstart<-(penfileno-1)*nsim
pennumend<-min(dim(penetranceabc)[1],pennumstart+nsim)

out.dir <- "Penetrance_results/"

penetrance<-penetrance[pennumstart:pennumend,1:70]
penetranceabc<-penetranceabc[pennumstart:pennumend]
penetranceabc<-select(penetranceabc,c(a,b,c))

kk<-calibrated[[1]][jj]  
duration.hpv<-fread(paste0("Data/duration.dist.",kk,".csv"),header=TRUE, fill=TRUE)

NHPVper100000<-NHPVper100000[jj,2:71]
duration.hpv<-as.matrix(duration.hpv[,1:70])
pop.size <- pop.size[16:70]

results.outer <- data.table(a = numeric(),
                            b = numeric(),
                            c = numeric(),
                            can.inc.30.34 = numeric(), 
                            can.inc.35.39 = numeric(), 
                            can.inc.40.44 = numeric(), 
                            can.inc.45.49 = numeric(),
                            can.inc.50.54 = numeric(), 
                            can.inc.55.59 = numeric(), 
                            can.inc.60.64 = numeric(), 
                            can.inc.65.69 = numeric(), 
                            can.inc.70.74 = numeric(),   
                            can.inc.75.79 = numeric(),   
                            can.inc.80.84 = numeric(),   
                            gof_rate_30_34 = numeric(),
                            gof_rate_35_39 = numeric(),
                            gof_rate_40_44 = numeric(),
                            gof_rate_45_49 = numeric(),
                            gof_rate_50_54 = numeric(),
                            gof_rate_55_59 = numeric(),
                            gof_rate_60_64 = numeric(),
                            gof_rate_65_69 = numeric(),
                            gof_rate_70_74 = numeric(),
                            gof_rate_75_79 = numeric(),
                            gof_rate_80_84 = numeric(),
                            gof_total30.69 = numeric(),
                            gof_total30.74 = numeric(), 
                            gof_total30.84 = numeric(), 
                            pen.data.1 = numeric(), 
                            pen.data.2 = numeric(),
                            pen.data.3 = numeric(),
                            pen.data.4 = numeric(),
                            pen.data.5 = numeric(),
                            pen.data.6 = numeric(),
                            pen.data.7 = numeric(),
                            pen.data.8 = numeric(),
                            pen.data.9 = numeric(),
                            pen.data.10 = numeric(),
                            pen.data.11 = numeric(),
                            pen.data.12 = numeric(),
                            pen.data.13 = numeric(),
                            pen.data.14 = numeric(),
                            pen.data.15 = numeric(),
                            pen.data.16 = numeric(),
                            pen.data.17 = numeric(),
                            pen.data.18 = numeric(),
                            pen.data.19 = numeric(),
                            pen.data.20 = numeric(),
                            pen.data.21 = numeric(),
                            pen.data.22 = numeric(),
                            pen.data.23 = numeric(),
                            pen.data.24 = numeric(),
                            pen.data.25 = numeric(),
                            pen.data.26 = numeric(),
                            pen.data.27 = numeric(),
                            pen.data.28 = numeric(),
                            pen.data.29 = numeric(),
                            pen.data.30 = numeric(),
                            pen.data.31 = numeric(),
                            pen.data.32 = numeric(),
                            pen.data.33 = numeric(),
                            pen.data.34 = numeric(),
                            pen.data.35 = numeric(),
                            pen.data.36 = numeric(),
                            pen.data.37 = numeric(),
                            pen.data.38 = numeric(),
                            pen.data.39 = numeric(),
                            pen.data.40 = numeric(),
                            pen.data.41 = numeric(),
                            pen.data.42 = numeric(),
                            pen.data.43 = numeric(),
                            pen.data.44 = numeric(),
                            pen.data.45 = numeric(),
                            pen.data.46 = numeric(),
                            pen.data.47 = numeric(),
                            pen.data.48 = numeric(),
                            pen.data.49 = numeric(), 
                            pen.data.50 = numeric(),
                            pen.data.51 = numeric(),
                            pen.data.52 = numeric(),
                            pen.data.53 = numeric(),
                            pen.data.54 = numeric(),
                            pen.data.55 = numeric(),
                            pen.data.56 = numeric(),
                            pen.data.57 = numeric(),
                            pen.data.58 = numeric(),
                            pen.data.59 = numeric(),
                            pen.data.60 = numeric(),
                            pen.data.61 = numeric(),
                            pen.data.62 = numeric(),
                            pen.data.63 = numeric(),
                            pen.data.64 = numeric(),
                            pen.data.65 = numeric(),
                            pen.data.66 = numeric(),
                            pen.data.67 = numeric(),
                            pen.data.68 = numeric(),
                            pen.data.69 = numeric(),
                            pen.data.70 = numeric())[1:nsim]

for (pen in 1:nsim) {
  pen.data = as.numeric(penetrance[pen,])
  a<-penetranceabc$a[pen]
  b<-penetranceabc$b[pen]
  c<-penetranceabc$c[pen]
  
  cancer <- numeric(length = 55)
  
  for (age in 30:84)  {
    popsize.n<-age-14
    dur.n<-age-29
    duration<-duration.hpv[dur.n,]
    Nhpvrate.age<-NHPVper100000[[popsize.n]]
    cancer[age - 29] <- Nhpvrate.age * pen.data %*% duration
  }
  
  inner.product <- cancer * pop.size
  inner.product.sums <- colSums(matrix(inner.product, nrow = 5))
  pop.size.sums <- colSums(matrix(pop.size, nrow = 5))
  
  can.inc <- inner.product.sums / pop.size.sums
  
  gof <- (can.inc - SEER_calibration_targets)^2 / SEER_calibration_targets
  gof.totals <- c(cumsum(gof)[8:9],cumsum(gof)[11])

out1<-c(a,b,c, can.inc, gof, gof.totals, pen.data)
    
  results.outer[pen, names(results.outer) := as.list(out1)]  
}

results.outer<- results.outer[order(results.outer$"gof_total30.74") , ]
selected<-head(results.outer,1)
fwrite(selected, paste0(out.dir,"cancer3074.",kk,".csv"))

# Stop the clock
proc.time() - ptm
