rm(list=ls(all=TRUE)) 
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")
library(data.table)
seeds<-fread("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/calibrated_seeds.csv")

#population estimates are backwards - first element is age 84 in 2021, all the way to age 15 in 2021
population_estimate<-c(
  451274.1383,
  517866.609,
  569138.7105,
  627494.7446,
  687549.9632,
  780716.9802,
  924613.0322,
  927033.9312,
  976326.2592,
  1010645.961,
  1401897.633,
  1362825.335,
  1394093.413,
  1455134.838,
  1501462.759,
  1567115.193,
  1644412.987,
  1725115.777,
  1821313.739,
  1850675.039,
  1937033.765,
  1976323.99,
  2004050.269,
  2095152.13,
  2082508.257,
  2071956.862,
  2087967.638,
  2102995.69,
  2082867.049,
  1973963.893,
  1944061.34,
  1954398.779,
  2004328.007,
  2125340.1,
  2096271.815,
  1989311.326,
  1905266.716,
  1887590.189,
  1960599.225,
  1901949.042,
  1961610.671,
  1990974.953,
  2036384.961,
  2184789.635,
  2139220.061,
  2171118.982,
  2181543.847,
  2158202.341,
  2239791.172,
  2234123.865,
  2230995.04,
  2272514.42,
  2332057.096,
  2430861.406,
  2443006.504,
  2412863.764,
  2357903.24,
  2318247.053,
  2270275.023,
  2215000.53,
  2188949.979,
  2181209.363,
  2177830.614,
  2210027.953,
  2168346.048,
  2109224.545,
  2113411.191,
  2127493.975,
  2123934.045,
  2131764.359)

birthcohort<-c(1937:2006)
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")

Nint<-250
vac_scen<-51
for (k in 1:50) {
  seed<-seeds[[k,1]]    
  
  atrisktotal<-fread(paste0("at_risk_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  cancerstotal<-fread(paste0("cancer_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  cancerinctotal<-fread(paste0("cancer_incidence_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  
  assign(paste0("atrisktotal_SQ26_",k),atrisktotal)
  assign(paste0("cancerstotal_SQ26_",k),cancerstotal)
  assign(paste0("cancerinctotal_SQ26_",k),cancerinctotal)
}

setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")

vac_scen<-55
for (k in 1:50) {
  seed<-seeds[[k,1]]    
  
  atrisktotal<-fread(paste0("at_risk_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  cancerstotal<-fread(paste0("cancer_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  cancerinctotal<-fread(paste0("cancer_incidence_means_",vac_scen,"_",seed,"_",Nint,".csv"))
  
  assign(paste0("atrisktotal_novac_",k),atrisktotal)
  assign(paste0("cancerstotal_novac_",k),cancerstotal)
  assign(paste0("cancerinctotal_novac_",k),cancerinctotal)
}


# for the number vaccinated from 2021 onwards, 
# first row is 1937 birth cohort, last row is 2006 
# first row, just the 70th
# second row, sum 69th + 70th

# first column is age 15, then 16 etc to 84
# so for year 2021, want last column of first row ()

#since comparing to different year, need to use incidence and not number of cancers

for (k in 1:50) {
  seed<-seeds[[k,1]]    

  cancers_per100000_SQ26_2021<-c(rep(NA,70))
  for (i in 1:70) {
    j=69-i+2
    cancers_per100000_SQ26_2021[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }
  # this is the number of cancers per 100,000 people from age 84 to age 15 in the year 2021
  assign(paste0("cancers_per100000_SQ26_2021_",k),cancers_per100000_SQ26_2021)
  
  atrisk_SQ26_2021<-c(rep(NA,70))
  for (i in 1:70) {
    j=69-i+2
    atrisk_SQ26_2021[i]<-eval(parse(text=paste0("atrisktotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("atrisk_SQ26_2021_",k),atrisk_SQ26_2021)

  cancerinc_per100000_SQ26_2021<-c(rep(NA,70))
  for (i in 1:70) {
    j=69-i+2
    cancerinc_per100000_SQ26_2021[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2021_",k),cancerinc_per100000_SQ26_2021)
  
  cancers_per100000_SQ26_2025<-c(rep(NA,70))
  for (i in 5:70) {
    j=69-i+6
    cancers_per100000_SQ26_2025[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancers_per100000_SQ26_2025_",k),cancers_per100000_SQ26_2025)
  
  atrisk_SQ26_2025<-c(rep(NA,70))
  for (i in 5:70) {
    j=69-i+6
    atrisk_SQ26_2025[i]<-eval(parse(text=paste0("atrisktotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("atrisk_SQ26_2025_",k),atrisk_SQ26_2025)

  cancerinc_per100000_SQ26_2025<-c(rep(NA,70))
  for (i in 5:70) {
    j=69-i+6
    cancerinc_per100000_SQ26_2025[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2025_",k),cancerinc_per100000_SQ26_2025)
  
  cancers_per100000_SQ26_2030<-c(rep(NA,70))
  for (i in 10:70) {
    j=69-i+11
    cancers_per100000_SQ26_2030[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancers_per100000_SQ26_2030_",k),cancers_per100000_SQ26_2030)

  cancerinc_per100000_SQ26_2030<-c(rep(NA,70))
  for (i in 10:70) {
    j=69-i+11
    cancerinc_per100000_SQ26_2030[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2030_",k),cancerinc_per100000_SQ26_2030)
  
  cancers_per100000_SQ26_2035<-c(rep(NA,70))
  for (i in 15:70) {
    j=69-i+16
    cancers_per100000_SQ26_2035[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancers_per100000_SQ26_2035_",k),cancers_per100000_SQ26_2035)

  cancerinc_per100000_SQ26_2035<-c(rep(NA,70))
  for (i in 15:70) {
    j=69-i+16
    cancerinc_per100000_SQ26_2035[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2035_",k),cancerinc_per100000_SQ26_2035)
  
  cancers_per100000_SQ26_2040<-c(rep(NA,70))
  for (i in 20:70) {
    j=69-i+21
    cancers_per100000_SQ26_2040[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancers_per100000_SQ26_2040_",k),cancers_per100000_SQ26_2040)
  
  cancerinc_per100000_SQ26_2040<-c(rep(NA,70))
  for (i in 20:70) {
    j=69-i+21
    cancerinc_per100000_SQ26_2040[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2040_",k),cancerinc_per100000_SQ26_2040)
  
  cancers_per100000_SQ26_2045<-c(rep(NA,70))
  for (i in 25:70) {
    j=69-i+26
    cancers_per100000_SQ26_2045[i]<-eval(parse(text=paste0("cancerstotal_SQ26_",k,"[[i, j]]")))
  }

  assign(paste0("cancers_per100000_SQ26_2045_",k),cancers_per100000_SQ26_2045)

    cancerinc_per100000_SQ26_2045<-c(rep(NA,70))
  for (i in 25:70) {
    j=69-i+26
    cancerinc_per100000_SQ26_2045[i]<-eval(parse(text=paste0("cancerinctotal_SQ26_",k,"[[i, j]]")))
  }
  
  assign(paste0("cancerinc_per100000_SQ26_2045_",k),cancerinc_per100000_SQ26_2045)
}

pop40_44_2021<-sum(population_estimate[41:45])
pop45_49_2021<-sum(population_estimate[36:40])
pop50_54_2021<-sum(population_estimate[31:35])
pop55_59_2021<-sum(population_estimate[26:30])
pop60_64_2021<-sum(population_estimate[21:25])
pop65_69_2021<-sum(population_estimate[16:20])
pop70_74_2021<-sum(population_estimate[11:15])
pop75_79_2021<-sum(population_estimate[6:10])
pop80_84_2021<-sum(population_estimate[1:5])

pop40_84_2021=sum(population_estimate[1:45])

pop40_44_2025<-sum(population_estimate[45:49])
pop45_49_2025<-sum(population_estimate[40:44])
pop50_54_2025<-sum(population_estimate[35:39])
pop55_59_2025<-sum(population_estimate[30:34])
pop60_64_2025<-sum(population_estimate[25:29])
pop65_69_2025<-sum(population_estimate[20:24])
pop70_74_2025<-sum(population_estimate[15:19])
pop75_79_2025<-sum(population_estimate[10:14])
pop80_84_2025<-sum(population_estimate[5:9])

pop40_84_2025=sum(population_estimate[5:49])

pop40_44_2030<-sum(population_estimate[50:54])
pop45_49_2030<-sum(population_estimate[45:49])
pop50_54_2030<-sum(population_estimate[40:44])
pop55_59_2030<-sum(population_estimate[35:39])
pop60_64_2030<-sum(population_estimate[30:34])
pop65_69_2030<-sum(population_estimate[25:29])
pop70_74_2030<-sum(population_estimate[20:24])
pop75_79_2030<-sum(population_estimate[15:19])
pop80_84_2030<-sum(population_estimate[10:14])

pop40_84_2030=sum(population_estimate[10:54])

pop40_44_2035<-sum(population_estimate[55:59])
pop45_49_2035<-sum(population_estimate[50:54])
pop50_54_2035<-sum(population_estimate[45:49])
pop55_59_2035<-sum(population_estimate[40:44])
pop60_64_2035<-sum(population_estimate[35:39])
pop65_69_2035<-sum(population_estimate[30:34])
pop70_74_2035<-sum(population_estimate[25:29])
pop75_79_2035<-sum(population_estimate[20:24])
pop80_84_2035<-sum(population_estimate[15:19])

pop40_84_2035=sum(population_estimate[15:59])

pop40_44_2040<-sum(population_estimate[60:64])
pop45_49_2040<-sum(population_estimate[55:59])
pop50_54_2040<-sum(population_estimate[50:54])
pop55_59_2040<-sum(population_estimate[45:49])
pop60_64_2040<-sum(population_estimate[40:44])
pop65_69_2040<-sum(population_estimate[35:39])
pop70_74_2040<-sum(population_estimate[30:34])
pop75_79_2040<-sum(population_estimate[25:29])
pop80_84_2040<-sum(population_estimate[20:24])

pop40_84_2040=sum(population_estimate[20:64])

pop40_44_2045<-sum(population_estimate[65:69])
pop45_49_2045<-sum(population_estimate[60:64])
pop50_54_2045<-sum(population_estimate[55:59])
pop55_59_2045<-sum(population_estimate[50:54])
pop60_64_2045<-sum(population_estimate[45:49])
pop65_69_2045<-sum(population_estimate[40:44])
pop70_74_2045<-sum(population_estimate[35:39])
pop75_79_2045<-sum(population_estimate[30:34])
pop80_84_2045<-sum(population_estimate[25:29])

pop40_84_2045=sum(population_estimate[25:69])

for (k in 1:50) {

  assign(paste0("cancerinc_per100000_SQ26_2021_40_44_",k), (((population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[41]")))) + (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[44]"))))+ (population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[45]")))))/pop40_44_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_45_49_",k), (((population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[36]")))) + (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[39]"))))+ (population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[40]")))))/pop45_49_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_50_54_",k), (((population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[31]")))) + (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[34]"))))+ (population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[35]")))))/pop50_54_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_55_59_",k), (((population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[26]")))) + (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[29]"))))+ (population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[30]")))))/pop55_59_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_60_64_",k), (((population_estimate[21]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[21]")))) + (population_estimate[22]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[22]"))))+ (population_estimate[23]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[23]"))))+ (population_estimate[24]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[24]"))))+ (population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[25]")))))/pop60_64_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_65_69_",k), (((population_estimate[16]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[16]")))) + (population_estimate[17]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[17]"))))+ (population_estimate[18]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[18]"))))+ (population_estimate[19]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[19]"))))+ (population_estimate[20]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[20]")))))/pop65_69_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_70_74_",k), (((population_estimate[11]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[11]")))) + (population_estimate[12]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[12]"))))+ (population_estimate[13]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[13]"))))+ (population_estimate[14]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[14]"))))+ (population_estimate[15]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[15]")))))/pop70_74_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_75_79_",k), (((population_estimate[6]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[6]")))) + (population_estimate[7]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[7]"))))+ (population_estimate[8]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[8]"))))+ (population_estimate[9]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[9]"))))+ (population_estimate[10]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[10]")))))/pop75_79_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_80_84_",k), (((population_estimate[1]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[1]")))) + (population_estimate[2]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[2]"))))+ (population_estimate[3]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[3]"))))+ (population_estimate[4]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[4]"))))+ (population_estimate[5]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_",k,"[5]")))))/pop80_84_2021))
  assign(paste0("cancerinc_per100000_SQ26_2021_40_84_",k), ((pop40_44_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))+(pop45_49_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))+
           (pop50_54_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))+(pop55_59_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))+
           (pop60_64_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))+(pop65_69_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))+
           (pop70_74_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))+(pop75_79_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))+
           (pop80_84_2021*eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))))/pop40_84_2021)
  
  assign(paste0("cancerinc_per100000_SQ26_2025_40_44_",k), (((population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[45]")))) + (population_estimate[46]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[46]"))))+ (population_estimate[47]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[47]"))))+ (population_estimate[48]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[48]"))))+ (population_estimate[49]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[49]")))))/pop40_44_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_45_49_",k), (((population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[40]")))) + (population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[41]"))))+ (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[44]")))))/pop45_49_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_50_54_",k), (((population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[35]")))) + (population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[36]"))))+ (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[39]")))))/pop50_54_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_55_59_",k), (((population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[30]")))) + (population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[31]"))))+ (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[34]")))))/pop55_59_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_60_64_",k), (((population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[25]")))) + (population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[26]"))))+ (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[29]")))))/pop60_64_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_65_69_",k), (((population_estimate[20]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[20]")))) + (population_estimate[21]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[21]"))))+ (population_estimate[22]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[22]"))))+ (population_estimate[23]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[23]"))))+ (population_estimate[24]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[24]")))))/pop65_69_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_70_74_",k), (((population_estimate[15]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[15]")))) + (population_estimate[16]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[16]"))))+ (population_estimate[17]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[17]"))))+ (population_estimate[18]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[18]"))))+ (population_estimate[19]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[19]")))))/pop70_74_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_75_79_",k), (((population_estimate[10]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[10]")))) + (population_estimate[11]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[11]"))))+ (population_estimate[12]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[12]"))))+ (population_estimate[13]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[13]"))))+ (population_estimate[14]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[14]")))))/pop75_79_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_80_84_",k), (((population_estimate[5]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[5]")))) + (population_estimate[6]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[6]"))))+ (population_estimate[7]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[7]"))))+ (population_estimate[8]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[8]"))))+ (population_estimate[9]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_",k,"[9]")))))/pop80_84_2025))
  assign(paste0("cancerinc_per100000_SQ26_2025_40_84_",k), ((pop40_44_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_40_44_",k))))+(pop45_49_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_45_49_",k))))+
           (pop50_54_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_50_54_",k))))+(pop55_59_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_55_59_",k))))+
           (pop60_64_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_60_64_",k))))+(pop65_69_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_65_69_",k))))+
           (pop70_74_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_70_74_",k))))+(pop75_79_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_75_79_",k))))+
           (pop80_84_2025*eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_80_84_",k)))))/pop40_84_2025)
  
  assign(paste0("cancerinc_per100000_SQ26_2030_40_44_",k), (((population_estimate[50]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[50]")))) + (population_estimate[51]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[51]"))))+ (population_estimate[52]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[52]"))))+ (population_estimate[53]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[53]"))))+ (population_estimate[54]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[54]")))))/pop40_44_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_45_49_",k), (((population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[45]")))) + (population_estimate[46]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[46]"))))+ (population_estimate[47]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[47]"))))+ (population_estimate[48]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[48]"))))+ (population_estimate[49]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[49]")))))/pop45_49_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_50_54_",k), (((population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[40]")))) + (population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[41]"))))+ (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[44]")))))/pop50_54_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_55_59_",k), (((population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[35]")))) + (population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[36]"))))+ (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[39]")))))/pop55_59_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_60_64_",k), (((population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[30]")))) + (population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[31]"))))+ (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[34]")))))/pop60_64_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_65_69_",k), (((population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[25]")))) + (population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[26]"))))+ (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[29]")))))/pop65_69_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_70_74_",k), (((population_estimate[20]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[20]")))) + (population_estimate[21]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[21]"))))+ (population_estimate[22]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[22]"))))+ (population_estimate[23]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[23]"))))+ (population_estimate[24]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[24]")))))/pop70_74_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_75_79_",k), (((population_estimate[15]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[15]")))) + (population_estimate[16]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[16]"))))+ (population_estimate[17]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[17]"))))+ (population_estimate[18]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[18]"))))+ (population_estimate[19]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[19]")))))/pop75_79_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_80_84_",k), (((population_estimate[10]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[10]")))) + (population_estimate[11]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[11]"))))+ (population_estimate[12]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[12]"))))+ (population_estimate[13]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[13]"))))+ (population_estimate[14]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_",k,"[14]")))))/pop80_84_2030))
  assign(paste0("cancerinc_per100000_SQ26_2030_40_84_",k), ((pop40_44_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_40_44_",k))))+(pop45_49_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_45_49_",k))))+
           (pop50_54_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_50_54_",k))))+(pop55_59_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_55_59_",k))))+
           (pop60_64_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_60_64_",k))))+(pop65_69_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_65_69_",k))))+
           (pop70_74_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_70_74_",k))))+(pop75_79_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_75_79_",k))))+
           (pop80_84_2030*eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_80_84_",k)))))/pop40_84_2030)
  
  assign(paste0("cancerinc_per100000_SQ26_2035_40_44_",k), (((population_estimate[55]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[55]")))) + (population_estimate[56]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[56]"))))+ (population_estimate[57]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[57]"))))+ (population_estimate[58]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[58]"))))+ (population_estimate[59]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[59]")))))/pop40_44_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_45_49_",k), (((population_estimate[50]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[50]")))) + (population_estimate[51]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[51]"))))+ (population_estimate[52]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[52]"))))+ (population_estimate[53]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[53]"))))+ (population_estimate[54]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[54]")))))/pop45_49_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_50_54_",k), (((population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[45]")))) + (population_estimate[46]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[46]"))))+ (population_estimate[47]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[47]"))))+ (population_estimate[48]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[48]"))))+ (population_estimate[49]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[49]")))))/pop50_54_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_55_59_",k), (((population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[40]")))) + (population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[41]"))))+ (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[44]")))))/pop55_59_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_60_64_",k), (((population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[35]")))) + (population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[36]"))))+ (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[39]")))))/pop60_64_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_65_69_",k), (((population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[30]")))) + (population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[31]"))))+ (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[34]")))))/pop65_69_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_70_74_",k), (((population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[25]")))) + (population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[26]"))))+ (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[29]")))))/pop70_74_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_75_79_",k), (((population_estimate[20]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[20]")))) + (population_estimate[21]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[21]"))))+ (population_estimate[22]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[22]"))))+ (population_estimate[23]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[23]"))))+ (population_estimate[24]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[24]")))))/pop75_79_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_80_84_",k), (((population_estimate[15]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[15]")))) + (population_estimate[16]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[16]"))))+ (population_estimate[17]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[17]"))))+ (population_estimate[18]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[18]"))))+ (population_estimate[19]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_",k,"[19]")))))/pop80_84_2035))
  assign(paste0("cancerinc_per100000_SQ26_2035_40_84_",k), ((pop40_44_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_40_44_",k))))+(pop45_49_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_45_49_",k))))+
           (pop50_54_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_50_54_",k))))+(pop55_59_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_55_59_",k))))+
           (pop60_64_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_60_64_",k))))+(pop65_69_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_65_69_",k))))+
           (pop70_74_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_70_74_",k))))+(pop75_79_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_75_79_",k))))+
           (pop80_84_2035*eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_80_84_",k)))))/pop40_84_2035)
  
  assign(paste0("cancerinc_per100000_SQ26_2040_40_44_",k), (((population_estimate[60]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[60]")))) + (population_estimate[61]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[61]"))))+ (population_estimate[62]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[62]"))))+ (population_estimate[63]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[63]"))))+ (population_estimate[64]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[64]")))))/pop40_44_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_45_49_",k), (((population_estimate[55]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[55]")))) + (population_estimate[56]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[56]"))))+ (population_estimate[57]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[57]"))))+ (population_estimate[58]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[58]"))))+ (population_estimate[59]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[59]")))))/pop45_49_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_50_54_",k), (((population_estimate[50]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[50]")))) + (population_estimate[51]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[51]"))))+ (population_estimate[52]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[52]"))))+ (population_estimate[53]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[53]"))))+ (population_estimate[54]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[54]")))))/pop50_54_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_55_59_",k), (((population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[45]")))) + (population_estimate[46]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[46]"))))+ (population_estimate[47]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[47]"))))+ (population_estimate[48]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[48]"))))+ (population_estimate[49]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[49]")))))/pop55_59_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_60_64_",k), (((population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[40]")))) + (population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[41]"))))+ (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[44]")))))/pop60_64_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_65_69_",k), (((population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[35]")))) + (population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[36]"))))+ (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[39]")))))/pop65_69_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_70_74_",k), (((population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[30]")))) + (population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[31]"))))+ (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[34]")))))/pop70_74_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_75_79_",k), (((population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[25]")))) + (population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[26]"))))+ (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[29]")))))/pop75_79_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_80_84_",k), (((population_estimate[20]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[20]")))) + (population_estimate[21]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[21]"))))+ (population_estimate[22]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[22]"))))+ (population_estimate[23]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[23]"))))+ (population_estimate[24]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_",k,"[24]")))))/pop80_84_2040))
  assign(paste0("cancerinc_per100000_SQ26_2040_40_84_",k), ((pop40_44_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_40_44_",k))))+(pop45_49_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_45_49_",k))))+
           (pop50_54_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_50_54_",k))))+(pop55_59_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_55_59_",k))))+
           (pop60_64_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_60_64_",k))))+(pop65_69_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_65_69_",k))))+
           (pop70_74_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_70_74_",k))))+(pop75_79_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_75_79_",k))))+
           (pop80_84_2040*eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_80_84_",k)))))/pop40_84_2040)
  
  assign(paste0("cancerinc_per100000_SQ26_2045_40_44_",k), (((population_estimate[65]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[65]")))) + (population_estimate[66]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[66]"))))+ (population_estimate[67]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[67]"))))+ (population_estimate[68]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[68]"))))+ (population_estimate[69]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[69]")))))/pop40_44_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_45_49_",k), (((population_estimate[60]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[60]")))) + (population_estimate[61]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[61]"))))+ (population_estimate[62]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[62]"))))+ (population_estimate[63]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[63]"))))+ (population_estimate[64]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[64]")))))/pop45_49_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_50_54_",k), (((population_estimate[55]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[55]")))) + (population_estimate[56]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[56]"))))+ (population_estimate[57]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[57]"))))+ (population_estimate[58]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[58]"))))+ (population_estimate[59]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[59]")))))/pop50_54_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_55_59_",k), (((population_estimate[50]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[50]")))) + (population_estimate[51]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[51]"))))+ (population_estimate[52]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[52]"))))+ (population_estimate[53]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[53]"))))+ (population_estimate[54]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[54]")))))/pop55_59_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_60_64_",k), (((population_estimate[45]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[45]")))) + (population_estimate[46]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[46]"))))+ (population_estimate[47]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[47]"))))+ (population_estimate[48]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[48]"))))+ (population_estimate[49]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[49]")))))/pop60_64_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_65_69_",k), (((population_estimate[40]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[40]")))) + (population_estimate[41]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[41]"))))+ (population_estimate[42]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[42]"))))+ (population_estimate[43]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[43]"))))+ (population_estimate[44]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[44]")))))/pop65_69_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_70_74_",k), (((population_estimate[35]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[35]")))) + (population_estimate[36]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[36]"))))+ (population_estimate[37]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[37]"))))+ (population_estimate[38]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[38]"))))+ (population_estimate[39]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[39]")))))/pop70_74_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_75_79_",k), (((population_estimate[30]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[30]")))) + (population_estimate[31]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[31]"))))+ (population_estimate[32]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[32]"))))+ (population_estimate[33]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[33]"))))+ (population_estimate[34]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[34]")))))/pop75_79_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_80_84_",k), (((population_estimate[25]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[25]")))) + (population_estimate[26]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[26]"))))+ (population_estimate[27]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[27]"))))+ (population_estimate[28]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[28]"))))+ (population_estimate[29]*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_",k,"[29]")))))/pop80_84_2045))
  assign(paste0("cancerinc_per100000_SQ26_2045_40_84_",k), ((pop40_44_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_40_44_",k))))+(pop45_49_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_45_49_",k))))+
           (pop50_54_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_50_54_",k))))+(pop55_59_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_55_59_",k))))+
           (pop60_64_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_60_64_",k))))+(pop65_69_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_65_69_",k))))+
           (pop70_74_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_70_74_",k))))+(pop75_79_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_75_79_",k))))+
           (pop80_84_2045*eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_80_84_",k)))))/pop40_84_2045)

#####
  assign(paste0("percent_reduction_inc_2025_40_44_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_40_44_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))
  assign(paste0("percent_reduction_inc_2030_40_44_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_40_44_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))
  assign(paste0("percent_reduction_inc_2035_40_44_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_40_44_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))
  assign(paste0("percent_reduction_inc_2040_40_44_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_40_44_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))
  assign(paste0("percent_reduction_inc_2045_40_44_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_40_44_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_44_",k))))
  
  assign(paste0("percent_reduction_inc_2025_45_49_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_45_49_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))
  assign(paste0("percent_reduction_inc_2030_45_49_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_45_49_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))
  assign(paste0("percent_reduction_inc_2035_45_49_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_45_49_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))
  assign(paste0("percent_reduction_inc_2040_45_49_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_45_49_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))
  assign(paste0("percent_reduction_inc_2045_45_49_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_45_49_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_45_49_",k))))
  
  assign(paste0("percent_reduction_inc_2025_50_54_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_50_54_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))
  assign(paste0("percent_reduction_inc_2030_50_54_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_50_54_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))
  assign(paste0("percent_reduction_inc_2035_50_54_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_50_54_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))
  assign(paste0("percent_reduction_inc_2040_50_54_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_50_54_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))
  assign(paste0("percent_reduction_inc_2045_50_54_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_50_54_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_50_54_",k))))
  
  assign(paste0("percent_reduction_inc_2025_55_59_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_55_59_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))
  assign(paste0("percent_reduction_inc_2030_55_59_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_55_59_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))
  assign(paste0("percent_reduction_inc_2035_55_59_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_55_59_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))
  assign(paste0("percent_reduction_inc_2040_55_59_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_55_59_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))
  assign(paste0("percent_reduction_inc_2045_55_59_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_55_59_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_55_59_",k))))
  
  assign(paste0("percent_reduction_inc_2025_60_64_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_60_64_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))
  assign(paste0("percent_reduction_inc_2030_60_64_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_60_64_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))
  assign(paste0("percent_reduction_inc_2035_60_64_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_60_64_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))
  assign(paste0("percent_reduction_inc_2040_60_64_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_60_64_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))
  assign(paste0("percent_reduction_inc_2045_60_64_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_60_64_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_60_64_",k))))
  
  assign(paste0("percent_reduction_inc_2025_65_69_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_65_69_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))
  assign(paste0("percent_reduction_inc_2030_65_69_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_65_69_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))
  assign(paste0("percent_reduction_inc_2035_65_69_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_65_69_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))
  assign(paste0("percent_reduction_inc_2040_65_69_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_65_69_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))
  assign(paste0("percent_reduction_inc_2045_65_69_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_65_69_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_65_69_",k))))
  
  assign(paste0("percent_reduction_inc_2025_70_74_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_70_74_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))
  assign(paste0("percent_reduction_inc_2030_70_74_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_70_74_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))
  assign(paste0("percent_reduction_inc_2035_70_74_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_70_74_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))
  assign(paste0("percent_reduction_inc_2040_70_74_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_70_74_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))
  assign(paste0("percent_reduction_inc_2045_70_74_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_70_74_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_70_74_",k))))
  
  assign(paste0("percent_reduction_inc_2025_75_79_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_75_79_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))
  assign(paste0("percent_reduction_inc_2030_75_79_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_75_79_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))
  assign(paste0("percent_reduction_inc_2035_75_79_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_75_79_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))
  assign(paste0("percent_reduction_inc_2040_75_79_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_75_79_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))
  assign(paste0("percent_reduction_inc_2045_75_79_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_75_79_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_75_79_",k))))
  
  assign(paste0("percent_reduction_inc_2025_80_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_80_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k))))
  assign(paste0("percent_reduction_inc_2030_80_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_80_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k))))
  assign(paste0("percent_reduction_inc_2035_80_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_80_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k))))
  assign(paste0("percent_reduction_inc_2040_80_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_80_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k))))
  assign(paste0("percent_reduction_inc_2045_80_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_80_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_80_84_",k))))
  
  assign(paste0("percent_reduction_inc_2025_40_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2025_40_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k))))
  assign(paste0("percent_reduction_inc_2030_40_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2030_40_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k))))
  assign(paste0("percent_reduction_inc_2035_40_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2035_40_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k))))
  assign(paste0("percent_reduction_inc_2040_40_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2040_40_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k))))
  assign(paste0("percent_reduction_inc_2045_40_84_",k), (eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k)))-eval(parse(text=paste0("cancerinc_per100000_SQ26_2045_40_84_",k))))/eval(parse(text=paste0("cancerinc_per100000_SQ26_2021_40_84_",k))))

}
#####
percent_reduction_inc_2025_40_44<-c(percent_reduction_inc_2025_40_44_1,percent_reduction_inc_2025_40_44_2,percent_reduction_inc_2025_40_44_3,percent_reduction_inc_2025_40_44_4,percent_reduction_inc_2025_40_44_5,
                                percent_reduction_inc_2025_40_44_6,percent_reduction_inc_2025_40_44_7,percent_reduction_inc_2025_40_44_8,percent_reduction_inc_2025_40_44_9,percent_reduction_inc_2025_40_44_10,
                                percent_reduction_inc_2025_40_44_11,percent_reduction_inc_2025_40_44_12,percent_reduction_inc_2025_40_44_13,percent_reduction_inc_2025_40_44_14,percent_reduction_inc_2025_40_44_15,
                                percent_reduction_inc_2025_40_44_16,percent_reduction_inc_2025_40_44_17,percent_reduction_inc_2025_40_44_18,percent_reduction_inc_2025_40_44_19,percent_reduction_inc_2025_40_44_20,
                                percent_reduction_inc_2025_40_44_21,percent_reduction_inc_2025_40_44_22,percent_reduction_inc_2025_40_44_23,percent_reduction_inc_2025_40_44_24,percent_reduction_inc_2025_40_44_25,
                                percent_reduction_inc_2025_40_44_26,percent_reduction_inc_2025_40_44_27,percent_reduction_inc_2025_40_44_28,percent_reduction_inc_2025_40_44_29,percent_reduction_inc_2025_40_44_30,
                                percent_reduction_inc_2025_40_44_31,percent_reduction_inc_2025_40_44_32,percent_reduction_inc_2025_40_44_33,percent_reduction_inc_2025_40_44_34,percent_reduction_inc_2025_40_44_35,
                                percent_reduction_inc_2025_40_44_36,percent_reduction_inc_2025_40_44_37,percent_reduction_inc_2025_40_44_38,percent_reduction_inc_2025_40_44_39,percent_reduction_inc_2025_40_44_40,
                                percent_reduction_inc_2025_40_44_41,percent_reduction_inc_2025_40_44_42,percent_reduction_inc_2025_40_44_43,percent_reduction_inc_2025_40_44_44,percent_reduction_inc_2025_40_44_45,
                                percent_reduction_inc_2025_40_44_46,percent_reduction_inc_2025_40_44_47,percent_reduction_inc_2025_40_44_48,percent_reduction_inc_2025_40_44_49,percent_reduction_inc_2025_40_44_50)

percent_reduction_inc_2030_40_44<-c(percent_reduction_inc_2030_40_44_1,percent_reduction_inc_2030_40_44_2,percent_reduction_inc_2030_40_44_3,percent_reduction_inc_2030_40_44_4,percent_reduction_inc_2030_40_44_5,
                                percent_reduction_inc_2030_40_44_6,percent_reduction_inc_2030_40_44_7,percent_reduction_inc_2030_40_44_8,percent_reduction_inc_2030_40_44_9,percent_reduction_inc_2030_40_44_10,
                                percent_reduction_inc_2030_40_44_11,percent_reduction_inc_2030_40_44_12,percent_reduction_inc_2030_40_44_13,percent_reduction_inc_2030_40_44_14,percent_reduction_inc_2030_40_44_15,
                                percent_reduction_inc_2030_40_44_16,percent_reduction_inc_2030_40_44_17,percent_reduction_inc_2030_40_44_18,percent_reduction_inc_2030_40_44_19,percent_reduction_inc_2030_40_44_20,
                                percent_reduction_inc_2030_40_44_21,percent_reduction_inc_2030_40_44_22,percent_reduction_inc_2030_40_44_23,percent_reduction_inc_2030_40_44_24,percent_reduction_inc_2030_40_44_25,
                                percent_reduction_inc_2030_40_44_26,percent_reduction_inc_2030_40_44_27,percent_reduction_inc_2030_40_44_28,percent_reduction_inc_2030_40_44_29,percent_reduction_inc_2030_40_44_30,
                                percent_reduction_inc_2030_40_44_31,percent_reduction_inc_2030_40_44_32,percent_reduction_inc_2030_40_44_33,percent_reduction_inc_2030_40_44_34,percent_reduction_inc_2030_40_44_35,
                                percent_reduction_inc_2030_40_44_36,percent_reduction_inc_2030_40_44_37,percent_reduction_inc_2030_40_44_38,percent_reduction_inc_2030_40_44_39,percent_reduction_inc_2030_40_44_40,
                                percent_reduction_inc_2030_40_44_41,percent_reduction_inc_2030_40_44_42,percent_reduction_inc_2030_40_44_43,percent_reduction_inc_2030_40_44_44,percent_reduction_inc_2030_40_44_45,
                                percent_reduction_inc_2030_40_44_46,percent_reduction_inc_2030_40_44_47,percent_reduction_inc_2030_40_44_48,percent_reduction_inc_2030_40_44_49,percent_reduction_inc_2030_40_44_50)

percent_reduction_inc_2035_40_44<-c(percent_reduction_inc_2035_40_44_1,percent_reduction_inc_2035_40_44_2,percent_reduction_inc_2035_40_44_3,percent_reduction_inc_2035_40_44_4,percent_reduction_inc_2035_40_44_5,
                                percent_reduction_inc_2035_40_44_6,percent_reduction_inc_2035_40_44_7,percent_reduction_inc_2035_40_44_8,percent_reduction_inc_2035_40_44_9,percent_reduction_inc_2035_40_44_10,
                                percent_reduction_inc_2035_40_44_11,percent_reduction_inc_2035_40_44_12,percent_reduction_inc_2035_40_44_13,percent_reduction_inc_2035_40_44_14,percent_reduction_inc_2035_40_44_15,
                                percent_reduction_inc_2035_40_44_16,percent_reduction_inc_2035_40_44_17,percent_reduction_inc_2035_40_44_18,percent_reduction_inc_2035_40_44_19,percent_reduction_inc_2035_40_44_20,
                                percent_reduction_inc_2035_40_44_21,percent_reduction_inc_2035_40_44_22,percent_reduction_inc_2035_40_44_23,percent_reduction_inc_2035_40_44_24,percent_reduction_inc_2035_40_44_25,
                                percent_reduction_inc_2035_40_44_26,percent_reduction_inc_2035_40_44_27,percent_reduction_inc_2035_40_44_28,percent_reduction_inc_2035_40_44_29,percent_reduction_inc_2035_40_44_30,
                                percent_reduction_inc_2035_40_44_31,percent_reduction_inc_2035_40_44_32,percent_reduction_inc_2035_40_44_33,percent_reduction_inc_2035_40_44_34,percent_reduction_inc_2035_40_44_35,
                                percent_reduction_inc_2035_40_44_36,percent_reduction_inc_2035_40_44_37,percent_reduction_inc_2035_40_44_38,percent_reduction_inc_2035_40_44_39,percent_reduction_inc_2035_40_44_40,
                                percent_reduction_inc_2035_40_44_41,percent_reduction_inc_2035_40_44_42,percent_reduction_inc_2035_40_44_43,percent_reduction_inc_2035_40_44_44,percent_reduction_inc_2035_40_44_45,
                                percent_reduction_inc_2035_40_44_46,percent_reduction_inc_2035_40_44_47,percent_reduction_inc_2035_40_44_48,percent_reduction_inc_2035_40_44_49,percent_reduction_inc_2035_40_44_50)

percent_reduction_inc_2040_40_44<-c(percent_reduction_inc_2040_40_44_1,percent_reduction_inc_2040_40_44_2,percent_reduction_inc_2040_40_44_3,percent_reduction_inc_2040_40_44_4,percent_reduction_inc_2040_40_44_5,
                                percent_reduction_inc_2040_40_44_6,percent_reduction_inc_2040_40_44_7,percent_reduction_inc_2040_40_44_8,percent_reduction_inc_2040_40_44_9,percent_reduction_inc_2040_40_44_10,
                                percent_reduction_inc_2040_40_44_11,percent_reduction_inc_2040_40_44_12,percent_reduction_inc_2040_40_44_13,percent_reduction_inc_2040_40_44_14,percent_reduction_inc_2040_40_44_15,
                                percent_reduction_inc_2040_40_44_16,percent_reduction_inc_2040_40_44_17,percent_reduction_inc_2040_40_44_18,percent_reduction_inc_2040_40_44_19,percent_reduction_inc_2040_40_44_20,
                                percent_reduction_inc_2040_40_44_21,percent_reduction_inc_2040_40_44_22,percent_reduction_inc_2040_40_44_23,percent_reduction_inc_2040_40_44_24,percent_reduction_inc_2040_40_44_25,
                                percent_reduction_inc_2040_40_44_26,percent_reduction_inc_2040_40_44_27,percent_reduction_inc_2040_40_44_28,percent_reduction_inc_2040_40_44_29,percent_reduction_inc_2040_40_44_30,
                                percent_reduction_inc_2040_40_44_31,percent_reduction_inc_2040_40_44_32,percent_reduction_inc_2040_40_44_33,percent_reduction_inc_2040_40_44_34,percent_reduction_inc_2040_40_44_35,
                                percent_reduction_inc_2040_40_44_36,percent_reduction_inc_2040_40_44_37,percent_reduction_inc_2040_40_44_38,percent_reduction_inc_2040_40_44_39,percent_reduction_inc_2040_40_44_40,
                                percent_reduction_inc_2040_40_44_41,percent_reduction_inc_2040_40_44_42,percent_reduction_inc_2040_40_44_43,percent_reduction_inc_2040_40_44_44,percent_reduction_inc_2040_40_44_45,
                                percent_reduction_inc_2040_40_44_46,percent_reduction_inc_2040_40_44_47,percent_reduction_inc_2040_40_44_48,percent_reduction_inc_2040_40_44_49,percent_reduction_inc_2040_40_44_50)

percent_reduction_inc_2045_40_44<-c(percent_reduction_inc_2045_40_44_1,percent_reduction_inc_2045_40_44_2,percent_reduction_inc_2045_40_44_3,percent_reduction_inc_2045_40_44_4,percent_reduction_inc_2045_40_44_5,
                                percent_reduction_inc_2045_40_44_6,percent_reduction_inc_2045_40_44_7,percent_reduction_inc_2045_40_44_8,percent_reduction_inc_2045_40_44_9,percent_reduction_inc_2045_40_44_10,
                                percent_reduction_inc_2045_40_44_11,percent_reduction_inc_2045_40_44_12,percent_reduction_inc_2045_40_44_13,percent_reduction_inc_2045_40_44_14,percent_reduction_inc_2045_40_44_15,
                                percent_reduction_inc_2045_40_44_16,percent_reduction_inc_2045_40_44_17,percent_reduction_inc_2045_40_44_18,percent_reduction_inc_2045_40_44_19,percent_reduction_inc_2045_40_44_20,
                                percent_reduction_inc_2045_40_44_21,percent_reduction_inc_2045_40_44_22,percent_reduction_inc_2045_40_44_23,percent_reduction_inc_2045_40_44_24,percent_reduction_inc_2045_40_44_25,
                                percent_reduction_inc_2045_40_44_26,percent_reduction_inc_2045_40_44_27,percent_reduction_inc_2045_40_44_28,percent_reduction_inc_2045_40_44_29,percent_reduction_inc_2045_40_44_30,
                                percent_reduction_inc_2045_40_44_31,percent_reduction_inc_2045_40_44_32,percent_reduction_inc_2045_40_44_33,percent_reduction_inc_2045_40_44_34,percent_reduction_inc_2045_40_44_35,
                                percent_reduction_inc_2045_40_44_36,percent_reduction_inc_2045_40_44_37,percent_reduction_inc_2045_40_44_38,percent_reduction_inc_2045_40_44_39,percent_reduction_inc_2045_40_44_40,
                                percent_reduction_inc_2045_40_44_41,percent_reduction_inc_2045_40_44_42,percent_reduction_inc_2045_40_44_43,percent_reduction_inc_2045_40_44_44,percent_reduction_inc_2045_40_44_45,
                                percent_reduction_inc_2045_40_44_46,percent_reduction_inc_2045_40_44_47,percent_reduction_inc_2045_40_44_48,percent_reduction_inc_2045_40_44_49,percent_reduction_inc_2045_40_44_50)

#####
percent_reduction_inc_2025_45_49<-c(percent_reduction_inc_2025_45_49_1,percent_reduction_inc_2025_45_49_2,percent_reduction_inc_2025_45_49_3,percent_reduction_inc_2025_45_49_4,percent_reduction_inc_2025_45_49_5,
                                percent_reduction_inc_2025_45_49_6,percent_reduction_inc_2025_45_49_7,percent_reduction_inc_2025_45_49_8,percent_reduction_inc_2025_45_49_9,percent_reduction_inc_2025_45_49_10,
                                percent_reduction_inc_2025_45_49_11,percent_reduction_inc_2025_45_49_12,percent_reduction_inc_2025_45_49_13,percent_reduction_inc_2025_45_49_14,percent_reduction_inc_2025_45_49_15,
                                percent_reduction_inc_2025_45_49_16,percent_reduction_inc_2025_45_49_17,percent_reduction_inc_2025_45_49_18,percent_reduction_inc_2025_45_49_19,percent_reduction_inc_2025_45_49_20,
                                percent_reduction_inc_2025_45_49_21,percent_reduction_inc_2025_45_49_22,percent_reduction_inc_2025_45_49_23,percent_reduction_inc_2025_45_49_24,percent_reduction_inc_2025_45_49_25,
                                percent_reduction_inc_2025_45_49_26,percent_reduction_inc_2025_45_49_27,percent_reduction_inc_2025_45_49_28,percent_reduction_inc_2025_45_49_29,percent_reduction_inc_2025_45_49_30,
                                percent_reduction_inc_2025_45_49_31,percent_reduction_inc_2025_45_49_32,percent_reduction_inc_2025_45_49_33,percent_reduction_inc_2025_45_49_34,percent_reduction_inc_2025_45_49_35,
                                percent_reduction_inc_2025_45_49_36,percent_reduction_inc_2025_45_49_37,percent_reduction_inc_2025_45_49_38,percent_reduction_inc_2025_45_49_39,percent_reduction_inc_2025_45_49_40,
                                percent_reduction_inc_2025_45_49_41,percent_reduction_inc_2025_45_49_42,percent_reduction_inc_2025_45_49_43,percent_reduction_inc_2025_45_49_44,percent_reduction_inc_2025_45_49_45,
                                percent_reduction_inc_2025_45_49_46,percent_reduction_inc_2025_45_49_47,percent_reduction_inc_2025_45_49_48,percent_reduction_inc_2025_45_49_49,percent_reduction_inc_2025_45_49_50)

percent_reduction_inc_2030_45_49<-c(percent_reduction_inc_2030_45_49_1,percent_reduction_inc_2030_45_49_2,percent_reduction_inc_2030_45_49_3,percent_reduction_inc_2030_45_49_4,percent_reduction_inc_2030_45_49_5,
                                percent_reduction_inc_2030_45_49_6,percent_reduction_inc_2030_45_49_7,percent_reduction_inc_2030_45_49_8,percent_reduction_inc_2030_45_49_9,percent_reduction_inc_2030_45_49_10,
                                percent_reduction_inc_2030_45_49_11,percent_reduction_inc_2030_45_49_12,percent_reduction_inc_2030_45_49_13,percent_reduction_inc_2030_45_49_14,percent_reduction_inc_2030_45_49_15,
                                percent_reduction_inc_2030_45_49_16,percent_reduction_inc_2030_45_49_17,percent_reduction_inc_2030_45_49_18,percent_reduction_inc_2030_45_49_19,percent_reduction_inc_2030_45_49_20,
                                percent_reduction_inc_2030_45_49_21,percent_reduction_inc_2030_45_49_22,percent_reduction_inc_2030_45_49_23,percent_reduction_inc_2030_45_49_24,percent_reduction_inc_2030_45_49_25,
                                percent_reduction_inc_2030_45_49_26,percent_reduction_inc_2030_45_49_27,percent_reduction_inc_2030_45_49_28,percent_reduction_inc_2030_45_49_29,percent_reduction_inc_2030_45_49_30,
                                percent_reduction_inc_2030_45_49_31,percent_reduction_inc_2030_45_49_32,percent_reduction_inc_2030_45_49_33,percent_reduction_inc_2030_45_49_34,percent_reduction_inc_2030_45_49_35,
                                percent_reduction_inc_2030_45_49_36,percent_reduction_inc_2030_45_49_37,percent_reduction_inc_2030_45_49_38,percent_reduction_inc_2030_45_49_39,percent_reduction_inc_2030_45_49_40,
                                percent_reduction_inc_2030_45_49_41,percent_reduction_inc_2030_45_49_42,percent_reduction_inc_2030_45_49_43,percent_reduction_inc_2030_45_49_44,percent_reduction_inc_2030_45_49_45,
                                percent_reduction_inc_2030_45_49_46,percent_reduction_inc_2030_45_49_47,percent_reduction_inc_2030_45_49_48,percent_reduction_inc_2030_45_49_49,percent_reduction_inc_2030_45_49_50)

percent_reduction_inc_2035_45_49<-c(percent_reduction_inc_2035_45_49_1,percent_reduction_inc_2035_45_49_2,percent_reduction_inc_2035_45_49_3,percent_reduction_inc_2035_45_49_4,percent_reduction_inc_2035_45_49_5,
                                percent_reduction_inc_2035_45_49_6,percent_reduction_inc_2035_45_49_7,percent_reduction_inc_2035_45_49_8,percent_reduction_inc_2035_45_49_9,percent_reduction_inc_2035_45_49_10,
                                percent_reduction_inc_2035_45_49_11,percent_reduction_inc_2035_45_49_12,percent_reduction_inc_2035_45_49_13,percent_reduction_inc_2035_45_49_14,percent_reduction_inc_2035_45_49_15,
                                percent_reduction_inc_2035_45_49_16,percent_reduction_inc_2035_45_49_17,percent_reduction_inc_2035_45_49_18,percent_reduction_inc_2035_45_49_19,percent_reduction_inc_2035_45_49_20,
                                percent_reduction_inc_2035_45_49_21,percent_reduction_inc_2035_45_49_22,percent_reduction_inc_2035_45_49_23,percent_reduction_inc_2035_45_49_24,percent_reduction_inc_2035_45_49_25,
                                percent_reduction_inc_2035_45_49_26,percent_reduction_inc_2035_45_49_27,percent_reduction_inc_2035_45_49_28,percent_reduction_inc_2035_45_49_29,percent_reduction_inc_2035_45_49_30,
                                percent_reduction_inc_2035_45_49_31,percent_reduction_inc_2035_45_49_32,percent_reduction_inc_2035_45_49_33,percent_reduction_inc_2035_45_49_34,percent_reduction_inc_2035_45_49_35,
                                percent_reduction_inc_2035_45_49_36,percent_reduction_inc_2035_45_49_37,percent_reduction_inc_2035_45_49_38,percent_reduction_inc_2035_45_49_39,percent_reduction_inc_2035_45_49_40,
                                percent_reduction_inc_2035_45_49_41,percent_reduction_inc_2035_45_49_42,percent_reduction_inc_2035_45_49_43,percent_reduction_inc_2035_45_49_44,percent_reduction_inc_2035_45_49_45,
                                percent_reduction_inc_2035_45_49_46,percent_reduction_inc_2035_45_49_47,percent_reduction_inc_2035_45_49_48,percent_reduction_inc_2035_45_49_49,percent_reduction_inc_2035_45_49_50)

percent_reduction_inc_2040_45_49<-c(percent_reduction_inc_2040_45_49_1,percent_reduction_inc_2040_45_49_2,percent_reduction_inc_2040_45_49_3,percent_reduction_inc_2040_45_49_4,percent_reduction_inc_2040_45_49_5,
                                percent_reduction_inc_2040_45_49_6,percent_reduction_inc_2040_45_49_7,percent_reduction_inc_2040_45_49_8,percent_reduction_inc_2040_45_49_9,percent_reduction_inc_2040_45_49_10,
                                percent_reduction_inc_2040_45_49_11,percent_reduction_inc_2040_45_49_12,percent_reduction_inc_2040_45_49_13,percent_reduction_inc_2040_45_49_14,percent_reduction_inc_2040_45_49_15,
                                percent_reduction_inc_2040_45_49_16,percent_reduction_inc_2040_45_49_17,percent_reduction_inc_2040_45_49_18,percent_reduction_inc_2040_45_49_19,percent_reduction_inc_2040_45_49_20,
                                percent_reduction_inc_2040_45_49_21,percent_reduction_inc_2040_45_49_22,percent_reduction_inc_2040_45_49_23,percent_reduction_inc_2040_45_49_24,percent_reduction_inc_2040_45_49_25,
                                percent_reduction_inc_2040_45_49_26,percent_reduction_inc_2040_45_49_27,percent_reduction_inc_2040_45_49_28,percent_reduction_inc_2040_45_49_29,percent_reduction_inc_2040_45_49_30,
                                percent_reduction_inc_2040_45_49_31,percent_reduction_inc_2040_45_49_32,percent_reduction_inc_2040_45_49_33,percent_reduction_inc_2040_45_49_34,percent_reduction_inc_2040_45_49_35,
                                percent_reduction_inc_2040_45_49_36,percent_reduction_inc_2040_45_49_37,percent_reduction_inc_2040_45_49_38,percent_reduction_inc_2040_45_49_39,percent_reduction_inc_2040_45_49_40,
                                percent_reduction_inc_2040_45_49_41,percent_reduction_inc_2040_45_49_42,percent_reduction_inc_2040_45_49_43,percent_reduction_inc_2040_45_49_44,percent_reduction_inc_2040_45_49_45,
                                percent_reduction_inc_2040_45_49_46,percent_reduction_inc_2040_45_49_47,percent_reduction_inc_2040_45_49_48,percent_reduction_inc_2040_45_49_49,percent_reduction_inc_2040_45_49_50)

percent_reduction_inc_2045_45_49<-c(percent_reduction_inc_2045_45_49_1,percent_reduction_inc_2045_45_49_2,percent_reduction_inc_2045_45_49_3,percent_reduction_inc_2045_45_49_4,percent_reduction_inc_2045_45_49_5,
                                percent_reduction_inc_2045_45_49_6,percent_reduction_inc_2045_45_49_7,percent_reduction_inc_2045_45_49_8,percent_reduction_inc_2045_45_49_9,percent_reduction_inc_2045_45_49_10,
                                percent_reduction_inc_2045_45_49_11,percent_reduction_inc_2045_45_49_12,percent_reduction_inc_2045_45_49_13,percent_reduction_inc_2045_45_49_14,percent_reduction_inc_2045_45_49_15,
                                percent_reduction_inc_2045_45_49_16,percent_reduction_inc_2045_45_49_17,percent_reduction_inc_2045_45_49_18,percent_reduction_inc_2045_45_49_19,percent_reduction_inc_2045_45_49_20,
                                percent_reduction_inc_2045_45_49_21,percent_reduction_inc_2045_45_49_22,percent_reduction_inc_2045_45_49_23,percent_reduction_inc_2045_45_49_24,percent_reduction_inc_2045_45_49_25,
                                percent_reduction_inc_2045_45_49_26,percent_reduction_inc_2045_45_49_27,percent_reduction_inc_2045_45_49_28,percent_reduction_inc_2045_45_49_29,percent_reduction_inc_2045_45_49_30,
                                percent_reduction_inc_2045_45_49_31,percent_reduction_inc_2045_45_49_32,percent_reduction_inc_2045_45_49_33,percent_reduction_inc_2045_45_49_34,percent_reduction_inc_2045_45_49_35,
                                percent_reduction_inc_2045_45_49_36,percent_reduction_inc_2045_45_49_37,percent_reduction_inc_2045_45_49_38,percent_reduction_inc_2045_45_49_39,percent_reduction_inc_2045_45_49_40,
                                percent_reduction_inc_2045_45_49_41,percent_reduction_inc_2045_45_49_42,percent_reduction_inc_2045_45_49_43,percent_reduction_inc_2045_45_49_44,percent_reduction_inc_2045_45_49_45,
                                percent_reduction_inc_2045_45_49_46,percent_reduction_inc_2045_45_49_47,percent_reduction_inc_2045_45_49_48,percent_reduction_inc_2045_45_49_49,percent_reduction_inc_2045_45_49_50)

#####
percent_reduction_inc_2025_50_54<-c(percent_reduction_inc_2025_50_54_1,percent_reduction_inc_2025_50_54_2,percent_reduction_inc_2025_50_54_3,percent_reduction_inc_2025_50_54_4,percent_reduction_inc_2025_50_54_5,
                                percent_reduction_inc_2025_50_54_6,percent_reduction_inc_2025_50_54_7,percent_reduction_inc_2025_50_54_8,percent_reduction_inc_2025_50_54_9,percent_reduction_inc_2025_50_54_10,
                                percent_reduction_inc_2025_50_54_11,percent_reduction_inc_2025_50_54_12,percent_reduction_inc_2025_50_54_13,percent_reduction_inc_2025_50_54_14,percent_reduction_inc_2025_50_54_15,
                                percent_reduction_inc_2025_50_54_16,percent_reduction_inc_2025_50_54_17,percent_reduction_inc_2025_50_54_18,percent_reduction_inc_2025_50_54_19,percent_reduction_inc_2025_50_54_20,
                                percent_reduction_inc_2025_50_54_21,percent_reduction_inc_2025_50_54_22,percent_reduction_inc_2025_50_54_23,percent_reduction_inc_2025_50_54_24,percent_reduction_inc_2025_50_54_25,
                                percent_reduction_inc_2025_50_54_26,percent_reduction_inc_2025_50_54_27,percent_reduction_inc_2025_50_54_28,percent_reduction_inc_2025_50_54_29,percent_reduction_inc_2025_50_54_30,
                                percent_reduction_inc_2025_50_54_31,percent_reduction_inc_2025_50_54_32,percent_reduction_inc_2025_50_54_33,percent_reduction_inc_2025_50_54_34,percent_reduction_inc_2025_50_54_35,
                                percent_reduction_inc_2025_50_54_36,percent_reduction_inc_2025_50_54_37,percent_reduction_inc_2025_50_54_38,percent_reduction_inc_2025_50_54_39,percent_reduction_inc_2025_50_54_40,
                                percent_reduction_inc_2025_50_54_41,percent_reduction_inc_2025_50_54_42,percent_reduction_inc_2025_50_54_43,percent_reduction_inc_2025_50_54_44,percent_reduction_inc_2025_50_54_45,
                                percent_reduction_inc_2025_50_54_46,percent_reduction_inc_2025_50_54_47,percent_reduction_inc_2025_50_54_48,percent_reduction_inc_2025_50_54_49,percent_reduction_inc_2025_50_54_50)

percent_reduction_inc_2030_50_54<-c(percent_reduction_inc_2030_50_54_1,percent_reduction_inc_2030_50_54_2,percent_reduction_inc_2030_50_54_3,percent_reduction_inc_2030_50_54_4,percent_reduction_inc_2030_50_54_5,
                                percent_reduction_inc_2030_50_54_6,percent_reduction_inc_2030_50_54_7,percent_reduction_inc_2030_50_54_8,percent_reduction_inc_2030_50_54_9,percent_reduction_inc_2030_50_54_10,
                                percent_reduction_inc_2030_50_54_11,percent_reduction_inc_2030_50_54_12,percent_reduction_inc_2030_50_54_13,percent_reduction_inc_2030_50_54_14,percent_reduction_inc_2030_50_54_15,
                                percent_reduction_inc_2030_50_54_16,percent_reduction_inc_2030_50_54_17,percent_reduction_inc_2030_50_54_18,percent_reduction_inc_2030_50_54_19,percent_reduction_inc_2030_50_54_20,
                                percent_reduction_inc_2030_50_54_21,percent_reduction_inc_2030_50_54_22,percent_reduction_inc_2030_50_54_23,percent_reduction_inc_2030_50_54_24,percent_reduction_inc_2030_50_54_25,
                                percent_reduction_inc_2030_50_54_26,percent_reduction_inc_2030_50_54_27,percent_reduction_inc_2030_50_54_28,percent_reduction_inc_2030_50_54_29,percent_reduction_inc_2030_50_54_30,
                                percent_reduction_inc_2030_50_54_31,percent_reduction_inc_2030_50_54_32,percent_reduction_inc_2030_50_54_33,percent_reduction_inc_2030_50_54_34,percent_reduction_inc_2030_50_54_35,
                                percent_reduction_inc_2030_50_54_36,percent_reduction_inc_2030_50_54_37,percent_reduction_inc_2030_50_54_38,percent_reduction_inc_2030_50_54_39,percent_reduction_inc_2030_50_54_40,
                                percent_reduction_inc_2030_50_54_41,percent_reduction_inc_2030_50_54_42,percent_reduction_inc_2030_50_54_43,percent_reduction_inc_2030_50_54_44,percent_reduction_inc_2030_50_54_45,
                                percent_reduction_inc_2030_50_54_46,percent_reduction_inc_2030_50_54_47,percent_reduction_inc_2030_50_54_48,percent_reduction_inc_2030_50_54_49,percent_reduction_inc_2030_50_54_50)

percent_reduction_inc_2035_50_54<-c(percent_reduction_inc_2035_50_54_1,percent_reduction_inc_2035_50_54_2,percent_reduction_inc_2035_50_54_3,percent_reduction_inc_2035_50_54_4,percent_reduction_inc_2035_50_54_5,
                                percent_reduction_inc_2035_50_54_6,percent_reduction_inc_2035_50_54_7,percent_reduction_inc_2035_50_54_8,percent_reduction_inc_2035_50_54_9,percent_reduction_inc_2035_50_54_10,
                                percent_reduction_inc_2035_50_54_11,percent_reduction_inc_2035_50_54_12,percent_reduction_inc_2035_50_54_13,percent_reduction_inc_2035_50_54_14,percent_reduction_inc_2035_50_54_15,
                                percent_reduction_inc_2035_50_54_16,percent_reduction_inc_2035_50_54_17,percent_reduction_inc_2035_50_54_18,percent_reduction_inc_2035_50_54_19,percent_reduction_inc_2035_50_54_20,
                                percent_reduction_inc_2035_50_54_21,percent_reduction_inc_2035_50_54_22,percent_reduction_inc_2035_50_54_23,percent_reduction_inc_2035_50_54_24,percent_reduction_inc_2035_50_54_25,
                                percent_reduction_inc_2035_50_54_26,percent_reduction_inc_2035_50_54_27,percent_reduction_inc_2035_50_54_28,percent_reduction_inc_2035_50_54_29,percent_reduction_inc_2035_50_54_30,
                                percent_reduction_inc_2035_50_54_31,percent_reduction_inc_2035_50_54_32,percent_reduction_inc_2035_50_54_33,percent_reduction_inc_2035_50_54_34,percent_reduction_inc_2035_50_54_35,
                                percent_reduction_inc_2035_50_54_36,percent_reduction_inc_2035_50_54_37,percent_reduction_inc_2035_50_54_38,percent_reduction_inc_2035_50_54_39,percent_reduction_inc_2035_50_54_40,
                                percent_reduction_inc_2035_50_54_41,percent_reduction_inc_2035_50_54_42,percent_reduction_inc_2035_50_54_43,percent_reduction_inc_2035_50_54_44,percent_reduction_inc_2035_50_54_45,
                                percent_reduction_inc_2035_50_54_46,percent_reduction_inc_2035_50_54_47,percent_reduction_inc_2035_50_54_48,percent_reduction_inc_2035_50_54_49,percent_reduction_inc_2035_50_54_50)

percent_reduction_inc_2040_50_54<-c(percent_reduction_inc_2040_50_54_1,percent_reduction_inc_2040_50_54_2,percent_reduction_inc_2040_50_54_3,percent_reduction_inc_2040_50_54_4,percent_reduction_inc_2040_50_54_5,
                                percent_reduction_inc_2040_50_54_6,percent_reduction_inc_2040_50_54_7,percent_reduction_inc_2040_50_54_8,percent_reduction_inc_2040_50_54_9,percent_reduction_inc_2040_50_54_10,
                                percent_reduction_inc_2040_50_54_11,percent_reduction_inc_2040_50_54_12,percent_reduction_inc_2040_50_54_13,percent_reduction_inc_2040_50_54_14,percent_reduction_inc_2040_50_54_15,
                                percent_reduction_inc_2040_50_54_16,percent_reduction_inc_2040_50_54_17,percent_reduction_inc_2040_50_54_18,percent_reduction_inc_2040_50_54_19,percent_reduction_inc_2040_50_54_20,
                                percent_reduction_inc_2040_50_54_21,percent_reduction_inc_2040_50_54_22,percent_reduction_inc_2040_50_54_23,percent_reduction_inc_2040_50_54_24,percent_reduction_inc_2040_50_54_25,
                                percent_reduction_inc_2040_50_54_26,percent_reduction_inc_2040_50_54_27,percent_reduction_inc_2040_50_54_28,percent_reduction_inc_2040_50_54_29,percent_reduction_inc_2040_50_54_30,
                                percent_reduction_inc_2040_50_54_31,percent_reduction_inc_2040_50_54_32,percent_reduction_inc_2040_50_54_33,percent_reduction_inc_2040_50_54_34,percent_reduction_inc_2040_50_54_35,
                                percent_reduction_inc_2040_50_54_36,percent_reduction_inc_2040_50_54_37,percent_reduction_inc_2040_50_54_38,percent_reduction_inc_2040_50_54_39,percent_reduction_inc_2040_50_54_40,
                                percent_reduction_inc_2040_50_54_41,percent_reduction_inc_2040_50_54_42,percent_reduction_inc_2040_50_54_43,percent_reduction_inc_2040_50_54_44,percent_reduction_inc_2040_50_54_45,
                                percent_reduction_inc_2040_50_54_46,percent_reduction_inc_2040_50_54_47,percent_reduction_inc_2040_50_54_48,percent_reduction_inc_2040_50_54_49,percent_reduction_inc_2040_50_54_50)

percent_reduction_inc_2045_50_54<-c(percent_reduction_inc_2045_50_54_1,percent_reduction_inc_2045_50_54_2,percent_reduction_inc_2045_50_54_3,percent_reduction_inc_2045_50_54_4,percent_reduction_inc_2045_50_54_5,
                                percent_reduction_inc_2045_50_54_6,percent_reduction_inc_2045_50_54_7,percent_reduction_inc_2045_50_54_8,percent_reduction_inc_2045_50_54_9,percent_reduction_inc_2045_50_54_10,
                                percent_reduction_inc_2045_50_54_11,percent_reduction_inc_2045_50_54_12,percent_reduction_inc_2045_50_54_13,percent_reduction_inc_2045_50_54_14,percent_reduction_inc_2045_50_54_15,
                                percent_reduction_inc_2045_50_54_16,percent_reduction_inc_2045_50_54_17,percent_reduction_inc_2045_50_54_18,percent_reduction_inc_2045_50_54_19,percent_reduction_inc_2045_50_54_20,
                                percent_reduction_inc_2045_50_54_21,percent_reduction_inc_2045_50_54_22,percent_reduction_inc_2045_50_54_23,percent_reduction_inc_2045_50_54_24,percent_reduction_inc_2045_50_54_25,
                                percent_reduction_inc_2045_50_54_26,percent_reduction_inc_2045_50_54_27,percent_reduction_inc_2045_50_54_28,percent_reduction_inc_2045_50_54_29,percent_reduction_inc_2045_50_54_30,
                                percent_reduction_inc_2045_50_54_31,percent_reduction_inc_2045_50_54_32,percent_reduction_inc_2045_50_54_33,percent_reduction_inc_2045_50_54_34,percent_reduction_inc_2045_50_54_35,
                                percent_reduction_inc_2045_50_54_36,percent_reduction_inc_2045_50_54_37,percent_reduction_inc_2045_50_54_38,percent_reduction_inc_2045_50_54_39,percent_reduction_inc_2045_50_54_40,
                                percent_reduction_inc_2045_50_54_41,percent_reduction_inc_2045_50_54_42,percent_reduction_inc_2045_50_54_43,percent_reduction_inc_2045_50_54_44,percent_reduction_inc_2045_50_54_45,
                                percent_reduction_inc_2045_50_54_46,percent_reduction_inc_2045_50_54_47,percent_reduction_inc_2045_50_54_48,percent_reduction_inc_2045_50_54_49,percent_reduction_inc_2045_50_54_50)
#####
percent_reduction_inc_2025_55_59<-c(percent_reduction_inc_2025_55_59_1,percent_reduction_inc_2025_55_59_2,percent_reduction_inc_2025_55_59_3,percent_reduction_inc_2025_55_59_4,percent_reduction_inc_2025_55_59_5,
                                percent_reduction_inc_2025_55_59_6,percent_reduction_inc_2025_55_59_7,percent_reduction_inc_2025_55_59_8,percent_reduction_inc_2025_55_59_9,percent_reduction_inc_2025_55_59_10,
                                percent_reduction_inc_2025_55_59_11,percent_reduction_inc_2025_55_59_12,percent_reduction_inc_2025_55_59_13,percent_reduction_inc_2025_55_59_14,percent_reduction_inc_2025_55_59_15,
                                percent_reduction_inc_2025_55_59_16,percent_reduction_inc_2025_55_59_17,percent_reduction_inc_2025_55_59_18,percent_reduction_inc_2025_55_59_19,percent_reduction_inc_2025_55_59_20,
                                percent_reduction_inc_2025_55_59_21,percent_reduction_inc_2025_55_59_22,percent_reduction_inc_2025_55_59_23,percent_reduction_inc_2025_55_59_24,percent_reduction_inc_2025_55_59_25,
                                percent_reduction_inc_2025_55_59_26,percent_reduction_inc_2025_55_59_27,percent_reduction_inc_2025_55_59_28,percent_reduction_inc_2025_55_59_29,percent_reduction_inc_2025_55_59_30,
                                percent_reduction_inc_2025_55_59_31,percent_reduction_inc_2025_55_59_32,percent_reduction_inc_2025_55_59_33,percent_reduction_inc_2025_55_59_34,percent_reduction_inc_2025_55_59_35,
                                percent_reduction_inc_2025_55_59_36,percent_reduction_inc_2025_55_59_37,percent_reduction_inc_2025_55_59_38,percent_reduction_inc_2025_55_59_39,percent_reduction_inc_2025_55_59_40,
                                percent_reduction_inc_2025_55_59_41,percent_reduction_inc_2025_55_59_42,percent_reduction_inc_2025_55_59_43,percent_reduction_inc_2025_55_59_44,percent_reduction_inc_2025_55_59_45,
                                percent_reduction_inc_2025_55_59_46,percent_reduction_inc_2025_55_59_47,percent_reduction_inc_2025_55_59_48,percent_reduction_inc_2025_55_59_49,percent_reduction_inc_2025_55_59_50)

percent_reduction_inc_2030_55_59<-c(percent_reduction_inc_2030_55_59_1,percent_reduction_inc_2030_55_59_2,percent_reduction_inc_2030_55_59_3,percent_reduction_inc_2030_55_59_4,percent_reduction_inc_2030_55_59_5,
                                percent_reduction_inc_2030_55_59_6,percent_reduction_inc_2030_55_59_7,percent_reduction_inc_2030_55_59_8,percent_reduction_inc_2030_55_59_9,percent_reduction_inc_2030_55_59_10,
                                percent_reduction_inc_2030_55_59_11,percent_reduction_inc_2030_55_59_12,percent_reduction_inc_2030_55_59_13,percent_reduction_inc_2030_55_59_14,percent_reduction_inc_2030_55_59_15,
                                percent_reduction_inc_2030_55_59_16,percent_reduction_inc_2030_55_59_17,percent_reduction_inc_2030_55_59_18,percent_reduction_inc_2030_55_59_19,percent_reduction_inc_2030_55_59_20,
                                percent_reduction_inc_2030_55_59_21,percent_reduction_inc_2030_55_59_22,percent_reduction_inc_2030_55_59_23,percent_reduction_inc_2030_55_59_24,percent_reduction_inc_2030_55_59_25,
                                percent_reduction_inc_2030_55_59_26,percent_reduction_inc_2030_55_59_27,percent_reduction_inc_2030_55_59_28,percent_reduction_inc_2030_55_59_29,percent_reduction_inc_2030_55_59_30,
                                percent_reduction_inc_2030_55_59_31,percent_reduction_inc_2030_55_59_32,percent_reduction_inc_2030_55_59_33,percent_reduction_inc_2030_55_59_34,percent_reduction_inc_2030_55_59_35,
                                percent_reduction_inc_2030_55_59_36,percent_reduction_inc_2030_55_59_37,percent_reduction_inc_2030_55_59_38,percent_reduction_inc_2030_55_59_39,percent_reduction_inc_2030_55_59_40,
                                percent_reduction_inc_2030_55_59_41,percent_reduction_inc_2030_55_59_42,percent_reduction_inc_2030_55_59_43,percent_reduction_inc_2030_55_59_44,percent_reduction_inc_2030_55_59_45,
                                percent_reduction_inc_2030_55_59_46,percent_reduction_inc_2030_55_59_47,percent_reduction_inc_2030_55_59_48,percent_reduction_inc_2030_55_59_49,percent_reduction_inc_2030_55_59_50)

percent_reduction_inc_2035_55_59<-c(percent_reduction_inc_2035_55_59_1,percent_reduction_inc_2035_55_59_2,percent_reduction_inc_2035_55_59_3,percent_reduction_inc_2035_55_59_4,percent_reduction_inc_2035_55_59_5,
                                percent_reduction_inc_2035_55_59_6,percent_reduction_inc_2035_55_59_7,percent_reduction_inc_2035_55_59_8,percent_reduction_inc_2035_55_59_9,percent_reduction_inc_2035_55_59_10,
                                percent_reduction_inc_2035_55_59_11,percent_reduction_inc_2035_55_59_12,percent_reduction_inc_2035_55_59_13,percent_reduction_inc_2035_55_59_14,percent_reduction_inc_2035_55_59_15,
                                percent_reduction_inc_2035_55_59_16,percent_reduction_inc_2035_55_59_17,percent_reduction_inc_2035_55_59_18,percent_reduction_inc_2035_55_59_19,percent_reduction_inc_2035_55_59_20,
                                percent_reduction_inc_2035_55_59_21,percent_reduction_inc_2035_55_59_22,percent_reduction_inc_2035_55_59_23,percent_reduction_inc_2035_55_59_24,percent_reduction_inc_2035_55_59_25,
                                percent_reduction_inc_2035_55_59_26,percent_reduction_inc_2035_55_59_27,percent_reduction_inc_2035_55_59_28,percent_reduction_inc_2035_55_59_29,percent_reduction_inc_2035_55_59_30,
                                percent_reduction_inc_2035_55_59_31,percent_reduction_inc_2035_55_59_32,percent_reduction_inc_2035_55_59_33,percent_reduction_inc_2035_55_59_34,percent_reduction_inc_2035_55_59_35,
                                percent_reduction_inc_2035_55_59_36,percent_reduction_inc_2035_55_59_37,percent_reduction_inc_2035_55_59_38,percent_reduction_inc_2035_55_59_39,percent_reduction_inc_2035_55_59_40,
                                percent_reduction_inc_2035_55_59_41,percent_reduction_inc_2035_55_59_42,percent_reduction_inc_2035_55_59_43,percent_reduction_inc_2035_55_59_44,percent_reduction_inc_2035_55_59_45,
                                percent_reduction_inc_2035_55_59_46,percent_reduction_inc_2035_55_59_47,percent_reduction_inc_2035_55_59_48,percent_reduction_inc_2035_55_59_49,percent_reduction_inc_2035_55_59_50)

percent_reduction_inc_2040_55_59<-c(percent_reduction_inc_2040_55_59_1,percent_reduction_inc_2040_55_59_2,percent_reduction_inc_2040_55_59_3,percent_reduction_inc_2040_55_59_4,percent_reduction_inc_2040_55_59_5,
                                percent_reduction_inc_2040_55_59_6,percent_reduction_inc_2040_55_59_7,percent_reduction_inc_2040_55_59_8,percent_reduction_inc_2040_55_59_9,percent_reduction_inc_2040_55_59_10,
                                percent_reduction_inc_2040_55_59_11,percent_reduction_inc_2040_55_59_12,percent_reduction_inc_2040_55_59_13,percent_reduction_inc_2040_55_59_14,percent_reduction_inc_2040_55_59_15,
                                percent_reduction_inc_2040_55_59_16,percent_reduction_inc_2040_55_59_17,percent_reduction_inc_2040_55_59_18,percent_reduction_inc_2040_55_59_19,percent_reduction_inc_2040_55_59_20,
                                percent_reduction_inc_2040_55_59_21,percent_reduction_inc_2040_55_59_22,percent_reduction_inc_2040_55_59_23,percent_reduction_inc_2040_55_59_24,percent_reduction_inc_2040_55_59_25,
                                percent_reduction_inc_2040_55_59_26,percent_reduction_inc_2040_55_59_27,percent_reduction_inc_2040_55_59_28,percent_reduction_inc_2040_55_59_29,percent_reduction_inc_2040_55_59_30,
                                percent_reduction_inc_2040_55_59_31,percent_reduction_inc_2040_55_59_32,percent_reduction_inc_2040_55_59_33,percent_reduction_inc_2040_55_59_34,percent_reduction_inc_2040_55_59_35,
                                percent_reduction_inc_2040_55_59_36,percent_reduction_inc_2040_55_59_37,percent_reduction_inc_2040_55_59_38,percent_reduction_inc_2040_55_59_39,percent_reduction_inc_2040_55_59_40,
                                percent_reduction_inc_2040_55_59_41,percent_reduction_inc_2040_55_59_42,percent_reduction_inc_2040_55_59_43,percent_reduction_inc_2040_55_59_44,percent_reduction_inc_2040_55_59_45,
                                percent_reduction_inc_2040_55_59_46,percent_reduction_inc_2040_55_59_47,percent_reduction_inc_2040_55_59_48,percent_reduction_inc_2040_55_59_49,percent_reduction_inc_2040_55_59_50)

percent_reduction_inc_2045_55_59<-c(percent_reduction_inc_2045_55_59_1,percent_reduction_inc_2045_55_59_2,percent_reduction_inc_2045_55_59_3,percent_reduction_inc_2045_55_59_4,percent_reduction_inc_2045_55_59_5,
                                percent_reduction_inc_2045_55_59_6,percent_reduction_inc_2045_55_59_7,percent_reduction_inc_2045_55_59_8,percent_reduction_inc_2045_55_59_9,percent_reduction_inc_2045_55_59_10,
                                percent_reduction_inc_2045_55_59_11,percent_reduction_inc_2045_55_59_12,percent_reduction_inc_2045_55_59_13,percent_reduction_inc_2045_55_59_14,percent_reduction_inc_2045_55_59_15,
                                percent_reduction_inc_2045_55_59_16,percent_reduction_inc_2045_55_59_17,percent_reduction_inc_2045_55_59_18,percent_reduction_inc_2045_55_59_19,percent_reduction_inc_2045_55_59_20,
                                percent_reduction_inc_2045_55_59_21,percent_reduction_inc_2045_55_59_22,percent_reduction_inc_2045_55_59_23,percent_reduction_inc_2045_55_59_24,percent_reduction_inc_2045_55_59_25,
                                percent_reduction_inc_2045_55_59_26,percent_reduction_inc_2045_55_59_27,percent_reduction_inc_2045_55_59_28,percent_reduction_inc_2045_55_59_29,percent_reduction_inc_2045_55_59_30,
                                percent_reduction_inc_2045_55_59_31,percent_reduction_inc_2045_55_59_32,percent_reduction_inc_2045_55_59_33,percent_reduction_inc_2045_55_59_34,percent_reduction_inc_2045_55_59_35,
                                percent_reduction_inc_2045_55_59_36,percent_reduction_inc_2045_55_59_37,percent_reduction_inc_2045_55_59_38,percent_reduction_inc_2045_55_59_39,percent_reduction_inc_2045_55_59_40,
                                percent_reduction_inc_2045_55_59_41,percent_reduction_inc_2045_55_59_42,percent_reduction_inc_2045_55_59_43,percent_reduction_inc_2045_55_59_44,percent_reduction_inc_2045_55_59_45,
                                percent_reduction_inc_2045_55_59_46,percent_reduction_inc_2045_55_59_47,percent_reduction_inc_2045_55_59_48,percent_reduction_inc_2045_55_59_49,percent_reduction_inc_2045_55_59_50)
#####
percent_reduction_inc_2025_60_64<-c(percent_reduction_inc_2025_60_64_1,percent_reduction_inc_2025_60_64_2,percent_reduction_inc_2025_60_64_3,percent_reduction_inc_2025_60_64_4,percent_reduction_inc_2025_60_64_5,
                                percent_reduction_inc_2025_60_64_6,percent_reduction_inc_2025_60_64_7,percent_reduction_inc_2025_60_64_8,percent_reduction_inc_2025_60_64_9,percent_reduction_inc_2025_60_64_10,
                                percent_reduction_inc_2025_60_64_11,percent_reduction_inc_2025_60_64_12,percent_reduction_inc_2025_60_64_13,percent_reduction_inc_2025_60_64_14,percent_reduction_inc_2025_60_64_15,
                                percent_reduction_inc_2025_60_64_16,percent_reduction_inc_2025_60_64_17,percent_reduction_inc_2025_60_64_18,percent_reduction_inc_2025_60_64_19,percent_reduction_inc_2025_60_64_20,
                                percent_reduction_inc_2025_60_64_21,percent_reduction_inc_2025_60_64_22,percent_reduction_inc_2025_60_64_23,percent_reduction_inc_2025_60_64_24,percent_reduction_inc_2025_60_64_25,
                                percent_reduction_inc_2025_60_64_26,percent_reduction_inc_2025_60_64_27,percent_reduction_inc_2025_60_64_28,percent_reduction_inc_2025_60_64_29,percent_reduction_inc_2025_60_64_30,
                                percent_reduction_inc_2025_60_64_31,percent_reduction_inc_2025_60_64_32,percent_reduction_inc_2025_60_64_33,percent_reduction_inc_2025_60_64_34,percent_reduction_inc_2025_60_64_35,
                                percent_reduction_inc_2025_60_64_36,percent_reduction_inc_2025_60_64_37,percent_reduction_inc_2025_60_64_38,percent_reduction_inc_2025_60_64_39,percent_reduction_inc_2025_60_64_40,
                                percent_reduction_inc_2025_60_64_41,percent_reduction_inc_2025_60_64_42,percent_reduction_inc_2025_60_64_43,percent_reduction_inc_2025_60_64_44,percent_reduction_inc_2025_60_64_45,
                                percent_reduction_inc_2025_60_64_46,percent_reduction_inc_2025_60_64_47,percent_reduction_inc_2025_60_64_48,percent_reduction_inc_2025_60_64_49,percent_reduction_inc_2025_60_64_50)

percent_reduction_inc_2030_60_64<-c(percent_reduction_inc_2030_60_64_1,percent_reduction_inc_2030_60_64_2,percent_reduction_inc_2030_60_64_3,percent_reduction_inc_2030_60_64_4,percent_reduction_inc_2030_60_64_5,
                                percent_reduction_inc_2030_60_64_6,percent_reduction_inc_2030_60_64_7,percent_reduction_inc_2030_60_64_8,percent_reduction_inc_2030_60_64_9,percent_reduction_inc_2030_60_64_10,
                                percent_reduction_inc_2030_60_64_11,percent_reduction_inc_2030_60_64_12,percent_reduction_inc_2030_60_64_13,percent_reduction_inc_2030_60_64_14,percent_reduction_inc_2030_60_64_15,
                                percent_reduction_inc_2030_60_64_16,percent_reduction_inc_2030_60_64_17,percent_reduction_inc_2030_60_64_18,percent_reduction_inc_2030_60_64_19,percent_reduction_inc_2030_60_64_20,
                                percent_reduction_inc_2030_60_64_21,percent_reduction_inc_2030_60_64_22,percent_reduction_inc_2030_60_64_23,percent_reduction_inc_2030_60_64_24,percent_reduction_inc_2030_60_64_25,
                                percent_reduction_inc_2030_60_64_26,percent_reduction_inc_2030_60_64_27,percent_reduction_inc_2030_60_64_28,percent_reduction_inc_2030_60_64_29,percent_reduction_inc_2030_60_64_30,
                                percent_reduction_inc_2030_60_64_31,percent_reduction_inc_2030_60_64_32,percent_reduction_inc_2030_60_64_33,percent_reduction_inc_2030_60_64_34,percent_reduction_inc_2030_60_64_35,
                                percent_reduction_inc_2030_60_64_36,percent_reduction_inc_2030_60_64_37,percent_reduction_inc_2030_60_64_38,percent_reduction_inc_2030_60_64_39,percent_reduction_inc_2030_60_64_40,
                                percent_reduction_inc_2030_60_64_41,percent_reduction_inc_2030_60_64_42,percent_reduction_inc_2030_60_64_43,percent_reduction_inc_2030_60_64_44,percent_reduction_inc_2030_60_64_45,
                                percent_reduction_inc_2030_60_64_46,percent_reduction_inc_2030_60_64_47,percent_reduction_inc_2030_60_64_48,percent_reduction_inc_2030_60_64_49,percent_reduction_inc_2030_60_64_50)

percent_reduction_inc_2035_60_64<-c(percent_reduction_inc_2035_60_64_1,percent_reduction_inc_2035_60_64_2,percent_reduction_inc_2035_60_64_3,percent_reduction_inc_2035_60_64_4,percent_reduction_inc_2035_60_64_5,
                                percent_reduction_inc_2035_60_64_6,percent_reduction_inc_2035_60_64_7,percent_reduction_inc_2035_60_64_8,percent_reduction_inc_2035_60_64_9,percent_reduction_inc_2035_60_64_10,
                                percent_reduction_inc_2035_60_64_11,percent_reduction_inc_2035_60_64_12,percent_reduction_inc_2035_60_64_13,percent_reduction_inc_2035_60_64_14,percent_reduction_inc_2035_60_64_15,
                                percent_reduction_inc_2035_60_64_16,percent_reduction_inc_2035_60_64_17,percent_reduction_inc_2035_60_64_18,percent_reduction_inc_2035_60_64_19,percent_reduction_inc_2035_60_64_20,
                                percent_reduction_inc_2035_60_64_21,percent_reduction_inc_2035_60_64_22,percent_reduction_inc_2035_60_64_23,percent_reduction_inc_2035_60_64_24,percent_reduction_inc_2035_60_64_25,
                                percent_reduction_inc_2035_60_64_26,percent_reduction_inc_2035_60_64_27,percent_reduction_inc_2035_60_64_28,percent_reduction_inc_2035_60_64_29,percent_reduction_inc_2035_60_64_30,
                                percent_reduction_inc_2035_60_64_31,percent_reduction_inc_2035_60_64_32,percent_reduction_inc_2035_60_64_33,percent_reduction_inc_2035_60_64_34,percent_reduction_inc_2035_60_64_35,
                                percent_reduction_inc_2035_60_64_36,percent_reduction_inc_2035_60_64_37,percent_reduction_inc_2035_60_64_38,percent_reduction_inc_2035_60_64_39,percent_reduction_inc_2035_60_64_40,
                                percent_reduction_inc_2035_60_64_41,percent_reduction_inc_2035_60_64_42,percent_reduction_inc_2035_60_64_43,percent_reduction_inc_2035_60_64_44,percent_reduction_inc_2035_60_64_45,
                                percent_reduction_inc_2035_60_64_46,percent_reduction_inc_2035_60_64_47,percent_reduction_inc_2035_60_64_48,percent_reduction_inc_2035_60_64_49,percent_reduction_inc_2035_60_64_50)

percent_reduction_inc_2040_60_64<-c(percent_reduction_inc_2040_60_64_1,percent_reduction_inc_2040_60_64_2,percent_reduction_inc_2040_60_64_3,percent_reduction_inc_2040_60_64_4,percent_reduction_inc_2040_60_64_5,
                                percent_reduction_inc_2040_60_64_6,percent_reduction_inc_2040_60_64_7,percent_reduction_inc_2040_60_64_8,percent_reduction_inc_2040_60_64_9,percent_reduction_inc_2040_60_64_10,
                                percent_reduction_inc_2040_60_64_11,percent_reduction_inc_2040_60_64_12,percent_reduction_inc_2040_60_64_13,percent_reduction_inc_2040_60_64_14,percent_reduction_inc_2040_60_64_15,
                                percent_reduction_inc_2040_60_64_16,percent_reduction_inc_2040_60_64_17,percent_reduction_inc_2040_60_64_18,percent_reduction_inc_2040_60_64_19,percent_reduction_inc_2040_60_64_20,
                                percent_reduction_inc_2040_60_64_21,percent_reduction_inc_2040_60_64_22,percent_reduction_inc_2040_60_64_23,percent_reduction_inc_2040_60_64_24,percent_reduction_inc_2040_60_64_25,
                                percent_reduction_inc_2040_60_64_26,percent_reduction_inc_2040_60_64_27,percent_reduction_inc_2040_60_64_28,percent_reduction_inc_2040_60_64_29,percent_reduction_inc_2040_60_64_30,
                                percent_reduction_inc_2040_60_64_31,percent_reduction_inc_2040_60_64_32,percent_reduction_inc_2040_60_64_33,percent_reduction_inc_2040_60_64_34,percent_reduction_inc_2040_60_64_35,
                                percent_reduction_inc_2040_60_64_36,percent_reduction_inc_2040_60_64_37,percent_reduction_inc_2040_60_64_38,percent_reduction_inc_2040_60_64_39,percent_reduction_inc_2040_60_64_40,
                                percent_reduction_inc_2040_60_64_41,percent_reduction_inc_2040_60_64_42,percent_reduction_inc_2040_60_64_43,percent_reduction_inc_2040_60_64_44,percent_reduction_inc_2040_60_64_45,
                                percent_reduction_inc_2040_60_64_46,percent_reduction_inc_2040_60_64_47,percent_reduction_inc_2040_60_64_48,percent_reduction_inc_2040_60_64_49,percent_reduction_inc_2040_60_64_50)

percent_reduction_inc_2045_60_64<-c(percent_reduction_inc_2045_60_64_1,percent_reduction_inc_2045_60_64_2,percent_reduction_inc_2045_60_64_3,percent_reduction_inc_2045_60_64_4,percent_reduction_inc_2045_60_64_5,
                                percent_reduction_inc_2045_60_64_6,percent_reduction_inc_2045_60_64_7,percent_reduction_inc_2045_60_64_8,percent_reduction_inc_2045_60_64_9,percent_reduction_inc_2045_60_64_10,
                                percent_reduction_inc_2045_60_64_11,percent_reduction_inc_2045_60_64_12,percent_reduction_inc_2045_60_64_13,percent_reduction_inc_2045_60_64_14,percent_reduction_inc_2045_60_64_15,
                                percent_reduction_inc_2045_60_64_16,percent_reduction_inc_2045_60_64_17,percent_reduction_inc_2045_60_64_18,percent_reduction_inc_2045_60_64_19,percent_reduction_inc_2045_60_64_20,
                                percent_reduction_inc_2045_60_64_21,percent_reduction_inc_2045_60_64_22,percent_reduction_inc_2045_60_64_23,percent_reduction_inc_2045_60_64_24,percent_reduction_inc_2045_60_64_25,
                                percent_reduction_inc_2045_60_64_26,percent_reduction_inc_2045_60_64_27,percent_reduction_inc_2045_60_64_28,percent_reduction_inc_2045_60_64_29,percent_reduction_inc_2045_60_64_30,
                                percent_reduction_inc_2045_60_64_31,percent_reduction_inc_2045_60_64_32,percent_reduction_inc_2045_60_64_33,percent_reduction_inc_2045_60_64_34,percent_reduction_inc_2045_60_64_35,
                                percent_reduction_inc_2045_60_64_36,percent_reduction_inc_2045_60_64_37,percent_reduction_inc_2045_60_64_38,percent_reduction_inc_2045_60_64_39,percent_reduction_inc_2045_60_64_40,
                                percent_reduction_inc_2045_60_64_41,percent_reduction_inc_2045_60_64_42,percent_reduction_inc_2045_60_64_43,percent_reduction_inc_2045_60_64_44,percent_reduction_inc_2045_60_64_45,
                                percent_reduction_inc_2045_60_64_46,percent_reduction_inc_2045_60_64_47,percent_reduction_inc_2045_60_64_48,percent_reduction_inc_2045_60_64_49,percent_reduction_inc_2045_60_64_50)
#####
percent_reduction_inc_2025_65_69<-c(percent_reduction_inc_2025_65_69_1,percent_reduction_inc_2025_65_69_2,percent_reduction_inc_2025_65_69_3,percent_reduction_inc_2025_65_69_4,percent_reduction_inc_2025_65_69_5,
                                percent_reduction_inc_2025_65_69_6,percent_reduction_inc_2025_65_69_7,percent_reduction_inc_2025_65_69_8,percent_reduction_inc_2025_65_69_9,percent_reduction_inc_2025_65_69_10,
                                percent_reduction_inc_2025_65_69_11,percent_reduction_inc_2025_65_69_12,percent_reduction_inc_2025_65_69_13,percent_reduction_inc_2025_65_69_14,percent_reduction_inc_2025_65_69_15,
                                percent_reduction_inc_2025_65_69_16,percent_reduction_inc_2025_65_69_17,percent_reduction_inc_2025_65_69_18,percent_reduction_inc_2025_65_69_19,percent_reduction_inc_2025_65_69_20,
                                percent_reduction_inc_2025_65_69_21,percent_reduction_inc_2025_65_69_22,percent_reduction_inc_2025_65_69_23,percent_reduction_inc_2025_65_69_24,percent_reduction_inc_2025_65_69_25,
                                percent_reduction_inc_2025_65_69_26,percent_reduction_inc_2025_65_69_27,percent_reduction_inc_2025_65_69_28,percent_reduction_inc_2025_65_69_29,percent_reduction_inc_2025_65_69_30,
                                percent_reduction_inc_2025_65_69_31,percent_reduction_inc_2025_65_69_32,percent_reduction_inc_2025_65_69_33,percent_reduction_inc_2025_65_69_34,percent_reduction_inc_2025_65_69_35,
                                percent_reduction_inc_2025_65_69_36,percent_reduction_inc_2025_65_69_37,percent_reduction_inc_2025_65_69_38,percent_reduction_inc_2025_65_69_39,percent_reduction_inc_2025_65_69_40,
                                percent_reduction_inc_2025_65_69_41,percent_reduction_inc_2025_65_69_42,percent_reduction_inc_2025_65_69_43,percent_reduction_inc_2025_65_69_44,percent_reduction_inc_2025_65_69_45,
                                percent_reduction_inc_2025_65_69_46,percent_reduction_inc_2025_65_69_47,percent_reduction_inc_2025_65_69_48,percent_reduction_inc_2025_65_69_49,percent_reduction_inc_2025_65_69_50)

percent_reduction_inc_2030_65_69<-c(percent_reduction_inc_2030_65_69_1,percent_reduction_inc_2030_65_69_2,percent_reduction_inc_2030_65_69_3,percent_reduction_inc_2030_65_69_4,percent_reduction_inc_2030_65_69_5,
                                percent_reduction_inc_2030_65_69_6,percent_reduction_inc_2030_65_69_7,percent_reduction_inc_2030_65_69_8,percent_reduction_inc_2030_65_69_9,percent_reduction_inc_2030_65_69_10,
                                percent_reduction_inc_2030_65_69_11,percent_reduction_inc_2030_65_69_12,percent_reduction_inc_2030_65_69_13,percent_reduction_inc_2030_65_69_14,percent_reduction_inc_2030_65_69_15,
                                percent_reduction_inc_2030_65_69_16,percent_reduction_inc_2030_65_69_17,percent_reduction_inc_2030_65_69_18,percent_reduction_inc_2030_65_69_19,percent_reduction_inc_2030_65_69_20,
                                percent_reduction_inc_2030_65_69_21,percent_reduction_inc_2030_65_69_22,percent_reduction_inc_2030_65_69_23,percent_reduction_inc_2030_65_69_24,percent_reduction_inc_2030_65_69_25,
                                percent_reduction_inc_2030_65_69_26,percent_reduction_inc_2030_65_69_27,percent_reduction_inc_2030_65_69_28,percent_reduction_inc_2030_65_69_29,percent_reduction_inc_2030_65_69_30,
                                percent_reduction_inc_2030_65_69_31,percent_reduction_inc_2030_65_69_32,percent_reduction_inc_2030_65_69_33,percent_reduction_inc_2030_65_69_34,percent_reduction_inc_2030_65_69_35,
                                percent_reduction_inc_2030_65_69_36,percent_reduction_inc_2030_65_69_37,percent_reduction_inc_2030_65_69_38,percent_reduction_inc_2030_65_69_39,percent_reduction_inc_2030_65_69_40,
                                percent_reduction_inc_2030_65_69_41,percent_reduction_inc_2030_65_69_42,percent_reduction_inc_2030_65_69_43,percent_reduction_inc_2030_65_69_44,percent_reduction_inc_2030_65_69_45,
                                percent_reduction_inc_2030_65_69_46,percent_reduction_inc_2030_65_69_47,percent_reduction_inc_2030_65_69_48,percent_reduction_inc_2030_65_69_49,percent_reduction_inc_2030_65_69_50)

percent_reduction_inc_2035_65_69<-c(percent_reduction_inc_2035_65_69_1,percent_reduction_inc_2035_65_69_2,percent_reduction_inc_2035_65_69_3,percent_reduction_inc_2035_65_69_4,percent_reduction_inc_2035_65_69_5,
                                percent_reduction_inc_2035_65_69_6,percent_reduction_inc_2035_65_69_7,percent_reduction_inc_2035_65_69_8,percent_reduction_inc_2035_65_69_9,percent_reduction_inc_2035_65_69_10,
                                percent_reduction_inc_2035_65_69_11,percent_reduction_inc_2035_65_69_12,percent_reduction_inc_2035_65_69_13,percent_reduction_inc_2035_65_69_14,percent_reduction_inc_2035_65_69_15,
                                percent_reduction_inc_2035_65_69_16,percent_reduction_inc_2035_65_69_17,percent_reduction_inc_2035_65_69_18,percent_reduction_inc_2035_65_69_19,percent_reduction_inc_2035_65_69_20,
                                percent_reduction_inc_2035_65_69_21,percent_reduction_inc_2035_65_69_22,percent_reduction_inc_2035_65_69_23,percent_reduction_inc_2035_65_69_24,percent_reduction_inc_2035_65_69_25,
                                percent_reduction_inc_2035_65_69_26,percent_reduction_inc_2035_65_69_27,percent_reduction_inc_2035_65_69_28,percent_reduction_inc_2035_65_69_29,percent_reduction_inc_2035_65_69_30,
                                percent_reduction_inc_2035_65_69_31,percent_reduction_inc_2035_65_69_32,percent_reduction_inc_2035_65_69_33,percent_reduction_inc_2035_65_69_34,percent_reduction_inc_2035_65_69_35,
                                percent_reduction_inc_2035_65_69_36,percent_reduction_inc_2035_65_69_37,percent_reduction_inc_2035_65_69_38,percent_reduction_inc_2035_65_69_39,percent_reduction_inc_2035_65_69_40,
                                percent_reduction_inc_2035_65_69_41,percent_reduction_inc_2035_65_69_42,percent_reduction_inc_2035_65_69_43,percent_reduction_inc_2035_65_69_44,percent_reduction_inc_2035_65_69_45,
                                percent_reduction_inc_2035_65_69_46,percent_reduction_inc_2035_65_69_47,percent_reduction_inc_2035_65_69_48,percent_reduction_inc_2035_65_69_49,percent_reduction_inc_2035_65_69_50)

percent_reduction_inc_2040_65_69<-c(percent_reduction_inc_2040_65_69_1,percent_reduction_inc_2040_65_69_2,percent_reduction_inc_2040_65_69_3,percent_reduction_inc_2040_65_69_4,percent_reduction_inc_2040_65_69_5,
                                percent_reduction_inc_2040_65_69_6,percent_reduction_inc_2040_65_69_7,percent_reduction_inc_2040_65_69_8,percent_reduction_inc_2040_65_69_9,percent_reduction_inc_2040_65_69_10,
                                percent_reduction_inc_2040_65_69_11,percent_reduction_inc_2040_65_69_12,percent_reduction_inc_2040_65_69_13,percent_reduction_inc_2040_65_69_14,percent_reduction_inc_2040_65_69_15,
                                percent_reduction_inc_2040_65_69_16,percent_reduction_inc_2040_65_69_17,percent_reduction_inc_2040_65_69_18,percent_reduction_inc_2040_65_69_19,percent_reduction_inc_2040_65_69_20,
                                percent_reduction_inc_2040_65_69_21,percent_reduction_inc_2040_65_69_22,percent_reduction_inc_2040_65_69_23,percent_reduction_inc_2040_65_69_24,percent_reduction_inc_2040_65_69_25,
                                percent_reduction_inc_2040_65_69_26,percent_reduction_inc_2040_65_69_27,percent_reduction_inc_2040_65_69_28,percent_reduction_inc_2040_65_69_29,percent_reduction_inc_2040_65_69_30,
                                percent_reduction_inc_2040_65_69_31,percent_reduction_inc_2040_65_69_32,percent_reduction_inc_2040_65_69_33,percent_reduction_inc_2040_65_69_34,percent_reduction_inc_2040_65_69_35,
                                percent_reduction_inc_2040_65_69_36,percent_reduction_inc_2040_65_69_37,percent_reduction_inc_2040_65_69_38,percent_reduction_inc_2040_65_69_39,percent_reduction_inc_2040_65_69_40,
                                percent_reduction_inc_2040_65_69_41,percent_reduction_inc_2040_65_69_42,percent_reduction_inc_2040_65_69_43,percent_reduction_inc_2040_65_69_44,percent_reduction_inc_2040_65_69_45,
                                percent_reduction_inc_2040_65_69_46,percent_reduction_inc_2040_65_69_47,percent_reduction_inc_2040_65_69_48,percent_reduction_inc_2040_65_69_49,percent_reduction_inc_2040_65_69_50)

percent_reduction_inc_2045_65_69<-c(percent_reduction_inc_2045_65_69_1,percent_reduction_inc_2045_65_69_2,percent_reduction_inc_2045_65_69_3,percent_reduction_inc_2045_65_69_4,percent_reduction_inc_2045_65_69_5,
                                percent_reduction_inc_2045_65_69_6,percent_reduction_inc_2045_65_69_7,percent_reduction_inc_2045_65_69_8,percent_reduction_inc_2045_65_69_9,percent_reduction_inc_2045_65_69_10,
                                percent_reduction_inc_2045_65_69_11,percent_reduction_inc_2045_65_69_12,percent_reduction_inc_2045_65_69_13,percent_reduction_inc_2045_65_69_14,percent_reduction_inc_2045_65_69_15,
                                percent_reduction_inc_2045_65_69_16,percent_reduction_inc_2045_65_69_17,percent_reduction_inc_2045_65_69_18,percent_reduction_inc_2045_65_69_19,percent_reduction_inc_2045_65_69_20,
                                percent_reduction_inc_2045_65_69_21,percent_reduction_inc_2045_65_69_22,percent_reduction_inc_2045_65_69_23,percent_reduction_inc_2045_65_69_24,percent_reduction_inc_2045_65_69_25,
                                percent_reduction_inc_2045_65_69_26,percent_reduction_inc_2045_65_69_27,percent_reduction_inc_2045_65_69_28,percent_reduction_inc_2045_65_69_29,percent_reduction_inc_2045_65_69_30,
                                percent_reduction_inc_2045_65_69_31,percent_reduction_inc_2045_65_69_32,percent_reduction_inc_2045_65_69_33,percent_reduction_inc_2045_65_69_34,percent_reduction_inc_2045_65_69_35,
                                percent_reduction_inc_2045_65_69_36,percent_reduction_inc_2045_65_69_37,percent_reduction_inc_2045_65_69_38,percent_reduction_inc_2045_65_69_39,percent_reduction_inc_2045_65_69_40,
                                percent_reduction_inc_2045_65_69_41,percent_reduction_inc_2045_65_69_42,percent_reduction_inc_2045_65_69_43,percent_reduction_inc_2045_65_69_44,percent_reduction_inc_2045_65_69_45,
                                percent_reduction_inc_2045_65_69_46,percent_reduction_inc_2045_65_69_47,percent_reduction_inc_2045_65_69_48,percent_reduction_inc_2045_65_69_49,percent_reduction_inc_2045_65_69_50)
#####
percent_reduction_inc_2025_70_74<-c(percent_reduction_inc_2025_70_74_1,percent_reduction_inc_2025_70_74_2,percent_reduction_inc_2025_70_74_3,percent_reduction_inc_2025_70_74_4,percent_reduction_inc_2025_70_74_5,
                                percent_reduction_inc_2025_70_74_6,percent_reduction_inc_2025_70_74_7,percent_reduction_inc_2025_70_74_8,percent_reduction_inc_2025_70_74_9,percent_reduction_inc_2025_70_74_10,
                                percent_reduction_inc_2025_70_74_11,percent_reduction_inc_2025_70_74_12,percent_reduction_inc_2025_70_74_13,percent_reduction_inc_2025_70_74_14,percent_reduction_inc_2025_70_74_15,
                                percent_reduction_inc_2025_70_74_16,percent_reduction_inc_2025_70_74_17,percent_reduction_inc_2025_70_74_18,percent_reduction_inc_2025_70_74_19,percent_reduction_inc_2025_70_74_20,
                                percent_reduction_inc_2025_70_74_21,percent_reduction_inc_2025_70_74_22,percent_reduction_inc_2025_70_74_23,percent_reduction_inc_2025_70_74_24,percent_reduction_inc_2025_70_74_25,
                                percent_reduction_inc_2025_70_74_26,percent_reduction_inc_2025_70_74_27,percent_reduction_inc_2025_70_74_28,percent_reduction_inc_2025_70_74_29,percent_reduction_inc_2025_70_74_30,
                                percent_reduction_inc_2025_70_74_31,percent_reduction_inc_2025_70_74_32,percent_reduction_inc_2025_70_74_33,percent_reduction_inc_2025_70_74_34,percent_reduction_inc_2025_70_74_35,
                                percent_reduction_inc_2025_70_74_36,percent_reduction_inc_2025_70_74_37,percent_reduction_inc_2025_70_74_38,percent_reduction_inc_2025_70_74_39,percent_reduction_inc_2025_70_74_40,
                                percent_reduction_inc_2025_70_74_41,percent_reduction_inc_2025_70_74_42,percent_reduction_inc_2025_70_74_43,percent_reduction_inc_2025_70_74_44,percent_reduction_inc_2025_70_74_45,
                                percent_reduction_inc_2025_70_74_46,percent_reduction_inc_2025_70_74_47,percent_reduction_inc_2025_70_74_48,percent_reduction_inc_2025_70_74_49,percent_reduction_inc_2025_70_74_50)

percent_reduction_inc_2030_70_74<-c(percent_reduction_inc_2030_70_74_1,percent_reduction_inc_2030_70_74_2,percent_reduction_inc_2030_70_74_3,percent_reduction_inc_2030_70_74_4,percent_reduction_inc_2030_70_74_5,
                                percent_reduction_inc_2030_70_74_6,percent_reduction_inc_2030_70_74_7,percent_reduction_inc_2030_70_74_8,percent_reduction_inc_2030_70_74_9,percent_reduction_inc_2030_70_74_10,
                                percent_reduction_inc_2030_70_74_11,percent_reduction_inc_2030_70_74_12,percent_reduction_inc_2030_70_74_13,percent_reduction_inc_2030_70_74_14,percent_reduction_inc_2030_70_74_15,
                                percent_reduction_inc_2030_70_74_16,percent_reduction_inc_2030_70_74_17,percent_reduction_inc_2030_70_74_18,percent_reduction_inc_2030_70_74_19,percent_reduction_inc_2030_70_74_20,
                                percent_reduction_inc_2030_70_74_21,percent_reduction_inc_2030_70_74_22,percent_reduction_inc_2030_70_74_23,percent_reduction_inc_2030_70_74_24,percent_reduction_inc_2030_70_74_25,
                                percent_reduction_inc_2030_70_74_26,percent_reduction_inc_2030_70_74_27,percent_reduction_inc_2030_70_74_28,percent_reduction_inc_2030_70_74_29,percent_reduction_inc_2030_70_74_30,
                                percent_reduction_inc_2030_70_74_31,percent_reduction_inc_2030_70_74_32,percent_reduction_inc_2030_70_74_33,percent_reduction_inc_2030_70_74_34,percent_reduction_inc_2030_70_74_35,
                                percent_reduction_inc_2030_70_74_36,percent_reduction_inc_2030_70_74_37,percent_reduction_inc_2030_70_74_38,percent_reduction_inc_2030_70_74_39,percent_reduction_inc_2030_70_74_40,
                                percent_reduction_inc_2030_70_74_41,percent_reduction_inc_2030_70_74_42,percent_reduction_inc_2030_70_74_43,percent_reduction_inc_2030_70_74_44,percent_reduction_inc_2030_70_74_45,
                                percent_reduction_inc_2030_70_74_46,percent_reduction_inc_2030_70_74_47,percent_reduction_inc_2030_70_74_48,percent_reduction_inc_2030_70_74_49,percent_reduction_inc_2030_70_74_50)

percent_reduction_inc_2035_70_74<-c(percent_reduction_inc_2035_70_74_1,percent_reduction_inc_2035_70_74_2,percent_reduction_inc_2035_70_74_3,percent_reduction_inc_2035_70_74_4,percent_reduction_inc_2035_70_74_5,
                                percent_reduction_inc_2035_70_74_6,percent_reduction_inc_2035_70_74_7,percent_reduction_inc_2035_70_74_8,percent_reduction_inc_2035_70_74_9,percent_reduction_inc_2035_70_74_10,
                                percent_reduction_inc_2035_70_74_11,percent_reduction_inc_2035_70_74_12,percent_reduction_inc_2035_70_74_13,percent_reduction_inc_2035_70_74_14,percent_reduction_inc_2035_70_74_15,
                                percent_reduction_inc_2035_70_74_16,percent_reduction_inc_2035_70_74_17,percent_reduction_inc_2035_70_74_18,percent_reduction_inc_2035_70_74_19,percent_reduction_inc_2035_70_74_20,
                                percent_reduction_inc_2035_70_74_21,percent_reduction_inc_2035_70_74_22,percent_reduction_inc_2035_70_74_23,percent_reduction_inc_2035_70_74_24,percent_reduction_inc_2035_70_74_25,
                                percent_reduction_inc_2035_70_74_26,percent_reduction_inc_2035_70_74_27,percent_reduction_inc_2035_70_74_28,percent_reduction_inc_2035_70_74_29,percent_reduction_inc_2035_70_74_30,
                                percent_reduction_inc_2035_70_74_31,percent_reduction_inc_2035_70_74_32,percent_reduction_inc_2035_70_74_33,percent_reduction_inc_2035_70_74_34,percent_reduction_inc_2035_70_74_35,
                                percent_reduction_inc_2035_70_74_36,percent_reduction_inc_2035_70_74_37,percent_reduction_inc_2035_70_74_38,percent_reduction_inc_2035_70_74_39,percent_reduction_inc_2035_70_74_40,
                                percent_reduction_inc_2035_70_74_41,percent_reduction_inc_2035_70_74_42,percent_reduction_inc_2035_70_74_43,percent_reduction_inc_2035_70_74_44,percent_reduction_inc_2035_70_74_45,
                                percent_reduction_inc_2035_70_74_46,percent_reduction_inc_2035_70_74_47,percent_reduction_inc_2035_70_74_48,percent_reduction_inc_2035_70_74_49,percent_reduction_inc_2035_70_74_50)

percent_reduction_inc_2040_70_74<-c(percent_reduction_inc_2040_70_74_1,percent_reduction_inc_2040_70_74_2,percent_reduction_inc_2040_70_74_3,percent_reduction_inc_2040_70_74_4,percent_reduction_inc_2040_70_74_5,
                                percent_reduction_inc_2040_70_74_6,percent_reduction_inc_2040_70_74_7,percent_reduction_inc_2040_70_74_8,percent_reduction_inc_2040_70_74_9,percent_reduction_inc_2040_70_74_10,
                                percent_reduction_inc_2040_70_74_11,percent_reduction_inc_2040_70_74_12,percent_reduction_inc_2040_70_74_13,percent_reduction_inc_2040_70_74_14,percent_reduction_inc_2040_70_74_15,
                                percent_reduction_inc_2040_70_74_16,percent_reduction_inc_2040_70_74_17,percent_reduction_inc_2040_70_74_18,percent_reduction_inc_2040_70_74_19,percent_reduction_inc_2040_70_74_20,
                                percent_reduction_inc_2040_70_74_21,percent_reduction_inc_2040_70_74_22,percent_reduction_inc_2040_70_74_23,percent_reduction_inc_2040_70_74_24,percent_reduction_inc_2040_70_74_25,
                                percent_reduction_inc_2040_70_74_26,percent_reduction_inc_2040_70_74_27,percent_reduction_inc_2040_70_74_28,percent_reduction_inc_2040_70_74_29,percent_reduction_inc_2040_70_74_30,
                                percent_reduction_inc_2040_70_74_31,percent_reduction_inc_2040_70_74_32,percent_reduction_inc_2040_70_74_33,percent_reduction_inc_2040_70_74_34,percent_reduction_inc_2040_70_74_35,
                                percent_reduction_inc_2040_70_74_36,percent_reduction_inc_2040_70_74_37,percent_reduction_inc_2040_70_74_38,percent_reduction_inc_2040_70_74_39,percent_reduction_inc_2040_70_74_40,
                                percent_reduction_inc_2040_70_74_41,percent_reduction_inc_2040_70_74_42,percent_reduction_inc_2040_70_74_43,percent_reduction_inc_2040_70_74_44,percent_reduction_inc_2040_70_74_45,
                                percent_reduction_inc_2040_70_74_46,percent_reduction_inc_2040_70_74_47,percent_reduction_inc_2040_70_74_48,percent_reduction_inc_2040_70_74_49,percent_reduction_inc_2040_70_74_50)

percent_reduction_inc_2045_70_74<-c(percent_reduction_inc_2045_70_74_1,percent_reduction_inc_2045_70_74_2,percent_reduction_inc_2045_70_74_3,percent_reduction_inc_2045_70_74_4,percent_reduction_inc_2045_70_74_5,
                                percent_reduction_inc_2045_70_74_6,percent_reduction_inc_2045_70_74_7,percent_reduction_inc_2045_70_74_8,percent_reduction_inc_2045_70_74_9,percent_reduction_inc_2045_70_74_10,
                                percent_reduction_inc_2045_70_74_11,percent_reduction_inc_2045_70_74_12,percent_reduction_inc_2045_70_74_13,percent_reduction_inc_2045_70_74_14,percent_reduction_inc_2045_70_74_15,
                                percent_reduction_inc_2045_70_74_16,percent_reduction_inc_2045_70_74_17,percent_reduction_inc_2045_70_74_18,percent_reduction_inc_2045_70_74_19,percent_reduction_inc_2045_70_74_20,
                                percent_reduction_inc_2045_70_74_21,percent_reduction_inc_2045_70_74_22,percent_reduction_inc_2045_70_74_23,percent_reduction_inc_2045_70_74_24,percent_reduction_inc_2045_70_74_25,
                                percent_reduction_inc_2045_70_74_26,percent_reduction_inc_2045_70_74_27,percent_reduction_inc_2045_70_74_28,percent_reduction_inc_2045_70_74_29,percent_reduction_inc_2045_70_74_30,
                                percent_reduction_inc_2045_70_74_31,percent_reduction_inc_2045_70_74_32,percent_reduction_inc_2045_70_74_33,percent_reduction_inc_2045_70_74_34,percent_reduction_inc_2045_70_74_35,
                                percent_reduction_inc_2045_70_74_36,percent_reduction_inc_2045_70_74_37,percent_reduction_inc_2045_70_74_38,percent_reduction_inc_2045_70_74_39,percent_reduction_inc_2045_70_74_40,
                                percent_reduction_inc_2045_70_74_41,percent_reduction_inc_2045_70_74_42,percent_reduction_inc_2045_70_74_43,percent_reduction_inc_2045_70_74_44,percent_reduction_inc_2045_70_74_45,
                                percent_reduction_inc_2045_70_74_46,percent_reduction_inc_2045_70_74_47,percent_reduction_inc_2045_70_74_48,percent_reduction_inc_2045_70_74_49,percent_reduction_inc_2045_70_74_50)

#####
percent_reduction_inc_2025_75_79<-c(percent_reduction_inc_2025_75_79_1,percent_reduction_inc_2025_75_79_2,percent_reduction_inc_2025_75_79_3,percent_reduction_inc_2025_75_79_4,percent_reduction_inc_2025_75_79_5,
                                    percent_reduction_inc_2025_75_79_6,percent_reduction_inc_2025_75_79_7,percent_reduction_inc_2025_75_79_8,percent_reduction_inc_2025_75_79_9,percent_reduction_inc_2025_75_79_10,
                                    percent_reduction_inc_2025_75_79_11,percent_reduction_inc_2025_75_79_12,percent_reduction_inc_2025_75_79_13,percent_reduction_inc_2025_75_79_14,percent_reduction_inc_2025_75_79_15,
                                    percent_reduction_inc_2025_75_79_16,percent_reduction_inc_2025_75_79_17,percent_reduction_inc_2025_75_79_18,percent_reduction_inc_2025_75_79_19,percent_reduction_inc_2025_75_79_20,
                                    percent_reduction_inc_2025_75_79_21,percent_reduction_inc_2025_75_79_22,percent_reduction_inc_2025_75_79_23,percent_reduction_inc_2025_75_79_24,percent_reduction_inc_2025_75_79_25,
                                    percent_reduction_inc_2025_75_79_26,percent_reduction_inc_2025_75_79_27,percent_reduction_inc_2025_75_79_28,percent_reduction_inc_2025_75_79_29,percent_reduction_inc_2025_75_79_30,
                                    percent_reduction_inc_2025_75_79_31,percent_reduction_inc_2025_75_79_32,percent_reduction_inc_2025_75_79_33,percent_reduction_inc_2025_75_79_34,percent_reduction_inc_2025_75_79_35,
                                    percent_reduction_inc_2025_75_79_36,percent_reduction_inc_2025_75_79_37,percent_reduction_inc_2025_75_79_38,percent_reduction_inc_2025_75_79_39,percent_reduction_inc_2025_75_79_40,
                                    percent_reduction_inc_2025_75_79_41,percent_reduction_inc_2025_75_79_42,percent_reduction_inc_2025_75_79_43,percent_reduction_inc_2025_75_79_44,percent_reduction_inc_2025_75_79_45,
                                    percent_reduction_inc_2025_75_79_46,percent_reduction_inc_2025_75_79_47,percent_reduction_inc_2025_75_79_48,percent_reduction_inc_2025_75_79_49,percent_reduction_inc_2025_75_79_50)

percent_reduction_inc_2030_75_79<-c(percent_reduction_inc_2030_75_79_1,percent_reduction_inc_2030_75_79_2,percent_reduction_inc_2030_75_79_3,percent_reduction_inc_2030_75_79_4,percent_reduction_inc_2030_75_79_5,
                                    percent_reduction_inc_2030_75_79_6,percent_reduction_inc_2030_75_79_7,percent_reduction_inc_2030_75_79_8,percent_reduction_inc_2030_75_79_9,percent_reduction_inc_2030_75_79_10,
                                    percent_reduction_inc_2030_75_79_11,percent_reduction_inc_2030_75_79_12,percent_reduction_inc_2030_75_79_13,percent_reduction_inc_2030_75_79_14,percent_reduction_inc_2030_75_79_15,
                                    percent_reduction_inc_2030_75_79_16,percent_reduction_inc_2030_75_79_17,percent_reduction_inc_2030_75_79_18,percent_reduction_inc_2030_75_79_19,percent_reduction_inc_2030_75_79_20,
                                    percent_reduction_inc_2030_75_79_21,percent_reduction_inc_2030_75_79_22,percent_reduction_inc_2030_75_79_23,percent_reduction_inc_2030_75_79_24,percent_reduction_inc_2030_75_79_25,
                                    percent_reduction_inc_2030_75_79_26,percent_reduction_inc_2030_75_79_27,percent_reduction_inc_2030_75_79_28,percent_reduction_inc_2030_75_79_29,percent_reduction_inc_2030_75_79_30,
                                    percent_reduction_inc_2030_75_79_31,percent_reduction_inc_2030_75_79_32,percent_reduction_inc_2030_75_79_33,percent_reduction_inc_2030_75_79_34,percent_reduction_inc_2030_75_79_35,
                                    percent_reduction_inc_2030_75_79_36,percent_reduction_inc_2030_75_79_37,percent_reduction_inc_2030_75_79_38,percent_reduction_inc_2030_75_79_39,percent_reduction_inc_2030_75_79_40,
                                    percent_reduction_inc_2030_75_79_41,percent_reduction_inc_2030_75_79_42,percent_reduction_inc_2030_75_79_43,percent_reduction_inc_2030_75_79_44,percent_reduction_inc_2030_75_79_45,
                                    percent_reduction_inc_2030_75_79_46,percent_reduction_inc_2030_75_79_47,percent_reduction_inc_2030_75_79_48,percent_reduction_inc_2030_75_79_49,percent_reduction_inc_2030_75_79_50)

percent_reduction_inc_2035_75_79<-c(percent_reduction_inc_2035_75_79_1,percent_reduction_inc_2035_75_79_2,percent_reduction_inc_2035_75_79_3,percent_reduction_inc_2035_75_79_4,percent_reduction_inc_2035_75_79_5,
                                    percent_reduction_inc_2035_75_79_6,percent_reduction_inc_2035_75_79_7,percent_reduction_inc_2035_75_79_8,percent_reduction_inc_2035_75_79_9,percent_reduction_inc_2035_75_79_10,
                                    percent_reduction_inc_2035_75_79_11,percent_reduction_inc_2035_75_79_12,percent_reduction_inc_2035_75_79_13,percent_reduction_inc_2035_75_79_14,percent_reduction_inc_2035_75_79_15,
                                    percent_reduction_inc_2035_75_79_16,percent_reduction_inc_2035_75_79_17,percent_reduction_inc_2035_75_79_18,percent_reduction_inc_2035_75_79_19,percent_reduction_inc_2035_75_79_20,
                                    percent_reduction_inc_2035_75_79_21,percent_reduction_inc_2035_75_79_22,percent_reduction_inc_2035_75_79_23,percent_reduction_inc_2035_75_79_24,percent_reduction_inc_2035_75_79_25,
                                    percent_reduction_inc_2035_75_79_26,percent_reduction_inc_2035_75_79_27,percent_reduction_inc_2035_75_79_28,percent_reduction_inc_2035_75_79_29,percent_reduction_inc_2035_75_79_30,
                                    percent_reduction_inc_2035_75_79_31,percent_reduction_inc_2035_75_79_32,percent_reduction_inc_2035_75_79_33,percent_reduction_inc_2035_75_79_34,percent_reduction_inc_2035_75_79_35,
                                    percent_reduction_inc_2035_75_79_36,percent_reduction_inc_2035_75_79_37,percent_reduction_inc_2035_75_79_38,percent_reduction_inc_2035_75_79_39,percent_reduction_inc_2035_75_79_40,
                                    percent_reduction_inc_2035_75_79_41,percent_reduction_inc_2035_75_79_42,percent_reduction_inc_2035_75_79_43,percent_reduction_inc_2035_75_79_44,percent_reduction_inc_2035_75_79_45,
                                    percent_reduction_inc_2035_75_79_46,percent_reduction_inc_2035_75_79_47,percent_reduction_inc_2035_75_79_48,percent_reduction_inc_2035_75_79_49,percent_reduction_inc_2035_75_79_50)

percent_reduction_inc_2040_75_79<-c(percent_reduction_inc_2040_75_79_1,percent_reduction_inc_2040_75_79_2,percent_reduction_inc_2040_75_79_3,percent_reduction_inc_2040_75_79_4,percent_reduction_inc_2040_75_79_5,
                                    percent_reduction_inc_2040_75_79_6,percent_reduction_inc_2040_75_79_7,percent_reduction_inc_2040_75_79_8,percent_reduction_inc_2040_75_79_9,percent_reduction_inc_2040_75_79_10,
                                    percent_reduction_inc_2040_75_79_11,percent_reduction_inc_2040_75_79_12,percent_reduction_inc_2040_75_79_13,percent_reduction_inc_2040_75_79_14,percent_reduction_inc_2040_75_79_15,
                                    percent_reduction_inc_2040_75_79_16,percent_reduction_inc_2040_75_79_17,percent_reduction_inc_2040_75_79_18,percent_reduction_inc_2040_75_79_19,percent_reduction_inc_2040_75_79_20,
                                    percent_reduction_inc_2040_75_79_21,percent_reduction_inc_2040_75_79_22,percent_reduction_inc_2040_75_79_23,percent_reduction_inc_2040_75_79_24,percent_reduction_inc_2040_75_79_25,
                                    percent_reduction_inc_2040_75_79_26,percent_reduction_inc_2040_75_79_27,percent_reduction_inc_2040_75_79_28,percent_reduction_inc_2040_75_79_29,percent_reduction_inc_2040_75_79_30,
                                    percent_reduction_inc_2040_75_79_31,percent_reduction_inc_2040_75_79_32,percent_reduction_inc_2040_75_79_33,percent_reduction_inc_2040_75_79_34,percent_reduction_inc_2040_75_79_35,
                                    percent_reduction_inc_2040_75_79_36,percent_reduction_inc_2040_75_79_37,percent_reduction_inc_2040_75_79_38,percent_reduction_inc_2040_75_79_39,percent_reduction_inc_2040_75_79_40,
                                    percent_reduction_inc_2040_75_79_41,percent_reduction_inc_2040_75_79_42,percent_reduction_inc_2040_75_79_43,percent_reduction_inc_2040_75_79_44,percent_reduction_inc_2040_75_79_45,
                                    percent_reduction_inc_2040_75_79_46,percent_reduction_inc_2040_75_79_47,percent_reduction_inc_2040_75_79_48,percent_reduction_inc_2040_75_79_49,percent_reduction_inc_2040_75_79_50)

percent_reduction_inc_2045_75_79<-c(percent_reduction_inc_2045_75_79_1,percent_reduction_inc_2045_75_79_2,percent_reduction_inc_2045_75_79_3,percent_reduction_inc_2045_75_79_4,percent_reduction_inc_2045_75_79_5,
                                    percent_reduction_inc_2045_75_79_6,percent_reduction_inc_2045_75_79_7,percent_reduction_inc_2045_75_79_8,percent_reduction_inc_2045_75_79_9,percent_reduction_inc_2045_75_79_10,
                                    percent_reduction_inc_2045_75_79_11,percent_reduction_inc_2045_75_79_12,percent_reduction_inc_2045_75_79_13,percent_reduction_inc_2045_75_79_14,percent_reduction_inc_2045_75_79_15,
                                    percent_reduction_inc_2045_75_79_16,percent_reduction_inc_2045_75_79_17,percent_reduction_inc_2045_75_79_18,percent_reduction_inc_2045_75_79_19,percent_reduction_inc_2045_75_79_20,
                                    percent_reduction_inc_2045_75_79_21,percent_reduction_inc_2045_75_79_22,percent_reduction_inc_2045_75_79_23,percent_reduction_inc_2045_75_79_24,percent_reduction_inc_2045_75_79_25,
                                    percent_reduction_inc_2045_75_79_26,percent_reduction_inc_2045_75_79_27,percent_reduction_inc_2045_75_79_28,percent_reduction_inc_2045_75_79_29,percent_reduction_inc_2045_75_79_30,
                                    percent_reduction_inc_2045_75_79_31,percent_reduction_inc_2045_75_79_32,percent_reduction_inc_2045_75_79_33,percent_reduction_inc_2045_75_79_34,percent_reduction_inc_2045_75_79_35,
                                    percent_reduction_inc_2045_75_79_36,percent_reduction_inc_2045_75_79_37,percent_reduction_inc_2045_75_79_38,percent_reduction_inc_2045_75_79_39,percent_reduction_inc_2045_75_79_40,
                                    percent_reduction_inc_2045_75_79_41,percent_reduction_inc_2045_75_79_42,percent_reduction_inc_2045_75_79_43,percent_reduction_inc_2045_75_79_44,percent_reduction_inc_2045_75_79_45,
                                    percent_reduction_inc_2045_75_79_46,percent_reduction_inc_2045_75_79_47,percent_reduction_inc_2045_75_79_48,percent_reduction_inc_2045_75_79_49,percent_reduction_inc_2045_75_79_50)

#####
percent_reduction_inc_2025_80_84<-c(percent_reduction_inc_2025_80_84_1,percent_reduction_inc_2025_80_84_2,percent_reduction_inc_2025_80_84_3,percent_reduction_inc_2025_80_84_4,percent_reduction_inc_2025_80_84_5,
                                    percent_reduction_inc_2025_80_84_6,percent_reduction_inc_2025_80_84_7,percent_reduction_inc_2025_80_84_8,percent_reduction_inc_2025_80_84_9,percent_reduction_inc_2025_80_84_10,
                                    percent_reduction_inc_2025_80_84_11,percent_reduction_inc_2025_80_84_12,percent_reduction_inc_2025_80_84_13,percent_reduction_inc_2025_80_84_14,percent_reduction_inc_2025_80_84_15,
                                    percent_reduction_inc_2025_80_84_16,percent_reduction_inc_2025_80_84_17,percent_reduction_inc_2025_80_84_18,percent_reduction_inc_2025_80_84_19,percent_reduction_inc_2025_80_84_20,
                                    percent_reduction_inc_2025_80_84_21,percent_reduction_inc_2025_80_84_22,percent_reduction_inc_2025_80_84_23,percent_reduction_inc_2025_80_84_24,percent_reduction_inc_2025_80_84_25,
                                    percent_reduction_inc_2025_80_84_26,percent_reduction_inc_2025_80_84_27,percent_reduction_inc_2025_80_84_28,percent_reduction_inc_2025_80_84_29,percent_reduction_inc_2025_80_84_30,
                                    percent_reduction_inc_2025_80_84_31,percent_reduction_inc_2025_80_84_32,percent_reduction_inc_2025_80_84_33,percent_reduction_inc_2025_80_84_34,percent_reduction_inc_2025_80_84_35,
                                    percent_reduction_inc_2025_80_84_36,percent_reduction_inc_2025_80_84_37,percent_reduction_inc_2025_80_84_38,percent_reduction_inc_2025_80_84_39,percent_reduction_inc_2025_80_84_40,
                                    percent_reduction_inc_2025_80_84_41,percent_reduction_inc_2025_80_84_42,percent_reduction_inc_2025_80_84_43,percent_reduction_inc_2025_80_84_44,percent_reduction_inc_2025_80_84_45,
                                    percent_reduction_inc_2025_80_84_46,percent_reduction_inc_2025_80_84_47,percent_reduction_inc_2025_80_84_48,percent_reduction_inc_2025_80_84_49,percent_reduction_inc_2025_80_84_50)

percent_reduction_inc_2030_80_84<-c(percent_reduction_inc_2030_80_84_1,percent_reduction_inc_2030_80_84_2,percent_reduction_inc_2030_80_84_3,percent_reduction_inc_2030_80_84_4,percent_reduction_inc_2030_80_84_5,
                                    percent_reduction_inc_2030_80_84_6,percent_reduction_inc_2030_80_84_7,percent_reduction_inc_2030_80_84_8,percent_reduction_inc_2030_80_84_9,percent_reduction_inc_2030_80_84_10,
                                    percent_reduction_inc_2030_80_84_11,percent_reduction_inc_2030_80_84_12,percent_reduction_inc_2030_80_84_13,percent_reduction_inc_2030_80_84_14,percent_reduction_inc_2030_80_84_15,
                                    percent_reduction_inc_2030_80_84_16,percent_reduction_inc_2030_80_84_17,percent_reduction_inc_2030_80_84_18,percent_reduction_inc_2030_80_84_19,percent_reduction_inc_2030_80_84_20,
                                    percent_reduction_inc_2030_80_84_21,percent_reduction_inc_2030_80_84_22,percent_reduction_inc_2030_80_84_23,percent_reduction_inc_2030_80_84_24,percent_reduction_inc_2030_80_84_25,
                                    percent_reduction_inc_2030_80_84_26,percent_reduction_inc_2030_80_84_27,percent_reduction_inc_2030_80_84_28,percent_reduction_inc_2030_80_84_29,percent_reduction_inc_2030_80_84_30,
                                    percent_reduction_inc_2030_80_84_31,percent_reduction_inc_2030_80_84_32,percent_reduction_inc_2030_80_84_33,percent_reduction_inc_2030_80_84_34,percent_reduction_inc_2030_80_84_35,
                                    percent_reduction_inc_2030_80_84_36,percent_reduction_inc_2030_80_84_37,percent_reduction_inc_2030_80_84_38,percent_reduction_inc_2030_80_84_39,percent_reduction_inc_2030_80_84_40,
                                    percent_reduction_inc_2030_80_84_41,percent_reduction_inc_2030_80_84_42,percent_reduction_inc_2030_80_84_43,percent_reduction_inc_2030_80_84_44,percent_reduction_inc_2030_80_84_45,
                                    percent_reduction_inc_2030_80_84_46,percent_reduction_inc_2030_80_84_47,percent_reduction_inc_2030_80_84_48,percent_reduction_inc_2030_80_84_49,percent_reduction_inc_2030_80_84_50)

percent_reduction_inc_2035_80_84<-c(percent_reduction_inc_2035_80_84_1,percent_reduction_inc_2035_80_84_2,percent_reduction_inc_2035_80_84_3,percent_reduction_inc_2035_80_84_4,percent_reduction_inc_2035_80_84_5,
                                    percent_reduction_inc_2035_80_84_6,percent_reduction_inc_2035_80_84_7,percent_reduction_inc_2035_80_84_8,percent_reduction_inc_2035_80_84_9,percent_reduction_inc_2035_80_84_10,
                                    percent_reduction_inc_2035_80_84_11,percent_reduction_inc_2035_80_84_12,percent_reduction_inc_2035_80_84_13,percent_reduction_inc_2035_80_84_14,percent_reduction_inc_2035_80_84_15,
                                    percent_reduction_inc_2035_80_84_16,percent_reduction_inc_2035_80_84_17,percent_reduction_inc_2035_80_84_18,percent_reduction_inc_2035_80_84_19,percent_reduction_inc_2035_80_84_20,
                                    percent_reduction_inc_2035_80_84_21,percent_reduction_inc_2035_80_84_22,percent_reduction_inc_2035_80_84_23,percent_reduction_inc_2035_80_84_24,percent_reduction_inc_2035_80_84_25,
                                    percent_reduction_inc_2035_80_84_26,percent_reduction_inc_2035_80_84_27,percent_reduction_inc_2035_80_84_28,percent_reduction_inc_2035_80_84_29,percent_reduction_inc_2035_80_84_30,
                                    percent_reduction_inc_2035_80_84_31,percent_reduction_inc_2035_80_84_32,percent_reduction_inc_2035_80_84_33,percent_reduction_inc_2035_80_84_34,percent_reduction_inc_2035_80_84_35,
                                    percent_reduction_inc_2035_80_84_36,percent_reduction_inc_2035_80_84_37,percent_reduction_inc_2035_80_84_38,percent_reduction_inc_2035_80_84_39,percent_reduction_inc_2035_80_84_40,
                                    percent_reduction_inc_2035_80_84_41,percent_reduction_inc_2035_80_84_42,percent_reduction_inc_2035_80_84_43,percent_reduction_inc_2035_80_84_44,percent_reduction_inc_2035_80_84_45,
                                    percent_reduction_inc_2035_80_84_46,percent_reduction_inc_2035_80_84_47,percent_reduction_inc_2035_80_84_48,percent_reduction_inc_2035_80_84_49,percent_reduction_inc_2035_80_84_50)

percent_reduction_inc_2040_80_84<-c(percent_reduction_inc_2040_80_84_1,percent_reduction_inc_2040_80_84_2,percent_reduction_inc_2040_80_84_3,percent_reduction_inc_2040_80_84_4,percent_reduction_inc_2040_80_84_5,
                                    percent_reduction_inc_2040_80_84_6,percent_reduction_inc_2040_80_84_7,percent_reduction_inc_2040_80_84_8,percent_reduction_inc_2040_80_84_9,percent_reduction_inc_2040_80_84_10,
                                    percent_reduction_inc_2040_80_84_11,percent_reduction_inc_2040_80_84_12,percent_reduction_inc_2040_80_84_13,percent_reduction_inc_2040_80_84_14,percent_reduction_inc_2040_80_84_15,
                                    percent_reduction_inc_2040_80_84_16,percent_reduction_inc_2040_80_84_17,percent_reduction_inc_2040_80_84_18,percent_reduction_inc_2040_80_84_19,percent_reduction_inc_2040_80_84_20,
                                    percent_reduction_inc_2040_80_84_21,percent_reduction_inc_2040_80_84_22,percent_reduction_inc_2040_80_84_23,percent_reduction_inc_2040_80_84_24,percent_reduction_inc_2040_80_84_25,
                                    percent_reduction_inc_2040_80_84_26,percent_reduction_inc_2040_80_84_27,percent_reduction_inc_2040_80_84_28,percent_reduction_inc_2040_80_84_29,percent_reduction_inc_2040_80_84_30,
                                    percent_reduction_inc_2040_80_84_31,percent_reduction_inc_2040_80_84_32,percent_reduction_inc_2040_80_84_33,percent_reduction_inc_2040_80_84_34,percent_reduction_inc_2040_80_84_35,
                                    percent_reduction_inc_2040_80_84_36,percent_reduction_inc_2040_80_84_37,percent_reduction_inc_2040_80_84_38,percent_reduction_inc_2040_80_84_39,percent_reduction_inc_2040_80_84_40,
                                    percent_reduction_inc_2040_80_84_41,percent_reduction_inc_2040_80_84_42,percent_reduction_inc_2040_80_84_43,percent_reduction_inc_2040_80_84_44,percent_reduction_inc_2040_80_84_45,
                                    percent_reduction_inc_2040_80_84_46,percent_reduction_inc_2040_80_84_47,percent_reduction_inc_2040_80_84_48,percent_reduction_inc_2040_80_84_49,percent_reduction_inc_2040_80_84_50)

percent_reduction_inc_2045_80_84<-c(percent_reduction_inc_2045_80_84_1,percent_reduction_inc_2045_80_84_2,percent_reduction_inc_2045_80_84_3,percent_reduction_inc_2045_80_84_4,percent_reduction_inc_2045_80_84_5,
                                    percent_reduction_inc_2045_80_84_6,percent_reduction_inc_2045_80_84_7,percent_reduction_inc_2045_80_84_8,percent_reduction_inc_2045_80_84_9,percent_reduction_inc_2045_80_84_10,
                                    percent_reduction_inc_2045_80_84_11,percent_reduction_inc_2045_80_84_12,percent_reduction_inc_2045_80_84_13,percent_reduction_inc_2045_80_84_14,percent_reduction_inc_2045_80_84_15,
                                    percent_reduction_inc_2045_80_84_16,percent_reduction_inc_2045_80_84_17,percent_reduction_inc_2045_80_84_18,percent_reduction_inc_2045_80_84_19,percent_reduction_inc_2045_80_84_20,
                                    percent_reduction_inc_2045_80_84_21,percent_reduction_inc_2045_80_84_22,percent_reduction_inc_2045_80_84_23,percent_reduction_inc_2045_80_84_24,percent_reduction_inc_2045_80_84_25,
                                    percent_reduction_inc_2045_80_84_26,percent_reduction_inc_2045_80_84_27,percent_reduction_inc_2045_80_84_28,percent_reduction_inc_2045_80_84_29,percent_reduction_inc_2045_80_84_30,
                                    percent_reduction_inc_2045_80_84_31,percent_reduction_inc_2045_80_84_32,percent_reduction_inc_2045_80_84_33,percent_reduction_inc_2045_80_84_34,percent_reduction_inc_2045_80_84_35,
                                    percent_reduction_inc_2045_80_84_36,percent_reduction_inc_2045_80_84_37,percent_reduction_inc_2045_80_84_38,percent_reduction_inc_2045_80_84_39,percent_reduction_inc_2045_80_84_40,
                                    percent_reduction_inc_2045_80_84_41,percent_reduction_inc_2045_80_84_42,percent_reduction_inc_2045_80_84_43,percent_reduction_inc_2045_80_84_44,percent_reduction_inc_2045_80_84_45,
                                    percent_reduction_inc_2045_80_84_46,percent_reduction_inc_2045_80_84_47,percent_reduction_inc_2045_80_84_48,percent_reduction_inc_2045_80_84_49,percent_reduction_inc_2045_80_84_50)

#####
percent_reduction_inc_2025_40_84<-c(percent_reduction_inc_2025_40_84_1,percent_reduction_inc_2025_40_84_2,percent_reduction_inc_2025_40_84_3,percent_reduction_inc_2025_40_84_4,percent_reduction_inc_2025_40_84_5,
                                    percent_reduction_inc_2025_40_84_6,percent_reduction_inc_2025_40_84_7,percent_reduction_inc_2025_40_84_8,percent_reduction_inc_2025_40_84_9,percent_reduction_inc_2025_40_84_10,
                                    percent_reduction_inc_2025_40_84_11,percent_reduction_inc_2025_40_84_12,percent_reduction_inc_2025_40_84_13,percent_reduction_inc_2025_40_84_14,percent_reduction_inc_2025_40_84_15,
                                    percent_reduction_inc_2025_40_84_16,percent_reduction_inc_2025_40_84_17,percent_reduction_inc_2025_40_84_18,percent_reduction_inc_2025_40_84_19,percent_reduction_inc_2025_40_84_20,
                                    percent_reduction_inc_2025_40_84_21,percent_reduction_inc_2025_40_84_22,percent_reduction_inc_2025_40_84_23,percent_reduction_inc_2025_40_84_24,percent_reduction_inc_2025_40_84_25,
                                    percent_reduction_inc_2025_40_84_26,percent_reduction_inc_2025_40_84_27,percent_reduction_inc_2025_40_84_28,percent_reduction_inc_2025_40_84_29,percent_reduction_inc_2025_40_84_30,
                                    percent_reduction_inc_2025_40_84_31,percent_reduction_inc_2025_40_84_32,percent_reduction_inc_2025_40_84_33,percent_reduction_inc_2025_40_84_34,percent_reduction_inc_2025_40_84_35,
                                    percent_reduction_inc_2025_40_84_36,percent_reduction_inc_2025_40_84_37,percent_reduction_inc_2025_40_84_38,percent_reduction_inc_2025_40_84_39,percent_reduction_inc_2025_40_84_40,
                                    percent_reduction_inc_2025_40_84_41,percent_reduction_inc_2025_40_84_42,percent_reduction_inc_2025_40_84_43,percent_reduction_inc_2025_40_84_44,percent_reduction_inc_2025_40_84_45,
                                    percent_reduction_inc_2025_40_84_46,percent_reduction_inc_2025_40_84_47,percent_reduction_inc_2025_40_84_48,percent_reduction_inc_2025_40_84_49,percent_reduction_inc_2025_40_84_50)

percent_reduction_inc_2030_40_84<-c(percent_reduction_inc_2030_40_84_1,percent_reduction_inc_2030_40_84_2,percent_reduction_inc_2030_40_84_3,percent_reduction_inc_2030_40_84_4,percent_reduction_inc_2030_40_84_5,
                                    percent_reduction_inc_2030_40_84_6,percent_reduction_inc_2030_40_84_7,percent_reduction_inc_2030_40_84_8,percent_reduction_inc_2030_40_84_9,percent_reduction_inc_2030_40_84_10,
                                    percent_reduction_inc_2030_40_84_11,percent_reduction_inc_2030_40_84_12,percent_reduction_inc_2030_40_84_13,percent_reduction_inc_2030_40_84_14,percent_reduction_inc_2030_40_84_15,
                                    percent_reduction_inc_2030_40_84_16,percent_reduction_inc_2030_40_84_17,percent_reduction_inc_2030_40_84_18,percent_reduction_inc_2030_40_84_19,percent_reduction_inc_2030_40_84_20,
                                    percent_reduction_inc_2030_40_84_21,percent_reduction_inc_2030_40_84_22,percent_reduction_inc_2030_40_84_23,percent_reduction_inc_2030_40_84_24,percent_reduction_inc_2030_40_84_25,
                                    percent_reduction_inc_2030_40_84_26,percent_reduction_inc_2030_40_84_27,percent_reduction_inc_2030_40_84_28,percent_reduction_inc_2030_40_84_29,percent_reduction_inc_2030_40_84_30,
                                    percent_reduction_inc_2030_40_84_31,percent_reduction_inc_2030_40_84_32,percent_reduction_inc_2030_40_84_33,percent_reduction_inc_2030_40_84_34,percent_reduction_inc_2030_40_84_35,
                                    percent_reduction_inc_2030_40_84_36,percent_reduction_inc_2030_40_84_37,percent_reduction_inc_2030_40_84_38,percent_reduction_inc_2030_40_84_39,percent_reduction_inc_2030_40_84_40,
                                    percent_reduction_inc_2030_40_84_41,percent_reduction_inc_2030_40_84_42,percent_reduction_inc_2030_40_84_43,percent_reduction_inc_2030_40_84_44,percent_reduction_inc_2030_40_84_45,
                                    percent_reduction_inc_2030_40_84_46,percent_reduction_inc_2030_40_84_47,percent_reduction_inc_2030_40_84_48,percent_reduction_inc_2030_40_84_49,percent_reduction_inc_2030_40_84_50)

percent_reduction_inc_2035_40_84<-c(percent_reduction_inc_2035_40_84_1,percent_reduction_inc_2035_40_84_2,percent_reduction_inc_2035_40_84_3,percent_reduction_inc_2035_40_84_4,percent_reduction_inc_2035_40_84_5,
                                    percent_reduction_inc_2035_40_84_6,percent_reduction_inc_2035_40_84_7,percent_reduction_inc_2035_40_84_8,percent_reduction_inc_2035_40_84_9,percent_reduction_inc_2035_40_84_10,
                                    percent_reduction_inc_2035_40_84_11,percent_reduction_inc_2035_40_84_12,percent_reduction_inc_2035_40_84_13,percent_reduction_inc_2035_40_84_14,percent_reduction_inc_2035_40_84_15,
                                    percent_reduction_inc_2035_40_84_16,percent_reduction_inc_2035_40_84_17,percent_reduction_inc_2035_40_84_18,percent_reduction_inc_2035_40_84_19,percent_reduction_inc_2035_40_84_20,
                                    percent_reduction_inc_2035_40_84_21,percent_reduction_inc_2035_40_84_22,percent_reduction_inc_2035_40_84_23,percent_reduction_inc_2035_40_84_24,percent_reduction_inc_2035_40_84_25,
                                    percent_reduction_inc_2035_40_84_26,percent_reduction_inc_2035_40_84_27,percent_reduction_inc_2035_40_84_28,percent_reduction_inc_2035_40_84_29,percent_reduction_inc_2035_40_84_30,
                                    percent_reduction_inc_2035_40_84_31,percent_reduction_inc_2035_40_84_32,percent_reduction_inc_2035_40_84_33,percent_reduction_inc_2035_40_84_34,percent_reduction_inc_2035_40_84_35,
                                    percent_reduction_inc_2035_40_84_36,percent_reduction_inc_2035_40_84_37,percent_reduction_inc_2035_40_84_38,percent_reduction_inc_2035_40_84_39,percent_reduction_inc_2035_40_84_40,
                                    percent_reduction_inc_2035_40_84_41,percent_reduction_inc_2035_40_84_42,percent_reduction_inc_2035_40_84_43,percent_reduction_inc_2035_40_84_44,percent_reduction_inc_2035_40_84_45,
                                    percent_reduction_inc_2035_40_84_46,percent_reduction_inc_2035_40_84_47,percent_reduction_inc_2035_40_84_48,percent_reduction_inc_2035_40_84_49,percent_reduction_inc_2035_40_84_50)

percent_reduction_inc_2040_40_84<-c(percent_reduction_inc_2040_40_84_1,percent_reduction_inc_2040_40_84_2,percent_reduction_inc_2040_40_84_3,percent_reduction_inc_2040_40_84_4,percent_reduction_inc_2040_40_84_5,
                                    percent_reduction_inc_2040_40_84_6,percent_reduction_inc_2040_40_84_7,percent_reduction_inc_2040_40_84_8,percent_reduction_inc_2040_40_84_9,percent_reduction_inc_2040_40_84_10,
                                    percent_reduction_inc_2040_40_84_11,percent_reduction_inc_2040_40_84_12,percent_reduction_inc_2040_40_84_13,percent_reduction_inc_2040_40_84_14,percent_reduction_inc_2040_40_84_15,
                                    percent_reduction_inc_2040_40_84_16,percent_reduction_inc_2040_40_84_17,percent_reduction_inc_2040_40_84_18,percent_reduction_inc_2040_40_84_19,percent_reduction_inc_2040_40_84_20,
                                    percent_reduction_inc_2040_40_84_21,percent_reduction_inc_2040_40_84_22,percent_reduction_inc_2040_40_84_23,percent_reduction_inc_2040_40_84_24,percent_reduction_inc_2040_40_84_25,
                                    percent_reduction_inc_2040_40_84_26,percent_reduction_inc_2040_40_84_27,percent_reduction_inc_2040_40_84_28,percent_reduction_inc_2040_40_84_29,percent_reduction_inc_2040_40_84_30,
                                    percent_reduction_inc_2040_40_84_31,percent_reduction_inc_2040_40_84_32,percent_reduction_inc_2040_40_84_33,percent_reduction_inc_2040_40_84_34,percent_reduction_inc_2040_40_84_35,
                                    percent_reduction_inc_2040_40_84_36,percent_reduction_inc_2040_40_84_37,percent_reduction_inc_2040_40_84_38,percent_reduction_inc_2040_40_84_39,percent_reduction_inc_2040_40_84_40,
                                    percent_reduction_inc_2040_40_84_41,percent_reduction_inc_2040_40_84_42,percent_reduction_inc_2040_40_84_43,percent_reduction_inc_2040_40_84_44,percent_reduction_inc_2040_40_84_45,
                                    percent_reduction_inc_2040_40_84_46,percent_reduction_inc_2040_40_84_47,percent_reduction_inc_2040_40_84_48,percent_reduction_inc_2040_40_84_49,percent_reduction_inc_2040_40_84_50)

percent_reduction_inc_2045_40_84<-c(percent_reduction_inc_2045_40_84_1,percent_reduction_inc_2045_40_84_2,percent_reduction_inc_2045_40_84_3,percent_reduction_inc_2045_40_84_4,percent_reduction_inc_2045_40_84_5,
                                    percent_reduction_inc_2045_40_84_6,percent_reduction_inc_2045_40_84_7,percent_reduction_inc_2045_40_84_8,percent_reduction_inc_2045_40_84_9,percent_reduction_inc_2045_40_84_10,
                                    percent_reduction_inc_2045_40_84_11,percent_reduction_inc_2045_40_84_12,percent_reduction_inc_2045_40_84_13,percent_reduction_inc_2045_40_84_14,percent_reduction_inc_2045_40_84_15,
                                    percent_reduction_inc_2045_40_84_16,percent_reduction_inc_2045_40_84_17,percent_reduction_inc_2045_40_84_18,percent_reduction_inc_2045_40_84_19,percent_reduction_inc_2045_40_84_20,
                                    percent_reduction_inc_2045_40_84_21,percent_reduction_inc_2045_40_84_22,percent_reduction_inc_2045_40_84_23,percent_reduction_inc_2045_40_84_24,percent_reduction_inc_2045_40_84_25,
                                    percent_reduction_inc_2045_40_84_26,percent_reduction_inc_2045_40_84_27,percent_reduction_inc_2045_40_84_28,percent_reduction_inc_2045_40_84_29,percent_reduction_inc_2045_40_84_30,
                                    percent_reduction_inc_2045_40_84_31,percent_reduction_inc_2045_40_84_32,percent_reduction_inc_2045_40_84_33,percent_reduction_inc_2045_40_84_34,percent_reduction_inc_2045_40_84_35,
                                    percent_reduction_inc_2045_40_84_36,percent_reduction_inc_2045_40_84_37,percent_reduction_inc_2045_40_84_38,percent_reduction_inc_2045_40_84_39,percent_reduction_inc_2045_40_84_40,
                                    percent_reduction_inc_2045_40_84_41,percent_reduction_inc_2045_40_84_42,percent_reduction_inc_2045_40_84_43,percent_reduction_inc_2045_40_84_44,percent_reduction_inc_2045_40_84_45,
                                    percent_reduction_inc_2045_40_84_46,percent_reduction_inc_2045_40_84_47,percent_reduction_inc_2045_40_84_48,percent_reduction_inc_2045_40_84_49,percent_reduction_inc_2045_40_84_50)

#####

rownames<-c("2025_40_44","2025_45_49","2025_50_54","2025_55_59","2025_60_64","2025_65_69","2025_70_74","2025_75_79","2025_80_84","2025_40_84",
            "2030_40_44","2030_45_49","2030_50_54","2030_55_59","2030_60_64","2030_65_69","2030_70_74","2030_75_79","2030_80_84","2030_40_84",
            "2035_40_44","2035_45_49","2035_50_54","2035_55_59","2035_60_64","2035_65_69","2035_70_74","2035_75_79","2035_80_84","2035_40_84",
            "2040_40_44","2040_45_49","2040_50_54","2040_55_59","2040_60_64","2040_65_69","2040_70_74","2040_75_79","2040_80_84","2040_40_84",
            "2045_40_44","2045_45_49","2045_50_54","2045_55_59","2045_60_64","2045_65_69","2045_70_74","2045_75_79","2045_80_84","2045_40_84")

output2<-rbind(percent_reduction_inc_2025_40_44,percent_reduction_inc_2025_45_49, percent_reduction_inc_2025_50_54, percent_reduction_inc_2025_55_59, percent_reduction_inc_2025_60_64, percent_reduction_inc_2025_65_69, percent_reduction_inc_2025_70_74,percent_reduction_inc_2025_75_79, percent_reduction_inc_2025_80_84,percent_reduction_inc_2025_40_84,
              percent_reduction_inc_2030_40_44,percent_reduction_inc_2030_45_49, percent_reduction_inc_2030_50_54, percent_reduction_inc_2030_55_59, percent_reduction_inc_2030_60_64, percent_reduction_inc_2030_65_69, percent_reduction_inc_2030_70_74, percent_reduction_inc_2030_75_79, percent_reduction_inc_2030_80_84,percent_reduction_inc_2030_40_84,
              percent_reduction_inc_2035_40_44,percent_reduction_inc_2035_45_49, percent_reduction_inc_2035_50_54, percent_reduction_inc_2035_55_59, percent_reduction_inc_2035_60_64, percent_reduction_inc_2035_65_69, percent_reduction_inc_2035_70_74, percent_reduction_inc_2035_75_79, percent_reduction_inc_2035_80_84,percent_reduction_inc_2035_40_84,
              percent_reduction_inc_2040_40_44,percent_reduction_inc_2040_45_49, percent_reduction_inc_2040_50_54, percent_reduction_inc_2040_55_59, percent_reduction_inc_2040_60_64, percent_reduction_inc_2040_65_69, percent_reduction_inc_2040_70_74, percent_reduction_inc_2040_75_79, percent_reduction_inc_2040_80_84,percent_reduction_inc_2040_40_84,
              percent_reduction_inc_2045_40_44,percent_reduction_inc_2045_45_49, percent_reduction_inc_2045_50_54, percent_reduction_inc_2045_55_59, percent_reduction_inc_2045_60_64, percent_reduction_inc_2045_65_69, percent_reduction_inc_2045_70_74, percent_reduction_inc_2045_75_79, percent_reduction_inc_2045_80_84,percent_reduction_inc_2045_40_84)
output2<-as.data.frame(output2)

setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")

fwrite(output2,"percent_reduction_inc_vs_SQ26_2021.csv")

