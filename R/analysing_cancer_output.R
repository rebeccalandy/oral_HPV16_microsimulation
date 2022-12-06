# Going through all output files
rm(list=ls(all=TRUE)) 
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")
library(data.table)
seeds<-fread("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/calibrated_seeds.csv")

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

Nint<-250
for (vac_scen in c(5:8,45:69)) {
    for (k in 1:50) {
seed<-seeds[[k,1]]    
print(vac_scen)
atrisktotal<-fread(paste0("at_risk_means_",vac_scen,"_",seed,"_",Nint,".csv"))
cancerstotal<-fread(paste0("cancer_means_",vac_scen,"_",seed,"_",Nint,".csv"))
cancerinctotal<-fread(paste0("cancer_incidence_means_",vac_scen,"_",seed,"_",Nint,".csv"))
vaccinated<-fread(paste0("vaccine_means_",vac_scen,"_",seed,"_",Nint,".csv"))

vaccinated$total=rowSums(vaccinated)

# for the number vaccinated from 2021 onwards, 
# first row is 1937 birth cohort, last row is 2006 
# first row, just the 70th
# second row, sum 69th + 70th


vaccinated_temp<-c(rep(NA,70))
for (i in 1:70) {
  j=70-i+1
  vaccinated_temp[i]<-sum(vaccinated[i, j :70])
}


cancers_temp<-c(rep(NA,70))
for (i in 1:70) {
  j=70-i+1
  cancers_temp[i]<-sum(cancerstotal[i, j :70])
}

Natrisk_temp<-c(rep(NA,70))
for (i in 1:70) {
  j=70-i+1
  Natrisk_temp[i]<-sum(atrisktotal[i, j:j])
}


Total2021<-as.data.frame(cbind(birthcohort,vaccinated_temp,
                               cancers_temp, Natrisk_temp,population_estimate))

Total2021$total_cancers<-Total2021$population_estimate*Total2021$cancers_temp/Total2021$Natrisk_temp
Total2021$total_vaccinated<-Total2021$population_estimate*Total2021$vaccinated_temp/Total2021$Natrisk_temp

colnames(Total2021)<-c("Birth cohort", "N vaccinated","Total cancers",
                       "N at risk total","Population estimate","total_cancers_weighted",
                       "total_vaccinated_weighted")

Ncancers<-sum(Total2021$total_cancers_weighted)
Nvaccinated<-sum(Total2021$total_vaccinated_weighted)

Ncancers1937.1975<-sum(Total2021$total_cancers_weighted[Total2021$`Birth cohort`>=1937 & Total2021$`Birth cohort`<=1975])
Ncancers1976.1994<-sum(Total2021$total_cancers_weighted[Total2021$`Birth cohort`>=1976 & Total2021$`Birth cohort`<=1994])
Ncancers1995.2006<-sum(Total2021$total_cancers_weighted[Total2021$`Birth cohort`>=1995 & Total2021$`Birth cohort`<=2006])
Nvacc1937.1975<-sum(Total2021$total_vaccinated_weighted[Total2021$`Birth cohort`>=1937 & Total2021$`Birth cohort`<=1975])
Nvacc1976.1994<-sum(Total2021$total_vaccinated_weighted[Total2021$`Birth cohort`>=1976 & Total2021$`Birth cohort`<=1994])
Nvacc1995.2006<-sum(Total2021$total_vaccinated_weighted[Total2021$`Birth cohort`>=1995 & Total2021$`Birth cohort`<=2006])

output3<-as.data.frame(cbind(seed, vac_scen , Ncancers, Nvaccinated,Ncancers1937.1975,Ncancers1976.1994,Ncancers1995.2006,Nvacc1937.1975,Nvacc1976.1994,Nvacc1995.2006))
fwrite(output3, "combined_output3.csv", append=TRUE)
    }
}

# producing summary results

# select one natural history, and calculate difference in N vaccinated and N cancers
rm(list=ls(all=TRUE)) 
setwd("L:/DCEG_ALL/Rebecca Landy/microsim-risk_by_subcohort_ageextension/Results/cancer")
results<-fread("combined_output3.csv")

Nvacc.Novacc<-summary(results$Nvaccinated[results$vac_scen==55])[4]
Nvacc.SQ45<-summary(results$Nvaccinated[results$vac_scen==50])[4]
Nvacc.SQ26<-summary(results$Nvaccinated[results$vac_scen==51])[4]
Nvacc.SQ30<-summary(results$Nvaccinated[results$vac_scen==52])[4]
Nvacc.SQ35<-summary(results$Nvaccinated[results$vac_scen==53])[4]
Nvacc.SQ40<-summary(results$Nvaccinated[results$vac_scen==54])[4]
Nvacc.overkill.30<-summary(results$Nvaccinated[results$vac_scen==45])[4]
Nvacc.overkill.35<-summary(results$Nvaccinated[results$vac_scen==46])[4]
Nvacc.overkill.40<-summary(results$Nvaccinated[results$vac_scen==47])[4]
Nvacc.overkill.45<-summary(results$Nvaccinated[results$vac_scen==48])[4]
Nvacc.overkill.26<-summary(results$Nvaccinated[results$vac_scen==49])[4]
Nvacc.optimise.30<-summary(results$Nvaccinated[results$vac_scen==5])[4]
Nvacc.optimise.35<-summary(results$Nvaccinated[results$vac_scen==6])[4]
Nvacc.optimise.40<-summary(results$Nvaccinated[results$vac_scen==7])[4]
Nvacc.optimise.45<-summary(results$Nvaccinated[results$vac_scen==8])[4]

Ncanc.Novacc<-summary(results$Ncancers[results$vac_scen==55])[4]
Ncanc.SQ45<-summary(results$Ncancers[results$vac_scen==50])[4]
Ncanc.SQ26<-summary(results$Ncancers[results$vac_scen==51])[4]
Ncanc.SQ30<-summary(results$Ncancers[results$vac_scen==52])[4]
Ncanc.SQ35<-summary(results$Ncancers[results$vac_scen==53])[4]
Ncanc.SQ40<-summary(results$Ncancers[results$vac_scen==54])[4]
Ncanc.overkill.30<-summary(results$Ncancers[results$vac_scen==45])[4]
Ncanc.overkill.35<-summary(results$Ncancers[results$vac_scen==46])[4]
Ncanc.overkill.40<-summary(results$Ncancers[results$vac_scen==47])[4]
Ncanc.overkill.45<-summary(results$Ncancers[results$vac_scen==48])[4]
Ncanc.overkill.26<-summary(results$Ncancers[results$vac_scen==49])[4]
Ncanc.optimise.30<-summary(results$Ncancers[results$vac_scen==5])[4]
Ncanc.optimise.35<-summary(results$Ncancers[results$vac_scen==6])[4]
Ncanc.optimise.40<-summary(results$Ncancers[results$vac_scen==7])[4]
Ncanc.optimise.45<-summary(results$Ncancers[results$vac_scen==8])[4]

output<-rbind(c(Nvacc.Novacc,Ncanc.Novacc),
              c(Nvacc.SQ26, Ncanc.SQ26),
              c(Nvacc.SQ30, Ncanc.SQ30),
              c(Nvacc.SQ35, Ncanc.SQ35),
              c(Nvacc.SQ40, Ncanc.SQ40),
              c(Nvacc.SQ45, Ncanc.SQ45),
              c(Nvacc.overkill.26, Ncanc.overkill.26),
              c(Nvacc.overkill.30, Ncanc.overkill.30),
              c(Nvacc.overkill.35, Ncanc.overkill.35),
              c(Nvacc.overkill.40, Ncanc.overkill.40),
              c(Nvacc.overkill.45, Ncanc.overkill.45),
              c(Nvacc.optimise.30, Ncanc.optimise.30),
              c(Nvacc.optimise.35, Ncanc.optimise.35),
              c(Nvacc.optimise.40, Ncanc.optimise.40),
              c(Nvacc.optimise.45, Ncanc.optimise.45))

rownames(output)<-c("No vacc","SQ26","SQ30","SQ35","SQ40","SQ45","overkill26","overkill30","overkill35","overkill40","overkill45",
                    "optimise30","optimise35","optimise40","optimise45")              


#####

Ncanc.Novacc.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==55])[4]
Ncanc.SQ45.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==50])[4]
Ncanc.SQ26.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==51])[4]
Ncanc.SQ30.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==52])[4]
Ncanc.SQ35.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==53])[4]
Ncanc.SQ40.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==54])[4]
Ncanc.overkill.30.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==45])[4]
Ncanc.overkill.35.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==46])[4]
Ncanc.overkill.40.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==47])[4]
Ncanc.overkill.45.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==48])[4]
Ncanc.overkill.26.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==49])[4]
Ncanc.optimise.30.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==5])[4]
Ncanc.optimise.35.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==6])[4]
Ncanc.optimise.40.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==7])[4]
Ncanc.optimise.45.37.75<-summary(results$Ncancers1937.1975[results$vac_scen==8])[4]


output37.75<-rbind(c(Nvacc.Novacc,Ncanc.Novacc.37.75),
                   c(Nvacc.SQ26, Ncanc.SQ26.37.75),
                   c(Nvacc.SQ30, Ncanc.SQ30.37.75),
                   c(Nvacc.SQ35, Ncanc.SQ35.37.75),
                   c(Nvacc.SQ40, Ncanc.SQ40.37.75),
                   c(Nvacc.SQ45, Ncanc.SQ45.37.75),
                   c(Nvacc.overkill.26, Ncanc.overkill.26.37.75),
                   c(Nvacc.overkill.30, Ncanc.overkill.30.37.75),
                   c(Nvacc.overkill.35, Ncanc.overkill.35.37.75),
                   c(Nvacc.overkill.40, Ncanc.overkill.40.37.75),
                   c(Nvacc.overkill.45, Ncanc.overkill.45.37.75),
                   c(Nvacc.optimise.30, Ncanc.optimise.30.37.75),
                   c(Nvacc.optimise.35, Ncanc.optimise.35.37.75),
                   c(Nvacc.optimise.40, Ncanc.optimise.40.37.75),
                   c(Nvacc.optimise.45, Ncanc.optimise.45.37.75))


rownames(output37.75)<-c("No vacc","SQ26","SQ30","SQ35","SQ40","SQ45","overkill26","overkill30","overkill35","overkill40","overkill45",
                         "optimise30.37.75","optimise35.37.75","optimise40.37.75","optimise45.37.75")              

Ncanc.Novacc.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==55])[4]
Ncanc.SQ45.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==50])[4]
Ncanc.SQ26.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==51])[4]
Ncanc.SQ30.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==52])[4]
Ncanc.SQ35.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==53])[4]
Ncanc.SQ40.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==54])[4]
Ncanc.overkill.30.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==45])[4]
Ncanc.overkill.35.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==46])[4]
Ncanc.overkill.40.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==47])[4]
Ncanc.overkill.45.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==48])[4]
Ncanc.overkill.26.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==49])[4]
Ncanc.optimise.30.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==5])[4]
Ncanc.optimise.35.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==6])[4]
Ncanc.optimise.40.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==7])[4]
Ncanc.optimise.45.76.94<-summary(results$Ncancers1976.1994[results$vac_scen==8])[4]

output76.94<-rbind(c(Nvacc.Novacc,Ncanc.Novacc.76.94),
                   c(Nvacc.SQ26, Ncanc.SQ26.76.94),
                   c(Nvacc.SQ30, Ncanc.SQ30.76.94),
                   c(Nvacc.SQ35, Ncanc.SQ35.76.94),
                   c(Nvacc.SQ40, Ncanc.SQ40.76.94),
                   c(Nvacc.SQ45, Ncanc.SQ45.76.94),
                   c(Nvacc.overkill.26, Ncanc.overkill.26.76.94),
                   c(Nvacc.overkill.30, Ncanc.overkill.30.76.94),
                   c(Nvacc.overkill.35, Ncanc.overkill.35.76.94),
                   c(Nvacc.overkill.40, Ncanc.overkill.40.76.94),
                   c(Nvacc.overkill.45, Ncanc.overkill.45.76.94),
                   c(Nvacc.optimise.30, Ncanc.optimise.30.76.94),
                   c(Nvacc.optimise.35, Ncanc.optimise.35.76.94),
                   c(Nvacc.optimise.40, Ncanc.optimise.40.76.94),
                   c(Nvacc.optimise.45, Ncanc.optimise.45.76.94))


rownames(output76.94)<-c("No vacc","SQ26","SQ30","SQ35","SQ40","SQ45","overkill26","overkill30","overkill35","overkill40","overkill45",              
"optimise30.76.94","optimise35.76.94","optimise40.76.94","optimise45.76.94")              

Ncanc.Novacc.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==55])[4]
Ncanc.SQ45.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==50])[4]
Ncanc.SQ26.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==51])[4]
Ncanc.SQ30.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==52])[4]
Ncanc.SQ35.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==53])[4]
Ncanc.SQ40.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==54])[4]
Ncanc.overkill.30.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==45])[4]
Ncanc.overkill.35.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==46])[4]
Ncanc.overkill.40.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==47])[4]
Ncanc.overkill.45.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==48])[4]
Ncanc.overkill.26.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==49])[4]
Ncanc.optimise.30.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==5])[4]
Ncanc.optimise.35.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==6])[4]
Ncanc.optimise.40.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==7])[4]
Ncanc.optimise.45.95.06<-summary(results$Ncancers1995.2006[results$vac_scen==8])[4]

output95.06<-rbind(c(Nvacc.Novacc,Ncanc.Novacc.95.06),
                   c(Nvacc.SQ26, Ncanc.SQ26.95.06),
                   c(Nvacc.SQ30, Ncanc.SQ30.95.06),
                   c(Nvacc.SQ35, Ncanc.SQ35.95.06),
                   c(Nvacc.SQ40, Ncanc.SQ40.95.06),
                   c(Nvacc.SQ45, Ncanc.SQ45.95.06),
                   c(Nvacc.overkill.26, Ncanc.overkill.26.95.06),
                   c(Nvacc.overkill.30, Ncanc.overkill.30.95.06),
                   c(Nvacc.overkill.35, Ncanc.overkill.35.95.06),
                   c(Nvacc.overkill.40, Ncanc.overkill.40.95.06),
                   c(Nvacc.overkill.45, Ncanc.overkill.45.95.06),
                   c(Nvacc.optimise.30, Ncanc.optimise.30.95.06),
                   c(Nvacc.optimise.35, Ncanc.optimise.35.95.06),
                   c(Nvacc.optimise.40, Ncanc.optimise.40.95.06),
                   c(Nvacc.optimise.45, Ncanc.optimise.45.95.06))

rownames(output95.06)<-c("No vacc","SQ26","SQ30","SQ35","SQ40","SQ45","overkill26","overkill30","overkill35","overkill40","overkill45",              
                         "optimise30.95.06","optimise35.95.06","optimise40.95.06","optimise45.95.06")              




