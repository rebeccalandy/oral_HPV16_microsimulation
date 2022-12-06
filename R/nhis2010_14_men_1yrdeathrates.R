rm(list=ls(all=TRUE)) 
library(survey)
setwd('~/Desktop/Lung cancer/lrisk/other/nhis2010_12/')
load(file="nhis2010.Rdata")
load(file="nhis2011.Rdata")
load(file="nhis2012.Rdata")
setwd('~/Desktop/Lung cancer/lrisk/other/nhis2013_15/')
load(file="nhis2013.Rdata")
load(file="nhis2014.Rdata")

nhis2010_14 <- rbind(nhis2010,nhis2011,nhis2012,nhis2013,nhis2014)
nhis2010_14$adj.wt_mort <- nhis2010_14$wt_mort/5 # ADJUSTED FOR POOLED ANALYSIS
nhis2010_14$flwup_time <- pmax(0.125,nhis2010_14$deathyear - nhis2010_14$intyear)

nhis2010_14$age.cat <- cut(nhis2010_14$age, breaks=c(17,19,24,29,34,39,44,49,54,59,64,69,74,79,85))
nhis2010_14$race4 <- ifelse(is.na(nhis2010_14$race)==0,
                            ifelse(nhis2010_14$race=="Non-Hispanic White","Non-Hispanic White",
                                   ifelse(nhis2010_14$race=="Non-Hispanic Black","Non-Hispanic Black",
                                          ifelse(nhis2010_14$race=="Hispanic","Hispanic","Other"))),"Other")
nhis2010_14$smokstat <- ifelse(nhis2010_14$never==1,"Never",
                               ifelse(nhis2010_14$former==1,"Former",
                                      ifelse(nhis2010_14$current==1,"Current","Unknown")))
nhis2010_14$group <- interaction(nhis2010_14$age.cat, nhis2010_14$race4, nhis2010_14$smokstat, drop=TRUE)
nhis2010_14$died_1yr <- ifelse(nhis2010_14$died==1 & nhis2010_14$flwup_time<=1,1,0)
rawtot <- table(nhis2010_14$group[nhis2010_14$female==0],nhis2010_14$died_1yr[nhis2010_14$female==0])
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis2010_14, nest=TRUE)
men <- subset(master, female==0)  #change to men
wghttot <- svytable(~group+died_1yr,men)
tab <- cbind(rowSums(rawtot),rawtot,rowSums(wghttot),wghttot,wghttot[,2]/rowSums(wghttot))
tab <- tab[which(tab[,1]>0),]
colnames(tab) <- c("Total","Alive","Dead","Weighted Total","Weighted Alive","Weighted Dead","1 year Death Rate")
write.table(tab,file="~/Desktop/nhis/other/nhis2010_14_1yrdeath_in_men.csv",sep=",")