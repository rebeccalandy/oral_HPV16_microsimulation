# create weights for men aged 80-84 using NHIS data
# rm(list=ls(all=TRUE))   ### Clears existing dataset/code from R
# library(data.table)
# library(haven)


nhis2009<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2009/samadult.dta")
nhis2009small<-dplyr::select(nhis2009, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa" ))
nhis2010<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2010/samadult.dta")
nhis2010small<-dplyr::select(nhis2010, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa"  ))
nhis2011<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2011/samadult.dta")
nhis2011small<-dplyr::select(nhis2011, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa"  ))
nhis2012<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2012/samadult.dta")
nhis2012small<-dplyr::select(nhis2012, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa"  ))
nhis2013<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2013/samadult.dta")
nhis2013small<-dplyr::select(nhis2013, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa"  ))
nhis2014<-read_dta("L:/DCEG_ALL/Rebecca Landy/Oropharyngeal/NHISdata/2014/samadult.dta")
nhis2014small<-dplyr::select(nhis2014, c("age_p", "sex", "smkstat2", "hispan_i","mracrpi2","srvy_yr","wtfa_sa"  ))

nhis2015<-fread("L:/DCEG_ALL/Rebecca Landy/NHISdata/NHISdata2015/samadult.csv")
nhis2015small<-dplyr::select(nhis2015, c("AGE_P", "SEX", "SMKSTAT2", "HISPAN_I","MRACRPI2","SRVY_YR","WTFA_SA"  ))
nhis2015small<-rename(nhis2015small, age_p = AGE_P)
nhis2015small<-rename(nhis2015small, sex = SEX)
nhis2015small<-rename(nhis2015small, smkstat2 = SMKSTAT2)
nhis2015small<-rename(nhis2015small, hispan_i = HISPAN_I)
nhis2015small<-rename(nhis2015small, mracrpi2 = MRACRPI2)
nhis2015small<-rename(nhis2015small, srvy_yr = SRVY_YR)
nhis2015small<-rename(nhis2015small, wtfa_sa = WTFA_SA)

nhis2016<-fread("L:/DCEG_ALL/Rebecca Landy/NHISdata/NHISdata2016/samadult.csv")
nhis2016small<-dplyr::select(nhis2016, c("AGE_P", "SEX", "SMKSTAT2", "HISPAN_I","MRACRPI2","SRVY_YR","WTFA_SA"  ))
nhis2016small<-rename(nhis2016small, age_p = AGE_P)
nhis2016small<-rename(nhis2016small, sex = SEX)
nhis2016small<-rename(nhis2016small, smkstat2 = SMKSTAT2)
nhis2016small<-rename(nhis2016small, hispan_i = HISPAN_I)
nhis2016small<-rename(nhis2016small, mracrpi2 = MRACRPI2)
nhis2016small<-rename(nhis2016small, srvy_yr = SRVY_YR)
nhis2016small<-rename(nhis2016small, wtfa_sa = WTFA_SA)

rm(nhis2009,nhis2010)
rm(nhis2011,nhis2012,nhis2013,nhis2014,nhis2015,nhis2016)

nhis2009_16<-rbind(nhis2009small,nhis2010small,nhis2011small,nhis2012small,nhis2013small,nhis2014small,nhis2015small,nhis2016small)
nhis2009_16<-dplyr::filter(nhis2009_16, sex==1)

# racecat=1: hispanic
# racecat=2: white
# racecat=3: black
# racecat=4: other;

nhis2009_16$racecat<-ifelse(nhis2009_16$hispan_i>=0 & nhis2009_16$hispan_i<12,1,
                            ifelse(nhis2009_16$mracrpi2==1,2,
                                   ifelse(nhis2009_16$mracrpi2==2,3,4)))

#current smokers
nhis2009_16$currentsmoking<-ifelse(nhis2009_16$smkstat2>=1 & nhis2009_16$smkstat2<=2,1,
                                  ifelse(nhis2009_16$smkstat2>=5,NA,0))
table(nhis2009_16$smkstat2, nhis2009_16$currentsmoking,useNA = "always")

nhis2009_16$race.smoking<-ifelse(nhis2009_16$racecat==1 & nhis2009_16$currentsmoking==0,1,
                                ifelse(nhis2009_16$racecat==2 & nhis2009_16$currentsmoking==0,2,
                                       ifelse(nhis2009_16$racecat==3 & nhis2009_16$currentsmoking==0,3,
                                              ifelse(nhis2009_16$racecat==4 & nhis2009_16$currentsmoking==0,4,
                                                     ifelse(nhis2009_16$racecat==1 & nhis2009_16$currentsmoking==1,5,
                                                            ifelse(nhis2009_16$racecat==2 & nhis2009_16$currentsmoking==1,6,
                                                                   ifelse(nhis2009_16$racecat==3 & nhis2009_16$currentsmoking==1,7,
                                                                          ifelse(nhis2009_16$racecat==4 & nhis2009_16$currentsmoking==1,8,NA))))))))

rm(nhis2009small)
rm(nhis2010small)
rm(nhis2011small)
rm(nhis2012small)
rm(nhis2013small)
rm(nhis2014small)
rm(nhis2015small)
rm(nhis2016small)
