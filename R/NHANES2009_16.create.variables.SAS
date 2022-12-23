title 'make.hpv4cycle.jan2020.sas';

libname demof xport 'DEMO_F.xpt';
libname ohpvf xport 'ORHPV_F.XPT';
libname alqf xport 'ALQ_F.XPT';
libname duqf xport ‘DUQ_F.XPT';
libname imqf xport 'IMQ_F.XPT';
libname rhqf xport 'RHQ_F.XPT';
libname smqf xport 'SMQ_F.XPT';
libname smqrtuf xport 'SMQRTU_F.XPT';
libname sxqf xport 'SXQ_F.XPT';

libname demog xport 'DEMO_G.XPT';
libname ohpvg xport 'ORHPV_G.XPT';
libname alqg xport 'ALQ_G.XPT';
libname duqg xport 'DUQ_G.XPT';
libname imqg xport 'IMQ_G.XPT';
libname rhqg xport 'RHQ_G.XPT';
libname smqg xport 'SMQ_G.XPT';
libname smqrtug xport ‘SMQRTU_G.XPT';
libname sxqg xport 'SXQ_G.XPT';

libname demoh xport 'DEMO_H.XPT';
libname ohpvh xport 'ORHPV_H.XPT';
libname alqh xport 'ALQ_H.XPT';
libname duqh xport 'DUQ_H.XPT';
libname imqh xport 'IMQ_H.XPT';
libname rhqh xport 'RHQ_H.XPT';
libname smqh xport 'SMQ_H.XPT';
libname smqrtuh xport 'SMQRTU_H.XPT';
libname sxqh xport 'SXQ_H.XPT';

libname demoi xport 'DEMO_I.XPT';
libname orhpvi xport 'ORHPV_I.XPT';
libname sxqi xport 'SXQ_I.XPT';
libname smqi xport 'SMQ_I.XPT';
libname imqi xport 'IMQ_I.XPT';
libname alqi xport 'ALQ_I.XPT';
libname duqi xport 'DUQ_I.XPT';
libname rhqi xport 'RHQ_I.XPT';
libname smqrtui xport 'SMQRTU_I.XPT';

libname out 'jan2020';

DATA wavef;
  MERGE alqf.ALQ_F demof.DEMO_F duqf.DUQ_F imqf.IMQ_F rhqf.RHQ_F smqf.SMQ_F smqrtuf.SMQRTU_F sxqf.SXQ_F ohpvf.ORHPV_F (in=o);
  by seqn;
  if o;
  run;

DATA waveg;
  MERGE alqg.ALQ_G demog.DEMO_G duqg.DUQ_G imqg.IMQ_G rhqg.RHQ_G smqg.SMQ_G smqrtug.SMQRTU_G sxqg.SXQ_G ohpvg.ORHPV_G (in=o);
  by seqn;
  if o;
  run;

DATA waveh;
  MERGE alqh.ALQ_H demoh.DEMO_H duqh.DUQ_H imqh.IMQ_H rhqh.RHQ_H smqh.SMQ_H smqrtuh.SMQRTU_H sxqh.SXQ_H ohpvh.ORHPV_H (in=o);
  by seqn;
  if o;
  run;


data nhanes;
  merge wavef waveg waveh;
  by seqn;
  run;


data nhanes3w;  *three waves, imq040 only asked of females;
  set nhanes;
  by seqn;
  if orxgh=. then delete;
  if (18 <= ridageyr <= 69);
  run;
  

  *****Includes ages 18-69 years and excludes 177 subjects with missing HPV results*************;


data new; 
  merge demoi.DEMO_I orhpvi.ORHPV_I (in=o) sxqi.SXQ_I smqi.SMQ_I imqi.IMQ_I alqi.ALQ_I duqi.DUQ_I rhqi.RHQ_I smqrtui.SMQRTU_I;
  by seqn;
  if o;
  if (18 <= ridageyr <= 69);

  *imq060=female received hpv - new variable in this wave;
  IMQ040=IMQ060;

  if orxgh=. then delete;

run;


**********below code combines new wave with old 3 wave data**********;

data nhanes4; 
  set nhanes3w new;
run;


*************************DELETING INDIVIDUALS WITH ORXGH > 1: BETA-GLOBIN NOT EVALUATED O NOT EVALUABLE*********************;

data nhanes4; 
set nhanes4;
if orxgh > 1 then delete;
run;

data nhanes4; 
set nhanes4;
*****Age group********************;

age=ridageyr;

if 14 <=ridageyr <=17 then agegrp=1;
else if 18 <= ridageyr <= 59 then agegrp=2;
else if ridageyr ge 60 then agegrp=3;

***Age in 5-year categories****************;

if ridageyr le 17 then agecat=1;
else if 18 <= ridageyr <= 24 then agecat=2;
else if 25 <= ridageyr <= 29 then agecat=3;
else if 30 <= ridageyr <= 34 then agecat=4;
else if 35 <= ridageyr <= 39 then agecat=5;
else if 40 <= ridageyr <= 44 then agecat=6;
else if 45 <= ridageyr <= 49 then agecat=7;
else if 50 <= ridageyr <= 54 then agecat=8;
else if 55 <= ridageyr <= 59 then agecat=9;
else if 60 <= ridageyr <= 64 then agecat=10;
else if 65 <= ridageyr <= 69 then agecat=11;
 

if age < 30 then agecat2=1;
else if 30 <= age <= 39 then agecat2=2;
else if 40 <= age <= 49 then agecat2=3;
else if 50 <= age <= 59 then agecat2=4;
else agecat2=5;

if 18 <=age <= 24 then agecat3=1;
else if 25 <= age <= 29 then agecat3=2;
else if 30 <= age <= 39 then agecat3=3;
else if 40 <= age <= 49 then agecat3=4;
else if 50 <= age <= 59 then agecat3=5;
else agecat3=6;

if age < 40 then ages=1;
else if 40 <= age <= 49 then ages=2;
else if 50 <= age <= 59 then ages=3;
else if 60 <= age <= 69 then ages=4;

**************Oral HPV variables*********************;
*****recoding HPV*****************;

if orxh16=2 then orxh16=0;
if orxh18=2 then orxh18=0;
if orxh26=2 then orxh26=0;
if orxh31=2 then orxh31=0;
if orxh33=2 then orxh33=0;
if orxh35=2 then orxh35=0;
if orxh39=2 then orxh39=0;
if orxh45=2 then orxh45=0;
if orxh51=2 then orxh51=0;
if orxh52=2 then orxh52=0;
if orxh53=2 then orxh53=0;
if orxh56=2 then orxh56=0;
if orxh58=2 then orxh58=0;
if orxh59=2 then orxh59=0;
if orxh66=2 then orxh66=0;
if orxh68=2 then orxh68=0;
if orxh73=2 then orxh73=0;
if orxh82=2 then orxh82=0;
if orxh06=2 then orxh06=0;
if orxh11=2 then orxh11=0;
if orxh40=2 then orxh40=0;
if orxh42=2 then orxh42=0;
if orxh54=2 then orxh54=0;
if orxh55=2 then orxh55=0;
if orxh61=2 then orxh61=0;
if orxh62=2 then orxh62=0;
if orxh64=2 then orxh64=0;
if orxh67=2 then orxh67=0;
if orxh69=2 then orxh69=0;
if orxh70=2 then orxh70=0;
if orxh71=2 then orxh71=0;
if orxh72=2 then orxh72=0;
if orxh81=2 then orxh81=0;
if orxh83=2 then orxh83=0;
if orxh84=2 then orxh84=0;
if orxhpc=2 then orxhpc=0;
if orxhpi=2 then orxhpi=0;

if orxh16=1 or orxh18=1 or orxh26=1 or orxh31=1 or orxh33=1 or orxh35=1 or
     orxh39=1 or orxh45=1 or orxh51=1 or orxh52=1 or orxh53=1 or orxh56=1 or
     orxh58=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 or
        orxh06=1 or orxh11=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then anyhpv=1;
   else anyhpv=0;

*hpvinf 0=no 1=low risk 2=high risk;
  if orxh16=1 or orxh18=1 or orxh26=1 or orxh31=1 or orxh33=1 or orxh35=1 or
     orxh39=1 or orxh45=1 or orxh51=1 or orxh52=1 or orxh53=1 or orxh56=1 or
     orxh58=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 then hpvinf=2;
   else if orxh06=1 or orxh11=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then hpvinf=1;
   else hpvinf=0;

 if orxh16=1 or orxh18=1 or orxh26=1 or orxh31=1 or orxh33=1 or orxh35=1 or
     orxh39=1 or orxh45=1 or orxh51=1 or orxh52=1 or orxh53=1 or orxh56=1 or
     orxh58=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 then highrisk=1;
         else highrisk=0;

if orxh06=1 or orxh11=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then lowrisk=1; else lowrisk=0;

        hpvsum= sum (of orxh16  orxh18  orxh26  orxh31  orxh33  orxh35
     orxh39  orxh45  orxh51  orxh52  orxh53  orxh56
     orxh58  orxh59  orxh66  orxh68  orxh73  orxh82
        orxh06  orxh11  orxh40  orxh42  orxh54  orxh55
           orxh61  orxh62  orxh64  orxh67  orxh69  orxh70
    orxh71  orxh72  orxh81  orxh83  orxh84  orxhpc
    orxhpi);

        if hpvsum=0 then multiple=0;
        else if hpvsum=1 then multiple=1;
        else if hpvsum > 1 then multiple=2;

if orxh16=1 or orxh18=1 or orxh31=1 or orxh33=1 or orxh35=1 or
     orxh39=1 or orxh45=1 or orxh51=1 or orxh52=1 or orxh56=1 or
     orxh58=1 or orxh59=1 then newhighrisk=1;
else newhighrisk=0;



***********************HPV outcomes**********************;

if orxh16=1 or orxh18=1 or orxh06=1 or orxh11=1 then vactypes=1;
else vactypes=0;

if orxh31=1 or orxh33=1 or orxh45=1 then crosstypes=1;
else crosstypes=0;

if orxh06=1 or orxh11=1 or orxh16=1 or orxh18=1 or orxh31=1 or orxh33=1 or orxh45=1 or orxh52=1 or orxh58=1 then hpv9=1;
else hpv9=0;

if vactypes=1 or crosstypes=1 then vactypes2=1;
else vactypes2=0;

if vactypes=1 or crosstypes=1 or hpv9=1 then vactypes3=1;
else vactypes3=0;


*********Nonvactypes: Types except 16, 18, 6, and 11*******************;

if orxh26=1 or orxh31=1 or orxh33=1 or orxh35=1 or
     orxh39=1 or orxh45=1 or orxh51=1 or orxh52=1 or orxh53=1 or orxh56=1 or
     orxh58=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then nonvactypes=1;
   else nonvactypes=0;

********Nonvactypes2: Types except HPV4 + cross types: 16, 18, 6, 11, 31, 33, 45****************;

   if orxh26=1 or orxh35=1 or
     orxh39=1 or orxh51=1 or orxh52=1 or orxh53=1 or orxh56=1 or
     orxh58=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then nonvactypes2=1;
   else nonvactypes2=0;

**********Nonvactypes 3: Types except HPV4 + 31, 33, 45 + 52 and 58*************;

   if orxh26=1 or orxh35=1 or
     orxh39=1 or orxh51=1 or orxh53=1 or orxh56=1 or orxh59=1 or orxh66=1 or orxh68=1 or orxh73=1 or orxh82=1 or orxh40=1 or orxh42=1 or orxh54=1 or orxh55=1 or
           orxh61=1 or orxh62=1 or orxh64=1 or orxh67=1 or orxh69=1 or orxh70=1 or
    orxh71=1 or orxh72=1 or orxh81=1 or orxh83=1 or orxh84=1 or orxhpc=1 or
    orxhpi=1 then nonvactypes3=1;
   else nonvactypes3=0;


* CREATE SPLINE FOR AGE WITH 5 KNOTS;


T1_5=18.6; T2_5=27.7; T3_5=41.6; T4_5=53.9; T5_5=64.7
; /*Location of knots 5 25 50 75 95*/

IF AGE <= T1_5 THEN SPL5AGE1=0;
IF T1_5<AGE<=T4_5 THEN DO;
SPL5AGE1=(AGE-T1_5)**3;
END;
IF T4_5<AGE<=T5_5 THEN DO;
SPL5AGE1=(AGE-T1_5)**3 + -((T5_5-T1_5)/(T5_5-T4_5))*(AGE-T4_5)**3;
END;
IF T5_5<AGE THEN DO;
SPL5AGE1=(AGE-T1_5)**3 + -((T5_5-T1_5)/(T5_5-T4_5))*(AGE-T4_5)**3
+ ((T4_5-T1_5)/(T5_5-T4_5))*(AGE-T5_5)**3;
END;
IF AGE= . THEN SPL5AGE1= .;

IF AGE <= T2_5 THEN SPL5AGE2=0;
IF T2_5<AGE<=T4_5 THEN DO;
SPL5AGE2=(AGE-T2_5)**3;
END;
IF T4_5<AGE<=T5_5 THEN DO;
SPL5AGE2=(AGE-T2_5)**3 + -((T5_5-T2_5)/(T5_5-T4_5))*(AGE-T4_5)**3;
END;
IF T5_5<AGE THEN DO;
SPL5AGE2=(AGE-T2_5)**3 + -((T5_5-T2_5)/(T5_5-T4_5))*(AGE-T4_5)**3
+ ((T4_5-T2_5)/(T5_5-T4_5))*(AGE-T5_5)**3;
END;
IF AGE= . THEN SPL5AGE2= .;

IF AGE <= T3_5 THEN SPL5AGE3=0;
IF T3_5<AGE<=T4_5 THEN DO;
SPL5AGE3=(AGE-T3_5)**3;
END;
IF T4_5<AGE<=T5_5 THEN DO;
SPL5AGE3=(AGE-T3_5)**3 + -((T5_5-T3_5)/(T5_5-T4_5))*(AGE-T4_5)**3;
END;
IF T5_5<AGE THEN DO;
SPL5AGE3=(AGE-T3_5)**3 + -((T5_5-T3_5)/(T5_5-T4_5))*(AGE-T4_5)**3
+ ((T4_5-T3_5)/(T5_5-T4_5))*(AGE-T5_5)**3;
END;
IF AGE= . THEN SPL5AGE3= .;


**************************3-knot spline for age**************************;


T1_3=19; T2_3=38
; T3_3=57
;  /*Location of knots*/

IF AGE <= T1_3 THEN SPL3AGE=0;
IF T1_3<AGE<=T2_3 THEN DO;
SPL3AGE=(AGE-T1_3)**3;
END;
IF T2_3<AGE<=T3_3 THEN DO;
SPL3AGE=(AGE-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(AGE-T2_3)**3;
END;
IF T3_3<AGE THEN DO;
SPL3AGE=(AGE-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(AGE-T2_3)**3
+((T2_3-T1_3)/(T3_3-T2_3))*(AGE-T3_3)**3;
END;
IF AGE= . THEN SPL3AGE= .;


********************Demographics and Sexual behavior variables*************************;

gender= riagendr;

race= ridreth1;

**racecat= ridreth3;

**********************fixing race********************************;

if ridreth1=1 or ridreth1=2 then racecat=1; **All hispanic****;
else if ridreth1=3 then racecat=2; **White**;
else if ridreth1=4 then racecat=3; **Black**;
else racecat=4;  **Other, including Asians**;



***Marital status**********;

if dmdmartl=. or dmdmartl=77 or dmdmartl=99 then marital=.;
else if dmdmartl=5 then marital=1;
else if dmdmartl=1 or dmdmartl=6 then marital=2;
else marital=3;

if age < 20 then marital2=1;
else if age ge 20 and marital=. then marital2=.;
else marital2=marital;

***Education********;

education= sum(of dmdeduc3 dmdeduc2);

if age le 19 and (education=77 or education=99 or education=.) then educat=.;
else if age > 19 and (education=7 or education=9 or education=.) then educat=.;
else if age le 19 and (education le 12 or education=55 or education=66) then educat=1;
else if age > 19 and (education=1 or education=2) then educat=1;
else if age le 19 and (education =13 or education=14) then educat=2;
else if age > 19 and education=3 then educat=2;
else if age le 19 and education=15 then educat=3;
else if age > 19 and (education=4 or education=5) then educat=3;

********** Poverty-income ratio*****************;

if indfmpir=. then poverty=.;
else if indfmpir < 1 then poverty=1;
else if 1 <= indfmpir < 2 then poverty=2;
else if 2 <= indfmpir < 3 then poverty=3;
else if indfmpir ge 3 then poverty=4;

*************Smoking***********************;

*age 18/19 in 9/10;
if age < 20 and sddsrvyr=6 then do;

 if (smq620=7 or smq620=9 or smq620=.) then smoking=.;
else if smq620=2 then smoking=1;
else if smq620=1 and (smd641=. or smd641 =0) then smoking=2;
else if smq620=1 and (smd641 <>. and smd641 >0) then smoking=3;
end;

*age 18/19 in 11/12;
else if age < 20 and sddsrvyr=7 then do;

 if (smq621=77 or smq621=99 or smq621=.) then smoking=.;
 else if (smq621=1 or smq621=2) then smoking=1;
 else if smq621 ge 3 and (smd641=. or smd641 =0) then smoking=2;
else if smq621 ge 3 and (smd641 ne . and smd641 >0) then smoking=3;
end;

*all rest;
else do;

 if smq020=. then smoking=.;
else if smq020=2 then smoking=1;
else if smq020=1 and smq040=3 then smoking=2;
else if smq020=1 and (smq040=1 or smq040=2) then smoking=3;
end;

*****Smoking intensity*********************************;
if smoking=. then smokeintensity=.;
else if smoking=1 or smoking=2 then smokeintensity=0;
else if smoking=3 and smd650=. then smokeintensity=.;
else if smoking=3 and smd650 le 10 then smokeintensity=1;
else if smoking=3 and 11 <= smd650 <= 20 then smokeintensity=2;
else if smoking=3 and smd650 > 20 then smokeintensity=3;

*********Marijuana********************;

if duq200=. or duq200=7 or duq200=9 then marijuana=.;
else if duq200=1 then marijuana=1;
else if duq200=2 then marijuana=0;


********************Sexual behavior******************************;

**Eversex**;
if sxd021=. or sxd021 >2 then eversex=.;
else if sxd021=1 then eversex=1;
else if sxd021=2 then eversex=0;

if sxq800 > 2 then sxq800=.;
if sxq803 > 2 then sxq803=.;
if sxq806 > 2 then sxq806=.;
if sxq809 > 2 then sxq809=.;

if sxq700 > 2 then sxq700=.;
if sxq703 > 2 then sxq703=.;
if sxq706 > 2 then sxq706=.;
if sxq709 > 2 then sxq709=.;

if riagendr=1 then eversame=sxq809;
else if riagendr=2 then eversame=sxq709;

if riagendr=1 then everoralopp=sxq803; else if riagendr=2 then everoralopp=sxq703;

if eversame=. then everoralsame=.;
else if eversame=2 then everoralsame=2;
else if riagendr=1 then everoralsame=sxq853;
else if riagendr=2 then everoralsame=sxq741;

if sxd021=2 and everoralsame=. then everoralsame=2;
if sxd021=2 and everoralopp=. then everoralopp=2;

if everoralsame=1 or everoralopp=1 then everoral=1;
 else if everoralsame=2 and everoralopp=2 then everoral=2;
 else everoral=.;



*************Coded oral sex as above to identify those who never reported any oral sex
sxq853 and sxq741 asked for 18-59 year-olds******;

if riagendr=1 then evervag= sxq800;
else if riagendr=2 then evervag=sxq700;

if eversex=0 then evervag=2;
else evervag=evervag;

if riagendr=1 then everanal=sxq806;
else if riagendr=2 then everanal=sxq706;

if eversex=0 then everanal=2;
else everanal=everanal;

***********Oral sex partners******************;

if sxq636 > 1000 then sxq636=.;
if sxq624 > 1000 then sxq624=.;
if sxq639 > 1000 then sxq639=.;
if sxq627 > 1000 then sxq627=.;
if sxd633 > 76 then sxd633=.;
if sxd621 > 76 then sxd621=.;
if (sxd642=77777 or sxd642=99999) then sxd642=.;
if (sxd630=77777 or sxd630=99999) then sxd630=.;

if everoral=. then lifetimeoral=.;
else if everoral=2 then lifetimeoral=0;
else if everoral=1 then lifetimeoral= sum(of sxq624 sxq636);

if lifetimeoral=. then lifetimeoralcat=.;
else if lifetimeoral=0 then lifetimeoralcat=0;
else if lifetimeoral=1 then lifetimeoralcat=1;
else if 2 <= lifetimeoral <= 5 then lifetimeoralcat=2;
else if 6 <= lifetimeoral <= 10 then lifetimeoralcat=3;
else if 11 <= lifetimeoral <= 20 then lifetimeoralcat=4;
else if lifetimeoral > 20 then lifetimeoralcat=5;

if everoral=. then recentoral=.;
else if everoral=2 then recentoral=0;
else if everoral=1 then recentoral= sum(of sxq639 sxq627);

if recentoral=. then recentoralcat=.;
else if recentoral=0 then recentoralcat=0;
else if recentoral=1 then recentoralcat=1;
else if recentoral > 1 then recentoralcat=2;

************Oral debut and time since*******************;
if everoral=. then oraldebut=.;
else if everoral=2 then oraldebut=.;
else if everoral=1 then oraldebut= min(sxd633, sxd621);

if everoral=2 then oraldebutcat=1;
else if oraldebut=. then oraldebutcat=.;
else if oraldebut le 18 then oraldebutcat=2;
else if oraldebut > 18 then oraldebutcat=3;

x= age-oraldebut;
x2= x*365.25;

if everoral=. then timesinceoral=.;
else if everoral=2 then timesinceoral=.;
else if everoral=1 then timesinceoral= min(sxd642, sxd630);

*************if everoral=1 and timesinceoral=. then timesinceoral= x2;
**************else timesinceoral= timesinceoral;

timesinceoral= timesinceoral/30.42;

if timesinceoral=. then timesincecat=.;
else if timesinceoral le 1 then timesincecat=1;
else if 1 < timesinceoral <= 3 then timesincecat=2;
else if 3 < timesinceoral <= 6 then timesincecat=3;
else if 6 < timesinceoral <= 12 then timesincecat=4;
else if timesinceoral > 12 then timesincecat=5;

if recentoralcat=. then oralprotection=.;
else if recentoralcat=0 then oralprotection=0;
else if recentoralcat ge 1 and sxq645 > 5 then oralprotection=.;
else if recentoralcat ge 1 and (sxq645=1 or sxq645=2) then oralprotection=1;
else if recentoralcat ge 1 and (sxq645=3 or sxq645=4) then oralprotection=2;

********* ANY SEX partners****************;

if sxd171 > 1000 then sxd171=.;
if sxd101 > 1000 then sxd101=.;
if sxd510 > 1000 then sxd510=.;
if sxd450 > 1000 then sxd450=.;

if eversex=. then lifetimeany=.;
else if eversex=0 then lifetimeany=0;
else lifetimeany= sum(of sxd171 sxd101);

if lifetimeany=. then lifetimeanycat=.;
else if lifetimeany=0 then lifetimeanycat=0;
else if lifetimeany=1 then lifetimeanycat=1;
else if 2 <= lifetimeany <= 5 then lifetimeanycat=2;
else if 6 <= lifetimeany <= 10 then lifetimeanycat=3;
else if 11 <= lifetimeany <= 20 then lifetimeanycat=4;
else if lifetimeany > 20 then lifetimeanycat=5;

if eversex=. then recentany=.;
else if eversex=0 then recentany=0;
else recentany= sum(of sxd510 sxd450);

if recentany=. then recentanycat=.;
else if recentany=0 then recentanycat=0;
else if recentany=1 then recentanycat=1;
else if recentany > 1 then recentanycat=2;


***************Vaginal sex partners********************;
if sxq824 > 1000 then sxq824=.;
if sxq724 > 1000 then sxq724=.;
if sxq827 > 1000 then sxq827=.;
if sxq727 > 1000 then sxq727=.;

if evervag=. then lifetimevag=.;
else if evervag=2 then lifetimevag=0;
else lifetimevag= sum(of sxq824 sxq724);

if lifetimevag=. then lifetimevagcat=.;
else if lifetimevag=0 then lifetimevagcat=0;
else if lifetimevag=1 then lifetimevagcat=1;
else if 2 <= lifetimevag <= 5 then lifetimevagcat=2;
else if 6 <= lifetimevag <= 10 then lifetimevagcat=3;
else if 11 <= lifetimevag <= 20 then lifetimevagcat=4;
else if lifetimevag > 20 then lifetimevagcat=5;

if evervag=. then recentvag=.;
else if evervag=2 then recentvag=0;
else recentvag= sum(of sxq827 sxq727);

if recentvag=. then recentvagcat=.;
else if recentvag=0 then recentvagcat=0;
else if recentvag=1 then recentvagcat=1;
else if recentvag > 1 then recentvagcat=2;


*****************orientation******************************;

if eversex=. then orientation=.;
else if eversex=0 then orientation=0;
else if eversex=1 and eversame=1 then orientation=1;
else if eversex=1 and (eversame=. or eversame=2)then orientation=2;


************normalizing lifetime oral partners***************;

time= timesinceoral;

ageatlast= age-time;

activity= abs(ageatlast-oraldebut);


if activity=. then activity=.;
else if activity=0 then activity=0.5;
else activity=activity;

scaledlifetimeoral= lifetimeoral/activity;

if oraldebutcat=1 then timesince=1;
else if time=. then timesince=.;
else if time le 12 then timesince=2;
else if 12 < time <= 24 then timesince=3;
else if time > 24 then timesince=4;

************Age at any debut****************;

if sxd031=77 then sxd031=.;
if sxd031=99 then sxd031=.;

if eversex=. then debut=.;
else if eversex=0 then debut=.;
else if eversex=1 then debut= sxd031;


*****************New partner in last year****************;

if sxq648=7 or sxq648=9 then sxq648=.;

if sxq610=77 or sxq610=99 then sxq610=.;

if sxq648=. and recentanycat=0 and recentoralcat=0 and recentvagcat=0 then recentnew=0;
else if sxq648=1 then recentnew=1;
else if sxq648=2 then recentnew=0;
else recentnew=.;

****************Ever herpes/warts******************;

if sxq260 > 2 then sxq260=.;
if sxq265 > 2 then sxq265=.;

if eversex=. and sxq260=. and sxq265=. then stds=.;
else if eversex=0 then stds=0;
else if eversex=1 and (sxq260=1 or sxq265=1) then stds=1;
else stds=0;

***************Alcohol consumption************************;
if alq120q=777 or alq120q=999 then alq120q=.;
else alq120q=alq120q; 

if alq120u=. or alq120u=7 or alq120u=9 then alcoholweekdays=.;
else if alq120u=1 then alcoholweekdays= alq120q;
else if alq120u=2 then alcoholweekdays= alq120q/4;
else if alq120u=3 then alcoholweekdays= alq120q/52;

if alq130=999 or alq130=777 then alq130=.;
else alq130=alq130;

if alq120q=0 then alcoholweekdrinks=0;
else if alq101=2 then alcoholweekdrinks=0;
else alcoholweekdrinks= alcoholweekdays*alq130;

if alq101=. and alcoholweekdrinks=. then alcoholcat=.;
else if alcoholweekdrinks=0 then alcoholcat=1;
else if alcoholweekdrinks < 1 then alcoholcat=2;
else if 1 <= alcoholweekdrinks <= 7 then alcoholcat=3;
else if 8 <= alcoholweekdrinks <= 14 then alcoholcat=4;
else if alcoholweekdrinks > 14 then alcoholcat=5;

*************marijuana use in past 30 days**************************;

if duq220u=. then marijuanalast=.;
else if duq220u=1 then marijuanalast=duq220q;
else if duq220u=2 then marijuanalast=duq220q*4;
else if duq220u=3 then marijuanalast=duq220q*30.25;
else if duq220u=4 then marijuanalast=duq220q*365.25;

if duq230=777 or duq230=999 then duq230=.;

if duq200=. and duq220q=. and duq230=. then marijuanafreq=.;
else if  0 < duq230 <= 15 then marijuanafreq=2;
else if 16 <= duq230 <= 30 then marijuanafreq=3;
else marijuanafreq=1;

if duq200=. and duq220q=. and duq230=. then marijuana2=.;
else if duq200=2 then marijuana2=1;
else if duq200=1 and (marijuanalast > 30.25 or marijuanalast=.) then marijuana2=2;
else if duq200=1 and 1 <= duq230 <= 30 then marijuana2=3;

*******************Reproductive health**********************;
if rhq520 > 2 then rhq520=.;
if rhq558 > 2 then rhq558=.;
if rhd442 > 2 then rhd442=.;
if rhq566 > 2 then rhq566=.;
if rhq574 > 2 then rhq574=.;
if rhq584 > 2 then rhq584=.;
if rhq600 > 2 then rhq600=.;

if gender=1 then hormones=.;
else if rhq420=. and rhq510=. and rhq540=. then hormones =.;
else if rhq520=1 or rhq558=1 or rhd442=1 or rhq566=1 or rhq574=1 or rhq584=1 or rhq600=1 then hormones=1;
else hormones=2;

*************Frequency of sex********************;

if evervag=. and everanal=. and sxq610=. then recentfreq=.;
else if evervag=2 and everanal=2 then recentfreq=0;
else recentfreq=sxq610;

****************Corrections for potential reporting erros in number of partners*****************************;

lifetimeany2= max (lifetimeany, lifetimevag, lifetimeoral);
recentany2= max (recentany, recentvag, recentoral);

duration= abs(age-debut)*12;

if eversex=. or eversex=0 then timesinceoral2=.;
else if recentnew=1 then timesinceoral2= min(timesinceoral, 6);
else if recentany2 > 1 then timseinceoral2= min(timesinceoral, 6);
else if lifetimeany2=1 then timesinceoral2= duration;
else if timesinceoral=0 then timesinceoral2=.;
else timesinceoral2= timesinceoral;

******************HPV vaccination*********************************;
if imq040=7 or imq040=9 then imq040=.;

if imq040=1 or imq070=1 then vaccine=1;
else vaccine=0;

if sddsrvyr=6 and gender=1 then vaccine=0;
else vaccine=vaccine;

if imq090=. or imq090=777 or imq090=999 or sddsrvyr=6 then vacage=.;
else if vaccine=0 then vacage=.;
else if vaccine=1 then vacage=imq090;

if vaccine=1 then timesincevac= age-vacage;
else if vaccine=0 then timesincevac=0;


if vaccine=0 and (18<= AGE <= 35) then vacagegroup=1;
else if vaccine=1 and (8 <= vacage <= 26) and (18<= age <= 35) then vacagegroup=1;
ELSE IF VACCINE=1 AND VACAGE=. AND (18<= age <= 26) THEN VACAGEGROUP=1;
else vacagegroup=.;


if sddsrvyr=6 then year=2010;
else if sddsrvyr=7 then year=2012;
else if sddsrvyr=8 then year=2014;
else if sddsrvyr=9 then year=2016;

birthyear= year- age;


if lifetimeanycat=. then lifetimeanycat2=.;
else if lifetimeanycat=0 or lifetimeanycat=1 then lifetimeanycat2=1;
else if lifetimeanycat=2 then lifetimeanycat2=2;
else if lifetimeanycat=3 then lifetimeanycat2=3;
else lifetimeanycat2=4;

if lifetimeoralcat=. then lifetimeoralcat2=.;
else if lifetimeoralcat=0 or lifetimeoralcat=1 then lifetimeoralcat2=1;
else if lifetimeoralcat=2 then lifetimeoralcat2=2;
else if lifetimeoralcat=3 then lifetimeoralcat2=3;
else lifetimeoralcat2=4;

if alcoholcat=. then alcoholcat2=.;
else if alcoholcat=1 or alcoholcat=2 then alcoholcat2=1;
else if alcoholcat=3  then alcoholcat2=2;
else if alcoholcat=4 then alcoholcat2=3;
else if alcoholcat=5 then alcoholcat2=4;

if recentoral=. then recentoralcat2=.;
else if recentoral=0 then recentoralcat2=0;
else if recentoral=1 then recentoralcat2=1;
else if recentoral > 1 then recentoralcat2=2;

if age le 26 then age26=0;
else age26=1;


if orxh16=1 then vactype16=1;
else vactype16=0;

if orxh18=1 or orxh11=1 or orxh06=1 then othervactypes=1;
else othervactypes=0;

run;

proc sort data=nhanes4;
  by seqn;
  run;

*add post-strat weight - created in another program;
data nhanes5;
  merge nhanes4 (in=n) out.fourwt;
  by seqn;
  if n;
  run;


data dec19;	
  set nhanes5;
run;



proc freq data=dec19;
  title2 'herd cohort';
  tables sddsrvyr*smoking/missing;
  tables sddsrvyr*poverty/missing;
  tables sddsrvyr*alcoholcat/missing;
  run;
  
proc means n nmiss min max mean data=dec19;
  class sddsrvyr;
  var INDFMPIR;
  run;

data morepeep (keep=seqn sddsrvyr riagendr ridageyr ridreth1 ridreth3 wtint2yr smoking smokeintensity);
  merge demof.DEMO_F demog.DEMO_G demoh.DEMO_H demoi.DEMO_I smqf.SMQ_F smqg.SMQ_G smqh.SMQ_H smqi.SMQ_I;
  by seqn;
  if (9 <= ridageyr <= 79);

  *************Smoking***********************;

*age 18/19 in 9/10;
if ridageyr < 20 and sddsrvyr=6 then do;

 if (smq620=7 or smq620=9 or smq620=.) then smoking=.;
else if smq620=2 then smoking=1;
else if smq620=1 and (smd641=. or smd641 =0) then smoking=2;
else if smq620=1 and (smd641 <>. and smd641 >0) then smoking=3;
end;

*age 18/19 in 11/12;
else if ridageyr < 20 and sddsrvyr=7 then do;

 if (smq621=77 or smq621=99 or smq621=.) then smoking=.;
 else if (smq621=1 or smq621=2) then smoking=1;
 else if smq621 ge 3 and (smd641=. or smd641 =0) then smoking=2;
else if smq621 ge 3 and (smd641 ne . and smd641 >0) then smoking=3;
end;

*all rest;
else do;

 if smq020=. then smoking=.;
else if smq020=2 then smoking=1;
else if smq020=1 and smq040=3 then smoking=2;
else if smq020=1 and (smq040=1 or smq040=2) then smoking=3;
end;

*****Smoking intensity*********************************;
if smoking=. then smokeintensity=.;
else if smoking=1 or smoking=2 then smokeintensity=0;
else if smoking=3 and smd650=. then smokeintensity=.;
else if smoking=3 and smd650 le 10 then smokeintensity=1;
else if smoking=3 and 11 <= smd650 <= 20 then smokeintensity=2;
else if smoking=3 and smd650 > 20 then smokeintensity=3;

  run;

proc sort data=dec19;
  by seqn;

data dec19big;
  merge dec19 (in=a) morepeep (in=b);
  by seqn;
  if a then newsubjflag=0;
    else newsubjflag=1;
  label newsubjflag='flag if subject is NOT in HPV cohort';
  run;

proc freq data=dec19big;
  title2 'jan 2020 data - more people';
  tables newsubjflag;
  run;

proc means data=dec19big;
  class newsubjflag;
  var wtint2yr ridageyr genowt behavwt behavweight4yr behavweight8yr;
  run;


data out.jan2020c4;
  set dec19big;
  by seqn;

proc contents varnum;
run;


