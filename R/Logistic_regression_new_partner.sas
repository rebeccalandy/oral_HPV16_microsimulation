data nhanes09_16; set 'C:\Users\landyrm\Desktop\microsim\Data\Final\jan2020c4';
run;

PROC IMPORT OUT= WORK.ALLCOMB 
            DATAFILE= "L:\DCEG_ALL\Rebecca Landy\microsim-risk_by_subcoh
ort_ageextension\Data\allcombinations.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


data allcomb; set allcomb;
currentsmoking=smoking;
RIAGENDR=1;
behavweight8yr=1;
lifetimeoral_quart=lifetimequart-1;
allcomb=1;

* defining splines for all ages;
T1_5=18.6; T2_5=27.7; T3_5=41.6; T4_5=53.9; T5_5=64.7;
/*Location of knots 5 25 50 75 95*/

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


run;

proc contents data=nhanes09_16;
run;



data nhanes09_16; set nhanes09_16;

***Age in 5-year categories****************;
age=ridageyr;
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
else if 70 <= ridageyr <= 74 then agecat=12;
else if 75 <= ridageyr <= 79 then agecat=13;

*****smoking**********;
*if SMQ020=2 then currentsmoking=0;
*else if 1<= SMQ040 <=2 then currentsmoking=1;
*else if SMQ040=3 then currentsmoking=0;

if smoking=3 then currentsmoking=1;
else if smoking>=1 and smoking<=2 then currentsmoking=0;
else currentsmoking=.;

* defining splines for all ages;
T1_5=18.6; T2_5=27.7; T3_5=41.6; T4_5=53.9; T5_5=64.7;
/*Location of knots 5 25 50 75 95*/

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

run;


* check if any outliers in number of recent oral partners of the opposite sex;
proc freq data=nhanes09_16;
weight behavweight8yr;
where gender=1;
table sxq639;
run;


data nhanes09_16; set nhanes09_16;
*********marital status;
* DMDMARTL - 1 or 6 is married or living with partner;

if DMDMARTL=1 or DMDMARTL=6 then stable=1;
else if 2<=DMDMARTL<=5 then stable=0;
else if DMDMARTL=77 or DMDMARTL=99 then stable=.;
else if 18<= age <=19 then stable=0;
else if DMDMARTL=. then stable=.;

* use recentoralcorrected to be recent partners of opposite sex;

if eversex=0 then recentoralcorrected=0;
else if lifetimeoral=0 then recentoralcorrected=0;
else if sxq627 NE . & sxq639 = . then recentoralcorrected=0;
else if sxq639=. then recentoralcorrected=.;
else recentoralcorrected=sxq639;

**********************for weights********************************;
one=1;

run;

proc freq data=combined;
where gender=1 and behavweight8yr>0 & age>=18 & age<=59;
table eversex*lifetimeoral/missing;
run;


proc means data=nhanes09_16  q1 median q3;
weight behavweight8yr;
where gender=1;
class  agecat;
var lifetimeany;
run;

data nhanes09_16; set nhanes09_16;
if gender=1 and agecat=2 and lifetimeoral<=0 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=2 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=2 and lifetimeoral<=3 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=2 and lifetimeoral>3 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=3 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=3 and lifetimeoral<=3 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=3 and lifetimeoral<=5 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=3 and lifetimeoral>5 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=4 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=4 and lifetimeoral<=3 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=4 and lifetimeoral<=8 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=4 and lifetimeoral>8 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=5 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=5 and lifetimeoral<=3 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=5 and lifetimeoral<=7 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=5 and lifetimeoral>7 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=6 and lifetimeoral<=2 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=6 and lifetimeoral<=4 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=6 and lifetimeoral<=10 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=6 and lifetimeoral>10 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=7 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=7 and lifetimeoral<=4 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=7 and lifetimeoral<=10 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=7 and lifetimeoral>10 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=8 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=8 and lifetimeoral<=4 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=8 and lifetimeoral<=10 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=8 and lifetimeoral>10 and lifetimeoral NE . then lifetimeoral_quart=3;

else if gender=1 and agecat=9 and lifetimeoral<=1 and lifetimeoral NE . then lifetimeoral_quart=0;
else if gender=1 and agecat=9 and lifetimeoral<=4 and lifetimeoral NE . then lifetimeoral_quart=1;
else if gender=1 and agecat=9 and lifetimeoral<=10 and lifetimeoral NE . then lifetimeoral_quart=2;
else if gender=1 and agecat=9 and lifetimeoral>10 and lifetimeoral NE . then lifetimeoral_quart=3;



* if lifetimeoral is missing, but lifetimeall is available, use that;

else if gender=1 and agecat=2 and lifetimeany<=1 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=2 and lifetimeany<=3 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=2 and lifetimeany<=8 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=2 and lifetimeany>8 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=3 and lifetimeany<=2 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=3 and lifetimeany<=5 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=3 and lifetimeany<=12 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=3 and lifetimeany>12 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=4 and lifetimeany<=2 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=4 and lifetimeany<=7 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=4 and lifetimeany<=18 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=4 and lifetimeany>18 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=5 and lifetimeany<=3 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=5 and lifetimeany<=8 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=5 and lifetimeany<=15 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=5 and lifetimeany>15 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=6 and lifetimeany<=4 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=6 and lifetimeany<=10 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=6 and lifetimeany<=20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=6 and lifetimeany>20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=7 and lifetimeany<=4 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=7 and lifetimeany<=10 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=7 and lifetimeany<=20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=7 and lifetimeany>20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=8 and lifetimeany<=3 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=8 and lifetimeany<=9 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=8 and lifetimeany<=20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=8 and lifetimeany>20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=9 and lifetimeany<=4 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=9 and lifetimeany<=9 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=9 and lifetimeany<=20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=9 and lifetimeany>20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=10 and lifetimeany<=3 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=10 and lifetimeany<=8 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=10 and lifetimeany<=20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=10 and lifetimeany>20 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

else if gender=1 and agecat=11 and lifetimeany<=3 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=0;
else if gender=1 and agecat=11 and lifetimeany<=6 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=1;
else if gender=1 and agecat=11 and lifetimeany<=15 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=2;
else if gender=1 and agecat=11 and lifetimeany>15 and lifetimeany NE . and lifetimeoral_quart=. then lifetimeoral_quart=3;

run;


proc sort data=nhanes09_16;
by agecat currentsmoking racecat lifetimeoral_quart;
run;


data nhanes09_16; set nhanes09_16;
if ridreth1=1 or ridreth1=2 then racecat=1; **All Hispanic-Americans****;
else if ridreth1=3 then racecat=2; **Non-Hispanic White**;
else if ridreth1=4 then racecat=3; **Non-Hispanic Black**;
else racecat=4;  **Other, including Asian-Americans**;

run;

proc freq data=nhanes09_16;
where gender=1 and recentoralcorrected>=2;
weight behavweight8yr;
table age*recentoralcorrected;
run;


data nhanes09_16; set nhanes09_16;
if age>=60 then recentnew=.;
else if eversex=0 then recentnew=0;
else if recentany=0 then recentnew=0;
else if SXQ648=. then recentnew=.;
else if SXQ648=2 then recentnew=0;
else if SXQ648=1 then recentnew=1;

if age>=60 then recentany=.;
else if age<=60 then recentany=recentany;

run;

proc freq data=nhanes09_16;
where gender=1;
table age*recentany/missing;
run; 


****** append dataset with all combinations, so get predictions for all combinations;

DATA combined; set nhanes09_16 allcomb; 

run;

proc freq data=combined;
table age;
run;
proc freq data=allcomb;
table age;
run;



data combined
proc contents data=combined;
run;

proc freq data=combined;
table recentoralcorrected ;
run;

proc freq data=combined;
table gender ;
run;

proc freq data=combined;
where RIAGENDR=1 & age>=55 & age<=65;
table age*lifetimeoral age*lifetimeany;
run;


proc freq data=nhanes09_16;
where RIAGENDR=1 and age>=18 & age<70;
table recentnew/missing;
run;

proc logistic data = combined descending;
weight behavweight8yr;
where RIAGENDR=1 and currentsmoking NE .;
class  racecat lifetimeoral_quart currentsmoking/param=ref ref=first;
model recentnew = currentsmoking racecat lifetimeoral_quart age spl5age1 spl5age2 spl5age3  / link=logit;
output out=predicted p=prob;
store WORK.regout;
run;

proc plm restore=work.regout;
   score data=combined;
run;

proc print data=data1 (obs=50) ;
where allcomb=1 and age>74 ;
var age predicted;
run;

data data1; set data1;
prediction = exp(predicted)/(1+exp(predicted));
run;

proc print data=data1 (obs=50) ;
where allcomb=1 and age>74 ;
var age prediction;
run;

libname out 'L:\DCEG_ALL\Rebecca Landy\microsim-risk_by_subcohort_ageextension\Data\newrecent';


data out.newrecentpredictions;
  set data1;
run;




proc means data=predicted;
where gender=1 ;
class age racecat  currentsmoking  lifetimeoral_quart;
var prob;
output out=predicted2 mean=m1 ;
run;

data predicted2; set predicted2;
where _type_=15;
run;


proc genmod data=nhanes09_16;
where  RIAGENDR=1 and age>=18 and age<=59; 
strata sdmvstra;
weight behavweight8yr;
class lifetimeoral_quart racecat  ;
model recentoralcorrected= age spl5age1 spl5age2 spl5age3 racecat lifetimeoral_quart currentsmoking recentnew
/ dist=negbin TYPE3;
output out=ltopreds predicted=p xbeta=xbeta STDXBETA=stderrsbeta;
*lsmeans racecat;
run;





