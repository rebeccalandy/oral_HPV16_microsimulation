title 'make.wt8yr.nogeno.sas';

libname demof xport 'DEMO_F.xpt';
libname demog xport 'DEMO_G.XPT';
libname demoh xport 'DEMO_H.XPT';
libname demoi xport 'DEMO_I.XPT';

libname ohpvf xport 'ORHPV_F.XPT';
libname ohpvg xport 'ORHPV_G.XPT';
libname ohpvh xport 'ORHPV_H.XPT';
libname ohpvi xport 'ORHPV_I.XPT';

libname out 'jan2020';

data wavef;
  merge demof.DEMO_F ohpvf.ORHPV_F (in=o);
  by seqn;
  if o;
  if (18 <= ridageyr <= 69);
  *use ridreth1;
  *1. code race/ethnicity into 3 categories;
	if ridreth1 in (1,2) then racecat=1; *all hisp;
	else if ridreth1=4 then racecat=2;  *nh black;
	else racecat=3;			   *nh white + other/multi;
  if aialang < 1 then hasacasi=0;
   else hasacasi=1;
*2. code age into 10 year intervals;
	
		 if 18<=ridageyr<=19 then agecat=1;
	else if 20<=ridageyr<=29 then agecat=2;
	else if 30<=ridageyr<=39 then agecat=3;
	else if 40<=ridageyr<=49 then agecat=4;
	else if 50<=ridageyr<=59 then agecat=5;
	else if 60<=ridageyr<=69 then agecat=6;
*check cell sizes- may need to combine if cell sizes are too small;
  if orxgh=1 then hasgeno=1;
    else hasgeno=0;
  if hasgeno=1 and hasacasi=1 then foracwt=1;
    else foracwt=0;
  label hasgeno='flag - has geno data'
        hasacasi='flag - has acasi data'
        foracwt='flag - has geno and acasi data';

run;

data waveg;
  merge demog.DEMO_G ohpvg.ORHPV_G (in=o);
  by seqn;
  if o;
  if (18 <= ridageyr <= 69);
  *use ridreth3;
*1. code race/ethnicity into 4 categories;
	if ridreth3 in (1,2) then racecat=1;  *all hisp;
	else if ridreth3=4 then racecat=2;    *nh black;
	else if ridreth3=6 then racecat=3;    *nh asian;
	else racecat=4;			      *nh white+other/multi;
  if aialanga < 1 then hasacasi=0;
   else hasacasi=1;
*2. code age into 10 year intervals;
	
		 if 18<=ridageyr<=19 then agecat=1;
	else if 20<=ridageyr<=29 then agecat=2;
	else if 30<=ridageyr<=39 then agecat=3;
	else if 40<=ridageyr<=49 then agecat=4;
	else if 50<=ridageyr<=59 then agecat=5;
	else if 60<=ridageyr<=69 then agecat=6;
*check cell sizes- may need to combine if cell sizes are too small;
  if orxgh=1 then hasgeno=1;
    else hasgeno=0;
   if hasgeno=1 and hasacasi=1 then foracwt=1;
    else foracwt=0;
  label hasgeno='flag - has geno data'
        hasacasi='flag - has acasi data'
        foracwt='flag - has geno and acasi data';
  run;


DATA waveh;
  MERGE demoh.DEMO_H ohpvh.ORHPV_H (in=o);
  by seqn;
  if o;
  if (18 <= ridageyr <= 69);
  *use ridreth3;
*1. code race/ethnicity into 4 categories;
	if ridreth3 in (1,2) then racecat=1;  *all hisp;
	else if ridreth3=4 then racecat=2;    *nh black;
	else if ridreth3=6 then racecat=3;    *nh asian;
	else racecat=4;			      *nh white+other/multi;
  if aialanga < 1 then hasacasi=0;
   else hasacasi=1;
*2. code age into 10 year intervals;
	
		 if 18<=ridageyr<=19 then agecat=1;
	else if 20<=ridageyr<=29 then agecat=2;
	else if 30<=ridageyr<=39 then agecat=3;
	else if 40<=ridageyr<=49 then agecat=4;
	else if 50<=ridageyr<=59 then agecat=5;
	else if 60<=ridageyr<=69 then agecat=6;
*check cell sizes- may need to combine if cell sizes are too small;
  if orxgh=1 then hasgeno=1;
    else hasgeno=0;
  if hasgeno=1 and hasacasi=1 then foracwt=1;
    else foracwt=0;
  label hasgeno='flag - has geno data'
        hasacasi='flag - has acasi data'
        foracwt='flag - has geno and acasi data';
  run;

DATA wavei;
  MERGE demoi.DEMO_I ohpvi.ORHPV_I (in=o);
  by seqn;
  if o;
  if (18 <= ridageyr <= 69);
  *use ridreth3;
*1. code race/ethnicity into 4 categories;
	if ridreth3 in (1,2) then racecat=1;  *all hisp;
	else if ridreth3=4 then racecat=2;    *nh black;
	else if ridreth3=6 then racecat=3;    *nh asian;
	else racecat=4;			      *nh white+other/multi;
  if aialanga < 1 then hasacasi=0;
   else hasacasi=1;
*2. code age into 10 year intervals;
	
		 if 18<=ridageyr<=19 then agecat=1;
	else if 20<=ridageyr<=29 then agecat=2;
	else if 30<=ridageyr<=39 then agecat=3;
	else if 40<=ridageyr<=49 then agecat=4;
	else if 50<=ridageyr<=59 then agecat=5;
	else if 60<=ridageyr<=69 then agecat=6;
*check cell sizes- may need to combine if cell sizes are too small;
  if orxgh=1 then hasgeno=1;
    else hasgeno=0;
  if hasgeno=1 and hasacasi=1 then foracwt=1;
    else foracwt=0;  
  label hasgeno='flag - has geno data'
        hasacasi='flag - has acasi data'
        foracwt='flag - has geno and acasi data';
  run;

*nh 9/10;

proc freq data=wavef;
  tables sddsrvyr hasgeno foracwt hasacasi*hasgeno;
  title2 'nh 09/10';
  run;

proc sort data=wavef;
	by sdmvstra sdmvpsu;

proc wtadjust data=wavef design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=6;
	class agecat racecat riagendr/ nofreq;
	model hasgeno=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matchg6 replace;
	run;

proc wtadjust data=wavef design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=6;
	class agecat racecat riagendr/ nofreq;
	model foracwt=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matcha6 replace;
	run;

*nh 11/12;

proc freq data=waveg;
  tables sddsrvyr hasgeno foracwt hasacasi*hasgeno;
  title2 'nh 11/12';
  run;

proc sort data=waveg;
	by sdmvstra sdmvpsu;


proc wtadjust data=waveg design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=7;
	class agecat racecat riagendr/ nofreq;
	model hasgeno=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matchg7 replace;
	run;

proc wtadjust data=waveg design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=7;
	class agecat racecat riagendr/ nofreq;
	model foracwt=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matcha7 replace;
	run;

*nh 13/14;

proc freq data=waveh;
  tables sddsrvyr hasgeno foracwt hasacasi*hasgeno;
  title2 'nh 13/14';
  run;

proc sort data=waveh;
	by sdmvstra sdmvpsu;


proc wtadjust data=waveh design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=8;
	class agecat racecat riagendr/ nofreq;
	model hasgeno=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matchg8 replace;
	run;

proc wtadjust data=waveh design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=8;
	class agecat racecat riagendr/ nofreq;
	model foracwt=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matcha8 replace;
	run;	

*nh 15/16;

proc freq data=wavei;
  tables sddsrvyr hasgeno foracwt hasacasi*hasgeno;
  title2 'nh 15/16';
  run;

proc sort data=wavei;
	by sdmvstra sdmvpsu;

proc wtadjust data=wavei design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=9;
	class agecat racecat riagendr/ nofreq;
	model hasgeno=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matchg9 replace;
	run;

proc wtadjust data=wavei design=wr adjust=nonresponse;
	nest sdmvstra sdmvpsu;
	weight wtmec2yr;
	subpopn sddsrvyr=9;
	class agecat racecat riagendr/ nofreq;
	model foracwt=agecat*racecat*riagendr;
	idvar seqn;
	output / predicted=all filename=matcha9 replace;
	run;	



data nhanes;
  set wavef waveg waveh wavei; 
  run;

proc freq data=nhanes;
  table hasacasi*hasgeno;
  title2 'all four waves together';
  run;


proc sort data=nhanes;
  by seqn;

data matchg (keep=seqn genowt);
  set matchg6 matchg7 matchg8 matchg9;
  genowt=wtfinal;
  label genowt='individual cycle new sample weight for genotype analysis';
  run;

proc sort data=matchg;
  by seqn;
  run;

data matcha (keep=seqn behavwt);
  set matcha6 matcha7 matcha8 matcha9;
  behavwt=wtfinal;
  label behavwt='individual cycle new sample weight for acasi/behavior analysis';
  run;

proc sort data=matcha;
  by seqn;
  run;

data nhanes;
  merge nhanes (in=c) matchg matcha;
  by seqn;
  if c;
  run;  

proc means n nmiss min max mean sum;
  title3 'new wts in analysis cohort';
  var wtmec2yr genowt behavwt;
  run;

proc means n nmiss min max mean sum;
  class hasgeno;
  title3 'new wts in analysis cohort - check stats';
  var wtmec2yr genowt ;
  run;
 
proc means n nmiss min max mean sum;
  class foracwt;
  title3 'new wts in analysis cohort - check stats';
  var wtmec2yr  behavwt;
  run;

data nhanes (keep=seqn genowt behavwt hasgeno hasacasi foracwt behavweight4yr behavweight8yr);
  set nhanes;
  by seqn;
  behavweight4yr=behavwt/2;
  behavweight8yr=behavwt/4;
  label hasgeno='flag - has geno data'
        hasacasi='flag - has acasi data'
        foracwt='flag - has geno and acasi data'
        behavweight4yr='post-stratification weight for acasi/behavior analysis when using 2 cycles'
        behavweight8yr='post-stratification weight for acasi/behavior analysis when using 4 cycles';
  run;

data out.fourwt;
  set nhanes;
  by seqn;
  run;

proc means n nmiss min max mean;
  title2 'look at post-stratification weights in all four cycles using public data';
  run;


