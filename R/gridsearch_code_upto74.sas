data try; 
input age m16	prev	Count	Pop;
datalines;

15	0.001963396	0.196339634	3	4900192
16	0.002324754	0.232475444	0	4957711
17	0.002752436	0.275243615	4	5014158
18	0.003258541	0.325854088	1	5028189
19	0.003857265	0.385726488	3	4974712
20	0.004561587	0.456158653	4	5074141
21	0.005379019	0.53790193	9	5160271
22	0.00631223	0.631223017	6	5219798
23	0.007356985	0.735698541	4	5259456
24	0.008499692	0.849969226	8	5254297
25	0.009715089	0.971508878	3	5214815
26	0.010964543	1.09645434	13	5122019
27	0.01219553	1.219552963	5	5056448
28	0.013343076	1.334307578	8	5024975
29	0.014350984	1.435098366	11	4981597
30	0.015195721	1.519572117	12	4983169
31	0.01586729	1.586728975	16	4895683
32	0.016366249	1.636624899	20	4841968
33	0.016702426	1.670242621	16	4774438
34	0.016893113	1.689311258	26	4720970
35	0.016961051	1.696105072	25	4658192
36	0.016932476	1.693247638	44	4596830
37	0.016835412	1.683541152	56	4545713
38	0.016698336	1.669833637	52	4565428
39	0.016549305	1.654930478	74	4621898
40	0.01641552	1.641551964	107	4643164
41	0.016323357	1.632335673	120	4664024
42	0.016298423	1.629842345	180	4664265
43	0.016351498	1.635149827	189	4700038
44	0.016474463	1.64744635	239	4760191
45	0.016658547	1.665854708	321	4792186
46	0.016895276	1.689527554	355	4802643
47	0.017176156	1.717615578	475	4796640
48	0.017492406	1.749240553	469	4815549
49	0.017834731	1.783473056	626	4876539
50	0.01819315	1.819314988	672	4880709
51	0.018556874	1.855687387	782	4899723
52	0.018914242	1.891424218	826	4856847
53	0.01925273	1.92527297	972	4797133
54	0.019559032	1.955903222	1031	4746289
55	0.019822954	1.982295354	1094	4644598
56	0.020045271	2.004527149	1124	4533260
57	0.020229229	2.022922936	1196	4384266
58	0.020378472	2.037847154	1228	4243488
59	0.02049697	2.049696962	1155	4114185
60	0.020588954	2.058895391	1165	3949915
61	0.020658852	2.065885185	1169	3823871
62	0.020711235	2.071123491	1246	3714707
63	0.020750775	2.075077463	1160	3506068
64	0.020782209	2.078220863	1076	3320769
65	0.020810295	2.081029541	1027	3148437
66	0.020838144	2.083814361	980		2997024
67	0.020866028	2.086602829	868		2808333
68	0.020893949	2.089394948	834		2616172
69	0.020921907	2.092190723	773		2425635
70	0.020949902	2.09499016	681		2201195
71	0.020977933	2.097793262	654		2079054
72	0.021006	2.100600034	596		1940356
73	0.021034105	2.103410481	530		1811918
74	0.021062246	2.106224607	472		1676312
;
run;

proc print data=try;
run;

data try; set try;
dur= age-14;
logdur= log(dur);
run;

proc univariate data=try;
var logdur;
run;

data try; set try;
t1_5=log(1);
run;
proc freq data=try;
tables t1_5;
run;


data try; set try;

T1_5=log(1); T2_5=log(16); T3_5=log(31); T4_5=log(46); T5_5=log(61)
; /*Location of knots 5 25 50 75 95*/
IF logdur <= T1_5 THEN SPL5logdur1=0;
IF T1_5<logdur<=T4_5 THEN DO;
SPL5logdur1=(logdur-T1_5)**3;
END;
IF T4_5<logdur<=T5_5 THEN DO;
SPL5logdur1=(logdur-T1_5)**3 + -((T5_5-T1_5)/(T5_5-T4_5))*(logdur-T4_5)**3;
END;
IF T5_5<logdur THEN DO;
SPL5logdur1=(logdur-T1_5)**3 + -((T5_5-T1_5)/(T5_5-T4_5))*(logdur-T4_5)**3
+ ((T4_5-T1_5)/(T5_5-T4_5))*(logdur-T5_5)**3;
END;
IF logdur= . THEN SPL5logdur1= .;

IF logdur <= T2_5 THEN SPL5logdur2=0;
IF T2_5<logdur<=T4_5 THEN DO;
SPL5logdur2=(logdur-T2_5)**3;
END;
IF T4_5<logdur<=T5_5 THEN DO;
SPL5logdur2=(logdur-T2_5)**3 + -((T5_5-T2_5)/(T5_5-T4_5))*(logdur-T4_5)**3;
END;
IF T5_5<logdur THEN DO;
SPL5logdur2=(logdur-T2_5)**3 + -((T5_5-T2_5)/(T5_5-T4_5))*(logdur-T4_5)**3
+ ((T4_5-T2_5)/(T5_5-T4_5))*(logdur-T5_5)**3;
END;
IF logdur= . THEN SPL5logdur2= .;

IF logdur <= T3_5 THEN SPL5logdur3=0;
IF T3_5<logdur<=T4_5 THEN DO;
SPL5logdur3=(logdur-T3_5)**3;
END;
IF T4_5<logdur<=T5_5 THEN DO;
SPL5logdur3=(logdur-T3_5)**3 + -((T5_5-T3_5)/(T5_5-T4_5))*(logdur-T4_5)**3;
END;
IF T5_5<logdur THEN DO;
SPL5logdur3=(logdur-T3_5)**3 + -((T5_5-T3_5)/(T5_5-T4_5))*(logdur-T4_5)**3
+ ((T4_5-T3_5)/(T5_5-T4_5))*(logdur-T5_5)**3;
END;
IF logdur= . THEN SPL5logdur3= .;

run;

data try; set try;

T1_3=(19); T2_3=(38)
; T3_3=(57)
;  /*Location of knots*/

IF DUR <= T1_3 THEN SPL3DUR=0;
IF T1_3<DUR<=T2_3 THEN DO;
SPL3DUR=(DUR-T1_3)**3;
END;
IF T2_3<DUR<=T3_3 THEN DO;
SPL3DUR=(DUR-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(DUR-T2_3)**3;
END;
IF T3_3<DUR THEN DO;
SPL3DUR=(DUR-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(DUR-T2_3)**3
+((T2_3-T1_3)/(T3_3-T2_3))*(DUR-T3_3)**3;
END;
IF DUR= . THEN SPL3DUR= .;

T1_3=log(19); T2_3=log(38)
; T3_3=log(57)
;  /*Location of knots*/

IF logdur <= T1_3 THEN SPL3logdur=0;
IF T1_3<logdur<=T2_3 THEN DO;
SPL3logdur=(logdur-T1_3)**3;
END;
IF T2_3<logdur<=T3_3 THEN DO;
SPL3logdur=(logdur-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(logdur-T2_3)**3;
END;
IF T3_3<logdur THEN DO;
SPL3logdur=(logdur-T1_3)**3 + -((T3_3-T1_3)/(T3_3-T2_3))*(logdur-T2_3)**3
+((T2_3-T1_3)/(T3_3-T2_3))*(logdur-T3_3)**3;
END;
IF logdur= . THEN SPL3logdur= .;
run;


data try; set try;

hpv16pop= pop*m16;

logdur= log(dur);
logdur2= logdur*logdur;
logdur3= logdur*logdur*logdur;

rate=count/hpv16pop;

lograte=log(rate);

logage=log(age);
logage2= logage*logage;
logage3= logage*logage*logage;

LNHPV16POP= LOG(HPV16POP);

count2= count*0.9*0.7;

run;

***Cubic********;
proc GENMOD data=try;
WHERE AGE < 75;
model COUNT2= LOGDUR logdur2 logdur3/LINK=LOG DIST=poisson OFFSET=LNHPV16POP corrb;
output out=preds3 predicted=p xbeta=x;
run;

***Quadratic*** - final model selected;
proc GENMOD data=try;
WHERE AGE < 75;
model COUNT2= LOGDUR logdur2 /LINK=LOG DIST=poisson OFFSET=LNHPV16POP corrb;
output out=preds3 predicted=p xbeta=x;
run;

***linear***********;
proc GENMOD data=try;
WHERE AGE < 75;
model COUNT2= LOGDUR  /LINK=LOG DIST=poisson OFFSET=LNHPV16POP corrb;
output out=preds3 predicted=p xbeta=x;
run;

*****3-knot spline**********;
proc GENMOD data=try;
WHERE AGE < 75;
model COUNT2= LOGDUR spl3logdur/LINK=LOG DIST=poisson OFFSET=LNHPV16POP corrb;
output out=preds3 predicted=p xbeta=x;
run;

*****5-knot spline**********;
proc GENMOD data=try;
WHERE AGE < 75;
model COUNT2= LOGDUR spl5logdur1 spl5logdur2 spl5logdur3/LINK=LOG DIST=poisson OFFSET=LNHPV16POP corrb;
output out=preds3 predicted=p xbeta=x;
run;

data preds3; set preds3;
pen= (p/hpv16pop)*100000;
run;

data pens; set preds3;
keep age dur rate hpv16pop count2 p pen x;
run;
proc plot data=preds3;
plot rate*dur;
run;


data try; set try;

rate2= count2/pop;

if 15 <= age <= 19 then agecat2=1;
else if 20 <= age <= 24 then agecat2=2;
else if 25 <= age <= 29 then agecat2=3;
else if 30 <= age <= 34 then agecat2=4;
else if 35 <= age <= 39 then agecat2=5;
else if 40 <= age <= 44 then agecat2=6;
else if 45 <= age <= 49 then agecat2=7;
else if 50 <= age <= 54 then agecat2=8;
else if 55 <= age <= 59 then agecat2=9;
else if 60 <= age <= 64 then agecat2=10;
else if 65 <= age <= 69 then agecat2=11;
else if 70 <= age <= 74 then agecat2=12;
else if 75 <= age <= 79 then agecat2=13;

run;

proc univariate data=try;
var count pop;
output out=seer sum=casesum popsum;
run;

data seer; set seer;
rate2= (casesum/popsum);
run;

proc print data=seer;
run;




