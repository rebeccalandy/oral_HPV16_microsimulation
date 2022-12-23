Creating input data:
1.	File name: creating_weights.R  
Calculates weights for each subcohort (race by smoking status by lifetime quartile of sexual partners, by age) for men aged 15-79, using NHANES 2009-2016 data.
2.	File name: nhis2009_16_for_weights_age_80up.R
Calculates weights for each subcohort for men aged 80-84, using NHIS 2009-2016 data.
3.	File name: create_splines.R
Calculates the spline terms when using age with a 5 knot spline
4.	File name: NHANES2009_16.create.variables.sas
File creating variable in NHANES 2009-16 for use in future code
5.	File name: NHANES0916.make.wt8yr.sas
File creating post stratification weights for NHANES 2009-2016
6.	File name: Logistic_regression_new_partner.sas
Logistic regression model to predict whether an individual has an oral sex partner in the past 12 months
7.	File name: negative_binomial_number_of_partners.R
Negative binomial model of the number of recent female oral sex partners, using data from men in NHANES 2009-16. 
8.	File name: partner_age_distribution.R
Calculates the age distribution of men’s sexual partners, using data from NHANES 2005-6 to get 3 broad categories, then data from NATSAL 3 to get 5-year groups (relative to the age of the man)
9.	File name: cervical_HPV_prevalence_median_quartile_modelled.R
Models cervical HPV16 prevalence in NHANES 2005-06, using a 3-knot spline for age
10.	File name: gridsearch_code_upto74.sas
Running a model to get the starting points of a grid search for the probability of developing cancer within the next year from an HPV16 infection, by duration of infection.  
11.	File name: penetrance.quadratic.gridsearch.R
Create a file containing all the penetrance combinations (probability of developing cancer given an x-year HPV infection)
12.	File name: nhis2010_14_men_1yrdeathrates.R
1-year mortality model using data from NHIS 2010-2014 

Running the simulations:
1.	File name: sims.R
Running natural history simulations; uses src/functions.cpp and R/functions.R
2.	File name: identifying_top_datasets.R
Identify best-calibrated natural history simulations, calibrating to oral HPV prevalence
3.	File name: sim_output.R
Get simulation output for the best-calibrated natural history simulations
4.	File name: create_files_from_output_for_penetrance.R
Create 3 input files for next step from the output from the natural history simulations: the duration distribution of prevalent HPV16 infections, number of people with HPV16 per 100,000, and total number of people with an HPV16 infection.
5.	File name: penetrace.R
Run all combinations of penetrance for each of the best calibrated natural history datasets
6.	File name: identifying_top_penetrance_datasets_gridsearch.R
Identify best-calibrated cancer incidence simulations, calibrating to cancer incidence, and outputting the penetrance values that provided the best calibration for each natural history dataset
7.	File name: sims_cancer.R
Run simulations with vaccination and herd immunity

Analyzing the output
1.	File name: analysing_cancer_output.R
Creating a summary of the total number of cancers under each vaccination scenario, grouped by birth cohort
2.	File name: cancer_reduction_SQ26_calc_for_each_natural_history.R
Calculating the reduction in cervical cancer incidence by birth cohort, relative to under status quo vaccination up to age 26
