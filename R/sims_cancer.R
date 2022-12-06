library(data.table)
library(Rcpp)
source("R/functions.R")
sourceCpp("src/functions.cpp")

# Number of simulations per file
M <- 1

# Size of each cohort
n <- 100000

# # Read initial seed from command line
args <- commandArgs(trailingOnly = F)
file_num <-  args[length(args)]
file_num <- as.numeric(unlist(strsplit(file_num,'-')))

seed_num<-file_num[1]
internal_imp_num<-file_num[2]
vac_num<-file_num[3]
print(seed_num)
print(internal_imp_num)

seed <- as.data.frame(fread("calibrated_seeds.csv"))[seed_num,1]
print(seed)
# Directory to store results
out.dir <- "Results/cancer/"

# Read distribution files
file_list <- list.files("Data/")
rp_mn_list <- lapply(grep("recentnew", file_list), 
                     function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
rp_pois_list <- lapply(grep("recentoral", file_list), 
                       function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
partner_age_dist <- as.matrix(fread("Data/partner_ages.csv"))
exposure_age_dist <- as.matrix(fread("Data/cervix_hpv.csv"))
mortality <- as.matrix(fread("Data/mortality32.csv"))
weights <- as.matrix(fread("Data/longitudinal_weights.csv"))
immune_scale <- 1

# For primary analysis, penetrance_no_smk is penetrance for full population
# so we set the two vectors to be equal.
penetrance_no_smk <-as.matrix(fread("Data/penetrance.csv"))[seed_num,]
penetrance_smk <- penetrance_no_smk

herd_immunity <- lapply(grep("herd_immunity", file_list),
                        function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
vaccine_probs_list <- lapply(grep("vac", file_list),
                             function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
internal_sim_N <- internal_imp_num
# if want to write state matrices by smoking status, change to 1
write_smoking<-0

# Value to scale clearance for smokers
clearance_scale <- c(rep(1,70))

system.time(
    sim_iter_cancer(vac_num, seed, n, rp_mn_list, rp_pois_list, partner_age_dist,
                     exposure_age_dist, weights, immune_scale, penetrance_no_smk, 
                     penetrance_smk, herd_immunity[[vac_num]],
                     vaccine_probs_list[[vac_num]], mortality, internal_sim_N, clearance_scale)
)

