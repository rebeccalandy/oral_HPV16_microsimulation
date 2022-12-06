library(data.table)
library(Rcpp)
source("R/functions.R")
sourceCpp("src/functions.cpp")

# Number of simulations per file
M <- 50

# Size of each cohort
n <- 100000

# # Read initial seed from command line
args <- commandArgs(trailingOnly = F)
file_num <-  args[length(args)]
file_num <- as.numeric(unlist(strsplit(file_num,'-')))

seed_initial <- (file_num - 1) * M + 1

# Directory to store results
out.dir <- "Results/"

# Read distribution files
file_list <- list.files("Data/")
rp_mn_list <- lapply(grep("recentnew", file_list), 
                     function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
rp_pois_list <- lapply(grep("recentoral", file_list), 
                     function(x) as.matrix(fread(paste0("Data/", file_list[x]))))
partner_age_dist <- as.matrix(fread("Data/partner_ages.csv"))
exposure_age_dist <- as.matrix(fread("Data/cervix_hpv.csv"))
mortality <- as.matrix(fread("Data/nomortality.csv"))
weights <- as.matrix(fread("Data/weights.csv"))
splines <- fread("Data/splines.csv")
splines[, pop := fread("Data/pop_size.csv")]
calibration_targets <- fread("Data/calibration_targets.csv")
immune_scale <- 1
# if want to write state matrices by smoking status, change to 1
write_smoking<-0

# Value to scale clearance for smokers
#clearance_scale <- c(0.83,0.83,0.83, rep(1,67))
clearance_scale <- c(rep(1,70))

out <- data.table(seed = seed_initial:(seed_initial + M - 1))
system.time(
out <- out[, sim_iter_R(seed, n, rp_mn_list, rp_pois_list,
                        partner_age_dist, exposure_age_dist, 
                        weights,
                        splines,
                        calibration_targets,
                        immune_scale,
                        clearance_scale,
                        write_smoking),
           by = seed]
)
fwrite(out, paste0(out.dir, "calibration_results_", file_num, ".csv"))
