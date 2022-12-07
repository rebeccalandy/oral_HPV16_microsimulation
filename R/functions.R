sim_iter_R <- function(seed, n, 
					   num_partner_dist_mn,
					   num_partner_dist_pois,
					   partner_age_dist,
					   exposure_by_age_list,
					   weights,
					   splines,
					   calibration_targets,
					   immune_scale,
                       clearance_scale,
		       write_smoking)
{
	splines <- copy(splines)
    # Draw seeds for each individual
    set.seed(seed)
    seed_vec <- sample(1:(.Machine$integer.max - 31), n)
	prev_list <- sim_iter(seed, seed_vec, n, num_partner_dist_mn, num_partner_dist_pois,
			 partner_age_dist, weights, exposure_by_age_list,
			 0, immune_scale, clearance_scale, write_smoking)
	
	shape <- prev_list[[2]]
	scale <- prev_list[[3]]

	splines[, prev := prev_list[[1]]]
	splines[, HPVpos := round(pop * prev)]
	splines[, pop.int := round(pop)]
	 
	spline_fit <- glm(cbind(HPVpos, pop.int - HPVpos) ~ age+SPL5AGE1+SPL5AGE2+SPL5AGE3, 
					family=binomial(logit), data=splines[!(age %in% c(15:17, 70:84))])

	splines[, pred := predict(spline_fit, newdata = splines, type = "response")]

	splines[, prev_grp := rep(c("15-17",
								"18-24", 
								"25-29", 
								"30-34", 
								"35-39", 
								"40-44", 
								"45-49", 
								"50-54",
								"55-59",
								"60-64",
								"65-69",
								"70-84"),
								c(3, 7, rep(5, 9), 15))]

	calibration <- splines[prev_grp != "15-17" & prev_grp != "70-84", 
								  pred %*% pop / sum(pop), by = prev_grp]
	setnames(calibration, "V1", "prev_pred")
	calibration[, prev_target := calibration_targets$prev]
	calibration[, gof := (prev_pred - prev_target)^2 / prev_target]
	calibration[, c("shape", "scale") := 
				list(shape, scale)]

	calibration_final <- dcast(calibration, shape + scale ~ 
							   prev_grp, value.var = c("prev_pred", "gof"))
	calibration_final[, paste0("p_clear_", 1:70) := as.list(prev_list[[5]])]
	calibration_final[, paste0("cum_inc", 15:84) := as.list(prev_list[[6]])]
	calibration_final[, trans_prob := prev_list[[4]]]

	calibration_final[, gof_total :=
					  sum(calibration_final[, 
						  grep("gof", names(calibration_final)), with = F])]
	
	splines[, prev_grp_2 := rep(c("15-17",
								"18-24", 
								"25-29", 
								"30-34", 
								"35-39", 
								"40-44", 
								"45-49", 
								"50-54",
								"55-59",
								"60-64",
								"65-69",
								"70-74",
								"75-79",
								"80-84"),
								c(3, 7, rep(5, 12)))]

	calibration_final[, paste0("prev_est_", unique(splines$prev_grp_2)) := 
					  as.list(splines[, prev %*% pop / sum(pop), by = prev_grp_2]$V1)]

	calibration_final[, paste0("prev_est_", unique(splines$age)) := 
					  as.list(splines[, prev %*% pop / sum(pop), by = age]$V1)]

	
	calibration_final
}

sim_iter_R_output <- function(seed, n, 
					  num_partner_dist_mn,
					   num_partner_dist_pois,
					   partner_age_dist,
					   exposure_by_age_list,
					   weights,
					   splines,
					   calibration_targets,
					   immune_scale,
                       clearance_scale,
                       write_smoking)
{
    # Draw seeds for each individual
    set.seed(seed)
    seed_vec <- sample(1:(.Machine$integer.max - 31), n)
	  sim_iter(seed, seed_vec, n, num_partner_dist_mn, num_partner_dist_pois,
			 partner_age_dist, weights, exposure_by_age_list,
			 1, immune_scale, clearance_scale, write_smoking)
}

sim_iter_cancer <- function(vac_scenario, seed, n, 
					  num_partner_dist_mn,
					   num_partner_dist_pois,
					   partner_age_dist,
					   exposure_by_age_list,
					   weights,
					   immune_scale,
                       penetrance_no_smk,
                       penetrance_smk,
                       herd_immunity,
                       vaccine_probs,
                       mortality,
                       internal_sim_N,
                       clearance_scale)
{
     # Draw seeds for each individual
    set.seed(seed)
    seed_vec <- sample(1:(.Machine$integer.max - 31), n)
	cancer_list <- sim_iter_cancer_cpp(vac_scenario, seed, seed_vec, n, 
                                       num_partner_dist_mn, num_partner_dist_pois,
			 partner_age_dist, weights, exposure_by_age_list, immune_scale, 
             penetrance_no_smk, penetrance_smk,
             herd_immunity,vaccine_probs, mortality, internal_sim_N, clearance_scale)
     cancer_list
	#as.data.table(Reduce("rbind", cancer_list))
}
