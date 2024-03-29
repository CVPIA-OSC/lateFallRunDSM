# Fitness Function ------------------
late_fall_run_fitness <- function(
  known_adults,
  seeds,
  params,
  surv_adult_enroute,
  surv_adult_prespawn,
  surv_egg_to_fry_mean_egg_temp_effect,
  sac_surv_juv,
  surv_juv_rear_contact_points,
  surv_juv_rear_prop_diversions,
  surv_juv_rear_total_diversions,
  surv_juv_bypass_int,
  surv_juv_delta_int,
  surv_juv_delta_contact_points,
  surv_juv_delta_total_diverted,
  surv_juv_outmigration_sj_int,
  default_ocean_entry_surv) {

  params_init <- params

  params_init$..surv_adult_enroute_int = surv_adult_enroute
  params_init$..surv_adult_prespawn_int = surv_adult_prespawn
  params_init$..surv_egg_to_fry_mean_egg_temp_effect =  surv_egg_to_fry_mean_egg_temp_effect
  params_init$..surv_juv_rear_int = rep(sac_surv_juv, 31)
  params_init$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params_init$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params_init$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params_init$..surv_juv_bypass_int = surv_juv_bypass_int
  params_init$..surv_juv_delta_int = surv_juv_delta_int
  params_init$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params_init$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params_init$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  params_init$..ocean_entry_success_int = rep(default_ocean_entry_surv, 31)

  keep <- c(1, 3, 7) # modify to 1, 3, 7
  num_obs <- rowSums(!is.na(known_adults[keep, 6:19]))
  total_obs <- sum(!is.na(known_adults[keep, 6:19]))
  weights <- num_obs / total_obs


  tryCatch({
    preds <- late_fall_run_model(mode = "calibrate",
                            seeds = seeds,
                            stochastic = FALSE,
                            ..params = params_init)

    known_nats <- known_adults[keep, 6:19] * (1 - params_init$proportion_hatchery[keep])
    mean_escapent <-rowMeans(known_nats, na.rm = TRUE)

    # watershed_cor <- sapply(1:length(keep), function(i) {
    #   cor(preds[i,], known_nats[i,], use = "pairwise.complete.obs")
    # })
    sse <- sum(((preds[keep,] - known_nats)^2 * weights)/mean_escapent, na.rm = TRUE)

    return(sse)
  },
  error = function(e) return(1e12),
  warning = function(w) return(1e12)
  )
}


# x <- runif(13)
# print(late_fall_run_fitness(
#   known_adults = DSMCalibrationData::grandtab_observed$late_fall,
#   seeds = DSMCalibrationData::grandtab_imputed$late_fall,
#   params = params,
#   x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
#   x[11], x[12], x[13]
# ))
# 



