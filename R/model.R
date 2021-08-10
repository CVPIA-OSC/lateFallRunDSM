#' @title Late-Fall Run Chinook Model
#' @description Late-Fall Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"} or \code{"calibrate"}. 
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels. More details at \code{\link{params}}
#' @source IP-117068
#' @examples
#' late_fall_run_seeds <- lateFallRunDSM::late_fall_run_model(mode = "seed")
#' lateFallRunDSM::late_fall_run_model(scenario = DSMscenario::scenarios$ONE,
#'                            mode = "simulate",
#'                            seeds = late_fall_run_seeds)
#' @export
late_fall_run_model <- function(scenario = NULL, mode = c("seed", "simulate", "calibrate"),
                                seeds = NULL, ..params = lateFallRunDSM::params,
                                stochastic = FALSE){
  
  mode <- match.arg(mode)
  
  if (mode == "simulate") {
    if (is.null(scenario)) {
      # the do nothing scenario to force habitat degradation
      scenario <- DSMscenario::scenarios$NO_ACTION
    }
    
    habitats <- list(
      spawning_habitat = ..params$spawning_habitat,
      inchannel_habitat_fry = ..params$inchannel_habitat_fry,
      inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
      floodplain_habitat = ..params$floodplain_habitat,
      weeks_flooded = ..params$weeks_flooded
    )
    
    scenario_data <- DSMscenario::load_scenario(scenario,
                                                habitat_inputs = habitats,
                                                species = DSMscenario::species$LATE_FALL_RUN)
    
    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded
  }
  
  output <- list(
    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(lateFallRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(lateFallRunDSM::watershed_labels, 1:20)),
    proportion_natural = matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(lateFallRunDSM::watershed_labels, 1:20))
  )
  
  # initialize 31 x 4 matrices for natal fish, migrants, and ocean fish
  lower_mid_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels, lateFallRunDSM::size_class_labels))
  lower_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels, lateFallRunDSM::size_class_labels))
  upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels[1:15], lateFallRunDSM::size_class_labels))
  sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels[1:15], lateFallRunDSM::size_class_labels))
  yolo_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels[18:20], lateFallRunDSM::size_class_labels))
  san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels[28:30], lateFallRunDSM::size_class_labels))
  north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels[1:23], lateFallRunDSM::size_class_labels))
  south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels, lateFallRunDSM::size_class_labels))
  juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(lateFallRunDSM::watershed_labels, lateFallRunDSM::size_class_labels))
  
  adults <- switch (mode,
                    "seed" = lateFallRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = seeds
  )
  
  simulation_length <- switch(mode,
                              "seed" = 5,
                              "simulate" = 20,
                              "calibrate" = 20)
  
  for (year in 1:simulation_length) {
    adults_in_ocean <- numeric(31)
    
    avg_ocean_transition_month <- ocean_transition_month(stochastic)
    
    # TODO stochastic?
    hatch_adults <- rmultinom(1, size = round(runif(1, 1753,7012)), prob = ..params$hatchery_allocation)[ , 1]
    
    spawners <- get_spawning_adults(year, round(adults), hatch_adults, mode = mode,
                                    prop_flow_natal = ..params$prop_flow_natal,
                                    south_delta_routed_watersheds = ..params$south_delta_routed_watersheds,
                                    cc_gates_days_closed = ..params$cc_gates_days_closed,
                                    gates_overtopped = ..params$gates_overtopped,
                                    tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                    yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                    month_return_proportions = ..params$month_return_proportions, 
                                    migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                    ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                    .adult_stray_intercept = ..params$.adult_stray_intercept,
                                    .adult_stray_wild = ..params$.adult_stray_wild,
                                    .adult_stray_natal_flow = ..params$.adult_stray_natal_flow,
                                    .adult_stray_cross_channel_gates_closed = ..params$.adult_stray_cross_channel_gates_closed,
                                    .adult_stray_prop_bay_trans = ..params$.adult_stray_prop_bay_trans,
                                    .adult_stray_prop_delta_trans = ..params$.adult_stray_prop_delta_trans,
                                    .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                    .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                    .adult_en_route_adult_harvest_rate = ..params$.adult_en_route_adult_harvest_rate,
                                    stochastic = stochastic)
    
    init_adults <- spawners$init_adults
    
    output$spawners[ , year] <- init_adults
    output$proportion_natural[ , year] <- spawners$proportion_natural
    
    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = 1 - ..params$proportion_hatchery,
      scour = ..params$prob_nest_scoured,
      temperature_effect = ..params$mean_egg_temp_effect,
      .proportion_natural = ..params$.surv_egg_to_fry_proportion_natural,
      .scour = ..params$.surv_egg_to_fry_scour,
      ..surv_egg_to_fry_int = ..params$..surv_egg_to_fry_int
    )
    
    next_year <- year + 1
    min_spawn_habitat <- apply(..params$spawning_habitat[ , c(12, 1, 2), c(year, next_year, next_year)], 1, min)
    
    accumulated_degree_days <- cbind(oct = rowSums(..params$degree_days[ , c(10:12, 1, 2), c(year, year, next_year, next_year)]),
                                     nov = rowSums(..params$degree_days[ , c(11:12, 1, 2), c(year, next_year, next_year)]),
                                     dec = rowSums(..params$degree_days[ , c(12, 1, 2), c(year, next_year, next_year)]),
                                     jan = rowSums(..params$degree_days[ , c(1, 2), c(next_year, next_year)]),
                                     feb = ..params$degree_days[ , 2, next_year])
    
    average_degree_days <- apply(accumulated_degree_days, 1, 
                                 weighted.mean, 
                                 ..params$month_return_proportions["Battle and Clear Creeks", ])
    
    average_degree_days[1] <- weighted.mean(accumulated_degree_days[1, ], 
                                            ..params$month_return_proportions["Upper Sacramento River", ])
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)
    
    juveniles <- spawn_success(escapement = init_adults,
                               adult_prespawn_survival = prespawn_survival,
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = ..params$prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat,
                               sex_ratio = ..params$spawn_success_sex_ratio,
                               redd_size = ..params$spawn_success_redd_size,
                               fecundity = ..params$spawn_success_fecundity,
                               stochastic = stochastic)
    
    fish_1 <- fish_2 <- fish_3 <- list(juveniles = juveniles,
                                       lower_mid_sac_fish = lower_mid_sac_fish,
                                       lower_sac_fish = lower_sac_fish,
                                       upper_mid_sac_fish = upper_mid_sac_fish,
                                       sutter_fish = sutter_fish,
                                       yolo_fish = yolo_fish,
                                       san_joaquin_fish = san_joaquin_fish,
                                       north_delta_fish = north_delta_fish,
                                       south_delta_fish = south_delta_fish,
                                       juveniles_at_chipps = juveniles_at_chipps,
                                       adults_in_ocean = adults_in_ocean)
    
    #regular outmigration ruleset
    for (month in 4:11) {
      habitat <- get_habitat(year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             delta_habitat = ..params$delta_habitat)
      
      rearing_survival <- get_rearing_survival(year, month, mode = mode,
                                               survival_adjustment = scenario_data$survival_adjustment,
                                               avg_temp = ..params$avg_temp,
                                               avg_temp_delta = ..params$avg_temp_delta,
                                               prob_strand_early = ..params$prob_strand_early,
                                               prob_strand_late = ..params$prob_strand_late,
                                               proportion_diverted = ..params$proportion_diverted,
                                               total_diverted = ..params$total_diverted,
                                               delta_proportion_diverted = ..params$delta_proportion_diverted,
                                               delta_total_diverted = ..params$delta_total_diverted,
                                               weeks_flooded = ..params$weeks_flooded,
                                               prop_high_predation = ..params$prop_high_predation,
                                               contact_points = ..params$contact_points,
                                               delta_contact_points = ..params$delta_contact_points,
                                               delta_prop_high_predation = ..params$delta_prop_high_predation,
                                               ..surv_juv_rear_int= ..params$..surv_juv_rear_int,
                                               ..surv_juv_rear_contact_points= ..params$..surv_juv_rear_contact_points,
                                               .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                               ..surv_juv_rear_prop_diversions= ..params$..surv_juv_rear_prop_diversions,
                                               .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                               ..surv_juv_rear_total_diversions= ..params$..surv_juv_rear_total_diversions,
                                               .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                                               .surv_juv_delta_contact_points = ..params$.surv_juv_delta_contact_points,
                                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted,
                                               .surv_juv_delta_total_diverted = ..params$.surv_juv_delta_total_diverted,
                                               .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                               .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                               .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                               .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                               .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                               .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                               .surv_juv_bypass_avg_temp_thresh = ..params$.surv_juv_bypass_avg_temp_thresh,
                                               .surv_juv_bypass_high_predation = ..params$.surv_juv_bypass_high_predation,
                                               .surv_juv_bypass_medium = ..params$.surv_juv_bypass_medium,
                                               .surv_juv_bypass_large = ..params$.surv_juv_bypass_large,
                                               .surv_juv_bypass_floodplain = ..params$.surv_juv_bypass_floodplain,
                                               .surv_juv_delta_avg_temp_thresh = ..params$.surv_juv_delta_avg_temp_thresh,
                                               .surv_juv_delta_high_predation = ..params$.surv_juv_delta_high_predation,
                                               .surv_juv_delta_prop_diverted = ..params$.surv_juv_delta_prop_diverted,
                                               .surv_juv_delta_medium = ..params$.surv_juv_delta_medium,
                                               .surv_juv_delta_large = ..params$.surv_juv_delta_large, 
                                               min_survival_rate = ..params$min_survival_rate,
                                               stochastic = stochastic)
      
      migratory_survival <- get_migratory_survival(year, month,
                                                   cc_gates_prop_days_closed = ..params$cc_gates_prop_days_closed,
                                                   freeport_flows = ..params$freeport_flows,
                                                   vernalis_flows = ..params$vernalis_flows,
                                                   stockton_flows = ..params$stockton_flows,
                                                   vernalis_temps = ..params$vernalis_temps,
                                                   prisoners_point_temps = ..params$prisoners_point_temps,
                                                   CVP_exports = ..params$CVP_exports,
                                                   SWP_exports = ..params$SWP_exports,
                                                   upper_sacramento_flows = ..params$upper_sacramento_flows,
                                                   delta_inflow = ..params$delta_inflow,
                                                   avg_temp_delta = ..params$avg_temp_delta,
                                                   avg_temp = ..params$avg_temp,
                                                   delta_proportion_diverted = ..params$delta_proportion_diverted,
                                                   .surv_juv_outmigration_sac_delta_intercept_one = ..params$.surv_juv_outmigration_sac_delta_intercept_one,
                                                   .surv_juv_outmigration_sac_delta_intercept_two = ..params$.surv_juv_outmigration_sac_delta_intercept_two,
                                                   .surv_juv_outmigration_sac_delta_intercept_three = ..params$.surv_juv_outmigration_sac_delta_intercept_three,
                                                   .surv_juv_outmigration_sac_delta_delta_flow = ..params$.surv_juv_outmigration_sac_delta_delta_flow,
                                                   .surv_juv_outmigration_sac_delta_avg_temp = ..params$.surv_juv_outmigration_sac_delta_avg_temp,
                                                   .surv_juv_outmigration_sac_delta_perc_diversions = ..params$.surv_juv_outmigration_sac_delta_perc_diversions,
                                                   .surv_juv_outmigration_sac_delta_medium = ..params$.surv_juv_outmigration_sac_delta_medium,
                                                   .surv_juv_outmigration_sac_delta_large = ..params$.surv_juv_outmigration_sac_delta_large,
                                                   ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                   ..surv_juv_outmigration_sac_int_one = ..params$..surv_juv_outmigration_sac_int_one,
                                                   ..surv_juv_outmigration_sac_prop_diversions = ..params$..surv_juv_outmigration_sac_prop_diversions,
                                                   ..surv_juv_outmigration_sac_total_diversions = ..params$..surv_juv_outmigration_sac_total_diversions,
                                                   ..surv_juv_outmigration_sac_int_two = ..params$..surv_juv_outmigration_sac_int_two,
                                                   .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                   .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large, 
                                                   min_survival_rate = ..params$min_survival_rate,
                                                   surv_juv_outmigration_sac_delta_model_weights = ..params$surv_juv_outmigration_sac_delta_model_weights,
                                                   stochastic = stochastic)
      
      fish_1 <- juvenile_month_dynamic(hypothesis = 1, fish_1, year = year, month = month, 
                                       rearing_survival = rearing_survival, 
                                       migratory_survival = migratory_survival, 
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic)
      
      fish_2 <- juvenile_month_dynamic(hypothesis = 2, fish_2, year = year, month = month, 
                                       rearing_survival = rearing_survival, 
                                       migratory_survival = migratory_survival, 
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic)
      
      fish_3 <- juvenile_month_dynamic(hypothesis = 3, fish_3, year = year, month = month, 
                                       rearing_survival = rearing_survival, 
                                       migratory_survival = migratory_survival, 
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic)
    } # end of month loop
    
    #combine the different models by weighting them equally. Will want to vary the weights in sensitivity analysis.
    juveniles_at_chipps <- ..params$juveniles_at_chipps_model_weights[1] * fish_1$juveniles_at_chipps +
      ..params$juveniles_at_chipps_model_weights[2] * fish_2$juveniles_at_chipps +
      ..params$juveniles_at_chipps_model_weights[3] * fish_3$juveniles_at_chipps 

    adults_in_ocean <- ..params$adults_in_ocean_model_weights[1] * fish_1$adults_in_ocean + 
      ..params$adults_in_ocean_model_weights[2] * fish_2$adults_in_ocean +
      ..params$adults_in_ocean_model_weights[3] * fish_3$adults_in_ocean
    
    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% lateFallRunDSM::params$mass_by_size_class
    
    adults_returning <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, adults_in_ocean[watershed], prob = c(.432, .566, .02))  
      } else {
        round(adults_in_ocean[watershed] * c(.432, .566, .02))
      }
    }))
    
    # distribute returning adults for future spawning
    if (mode != "calibrate") {
      adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + adults_returning
    }
    
  } # end year for loop
  
  if (mode == "seed") {
    return(adults[ , 6:30])
  } else if (mode == "calibrate") {
    return(output)
  }
  
  spawn_change <- sapply(1:19, function(year) {
    output$spawners[ , year] / (output$spawners[ , year + 1] + 1)
  })
  
  viable <- spawn_change >= 1 & output$proportion_natural[ , -1] >= 0.9 & output$spawners[ , -1] >= 833
  
  output$viability_metrics <- sapply(1:4, function(group) {
    colSums(viable[which(lateFallRunDSM::params$diversity_group == group), ])
  })
  
  return(output)
  
}