#' Juvenile Month Dynamics
#' @param hypothesis
#' @param fish list object containing tracking matrices for juveniles, juveniles at chips, and adults in ocean
#' @param year
#' @param month
#' @param rearing_survival
#' @param migratory_survival
#' @param habitat
#' @param ..params   
#' @examples 
#' fish = list(juveniles = juveniles, north_delta_fish = north_delta_fish, south_delta_fish = south_delta_fish,
#' juveniles_at_chipps = juveniles_at_chipps, 
#' adults_in_ocean = adults_in_ocean)
juvenile_month_dynamic <- function(hypothesis, fish, year, month, rearing_survival, 
                                   migratory_survival, habitat, ..params, 
                                   avg_ocean_transition_month, stochastic) {
  
  juveniles <- fish$juveniles
  lower_mid_sac_fish <- fish$lower_mid_sac_fish
  lower_sac_fish <- fish$lower_sac_fish
  upper_mid_sac_fish <- fish$upper_mid_sac_fish
  sutter_fish <- fish$sutter_fish
  yolo_fish <- fish$yolo_fish
  san_joaquin_fish <- fish$san_joaquin_fish
  north_delta_fish <- fish$north_delta_fish
  south_delta_fish <- fish$south_delta_fish
  juveniles_at_chipps <- fish$juveniles_at_chipps
  adults_in_ocean <- fish$adults_in_ocean
  
  if (hypothesis == 3) {
    route <- lateFallRunDSM::route_alternative
    route_regional <- lateFallRunDSM::route_regional_alternative
    route_bypass <- lateFallRunDSM::route_bypass_alternative
  } 
  
  migrants <- matrix(0, nrow = 31, ncol = 4, 
                     dimnames = list(lateFallRunDSM::watershed_labels, 
                                     lateFallRunDSM::size_class_labels))
  
  if (month == 11) {
    # all remaining fish outmigrate
    migrants <- juveniles
    
    sutter_fish <- migrate(sutter_fish, migratory_survival$sutter, stochastic = stochastic)
    upper_mid_sac_fish <- migrate(upper_mid_sac_fish + migrants[1:15, ], migratory_survival$uppermid_sac, stochastic = stochastic)
    migrants[1:15, ] <- upper_mid_sac_fish + sutter_fish
    
    lower_mid_sac_fish <- migrate(lower_mid_sac_fish + migrants[1:20, ], migratory_survival$lowermid_sac, stochastic = stochastic)
    yolo_fish <- migrate(yolo_fish, migratory_survival$yolo, stochastic = stochastic)
    migrants[1:20, ] <- lower_mid_sac_fish + yolo_fish
    
    lower_sac_fish <- migrate(lower_sac_fish + migrants[1:27, ], migratory_survival$lower_sac, stochastic = stochastic)
    
    san_joaquin_fish <- migrate(migrants[28:30, ] + san_joaquin_fish, migratory_survival$san_joaquin, stochastic = stochastic)
    migrants[28:30, ] <- san_joaquin_fish
    
    delta_fish <- route_and_rear_deltas(year = year, month = month,
                                        migrants = round(migrants),
                                        north_delta_fish = north_delta_fish,
                                        south_delta_fish = south_delta_fish,
                                        north_delta_habitat = habitat$north_delta,
                                        south_delta_habitat = habitat$south_delta,
                                        freeport_flows = ..params$freeport_flows,
                                        cc_gates_days_closed = ..params$cc_gates_days_closed,
                                        rearing_survival_delta = rearing_survival$delta,
                                        migratory_survival_delta = migratory_survival$delta,
                                        migratory_survival_bay_delta = migratory_survival$bay_delta,
                                        juveniles_at_chipps = juveniles_at_chipps,
                                        growth_rates = ..params$growth_rates,
                                        territory_size = ..params$territory_size,
                                        stochastic = stochastic)
    
    juveniles_at_chipps <- delta_fish$juveniles_at_chipps
    migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
    
  } else {
    
    if (hypothesis == 2 & month == 4) {
      # send specified proportion of fry out of tributaries
      leave_battle <- if (stochastic) {
        rbinom(1, juveniles[3, ], ..params$prob_fry_leave)
      } else {
        round(juveniles[3, ] * ..params$prob_fry_leave)
      }
      
      leave_clear <- if (stochastic) {
        rbinom(1, juveniles[7, ], ..params$prob_fry_leave)
      } else {
        round(juveniles[7, ] * ..params$prob_fry_leave)
      }
      
      juveniles[1, ] <- juveniles[1, ] + leave_battle + leave_clear
      juveniles[3, ] <- juveniles[3, ] - leave_battle
      juveniles[7, ] <- juveniles[7, ] - leave_clear
    }
    
    upper_sac_trib_fish <-  route(year = year,
                                  month = month,
                                  juveniles = juveniles[1:15, ],
                                  inchannel_habitat = habitat$inchannel[1:15],
                                  floodplain_habitat = habitat$floodplain[1:15],
                                  prop_pulse_flows = ..params$prop_pulse_flows[1:15, ],
                                  .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                  .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                  .pulse_movement_medium = ..params$.pulse_movement_medium,
                                  .pulse_movement_large = ..params$.pulse_movement_large,
                                  .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                  .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                  .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                  .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                  territory_size = ..params$territory_size,
                                  temperature_downstream = ..params$avg_temp[16, month, year],
                                  density_dependent_survival = ..params$density_dependent_survival,
                                  stochastic = stochastic)
    
    upper_sac_trib_rear <- rear(juveniles = upper_sac_trib_fish$inchannel,
                                survival_rate = rearing_survival$inchannel[1:15, ],
                                growth = ..params$growth_rates,
                                floodplain_juveniles = upper_sac_trib_fish$floodplain,
                                floodplain_survival_rate = rearing_survival$floodplain[1:15, ],
                                floodplain_growth = ..params$growth_rates_floodplain,
                                weeks_flooded = ..params$weeks_flooded[1:15, month, year],
                                stochastic = stochastic)
    
    juveniles[1:15, ] <- upper_sac_trib_rear$inchannel + upper_sac_trib_rear$floodplain

    # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
    # regional fish stay and rear
    # or migrate further downstream or in sutter bypass
    
    upper_mid_sac_fish <- route_regional(month = month,
                                         year = year,
                                         migrants = upper_mid_sac_fish + upper_sac_trib_fish$migrants,
                                         inchannel_habitat = habitat$inchannel[16],
                                         floodplain_habitat = habitat$floodplain[16],
                                         prop_pulse_flows = ..params$prop_pulse_flows[16, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$uppermid_sac,
                                         proportion_flow_bypass = ..params$proportion_flow_bypass,
                                         detour = "sutter",
                                         territory_size = ..params$territory_size,
                                         temperature_downstream = ..params$avg_temp[21, month, year],
                                         density_dependent_survival = ..params$density_dependent_survival,
                                         stochastic = stochastic)
    
    sutter_fish <- route_bypass(bypass_fish = sutter_fish + upper_mid_sac_fish$detoured,
                                bypass_habitat = habitat$sutter,
                                migration_survival_rate = migratory_survival$sutter,
                                territory_size = ..params$territory_size,
                                temperature_downstream = ..params$avg_temp[21, month, year],
                                density_dependent_survival = ..params$density_dependent_survival,
                                stochastic = stochastic)
    
    migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants
    
    upper_mid_sac_fish <- rear(juveniles = upper_mid_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[16, ],
                               growth = ..params$growth_rates,
                               floodplain_juveniles = upper_mid_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[16, ],
                               floodplain_growth = ..params$growth_rates_floodplain,
                               weeks_flooded = rep(..params$weeks_flooded[16, month, year], nrow(upper_mid_sac_fish$inchannel)),
                               stochastic = stochastic)
    
    upper_mid_sac_fish <- upper_mid_sac_fish$inchannel + upper_mid_sac_fish$floodplain
    
    sutter_fish <- rear(juveniles = sutter_fish$inchannel,
                        survival_rate = matrix(rep(rearing_survival$sutter, nrow(sutter_fish$inchannel)), ncol = 4, byrow = TRUE),
                        growth = ..params$growth_rates,
                        stochastic = stochastic)
    
    
    
    # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
    # regional fish stay and rear
    # or migrate further downstream  or in yolo bypass
    lower_mid_sac_trib_fish <- route(year = year,
                                     month = month,
                                     juveniles = juveniles[18:20, ],
                                     inchannel_habitat = habitat$inchannel[18:20],
                                     floodplain_habitat = habitat$floodplain[18:20],
                                     prop_pulse_flows =  ..params$prop_pulse_flows[18:20, ],
                                     .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                     .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                     .pulse_movement_medium = ..params$.pulse_movement_medium,
                                     .pulse_movement_large = ..params$.pulse_movement_large,
                                     .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                     .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                     .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                     .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                     territory_size = ..params$territory_size,
                                     temperature_downstream = ..params$avg_temp[24, month, year],
                                     density_dependent_survival = ..params$density_dependent_survival,
                                     stochastic = stochastic)
    
    lower_mid_sac_trib_rear <- rear(juveniles = lower_mid_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[18:20, ],
                                    growth = ..params$growth_rates,
                                    floodplain_juveniles = lower_mid_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[18:20, ],
                                    floodplain_growth = ..params$growth_rates_floodplain,
                                    weeks_flooded = ..params$weeks_flooded[18:20, month, year],
                                    stochastic = stochastic)
    
    juveniles[18:20, ] <- lower_mid_sac_trib_rear$inchannel + lower_mid_sac_trib_rear$floodplain
    
    migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants 
    
    lower_mid_sac_fish <- route_regional(month = month,
                                         year = year,
                                         migrants = lower_mid_sac_fish + migrants[1:20, ],
                                         inchannel_habitat = habitat$inchannel[21],
                                         floodplain_habitat = habitat$floodplain[21],
                                         prop_pulse_flows = ..params$prop_pulse_flows[21, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$lowermid_sac,
                                         proportion_flow_bypass = ..params$proportion_flow_bypass,
                                         detour = 'yolo',
                                         territory_size = ..params$territory_size,
                                         temperature_downstream = ..params$avg_temp[24, month, year],
                                         density_dependent_survival = ..params$density_dependent_survival,
                                         stochastic = stochastic)
    
    
    yolo_fish <- route_bypass(bypass_fish = yolo_fish + lower_mid_sac_fish$detoured,
                              bypass_habitat = habitat$yolo,
                              migration_survival_rate = migratory_survival$yolo,
                              territory_size = ..params$territory_size,
                              temperature_downstream = 17, # hardcoded for <18C
                              density_dependent_survival = ..params$density_dependent_survival,
                              stochastic = stochastic)
    
    
    migrants[1:20, ] <- lower_mid_sac_fish$migrants + yolo_fish$migrants
    
    lower_mid_sac_fish <- rear(juveniles = lower_mid_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[21, ],
                               growth = ..params$growth_rates,
                               floodplain_juveniles = lower_mid_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[21, ],
                               floodplain_growth = ..params$growth_rates_floodplain,
                               weeks_flooded = rep(..params$weeks_flooded[21, month, year], nrow(lower_mid_sac_fish$inchannel)),
                               stochastic = stochastic)
    
    lower_mid_sac_fish <- lower_mid_sac_fish$inchannel + lower_mid_sac_fish$floodplain
    
    yolo_fish <- rear(juveniles = yolo_fish$inchannel,
                      survival_rate = matrix(rep(rearing_survival$yolo, nrow(yolo_fish$inchannel)), ncol = 4, byrow = TRUE),
                      growth = ..params$growth_rates,
                      stochastic = stochastic)
    
    # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
    # regional fish stay and rear
    # or migrate north delta
    lower_sac_trib_fish <- route(year = year,
                                 month = month,
                                 juveniles = juveniles[23, , drop = FALSE],
                                 inchannel_habitat = habitat$inchannel[23],
                                 floodplain_habitat = habitat$floodplain[23],
                                 prop_pulse_flows =  ..params$prop_pulse_flows[23, , drop = FALSE],
                                 .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                 .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                 .pulse_movement_medium = ..params$.pulse_movement_medium,
                                 .pulse_movement_large = ..params$.pulse_movement_large,
                                 .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                 .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                 .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                 .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                 territory_size = ..params$territory_size,
                                 temperature_downstream = ..params$avg_temp_delta[month, year, 1],
                                 density_dependent_survival = ..params$density_dependent_survival,
                                 stochastic = stochastic)
    
    lower_sac_trib_rear <- rear(juveniles = lower_sac_trib_fish$inchannel,
                                survival_rate = rearing_survival$inchannel[23, , drop = FALSE],
                                growth = ..params$growth_rates,
                                floodplain_juveniles = lower_sac_trib_fish$floodplain,
                                floodplain_survival_rate = rearing_survival$floodplain[23, , drop = FALSE],
                                floodplain_growth = ..params$growth_rates_floodplain,
                                weeks_flooded = ..params$weeks_flooded[23, month, year],
                                stochastic = stochastic)
    
    juveniles[23, ] <- lower_sac_trib_rear$inchannel + lower_sac_trib_rear$floodplain
    
    migrants[23, ] <- lower_sac_trib_fish$migrants
    
    lower_sac_fish <- route_regional(month = month,
                                     year = year,
                                     migrants = lower_sac_fish + migrants[1:27, ],
                                     inchannel_habitat = habitat$inchannel[24],
                                     floodplain_habitat = habitat$floodplain[24],
                                     prop_pulse_flows = ..params$prop_pulse_flows[24, , drop = FALSE],
                                     migration_survival_rate = migratory_survival$lower_sac,
                                     territory_size = ..params$territory_size,
                                     temperature_downstream = 17, #hardcoded <18C
                                     density_dependent_survival = ..params$density_dependent_survival,
                                     stochastic = stochastic)
    
    migrants[1:27, ] <- lower_sac_fish$migrants
    
    lower_sac_fish <- rear(juveniles = lower_sac_fish$inchannel,
                           survival_rate = rearing_survival$inchannel[24, ],
                           growth = ..params$growth_rates,
                           floodplain_juveniles = lower_sac_fish$floodplain,
                           floodplain_survival_rate = rearing_survival$floodplain[24, ],
                           floodplain_growth = ..params$growth_rates_floodplain,
                           weeks_flooded = rep(..params$weeks_flooded[24, month, year], nrow(lower_sac_fish$inchannel)),
                           stochastic = stochastic)
    
    lower_sac_fish <- lower_sac_fish$inchannel + lower_sac_fish$floodplain
    
    # route southern natal fish stay and rear or migrate downstream ------
    
    # route migrant fish into South Delta Region (fish from watersheds 25:27)
    # regional fish stay and rear
    # or migrate to south delta
    south_delta_trib_fish <- route(year = year,
                                   month = month,
                                   juveniles = juveniles[25:27, ],
                                   inchannel_habitat = habitat$inchannel[25:27],
                                   floodplain_habitat = habitat$floodplain[25:27],
                                   prop_pulse_flows =  ..params$prop_pulse_flows[25:27, ],
                                   .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                   .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                   .pulse_movement_medium = ..params$.pulse_movement_medium,
                                   .pulse_movement_large = ..params$.pulse_movement_large,
                                   .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                   .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                   .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                   .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                   territory_size = ..params$territory_size,
                                   temperature_downstream = 17, #hardcoded <18C
                                   density_dependent_survival = ..params$density_dependent_survival,
                                   stochastic = stochastic)
    
    south_delta_trib_rear <- rear(juveniles = south_delta_trib_fish$inchannel,
                                  survival_rate = rearing_survival$inchannel[25:27, ],
                                  growth = ..params$growth_rates,
                                  floodplain_juveniles = south_delta_trib_fish$floodplain,
                                  floodplain_survival_rate = rearing_survival$floodplain[25:27, ],
                                  floodplain_growth = ..params$growth_rates_floodplain,
                                  weeks_flooded = ..params$weeks_flooded[25:27, month, year],
                                  stochastic = stochastic)
    
    juveniles[25:27, ] <- south_delta_trib_rear$inchannel + south_delta_trib_rear$floodplain
    
    migrants[25:27, ] <- south_delta_trib_fish$migrants
    
    # route migrant fish into San Joquin River (fish from watersheds 28:30)
    # regional fish stay and rear
    # or migrate to south delta
    
    san_joaquin_trib_fish <- route(year = year,
                                   month = month,
                                   juveniles = juveniles[28:30, ],
                                   inchannel_habitat = habitat$inchannel[28:30],
                                   floodplain_habitat = habitat$floodplain[28:30],
                                   prop_pulse_flows =  ..params$prop_pulse_flows[28:30, ],
                                   .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                   .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                   .pulse_movement_medium = ..params$.pulse_movement_medium,
                                   .pulse_movement_large = ..params$.pulse_movement_large,
                                   .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                   .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                   .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                   .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                   territory_size = ..params$territory_size,
                                   temperature_downstream = ..params$avg_temp[31, month, year],
                                   density_dependent_survival = ..params$density_dependent_survival,
                                   stochastic = stochastic)
    
    san_joaquin_trib_rear <- rear(juveniles = san_joaquin_trib_fish$inchannel,
                                  survival_rate = rearing_survival$inchannel[28:30, ],
                                  growth = ..params$growth_rates,
                                  floodplain_juveniles = san_joaquin_trib_fish$floodplain,
                                  floodplain_survival_rate = rearing_survival$floodplain[28:30, ],
                                  floodplain_growth = ..params$growth_rates_floodplain,
                                  weeks_flooded = ..params$weeks_flooded[28:30, month, year],
                                  stochastic = stochastic)
    
    juveniles[28:30, ] <- san_joaquin_trib_rear$inchannel + san_joaquin_trib_rear$floodplain
    
    san_joaquin_fish <- route_regional(month = month,
                                       year = year,
                                       migrants = san_joaquin_fish + san_joaquin_trib_fish$migrants,
                                       inchannel_habitat = habitat$inchannel[31],
                                       floodplain_habitat = habitat$floodplain[31],
                                       prop_pulse_flows = ..params$prop_pulse_flows[31, , drop = FALSE],
                                       migration_survival_rate = migratory_survival$san_joaquin,
                                       territory_size = ..params$territory_size,
                                       temperature_downstream = ..params$avg_temp[31, month, year],
                                       density_dependent_survival = ..params$density_dependent_survival,
                                       stochastic = stochastic)
    
    migrants[28:30, ] <- san_joaquin_fish$migrants
    
    san_joaquin_fish <- rear(juveniles = san_joaquin_fish$inchannel,
                             survival_rate = rearing_survival$inchannel[31, ],
                             growth = ..params$growth_rates,
                             floodplain_juveniles = san_joaquin_fish$floodplain,
                             floodplain_survival_rate = rearing_survival$floodplain[31, ],
                             floodplain_growth = ..params$growth_rates_floodplain,
                             weeks_flooded = rep(..params$weeks_flooded[31, month, year], nrow(san_joaquin_fish$inchannel)),
                             stochastic = stochastic)
    
    san_joaquin_fish <- san_joaquin_fish$inchannel + san_joaquin_fish$floodplain
    
    delta_fish <- route_and_rear_deltas(year = year, month = month,
                                        migrants = round(migrants),
                                        north_delta_fish = north_delta_fish,
                                        south_delta_fish = south_delta_fish,
                                        north_delta_habitat = habitat$north_delta,
                                        south_delta_habitat = habitat$south_delta,
                                        freeport_flows = ..params$freeport_flows,
                                        cc_gates_days_closed = ..params$cc_gates_days_closed,
                                        rearing_survival_delta = rearing_survival$delta,
                                        migratory_survival_delta = migratory_survival$delta,
                                        migratory_survival_bay_delta = migratory_survival$bay_delta,
                                        juveniles_at_chipps = juveniles_at_chipps,
                                        growth_rates = ..params$growth_rates,
                                        territory_size = ..params$territory_size,
                                        stochastic = stochastic)
    
    migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
    north_delta_fish <- delta_fish$north_delta_fish
    south_delta_fish <- delta_fish$south_delta_fish
    juveniles_at_chipps <- delta_fish$juveniles_at_chipps
  }
  
  adults_in_ocean <- adults_in_ocean + ocean_entry_success(migrants = migrants_at_golden_gate,
                                                           month = month,
                                                           avg_ocean_transition_month = avg_ocean_transition_month,
                                                           .ocean_entry_success_length = ..params$.ocean_entry_success_length,
                                                           ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                                           .ocean_entry_success_months = ..params$.ocean_entry_success_months,
                                                           stochastic = stochastic)
  return(list(juveniles = juveniles,
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
  )
  
}