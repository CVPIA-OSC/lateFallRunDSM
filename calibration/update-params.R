update_params <- function(x, params) {

  params$..surv_adult_enroute_int = x[1]
  params$..surv_adult_prespawn_int = x[2]
  params$..surv_egg_to_fry_mean_egg_temp_effect =  x[3]
  params$..surv_juv_rear_int = rep(x[4], 31)
  params$..surv_juv_rear_contact_points = x[5]
  params$..surv_juv_rear_prop_diversions = x[6]
  params$..surv_juv_rear_total_diversions = x[7]
  params$..surv_juv_bypass_int = x[8]
  params$..surv_juv_delta_int = x[9]
  params$..surv_juv_delta_contact_points = x[10]
  params$..surv_juv_delta_total_diverted = x[11]
  params$..surv_juv_outmigration_sj_int = x[12]
  params$..ocean_entry_success_int = rep(x[13], 31)
  
  return(params)

}
