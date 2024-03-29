#' @title Get Spawning Adults
#' @details See \code{\link{params}} for details on parameter sources
#' @param year the year of simulation
#' @param adults potential spawning adults for each watershed (length = 31) values must be integer
#' @param hatch_adults total hatchery adults
#' @param mode a value meant to be inherited to determine if model is in "seed", "calibrate", or "simulate" mode
#' @param month_return_proportions The proportion of fish returning for each month
#' @param prop_flow_natal More details at \code{\link[DSMflow]{proportion_flow_natal}}
#' @param south_delta_routed_watersheds More details at \code{\link[DSMhabitat]{south_delta_routed_watersheds}}
#' @param cc_gates_days_closed More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#' @param gates_overtopped More details at \code{\link[DSMflow]{gates_overtopped}}
#' @param tisdale_bypass_watershed More details at \code{\link[DSMhabitat]{tisdale_bypass_watershed}}
#' @param yolo_bypass_watershed More details at \code{\link[DSMhabitat]{yolo_bypass_watershed}}
#' @param migratory_temperature_proportion_over_20 More details at \code{\link[DSMtemperature]{migratory_temperature_proportion_over_20}}
#' @param natural_adult_removal_rate More details at \code{\link{natural_adult_removal_rate}}
#' @param cross_channel_stray_rate More details at \code{\link{cross_channel_stray_rate}}
#' @param stray_rate More details at \code{\link{stray_rate}}
#' @param ..surv_adult_enroute_int Intercept for \code{\link{surv_adult_enroute}}
#' @param .adult_stray_intercept Intercept for \code{\link{adult_stray}}
#' @param .adult_stray_wild Coefficient for \code{\link{adult_stray}} \code{wild} variable
#' @param .adult_stray_natal_flow  Coefficient for \code{\link{adult_stray}} \code{natal_flow} variable
#' @param .adult_stray_cross_channel_gates_closed  Coefficient for \code{\link{adult_stray}} \code{cross_channel_gates_closed} variable
#' @param .adult_stray_prop_bay_trans  Coefficient for \code{\link{adult_stray}} \code{prop_bay_trans} variable
#' @param .adult_stray_prop_delta_trans  Coefficient for \code{\link{adult_stray}} \code{prop_delta_trans} variable
#' @param .adult_en_route_migratory_temp Coefficient for \code{\link{surv_adult_enroute}} \code{migratory_temp} variable
#' @param .adult_en_route_bypass_overtopped Coefficient for \code{\link{surv_adult_enroute}} \code{bypass_overtopped} variable
#' @param .adult_en_route_adult_harvest_rate  Adult harvest rate for \code{\link{surv_adult_enroute}}
#' @param stochastic TRUE FALSE value indicating if model is being run stochastically
#' @source IP-117068
#' @export
get_spawning_adults <- function(year, adults, hatch_adults, mode,
                                month_return_proportions,
                                prop_flow_natal,
                                south_delta_routed_watersheds,
                                cc_gates_days_closed,
                                gates_overtopped,
                                tisdale_bypass_watershed,
                                yolo_bypass_watershed,
                                migratory_temperature_proportion_over_20,
                                natural_adult_removal_rate,
                                cross_channel_stray_rate,
                                stray_rate,
                                ..surv_adult_enroute_int,
                                .adult_stray_intercept,
                                .adult_stray_wild,
                                .adult_stray_natal_flow,
                                .adult_stray_cross_channel_gates_closed,
                                .adult_stray_prop_bay_trans,
                                .adult_stray_prop_delta_trans,
                                .adult_en_route_migratory_temp,
                                .adult_en_route_bypass_overtopped,
                                .adult_en_route_adult_harvest_rate,
                                stochastic) {
  
  # during the seeding stage just reuse the seed adults as the input, and apply no
  # en-route survival
  if (mode %in% c("seed", "calibrate")) {
    adult_index <- ifelse(mode == "seed", 1, year)
    adults_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, adults[watershed, adult_index], month_return_proportions["Battle and Clear Creeks",])
      } else {
        round(adults[watershed, adult_index] * month_return_proportions["Battle and Clear Creeks",])
      }
    }))
    
    adults_by_month[1,] <- if (stochastic) {
      rmultinom(1, adults[1, adult_index], month_return_proportions["Upper Sacramento River",])
    } else {
      round(adults[1, adult_index] * month_return_proportions["Upper Sacramento River",])
    }
    
    adults_by_month_hatchery_removed <- sapply(1:5, function(month) {
      if (stochastic) {
        rbinom(n = 31,
               size = round(adults_by_month[, month]),
               prob = 1 - natural_adult_removal_rate)
      } else {
        round(adults_by_month[, month] * (1 - natural_adult_removal_rate))
      }
    })
    
    init_adults <- rowSums(adults_by_month_hatchery_removed)
    proportion_natural <- 1 - lateFallRunDSM::params$proportion_hatchery
    init_adults_by_month <- adults_by_month_hatchery_removed
    
  } else  {
    
    adults_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, adults[watershed, year], month_return_proportions["Battle and Clear Creeks",])
      } else {
        round(adults[watershed, year] * month_return_proportions["Battle and Clear Creeks",])
      }
    }))
    
    adults_by_month[1,] <- if (stochastic) {
      rmultinom(1, adults[1, year], month_return_proportions["Upper Sacramento River",])
    } else {
      round(adults[1, year] * month_return_proportions["Upper Sacramento River",])
    }
    
    hatchery_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, hatch_adults[watershed], month_return_proportions["Battle and Clear Creeks",])
      } else {
        round(hatch_adults[watershed] * month_return_proportions["Battle and Clear Creeks",])
      }
    }))
    
    hatchery_by_month[1,] <- if (stochastic) {
      rmultinom(1, hatch_adults[1], month_return_proportions["Upper Sacramento River",])
    } else {
      round(hatch_adults[1] * month_return_proportions["Upper Sacramento River",])
    }
    
    stray_props <- sapply(c(10:12,1,2), function(month) {
      adult_stray(wild = 1,
                  natal_flow = prop_flow_natal[ , year + (month < 3)],
                  south_delta_watershed = south_delta_routed_watersheds,
                  cross_channel_gates_closed = cc_gates_days_closed[month],
                  .intercept = .adult_stray_intercept,
                  .wild = .adult_stray_wild,
                  .natal_flow = .adult_stray_natal_flow,
                  .cross_channel_gates_closed = .adult_stray_cross_channel_gates_closed,
                  .prop_bay_trans = .adult_stray_prop_bay_trans,
                  .prop_delta_trans = .adult_stray_prop_delta_trans)
    })
    
    straying_adults <- sapply(1:5, function(month) {
      if (stochastic) {
        rbinom(n = 31, adults_by_month[, month], stray_props[, month])
      } else {
        round(adults_by_month[, month] * stray_props[, month])
      }
    })
    
    south_delta_routed_adults <- round(colSums(straying_adults * south_delta_routed_watersheds))
    south_delta_stray_adults <- sapply(1:5, function(month) {
      if (stochastic) {
        as.vector(rmultinom(1, south_delta_routed_adults[month], cross_channel_stray_rate))
      } else {
        as.vector(round(south_delta_routed_adults[month] * cross_channel_stray_rate))
      }
    })
    
    remaining_stray_adults <- round(colSums(straying_adults * (1 - south_delta_routed_watersheds)))
    stray_adults <- sapply(1:5, function(month) {
      if (stochastic) {
        as.vector(rmultinom(1, remaining_stray_adults[month], stray_rate))
      } else {
        as.vector(round(remaining_stray_adults[month] * stray_rate))
      }
    })
    
    adults_after_stray <- adults_by_month - straying_adults + south_delta_stray_adults + stray_adults
    
    bypass_is_overtopped <- sapply(c(10:12,1,2), function(month) {
      
      tis <- gates_overtopped[month, year + (month < 3), "Sutter Bypass"] * tisdale_bypass_watershed
      yolo <- gates_overtopped[month, year + (month < 3), "Yolo Bypass"] * yolo_bypass_watershed
      as.logical(tis + yolo)
    })
    
    en_route_temps <- migratory_temperature_proportion_over_20[, c(10:12,1,2)]
    
    
    adult_en_route_surv <- sapply(1:5, function(month) {
      
      adult_en_route_surv <- surv_adult_enroute(migratory_temp = en_route_temps[,month],
                                                bypass_overtopped = bypass_is_overtopped[,month],
                                                adult_harvest = .adult_en_route_adult_harvest_rate,
                                                ..surv_adult_enroute_int = ..surv_adult_enroute_int,
                                                .migratory_temp = .adult_en_route_migratory_temp,
                                                .bypass_overtopped = .adult_en_route_bypass_overtopped)
    })
    
    
    adults_survived_to_spawning <- sapply(1:5, function(month) {
      if (stochastic) {
        rbinom(31, round(adults_after_stray[, month]), adult_en_route_surv[, month])
      } else {
        round(adults_after_stray[, month] * adult_en_route_surv[, month])
      }
    })
    
    surviving_natural_adults_by_month <- sapply(1:5, function(month) {
      if (stochastic) {
        rbinom(31, round(adults_survived_to_spawning[, month]), (1 - natural_adult_removal_rate))
      } else {
        round(adults_survived_to_spawning[, month] * (1 - natural_adult_removal_rate))
      }
    })
    
    surviving_hatchery_adults_by_month <- sapply(1:5, function(month) {
      if (stochastic) {
        rbinom(31, round(hatchery_by_month[, month]), adult_en_route_surv[, month])
      } else {
        round(hatchery_by_month[, month] * adult_en_route_surv[, month])
      }
    })
    
    surviving_natural_adults <- rowSums(surviving_natural_adults_by_month)
    surviving_hatchery_adults <- rowSums(surviving_hatchery_adults_by_month)
    init_adults <- surviving_natural_adults + surviving_hatchery_adults
    init_adults_by_month <- surviving_natural_adults_by_month + surviving_hatchery_adults_by_month
    proportion_natural <- surviving_natural_adults / init_adults
    
  }
  
  
  list(init_adults = init_adults,
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0),
       init_adults_by_month = init_adults_by_month)
  
}
