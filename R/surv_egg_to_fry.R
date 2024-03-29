#' @title Egg to Fry Survival
#' @description Calculates the survival of transitioning from egg to fry
#' @details See \code{\link{params}} for details on parameter sources
#' @param proportion_natural Variable describing the proportion of natural-origin spawners
#' @param scour Variable describing the probability of redd scouring event
#' @param temperature_effect Variable describing the effect of inchannel temperature on egg survival.
#' Fall and spring estimated by C. Hammersmark (CBEC Ecoengineering Inc.). Winter-run value was calibrated.
#' @param .surv_egg_to_fry_int Intercept
#' @param .proportion_natural Coefficient for \code{proportion_natural} variable
#' @param ..surv_egg_to_fry_mean_egg_temp_effect TODO
#' @param .scour Coefficient for \code{scour} variable
#' @source IP-117068
#' @export
surv_egg_to_fry <- function(proportion_natural,
                            scour,
                            .surv_egg_to_fry_int = lateFallRunDSM::params$.surv_egg_to_fry_int,
                            ..surv_egg_to_fry_mean_egg_temp_effect = lateFallRunDSM::params$..surv_egg_to_fry_mean_egg_temp_effect,
                            .proportion_natural = lateFallRunDSM::params$.surv_egg_to_fry_proportion_natural,
                            .scour = lateFallRunDSM::params$.surv_egg_to_fry_scour){
  
  boot::inv.logit(.surv_egg_to_fry_int + .proportion_natural * proportion_natural +
                    .scour * scour) * ..surv_egg_to_fry_mean_egg_temp_effect
}