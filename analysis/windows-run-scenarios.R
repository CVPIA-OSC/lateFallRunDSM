library(DSMscenario)
library(parallel)
library(doParallel)
library(purrr)
library(lateFallRunDSM)
library(dplyr)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)


run_scenario <- function(scenario) {
  s <- late_fall_run_model(mode = 'seed', stochastic = TRUE)
  output <- late_fall_run_model(scenario = scenario, mode = 'simulate',
                           stochastic = TRUE, seed = s)

  nat_spawn <- output$spawners * output$proportion_natural
  juv_biomass <- output$juvenile_biomass
  viability <- output$viability_metrics

  return(list(
    nat_spawn = nat_spawn,
    juv_biomass = juv_biomass,
    viability = viability
  ))
}

clusterExport(cl, list('run_scenario', 'late_fall_run_model'))

run_scenarios_parallel <- function(scenario, number_of_runs = 5000) {
  parLapply(cl, 1:number_of_runs,
           fun = function(i) {
             run_scenario(scenario = scenario)
           })
}

number_of_runs <- 5

baseline_results <- run_scenarios_parallel(DSMscenario::scenarios$NO_ACTION, number_of_runs)
scenario_1_results <- run_scenarios_parallel(DSMscenario::scenarios$ONE, number_of_runs)
scenario_2_results <- run_scenarios_parallel(DSMscenario::scenarios$TWO, number_of_runs)
scenario_3_results <- run_scenarios_parallel(DSMscenario::scenarios$THREE, number_of_runs)
scenario_4_results <- run_scenarios_parallel(DSMscenario::scenarios$FOUR, number_of_runs)
scenario_5_results <- run_scenarios_parallel(DSMscenario::scenarios$FIVE, number_of_runs)
scenario_6_results <- run_scenarios_parallel(DSMscenario::scenarios$SIX, number_of_runs)
scenario_7_results <- run_scenarios_parallel(DSMscenario::scenarios$SEVEN, number_of_runs)
scenario_8_results <- run_scenarios_parallel(DSMscenario::scenarios$EIGHT, number_of_runs)
scenario_9_results <- run_scenarios_parallel(DSMscenario::scenarios$NINE, number_of_runs)
scenario_10_results <- run_scenarios_parallel(DSMscenario::scenarios$TEN, number_of_runs)
scenario_11_results <- run_scenarios_parallel(DSMscenario::scenarios$ELEVEN, number_of_runs)
scenario_12_results <- run_scenarios_parallel(DSMscenario::scenarios$TWELVE, number_of_runs)
scenario_13_results <- run_scenarios_parallel(DSMscenario::scenarios$THIRTEEN, number_of_runs)


baseline_nat_spawn <- transpose(baseline_results)$nat_spawn
s0_mean_valley_wide_nat_spawn <- map_dbl(1:number_of_runs, ~mean(colSums(baseline_nat_spawn[[.]])))

s1_nat_spawn <- transpose(scenario_1_results)$nat_spawn
s1_mean_valley_wide_nat_spawn <- map_dbl(1:number_of_runs, ~mean(colSums(s1_nat_spawn[[.]])))

plot(1:number_of_runs, cummean(s0_mean_valley_wide_nat_spawn))
plot(1:number_of_runs, cummean(s1_mean_valley_wide_nat_spawn))

(mean(s1_mean_valley_wide_nat_spawn) - mean(s0_mean_valley_wide_nat_spawn)) / mean(s0_mean_valley_wide_nat_spawn)

# close all cluster connections
closeAllConnections()
