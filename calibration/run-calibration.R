library(lateFallRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(lateFallRunDSM::params)

current_best_solution <- read_rds("calibration/calibration-results.rds")

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -late_fall_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$late_fall,
              seeds = DSMCalibrationData::grandtab_imputed$late_fall,
              params = params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13]
            ),
          lower = c(2.5, -3.5, 0, rep(-3.5, 10)),
          upper = c(3.5,  3.5, 1, rep(3.5, 10)),
          popSize = 150,
          maxiter = 10000,
          run = 50,
          parallel = TRUE,
          pmutation = .4)

readr::write_rds(res, paste0("calibration/fits/result-", Sys.Date(), ".rds"))

# Evaluate Results ------------------------------------
r1_solution <- res@solution[1, ]
r1_solution <- x
keep <- c(1, 3, 7)

get_solution_data <- function(solution, watershed_to_keep) {
  r1_params <- update_params(x = solution, lateFallRunDSM::params)
  r1_params <- DSMCalibrationData::set_synth_years(r1_params)
  
  r1_sim <- late_fall_run_model(seeds = DSMCalibrationData::grandtab_imputed$late_fall, 
                                mode = "calibrate",
                                ..params = r1_params,
                                stochastic = FALSE)
  
  r1_nat_spawners <- as_tibble(r1_sim[watershed_to_keep, ,drop = F]) %>%
    mutate(watershed = DSMscenario::watershed_labels[watershed_to_keep]) %>%
    gather(year, spawners, -watershed) %>%
    mutate(type = "simulated",
           year = readr::parse_number(year) + 5)
  
  
  r1_observed <- as_tibble((1 - lateFallRunDSM::params$proportion_hatchery[watershed_to_keep]) * 
                             DSMCalibrationData::grandtab_observed$late_fall[watershed_to_keep,, drop=F]) %>%
    mutate(watershed = DSMscenario::watershed_labels[watershed_to_keep]) %>%
    gather(year, spawners, -watershed) %>%
    mutate(type = "observed", year = as.numeric(year) - 1997) %>%
    filter(!is.na(spawners),
           year > 5)
  
  r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)
  
  return(r1_eval_df)
}

data1 <- get_solution_data(r1_solution, keep)

data1 %>%
  ggplot(aes(year, spawners, color = type)) + 
  geom_line() + 
  geom_point() +  
  facet_wrap(~watershed, scales = "free_y") 

r <- data1 %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

data1 %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 10000) +
  ylim(0, 10000) +
  labs(title = paste("Late-fall Run: r =", round(r, 2))) +
  theme(text = element_text(size=14))

ggsave('calibration/late-fall_run_r.jpg', height = 6, width = 10, units='in')

data1 %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )
