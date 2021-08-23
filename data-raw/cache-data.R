library(tidyverse)
# Watershed Labels 
watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                      "Feather River", "Yuba River", "Lower-mid Sacramento River",
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                      "Tuolumne River", "San Joaquin River")
usethis::use_data(watershed_labels, overwrite = TRUE)

# Adult seeds
adult_seeds <- matrix(0, nrow = 31, ncol = 30)
no_lfr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$lfr *
                             DSMhabitat::watershed_species_present[1:31,]$spawn)

adult_seed_values <- DSMCalibrationData::mean_escapement_2013_2017 %>%
  bind_cols(no_lfr_spawn = no_lfr_spawn) %>%
  select(watershed, `Late-Fall`, no_lfr_spawn) %>%
  mutate(corrected_late_fall = case_when(
    no_lfr_spawn ~ 0,
    is.na(`Late-Fall`) | `Late-Fall` < 10 ~ 12,
    TRUE ~ `Late-Fall`)
  ) %>% pull(corrected_late_fall)

adult_seeds[ , 1] <- adult_seed_values

rownames(adult_seeds) <- watershed_labels
usethis::use_data(adult_seeds, overwrite = TRUE)

# Proportion Hatchery 
proportion_hatchery <- c(0.075, 0, 0.027, rep(0, 28)) #proportion hatchery based on CWT reports
names(proportion_hatchery) <- watershed_labels
usethis::use_data(proportion_hatchery, overwrite = TRUE)

# Month Return Proportions 
month_return_proportions <- matrix(c(0.1,0.2,0.4,0.2,0.1,
                                     0,0.1,0.4,0.4,0.1), byrow=T,nrow=2, 
                                   dimnames = list(c("Upper Sacramento River", "Battle and Clear Creeks"), 
                                                   c("Oct","Nov", "Dec", "Jan", "Feb")))
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)

cross_channel_stray_rate <- c(rep(1, 15), 0, 0, 2, 2, 2, 0, 0, 3, 0, rep(0, 7)) / 24
names(cross_channel_stray_rate) <- watershed_labels
usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)

stray_rate <- c(rep(1, 15), 0, 0, 1, 1, 1, 0, 0, 1, 0, rep(1, 6), 0) / 25
names(stray_rate) <- watershed_labels
usethis::use_data(stray_rate, overwrite = TRUE)

# Adult Harvest Rate
adult_harvest_rate <- c(0.0676, 0, 0.0676, 0, 0, 0, 0.0676, rep(0, 24))
names(adult_harvest_rate) <- watershed_labels
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

# Natural Adult Removal Rate
natural_adult_removal_rate <- c(0.014, rep(0, 30))
names(natural_adult_removal_rate) <- watershed_labels
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)

# Hatchery Allocation
hatchery_allocation <- c(0.083, 0, 0.015, rep(0, 28)) # hatchery allocation based on CWT reports
names(hatchery_allocation) <- watershed_labels
usethis::use_data(hatchery_allocation, overwrite = TRUE)

# Diversity Groups
original_groups <- read_csv("data-raw/misc/Grouping.csv")
diversity_group <- original_groups$diversity_group
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)

# Size class labels 
size_class_labels <- c('s', 'm', 'l', 'vl')
usethis::use_data(size_class_labels, overwrite = TRUE)

# calculate growth rates
growth_rates_inchannel <- growth()
usethis::use_data(growth_rates_inchannel, overwrite = TRUE)
growth_rates_floodplain <- growth_floodplain()
usethis::use_data(growth_rates_floodplain, overwrite = TRUE)













