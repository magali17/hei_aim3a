# Author: Magali Blanco
# Date: 1/19/2023
# Purpose: to clean and prepare data for a cross-sectional UFP-CASI-IRT analysis

######################################################################
# SETUP
######################################################################
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, kableExtra)    

set.seed(1)

#source("functions.R")
output_data_path <- file.path("Output", "epi")
if(!file.exists(output_data_path)) {dir.create(output_data_path)}

#image_path <- file.path("..", "manuscript", "images")
# --> keep?

print_diagnostics <- FALSE

######################################################################
# FUNCTIONS
######################################################################


# exclusion counts
exclusion_table <- tibble(description = as.character(),  
                          persons = as.numeric(),
                          persons_dropped = as.numeric(),
                          person_years = as.numeric(),
                          person_years_dropped = as.numeric(),
                          notes = as.character(),  
)

# fn adds the number of unique persons and person-years for any given dataset to the exclusion tibble

count_remaining_sample <- function(dt, description., notes.=NA) {
  #dt <- distinct(dt, study_id, VISIT)
  
  exclusion_table <- add_row(exclusion_table,
                             description = description.,
                             persons = length(unique(dt$study_id)),
                             person_years = nrow(dt),
                             notes = notes.)
  
  exclusion_table <- mutate(exclusion_table,
                            persons_dropped = lag(persons) - persons,
                            person_years_dropped = lag(person_years) - person_years,
                            
                            persons_dropped_prop = round(persons_dropped/lag(persons), 2),
                            person_years_dropped_prop = round(person_years_dropped/lag(person_years), 2)
                            
  )
  
  return(exclusion_table)
}
######################################################################
# LOAD DATA
######################################################################
#health_dt_path <- file.path("..", "..", "issue_12", "issue_012_degapped_0113.rda")
#health_dt_path <- file.path("..", "..", "..", "", "ACT AP", "Manuscripts", "Longitudinal", "Data", "issue_12", "issue_012_degapped_0113.rda")

# outcome & covariate data
health_dt_path <-file.path("data", "issue_12", "issue_012_degapped_0113.rda")

if(file.exists(health_dt_path)) {health0 <- readRDS(health_dt_path)} else {
    health0 <- haven::read_sas(gsub(".rda", ".sas7bdat", health_dt_path) , NULL)
    saveRDS(health0, health_dt_path)
}

exclusion_table <- count_remaining_sample(health0, description. = "Full dataset")

# exposure predictions from different models

# --> UPDATE THIS TO ISSUE 17

exposure_dt_path <- file.path("data", "issue_16", "issue_016_01132023.rda")
# file.exists(exposure_dt_path) 

if(file.exists(exposure_dt_path)) {
  exposure0 <- readRDS(exposure_dt_path)
  } else {
  load(file.path("data", "issue_16", "issue_016_01132023.Rdata")) #loads as issue16
  saveRDS(issue16, file.path(exposure_dt_path))
}

######################################################################
# COMMON VARIABLES
######################################################################
first_exposure_year <- 2000 #since exposure window is 5 yr (going back to 1995)
save(first_exposure_year, file = file.path(output_data_path, "first_exposure_year.rda"))

######################################################################
# PREP DATASET
######################################################################
# baseline data 
health <- filter(health0, VISIT==0)
exclusion_table <- count_remaining_sample(health, description. = "Baseline data")

# --> ONLY KEEP 2005+?
health <- health %>%
  mutate(year = year(visitdt)) %>%
  filter(year >= first_exposure_year)
exclusion_table <- count_remaining_sample(health, description. = "2005+")

# valid casi score. everybody has a CASI score at baseline 
health <- filter(health, casi_valid==1)
exclusion_table <- count_remaining_sample(health, description. = "Valid CASI score")


# High exposure coverage
coverage_threshold <- 0.95

# --> avg_wc_ should also match health$exp_coverage...but it doesn't??

# high exposure coverage. all bl exp_coverage values are the same for each participant. Only keep the ones w/ good coverage (i.e., in MM area)
good_exposure_ids <- filter(exposure0, exp_coverage >= coverage_threshold) %>%
  distinct(study_id) %>% pull()
health <- filter(health, study_id %in% good_exposure_ids)
exclusion_table <- count_remaining_sample(health, description. = "High exposure coverage")

# --> nses_z_cx will change to NDI
model_covars <- c("visit_age_centered75", "year2", "apoe", "male", "degree", "race_white", "nses_z_cx")
saveRDS(model_covars, file.path(output_data_path, "model_covars.rda"))

health <- health %>%
  mutate(
    # 2 yr time bins
    year2 = floor(year/2)*2,
    year2 = factor(year2),
    race_white = ifelse(race == 1, 1, 0),
    # --> should 9 be a 6 instead? but would change Rachel's models
    # make unknown degree=9 "none" 
    degree = ifelse(degree == 9, 0, degree),
    # combine GED and HS 
    degree = ifelse(degree %in% c(1:2), 1, degree),
    degree = factor(degree),
    visit_age_centered75 = visit_age - 75
    ) %>%
  select(study_id, casi_irt, all_of(model_covars),
         #QC variables for NO2 and UFP
         ends_with(c("no2_MM_05_yr", "ufp_10_42_MM_05_yr")),
         -starts_with(c("cum_exp_", "var_avg_", "num_years_")),
         
         visitdt
    )



######################################################################
# MISSINGNESS
######################################################################
# Proportion missing: 31% APOE, .3% race, 5% NSES is missing 
health %>%
  summarize_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "count") %>%
  mutate(prop_missing = count/nrow(health)) %>%
  #filter(prop_missing >0) %>%
  arrange(-prop_missing) 


# --> ? impute missingness?


 

######################################################################
# ?? IPW FOR APOE MISSINGNESS
######################################################################









# apoe available
health <- filter(health, !is.na(apoe))
exclusion_table <- count_remaining_sample(health, description. = "Have APOE")

# --> delete? 
health <- drop_na(health, all_of(model_covars))
exclusion_table <- count_remaining_sample(health, description. = "all covariates available")

######################################################################
# COMBINE HEALTH AND EXPOSURE DATA
######################################################################

# combine 
cs <- left_join(health, exposure0, by="study_id")

# #check that each study ID has same # of models.
# cs%>% group_by(study_id) %>% summarize(models = n()) %>% distinct(models) %>% length() == 1


######################################################################
# QC VARIABLES
######################################################################

# --> WHY not the same?? Amanda looking into this?

# these should be very similar/identical. they are off by as much as 11%
summary(cs$exp_coverage-cs$avg_wc_ufp_10_42_MM_05_yr)

######################################################################
# SAVE DATA
######################################################################
write.csv(exclusion_table, file.path(output_data_path, "exclusion_table.csv"), row.names = F)
saveRDS(cs, file.path(output_data_path, "dt_for_cross_sectional_analysis.rda"))

