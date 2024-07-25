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

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

output_data_path <- file.path(dt_path, "epi")
if(!file.exists(output_data_path)) {dir.create(output_data_path, recursive = T)}

if(!file.exists(file.path(dt_path, "error"))) {dir.create(file.path(dt_path, "error"), recursive = T)}

if(!file.exists(file.path("data", "issue_12"))) {dir.create(file.path("data", "issue_12"))}
if(!file.exists(file.path("data", "issue_16"))) {dir.create(file.path("data", "issue_16"))}
if(!file.exists(file.path("data", "issue_17"))) {dir.create(file.path("data", "issue_17"))}

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

# fn: a) reads rda data file, or b) sas file from KP if it is not available & saves it as rda
read_large_file <- function(data_path, override_existing_file=F){
  if(file.exists(data_path) & override_existing_file==FALSE) {
    result <- readRDS(data_path)
  } else {
    result <- haven::read_sas(gsub(".rda", ".sas7bdat", data_path) , NULL)
    saveRDS(result, data_path)
  }
  return(result)
}

######################################################################
# LOAD DATA
######################################################################
message("loading data")
# outcome & covariate data
#health_dt_path <-file.path("data", "issue_12", "issue_012_rerun_for_release20231010", "issue_012.rda")

#use this newer file. dimensions are the same, but some of the exposure-related columns differ
health_dt_path <-file.path("data", "issue_12", "issue_012_rerun_for_release20231108", "issue_012.rda")

health0 <- read_large_file(health_dt_path)

exclusion_table <- count_remaining_sample(health0, description. = "Full dataset")


 
# exposure predictions from different datasets & models
exposure_dt_path_combined <- file.path("data", "issue_17", "issue_017_final_20240712.rda")
if(file.exists(exposure_dt_path_combined)){
  exposure0 <- readRDS(exposure_dt_path_combined)
  } else{
    ## prior predictions (drop all all-road models & stationary fewer hour desings + old season designs)
    exposure0.0 <- read_large_file(file.path("data", "issue_17", "issue_017_rerun20231020.rda")) %>%
      filter(!grepl("^r_", model),
             !grepl("_rh_|_bh_|_s1_|_s2_|_s3_|_s4_", model))
    
    ## part 1 (half the cohort; new temp adj stationary & all new on-road models)
    exposure0.1 <- read_large_file(file.path("data", "issue_17", "issue_017_final_part1_20240712.rda"))
    
    ## part 2 (other half the cohort; new temp adj stationary & all new on-road models)
    exposure0.2 <- read_large_file(file.path("data", "issue_17", "issue_017_final_part2_20240712.rda"))
    
    exposure0 <- rbind(exposure0.1, exposure0.2) %>%
      rbind(exposure0.0)
    saveRDS(exposure0, exposure_dt_path_combined)
    
    rm(exposure0.0, exposure0.1, exposure0.2)
  }


# measurement error dataset - bootstrapped site/visit samples used to develop exposure prediction models
me_dt_path <- file.path("data", "issue_16", "issue_16_rerun", "issue_016_20230929.rda")

if(file.exists(me_dt_path)) {
  me_exposure <- readRDS(me_dt_path)
} else {
  me_exposure <- haven::read_sas(gsub(".rda", ".sas7bdat", me_dt_path), NULL) %>%
    #drop exp coverage, exact_coverage, imp_coverage, imp quality
    select(study_id, avg_0_5_yr, model)
  saveRDS(me_exposure, me_dt_path)
}

########################################################
message("selecting exposure models")
# see "ACT and HEI Data Documentation for UW" doc for all model crosswalks
# ID model names for LCS, ML, onroad ("^r")
# other_model_names <- str_subset(unique(exposure0$model), "^s_|^r_", negate = T)
# other_model_names

# si's ML UFP models
ml_models <- c("upls", "uspatpl", "uspatcv", "urf", "utprs", "urt", "utr")
# low-cost sensor/monitor (LCM) models

# --> UPDATE??

# #non- MM models
# grep("r_|s_", unique(exposure0$model), invert = T, value = T)
lcm_models <- str_subset(unique(exposure0$model), "^pm25_fptv_|^pm25_fp_|^pm25_rp_|^pm25_fp_|^no2")

########################################################
# other models we'll eventually use 
## Road models
exposure0_r <- filter(exposure0, grepl("^r_", model))
## Si's ML models
exposure0_ml <- filter(exposure0, grepl(paste(ml_models, collapse = "|"), model))
## LCM
exposure0_lcm <- filter(exposure0, grepl(paste(lcm_models, collapse = "|"), model))
#main models use the stop data
exposure0 <- filter(exposure0, grepl("^s_", model))

######################################################################
# PREP DATASET
######################################################################
message("cleaning health data")
# baseline data 
health <- filter(health0, VISIT==0)
exclusion_table <- count_remaining_sample(health, description. = "Baseline data")

######################################################################
exposure0_lcm_ref <- health %>% 
  select(study_id, 
         full_pm25_ST = cum_exp_pm25_ST_05_yr, 
         full_pm25_SP = cum_exp_pm25_SP_05_yr,
         # note that all of these QC indicator variabels are for ST (the true ref model), not SP
         exp_coverage= avg_wc_pm25_ST_05_yr,
         exact_coverage=avg_ec_pm25_ST_05_yr,
         imp_coverage=avg_ic_pm25_ST_05_yr, 
         imputation_quality=avg_iq_pm25_ST_05_yr) %>%
  pivot_longer(cols = c(full_pm25_ST, full_pm25_SP), names_to = "model", values_to = "avg_0_5_yr")

######################################################################
health <- health %>%
  mutate(year = year(visitdt)) 

# valid casi score. everybody has a CASI score at baseline 
health <- filter(health, casi_valid==1)
exclusion_table <- count_remaining_sample(health, description. = "Valid CASI score")

######################################################################
# check that exposure coverage variables are similar across datasets. # looks good
# select(health,
#        study_id,
#        health_avg_wc_ufp_10_42_MM_05_yr = avg_wc_ufp_10_42_MM_05_yr,
#        health_avg_wc_no2_MM_05_yr = avg_wc_no2_MM_05_yr) %>%
#  left_join(distinct(exposure0, study_id, exp_coverage)) %>%
#  mutate(diff_ufp = health_avg_wc_ufp_10_42_MM_05_yr - exp_coverage,
#         diff_no2 = health_avg_wc_no2_MM_05_yr - exp_coverage) %>%
#  summary()

# High exposure coverage

# this is avg_wc_ in health (issue 12))
# only ~4% of predictions have threshold < 0.95, so OK to leave this as is?
coverage_threshold <- 0.95

# # check that definition of avg_wc_ isn't dropping early years by definition. # looks fine?
# health %>%
#   group_by(year) %>%
#   summarize(mean = mean(avg_wc_bc_MM_05_yr, na.rm = T))

# high exposure coverage (based on statinary measures, which should/are be the same as road & ML for stationary models)
good_exposure_ids <- filter(exposure0, exp_coverage >= coverage_threshold) %>%
  distinct(study_id) %>% pull()
health <- filter(health, study_id %in% good_exposure_ids)

exclusion_table <- count_remaining_sample(health, description. = "High exposure coverage")


model_covars <- c("visit_age_centered75", "year2", "male", "degree"#, 
                  #"apoe"#, dropping this requirement b/c drops ~ 16% of people (post 2018) w/o APOE genotyping. this is different from Nancy's work, but OK since this is not a confounder anyway so results should be similar
                  )
saveRDS(model_covars, file.path(output_data_path, "model_covars.rda"))

# for sensitivity analyses
model_covars_extended <- c(model_covars, "race_white" , "nses_z_cx")
saveRDS(model_covars_extended, file.path(output_data_path, "model_covars_extended.rda"))

health <- health %>%
  mutate(
    # 2 yr time bins most of the time other than during the first wave (1994-1995) - following Nancy's cross-sectional analysis approach
    year2 = floor(year/2)*2,
    year2 = ifelse(year==1995, 1995, year2),
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
  select(study_id, casi_irt, all_of(model_covars_extended), #, model_covars),
         #QC variables for NO2 and UFP
         ends_with(c("no2_MM_05_yr", "ufp_10_42_MM_05_yr")),
         -starts_with(c("cum_exp_", "var_avg_", "num_years_")), 
         #keep NS & P-TRAK exposure estimate from main epi model (for comparision against the all-data HEI model)
         cum_exp_ufp_10_42_MM_05_yr, cum_exp_ufp_20_1k_MM_05_yr, 
         visitdt)

######################################################################
# MISSINGNESS
######################################################################
# Proportion missing: APOE, 0.2% race, 2.2% NSES is missing [has this been updated?]
health %>%
  summarize_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "count") %>%
  mutate(prop_missing = count/nrow(health)) %>%
  #filter(prop_missing >0) %>%
  arrange(-prop_missing) %>%
  filter(count>0)

# # apoe available
# ## note: last visitdt becomes 2018-05-02 (vs 2020-03-05) when we filter by APOE availability
# health <- filter(health, !is.na(apoe))
# exclusion_table <- count_remaining_sample(health, description. = "Have APOE")

health <- drop_na(health, all_of(model_covars))
exclusion_table <- count_remaining_sample(health, description. = "all primary covariates available")

saveRDS(health, file.path("data", "issue_12", "issue_012_rerun_for_release20231108", "issue_012_clean.rda"))

# counts for sensitivity analyses
exclusion_table <- drop_na(health, all_of(model_covars_extended)) %>%
  count_remaining_sample(., description. = "all primary & secondary covariates available")


# 2010+ only
exclusion_table <- health %>%
  drop_na(all_of(model_covars_extended)) %>%
  filter(as.numeric(as.character(year2))>=2010) %>%
  count_remaining_sample(., description. = "2010+")

######################################################################
# COMBINE HEALTH AND EXPOSURE DATA
######################################################################
message("merging health & exposure data")
# main, stationary models
cs <- left_join(health, exposure0, by="study_id")
# road models
cs_r <- left_join(health, exposure0_r, by="study_id")
# ML models w/ stationary data
cs_ml <- left_join(health, exposure0_ml, by="study_id")
# LCM models
## include full ST (and SP) ref models
cs_lcm <- left_join(health, exposure0_lcm, by="study_id") %>%
  rbind(left_join(health, exposure0_lcm_ref, by="study_id"))
# data for HEI Aim 3b - non-parametric part?
cs_error <- left_join(health, me_exposure, by="study_id")

######################################################################
# QC VARIABLES
######################################################################
# # a quick look at these. no red flags
# cs %>%
#   select(exact_coverage_prop = avg_ec_ufp_10_42_MM_05_yr, 
#          imputed_address_prop = avg_ic_ufp_10_42_MM_05_yr, 
#          imputation_quality = avg_iq_ufp_10_42_MM_05_yr) %>%
#   summary()

######################################################################
# SAVE DATA
######################################################################
message("saving datasets for modeling")
write.csv(exclusion_table, file.path(output_data_path, paste0("exclusion_table", Sys.Date(), ".csv")), row.names = F)
saveRDS(cs, file.path(output_data_path, "dt_for_cross_sectional_analysis.rda"))
saveRDS(cs_r, file.path(output_data_path, "dt_for_cross_sectional_analysis_road.rda"))
saveRDS(cs_ml, file.path(output_data_path, "dt_for_cross_sectional_analysis_machine_learning.rda"))
saveRDS(cs_lcm, file.path(output_data_path, "dt_for_cross_sectional_analysis_lcm.rda"))
saveRDS(cs_error, file.path(output_data_path, "dt_for_cross_sectional_analysis_error.rda"))

message("Done with 7_clean_act_dt.R")

