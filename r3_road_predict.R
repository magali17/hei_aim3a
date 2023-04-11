# this script is almost identical to 5_prediction_program.R but slightly shorter for mobile data
# since QC, new covariate development are not needed

################################################################################
# SETUP
################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load the required libraries for: plotting, modeling, spatial features, script timing
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, pls, tools, parallel,
               gstat, sf,
               lubridate
               #ggpubr, #ggspatial, 
)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

set.seed(1)

################################################################################
# DATA
################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)
# user_arguments <- "onroad_modeling_data_SP_FALSE_ADJ_FALSE.rda" 
# onroad_modeling_data_SP_FALSE_ADJ_TRUE.rda
# onroad_modeling_data_SP_TRUE_ADJ_FALSE.rda
# onroad_modeling_data_SP_TRUE_ADJ_TRUE.rda

modeling_dt <- user_arguments[1]

message("loading data")

# new covariate file
# modeling_data <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data.rda"))
modeling_data <- readRDS(file.path(dt_path, "Selected Campaigns", modeling_dt))


dt <- readRDS(file.path("data", "dr0357_cohort_covar_20220404_in_mm_area_prepped.rda"))
#cov_ext <- tools::file_ext(covariate_file_path)

#where predictions should be saved
prediction_directory <- file.path(dt_path, "UK Predictions", "cohort", "onroad_pnc_noscreen")
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = T)}

###########################################################################################
# Universal Kriging - Partial Least Squares Model function
cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# desired PLS components to use (from a different script): 3
pls_comp_n <- read_rds(file.path(dt_path, "pls_comp_n.rda")) 

#prediction model
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
message("Generating predictions at new locations")

new_predictions0 <- mclapply(group_split(modeling_data, model, variable)[1:2],
  mc.cores = 1,# 4,
  function(x) {
    
    print("model: " ,paste(first(x$model_no2), first(x$model)))
    
    temp <- dt %>%
      mutate(model = first(x$model),
             variable = first(x$variable)) %>%
      uk_pls(new_data = ., modeling_data = x)
  }) %>%
  bind_rows()  

# save the location and prediction information
# new_predictions <- new_predictions0 %>%
#   select(location_id, latitude, longitude, model, variable, prediction) %>%
#   mutate(prediction = exp(prediction))
#    
# saveRDS(new_predictions, file.path(prediction_directory, "onroad_predictions.rda"))

p_name <- substr(modeling_dt, 21, nchar(modeling_dt)-4)
message("saving TEMPORARY predictions")
saveRDS(new_predictions0, file.path(prediction_directory, paste0("TEMP_onroad_predictions_", p_name, Sys.Date(),".rda")))

###########################################################################################
# CLEAN DATA FOR KP
###########################################################################################
new_predictions <- new_predictions0 %>%
  mutate(
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01"),
    end_date = ymd("2021-07-09")
    ) %>%
  select(location_id, start_date, end_date, model, variable, prediction) 

message("saving predictions")

saveRDS(new_predictions, file.path(prediction_directory, paste0("onroad_predictions_", Sys.Date(),".rda")))

new_predictions %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_directory, paste0("onroad_predictions_", p_name, Sys.Date(),".csv")))

###########################################################################################
# QC CHECKS
###########################################################################################
qc <- TRUE

#if(qc==TRUE) {stop}

# summary of predictions
if(qc==TRUE) {
  message("QC Summary")
  
  print("distribution of predictions. N = models x cohort locations")
  predictions %>%
    group_by(variable) %>%
    summarize(n = n(),
              min = min(prediction),
              mean = mean(prediction),
              max = max(prediction),
              NAs = sum(is.na(prediction))
    )}

###########################################################################################
# check that csv file saved correctly

qc <- FALSE

if(qc==TRUE) {
  predictions_csv2 <- read.csv(file.path(prediction_directory, paste0("onroad_predictions_", Sys.Date(),".csv")))
  predictions_rda2 <- readRDS(file.path(prediction_directory, paste0("onroad_predictions_", Sys.Date(),".rda"))) %>% 
    as.data.frame()
  
  # check that the files are exactly the same
  same_files <- all(predictions_csv2$prediction==predictions_rda2$prediction) &
    all(predictions_csv2$model==predictions_rda2$model) &
    all(predictions_csv2$model==predictions_rda2$model)
  
  if (same_files) {
    message("Checks PASSED: the CSV and RDA file predictions are the same")
  } else{
    message("Checks FAILED: the CSV and RDA file predictions are NOT the same")}
  
  
  # summary of rows, models, locations
  csv_summary <- predictions_csv2 %>%
    summarize(
      file = "csv",
      rows = n(),
      models = length(unique(model)),
      locations = length(unique(location_id)))
  
  rda_summary <- predictions_rda2 %>% 
    as.data.frame() %>%
    summarize(
      file = "rda",
      rows = n(),
      models = length(unique(model)),
      locations = length(unique(location_id)))
  
  rbind(csv_summary, rda_summary)
}

message("done with r3")
