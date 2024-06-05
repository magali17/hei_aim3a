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
               lubridate)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
dt_path_onroad <- file.path(dt_path, "onroad")

set.seed(1)

################################################################################
# DATA
################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)
# user_arguments <- "onroad_modeling_data_SP_FALSE_ADJ_FALSE.rda" 

modeling_dt <- user_arguments[1]

message("loading data")

# new covariate file
modeling_data <- readRDS(file.path(dt_path_onroad, "modeling_data", modeling_dt))

dt <- readRDS(file.path("data", "dr0357_cohort_covar_20220404_in_mm_area_prepped.rda")) 

#where predictions should be saved
new_prediction_location <- user_arguments[2]

prediction_directory <- file.path(dt_path_onroad, "predictions", new_prediction_location)
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = T)}

# --> UPDATE FILE NAME NAME
# prediction file label
#p_name <- substr(modeling_dt, 21, nchar(modeling_dt)-4)
p_name <- substr(modeling_dt, 1, nchar(modeling_dt)-4)

###########################################################################################
# Universal Kriging - Partial Least Squares Model function
cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# desired PLS components to use (from a different script):  
pls_comp_n <- read_rds(file.path(dt_path, "pls_comp_n.rda")) 

#prediction model
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
message("Generating predictions at new locations")

predictions0 <- lapply(group_split(modeling_data, model), #[1:2], #mc.cores = 1,# 4,
  function(x) {
    
    message(paste("model: " , first(x$model_no2), first(x$model)))
    
    temp <- dt %>%
      mutate(model = first(x$model),
             variable = first(x$variable)) %>%
      uk_pls(new_data = ., modeling_data = x)
  }) %>%
  bind_rows()  


# message("saving TEMPORARY predictions")
# saveRDS(predictions0, file.path(prediction_directory, paste0("TEMP_onroad_predictions_", p_name, "_", Sys.Date(),".rda")))

###########################################################################################
# CLEAN DATA FOR KP
###########################################################################################
predictions <- predictions0 %>%
  
  st_drop_geometry() %>%
  
  mutate(
    #put back on the native scale
    prediction = exp(prediction),
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01"),
    end_date = ymd("2021-07-09")
    ) %>%
  select(location_id, start_date, end_date, model, variable, prediction) 

message("saving predictions")

saveRDS(predictions, file.path(prediction_directory, paste0("onroad_predictions_", p_name, "_",Sys.Date(),".rda")))

predictions %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_directory, paste0("onroad_predictions_", p_name, "_", Sys.Date(),".csv")))

###########################################################################################
# QC CHECKS
###########################################################################################
qc <- TRUE

#if(qc==TRUE) {stop}

# summary of predictions
if(qc==TRUE) {
  message("QC Summary")
  
  print("distribution of predictions. N = models x cohort locations")
  result <- predictions %>%
    group_by(variable) %>%
    summarize(n = n(),
              models = length(unique(model)),
              min = min(prediction),
              mean = mean(prediction),
              max = max(prediction),
              NAs = sum(is.na(prediction)))
  print(result)
  }

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R3_ROAD_PREDICT.R")
