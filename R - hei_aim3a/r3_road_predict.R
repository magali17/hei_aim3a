# this script is almost identical to 5_prediction_program.R but slightly shorter for mobile data since QC, new covariate development are not needed

# Rscript r3_road_predict.R balanced.rda 20240605

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

use_cores <- 6 # 4 works w/ smp ~10+ 

# QC
print_prediction_summary <- FALSE # TRUE when want to check a few models. 
################################################################################
# DATA
################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)

modeling_dt <- user_arguments[1]

# prediction file label
p_name <- substr(modeling_dt, 1, nchar(modeling_dt)-4)

message(paste("prediction label:", p_name))

message("loading data")

# new covariate file
modeling_data <- readRDS(file.path(dt_path_onroad, "modeling_data", modeling_dt))

dt <- readRDS(file.path("data", "dr0357_cohort_covar_20220404_in_mm_area_prepped.rda")) 

#where predictions should be saved
prediction_date <- user_arguments[2]

prediction_directory <- file.path(dt_path_onroad, "predictions", prediction_date, p_name)
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = T)}
message(paste("predictions will be stored here:", prediction_directory))

###########################################################################################
# for Universal Kriging - Partial Least Squares Model function
cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# desired PLS components to use (from a different script):  
pls_comp_n <- read_rds(file.path(dt_path, "pls_comp_n.rda")) 

#prediction model
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
message("Generating predictions at new locations")

mclapply(group_split(modeling_data, model), mc.cores = use_cores, function(x){
  this_model <- first(x$model)
  file_name <- file.path(prediction_directory, paste0(this_model, ".rda"))
  message(this_model)
  
  if(!file.exists(file_name)){
    
    predictions <- dt %>%
      mutate(model = this_model,
             variable = first(x$variable)) %>%
      uk_pls(new_data = ., modeling_data = x) %>%
      
      # clean data for KP
      st_drop_geometry() %>%
      mutate(
        #put back on the native scale
        prediction = exp(prediction),
        # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
        start_date = ymd("1988-01-01"),
        end_date = ymd("2021-07-09")) %>%
      select(location_id, start_date, end_date, model, variable, prediction) 
    
    message("...saving predictions")
 
    saveRDS(predictions, file_name)
    
    # predictions %>%
    #   select(-variable) %>%
    #   write_csv(., gsub(".rda", ".csv", file_name))
    
    
    #########################################################
    # QC
    
    # summary of predictions
    if(print_prediction_summary==TRUE) {
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
    }
  })
  
  
  
   









##########################################

# predictions0 <- mclapply(group_split(modeling_data, model), mc.cores = use_cores, function(x) {
#     
#     message(first(x$model))
#     
#     temp <- dt %>%
#       mutate(model = first(x$model),
#              variable = first(x$variable)) %>%
#       uk_pls(new_data = ., modeling_data = x)
#   }) %>%
#   bind_rows()  
# 
# 
# # message("saving TEMPORARY predictions")
# # saveRDS(predictions0, file.path(prediction_directory, paste0("TEMP_onroad_predictions_", p_name, "_", Sys.Date(),".rda")))
# 
# ###########################################################################################
# # CLEAN DATA FOR KP
# ###########################################################################################
# predictions <- predictions0 %>%
#   
#   st_drop_geometry() %>%
#   
#   mutate(
#     #put back on the native scale
#     prediction = exp(prediction),
#     # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
#     start_date = ymd("1988-01-01"),
#     end_date = ymd("2021-07-09")
#     ) %>%
#   select(location_id, start_date, end_date, model, variable, prediction) 
# 
# message("saving predictions")
# 
# file_name <- file.path(prediction_directory, paste0(Sys.Date(), "_predictions_", p_name, ".rda"))
# 
# saveRDS(predictions, file_name)
# 
# predictions %>%
#   select(-variable) %>%
#   write_csv(., gsub(".rda", ".csv", file_name))
# 
# ###########################################################################################
# # QC CHECKS
# ###########################################################################################
# print_prediction_summary <- TRUE
# 
# #if(qc==TRUE) {stop}
# 
# # summary of predictions
# if(print_prediction_summary==TRUE) {
#   message("QC Summary")
#   
#   print("distribution of predictions. N = models x cohort locations")
#   result <- predictions %>%
#     group_by(variable) %>%
#     summarize(n = n(),
#               models = length(unique(model)),
#               min = min(prediction),
#               mean = mean(prediction),
#               max = max(prediction),
#               NAs = sum(is.na(prediction)))
#   print(result)
#   }

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R3_ROAD_PREDICT.R")
