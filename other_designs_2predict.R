# Rscript other_designs_2predict.R other_stop_designs_data_fewhrs.rda cohort/other\ designs/fewhrs
# Rscript other_designs_2predict.R other_stop_designs_data_balsea_1.rda cohort/other\ designs/balsea_1
# Rscript other_designs_2predict.R other_stop_designs_data_balsea_2.rda cohort/other\ designs/balsea_2
# Rscript other_designs_2predict.R other_stop_designs_data_balsea_3.rda cohort/other\ designs/balsea_3

# Rscript other_designs_2predict.R other_stop_designs_data_balsea_4.rda cohort/other\ designs/balsea_4

# Rscript other_designs_2predict.R other_stop_designs_data_sitetype.rda cohort/other\ designs/sitetype

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

# install.packages(c("pls", "gstat", "dplyr"))
pacman::p_load(pls, gstat, sf, 
               lubridate,
               #tools,
               parallel,
               dplyr  #tidyverse
               )

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

set.seed(1)

################################################################################
# DATA
################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)
# user_arguments <- "other_stop_designs_data.rda" 

modeling_dt <- user_arguments[1]
prediction_directory <-user_arguments[2]
  
message("loading data")
modeling_data <- readRDS(file.path(dt_path, "Selected Campaigns", modeling_dt))

dt <- readRDS(file.path("data", "dr0357_cohort_covar_20220404_in_mm_area_prepped.rda")) 
#cov_ext <- tools::file_ext(covariate_file_path)

#where predictions should be saved
prediction_directory <- file.path(dt_path, "UK Predictions", prediction_directory)
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = T)}

###########################################################################################
# Universal Kriging - Partial Least Squares Model function
cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# desired PLS components to use (from a different script): 3
pls_comp_n <- readRDS(file.path(dt_path, "pls_comp_n.rda")) 

#prediction model
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
message("Generating predictions at new locations")

# this doesn'st help b/c order is not maintained when use group_split()?
tot_models <- length(unique(modeling_data$model_no))

predictions0 <- mclapply(group_split(modeling_data, model), #[1:2]
                       mc.cores = 6, #4
                       function(x) {
                         message(paste0("model " , first(x$model_no), " of ", tot_models, ": ", first(x$model)))
                         
                         temp <- dt %>%
                           mutate(model = first(x$model),
                                  variable = first(x$variable)) %>%
                           uk_pls(new_data = ., modeling_data = x)
                       }) %>%
  bind_rows()  

#message("saving TEMPORARY predictions")
#saveRDS(predictions0, file.path(prediction_directory, paste0("TEMP_predictions_", Sys.Date(),".rda")))

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

message("saving predictions here:")
#message(file.path(prediction_directory, paste0("other_design_predictions_", Sys.Date(),".csv")))
message(prediction_directory)

# save as RDS & CSV file
saveRDS(predictions, file.path(prediction_directory, paste0("predictions_", Sys.Date(),".rda")))
# predictions %>%
#   select(-variable) %>%
#   write.csv(., file.path(prediction_directory, paste0("predictions_", Sys.Date(),".csv")), row.names = F)

###########################################################################################
# QC CHECKS
###########################################################################################
qc <- FALSE #TRUE

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


message("done with other_designs_2predict.R")
