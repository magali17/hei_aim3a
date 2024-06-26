# script generates onroad out-of-sample model predictions at stop locations
# evaluates each model 
# RESULTS are in: Output/v3_20230321/onroad/model_eval

# Rscript r2_road_model_eval.R balanced.rda

##################################################################################################
# SETUP
##################################################################################################

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse,
               parallel, #mclapply() 
               pls, gstat, sf # UK-PLS MODEL
)    

source("functions.R")
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
dt_path_onroad <- file.path(dt_path, "onroad")


if(!dir.exists(file.path(dt_path_onroad, "model_eval", "predictions_at_stationary_sites"))){dir.create(file.path(dt_path_onroad, "model_eval", "predictions_at_stationary_sites"), recursive = T)}

#load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))

use_cores <- 4 #6 receives warning w/ 20 smp for use all.rda
set.seed(1)

##################################################################################################
# speed things up
#testing_mode <- FALSE # TRUE if want to reduce models

##################################################################################################
# DATA
##################################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)

modeling_dt <- user_arguments[1]

# prediction file label
p_name <- substr(modeling_dt, 1, nchar(modeling_dt)-4)

message(paste("prediction file label:", p_name))

message("loading data")

onroad <- readRDS(file.path(dt_path_onroad, "modeling_data", modeling_dt))
# onroad <- lapply(file_names, function(f) {
#   f_name <- file.path(dt_path_onroad, "modeling_data", f)
#   message(paste0("...", f_name))
# 
#   readRDS(f_name)
#   }) %>%
#   bind_rows()

# stationary data; for out-of-sample validation
stationary <- filter(annual,
                     design=="full",
                     variable=="pnc_noscreen")

##################################################################################################
# OUT-OF-SAMPLE VALIDATION AT 309 STOP LOCATIONS
##################################################################################################
message("Generating predictions at stop locations")

start_time <- Sys.time()

stationary_predictions <- mclapply(group_split(onroad, model), mc.cores = use_cores, function(x) {
  
  message(first(x$model))
  
  uk_pls(modeling_data = x, new_data = stationary) %>%
    #fn has binding issues later if don't drop geom 
     st_drop_geometry() %>%
     #add info to new dataset about the prediction model
     mutate(model = first(x$model))
                                     }) %>%
  bind_rows()  

end_time <- Sys.time()

message("run time:")
end_time-start_time
        
# message("saving TEMP predictions")
# saveRDS(stationary_predictions, file.path(dt_path_onroad, "model_eval", "predictions_at_stationary_sites", paste0("TEMP_predictions_", p_name, ".rda")))
##################################################################################################
# COMBINE PREDICTIONS; FORMAT DF 
##################################################################################################
message("combining predictions with estimates")

predictions <- stationary_predictions %>% 
  select(location, prediction, model) %>%
  mutate(out_of_sample = "Stationary Sites") %>%
  drop_na(prediction)

annual_gs_estimates <- stationary %>% st_drop_geometry() %>%
  select(location, gs_estimate = value) 
   
predictions <- predictions %>%
  #left join b/c locations w/ predictions may be fewer than the 309 sites if dno't do 10FCV
  left_join(annual_gs_estimates) %>%
  #put back on native scale before evaluating
  mutate_at(vars(contains("estimate"), prediction), ~exp(.)) 

predictions_path <- file.path(dt_path_onroad, "model_eval", "predictions_at_stationary_sites", paste0(p_name, ".rda"))
message(paste("saving predictions:", predictions_path))
saveRDS(predictions, predictions_path)

##################################################################################################
# CV STATS FUNCTION
##################################################################################################
validation_stats <- function(dt, prediction, reference){
  
  # MSE of predictions
  MSE_pred <- mean((dt[[reference]] - dt[[prediction]])^2)
  # MSE of observations (for R2 denominator)
  MSE_obs <- mean((dt[[reference]] - mean(dt[[reference]]))^2)
  
  RMSE = sqrt(MSE_pred)
  MSE_based_R2 = max(1 - MSE_pred/MSE_obs, 0)
  # alternative gives same mse-based R2
  # caret::R2(pred = dt$prediction,obs =dt$estimate, form = "traditional")
  reg_based_R2 = cor(dt[[reference]], dt[[prediction]], method = "pearson")^2
  
  result <- distinct(dt, model, out_of_sample# , reference
                     ) %>%
    mutate(
      no_sites = nrow(dt),
      RMSE = RMSE,
      MSE_based_R2 = MSE_based_R2,
      reg_based_R2 = reg_based_R2
      )
  
  return(result)
}

message("calculating performance statistics")

model_perf0 <- mclapply(group_split(predictions, model, out_of_sample), 
                        mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "gs_estimate") %>%
  bind_rows()

##################################################################################################
model_eval_path <- file.path(dt_path_onroad, "model_eval", paste0(p_name, "_model_eval_at_stationary_sites.rda"))
message(paste("saving model evaluation statistics:", model_eval_path))

saveRDS(model_perf0, model_eval_path)

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R2_ROAD_MODEL_EVAL.R")

