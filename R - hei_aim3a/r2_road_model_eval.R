# script generates onroad out-of-sample model predictions at stop locations
# evaluates each model 

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
               parallel, #mclapply; detectCores()
               pls, gstat, sf # UK-PLS MODEL
)    

source("functions.R")
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

#load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))
if(!dir.exists(file.path(dt_path, "UK Predictions"))){dir.create(file.path(dt_path, "UK Predictions"))}

use_cores <- 4#1
set.seed(1)

##################################################################################################
# DATA
##################################################################################################
message("loading data")
onroad <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda"))

# stationary data; for out-of-sample validation
stationary <- filter(annual,
                     design=="full",
                     variable=="pnc_noscreen")

##################################################################################################
# OUT-OF-SAMPLE VALIDATION AT 309 STOP LOCATIONS
##################################################################################################
message("Generating predictions at stop locations")

stationary_predictions <- mclapply(group_split(onroad, model), mc.cores = use_cores, 
                                   function(x) {
  
  uk_pls(modeling_data = x, new_data = stationary) %>%
    #fn has binding issues later if don't drop geom 
     st_drop_geometry() %>%
     #add info to new dataset about the prediction model
     mutate(model = first(x$model))
                                     }) %>%
  bind_rows()  

# message("saving TEMP predictions")
# saveRDS(stationary_predictions, file.path(dt_path, "UK Predictions", "TEMP_onroad_predictions.rda"))
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

message("saving predictions")
saveRDS(predictions, file.path(dt_path, "UK Predictions", "onroad_predictions_20240313.rda"))

##################################################################################################
# CV STATS FUNCTION
##################################################################################################
message("calculating performance statistics")

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

model_perf0 <- mclapply(group_split(predictions, model, out_of_sample), 
                        mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "gs_estimate") %>%
  bind_rows()

##################################################################################################
message("saving model evaluation statistics")
saveRDS(model_perf0, file.path(dt_path, "onroad_model_eval_20240313.rda"))

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R2_ROAD_MODEL_EVAL.R")

