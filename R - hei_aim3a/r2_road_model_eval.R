# script generates onroad out-of-sample model predictions at stop locations
# evaluates each model 

# RESULTS are in: Output/v3_20230321/onroad/model_eval

# --> TO DO: MAKE THIS INTO A PROGRAM & SAVE INDIVIDUAL/SMALLER FILES? 

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
dt_path_onroad <- file.path(dt_path, "onroad")
#if(!dir.exists(file.path(dt_path_onroad, "predictions"))){dir.create(file.path(dt_path_onroad, "predictions"), recursive = T)}
if(!dir.exists(file.path(dt_path_onroad, "model_eval"))){dir.create(file.path(dt_path_onroad, "model_eval"), recursive = T)}

#load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))

use_cores <- 3 #4 Braincluster crashes w/ 4?
set.seed(1)

##################################################################################################
# speed things up
testing_mode <- FALSE # TRUE if want to reduce models

##################################################################################################
# DATA
##################################################################################################
message("loading data")

# errors out if you try to load the full file "all.rda" at once
file_names <- list.files(file.path(dt_path_onroad, "modeling_data")) %>%
  grep("all.rda", ., invert = T, value = T)
 
if(testing_mode==TRUE){file_names <- file_names[1]}

onroad <- lapply(file_names, function(f) {
  f_name <- file.path(dt_path_onroad, "modeling_data", f)
  message(paste("...", f_name))

  readRDS(f_name)
  }) %>%
  bind_rows()

# stationary data; for out-of-sample validation
stationary <- filter(annual,
                     design=="full",
                     variable=="pnc_noscreen")

##################################################################################################
# OUT-OF-SAMPLE VALIDATION AT 309 STOP LOCATIONS
##################################################################################################
message("Generating predictions at stop locations")

stationary_predictions <- mclapply(group_split(onroad, model), mc.cores = use_cores, function(x) {
  
  message(first(x$model))
  
  uk_pls(modeling_data = x, new_data = stationary) %>%
    #fn has binding issues later if don't drop geom 
     st_drop_geometry() %>%
     #add info to new dataset about the prediction model
     mutate(model = first(x$model))
                                     }) %>%
  bind_rows()  

message("saving TEMP predictions")
saveRDS(stationary_predictions, file.path(dt_path_onroad, "model_eval", "TEMP_onroad_predictions.rda"))
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
saveRDS(predictions, file.path(dt_path_onroad, "model_eval", "oos_predictions_at_stationary_sites.rda"))

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
message("saving model evaluation statistics")
saveRDS(model_perf0, file.path(dt_path_onroad, "model_eval", "model_eval.rda"))

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R2_ROAD_MODEL_EVAL.R")

