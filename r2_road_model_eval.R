# script generates onroad out-of-sample model predictions at stop locations
# evaluates each model 

##################################################################################################
# setup
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

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
# message("loading data")
# 
# ## 5884 locations used. does not include all necessary covariates used (e.g. pop10, bus)
# road_locations_used <- readRDS(file.path("data", "onroad", "annie", "cov_onroad_preprocessed.rds")) %>%
#   pull(native_id)
# 
# cov <- read.csv(file.path("data", "onroad", "dr0364d_20230331.txt")) %>%
#   filter(native_id %in% road_locations_used) %>%
#   mutate(native_id = as.character(native_id),
#          location = substr(native_id, nchar(native_id)-3, nchar(native_id)),
#          location = as.numeric(location)) %>%
#   generate_new_vars() %>%
#   select(location, latitude, longitude, all_of(cov_names))
#   
# ## 5874 locations
# onroad_ns <- readRDS(file.path("data", "onroad", "annie", "PNC_nonspatial_annavgs.rds")) %>%
#   mutate(spatial_code = "sn")
# onroad_s <- readRDS(file.path("data", "onroad", "annie", "PNC_spatial_annavgs.rds")) %>%
#   mutate(spatial_code = "sy")
# onroad0 <- rbind(onroad_ns, onroad_s) %>%
#   rename(location=id,
#          value = annual_mean) %>%
#   # log transform pollutant concentrations before modeling
#   mutate(value = ifelse(value== 0, 1, value),
#          value = log(value),
#          variable = "pnc_noscreen"
#          ) 
#    
# ### TEST 
# #distinct(onroad_ns, design, version, visits, adjusted)
# # distinct(onroad_s, design, visits, version, adjusted )
# 
# #rm(onroad_ns, onroad_s)


onroad <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data.rda"))

# stationary data; for out-of-sample validation
stationary <- filter(annual,
                     design=="full",
                     variable=="pnc_noscreen")

##################################################################################################
# OUT-OF-SAMPLE VALIDATION AT 309 STOP LOCATIONS
##################################################################################################
message("Generating predictions at stop locations")

stationary_predictions <- mclapply(group_split(onroad, model), mc.cores = 1,#use_cores, 
                                   function(x) {
  
  uk_pls(modeling_data = x, new_data = stationary) %>%
    #fn has binding issues later if don't drop geom 
     st_drop_geometry() %>%
     #add info to new dataset about the prediction model
     mutate(model = first(x$model))
                                     }) %>%
  bind_rows()  

message("saving TEMP predictions")
saveRDS(stationary_predictions, file.path(dt_path, "UK Predictions", "TEMP_onroad_predictions.rda"))
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
saveRDS(predictions, file.path(dt_path, "UK Predictions", "onroad_predictions.rda"))

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
  
  result <- distinct(dt, model, out_of_sample# , reference
                     ) %>%
    mutate(
      no_sites = nrow(dt),
      RMSE = RMSE,
      MSE_based_R2 = MSE_based_R2
      )
  
  return(result)
}


validation_stats(dt=group_split(predictions, model, out_of_sample)[[1]], prediction = "prediction", reference = "gs_estimate")


model_perf0 <- mclapply(group_split(predictions, model, out_of_sample), 
                        mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "gs_estimate") %>%
  bind_rows()

##################################################################################################

message("saving model evaluation statistics")

model_perf0 %>%
  saveRDS(., file.path(dt_path, "onroad_model_eval.rda"))



message("done with r2")

