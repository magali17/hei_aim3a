#script purpose: evaluate UK-PLS model performances

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
               sf,  
               units # set_units()
)    

use_cores <- 5

set.seed(1)

##################################################################################################
# LOAD DATA
##################################################################################################
# mapping variables
project_crs <- 4326  #lat/long
m_crs <- 32148
 
# uk predictions
predictions <- readRDS(file.path("Output", "UK Predictions", "all_predictions.rda")) %>% 
  pivot_longer(contains("estimate"), names_to = "reference", values_to = "estimate")  

# simulation details
sims <- readRDS(file.path("Output", "annual_training_set.rda")) %>%  
  distinct(location, visits, campaign, design, version, spatial_temporal)


#location lat/long 
loc_lat_long <- readRDS(file.path(
  "data",
  "location_lat_long.rda")) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs)  

##################################################################################################
# CV STATS FUNCTION
##################################################################################################

# Fn returns RMSE and MSE-based R2 for a given dataset
validation_stats <- function(dt, prediction, reference){

  # MSE of predictions
  MSE_pred <- mean((dt[[reference]] - dt[[prediction]])^2)
  # MSE of observations (for R2 denominator)
  MSE_obs <- mean((dt[[reference]] - mean(dt[[reference]]))^2)
  
  RMSE = sqrt(MSE_pred)
  MSE_based_R2 = max(1 - MSE_pred/MSE_obs, 0)
  # alternative gives same mse-based R2
  # caret::R2(pred = dt$prediction,obs =dt$estimate, form = "traditional")
  
  result <- distinct(dt, campaign, design, version, variable, out_of_sample, reference) %>%
    mutate(
      no_sites = nrow(dt),
      RMSE = RMSE,
      MSE_based_R2 = MSE_based_R2,
      #reg_based_R2 = reg_based_R2
    )
  
  return(result)
  
}

saveRDS(validation_stats, file.path("Output", "validation_stats_fn.rda"))
##################################################################################################
# don't do traditional assessment for spatial clustering - distance analysis
message("calculating performance statistics")
 

model_perf0 <- mclapply(group_split(predictions, campaign, design, version, variable, out_of_sample, reference), 
                       mc.cores = use_cores,
                       validation_stats, prediction = "prediction", reference = "estimate") %>%
  bind_rows()


# label performance order 
model_perf <- model_perf0 %>% 
  group_by(design, version, variable, out_of_sample, reference) %>%
  arrange(MSE_based_R2) %>%
  mutate(performance = row_number()) %>%
  
  arrange(design, version, variable,
          out_of_sample, 
          #campaign, 
          performance) %>%
  
  #group_by( design, version, variable, campaign) %>%
  
  ungroup() %>%
  mutate(campaign_id = row_number() 
         #campaign_id =cur_group_id()
         ) %>%
ungroup() 
  
##################################################################################################
# SAVE DATA
##################################################################################################
select(model_perf , -no_sites) %>%
  saveRDS(., file.path("Output", "model_eval.rda"))

message("done with 3_model_eval.R")

