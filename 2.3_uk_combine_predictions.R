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

pacman::p_load(tidyverse, sf)    

set.seed(1)


##################################################################################################
# LOAD DATA & PREDICTIONS
##################################################################################################
# estimates
annual <- readRDS(file.path("Output", "annual_training_set2.rda"))
#annual_test_set <- readRDS(file.path("Output", "annual_test_set2.rda"))

#predictions
#test_set_predictions <- readRDS(file.path("Output", "UK Predictions", "test_set_predictions.rda"))
cv_predictions <- readRDS(file.path("Output", "UK Predictions", "cv_predictions.rda"))

##################################################################################################
# COMBINE PREDICTIONS
##################################################################################################

# add campaign-specific and gold-standard estimates to predictions 
# in UK, can't predict at same places, so can't compare estimates & predictions w/o doing CV

#GS estimates for 278 sites
annual_gs_estimates <- annual %>% st_drop_geometry() %>%
  filter(grepl("full", design)) %>%
  distinct(location, variable, value) %>%
  rename(gs_estimate = value)

# #GS estimates for 31 sites
# test_gs_estimates <- annual_test_set %>% st_drop_geometry() %>%
#   distinct(location, variable, value) %>%
#   rename(gs_estimate = value)

#GS estimates for all 309 sites
#gs_estimates <- bind_rows(annual_gs_estimates, test_gs_estimates)

# estimates from specific campaign simultaions (n=278 sites)
## note that these only change w/ temporal sims where fewer visits/site are used to estimate annual averages
campaign_estimates <- annual %>% st_drop_geometry() %>%
  distinct(location, design, version, campaign, variable, value) %>% 
  rename(campaign_estimate = value)


# combine predictions & estimates
#predictions0 <- cv_predictions
#predictions0 <- rbind(test_set_predictions, cv_predictions) 
# predictions0 <- test_set_predictions 

predictions <- cv_predictions %>%
  #left join b/c locations w/ predictions may be fewer than the 309 sites if dno't do 10FCV
  left_join(annual_gs_estimates) %>%
  left_join(campaign_estimates) %>%
  #put back on native scale before evaluating
  mutate_at(vars(contains("estimate"), prediction), ~exp(.)) 


##################################################################################################
# SAVE DATA
##################################################################################################
print("saving predictions")
saveRDS(predictions, file.path("Output", "UK Predictions", "all_predictions.rda"))


message("done with 2.3") 
