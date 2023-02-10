# THIS SCRIPT NO LONGER IS IN USE B/C WE ARE USING ALL OF THE SITE IN THE TRAINING MODEL

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
               pls, gstat, sf  # for PLS-UK model
               )    

#load the prediction workspace
load(file.path("Output", "uk_workspace.rdata"))
if(!dir.exists(file.path("Output", "UK Predictions"))){dir.create(file.path("Output", "UK Predictions"))}

##################################################################################################
# TEST SET PREDICTIONS
##################################################################################################
set.seed(1)

# WARNNINGS: In fit.variogram(object, x, fit.sills = fit.sills, fit.ranges = fit.ranges,  ... : singular model in variogram fit
# see Note in help(fit.variogram). This has to do with flat variograms w/ little spatial correlation. Try plotting the variograms.

print("test set validation")

test_set_predictions0 <- data.frame()

#1 pollutant at a time to make sure things are arranged correctly with the test set
for(i in seq_along(var_names)) {
  
  df <- mclapply(group_split(filter(annual, variable == var_names[i]), spatial_temporal, design, version, campaign), 
                 mc.cores = use_cores,
                 function(x) {
                   df = uk_pls(modeling_data = x, new_data = filter(annual_test_set, variable == var_names[i])) %>%
                     #fn has binding issues later if don't drop geom 
                     st_drop_geometry() %>%
                     #add info to new dataset about the prediction model
                     mutate(
                       spatial_temporal = first(x$spatial_temporal), 
                       design = first(x$design), 
                       version  = first(x$version), 
                       campaign = first(x$campaign)
                     )  
                 }) %>%
    bind_rows()
  
  test_set_predictions0 <- bind_rows(test_set_predictions0, df)
  
}

test_set_predictions <- test_set_predictions0 %>% 
  select(all_of(common_vars)) %>%
  mutate(out_of_sample = "Test")

##################################################################################################
# SAVE DATA
##################################################################################################
saveRDS(test_set_predictions, file.path("Output", "UK Predictions", "test_set_predictions.rda"))

message("done with 2_uk_test.R")
