
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

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

#load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))
if(!dir.exists(file.path(dt_path, "UK Predictions"))){dir.create(file.path(dt_path, "UK Predictions"))}

set.seed(1)

##################################################################################################
# STANDARD CROSS-VALIDATION
##################################################################################################

## each df in the list is a simulation; running 10-FCV within each simulation to get CV predictions
print(paste0("Running random ", k, " FCV"))

cv_predictions0 <- mclapply(group_split(annual, spatial_temporal, design, version, campaign, variable), 
                            mc.cores = use_cores, 
                            FUN = do_cv, fold_name = "random_fold") %>%
  bind_rows() 


cv_predictions <- cv_predictions0 %>% 
  select(all_of(common_vars)) %>%
  mutate(out_of_sample = "CV") %>%
  drop_na(prediction)

##################################################################################################
# SAVE PREDICTIONS
##################################################################################################

message("saving predictions")
saveRDS(cv_predictions, file.path(dt_path, "UK Predictions", "cv_predictions.rda"))

message("done with 2.1")
