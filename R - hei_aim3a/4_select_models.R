#script purpose: select models to send to KPRI

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
               pls, gstat, sf  # for PLS-UK model
               )    

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

load(file.path(dt_path, "uk_workspace.rdata"))

set.seed(1)

source("functions.R")
if(!dir.exists(file.path(dt_path, "UK Predictions"))){dir.create(file.path(dt_path, "UK Predictions"))}
if(!dir.exists(file.path(dt_path, "Selected Campaigns"))){dir.create(file.path(dt_path, "Selected Campaigns"))}

# # place to save predictions later
# if(!dir.exists(file.path(dt_path, "UK Predictions", "cohort"))){dir.create(file.path(dt_path, "UK Predictions", "cohort"), recursive = T)}
# if(!dir.exists(file.path(dt_path, "UK Predictions", "grid"))){dir.create(file.path(dt_path, "UK Predictions", "grid"), recursive = T)}


##################################################################################################
# LOAD DATA
##################################################################################################
cw <- read.csv(file.path(dt_path, "model_cw.csv"))

selected_campaigns0 <- read_rds(file.path(dt_path, "model_eval.rda")) %>%
  filter(reference == "gs_estimate")

saveRDS(selected_campaigns0, file.path(dt_path, "Selected Campaigns", "selected_campaigns.rda"))


#save reference model_ids
selected_campaigns0 %>%
  filter(version=="all training data") %>%
  #mutate(model_id = paste0("mb_", campaign_id)) %>%
  select(#model_id, 
    model,
    design, version, variable) %>%  
  saveRDS(file.path(dt_path, "Selected Campaigns", "all_data_campaign_refs.rda"))


# only keep annual averages for selected campaigns
selected_campaigns  <- selected_campaigns0 %>%
  select(campaign, design, version, variable, #performance, 
         model) %>%
  left_join(annual) %>%
  select(model, variable, location, value:last_col())
  
# save all data
saveRDS(selected_campaigns, file.path(dt_path, "Selected Campaigns", "site_data_for_all_selected_campaigns.rda"))

# save smaller files (program has issues later w/ large file)
lapply(var_names, function(x) {
  filter(selected_campaigns, variable== x) %>%
    saveRDS(., file.path(dt_path, "Selected Campaigns", paste0("site_data_for_", x,".rda")))
})


message("done with 4")

