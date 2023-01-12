#script purpose: select models

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

load(file.path("Output", "uk_workspace.rdata"))

set.seed(1)

#source("file_paths.R")
source("functions.R")
if(!dir.exists(file.path("Output", "UK Predictions"))){dir.create(file.path("Output", "UK Predictions"))}

##################################################################################################
# LOAD DATA
##################################################################################################

selected_campaigns0 <- read_rds(file.path("Output", "model_eval.rda")) %>%
  filter(out_of_sample == "Test",
         reference == "gs_estimate")

saveRDS(selected_campaigns0, file.path("Output", "selected_campaigns.rda"))


# only keep annual averages for selected campaigns
selected_campaigns  <- selected_campaigns0 %>%
  select(campaign, design, version, variable, performance, campaign_id) %>%
  left_join(annual) 
  
saveRDS(selected_campaigns, file.path("Output", "site_data_for_selected_campaigns.rda"))

 
