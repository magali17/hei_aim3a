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

source("file_paths.R")
source("functions.R")
if(!dir.exists(file.path("Output", "UK Predictions"))){dir.create(file.path("Output", "UK Predictions"))}

##################################################################################################
# LOAD DATA
##################################################################################################

annual <- annual %>%
  mutate(
    # select the same campaign for all NS models (total & bin-specific)
    variable2 = variable,
    variable = ifelse(grepl("ns_", variable), "ns_total_conc", variable)
  )

selected_campaigns0 <- read_rds(file.path("Output", "selected_campaigns.rda")) %>%
  
  # -->  ? select based on out-of-sample performance?
  filter(out_of_sample=="Test")


# only keep annual averages for selected campaigns
selected_campaigns  <- left_join(selected_campaigns0, annual) %>%
  #add original names back
  mutate(variable = variable2) %>%
  select(-variable2) %>%
  #unique id for each campaign, design, version, performnace
  group_by(campaign, design, version, variable, performance) %>%
  mutate(campaign_id = cur_group_id()) %>%
  ungroup() %>%
  select(campaign_id, everything())
  
saveRDS(selected_campaigns, file.path("Output", "site_data_for_selected_campaigns.rda"))

##################################################################################################
# SAVE UK PARAMETERS WITH FULL TRAINING DATA FOR EACH UNIQUE CAMPAIGN
##################################################################################################

# --> DELETE? don't need? 

set.seed(1)

model_parameters <- list()
model_fits <- c()

for(i in seq_along(var_names)) {
  # i=1
  # x = group_split(filter(selected_campaigns, variable == var_names[i]), design, version, campaign, performance)[[3]]
  model_fits0 <- lapply(group_split(filter(selected_campaigns, variable == var_names[i]), design, version, campaign, performance),
                   #mc.cores = use_cores,
                   function(x) {
                     df = uk_pls(modeling_data = x, new_data = filter(annual_test_set, variable == var_names[i]),
                                 fn_result = "models"#,
                                 # use an exponential variogram for all models
                                 #var_choice = "Exp"
                                 )
                     df$campaign_info <- distinct(x, campaign_id, design, version, variable, performance, out_of_sample, reference)
                     
                     df
                   })
  model_fits <- c(model_fits, model_fits0)
  
}

# SAVE
saveRDS(model_fits, file.path("Output", "model_fits.rda"))


