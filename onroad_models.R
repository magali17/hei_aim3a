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
onroad <- 


## each df in the list is a simulation; running 10-FCV within each simulation to get CV predictions
print(paste0("Running random ", k, " FCV"))

cv_predictions0 <- mclapply(group_split(onroad, spatial_temporal, design, version, campaign, variable), 
                            mc.cores = use_cores, 
                            FUN = do_cv, fold_name = "random_fold") %>%
  bind_rows() 


cv_predictions <- cv_predictions0 %>% 
  select(all_of(common_vars)) %>%
  mutate(out_of_sample = "CV") %>%
  drop_na(prediction)

# message("saving predictions")
# saveRDS(cv_predictions, file.path(dt_path, "UK Predictions", "cv_onroad_predictions.rda"))

##################################################################################################
# predictions vs estimates
##################################################################################################


annual_gs_estimates <- onroad %>% st_drop_geometry() %>%
  filter(grepl("full", design)) %>%
  distinct(location, variable, value) %>%
  rename(gs_estimate = value)

# estimates from specific campaign simultaions (n=278 sites)
## note that these only change w/ temporal sims where fewer visits/site are used to estimate onroad averages
campaign_estimates <- onroad %>% st_drop_geometry() %>%
  distinct(location, design, version, campaign, variable, value) %>% 
  rename(campaign_estimate = value)

predictions <- cv_predictions %>%
  #left join b/c locations w/ predictions may be fewer than the 309 sites if dno't do 10FCV
  left_join(onroad_gs_estimates) %>%
  left_join(campaign_estimates) %>%
  #put back on native scale before evaluating
  mutate_at(vars(contains("estimate"), prediction), ~exp(.)) 

print("saving predictions")
saveRDS(predictions, file.path(dt_path, "UK Predictions", "onroad_predictions.rda"))

##################################################################################################
# CV STATS FUNCTION
##################################################################################################
validation_stats <- file.path(dt_path, "validation_stats_fn.rda")

message("calculating performance statistics")


model_perf0 <- mclapply(group_split(predictions, campaign, design, version, variable, out_of_sample, reference), 
                        mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "estimate") %>%
  bind_rows()



##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################

# --> UPDATE

cw <- model_perf0 %>% 
  select(variable, design, version, campaign) %>% 
  distinct() %>%
  arrange(variable, design, version, campaign) %>%
  mutate(
    var_code = gsub("\\.\\d", "", variable),
    var_code = gsub("_","", var_code),
    var_code = ifelse(var_code=="nstotalconc", "nstot", 
                      #ifelse(var_code=="ns10100", "ns100", 
                      var_code #)
    ),
    version_code = case_when(
      
      
      
      version=="rush" ~"rh",
      version=="business" ~"bh",
      grepl("12_visits", version) ~ "v12",
      grepl("6_visits", version) ~ "v06",
      grepl("4_visits", version) ~ "v04",
      
      grepl("full", design) ~ "all" #, TRUE~NA
    ),
    
    model = paste(var_code, 
                  version_code, 
                  str_pad(campaign, 2, pad = "0"), 
                  sep = "_"),
    model_no = row_number())

write.csv(cw, file.path(dt_path, "onroad_model_cw.csv"), row.names = F)

##################################################################################################
model_perf <- model_perf0 %>% 
  # group_by(design, version, variable, out_of_sample, reference) %>%
  # arrange(MSE_based_R2) %>%
  # mutate(performance = row_number()) %>%
  # 
  # arrange(design, version, variable,
  #         out_of_sample, 
  #         #campaign, 
  #         performance) %>%
  # 
  # #group_by( design, version, variable, campaign) %>%
  # 
  # ungroup() %>%
  #mutate(campaign_id = row_number() ) %>%
  #ungroup() 
  left_join(select(cw, -contains(c("code", "model_no"))), by = c("campaign", "design", "version", "variable"))

select(model_perf , 
       
       # --> don't use for onroad??
       #-no_sites
       ) %>%
  saveRDS(., file.path(dt_path, "onroad_model_eval.rda"))






