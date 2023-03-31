# script takes annual averages Annie generated and generates CV predictions, evaluates each model, and crates a model crosswalk

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
## 5884 locations used. does not include all necessary covariates used (e.g. pop10, bus)
road_locations_used <- readRDS(file.path("data", "onroad", "annie", "cov_onroad_preprocessed.rds")) %>%
  pull(native_id)

cov <- read.csv(file.path("data", "onroad", "dr0364d_20230331.txt")) %>%
  filter(native_id %in% road_locations_used) %>%
  mutate(native_id = as.character(native_id),
         location = substr(native_id, nchar(native_id)-3, nchar(native_id)),
         location = as.numeric(location)) %>%
  generate_new_vars() %>%
  select(location, latitude, longitude, all_of(cov_names))
  
## 5874 locations
onroad_ns <- readRDS(file.path("data", "onroad", "annie", "PNC_nonspatial_annavgs.rds")) %>%
  mutate(spatial_code = "sn")
onroad_s <- readRDS(file.path("data", "onroad", "annie", "PNC_spatial_annavgs.rds")) %>%
  mutate(spatial_code = "sy")
onroad0 <- rbind(onroad_ns, onroad_s) %>%
  rename(location=id,
         value = annual_mean) %>%
  # log transform pollutant concentrations before modeling
  mutate(value = ifelse(value== 0, 1, value),
         value = log(value),
         variable = "pnc_noscreen"
         ) 
   
### TEST 
#distinct(onroad_ns, design, version, visits, adjusted)
# distinct(onroad_s, design, visits, version, adjusted )

#rm(onroad_ns, onroad_s)

# stationary data; for out-of-sample validation
stationary <- filter(annual,
                     design=="full",
                     variable=="pnc_noscreen")

##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################
cw <- onroad0 %>% 
  distinct(spatial_code, design, version, visits, campaign, adjusted) %>%  
  arrange(spatial_code, design, version, visits, campaign, adjusted) %>%  
  mutate(
    design_code = case_when( 
      design=="balanced" ~ "baly",
      design=="unbalanced" ~ "baln",
      design=="clustered" ~ "clus",
      design=="sensible spatial" ~ "seny",
      design=="unsensible spatial" ~ "senn",
      design=="road type" ~ "road"
      ),
    
    version_code = case_when(
      grepl("all", version) ~ "al",
      grepl("business", version) ~ "bh"),
    
    visit_code = readr::parse_number(visits),
    visit_code = str_pad(visit_code, 2, pad = "0"), 
    visit_code = paste0("v", visit_code),
    
    adjusted_code = ifelse(adjusted=="adjusted", "adjy", "adjn"),
    
    model = paste("r", 
                  spatial_code,
                  design_code,
                  version_code,
                  visit_code,
                  adjusted_code,
                  str_pad(campaign, 2, pad = "0"), 
                  sep = "_"),
    model_no = row_number())

# View(cw %>% filter(campaign==1))

write.csv(cw, file.path(dt_path, "onroad_model_cw.csv"), row.names = F)

onroad1 <- left_join(onroad0, cw) %>%
  select(location, value, model, variable) %>% 
  left_join(cov) 

onroad <- onroad1 %>% 
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) 

saveRDS(onroad, file.path(dt_path, "Selected Campaigns", "onroad_modeling_data.rda"))
##################################################################################################
# STANDARD CROSS-VALIDATION
##################################################################################################
# don't do CV b/c care about performance at more representative stationary locations (below)

# #add CV folds 
# message("generating random folds")
# 
# random_fold_df <- lapply(group_split(onroad, model), function(x) {
#   distinct(x, location, model) %>%
#     mutate(random_fold = sample(1:k,size = nrow(.), replace = T ))
#   }) %>%
#   bind_rows()
# 
# onroad <- suppressMessages(left_join(onroad, random_fold_df)) %>%
#   select(random_fold, everything())
# 
# saveRDS(annual, file.path(dt_path, "onroad_training_set2.rda"))
# 
# ## CV
# print(paste0("Running random ", k, " FCV"))
# 
# cv_predictions0 <- mclapply(group_split(onroad, model), 
#                             mc.cores = use_cores, 
#                             FUN = do_cv, fold_name = "random_fold") %>%
#   bind_rows() 

# --> NEED TO exp(value) if use these predictions

##################################################################################################
# OUT-OF-SAMPLE VALIDATION AT 309 STOP LOCATIONS
##################################################################################################

# --> ERROR: NEED POP10 VARIABLES TO WORK

# x = group_split(onroad, model)[[1]]
stationary_predictions <- mclapply(group_split(onroad, model), mc.cores = 1,#use_cores, 
                                   function(x) {
  
  uk_pls(modeling_data = x, new_data = stationary) %>%
    #fn has binding issues later if don't drop geom 
     st_drop_geometry() %>%
     #add info to new dataset about the prediction model
     mutate(model = first(x$model))
                                     }) %>%
  bind_rows()  



##################################################################################################
# COMBINE PREDICTIONS; FORMAT DF 
##################################################################################################
predictions <- stationary_predictions %>% 
  #select(all_of(common_vars)) %>%
  select(location, prediction, model) %>%
  mutate(out_of_sample = "Stationary Sites") %>%
  drop_na(prediction)

annual_gs_estimates <- stationary %>% st_drop_geometry() %>%
  select(location, gs_estimate = value) 
   
# # estimates from specific campaign simultaions (n=278 sites)
# campaign_estimates <- onroad %>% #st_drop_geometry() %>%
#   distinct(location, value, model) %>% 
#   rename(campaign_estimate = value)

predictions <- predictions %>%
  #left join b/c locations w/ predictions may be fewer than the 309 sites if dno't do 10FCV
  left_join(annual_gs_estimates) #%>%
  #left_join(campaign_estimates) %>%
  #put back on native scale before evaluating
  mutate_at(vars(contains("estimate"), prediction), ~exp(.)) 

print("saving predictions")
saveRDS(predictions, file.path(dt_path, "UK Predictions", "onroad_predictions.rda"))

##################################################################################################
# CV STATS FUNCTION
##################################################################################################
validation_stats <- readRDS(file.path(dt_path, "validation_stats_fn.rda"))

message("calculating performance statistics")

model_perf0 <- mclapply(group_split(predictions, model, out_of_sample), 
                        mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "gs_estimate") %>%
  bind_rows()

##################################################################################################
# model_perf <- model_perf0 %>% 
# 
#   left_join(select(cw, -contains(c("code", "model_no"))), by = c("campaign", "design", "version", "variable"))
# 
# select(model_perf , 
#        
#        # --> don't use for onroad??
#        #-no_sites
#        ) %>%
#   saveRDS(., file.path(dt_path, "onroad_model_eval.rda"))

model_perf0 %>%
  saveRDS(., file.path(dt_path, "onroad_model_eval.rda"))




