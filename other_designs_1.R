
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

pacman::p_load(tidyverse, sf,
               parallel, #mclapply; detectCores()
               # future.apply, #future_replicate()
               pls, #UK 
               gstat #variogram()
)    

#source("functions.R")
set.seed(1)

#plan(multisession, workers = 6)
use_cores <- 2

latest_version <- "v3_20230321" 

dt_path <- file.path("Output", latest_version)

#takes long to run full script, dont always ned
run_cv <- FALSE

##################################################################################################
# LOAD DATA
##################################################################################################
temporal <- readRDS(file.path(dt_path, "temporal_adjustment.rda")) %>%
  mutate(visits = 12) %>%
  pivot_longer(cols = ns_total_conc, names_to = "variable", values_to = "value")

keep_vars2 <- c("ns_total_conc", "ns_10_100", "pnc_noscreen", "no2")

site_type <- readRDS(file.path(dt_path, "annual_site_type.rda")) %>%
  select(design, version, campaign, location, visits, all_of(keep_vars2)) %>%  
  pivot_longer(cols = c(all_of(keep_vars2)), names_to = "variable", values_to = "value")

#add updated/new season samples
keep_vars <- readRDS(file.path(dt_path, "keep_vars.rda"))

season <- readRDS(file.path(dt_path, "fixed_season_annual.rda")) %>%
  mutate(visits = 12) %>%
  select(design, version, campaign, location, visits, all_of(keep_vars)) %>%  
  pivot_longer(cols = c(all_of(keep_vars)), names_to = "variable", values_to = "value")


dt <- bind_rows(temporal, site_type) %>%
  bind_rows(season)

cov <-readRDS(file.path("data", "stop_modeling_covars.rda"))
cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# true annual avg estimates
true_annual <- readRDS(file.path(dt_path, "all_data_annual_estimates.rda")) %>%
  select(location, all_of(keep_vars#keep_vars2
                          )) %>%
  pivot_longer(cols = c(all_of(keep_vars
                               #keep_vars2
                               )), names_to = "variable", values_to = "gs_estimate")
        
##################################################################################################
# VARIABLES
##################################################################################################
project_crs <- 4326  #lat/long
m_crs <- 32148

cov_names <- read_rds(file.path(dt_path, "cov_names.rda")) 
pls_comp_n <- read_rds(file.path(dt_path, "pls_comp_n.rda")) #2

#k-folds for CV
k <- 5  
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################
message ("creating model crosswalks")

create_new_cw <- TRUE

if(create_new_cw == TRUE) {
  
  cw <- dt %>% 
    distinct(design, version, campaign, 
             variable #? 
             ) %>%  
    arrange(design, version,campaign, 
            variable #?
            ) %>%  
    mutate(
      var_code = case_when( 
        variable=="ns_total_conc" ~ "nstot",
        variable=="ns_10_100" ~ "ns10100",
        variable=="pnc_noscreen" ~ "pncnoscreen",
        variable=="no2" ~ "no2",
        TRUE ~variable
        ),
      var_code = gsub("ns_", "ns", var_code),
      var_code = gsub("\\..", "", var_code),
      
      design_code = case_when( 
        design=="fewer hours" ~ "fewhrs",
        design=="site type" ~ "sitetype",
        design=="balanced seasons" ~ "balsea",
        ),
      # # note, this replicates version #. should be (or something like this):
      # design_code = ifelse(design=="balanced seasons", "s", design_code),
      # #design_code = ifelse(design=="balanced seasons", paste0("s", version), design_code),
      
      version_code = case_when(
        version == "business" ~ "bh",
        version == "business temp adj" ~ "bhadj",
        version == "rush" ~ "rh",
        version == "rush temp adj" ~ "rhadj",
        #TRUE ~ gsub(" ", "", version) #%>% str_to_lower()
        TRUE ~ version
        ),
      version_code = gsub(" ", "", version_code),
      
      model = paste("s",
                    var_code,
                    design_code,
                    version_code,
                    str_pad(campaign, 2, pad = "0"), 
                    sep = "_"),
      model_no = row_number())
  
  write.csv(cw, file.path(dt_path, "other_designs_model_cw.csv"), row.names = F)
  } else {
    cw <- file.path(dt_path, "other_designs_model_cw.csv")
    }
  
dt <- left_join(dt, cw) %>%
  select(location, value, model, model_no, variable, visits) %>%
  arrange(model_no) %>%
  left_join(cov) %>%
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) %>%
  
  # --> ? drop a few sites w/ NaN log values 
  drop_na(value) %>%
  
  mutate(value = log(value))
  
# # summarize visits. # looks OK
# # lowest no. of total visits remaining is 3704 (vs 3708), so total number of stops for a given campaign are not impacted. so 2 sites are dropped in these models.
# dt %>%
#   group_by(model) %>%
#   summarize(total_visits = sum(visits)) %>% 
#   pull(total_visits) %>%
#   summary()

message("saving modeling data")
saveRDS(dt, file.path(dt_path, "Selected Campaigns", "other_stop_designs_data.rda"))
  
# ##################################################################################################
# # CV 
# ##################################################################################################
# # function returns cross-valited predictions for a given dataset
# do_cv <- function (x, fold_name) {
#   
#   print(first(x$model))
#   
#   #code to make sure this fn works even if folds don't have all numbers in a sequence (e.g., if too few sites)
#   k = sort(unique(x[[fold_name]]))
#   
#   df <- data.frame()
#   
#   for(f in k) {
#     modeling_data0 = filter(x, !!as.symbol(fold_name) != f)
#     new_data0 = filter(x, !!as.symbol(fold_name) == f)
#     
#     temp <- uk_pls(modeling_data = modeling_data0, new_data = new_data0) %>% st_drop_geometry() 
#     df <- rbind(df, temp)
#   }
#   return(df)
# }
# 
# ##################################################################################################
# #add CV folds
# message("generating random folds")
# 
# random_fold_df <- lapply(group_split(dt, model), function(x) {
#   distinct(x, location, model) %>%
#     mutate(random_fold = sample(1:k, size = nrow(.), replace = T ))
#   }) %>%
#   bind_rows()
# 
# dt <- suppressMessages(left_join(dt, random_fold_df)) %>%
#   select(random_fold, everything())
# 
# # saveRDS(dt, file.path(dt_path, "other_stop_designs_data2.rda"))
# 
# ## CV
# print(paste0("Running random ", k, " FCV"))
# 
# cv_predictions0 <- lapply(group_split(dt, model),
#                             #mc.cores = 1, #use_cores,
#                             FUN = do_cv, fold_name = "random_fold") %>%
#   bind_rows()
# 
# saveRDS(cv_predictions0, file.path(dt_path, "UK Predictions", "TEMP_other_design_cv_predictions.rda"))
# 
# ##################################################################################################
# # COMBINE PREDICTIONS & BEST ESTIMATES
# ##################################################################################################
# message("combining predictions with estimates")
# 
# predictions <- cv_predictions0 %>% 
#   select(model, location, value, prediction) %>%
#   rename(campaign_estimate = value) %>%
#   mutate(out_of_sample = "CV") %>% 
#   #drop_na(prediction) %>%
#   left_join(select(cw, model, variable)) %>%
#   left_join(true_annual) %>% 
#   #put back on native scale before evaluating
#   mutate_at(#vars(contains("estimate"), 
#                  vars(campaign_estimate, prediction), ~exp(.)) 
# 
# message("saving predictions")
# saveRDS(predictions, file.path(dt_path, "UK Predictions", "other_design_predictions.rda"))
# 
# 
# ##################################################################################################
# # CV STATS FUNCTION
# ##################################################################################################
# message("calculating performance statistics")
# 
# validation_stats <- function(dt, prediction, reference){
#   
#   # MSE of predictions
#   MSE_pred <- mean((dt[[reference]] - dt[[prediction]])^2)
#   # MSE of observations (for R2 denominator)
#   MSE_obs <- mean((dt[[reference]] - mean(dt[[reference]]))^2)
#   
#   RMSE = sqrt(MSE_pred)
#   MSE_based_R2 = max(1 - MSE_pred/MSE_obs, 0)
#   # alternative gives same mse-based R2
#   # caret::R2(pred = dt$prediction,obs =dt$estimate, form = "traditional")
#   
#   result <- distinct(dt, model, out_of_sample# , reference
#   ) %>%
#     mutate(
#       no_sites = nrow(dt),
#       RMSE = RMSE,
#       MSE_based_R2 = MSE_based_R2
#     )
#   return(result)
# }
# 
# 
# 
# model_perf0 <- mclapply(group_split(predictions, model, out_of_sample), 
#                         mc.cores = use_cores,
#                         validation_stats, prediction = "prediction", reference = "gs_estimate") %>%
#   bind_rows()
# 
# message("saving model evaluation statistics")
# 
# model_perf0 %>%
#   saveRDS(., file.path(dt_path, "other_designs_model_eval.rda"))
# 
# 
# 
# ##################################################################################################
# # APPENDIX 
# ##################################################################################################
# if(FALSE) {
#   # version levels
#   visit_count <- seq(2,22, 2)
#   fewer_hr_lvls <- unique(cw$version)[str_detect(unique(cw$version), "business|rush")]
#   site_type_lvls <- paste("H", rev(visit_count), "L", visit_count)
#   
#   temp <- model_perf0 %>%
#     left_join(cw) %>%  
#     mutate(version = factor(version, levels=c(fewer_hr_lvls, site_type_lvls))) %>%
#     filter(variable == "ns_total_conc") 
#   
#   
#   temp %>%
#     pivot_longer(cols = c(MSE_based_R2, RMSE)) %>%  
#     
#     ggplot(aes(x=version, y=value)) + 
#     facet_grid(name~design, scales = "free", switch = "both", space = "free_x") + 
#     geom_boxplot() + 
#     labs(title = "Distribution of UK-PLS campaign performances", 
#          subtitle = "30 campaigs per version" #, each model performance is based on  309 sites
#          )
#   
#   ggsave(file.path("..", "Manuscript", "Images", "v3_20230321", "other", "other_designs_model_performances.png"), width = 14, height = 8)
#   }
# 
message("done with other_designs_1.R")
