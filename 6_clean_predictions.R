#---> START HERE




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

pacman::p_load(dplyr, readr, lubridate, sf)    

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

prediction_path <- file.path(dt_path, "UK Predictions", 
                             "cohort"
                             #"grid"
                             #"grid_test"#, "test", "predictions.rda"
                             )

if(!dir.exists(file.path(prediction_path, "KP"))){dir.create(file.path(prediction_path, "KP"))}

##################################################################################################
# PREP PREDICTION FILE FOR KP
##################################################################################################
# var_names = "test"

var_names <- readRDS(file.path(prediction_path, "keep_vars.rda"))

predictions0 <- lapply(var_names,
                       function(x) {
                         dt <- readRDS(file.path(prediction_path, x, "predictions.rda")) %>%
                           st_drop_geometry()
                         
                         
                         }) %>%
  bind_rows()

predictions <- predictions0 %>%
  # only predict at locations in the monitoring area w/o NAs
  filter(in_monitoring_area,
         !is.na(prediction)) %>%
  mutate(
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01 "),
    end_date = ymd("2021-07-09 "),
    model = paste0("mb_", campaign_id)
    ) %>%
  select(location_id, start_date, end_date, model, 
         variable,
         prediction) 
  
saveRDS(predictions, file.path(prediction_path, "KP", paste0("predictions_", Sys.Date(),".rda")))

predictions %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_path, "KP", paste0("predictions_", Sys.Date(),".csv")))


##################################################################################################
# save the true, all data predictions for UFP separately (e.g., for survival/other analyses)
##################################################################################################
all_data_campaign_refs <- readRDS(file.path(prediction_path, "Selected Campaigns", "all_data_campaign_refs.rda")) %>%
  select(model=model_id, variable) %>%
  filter(variable != "no2")

all_data_predictions <- predictions %>%
  filter(model %in% all_data_campaign_refs$model) %>%
  left_join(all_data_campaign_refs)

saveRDS(all_data_predictions, file.path(prediction_path, "KP", paste0("all_data_psd_predictions_", Sys.Date(),".rda")))

all_data_predictions %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_path, "KP", paste0("all_data_psd_predictions_", Sys.Date(),".csv")))






##################################################################################################
# QC check
##################################################################################################
eval <- FALSE

if(eval==TRUE){
  
  #can't view everything on excel. Looks good in R.
  df <- readRDS(file.path(prediction_path, "KP", paste0("predictions_", Sys.Date(),".csv"))) %>%
                  arrange(location_id)
                
                #df_small <- slice(df, 1:1e3)
                
                df0 <- df %>% group_by(location_id, variable) %>% summarize(n=n(), models = length(unique(model))) %>% ungroup()
                
                #dups <- c(34496703, 34496704, 34496708)
                dups <- filter(df0, n == max(n)) %>%
                  distinct(location_id) %>% pull()
                
                df1 <- filter(df, location_id %in% dups)
                
                
                ####
                cohort0 <- read.csv("../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv")
                cohort_dups <- cohort0 %>% group_by(location_id) %>% summarize(n = n()) %>% filter(n>1) %>% pull(location_id)
                
                cohort <- filter(cohort0, location_id %in% dups) %>%
                  arrange(location_id)
                
                }





