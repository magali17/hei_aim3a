# combine multiple prediction files into one
##################################################################################################
# SETUP
##################################################################################################
pacman::p_load(tidyverse)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
prediction_directory <- file.path(dt_path, "UK Predictions", "cohort", "other designs")
kp_directory <- file.path(dt_path, "UK Predictions", "cohort", "KP")

source("functions.R")


##################################################################################################
# 2024-05-16: roadside paper - fixed the temporal adjustment 1 (UTM hour merging issue); added p-trak adjusted
# --> onroad paper - 
##################################################################################################
# fixed the temporal adjustment 1 (UTM hour merging issue); added p-trak temporally adjusted
## removed original 2nd temporal adj. decided not to apply the UW approach b/c its currently based on ptrak data AND from 'onroad' data which we have said we don't trust as much
temp_adj_predictions <- readRDS(file.path(prediction_directory, "temp_adj", "predictions_2024-05-16.rda"))

# added temporal adjustment (had to rerun & save BH), updated the clusters, added route-based sampling
onroad_predictions <- readRDS(file.path(dt_path, "UK Predictions", "cohort", "onroad_pnc_noscreen", 
                                        # --> TO DO
                                        #"predictions_2024-05-16.rda"
                                        ))

predictions_20240520 <- rbind(temp_adj_predictions, onroad_predictions)

saveRDS(predictions_20240520, file.path(kp_directory, paste0("fewhrs_", Sys.Date(), ".rda")))
write.csv(predictions_20240520, file.path(kp_directory, paste0("fewhrs_", Sys.Date(), ".csv")))

##################################################################################################
# 2024-03-14: fixed the temporal adjustment (main effect), added 2nd temporal adjustment 
##################################################################################################

# save file elsewhere and as a CSV for KP
fewhrs_predictions <- readRDS(file.path(prediction_directory, "fewhrs", "predictions_2024-03-14.rda"))
saveRDS(fewhrs_predictions, file.path(kp_directory, paste0("fewhrs_", Sys.Date(), ".rda")))
write.csv(fewhrs_predictions, file.path(kp_directory, paste0("fewhrs_", Sys.Date(), ".csv")))


# # where cohort predictions are saved
# onrod_dt <- file.path(dt_path, "UK Predictions", "cohort", "onroad")
# if(!dir.exists(onrod_dt)){dir.create(onrod_dt)}
# 
# # --> UPDATE
# 
# onraod_cw <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda")) %>%
#   distinct(design_code) %>% pull(design_code)
# 
# onroad_files <- list.files(onrod_dt) %>%
#   # don't want old files
#   str_subset(#"_SP_", negate = T
#               paste(onraod_cw, collapse="|")
#              )
# 
# lapply(onroad_files, function(x) {
#   temp <- readRDS(file.path(onrod_dt, x)) %>%
#     clean_predictions()
# 
#   new_file_name <- gsub("onroad_predictions_|.rda|.csv","", x)
# 
#   saveRDS(temp, file.path(kp_directory, "onroad", paste0(x,"_", Sys.Date(), ".rda")))
#   write.csv(temp, file.path(kp_directory, paste0("fewhrs_", Sys.Date(), ".csv")))
# 
# })











##################################################################################################
# 2023-12-20: temp adjustment & re-did seasons
##################################################################################################
file_date <- "2023-12-20" # Sys.Date()

model_designs <- c("fewhrs", 
                   paste0("sitetype_", c("no2", "ns_10_100", "ns_total_conc", "pnc_noscreen")),
                   paste0("balsea_", 1:4)  
)


message("reading in individual files")
predictions <- lapply(model_designs, function(x) {
  readRDS(file.path(prediction_directory, x, paste0("predictions_", file_date, ".rda")))
}) %>%
  bind_rows() %>%
  select(-variable)

message("saving compiled predictions")
write.csv(predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_", file_date, ".csv")), row.names = F)
saveRDS(predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_", file_date, ".rda")))

# save a smaller file with only the predictions to be updated - temporally adjusted BH & RH designs
temp_adj_predictions <- predictions %>%
  filter(grepl("bhadj|rhadj", model)) 
write.csv(temp_adj_predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_temp_adj_", file_date, ".csv")), row.names = F)
saveRDS(temp_adj_predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_temp_adj_", file_date, ".rda")))

# unique(temp_adj_predictions$model)


# dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
# prediction_directory <- file.path(dt_path, "UK Predictions", "cohort", "other designs" #"onroad_pnc_noscreen"
#                                   )
# 
# new_directory <-file.path(prediction_directory, "fixed") 
# if(!dir.exists(new_directory)){dir.create(new_directory)}
# 
# files <- c("onroad_predictions__SP_FALSE_ADJ_FALSE_2023-04-15",
#             "onroad_predictions__SP_FALSE_ADJ_TRUE_2023-04-15",
#             "onroad_predictions__SP_TRUE_ADJ_FALSE_2023-04-16",
#             "onroad_predictions__SP_TRUE_ADJ_TRUE_2023-04-16") 
# 
# 
# # p_name <- files[1]
# # read in predictions
# lapply(files, function(p_name) { 
#   message(p_name)
#   readRDS(file.path(prediction_directory, paste0(p_name, ".rda"))) %>%
#     mutate(prediction = exp(prediction)) %>%
#     saveRDS(., file.path(new_directory, paste0(p_name, ".rda")))
#   
#   read.csv(file.path(prediction_directory, paste0(p_name, ".csv"))) %>%
#     mutate(prediction = exp(prediction)) %>%
#     write.csv(., file.path(new_directory, paste0(p_name, ".csv")))
#   
#   message("saved")
#   })


##################################################################################################
# QC: check that the RDA and CSV files identical
##################################################################################################
if(qc==FALSE) {
  predictions_csv2 <- read.csv(file.path(dt_path, "UK Predictions", "cohort", "KP", "other_design_predictions_2023-12-20.csv"))
  predictions_rda2 <- readRDS(file.path(dt_path, "UK Predictions", "cohort", "KP", "other_design_predictions_2023-12-20.rda")) %>% as.data.frame()
  
  # check that the files are exactly the same
  same_files <- all(predictions_csv2$prediction==predictions_rda2$prediction) &
    all(predictions_csv2$model==predictions_rda2$model) &
    all(predictions_csv2$model==predictions_rda2$model)
  
  if (same_files) {
    message("Check PASSED: the CSV and RDA file predictions are the same")
  } else{
    message("Check FAILED: the CSV and RDA file predictions are NOT the same")}
  
  
  # summary of rows, models, locations
  csv_summary <- predictions_csv2 %>%
    summarize(
      file = "csv",
      rows = n(),
      models = length(unique(model)),
      locations = length(unique(location_id)))
  
  rda_summary <- predictions_rda2 %>% 
    as.data.frame() %>%
    summarize(
      file = "rda",
      rows = n(),
      models = length(unique(model)),
      locations = length(unique(location_id)))
  
  rbind(csv_summary, rda_summary)
}



message("done with script")
