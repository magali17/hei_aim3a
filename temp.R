# combine multiple prediction files into one

pacman::p_load(dplyr)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

prediction_directory <- file.path(dt_path, "UK Predictions", "cohort", "other designs")
model_designs <- c("fewhrs", 
                   paste0("sitetype_", c("no2", "ns_10_100", "ns_total_conc", "pnc_noscreen")),
                   paste0("balsea_", 1:4)  
)


message("reading in individual files")
predictions <- lapply(model_designs, function(x) {
  readRDS(file.path(prediction_directory, x, paste0("predictions_", Sys.Date(), ".rda")))
}) %>%
  bind_rows() %>%
  select(-variable)

message("saving compiled predictions")
write.csv(predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_", Sys.Date(), ".csv")), row.names = F)
saveRDS(predictions, file.path(dt_path, "UK Predictions", "cohort", "KP", paste0("other_design_predictions_", Sys.Date(), ".csv")))


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

 
message("done with script")
