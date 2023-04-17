# put predictions back on native scale
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
prediction_directory <- file.path(dt_path, "UK Predictions", "cohort", "onroad_pnc_noscreen")

new_directory <-file.path(prediction_directory, "fixed") 
if(!dir.exists(new_directory)){dir.create(new_directory)}

files <- c("onroad_predictions__SP_FALSE_ADJ_FALSE_2023-04-15",
            "onroad_predictions__SP_FALSE_ADJ_TRUE_2023-04-15",
            "onroad_predictions__SP_TRUE_ADJ_FALSE_2023-04-16",
            "onroad_predictions__SP_TRUE_ADJ_TRUE_2023-04-16") 


# p_name <- files[1]
# read in predictions
lapply(files, function(p_name) { 
  readRDS(file.path(prediction_directory, paste0(p_name, ".rda"))) %>%
    mutate(prediction = exp(prediction)) %>%
    saveRDS(., file.path(new_directory, paste0(p_name, ".rda")))
  
  read.csv(file.path(prediction_directory, paste0(p_name, ".csv"))) %>%
    mutate(prediction = exp(prediction)) %>%
    write.csv(., file.path(new_directory, paste0(p_name, ".csv")))
  })

 



# ? bind together?