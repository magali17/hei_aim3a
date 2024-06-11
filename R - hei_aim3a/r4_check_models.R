# script QCs the road cohort predictions to check that all the models have run & for all locations
# selects models to send to KP if there are too many

# RESULTS are in: Output/v3_20230321/onroad/qc and ...onroad/predictions/kp
################################################################################
# SETUP
################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load the required libraries for: plotting, modeling, spatial features, script timing
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, parallel)

dt_path_onroad <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "onroad")
prediction_path <- file.path(dt_path_onroad, "predictions", "20240605")

# where cohort predictions are stored
prediction_folders <- list.files(file.path(dt_path_onroad, "modeling_data")) %>%
  grep("all",., value = T, invert = T) %>%
  gsub(".rda", "", .)

lapply(prediction_folders, function(f){
  if(!dir.exists(file.path(prediction_path, f))){dir.create(file.path(prediction_path, f), recursive = T)}
  })

dt_path_qc <- file.path(dt_path_onroad, "qc")
if(!dir.exists(dt_path_qc)){dir.create(dt_path_qc, recursive = T)}

# predictions ready for KP
dt_path_kp <- file.path(dt_path_onroad, "predictions", "kp", "20240605")
if(!dir.exists(dt_path_kp)){dir.create(dt_path_kp, recursive = T)}

set.seed(1)

use_cores <- 6 

# QC
run_QC <- TRUE  
testing_mode <- FALSE # TRUE if want to reduce models
override_qc_files <- FALSE # TRUE when e.g., uploading more prediction files. FALSE when QC checks have passed
override_kp_file <- TRUE # TRUE when e.g., adding different model predictions
################################################################################
# DATA
################################################################################
# ufp prediction models 
cw <- read.csv(file.path(dt_path_onroad, "onroad_model_cw.csv"))

# no. of cohort locations that should have predictions
cohort_n <- readRDS(file.path("data", "dr0357_cohort_covar_20220404_in_mm_area_prepped.rda")) %>%
  nrow()
################################################################################
# QC Predictions
################################################################################
if(run_QC==TRUE){
  
  message("running QC check")
  
  if(file.exists(file.path(dt_path_qc, "prediction_summary.rda")) &
     override_qc_files ==FALSE){
    
    message("...reading in existing file")
    
    prediction_summary <- readRDS(file.path(dt_path_qc, "prediction_summary.rda"))
    
    }else{
      message("...creating summary file")
      # predictions per file
      # f="road_type"
      prediction_summary <- lapply(prediction_folders, function(f){
        file_names <- list.files(file.path(prediction_path, f))
        
        if(testing_mode==TRUE){file_names <- file_names[1:5]}
        
        # x=file_names[1]
        mclapply(file_names, mc.cores=use_cores, function(x){
          model_name <- gsub(".rda", "", x)
          this_file <- file.path(file.path(prediction_path, f, x))
          message(paste("summarizing:", this_file))
          
          readRDS(this_file) %>%
            group_by(variable) %>%
            summarize(no_predictions = n(),
                      #models = length(unique(model)),
                      min = min(prediction),
                      mean = mean(prediction),
                      max = max(prediction),
                      NAs = sum(is.na(prediction))) %>%
            ungroup() %>%
            mutate(model=model_name,
                   rda_file_size_b = file.size(this_file),
                   #size_kb = rda_size_b/1e3,
                   #rda_size_mb = rda_size_b/1e6
                   ) %>%
            select(model, everything())
          
        }) %>%
          bind_rows()
      }) %>%
        bind_rows()
    
      message("...saving prediction_summary.rda file")
      saveRDS(prediction_summary, file.path(dt_path_qc, "prediction_summary.rda"))
  }
  
  ################################################################################
  # want to see nothing/0s for these. otherwise, delete & rerun specific model files
  
  message("check that no models/predictions are missing")
  
  ## check that no models are missing
  
  cw %>%
    group_by(design) %>%
    mutate(total_models = n()) %>%
    ungroup() %>%
    filter(model %in% prediction_summary$model) %>%
    group_by(design) %>%
    summarize(total_models = first(total_models),
              available_models = n(),
              proportion_available = available_models/total_models %>% round(2))  
  
  print("missing models:")
  cw$model[!cw$model %in% prediction_summary$model] 
  
  ##check that all models have predictions for everyone
  print("missing location predictions in available models:")
  prediction_summary$model[!prediction_summary$no_predictions == cohort_n]
  
}


################################################################################
# SELECT MODELS/COMBINE PREDICTIONS
################################################################################
message("combining design type files for KP")

# f=prediction_folders[1]
lapply(prediction_folders, function(f){
  file_names <- list.files(file.path(prediction_path, f))

  new_prediction_file <- paste0(f, ".rda")

  if(!file.exists(file.path(dt_path_kp, new_prediction_file)) |
     !file.exists(file.path(dt_path_kp, gsub(".rda", ".csv", new_prediction_file))) |
     override_kp_file ==TRUE){

    # x=file_names[1]
    predictions <- mclapply(file_names, mc.cores=use_cores, function(x){
      this_file <- file.path(file.path(prediction_path, f, x))
      message(paste("reading in:", this_file))

      readRDS(this_file) %>%
        # drop pollutant label variable for KP
        select(-variable)

    }) %>%
      bind_rows()

    # save predictions for each design type separately
    message(paste("saving predictions:", file.path(dt_path_kp, gsub(".rda", "", new_prediction_file))))
    saveRDS(predictions, file.path(dt_path_kp, new_prediction_file))
    write.csv(predictions, file.path(dt_path_kp, gsub(".rda", ".csv", new_prediction_file)), row.names = F)
  }

}) 



################################################################################
# SAVE
################################################################################
message("DONE WITH R4_check_models.R")




