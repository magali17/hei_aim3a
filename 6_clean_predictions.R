
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

pacman::p_load(#tidyverse
  dplyr, lubridate
  )    

set.seed(1)

prediction_path <- file.path("Output", "UK Predictions", "grid")

if(!dir.exists(file.path(prediction_path, "KP"))){dir.create(file.path(prediction_path, "KP"))}

##################################################################################################
#  
##################################################################################################

# my_text <- str('rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_no2_.rda data/dr0311_grid_covars.rda Output/"UK Predictions"/grid/no2 csv', )


var_names <- readRDS(file.path("Output", "keep_vars.rda"))
grp1_vars <- c("no2", "ns_total_conc")
grp2_vars <- setdiff(var_names, grp1_vars)



predictions0 <- lapply(var_names, #c("no2", "ns_total_conc"), 
                       function(x) {
  read_csv(file.path(prediction_path, x, "predictions.csv"))
  }) %>%
  bind_rows() 

predictions <- predictions0 %>%
  mutate(
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01 "),
    end_date = ymd("2021-07-09 "),
    model = paste0("mb_", campaign_id)
    ) %>%
  select(location_id, start_date, end_date, model, prediction)
  

#write.csv(predictions, file.path(prediction_path, "KP", "predictions_no2_total_pnc.csv"), row.names = F)
#write_csv(predictions, file.path(prediction_path, "KP", "predictions_no2_total_pnc.csv.gz"))

#
filter(predictions, variable %in% grp1_vars) %>%
  write_csv(., file.path(prediction_path, "KP", "predictions_grp1.csv"))

filter(predictions, variable %in% grp2_vars) %>%
  write_csv(., file.path(prediction_path, "KP", "predictions_grp2.csv"))


  