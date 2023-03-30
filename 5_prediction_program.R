
# user_arguments <- c(file.path("Output", "Selected Campaigns", "site_data_for_ns_10_100.rda"), 
#                     file.path("data", "dr0311_grid_covars.rda"), 
#                     "Output/UK Predictions/grid_test/test", 
#                     "rda")

################################################################################
# ABOUT THIS SCRIPT
################################################################################
# AUTHOR, DATE: 
# Magali Blanco, 1/09/2023

# OBJECTIVE
# This program takes in a dataset with locations and their respective geographic covariates, and it returns a dataset with annual average air pollution predictions at those locations.

# INPUTS
# 4 inputs are required: 
#   1. the file path for the geocovariate dataset where predictions are desired
#   2. the repository where prediction files should be saved
#   3. the desired prediction file format, either csv or rda 

# OUTPUTS
# The output is a dataset with annual average air pollution predictions for the locations with geographic covariates. 
# Plots and maps to summarize the resulting predictions are also included.

# ERROR/WARNING MESSAGES
# Error messages occur if:
#   1. three arguments are not included when running this program
#   2. there are locations with missing covariate values or missing covariates altogether that are required for the prediction models to run

# EXAMPLE OF HOW TO USE THIS PROGRAM
# in a terminal open to the R program project directory, type: rscript 5_prediction_program.R  <modeling_data_path> <covariate_file_path> <prediction_directory> <prediction_file_format>
## rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_total_conc_.rda data/dr0311_grid_covars.rda Output/"UK Predictions"/grid/ns_total_conc csv


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
pacman::p_load(tidyverse, pls, tools, parallel,
               gstat, sf
               #ggpubr, #ggspatial, 
               )

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

set.seed(1)
###########################################################################################
# TAKE IN USER ARGUMENTS 
###########################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)
# user_arguments <- c(file.path("Output", "Selected Campaigns", "site_data_for_all_selected_campaigns.rda"), "../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv", "Output/UK Predictions/cohort/psd_and_no2", "rda")

if (length(user_arguments) !=4) {
  print("Usage error. Enter: 1. the location of the covariate dataset for which you would like predictions, 2. where the prediction outputs should be saved, and 3. the desired prediction file fomat (csv or rda). Usage:")
  print("rscript 5_prediction_program.R <modeling_data_path> <covariate_file_path> <prediction_directory> <prediction_file_format>")
  stop()
}

# new covariate file
modeling_data_path <- user_arguments[1]
covariate_file_path <- user_arguments[2]
cov_ext <- tools::file_ext(covariate_file_path)

#where predictions should be saved
prediction_directory <- user_arguments[3]
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = T)}

# the prediction file format (e.g., 'rda')
prediction_file_format <- tolower(user_arguments[4])

###########################################################################################
# SAVE USER TERMINAL INPUTS
###########################################################################################
write.table(paste("Rstudio 5_prediction_program.R", paste(user_arguments, collapse = " ")), 
            file = file.path(prediction_directory, "user_arguments.txt"), row.names = F, col.names = F, quote = F)

###########################################################################################
# UPLOAD THE NEW DATASET WHERE PREDICTIONS ARE DESIRED
###########################################################################################
if(cov_ext == "rda") {dt0 <- readRDS(covariate_file_path)}
if(cov_ext == "csv") {dt0 <- read_csv(covariate_file_path)}

if(!cov_ext %in% c("csv", "rda")) {stop("Error. Covariate file must be a CSV or RDA file")}

###########################################################################################
# UPLOAD MODELING DATA
###########################################################################################
modeling_data <- read_rds(modeling_data_path)

# --> CHECK THAT THIS WORKS W/ MOBILE DATA AS EXPECTED

# # the covariate names that will be used in the model
cov_names <- select(modeling_data, log_m_to_a1:last_col()) %>%
  select(-geometry) %>%
  names()

# load a spatial file of the original monitoring area to assess spatial extrapolation later
monitoring_area <- readRDS(file.path("data", "GIS", "monitoring_land_zero_water_shp.rda"))  
lat_long_crs <- 4326

var_names <- readRDS(file.path(dt_path, "keep_vars.rda"))

###########################################################################################
# Universal Kriging - Partial Least Squares Model function

# desired PLS components to use (from a different script): 3
pls_comp_n <- read_rds(file.path(dt_path, "pls_comp_n.rda")) 

#prediction model
uk_pls <- readRDS(file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

###########################################################################################
# GENERATE NEW COVARIATES FOR THE DATASET
###########################################################################################
# created some new proximity variables
# log transform land proximity variables (e.g., distance to roadways)

combine_a23_ll <- function(df) {
  #find buffers for a2-3 length variables
  buffers <- str_subset(names(df), "ll_a[2:3]") %>% str_extract("s[0:9].*")

  #for each buffer, calculate sum of a2+a3 length
  for (i in seq_along(buffers)) {
    old_vars <- paste0(c("ll_a2_", "ll_a3_"), buffers[i])
    new_var <- paste0("ll_a23_", buffers[i])

    df[new_var] <- apply(df[old_vars], 1, sum)
  }
  return(df)
}

generate_new_vars <- function(df) {
  # for the NO2 covariate, use the average levels from several available years
  no2_behr_vars <- c("no2_behr_2005","no2_behr_2006", "no2_behr_2007")

  df <- df %>%
    rowwise() %>%
    mutate(m_to_a123 = min(m_to_a1, m_to_a2, m_to_a3),
           m_to_a23 = min(m_to_a2, m_to_a3),
           no2_behr = mean(!!as.symbol(no2_behr_vars))
    ) %>%
    ungroup() %>%
    #make min distance 1 m before log transforming
    mutate_at(vars(starts_with("m_to_")), ~ifelse(.==0, 1, .) %>% log(.)) %>%
    rename_at(vars(starts_with("m_to_")), ~gsub("m_to_", "log_m_to_", .)) %>%
    # calculate sum of a2 and a3 roads in each buffer
    combine_a23_ll()
}

###########################################################################################
dt <- dt0 %>%
  # don't need to do this here b/c modeling covariates already exist 
  generate_new_vars() %>%
  
  st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs, remove=F)

###########################################################################################
# ADD LOCATION INDICATORS
###########################################################################################
# add indicators of whether or not the prediction locations are in the monitoring area
dt$in_monitoring_area <- suppressMessages(
  dt %>%
    #st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs) %>%
    st_intersects(., monitoring_area, sparse = F) %>%
    apply(., 1, any)
)

###########################################################################################
# QC CHECKS
###########################################################################################
# 1. ensure that the new dataset has all the necessary geograpahic covariates for modeling, otherwise give an error message with the missing covariates
has_all_covariates <- all(cov_names %in% names(dt))

if(has_all_covariates==FALSE) {
  missing_cov <- cov_names[!cov_names %in% names(dt)]
  error_msg <- paste("The following covariates are needed but missing from the dataset:", paste(missing_cov, collapse = ", "), ". Please fix this before continuing.")
  stop(error_msg)
}

# 2. check that there are no missing covariate values for any location
has_missing_values <- sapply(dt[cov_names], function(x) any(is.na(x) )) %>% 
  as.data.frame() %>% rownames_to_column() %>%
  rename(true_or_false = '.')  

if(any(has_missing_values$true_or_false) == TRUE) {
  covariates_with_missingness <- filter(has_missing_values, true_or_false==TRUE) %>%
    pull(rowname)
  
  error_msg <- paste("The following covariates have 1+ rows with missing values:", paste(covariates_with_missingness, collapse = ", "), ". These rows will have missing predictions.")
  
  print(error_msg)
  #stop(error_msg)
}

# 3. print a 'pass' message if all of the covariates are present and there are no locations with missing values
if(has_all_covariates ==TRUE & any(has_missing_values$.) == FALSE) {print("Covariate checks passed.")} 

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
print("Generating predictions...")

#x = group_split(modeling_data, model, variable)[[1]]
#temp <- filter(modeling_data, model <=66) 

new_predictions0 <- mclapply(
  group_split(modeling_data, 
              #temp,
              model, variable),
                             mc.cores = 10, #1,# 4,
                             function(x) {
                               temp <- dt %>%
                                 mutate(model = first(x$model),
                                        variable = first(x$variable)) %>%
                                 uk_pls(new_data = ., modeling_data = x)
                               }) %>%
  bind_rows()  

# save the location and prediction information
new_predictions <- new_predictions0 %>%
  select(location_id,
         latitude, longitude, in_monitoring_area, model, variable, prediction) %>%
  mutate(
    prediction = exp(prediction),
    variable = factor(variable, levels = var_names)
  ) %>%
  arrange(variable)


###########################################################################################
# SAVE THE PREDICTIONS
###########################################################################################
prediction_file_name <- file.path(prediction_directory, paste0("predictions.", prediction_file_format))

if(prediction_file_format == "csv") {write.csv(new_predictions, prediction_file_name, row.names = F)}
if(prediction_file_format == "rda") {saveRDS(new_predictions, prediction_file_name)}

print(paste0("Predictions saved: ", prediction_file_name))

print("Program done.")

###########################################################################################
