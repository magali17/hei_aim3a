
######################################################################
# SETUP
######################################################################
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, kableExtra)    

set.seed(1)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

output_data_path <- file.path(dt_path, "error")
if(!file.exists(output_data_path)) {dir.create(output_data_path, recursive = T)}

######################################################################
# LOAD DATA
######################################################################
# # outcome & covariate data
cs <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis_error.rda"))


# --> load site estimates, predictions....



######################################################################
# PREP DATA
######################################################################



