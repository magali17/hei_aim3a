# script temporally adjusts onroad data

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
               # parallel, #mclapply; detectCores()
               # pls, gstat, 
               sf # UK-PLS MODEL
)    

source("functions.R")
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
# fixed site temporal adjustment (use the winsorized adjusted values, as before)
# this adjustment comes from 1.1_temporal_adjustment.Rmd
fixed_site_temp_adj <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox_temp_adjustment.rda")) %>%
  select(time, ufp_adjustment = diff_adjustment_winsorize)

# --> on-road segment-level samples; check that it has a 'time' variable 



##################################################################################################
# TEMPORAL ADJ 1: FIXED SITE
##################################################################################################

# --> on-road segment-level samples - check that it has a 'time' variable; merge to fixed_site_temp_adj dt
# mutate(value = value + ufp_adjustment)
# calcualte segment annual averages




##################################################################################################
# TEMPORAL ADJ 2: BACKGROUND VALUES
##################################################################################################
# --> and weekday vs weekend adjustment? how would a weekday bsh do this?

# using the non-stationary, use the 1st percentile of distribution of measurments duing ________ time [the hour?]
# Then once u have background air; u can estimate background annual avg & then hourly adjustments



# ????? approach? how do you adjust for overnight hours that are never meaasured in BH/RH designs? 
# does the Hankey do this w/ their "underwrite"?



