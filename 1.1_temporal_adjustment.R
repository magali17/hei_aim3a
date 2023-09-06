
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

pacman::p_load(tidyverse,
               parallel, #mclapply; detectCores()
               future.apply, #future_replicate()
               lubridate # %within%
)    

source("functions.R")
set.seed(1)

## for future.apply::future_replicate()  
# availableCores() #8 
#plan(multisession, workers = 6)

latest_version <- "v3_20230321" 
dt_path <- file.path("Output", latest_version)

##################################################################################################
# LOAD DATA
##################################################################################################
# NO2 data is avaialble at 2 sites, whereas BC data is only available at 10W.  
# only hourly (not minute-level) data are available through the PSCAA Air Data website (https://secure.pscleanair.org/airgraphing)
# 2019 report: https://pscleanair.gov/DocumentCenter/View/4164/Air-Quality-Data-Summary-2019 
# study dates: "2019-02-22" "2020-03-17"

nox <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox.rda")) %>%
  mutate(
    time = paste(date_local, time_local),
    time = ymd_hm(time, tz="America/Los_Angeles")) #%>%
#select(contains(c("local", "time"))) %>% View()

hrs <- readRDS(file.path(dt_path, "campaign visit samples", "fewer_hours.rda"))
seasons <- readRDS(file.path(dt_path, "campaign visit samples", "fewer_seasons.rda"))
   

##################################################################################################
# USE REFERENCE SITE TO GENERATE TEMPORAL ADJUSTMENT FACTORS
##################################################################################################




# --> check that this approach is OK
bh_annual_avg <- mean(bh_ref$no2_ref, na.rm = T)

bh_ref <-bh_ref %>%
  mutate(adj_factor = bh_annual_avg/no2_ref)



# # --> this is in local (PST, PDT), right?
# bh_ref <- read.csv(file.path("data","pscaa", "beacon_hill_no2.csv"), skip = 7, stringsAsFactors = F) %>%
#   rename(no2 = Seattle.Beacon.Hill...NO2_CAPS...ppb...1Hr.Avg,
#          ) %>%
#   
#   # --> 2 spring daylight savings are left as NAs??
#   
#   mutate(Observation.Time = mdy_hms(Observation.Time),
#          Observation.Time = force_tz(Observation.Time, tz="America/Los_Angeles"),
#          site = "Beacon Hill",
#          )




##################################################################################################
# ADJUST VISIT DATA
##################################################################################################
 

 
  