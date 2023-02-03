
##################################################################################################
# SETUP
##################################################################################################
tictoc::tic()

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

##################################################################################################
# COMPARE NS TOTAL CONC
##################################################################################################


dustin <- read_csv(file.path("data", "tr0090_averaged_stops.csv")) %>%
  filter(grepl("PMSCAN_", instrument_id)) %>%
  select(-c(instrument_id, primary_instrument, mean_value)) %>%
  mutate(variable = if_else(variable=="total.conc", "ns_total_conc", as.character(variable)),
         
         time = ymd_hms(time, tz = "PST8PDT"),
         ) %>%
  
  filter(variable== "ns_total_conc") %>%
  
  rename(value = median_value) %>%
  #winsorize median values
  group_by(variable, location) %>%
  winsorize_fn(value = "value") %>%
  ungroup() %>%
  select(-value) %>%
  #use winsorized values for all subsequent data description
  rename(value_dustin=win_value) %>%
  # make bin count times the same as total_conc
  group_by(location, stop_id) %>%
  #mutate(time = time[variable=="ns_total_conc"]) %>%
  ungroup() %>%
  
  select(time, value_dustin)

magali <- readRDS(file.path(#act_campaign_path, 
  "data",
  "stop_data_win_medians.rda")) %>%
  #drop duplicate UFP instruments
  filter(variable == "ns_total_conc") %>%
  rename(value_magali=value)  %>%
  mutate(
    #time = ymd_hms(time, tz = "PST8PDT"),
  ) %>%
  
  select(#time, 
         value_magali)

# --> has time zone merging issues
#comp <- full_join(magali, dustin)
comp <- cbind(magali, dustin) %>%
  mutate(diff = value_dustin - value_magali)

# looks good - values are almost identical. 
summarize(comp, diff = mean(diff))
   









