
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
#source("file_paths.R")
set.seed(1)

## for future.apply::future_replicate()  
# availableCores() #8 
plan(multisession, workers = 6)
 

##################################################################################################
# LOAD DATA
##################################################################################################
#stop data with temporal variables included

# Nanoscan bin estimates
ns_psd0 <- read_csv(file.path("data", "tr0090_averaged_stops.csv")) %>%
  filter(grepl("PMSCAN_", instrument_id)) %>%
  select(-c(instrument_id, primary_instrument, mean_value)) %>%
  mutate(variable = if_else(variable=="total.conc", "ns_total_conc", as.character(variable))) %>%
  rename(value = median_value)  

# ns_bins <- ns_psd0 %>%
#   #filter(!grepl("total", variable)) %>%
#   distinct(variable) %>% pull() %>% 
#   sort(decreasing = T)


# --> drop pnc_noscreen to increase visits/site later?

keep_vars1 <- c(#"ma200_ir_bc1", "co2_umol_mol", "pm2.5_ug_m3", "ns_total_conc",
  "no2"#, "pnc_noscreen" 
  )

bins <- paste0("ns_", 
               c("11.5", "15.4", "20.5", "27.4", "36.5", "48.7", "64.9", "86.6", "115.5", "154.0","205.4", "273.8", "365.2"))

keep_vars <- c(keep_vars1, "ns_total_conc", bins)


ns_psd <- ns_psd0 %>%
  mutate(
    # for merging later
    time = ymd_hms(time, tz = "PST8PDT"),
    
    day = wday(time,label = T, week_start = 1),
    hour = hour(time),
    #pollutant = variable
    ) %>%
  mutate(variable = ifelse(!grepl("ns_", variable),
                            paste0("ns_", variable),
                            as.character(variable))) %>% 
  add_season(.date_var = "date") %>%
  arrange(time) %>%
  
  #winsorize median values
  group_by(variable, location) %>%
  
  winsorize_fn(value = "value") %>%
  ungroup() %>%
  select(-value) %>%
  #use winsorized values for all subsequent data description
  rename(value=win_value) %>%
  # make bin count times the same as total_conc
  group_by(location, stop_id) %>%
  mutate(time = time[variable=="ns_total_conc"]) %>%
  ungroup()

stops_no2 <- readRDS(file.path(#act_campaign_path, 
  "data",
  "stop_data_win_medians.rda")) %>%
  #drop duplicate UFP instruments
  filter(variable %in% keep_vars1) %>%
  select(names(ns_psd)) %>%
  arrange(time)

stops_all <- rbind(stops_no2, ns_psd) %>%
  mutate(tow2 = ifelse(day %in% c("Sat", "Sun"), "weekend", "weekday"))
    

# --> DROP? KEEP ALL SITES?

# only use non-test sites for simulations
non_test_sites <- readRDS(file.path("Output", "mm_cov_train_set_hei.rda")) %>%
  distinct(location) %>% pull()

# 278 locations from 309
stops <- filter(stops_all, location %in% non_test_sites)

##################################################################################################
# COMMON VARIABLES
##################################################################################################
# number of simulations
sim_n <- 30

rush_hours <- c(7:10, 15:18)
business_hours <- c(9:17)

unique_seasons <- unique(stops_all$season) %>% as.character()

# number of samples for fewer hours, seasons, reduced balance
fewer_hrs_seasons_n <- 12

##################################################################################################
# ONLY KEEP STOPS W/ LITTLE MISSINGNESS & MOSTLY NON-ZEROS
##################################################################################################

keep_times0 <- stops %>% 
  distinct(stop_id, time, variable, value) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(original_stops = n()) 

# proportion of the time bins are missing
prop_time_missing <- apply(keep_times0, 2, function(x) sum(is.na(x))/length(x)) %>%
  as.data.frame() %>% arrange(.) %>%
  rownames_to_column()

prop_time_missing

# drop bins with mostly 0s
(zero_counts <- stops %>%
  group_by(variable) %>%
  summarize(n = n(),
            n_zero = sum(value==0),
            prop_zero = n_zero/n) 
  )

zero_counts <- filter(zero_counts, prop_zero > .6) %>% pull(variable)

# drop bins w/ lot of missingness or mostly zero counts
bad_bins <- filter(prop_time_missing, .>0.5) %>% pull(rowname) %>%
  c(., zero_counts)
keep_vars <- setdiff(keep_vars, bad_bins)
saveRDS(keep_vars, file.path("Output", "keep_vars.rda"))

#stops_all <- filter(stops_all, variable %in% keep_vars)
stops <- filter(stops, variable %in% keep_vars)


# ############## DON'T DO??? ##############
# keep_times0 <- keep_times0 %>%
#   drop_na() %>%
#   mutate(
#     remaining_stops = n(),
#     prop_remaining_stops = remaining_stops/original_stops
#     )
# # 
# keep_times <- keep_times0 %>% #7345 stops left (90.12%)
#   distinct(time) %>% pull()
# 
# stops <- filter(stops, time %in% keep_times)
# 
# ##############################################

# 7,806/8163 = 96% stops remain
stops_w <- pivot_wider(data = stops, names_from = "variable",values_from =  "value") %>%
  drop_na()

saveRDS(stops_w, file.path("Output", "stops_used.rda"))

##################################################################################################
# TRUE ANNUAL AVERAGE
##################################################################################################
true_annual <- stops_w %>%
  group_by(location) %>%
  # note, visit # is wrong here b/c there are NAs
  mutate(visits = n()) %>%
  summarize_at(all_of(vars(keep_vars, visits)), ~mean(., na.rm=T)) %>%
  mutate(
    campaign = 1,
    design = "full",
    version = "all training data"
  ) %>%
  ungroup()
  
   
# save est set annual averages for validation later
## this also uses the winsorized stop data & the same date ranges
annual_test_set <- stops_all %>%
  filter(!location %in% non_test_sites,
         #same date range as training data; only keep times where all pollutants have values
         #time %in% keep_times
         ) %>%
  group_by(variable, location) %>%
  summarize(value = mean(value,  na.rm=T),
            visits = n(),
            campaign = 1,
            design = "test set",
            version = "test set"
  )

saveRDS(annual_test_set, file.path("Output", "annual_test_set.rda"))

##################################################################################################
# SAMPLING DESIGNS
##################################################################################################
# FEWER VISITS

##################################################################################################
## functions 

### sampling functions
# s_tow2_sample_fn <- function(df, wkday_visits = 2, wkend_visits = 1) {
#   
#   no_visits <- ifelse(first(df$tow2) == "weekday", wkday_visits, wkend_visits)
#   df0 <- slice_sample(df, n  = no_visits)
#   
#   return(df0)
#   }

# fn takes one random sample from each list item, according to FUN, and calculates a 'value' average
#list should be in wide format (variable/pollutant names) so that same [temporal] samples are collected across pollutants
                             #list   #sample fn    #descriptor
one_sample_avg <- function(my_list,# = stops_w_list, 
                             my_sampling_fn # =s_tow2_sample_fn  
                             ) {
  result <- suppressWarnings( 
    mclapply(my_list, mc.cores = 6, FUN = my_sampling_fn) %>%
    #unlist results 
    bind_rows() %>%
    group_by(location) %>%
    mutate(visits = n()) %>%
    #calculate annual average
    summarize_at(all_of(c(keep_vars, "visits")), ~mean(., na.rm=T))
  )
    
    return(result)
} 


##################################################################################################
#2. fewer seasons. keep the number of samples the same. 6 is approximately the # of samples/site/season (i.e., max for season ==1)

## notice that a few sites (e.g., MS0138 in winter) have < 6 samples/season, so we'll sample w/ replacement to keep the numebr of samples the same
message("fewer seasons")

season_n <- c(1:4)  
#season_n <- c(2:3)  

season_times <- data.frame()

for (i in seq_along(season_n)) {

  temp <- future_replicate(n = sim_n,  
                           simplify = F,
                                    expr = one_sample_avg(my_list = group_split(stops_w, #variable, 
                                                                                location), 
                                                          my_sampling_fn = function(x)  {
                                                            seasons_to_sample <- sample(c("spring", "summer", "fall", "winter"), size = season_n[i], replace = F)
                                                            
                                                            df <- filter(x, season %in% seasons_to_sample) %>%
                                                              group_by(season) %>%
                                                              # a few sites have < 6 samples/season, so use replacement=True here
                                                              slice_sample(n = fewer_hrs_seasons_n/season_n[i], replace=T)
                                                            }
                                                          )
                           ) %>%
    #unlist
    bind_rows() %>%
    #add simulation number & label
    group_by(location) %>%
    mutate(campaign =  row_number(),
           design = "balanced seasons",
           version = paste0(season_n[i])
           )%>%
    ungroup()
  
  season_times <- rbind(season_times, temp)

}

##################################################################################################
message("BH, RH")

rh_bh <- list(#sort(unique(c(rush_hours, business_hours))),
              business_hours, rush_hours
              )

names(rh_bh) <- c(#"business & rush", 
                  "business", "rush")

rh_bh_df <- data.frame()

for(i in seq_along(rh_bh)) {
  #i=1
  temp <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg(my_list = group_split(stops_w, location), 
                           #mc.cores = 4, 
                           my_sampling_fn = function(x) {
                             x %>% filter(tow2 == "weekday",
                                          hour %in% rh_bh[[i]]) %>%
                               # NOTE: using sampling w/ replacement to ensure x samples/site 
                               slice_sample(n=fewer_hrs_seasons_n, replace=T)
                             }
                           )
                           ) %>%
    bind_rows() %>%
    group_by(location) %>%
    mutate(
      campaign = row_number(),
      version = names(rh_bh)[i],
      design = "fewer hours"
    ) %>%
    as.data.frame()
  
  rh_bh_df <- rbind(rh_bh_df, temp)
  
  }


##################################################################################################
# combine TEMPORAL simulation results
message("combining temporal sims")

temporal_sims <- rbind(
  true_annual,
  #days,
  season_times, 
  rh_bh_df
  ) %>%
  mutate(spatial_temporal = ifelse(grepl("full", design), "gold standard", "temporal"))


##################################################################################################
# FEWER SITES & VISITS (TOTAL STOPS)
##################################################################################################
# site_n <- c(25, seq(50, 250, 50))
# visit_n <- seq(4,24,4) 

message("fewer total stops")

#site_n2 <- append(site_n, length(unique(stops_w$location)))

site_n2 <- c(150, 278)
#visit_n2 <- append(visit_n, round(mean(true_annual$visits))) 
visit_n2 <- 12

site_visit_df <- data.frame()

for(v in visit_n2) {
  # v = visit_n2[7]
  temp <- replicate(n = sim_n, simplify = F,
                         expr = mclapply(site_n2, mc.cores = 5, function(x) {
                                                 #sample sites
                                                 sample_sites <- sample(unique(stops_w$location), size = x, replace = F)
                                                 
                                                 #sample visits
                                                 filter(stops_w, location %in% sample_sites) %>%
                                                   group_by(location) %>%
                                                   slice_sample(n = v) %>% 
                                                   mutate(visits = n()) %>%
                                                   #calculate annual average
                                                   summarize_at(all_of(c(keep_vars, "visits")), ~mean(., na.rm=T)) %>%
                                                   mutate(
                                                     version = paste0(v, "_visits ", x, "_sites")
                                                   )
                                                 })
                         ) %>%
  #unlist
  bind_rows() %>%
    
  ungroup() %>%
  mutate(
    campaign = rep(1:sim_n, each=nrow(.)/sim_n),
    design = "fewer total stops",
    spatial_temporal = "spatial temporal"
  ) 
  
  site_visit_df <- rbind(site_visit_df, temp)
  
  }

site_visit_df <- select(site_visit_df, names(temporal_sims))

# # how many total stops are there in the 278 sites x 26 visits since some sites have a max od 21-25 visits? 
# site_visit_df  %>% 
#   filter(version == "26_visits 278_sites") %>% 
#   distinct(location, visits) %>% 
#   summarize(max_total_stops = sum(visits))

##################################################################################################
# combine spatial and temporal simulations

annual_training_set <- rbind(temporal_sims, site_visit_df) 

##################################################################################################
# SAVE DATA
##################################################################################################
saveRDS(annual_training_set, file.path("Output", "annual_training_set.rda"))

message("done with 1_act_annual.R")

tictoc::toc()


