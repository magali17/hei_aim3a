# script pulls in all business hours visit campaigns and temporally adjusts the visits; calculates annual averages

# --> TO DO: SEARCH FOR 'TEMP/TO DO' (E.G., remove things that helped run code faster)

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

pacman::p_load(tidyverse, lubridate, zoo, parallel#, #mclapply()
               #DescTools, # Winsorize() #has issues downloading in the cluster
                )    

source("functions.R")
dt_pt <- file.path("data", "onroad", "annie", "v2")
dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
dt_out <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "qc", "road")
if(!dir.exists(dt_out)){dir.create(dt_out, recursive = T)}

lapply(c("visits", "site_avgs"), function(d){if(!dir.exists(file.path(dt_pt2, d))){dir.create(file.path(dt_pt2, d), recursive = T)}} )

set.seed(1)

##################################################################################################
# FILE GENERATION & TESTING MODE
##################################################################################################
# should existing files be regenerated (e.g., due to code changes) or to speed things up?

# clean road files
clean_road_files <- FALSE #TRUE when make updates to the 1sec road file
overwrite_time_series <- FALSE #TRUE when make updates to the 1sec road file
# rolling quantiles
overwrite_existing_background_file <- FALSE #TRUE when e.g., 1sec file is updated

# speed thigns up
testing_mode <- TRUE #reduce visit files
overwrite_fixed_site_adjusted_visits <- FALSE # TRUE when e.g., update visits
overwrite_uw_adjusted_visits <- FALSE #TRUE when update visits (e.g., testing_mode==TRUE)

use_cores <- 6  

##################################################################################################
# FUNCTIONS
##################################################################################################
# COUNT REMAINING ROWS
missingness_table <- data.frame(n = integer(),
                                description = character())

count_missingness <- function(dt, notes) {
  temp <- data.frame(n = nrow(dt),
             description = notes)
  rbind(missingness_table, temp)
}

# NOTES FILE TO TRACK PROGRESS
note_file <- file.path(dt_pt2, paste0("notes_r0_temporal_adjustment_", Sys.Date(),".csv"))
write.table(data.frame(date = POSIXct(),
                       comment = character()),
            file=note_file, sep=",", row.names = F)

add_progress_notes <- function(note) {
  write.table(data.frame(Sys.time(), note), 
              file=note_file, sep=",", append=T, col.names = F, row.names = F)
}

##################################################################################################
# DATA
##################################################################################################
# using fixedsite temporal adjustments previously developed in 1.1_temporal_adjustment.Rmd # using the winsorized adjusted values, as before
set.seed(2)
fixed_site_temp_adj <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox_temp_adjustment.rda")) %>%
  # use ptrak-based temporal adjustment  
  select(time, ufp_adjustment = diff_adjustment_winsorize_ptrak) %>%
  mutate(ufp_adjustment_random = sample(ufp_adjustment, replace = T))

design_types <- readRDS(file.path(dt_pt, "design_types_list.rds"))

message("loading visit data")
add_progress_notes("loading visit data")
# x=design_types[1]
visits <- lapply(design_types, function(x){
  
  file_names <- list.files(file.path(dt_pt, "visits", x)) %>%
    grep("business", value = T, .)
  
  if(testing_mode==TRUE){file_names <- file_names[1]}
  
  message(paste0("...reading ", x, " visit files"))
  
  lapply(file_names, function(f){readRDS(file.path(dt_pt, "visits", x, f))}) %>% bind_rows()
  
  }) %>%  
  bind_rows() %>%
  ungroup() %>%  
  select(id, date, hour, median_value, adjusted, cluster_type, cluster_value, actual_visits, campaign, design, visits, version)

local_tz <- tz(fixed_site_temp_adj$time)

##################################################################################################
# COMMON VARIABLES
##################################################################################################

##################################################################################################

## note, we do things slightly different from what Doubleday et al. did since the purpose here is to use as much (good) data as possible to try to characterize background concentrations. 
# from Doubleday: "We excluded A1 roads (interstates and highways with restricted access) since these are not representative of residential exposures, segments with fewer than a median of 5 1-second measurements per visit, and segments with less than 23 repeat visits. We also excluded road segments immediately before (approaching) or after (departing) a stop location were excluded since these were not fully on-road measures. We averaged the PNC measurements to 10s periods, calculated the median PNC across all 10s measures within segment and visit; winsorized these across visits at the segment level (set values below the 2.5th and above the 97.5th quantile [this should actually be 5 & 95th like Blanco & how Doubleday actually coded this] to those thresholds to reduce the influence of extreme values); and calculated mean visit concentrations per road segment."

message("loading 1s onroad data")
add_progress_notes("loading 1s onroad data")

if(!file.exists(file.path(dt_pt2, "TEMP_road_dt.rda")) | 
   !file.exists(file.path(dt_pt2, "TEMP_road_dt_no_hwy.rda")) | 
   overwrite_time_series==TRUE) {
  
  if(!file.exists(file.path(dt_pt, "TEMP_road_dt0.rda")) |
     clean_road_files==TRUE) {
  
    message("cleaning road file from scratch")
    
    road_dt0 <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
      select(-c(native_id, contains("gps_"), LABEL, num_visit, median_measurement,exclude_flag,update_exclude_flag, speed, ufp_median_5min, A1_flag, hour, minute, second)) %>%
      arrange(time)  
  
    missingness_table <- count_missingness(road_dt0, notes="all data")
    
    segment_lat_long <- distinct(road_dt0, id, mx, my, road_type)
    saveRDS(segment_lat_long, file.path(dt_pt, "segment_lat_long.rda"))
    rm(segment_lat_long)
    
    road_dt0 <- select(road_dt0, -c(mx,my))
      
    road_dt0 <- road_dt0 %>%
      drop_na(ufp)
    missingness_table <- count_missingness(road_dt0, notes="missing 'ufp'")
    
    # some duplicate rows. some remain if same value was assigned to 2 locations during the same second
    road_dt0 <- distinct(road_dt0)
    missingness_table <- count_missingness(road_dt0, notes="drop duplicate rows")
    
    #only keep the first segment/reading when the same reading is assigned to 2 locations/neighboring segments?
    road_dt0 <- road_dt0 %>%
      group_by(time) %>%
      slice(1) %>%
      ungroup()  %>% suppressMessages()
    missingness_table <- count_missingness(road_dt0, notes="only keep 1st location when same sec reading is assigned to 2")
    
    # drop roosevelt garage
    road_dt0 <- road_dt0 %>%
      filter(!(id %in% c(6796, 534, 3064, 867)))
    missingness_table <- count_missingness(road_dt0, notes="drop Roosevelt Garage")
    
    write.csv(missingness_table, file.path(dt_pt, "missing_counts_second_dt_cleanup_pt1.csv"), row.names = F)
    
    saveRDS(road_dt0, file.path(dt_pt, "TEMP_road_dt0.rda"))
    
    } else {road_dt0 <- readRDS(file.path(dt_pt, "TEMP_road_dt0.rda"))}
  
  message("cleaning road file & regenerating time series")
  
  # winsorize at the segment level (drop extremes)
  road_dt0 <- road_dt0 %>%
    group_by(id) %>%
    mutate(ufp = winsorize(ufp, minval=quantile(ufp, 0.05, na.rm=T), maxval=quantile(ufp, 0.95, na.rm=T))) %>%
    ungroup()
    
  saveRDS(road_dt0, file.path(dt_pt, "TEMP_road_dt0_winsorized.rda"))
  # road_dt0 <- readRDS(file.path(dt_pt, "TEMP_road_dt0_winsorized.rda"))
  
  # one runname per day - if during the same day, give them the same "runname" - this seems to have happened on accident? there are several files with different runnames intertwined 
  road_dt0 <- road_dt0 %>%
    mutate(date=date(time),
           hour = hour(time)) %>%
    group_by(date) %>%
    # ## e.g., 2019-06-13 has 6 runnames?!
    # mutate(n=n()) %>% 
    # filter(n>1) %>%
    # group_by(runname) %>%
    # summarize(
    #   start = min(time),
    #   stop = max(time)
    # ) %>% View()
    
    # "route 00 are makeup routes or weird file names". drops 275 runnames to 254
    mutate(runname = ifelse(hour>=4 & hour <=23 & length(unique(runname))>1, paste0(date, "_R00"), runname)) %>% 
    ungroup() %>%
    select(-c(date, hour))
  
  # # QC: check 4-29/30; 6-13 #looks good
  # test %>% group_by(runname) %>%
  #   summarise(start = min(time),
  #             end = max(time),
  #             duration = difftime(end, start, units="hours") 
  #             ) %>% View()
  
  # drop hwy for some of these
  road_dt0_no_hwy <- filter(road_dt0, road_type != "A1") %>%
    select(-road_type)
  
  road_dt0 <- select(road_dt0, -road_type) 
  
  missingness_table <- count_missingness(road_dt0_no_hwy, notes="drop A1 segments")
  write.csv(missingness_table, file.path(dt_pt2, "missing_counts_second_dt_pt2.csv"), row.names = F)
  
  ############################
  time_series <- mclapply(unique(road_dt0$runname), mc.cores=use_cores, function(x){
    a_run <- filter(road_dt0, runname==x)
    data.frame(runname = x,
               time= seq(min(a_run$time), max(a_run$time), by=1))  
  }) %>%
    bind_rows()
  
  # NAs in hour start here b/c of missing readings
  road_dt <- left_join(time_series, road_dt0, by=c("runname", "time")) %>%
    select(runname, time, id, ufp)
  
  road_dt_no_hwy <- left_join(time_series, road_dt0_no_hwy, by=c("runname", "time")) %>%
    select(runname, time, id, ufp)
  
  saveRDS(road_dt, file.path(dt_pt2, "TEMP_road_dt.rda"))
  saveRDS(road_dt_no_hwy, file.path(dt_pt2, "TEMP_road_dt_no_hwy.rda"))
  
  rm(list=c("time_series", "road_dt0", "road_dt0_no_hwy"))
} else {
  road_dt <- readRDS(file.path(dt_pt2, "TEMP_road_dt.rda"))
  road_dt_no_hwy <- readRDS(file.path(dt_pt2, "TEMP_road_dt_no_hwy.rda"))
  }

##################################################################################################
# VARIABLES
##################################################################################################
# e.g., 3 hr * 60 min/hr * 60 sec/min
windows <- c(1,3)*60*60
quantiles <- c(0.01, 0.03, 0.05, 0.10)

##################################################################################################
# 1. TEMPORAL ADJUSTMENT: PSEUDO FIXED SITES (FROM PREDICTED UFP)
##################################################################################################
if(!file.exists(file.path(dt_pt2, "site_avgs", "temp_adj1.rds")) | 
   overwrite_fixed_site_adjusted_visits == TRUE) {
  message("running fixed site temporal adjustment from predicted UFP based on NO2")

  visits_adj1 <- visits %>% 
    mutate(time = ymd_h(paste(date, hour), tz=local_tz)) %>%  
    # add temporal adjustment
    left_join(fixed_site_temp_adj, by="time") %>%
    mutate(median_value_adjusted = median_value + ufp_adjustment,
           median_value_adjusted_random = median_value + ufp_adjustment_random,
           version = paste(version, "temp adj 1"))

  message("...saving adjusted visits")
  saveRDS(visits_adj1, file.path(dt_pt2, "visits", "temp_adj1.rds"))

  annual_adj1 <- visits_adj1 %>%
    group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
    summarize(annual_mean = mean(median_value_adjusted, na.rm=T),
              annual_mean_random = mean(median_value_adjusted_random, na.rm=T),) %>%
    ungroup() %>% suppressMessages()

  message("...saving annual averages")
  saveRDS(annual_adj1, file.path(dt_pt2, "site_avgs", "temp_adj1.rds"))
}

##################################################################################################
# 2. UNDERWRITE FN: PSEUDO FIXED SITE FROM COLLECTED UFP MEASURES
##################################################################################################
calculate_rolling_quantile <- function(dt, windows.=windows, quantiles.=quantiles, file_label="", 
                                       override_existing_file = overwrite_existing_background_file) {
  
  adj_lvls <- apply(expand.grid(paste0("hr", sort(windows)/3600), 
                                paste0("_pct", sort(quantiles)*100)), 
                    1, paste, collapse="")
  
  dt <- lapply(windows., function(w) {
    lapply(quantiles., function(p) {
      
      bg_label <- paste0("hr", w/3600, "_pct", p*100)
      file_label. <- paste0(bg_label, file_label)
      print(paste(Sys.time(), file_label.))
      
      file_name <- file.path(dt_pt2, paste0("uw_temp_adj_1s_", file_label., ".rda"))
      
      if(!file.exists(file_name) | 
         override_existing_file==TRUE) {
        message("generating new rolling quantiles")
        
        # rolling quantiles for each runname & underwrite approach
        result <- mclapply(group_split(dt, runname), mc.cores=use_cores, function(x) {
          mutate(x, background_adj = bg_label,
                   # rolling quantile, ignore NAs (i.e., periods of time w/o UFP readings) may still get background estimates
                   background = rollapply(ufp, width = w, FUN = quantile, prob = p, align = "center", partial = TRUE, na.rm=T))
          }) %>%
          bind_rows()
        # save individual window-quantile combinations
        saveRDS(result, file_name)
        
        result
      } else{readRDS(file_name)}
      })}) %>%
    bind_rows() %>%
    mutate(background_adj = factor(background_adj, levels=adj_lvls))
}

##################################################################################################
message("running underwrite temporal adjustment for all road data")
add_progress_notes("running underwrite tmeporal adjustment for all data")
road_dt <- calculate_rolling_quantile(dt=road_dt)
saveRDS(road_dt, file.path(dt_pt2, "underwrite_temp_adj_all_1s_data.rda"))

message("running underwrite temporal adjustment for non-highway data")
add_progress_notes("running underwrite tmeporal adjustment for non-highway data")
road_dt_no_hwy <- calculate_rolling_quantile(dt=road_dt_no_hwy, file_label="_no_hwy")
saveRDS(road_dt_no_hwy, file.path(dt_pt2, "underwrite_temp_adj_all_1s_data_no_hwy.rda")) 

# QC - check that tz is still correct.
# test_road_dt_no_hwy <- calculate_rolling_quantile(dt=road_dt_no_hwy, file_label="_no_hwy", windows.=10800, quantiles.=0.01)
# test_road_dt_no_hwy$times[1]

##################################################################################################
# note: some hours don't have UFP but still have hourly adjustments b/c rm.na=T for rolling window calculations
## dt=road_dt
get_hourly_adjustment <- function(dt) {
  temp <- dt %>%
    group_by(background_adj) %>%
    # note: this uses 'background' 1sec rolling quantiles even in places where there were no readings if there were readings some time before/after
    mutate(bg_lta = mean(background, na.rm = T),
           date = date(time), # important!! don't use as.Date() - automatically sets date to UTC
           hour = hour(time)) %>%
    group_by(runname, date, hour, background_adj, bg_lta) %>%
    summarize(bg_hour_avg = mean(background, na.rm = T),
              #bg_hour_median = median(background, na.rm = T)
              ) %>%
    ungroup() %>% suppressMessages() %>%
    mutate(avg_hourly_adj = bg_lta - bg_hour_avg,
           #median_hourly_adj = bg_lta - bg_hour_median,
           time = ymd_h(paste(date, hour), tz=local_tz)) %>%
    select(runname, date, hour, time, background_adj, bg_lta, bg_hour_avg, avg_hourly_adj#, bg_hour_median, median_hourly_adj
           ) %>%
    # drop NA & NaN caused from no MM data for entire hours early on in the day (why was this start time here to begin with?)
    drop_na()
}

##################################################################################################
message("...estimating hourly adjustments")
add_progress_notes("estimating hourly adjustments")

if(file.exists(file.path(dt_pt2, "underwrite_temp_adj.rda")) &
   overwrite_existing_background_file == FALSE) {
  underwrite_adj <- readRDS(file.path(dt_pt2, "underwrite_temp_adj.rda"))
  } else { 
  set.seed(2)
  underwrite_adj <- get_hourly_adjustment(road_dt) %>%
    # randomly sample the entire temporal adjustment dataset
    group_by(background_adj) %>%
    mutate(avg_hourly_adj_random = sample(avg_hourly_adj, replace = T)) %>%
    ungroup()
  
  saveRDS(underwrite_adj, file.path(dt_pt2, "underwrite_temp_adj.rda"))
}  


if(file.exists(file.path(dt_pt2, "underwrite_temp_adj_no_hwy.rda")) &
   overwrite_existing_background_file == FALSE) {
  underwrite_adj_no_hwy <- readRDS(file.path(dt_pt2, "underwrite_temp_adj_no_hwy.rda"))
  } else { 
  set.seed(2)
  underwrite_adj_no_hwy <- get_hourly_adjustment(road_dt_no_hwy)%>%
    # randomly sample the entire temporal adjustment dataset
    group_by(background_adj) %>%
    mutate(avg_hourly_adj_random = sample(avg_hourly_adj, replace = T)) %>%
    ungroup()
  
  saveRDS(underwrite_adj_no_hwy, file.path(dt_pt2, "underwrite_temp_adj_no_hwy.rda"))
}

##################################################################################################
message("...applying temporal adjustment using all segments")
add_progress_notes("applying temporal adjustment using all segments")

lapply(group_split(underwrite_adj, background_adj), function(x){
  background_adj_label <- first(x$background_adj)
  message(paste0("......", background_adj_label))
  
  visit_file <- file.path(dt_pt2, "visits", paste0("temp_adj2_", background_adj_label, ".rds"))
  annual_file <- file.path(dt_pt2, "site_avgs", paste0("temp_adj2_", background_adj_label, ".rds"))
  
  if(!file.exists(visit_file) |
     !file.exists(annual_file) |
     overwrite_uw_adjusted_visits == TRUE) {
    
    visits_adj2 <- visits %>%
      mutate(time = ymd_h(paste(date, hour), tz=local_tz)) %>%
      # add temporal adjustment
      left_join(select(x, time, background_adj, avg_hourly_adj, avg_hourly_adj_random), by="time") %>% 
      mutate(median_value_adjusted = median_value + avg_hourly_adj,
             median_value_adjusted_random = median_value + avg_hourly_adj_random,
             version = paste(version, "temp adj 2"))
    
    saveRDS(visits_adj2, visit_file)
    
    message("...estimating location annual averages using all segments")
    add_progress_notes("estimating location annual averages using all segments")
    
    annual_adj2 <- visits_adj2 %>%
      group_by(background_adj, id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
      summarize(annual_mean = mean(median_value_adjusted, na.rm=T),
                annual_mean_random = mean(median_value_adjusted_random, na.rm=T),
                ) %>%
      ungroup()  %>% suppressMessages()
    
    saveRDS(annual_adj2, annual_file)
    }
  })

##################################################################################################
message("...applying temporal adjustment to non-hwy segments")
add_progress_notes("applying temporal adjustment using non-hwy segments")

lapply(group_split(underwrite_adj_no_hwy, background_adj), function(x){
  background_adj_label <- first(x$background_adj)
  message(paste0("......", background_adj_label))
  
  visit_file <- file.path(dt_pt2, "visits", paste0("temp_adj2_no_hwy_", background_adj_label, ".rds"))
  annual_file <- file.path(dt_pt2, "site_avgs", paste0("temp_adj2_no_hwy_", background_adj_label, ".rds"))
  
  if(!file.exists(visit_file) |
     !file.exists(annual_file) |
     overwrite_uw_adjusted_visits == TRUE) {
    
  visits_adj2_no_hwy <- visits %>%
    mutate(time = ymd_h(paste(date, hour), tz=local_tz)) %>%
    # add temporal adjustment
    left_join(select(x, time, background_adj, avg_hourly_adj, avg_hourly_adj_random), by="time") %>% 
    mutate(median_value_adjusted = median_value + avg_hourly_adj,
           median_value_adjusted_random = median_value + avg_hourly_adj_random,
           version = paste(version, "temp adj 2"))
  
  saveRDS(visits_adj2_no_hwy, visit_file)
                                         
  
  message("...estimating location annual averages using non-hwy segments")
  add_progress_notes("estimating location annual averages using non-hwy segments")
  annual_adj2_no_hwy <- visits_adj2_no_hwy %>%
    group_by(background_adj, id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
    summarize(annual_mean = mean(median_value_adjusted, na.rm=T),
              annual_mean_random = mean(median_value_adjusted_random, na.rm=T)) %>%
    ungroup()  %>% suppressMessages()
  
  saveRDS(annual_adj2_no_hwy, annual_file)
  }
})

##################################################################################################
# DONE
##################################################################################################
message("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")
add_progress_notes("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")

##################################################################################################
# APPENDIX
##################################################################################################

