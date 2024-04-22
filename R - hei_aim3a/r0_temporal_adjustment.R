# script temporally adjusts onroad data

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

pacman::p_load(tidyverse, lubridate, zoo,
               #DescTools, # Winsorize() #has issues downloading in the cluster
               parallel #mclapply()
               )    

source("functions.R")
dt_pt <- file.path("data", "onroad", "annie", "v2")
dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
dt_out <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "qc", "road")
if(!dir.exists(dt_out)){dir.create(dt_out, recursive = T)}
if(!dir.exists(dt_pt2)){dir.create(dt_pt2, recursive = T)}

set.seed(1)

# have issues if increase this w/ the rolling quantiles function
use_cores <- 1
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

# NOTSE FILE TO TRACK PROGRESS
note_file <- file.path(dt_pt2, "progress_notes_r0_temporal_adjustment.R.csv")
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
message("loading data")
add_progress_notes("loading data")

# # using fixed-site temporal adjustments previously developed in 1.1_temporal_adjustment.Rmd # using the winsorized adjusted values, as before
# fixed_site_temp_adj <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox_temp_adjustment.rda")) %>%
#   select(time, ufp_adjustment = diff_adjustment_winsorize)

# BH samples
if(!file.exists(file.path(dt_pt, "TEMP_bh_visits.rda"))) {
  v1 <- readRDS(file.path(dt_pt, "nonspatial_visit_samples.rds")) %>%
    filter(version == "business hours")
  v2 <- readRDS(file.path(dt_pt, "cluster_visit_samples.rds")) %>%
    filter(version == "business hours") %>%
    select(-visit_samples)

  visits <- bind_rows(v1, v2) %>%
    select(-c(visit_num, runname, dow, dow2))

  saveRDS(visits, file.path(dt_pt, "TEMP_bh_visits.rda"))
  rm(list=c("v1", "v2"))
} else {
    visits <- readRDS(file.path(dt_pt, "TEMP_bh_visits.rda")) %>%
      #visits %>%

      # --> TEMP TO MAKE THINGS GO FASTER 
      
      filter(!cluster_type %in% c("cluster2", "cluster3"),
             !design %in% c("unbalanced", "unsensible", "road_type"))
  }

bh_version <- unique(visits$version) %>% as.character()

visits <- select(visits, -version)

##################################################################################################

## note, we do things slightly different from what Doubleday et al. did since the purpose here is to use as much (good) data as possible to try to characterize background concentrations. 
# from Doubleday: "We excluded A1 roads (interstates and highways with restricted access) since these are not representative of residential exposures, segments with fewer than a median of 5 1-second measurements per visit, and segments with less than 23 repeat visits. We also excluded road segments immediately before (approaching) or after (departing) a stop location were excluded since these were not fully on-road measures. We averaged the PNC measurements to 10s periods, calculated the median PNC across all 10s measures within segment and visit; winsorized these across visits at the segment level (set values below the 2.5th and above the 97.5th quantile [this should actually be 5 & 95th like Blanco & how Doubleday actually coded this] to those thresholds to reduce the influence of extreme values); and calculated mean visit concentrations per road segment."

if(!file.exists(file.path(dt_pt2, "TEMP_road_dt.rda")) | !file.exists(file.path(dt_pt2, "TEMP_road_dt_no_hwy.rda"))) {
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
    ungroup()  
  
  saveRDS(road_dt0, file.path(dt_pt2, "TEMP_road_dt0.rda"))
  
  missingness_table <- count_missingness(road_dt0, notes="only keep 1st location when same sec reading is assigned to 2")
  
  # drop roosevelt garage
  road_dt0 <- road_dt0 %>%
    filter(!(id %in% c(6796, 534, 3064, 867)))
  missingness_table <- count_missingness(road_dt0, notes="drop Roosevelt Garage")
  
  # winsorize at the segment level (drop extremes)
  road_dt0 <- road_dt0 %>%
    group_by(id) %>%
    mutate(ufp = winsorize(ufp, minval=quantile(ufp, 0.05, na.rm=T), maxval=quantile(ufp, 0.95, na.rm=T))) %>%
    ungroup()
    
  # drop hwy for some of these
  road_dt0_no_hwy <- filter(road_dt0, road_type != "A1") %>%
    select(-road_type)
  
  road_dt0 <- select(road_dt0, -road_type) 
  
  missingness_table <- count_missingness(road_dt0_no_hwy, notes="drop A1 segments")
  
  write.csv(missingness_table, file.path(dt_pt2, "missing_counts_second_dt.csv"), row.names = F)
  
  # x=unique(road_dt0$runname)[1]
  time_series <- mclapply(unique(road_dt0$runname), mc.cores=use_cores, function(x){
    a_run <- filter(road_dt0, runname==x)
    data.frame(runname = x,
               time= seq(min(a_run$time), max(a_run$time), by=1)) #%>%
      
      # --> TO DO: drop hour here and don't add it until do hourly adjustment function?
      #mutate(hour = hour(time) %>% as.character())
  }) %>%
    bind_rows()
  
  # NAs in hour start here b/c of missing readings
  road_dt <- left_join(time_series, road_dt0, by=c("runname", "time")) %>%
    select(runname, time, #hour, 
           id, ufp)
  
  road_dt_no_hwy <- left_join(time_series, road_dt0_no_hwy, by=c("runname", "time")) %>%
    select(runname, time, #hour, 
           id, ufp)
  
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

# --> TEMP

windows <- 60*60*1
quantiles <- c(0.01, 0.05)
# windows <- c(1,3)*60*60
# quantiles <- c(0.01, 0.03, 0.05, 0.10)

##################################################################################################
# 1. TEMPORAL ADJUSTMENT: PSEUDO FIXED SITES (FROM PREDICTED UFP)
##################################################################################################

# if(!file.exists(file.path(dt_pt2, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))) {
#   message("running fixed site temporal adjustment from predicted UFP based on NO2")
# 
#   visits_adj1 <- visits %>%
#     mutate(time = ymd_h(paste(date, hour))) %>%
#     # add temporal adjustment
#     left_join(fixed_site_temp_adj, by="time") %>%
#     mutate(median_value_adjusted = median_value + ufp_adjustment,
#            version = paste(bh_version, "temp adj 1"))
# 
#   saveRDS(visits_adj1, file.path(dt_pt2, "bh_visits_fixed_site_temporal_adj.rds"))
# 
#   annual_adj1 <- visits_adj1 %>%
#     group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
#     summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
#     ungroup()
# 
#   # temp file
#   saveRDS(annual_adj1, file.path(dt_pt2, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))
# } else {
#   annual_adj1 <- readRDS(file.path(dt_pt2, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))
# }

##################################################################################################
# 2. UNDERWRITE FN: PSEUDO FIXED SITE FROM COLLECTED UFP MEASURES
##################################################################################################
 #dt=road_dt_no_hwy %>% filter(runname == first(runname))
# windows.=windows 
# quantiles.=quantiles 
calculate_rolling_quantile <- function(dt, windows.=windows, quantiles.=quantiles) {
  
  adj_lvls <- apply(expand.grid(paste0("hr", sort(windows)/3600), 
                                paste0("_pct", sort(quantiles)*100)), 
                    1, paste, collapse="")
  
  # w=windows.[1]
  # p=quantiles.[1]
  dt <- lapply(windows., function(w) {
    mclapply(quantiles., mc.cores=use_cores, function(p) {
      
      bg_label <- paste0("hr", w/3600, "_pct", p*100)
      print(paste(Sys.time(), bg_label))
      
      file_name <- file.path(dt_pt2, paste0("uw_temp_adj_1s_", bg_label, ".rda"))
      
      if(!file.exists(file_name)) {
        result <- dt %>%
          group_by(runname) %>%
          mutate(
            background_adj = bg_label,
            # rolling quantile, ignore NAs (i.e., periods of time w/o UFP readings) may still get background estimates
            background = rollapply(ufp, width = w, FUN = quantile, prob = p, align = "center", partial = TRUE, na.rm=T))
        
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

message("running underwrite temporal adjustment for non-highway data")
add_progress_notes("running underwrite tmeporal adjustment for non-highway data")
road_dt_no_hwy <- calculate_rolling_quantile(dt=road_dt_no_hwy)
 
##################################################################################################
# note: some hours don't have UFP but still have hourly adjustments b/c rm.na=T for rolling window calculations
get_hourly_adjustment <- function(dt) {
  dt %>%
    group_by(background_adj) %>%
    mutate(bg_lta = mean(background, na.rm = T),
           date = date(time), # important!! don't use as.Date() - automatically sets date to UTC
           hour = hour(time) #%>% as.character()
           ) %>%
    group_by(runname, date, hour, background_adj, bg_lta) %>%
    summarize(bg_hour_avg = mean(background, na.rm = T),
              #bg_hour_median = median(background, na.rm = T)
              ) %>%
    ungroup() %>%
    mutate(avg_hourly_adj = bg_lta - bg_hour_avg,
           #median_hourly_adj = bg_lta - bg_hour_median,
           time = ymd_h(paste(date, hour))) %>%
    select(runname, date, hour, time, background_adj, bg_lta, bg_hour_avg, avg_hourly_adj#, bg_hour_median, median_hourly_adj
           ) %>%
    # drop NA & NaN caused from no MM data for entire hours early on in the day (why was this start time here to begin with?)
    drop_na()
}

##################################################################################################
message("estimating hourly adjustments")
add_progress_notes("estimating hourly adjustments")

underwrite_adj <- get_hourly_adjustment(road_dt)
underwrite_adj_no_hwy <- get_hourly_adjustment(road_dt_no_hwy)

saveRDS(underwrite_adj, file.path(dt_pt2, "underwrite_temp_adj.rda"))
saveRDS(underwrite_adj_no_hwy, file.path(dt_pt2, "underwrite_temp_adj_no_hwy.rda"))

##################################################################################################
message("applying temporal adjustment using all segments")
add_progress_notes("applying temporal adjustment using all segments")

visits_adj2 <- visits %>%
  mutate(time = ymd_h(paste(date, hour))) %>%
  # add temporal adjustment
  left_join(select(underwrite_adj, time, background_adj, avg_hourly_adj), by="time"
            #multiple = "all" #receive warning message w/o this 
            ) %>% 
  mutate(median_value_adjusted = median_value + avg_hourly_adj,
         version = paste(bh_version, "temp adj 2"))

saveRDS(visits_adj2, file.path(dt_pt2, "bh_visits_fixed_site_temporal_adj_uw.rds")) 

message("estimating location annual averages using all segments")
add_progress_notes("estimating location annual averages using all segments")

annual_adj2 <- visits_adj2 %>%
  group_by(background_adj, id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup()  

saveRDS(annual_adj2, file.path(dt_pt2, "TEMP_bh_site_avgs_uw_adj.rds"))

##################################################################################################
message("applying temporal adjustment to non-hwy segments")
add_progress_notes("applying temporal adjustment using non-hwy segments")

visits_adj2_no_hwy <- visits %>%
  mutate(time = ymd_h(paste(date, hour))) %>%
  # add temporal adjustment
  left_join(select(underwrite_adj_no_hwy, time, background_adj, avg_hourly_adj), by="time") %>% 
  mutate(median_value_adjusted = median_value + avg_hourly_adj,
         version = paste(bh_version, "temp adj 2"))

saveRDS(visits_adj2_no_hwy, file.path(dt_pt2, "bh_visits_fixed_site_temporal_adj_uw_no_hwy.rds")) 

message("estimating location annual averages using non-hwy segments")
add_progress_notes("estimating location annual averages using non-hwy segments")
annual_adj2_no_hwy <- visits_adj2_no_hwy %>%
  group_by(background_adj, id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup()  

saveRDS(annual_adj2_no_hwy, file.path(dt_pt2, "TEMP_bh_site_avgs_uw_adj_no_hwy.rds"))

##################################################################################################
# DONE
##################################################################################################
message("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")
add_progress_notes("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")

##################################################################################################
# APPENDIX
##################################################################################################

##################################################################################################
# # Overnight data
# overnight <- readRDS(file.path("data", "Overnight Collocations", "overnight_data_2023-09-11.rda")) %>%
#   #most of the data is at BH
#   filter(
#     # 10W data is only for March 2020; WILCOX data is for after the study period
#     site == "AQSBH",
#     variable == "ns_total_conc",
#     # most (86.5%) of the data comes from PMSCAN_3 (vs PMSCAN_2)
#     # drop PMSCAN_2. It is used less & produces higher readings when collocated in June that would probably need to be calibrated
#     instrument_id=="PMSCAN_3") %>%
#   rename(time=timeint) %>%
#   mutate(date = date(time),
#          month = month(time, label=TRUE),
#          month = factor(month, ordered = F),
#          dow = wday(time, label=T, week_start = "Monday"),
#          dow = factor(dow, ordered = F),
#          weekday = ifelse(dow %in% c("Sat", "Sun"), "weekend", "weekday"),
#          hour = hour(time),
#          hour = factor(hour),
#   ) #%>%
# 
#   # --> don't average?
# 
#   # # hourly averages
#   # group_by(site, date, hour, month, dow, weekday, variable) %>%
#   # summarize(value = mean(value, na.rm = T)) %>%
#   # ungroup()

# ##################################################################################################
# # 2. BACKGROUND ADJUSTMENT USING THE COLLECTED DATA
# ##################################################################################################
# # e.g., see Hankey 2015 LUR... method summary:
# # "(1) subtracting instantaneous background concentration estimates (via the underwrite function) from all
# # instrument-reported concentrations, (2) calculating mean reference-site (i.e., background) measurements among all
# # sampling days, and (3) adding the mean reference-site concentrations (from step 2) to the underwrite adjusted
# # concentrations from step 1."
# # 
# # approach gap: does not adjust for overnight or weekend hours that are never measured in BH/RH designs
# 
# message("running background/local adjustment using the collected data")
# set.seed(1)
# 
# low_conc <- 0.01 #0.05
# 
# 
# # --> UPDATE
# 
# # x = group_split(visits, adjusted, campaign, design, visits, cluster_type)[[1]]
# visits_adj2 <- lapply(group_split(visits, adjusted, campaign, design, visits, cluster_type), function(x){
#   temp <- x %>%
#     mutate(hour = as.numeric(hour)) %>%
#     group_by(hour) %>%
#     mutate(quantile_background = quantile(median_value, low_conc)) %>% 
#     ungroup()
#   
#   # make the hourly adjustments smoother
#   temp_hrs <- distinct(temp, hour, quantile_background)
#   smooth_fit <- lm(quantile_background ~ ns(hour, knots=c(12, 15)), data = temp_hrs)
#   temp_hrs <- temp_hrs %>%
#     mutate(smooth_background = predict(smooth_fit, newdata=.),
#            # long-term average estimate based on background concentrations from the collected campaign data
#            lta_background = mean(smooth_background))
#      
#   # apply hourly adjustments, add the LTA background back in
#   temp <- left_join(temp, temp_hrs, by=c("hour", "quantile_background")) %>%
#     mutate(local_conc = median_value - smooth_background,
#            median_value_adjusted = local_conc + lta_background)
#   }) %>%
#   bind_rows() %>%
#   mutate(version = paste(bh_version, "temp adj 2"))
# 
# saveRDS(visits_adj2, file.path(dt_pt, "bh_visits_background_temporal_adj.rds")) 
# 
# annual_adj2 <- visits_adj2 %>%
#   group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
#   summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
#   ungroup() 
# 
# # temp file
# saveRDS(annual_adj2, file.path(dt_pt, "bh_site_avgs_background_temporal_adj.rds"))




