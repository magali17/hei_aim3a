# script temporally adjusts onroad data

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
               parallel #mclapply()
               #splines #, #bs() ns()
               )    

source("functions.R")
#dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
dt_pt <- file.path("data", "onroad", "annie", "v2")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
dt_out <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "qc", "road")
if(!dir.exists(dt_out)){dir.create(dt_out, recursive = T)}

set.seed(1)

use_cores <- 4
##################################################################################################
# FUNCTION
##################################################################################################
# COUNT REMAINING ROWS
missingness_table <- data.frame(n = integer(),
                                description = character())

count_missingness <- function(dt, notes) {
  temp <- data.frame(n = nrow(dt),
             description = notes)
  rbind(missingness_table, temp)
}

##################################################################################################
# DATA
##################################################################################################
message("loading data")

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
    select(-c(visit_num, runname)) 
  
  saveRDS(visits, file.path(dt_pt, "TEMP_bh_visits.rda"))
  rm(list=c("v1", "v2"))
} else {
    visits <- readRDS(file.path(dt_pt, "TEMP_bh_visits.rda"))
  }


# alternative synthetic long-term UFP monitoring site w/ the collected data
## all the data before segments are excluded: A1 roads, <5 1sec measurements/visit, <23 repeat visits
## note some measurements are repeated for same time, e.g. 2019-02-25 18:55:23 for I405 and I5 (UFP is the same)
## n= 3,688,353 (not what was reported by Annie: 3,769,325 1s measures)

if(!file.exists(file.path(dt_pt, "TEMP_road_dt.rda") | !file.exists(file.path(dt_pt, "TEMP_road_dt_no_hwy.rda")))) {
  road_dt0 <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
    arrange(time) %>%
    mutate(
      dow = wday(time, label=T, week_start = "Monday"),
      dow = factor(dow, ordered = F),
      weekday = ifelse(dow %in% c("Sat", "Sun"), "weekend", "weekday"))
  
  missingness_table <- count_missingness(road_dt0, notes="all data")
  
  # some duplicate rows. some remain if same value was assigned to 2 locations during the same second
  road_dt0 <- distinct(road_dt0)
  missingness_table <- count_missingness(road_dt0, notes="drop duplicate rows")
  
  #only keep the first segment/reading when the same reading is assigned to 2 locations/neighboring segments?
  road_dt0 <- road_dt0 %>%
    group_by(time) %>%
    slice(1) %>%
    ungroup()  
  
  saveRDS(road_dt0, file.path(dt_pt, "TEMP_road_dt0.rda"))
  
  missingness_table <- count_missingness(road_dt0, notes="only keep 1st location when same sec reading is assigned to 2")
  
  # drop hwy for some of these
  road_dt0_no_hwy <- filter(road_dt0, road_type != "A1")
  missingness_table <- count_missingness(road_dt0_no_hwy, notes="drop A1 segments")
  
  write.csv(missingness_table, file.path(dt_pt, "missing_counts_second_dt.csv"), row.names = F)
  
  time_series <- mclapply(unique(road_dt0$runname), mc.cores=use_cores, function(x){
    a_run <- filter(road_dt0, runname==x)
    data.frame(runname = x,
               time= seq(min(a_run$time), max(a_run$time), by=1))
  }) %>%
    bind_rows()
  
  # NAs in hour start here b/c of missing readings
  road_dt <- left_join(time_series, road_dt0, by=c("runname", "time")) %>%
    select(runname, time, hour, weekday, id, ufp)
  
  road_dt_no_hwy <- left_join(time_series, road_dt0_no_hwy, by=c("runname", "time")) %>%
    select(runname, time, hour, weekday, id, ufp)
  
  saveRDS(road_dt, file.path(dt_pt, "TEMP_road_dt.rda"))
  saveRDS(road_dt_no_hwy, file.path(dt_pt, "TEMP_road_dt_no_hwy.rda"))
  
  rm(list=c("time_series", "road_dt0", "road_dt0_no_hwy"))
} else {
  road_dt <- readRDS(file.path(dt_pt, "TEMP_road_dt.rda"))
  road_dt_no_hwy <- readRDS(file.path(dt_pt, "TEMP_road_dt_no_hwy.rda"))
  }


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

##################################################################################################
# VARIABLES
##################################################################################################
# e.g., 3 hr * 60 min/hr * 60 sec/min
# windows <- 60*60#c(1:3)*60*60  
# quantiles <- 0.10 #c(0.01, 0.05, 0.10, 0.20)
windows <- c(1,3)*60*60
# --> could drop 0.20
quantiles <- c(0.01, 0.05#, 0.10#, 0.20
               )

##################################################################################################
# FUNCTIONS
##################################################################################################

calculate_rolling_quantile <- function(dt, windows.=windows, quantiles.=quantiles) {
  
  adj_lvls <- apply(expand.grid(paste0("hr", sort(windows)/3600), 
                                paste0("_pct", sort(quantiles)*100)), 
                    1, paste, collapse="")
  
  dt <- lapply(windows., function(w) {
    mclapply(quantiles., mc.cores=use_cores, function(p) {
      
      bg_label <- paste0("hr", w/3600, "_pct", p*100)
      
      dt %>%
        group_by(runname) %>%
        mutate(
          background_adj = bg_label,
          # rolling quantile, ignore NAs (i.e., periods of time w/o UFP readings) may still get background estimates
          background = rollapply(ufp, width = w, FUN = quantile, prob = p, align = "center", partial = TRUE, na.rm=T))
    })
  }) %>%
    bind_rows() %>%
    mutate(background_adj = factor(background_adj, levels=adj_lvls))
}

##################################################################################################
# 1. TEMPORAL ADJUSTMENT: PSEUDO FIXED SITES (FROM PREDICTED UFP)
##################################################################################################

# if(!file.exists(file.path(dt_pt, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))) {
#   message("running fixed site temporal adjustment from predicted UFP based on NO2")
# 
#   visits_adj1 <- visits %>%
#     mutate(time = ymd_h(paste(date, hour))) %>%
#     # add temporal adjustment
#     left_join(fixed_site_temp_adj, by="time") %>%
#     mutate(median_value_adjusted = median_value + ufp_adjustment,
#            version = paste(version, "temp adj 1"))
# 
#   saveRDS(visits_adj1, file.path(dt_pt, "bh_visits_fixed_site_temporal_adj.rds"))
# 
#   annual_adj1 <- visits_adj1 %>%
#     group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
#     summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
#     ungroup()
# 
#   # temp file
#   saveRDS(annual_adj1, file.path(dt_pt, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))
# } else {
#   annual_adj1 <- readRDS(file.path(dt_pt, "TEMP_bh_site_avgs_fixed_site_temporal_adj.rds"))
# }

##################################################################################################
# # QC - check that few annual averages are <=0
# message("temp adj 1 - predicted UFP from NO2")
# summary(annual_adj1$annual_mean)
# table(annual_adj1$annual_mean<=0)
# prop.table(table(annual_adj1$annual_mean<=0))


##################################################################################################
# 2. UNDERWRITE FN: PSEUDO FIXED SITE FROM COLLECTED UFP MEASURES
##################################################################################################
message("running fixed site temporal adjustment from the collected UFP measures")

if(!file.exists(file.path(dt_pt, "underwrite_temp_adj_all_1s_data.rda"))) {
  road_dt <- calculate_rolling_quantile(dt=road_dt)
  saveRDS(road_dt, file.path(dt_pt, "underwrite_temp_adj_all_1s_data.rda"))
  } else {
    road_dt <- readRDS(file.path(dt_pt, "underwrite_temp_adj_all_1s_data.rda"))
    }

if(!file.exists(file.path(dt_pt, "underwrite_temp_adj_all_1s_data_no_hwy.rda"))) {
  road_dt_no_hwy <- calculate_rolling_quantile(dt=road_dt_no_hwy)
  saveRDS(road_dt_no_hwy, file.path(dt_pt, "underwrite_temp_adj_all_1s_data_no_hwy.rda"))
  } else {
    road_dt_no_hwy <- readRDS(file.path(dt_pt, "underwrite_temp_adj_all_1s_data_no_hwy.rda"))
    }

##################################################################################################
# note: some hours don't have UFP but still have hourly adjustments b/c rm.na=T for rolling window calculations
get_hourly_adjustment <- function(dt) {
  dt %>%
    group_by(background_adj) %>%
    mutate(bg_lta = mean(background, na.rm = T),
           date= as.Date(time)) %>%
    group_by(runname, date, hour, background_adj, bg_lta) %>%
    summarize(bg_hour_avg = mean(background, na.rm = T),
              bg_hour_median = median(background, na.rm = T)) %>%
    ungroup() %>%
    mutate(avg_hourly_adj = bg_lta - bg_hour_avg,
           median_hourly_adj = bg_lta - bg_hour_median,
           time = ymd_h(paste(date, hour))) %>%
    select(runname, date, hour, time, background_adj, bg_lta, bg_hour_avg, avg_hourly_adj, bg_hour_median, median_hourly_adj)
  }
##################################################################################################

underwrite_adj <- get_hourly_adjustment(road_dt)
underwrite_adj_no_hwy <- get_hourly_adjustment(road_dt_no_hwy)

saveRDS(underwrite_adj, file.path(dt_pt, "underwrite_temp_adj.rda"))
saveRDS(underwrite_adj_no_hwy, file.path(dt_pt, "underwrite_temp_adj_no_hwy.rda"))

# underwrite_adj <- readRDS(file.path(dt_pt, "underwrite_temp_adj.rda"))

##################################################################################################
visits_adj2 <- visits %>%
  mutate(time = ymd_h(paste(date, hour))) %>%
  # add temporal adjustment
  left_join(select(underwrite_adj, time, background_adj, avg_hourly_adj), by="time") %>% 
  #group_by(background_adj) %>%
  mutate(median_value_adjusted = median_value + avg_hourly_adj,
         version = paste(version, "temp adj 2"))

saveRDS(visits_adj2, file.path(dt_pt, "bh_visits_fixed_site_temporal_adj_uw.rds")) 

annual_adj2 <- visits_adj2 %>%
  group_by(background_adj,
           id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup()  

saveRDS(annual_adj2, file.path(dt_pt, "TEMP_bh_site_avgs_uw_adj.rds"))


visits_adj2_no_hwy <- visits %>%
  mutate(time = ymd_h(paste(date, hour))) %>%
  # add temporal adjustment
  left_join(select(underwrite_adj_no_hwy, time, background_adj, avg_hourly_adj), by="time") %>% 
  #group_by(background_adj) %>%
  mutate(median_value_adjusted = median_value + avg_hourly_adj,
         version = paste(version, "temp adj 2"))

saveRDS(visits_adj2_no_hwy, file.path(dt_pt, "bh_visits_fixed_site_temporal_adj_uw_no_hwy.rds")) 

annual_adj2_no_hwy <- visits_adj2_no_hwy %>%
  group_by(background_adj,
           id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup()  

saveRDS(annual_adj2_no_hwy, file.path(dt_pt, "TEMP_bh_site_avgs_uw_adj_no_hwy.rds"))




# # --> UPDATE/TO DO: select 1 background adjustment; compare distributions

# selected_pct <- "hr1_pct10"
# annual_adj2 <- filter(annual_adj2, background_adj == selected_pct)
# annual_adj2_no_hwy <- filter(annual_adj2_no_hwy, background_adj == selected_pct)
# saveRDS(annual_adj2, file.path(dt_pt, "bh_site_avgs_uw_adj.rds"))

##################################################################################################
# --> QC: check that no or few annual averages are < 0

# summary(annual_adj2$annual_mean)
# prop.table(table(annual_adj2$annual_mean<=0))




##################################################################################################
# QC - INVESTIGATE NEGATIVE ANNUAL AVERAGES
##################################################################################################
# --> COMPARE CONC VISITS VS ADJUSTMENTS






##################################################################################################
# QC - VISUALIZE TEMPORAL ADJUSTMENT PATTERNS 
##################################################################################################
# TIME SERIES: ONROAD MEASURES VS 'BACKGROUND' (E.G., Brantley Fig 5)

# --> 



##################################################################################################
# --> plot percentiles: dots w/ smoother
# --> add example of ~1 wk 
# --> consider overlaying same route driven on same time…driving same structure




##################################################################################################
# # hourly adjustments
# adj_lvls <- apply(expand.grid(paste0("hr", windows/3600), 
#                               paste0("_pct", quantiles*100)), 
#                   1, paste, collapse="")
# 
# 
# underwrite_adj %>%
#   drop_na(hour) %>%
#   mutate(background_adj = factor(background_adj, levels=adj_lvls)) %>%
#   # couple of day examples
#   filter(runname %in% unique(underwrite_adj$runname)[1:6]) %>%  
#   
#   ggplot(aes(x=hour, y=avg_hourly_adj, group=background_adj, col=background_adj)) + 
#   facet_wrap(facets = vars(runname)) +
#   geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
#   geom_point() + 
#   geom_line() + 
#   scale_color_ordinal()
# 
# ggsave(file.path(dt_out, "temp_adj2_example.png"), width = 10, height = 6)
# 
# # time series
# underwrite_adj %>%
#   drop_na(hour) %>%
#   mutate(background_adj = factor(background_adj, levels=adj_lvls)) %>%
#   
#   ggplot(aes(x=time, y=avg_hourly_adj)) + 
#   facet_wrap(facets = vars(background_adj)) +
#   geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
#   geom_point(alpha=0.4, aes(col=background_adj)) + 
#   geom_smooth()
# 
# ggsave(file.path(dt_out, "temp_adj2_time_series.png"), width = 14, height = 8)
# 



##################################################################################################
# QC - VISUALIZE SPATIAL ADJUSTMENT PATTERNS 
##################################################################################################
# maps to make sure background doesnt vary by space





##################################################################################################
# SAVE DATA
################################################################################################
# message("saving final files")
# rbind(annual_adj1, annual_adj2) %>%
#   saveRDS(., file.path(dt_pt, "temp_adj_site_avgs.rds"))

##################################################################################################
# DONE
##################################################################################################
message("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")






##################################################################################################
# APPENDIX
##################################################################################################

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
#   mutate(version = paste(version, "temp adj 2"))
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




