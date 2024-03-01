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
#all_pollutant_10s <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "all_pollutants_10s_20220125.rds"))

road_type <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
  distinct(id, road_type) %>%
  filter(road_type != "A!")

clusters <- readRDS(file.path("data", "onroad", "annie", "segment_clusters_updated.rds")) %>%
  rename(id = location)
# --> on-road segment-level samples; check that it has a 'time' variable 

#road_type <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds"))

adj_visits <- readRDS(file.path("data", "onroad", "annie", "Additional Sampling", "AdjPNCMedian.rds")) %>%
  mutate(adjusted = "adjusted")
visits <- readRDS(file.path("data", "onroad", "annie", "Additional Sampling", "UnadjPNCMedian.rds")) %>%
  mutate(adjusted = "unadjusted") %>%
  select(names(adj_visits)) %>%
  bind_rows(adj_visits) %>%
  # add road types
  left_join(road_type) %>%
  left_join(clusters)




# fixed site temporal adjustment (use the winsorized adjusted values, as before)
# this adjustment comes from 1.1_temporal_adjustment.Rmd
fixed_site_temp_adj <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox_temp_adjustment.rda")) %>%
  select(time, ufp_adjustment = diff_adjustment_winsorize)






##################################################################################################
# CLUSTERS
##################################################################################################


#rank order clusters by average annual average OR average SD; then rank order lognormal dist and assign that way
cluster_rank = visits %>% 
  filter(adjusted == "unadjusted") %>%
  group_by(id, cluster) %>% 
  summarise(annual_avg = mean(median_value, na.rm=T), 
            sd = sd(median_value, na.rm=T)) %>%
  group_by(cluster) %>% 
  summarise(avg_ann_avg = mean(annual_avg, na.rm=T),
            avg_sd = mean(sd, na.rm=T)) %>% 
  arrange(desc(avg_ann_avg)) %>% 
  mutate(ann_avg_rank = row_number()) %>% 
  arrange(desc(avg_sd)) %>% 
  mutate(sd_avg_rank = row_number())


##cluster road type 

id_road_type = all_pollutant_10s %>% 
  as.data.frame() %>% 
  select(id, road_type) %>% 
  distinct()

cluster_road_type = clusters %>% 
  left_join(id_road_type) %>% 
  group_by(cluster) %>% 
  mutate(Total = n()) %>% 
  ungroup() %>% 
  group_by(cluster, road_type, Total) %>% 
  summarise(Freq = n()) %>% 
  mutate(percent = Freq/Total*100) %>% 
  pivot_wider(id_cols = cluster, names_from = "road_type", values_from = "percent") 

assign_rank1 = cluster_road_type %>% 
  filter(!is.na(A2)) %>% 
  arrange(desc(A2)) %>% 
  ungroup() %>% 
  mutate(rank = row_number()) %>% 
  select(cluster, rank)

assign_rank2 = cluster_road_type %>% 
  filter(is.na(A2)) %>% 
  arrange(desc(A3)) %>%  
  ungroup() %>% 
  mutate(rank = row_number()) %>% 
  mutate(rank = rank+20) %>% 
  select(cluster, rank)

cluster_road_type_rank = assign_rank1 %>% 
  bind_rows(assign_rank2)



##################################################################################################
# SAMPLE VISITS 
##################################################################################################










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



