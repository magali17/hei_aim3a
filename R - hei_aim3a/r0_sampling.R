 
########################################################################################################
# SETUP
########################################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
    res <- suppressWarnings(
        lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
               detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

pacman::p_load(knitr, data.table, ggplot2, tidyverse, kableExtra, lubridate, ggpubr, tsibble, DescTools,
              parallel, future.apply, purrr,
              psych, sf, tmap, mapview #added 2/29/24
              )

dt_path <- file.path("data", "onroad", "annie", "Additional Sampling")
#source("./TRAP_rotation/code/ad_functions.R")

#save updated estimates
new_dt_pt <- file.path("data", "onroad", "annie", "v2")
if(!dir.exists(file.path(new_dt_pt, "design_samples"))){dir.create(file.path(new_dt_pt, "design_samples"), recursive = T)}

core_count <- 4
set.seed(21)
########################################################################################################
# COMMON VARIABLES
########################################################################################################
sim_n <- 2#30
all_hours <- c(0:23)
business_hours <- c(9:17)

all_days <- c("weekday", "weekend")
business_days <- "weekday"

visit_count2 <- 12
visit_count1 <- 4

# --> ??
# visit_med2 <- log(12)
# visit_med1 <- log(4)

visit_sd <- 2 # use log(visit_sd)

adjusted_vars <- c("adjusted", "unadjusted")
########################################################################################################
# COMMON FUNCTIONS
########################################################################################################
log_normal_sample <- function(med_log, sd_log = visit_sd, max_value=28) {
  value <- rlnorm(1, med_log, sd_log) %>%
    round()
  # set min/max # of samples
  value <- ifelse(value==0, 1, value)
  value <- min(value, max_value)
  
  value <- as.integer(value)
  return(value)
}

########################################################################################################
# LOAD DATA
########################################################################################################\

cov_mm <- readRDS(file=file.path("/projects/rad/Transfer/Magali/hei_aim3a/R - hei_aim3a/data/onroad/annie/OnRoad Paper Code Data/data/", "cov_onroad_preprocessed.rds")) %>% 
  select(id, latitude, longitude)

#Adjusted PNC Medians 
adj_pnc_med <- readRDS(file = file.path("data", "onroad", "annie", "Additional Sampling", "AdjPNCMedian.rds")) %>% 
  mutate(dow = lubridate::wday(date, label=TRUE, abbr=FALSE),
         dow2 = ifelse(dow %in% c("Saturday", "Sunday"), "weekend", "weekday"),
         adjusted = "adjusted"
         )  

#Unadjusted PNC Medians
unadj_pnc_med <- readRDS(file = file.path("data", "onroad", "annie", "Additional Sampling", "UnadjPNCMedian.rds")) %>% 
  group_by(id) %>% 
  mutate(visit_num = n(),
         adjusted = "unadjusted"
         )

#combine unadjusted and adjusted
pnc_med <- bind_rows(adj_pnc_med, unadj_pnc_med)



segment_clusters <- readRDS(file.path("data", "onroad", "annie", "segment_clusters_updated.rds")) %>%
  select(id=location, contains("cluster")) 

segment_clusters_l <- segment_clusters %>%
  pivot_longer(cols = contains("cluster"), names_to = "cluster_type", values_to = "cluster_value")
   
road_type <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
  distinct(id, road_type) %>%
  filter(road_type != "A!")


########################################################################################################
# LOCATION ANNUAL AVGS & VARIABILITY
########################################################################################################\
unadj_pnc_summary = unadj_pnc_med %>% 
  group_by(id) %>% 
  summarise(annual_avg = mean(median_value, na.rm=T), 
            sd = sd(median_value, na.rm=T))

adj_pnc_summary = adj_pnc_med %>% 
  group_by(id) %>% 
  summarise(annual_avg = mean(median_value, na.rm=T), 
            sd = sd(median_value, na.rm=T))

unadj_pnc_summary_map = unadj_pnc_summary %>% 
  inner_join(segment_clusters) %>% 
  left_join(cov_mm) %>%
  
  pivot_longer(cols = contains("cluster"), names_to = "cluster_type", values_to = "cluster_value") %>%
  
  mutate(cluster_value = factor(cluster_value))

# pts = st_as_sf(unadj_pnc_summary_map, coords = c("longitude","latitude"),  crs = 4326)
# 
# tm_shape(pts) +
#   tm_dots(col = "cluster")
# 
# # --> adds background map; can zoom in
# mapview(pts, zcol="cluster")


#rank order clusters by average annual average OR average SD; then rank order lognormal dist and assign that way
cluster_rank = unadj_pnc_summary_map %>%
  group_by(cluster_type, cluster_value) %>% 
  summarise(avg_ann_avg = mean(annual_avg, na.rm=T),
            avg_sd = mean(sd, na.rm=T)) %>% 
  arrange(desc(avg_ann_avg)) %>% 
  mutate(ann_avg_rank = row_number()) %>% 
  arrange(desc(avg_sd)) %>% 
  mutate(sd_avg_rank = row_number())

##cluster road type 
cluster_road_type = segment_clusters_l %>% 
  left_join(road_type) %>% 
  group_by(cluster_type, cluster_value) %>% 
  mutate(Total = n()) %>% 
  ungroup() %>% 
  group_by(cluster_type, cluster_value, road_type, Total) %>% 
  summarise(Freq = n()) %>% 
  mutate(percent = Freq/Total*100) %>% 
  pivot_wider(id_cols = c(cluster_type, cluster_value), 
              names_from = "road_type", values_from = "percent") 

assign_rank1 = cluster_road_type %>% 
  filter(!is.na(A2)) %>% 
  group_by(cluster_type, cluster_value) %>%
  arrange(desc(A2)) %>% 
  group_by(cluster_type) %>%
  mutate(rank = row_number())# %>% 
  select(cluster_type, cluster_value, rank)
 
assign_rank2 = cluster_road_type %>% 
  filter(is.na(A2)) %>% 
  group_by(cluster_type, cluster_value) %>%
  arrange(desc(A3)) %>%  
  ungroup() %>% 
  group_by(cluster_type) %>%
  mutate(rank = row_number()) %>% 
  mutate(rank = rank+20) %>% 
  select(cluster_type, cluster_value, rank)

cluster_road_type_rank = assign_rank1 %>% 
  bind_rows(assign_rank2)

########################################################################################################
# DESIGNS WITHOUT SPATIAL STRUCTURE
########################################################################################################

# No Spatial Structure designs

# --> add group by id, clusters?

sampling_combos <-expand.grid(
  adjusted = adjusted_vars,
  visit_count = c(visit_count2, visit_count1),
  balanced = c("balanced", "unbalanced"),
  hours = c("all hours", "business hours")
  )

################################################################################################
# function samples a dataset and returns random samples (rows) following various designs

# df <- pnc_med %>%
#     filter(adjusted == temp$adjusted,
#            hour %in% sampling_hours,
#            dow2 %in% sampling_days)
# visit_count <- temp$visit_count
# balanced <- temp$balanced

one_sample = function(df, 
                      visit_count,
                      balanced) {

  if(balanced=="balanced") {
    one_sample = df %>%
    group_by(id) %>% 
    slice_sample(n= visit_count, replace=T) %>% 
    mutate(actual_visits = n())}
  
  if(balanced=="unbalanced") {
    # randomly select samples for each location at a time
    one_sample = lapply(group_split(ungroup(df), id), function(u){
      slice_sample(u, 
                   n = log_normal_sample(med=log(visit_count), sd=log(visit_sd)),
                   replace=T) %>% 
    mutate(actual_visits = n())}) %>%
      bind_rows()

  } 
  # #calculate annual average
  # if(get_annual_avg == TRUE) {
  #   one_sample <- one_sample %>%
  #     summarise(annual_mean = mean(median_value, na.rm=T))
  # }
  return(one_sample)
}

many_times <- function(sims=sim_n, df, ...) {
  mclapply(1:sims, mc.cores = core_count, function(s) {
    one_sample(df, ...) %>%
      mutate(campaign = s)
    }) %>%
    bind_rows()
  }

################################################################################################
# x=16
# --> could separate each design & set a unique seed for each "design"
set.seed(1)
nonspatial_visit_samples <- lapply(1:nrow(sampling_combos), 
                                  #8:9,
                                   function(x) {
  temp <- sampling_combos[x,]
  
  message("running:")
  message(temp)
  
  # all vs business hours/days
  if(temp$hours == "all hours"){
     sampling_hours <- all_hours
     sampling_days <- all_days} 
  
  if(temp$hours == "business hours"){
     sampling_hours <- business_hours
     sampling_days <- business_days} 
  
  my_samples <- pnc_med %>%
    filter(adjusted == temp$adjusted,
           hour %in% sampling_hours,
           dow2 %in% sampling_days) %>%
    many_times(df = ., visit_count = temp$visit_count, balanced = temp$balanced) %>%
    mutate(
      adjusted = temp$adjusted,
      design = temp$balanced,
      visits = paste0(temp$visit_count, " visits"), #approximate visit count for unbalanced designs
      version = temp$hours)
  
}) %>%
  bind_rows() %>%
  ungroup()

saveRDS(nonspatial_visit_samples, file = file.path(new_dt_pt, "design_samples", "nonspatial_visit_samples.rds"))


# ---> START HERE


########################################################################################################
# CLUSTERED SAMPLING DESIGNS
########################################################################################################

clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)  

set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
visit_nums = rlnorm(63, log(4), log(2))
visit_nums = round(visit_nums, digits=0)

one_sample_avg_unb_cluster = function(df) {
  one_sample = df %>%
    left_join(clusters) %>% 
    group_by(cluster) %>% 
    mutate(weight = sample(visit_nums, 1)) 
 
   sample_days = one_sample %>% 
     select(date, cluster, weight) %>% 
     distinct() %>% 
     group_by(cluster, weight) %>% 
     nest() %>% 
     mutate(samp = map2(data, weight, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "weight")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T)) %>% 
     ungroup() 
  
  return(one_sample_final)
}



unbal_cluster_unadj <- future_replicate(n = sim_n,
                           #future.label = TRUE,
                           simplify = F,
                           expr = one_sample_avg_unb_cluster(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    #group_by(id) %>%
    mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "clustered", 
      visits = "median 4 visits",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


  
unbal_cluster_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_cluster(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    #group_by(id) %>%
    mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "clustered", 
      visits = "median 4 visits",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>%
    mutate(dif = id - lag(id)) %>%
    mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_cluster_design = rbind(unbal_cluster_unadj, unbal_cluster_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_cluster_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_cluster.rds"))
```

# Pre-defined site clustering: business hours   
Randomly pick number of visits per CLUSTER from the lognormal distribution  
Pick drive days to apply to the entire cluster.

```{r}
clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)  

set.seed(21)
business_hours <- c(9:17)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
#visit_nums = rlnorm(50, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

one_sample_avg_unb_cluster_bh = function(df) {
  one_sample = df %>%
    filter(dow2 == "weekday", hour %in% business_hours) %>%
    left_join(clusters) %>% 
    group_by(cluster) %>% 
    mutate(weight = sample(visit_nums, 1)) 
 
   sample_days = one_sample %>% 
     select(date, cluster, weight) %>% 
     distinct() %>% 
     group_by(cluster, weight) %>% 
     nest() %>% 
     mutate(samp = map2(data, weight, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "weight")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_cluster_bh_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_cluster_bh(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    #group_by(id) %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "clustered", 
      visits = "median 4 visits",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_cluster_bh_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_cluster_bh(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    #group_by(id) %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "clustered", 
      visits = "median 4 visits",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_cluster_bh_design = rbind(unbal_cluster_bh_unadj, unbal_cluster_bh_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_cluster_bh_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_cluster_bh.rds"))
```


# Spatial structure: Model 1  
Using the pre-defined site clusters, sample from the lognormal distribution to create spatial structure in the #visits per site, using the same drive day algorithm.  

Model 1: High number of visits along chunks/routes where we expect a lot of exposure contrast; fewer where we don't. Assign clusters with high annual averages and high SD to higher number of visits. 

```{r}
clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)

set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
#visit_nums = rlnorm(56, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

visit_nums_rank = as.data.frame(visit_nums) %>% 
  arrange(desc(visit_nums)) %>% 
  mutate(rank = row_number())

cluster_rank_select = cluster_rank %>% 
  select(cluster, ann_avg_rank) %>% 
  left_join(visit_nums_rank, by=c("ann_avg_rank" = "rank"))
 

one_sample_avg_unb_spatial = function(df) {
  
  one_sample = df %>%
    left_join(clusters) %>% 
    left_join(cluster_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    #group_by(id) %>%
    mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "sensible spatial", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "sensible spatial", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial_design = rbind(unbal_spatial_unadj, unbal_spatial_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_spatial_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial.rds"))

```


# Spatial structure: Model 1, business hours   
Using the pre-defined site clusters, sample from the lognormal distribution to create spatial structure in the #visits per site, using the same drive day algorithm.  

Model 1: High number of visits along chunks/routes where we expect a lot of exposure contrast; fewer where we don't. Assign clusters (or full routes?) with high annual averages and high SD to higher number of visits. 

```{r}
clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)


set.seed(21)
business_hours <- c(9:17)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#visit_nums = rlnorm(56, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

#visit_nums_rank = as.data.frame(visit_nums) %>% 
#  arrange(desc(visit_nums)) %>% 
#  mutate(rank = row_number())

cluster_rank_select = cluster_rank %>% 
  select(cluster, ann_avg_rank) %>% 
  left_join(visit_nums_rank, by=c("ann_avg_rank" = "rank"))
 

one_sample_avg_unb_spatial_bh = function(df) {
  
  one_sample = df %>%
    filter(dow2 == "weekday", hour %in% business_hours) %>%
    left_join(clusters) %>% 
    left_join(cluster_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial_bh_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_bh(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "sensible spatial", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_spatial_bh_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_bh(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "sensible spatial", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial_design_bh = rbind(unbal_spatial_bh_unadj, unbal_spatial_bh_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_spatial_design_bh, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial_bh.rds"))

```


# Spatial structure: Model 2  
Using the pre-defined site clusters, sample from the lognormal distribution to create spatial structure in the #visits per site, using the same drive day algorithm.  

Model 2: High number of visits along chunks/routes where we expect a lot of exposure contrast; fewer where we don't. Assign clusters  with high annual averages and high SD to LOWER number of visits. 

```{r}
clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)

set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
visit_nums = rlnorm(63, log(4), log(2))
visit_nums = round(visit_nums, digits=0)

visit_nums_rank = as.data.frame(visit_nums) %>% 
  arrange(visit_nums) %>% 
  mutate(rank = row_number())

cluster_rank_select = cluster_rank %>% 
  select(cluster, ann_avg_rank) %>% 
  left_join(visit_nums_rank, by=c("ann_avg_rank" = "rank"))

one_sample_avg_unb_spatial2 = function(df) {
  
  one_sample = df %>%
    left_join(clusters) %>% 
    left_join(cluster_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial2_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial2(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
  mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "unsensible spatial", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_spatial2_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial2(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "unsensible spatial", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial2_design = rbind(unbal_spatial2_unadj, unbal_spatial2_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_spatial2_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial2.rds"))


```


# Spatial structure: Model 2, business hours  
Using the pre-defined site clusters, sample from the lognormal distribution to create spatial structure in the #visits per site, using the same drive day algorithm.  

Model 2: High number of visits along chunks/routes where we expect a lot of exposure contrast; fewer where we don't. Assign clusters  with high annual averages and high SD to LOWER number of visits. 

```{r}
clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)

set.seed(21)
business_hours <- c(9:17)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
#visit_nums = rlnorm(56, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

visit_nums_rank = as.data.frame(visit_nums) %>% 
  arrange(visit_nums) %>% 
  mutate(rank = row_number())

cluster_rank_select = cluster_rank %>% 
  select(cluster, ann_avg_rank) %>% 
  left_join(visit_nums_rank, by=c("ann_avg_rank" = "rank"))

one_sample_avg_unb_spatial2_bh = function(df) {
  
  one_sample = df %>%
    filter(dow2 == "weekday", hour %in% business_hours) %>%
    left_join(clusters) %>% 
    left_join(cluster_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial2_bh_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial2_bh(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "unsensible spatial", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_spatial2_bh_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial2_bh(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "unsensible spatial", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial2_bh_design = rbind(unbal_spatial2_bh_unadj, unbal_spatial2_bh_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_spatial2_bh_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial2_bh.rds"))


```

## Spatial Structure, Model 3: Sample by road type  

Classify clusters by dominant road type and rank order by clusters with highest # of A2s, then A3s, then A4s. Assign lognormal distribution of vistis by this order.  

```{r}

clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)

set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
#visit_nums = rlnorm(56, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

visit_nums_rank = as.data.frame(visit_nums) %>% 
  arrange(desc(visit_nums)) %>% 
  mutate(rank = row_number())

cluster_road_rank_select = cluster_road_type_rank %>% 
  left_join(visit_nums_rank)

one_sample_avg_unb_spatial_road = function(df) {
  
  one_sample = df %>%
    left_join(clusters) %>% 
    left_join(cluster_road_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial_road_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_road(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
     mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "road type", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_spatial_road_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_road(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
     mutate(
      #campaign = row_number(),
      version = "all hours",
      design = "road type", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial_road_design = rbind(unbal_spatial_road_unadj, unbal_spatial_road_adj) %>% 
  select(id, annual_mean, campaign, version, design, visits, adjusted)

saveRDS(unbal_spatial_road_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial_road.rds"))
```

## Spatial Structure, Model 3: Sample by road type, Business hours  

Classify clusters by dominant road type and rank order by clusters with highest # of A2s, then A3s, then A4s. Assign lognormal distribution of vistis by this order.  

```{r}

clusters = readRDS(file = file.path("TRAP_rotation", "data", "segment_clusters.rds")) %>% 
  select(id, cluster)

set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
#visit_nums = rlnorm(56, log(4), log(2))
#visit_nums = round(visit_nums, digits=0)

visit_nums_rank = as.data.frame(visit_nums) %>% 
  arrange(desc(visit_nums)) %>% 
  mutate(rank = row_number())

cluster_road_rank_select = cluster_road_type_rank %>% 
  left_join(visit_nums_rank)

one_sample_avg_unb_spatial_road_bh = function(df) {
  
  one_sample = df %>% 
    filter(dow2 == "weekday", hour %in% business_hours) %>%
    left_join(clusters) %>% 
    left_join(cluster_road_rank_select) %>% 
    ungroup()
 
   sample_days = one_sample %>% 
     select(date, cluster, visit_nums) %>% 
     distinct() %>% 
     filter(!is.na(cluster)) %>% 
     group_by(cluster, visit_nums) %>% 
     nest() %>% 
     mutate(samp = map2(data, visit_nums, sample_n, replace = T)) %>% 
     select(-data) %>% 
     unnest(samp)
   
   one_sample_final = one_sample %>% 
     ungroup() %>% 
     inner_join(sample_days, by=c("date", "cluster", "visit_nums")) %>% 
     group_by(id) %>% 
     summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample_final)
}


unbal_spatial_road_bh_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_road_bh(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "road type", 
      visits = "median 4",
      adjusted = "unadjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))

unbal_spatial_road_bh_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb_spatial_road_bh(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    mutate(
      #campaign = row_number(),
      version = "business hours",
      design = "road type", 
      visits = "median 4",
      adjusted = "adjusted"
    ) %>%
    as.data.frame() %>% 
  mutate(dif = id - lag(id)) %>% 
  mutate(campaign = accumulate(dif[-1], .init = 1,
                            ~ if(.y < 0) {
                              .x + 1
                            } else {
                              .x
                            }))


unbal_spatial_road_bh_design = rbind(unbal_spatial_road_bh_unadj, unbal_spatial_road_bh_adj)

saveRDS(unbal_spatial_road_bh_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbal_spatial_road_bh.rds"))
```

 
```{r}
#combine spatial datasets

save_dir = "//projects/trap/transfer/annie/OnroadAnnAvgs"

PNC_spatial = unbal_cluster_design %>% 
  bind_rows(unbal_cluster_bh_design, 
            unbal_spatial_design, 
            unbal_spatial_design_bh,
            unbal_spatial2_design, 
            unbal_spatial2_bh_design, 
            unbal_spatial_road_design, 
            unbal_spatial_road_bh_design) %>% 
  select(-dif)

saveRDS(PNC_spatial, file=file.path(save_dir, "PNC_spatial_annavgs.rds"))


```



**OLD**  

## Design 1: Sample 6 or 12 number of visits per site

```{r}
#try for 1 campaign
# one_sample = unadj_pnc_med %>%
#   group_by(id, visit_num) %>% 
#   nest() %>%            
#   ungroup() %>% 
#   mutate(n = round(visit_num/2, 0)) %>%
#   mutate(samp = map2(data, n, sample_n)) %>% 
#   select(-data) %>%
#   unnest(samp) %>% 
#   group_by(id) %>% 
#   mutate(visits = n()) %>%
#     #calculate annual average
#   summarise(annual_mean = mean(median_value, na.rm=T))


one_sample_avg = function(df) {
  one_sample = df %>%
  group_by(id, visit_num) %>% 
  nest() %>%            
  ungroup() %>% 
  mutate(n = round(visit_num/2, 0)) %>%
  mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>%
  unnest(samp) %>% 
  group_by(id) %>% 
  mutate(visits = n()) %>%
    #calculate annual average
  summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample)
}
  



half_visits_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "unadjusted",
      design = "half visits"
    ) %>%
    as.data.frame()

half_visits_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "adjusted",
      design = "half visits"
    ) %>%
    as.data.frame()


half_visits = rbind(half_visits_unadj, half_visits_adj)

saveRDS(half_visits, file = file.path("TRAP_rotation", "data", "PNC_annavg_halfvisits.rds"))
  
```


## Unbalanced design 1  
Sample all segments, where the visits sampled is a skewed distribution, but same number within route  

```{r}
set.seed(21)
#visits=12
#create lognormal distribution of visits to sample from by route
#median for one is 12, for another is 6

#GM = median = 6; geometric SD = 2
visit_nums = rlnorm(50, log(6), log(2))
visit_nums = round(visit_nums, digits=0)

#try for 1 campaign
# one_sample = unadj_pnc_med %>%
#   mutate(route = substr(runname,12, 14)) %>% 
#   group_by(route) %>% 
#   mutate(weight = sample(visit_nums, 1)) %>% 
#   group_by(id, weight) %>%
#   nest() %>%
#   ungroup() %>%
#   mutate(samp = map2(data, weight, sample_n, replace=T)) %>%
#   select(-data) %>%
#   unnest(samp) %>%
#   group_by(id) %>%
#   mutate(visits = n()) %>%
#     #calculate annual average
#   summarise(annual_mean = mean(median_value, na.rm=T))

one_sample_avg_unb = function(df) {
  one_sample = df%>%
    mutate(route = substr(runname,12, 14)) %>% 
    group_by(route) %>% 
    mutate(weight = sample(visit_nums, 1)) %>% 
    group_by(id, weight) %>%
    nest() %>%
    ungroup() %>%
    mutate(samp = map2(data, weight, sample_n, replace=T)) %>%
    select(-data) %>%
    unnest(samp) %>%
    group_by(id) %>%
    mutate(visits = n()) %>%
      #calculate annual average
    summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample)
}


unbal_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "unadjusted",
      design = "skewed visits"
    ) %>%
    as.data.frame()

unbal_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "adjusted",
      design = "skewed visits"
    ) %>%
    as.data.frame()


unbal_design = rbind(unbal_unadj, unbal_adj)

saveRDS(unbal_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbalanced.rds"))
```


## Unbalanced design 2  
Sample all segments among only 4 routes, where the visits sampled is a skewed distribution, but same number within route 
4 routes: 1 (downtown); 4 (Airport and south); 7 (north/residential); 5 (bellevue)

```{r}

visits=12
#create skewed distribution of visits to sample from by route
visit_nums = c(1, 2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,7,7,7,7,
               8,8,8,8,8,9,9,9,10,10,10,11,12,12,12,12,12,13,14,15, 
               15, 16,17, 17, 18, 19, 20)


one_sample_avg_unb2 = function(df) {
  one_sample = df%>%
    mutate(route = substr(runname,12, 14)) %>% 
    filter(route %in% c("R01", "R04", "R05", "R07")) %>% 
    group_by(route) %>% 
    mutate(weight = sample(visit_nums, 1)) %>% 
    group_by(id, weight) %>%
    nest() %>%
    ungroup() %>%
    mutate(samp = map2(data, weight, sample_n, replace=T)) %>%
    select(-data) %>%
    unnest(samp) %>%
    group_by(id) %>%
    mutate(visits = n()) %>%
      #calculate annual average
    summarise(annual_mean = mean(median_value, na.rm=T))
  
  return(one_sample)
}


unbal2_unadj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb2(unadj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "unadjusted",
      design = "skewed visits, spatially clustered"
    ) %>%
    as.data.frame()

unbal2_adj <- future_replicate(n = sim_n,
                           simplify = F,
                           expr = one_sample_avg_unb2(adj_pnc_med), 
                           mc.cores = 4, 
                           ) %>%
    bind_rows() %>%
    group_by(id) %>%
    mutate(
      campaign = row_number(),
      version = "all hours",
      adjusted = "adjusted",
      design = "skewed visits, spatially clustered"
    ) %>%
    as.data.frame()


unbal2_design = rbind(unbal2_unadj, unbal2_adj)

saveRDS(unbal2_design, file = file.path("TRAP_rotation", "data", "PNC_annavg_unbalanced_spatial.rds"))
```
