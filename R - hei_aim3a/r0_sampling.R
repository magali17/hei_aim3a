 
########################################################################################################
# SETUP
########################################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
    res <- suppressWarnings(
        lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
               detach, character.only=TRUE, unload=TRUE, force=TRUE))}

# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

pacman::p_load(#knitr, data.table, ggplot2, kableExtra,
               tidyverse,
               #lubridate, 
               ggpubr, #ggarrange()
               tsibble, DescTools,
               parallel, #future.apply, purrr,
               #psych, 
               sf#, tmap, mapview #added 2/29/24
               )

dt_path <- file.path("data", "onroad", "annie", "Additional Sampling")

#save updated estimates
new_dt_pt <- file.path("data", "onroad", "annie", "v2")
if(!dir.exists(file.path(new_dt_pt))){dir.create(file.path(new_dt_pt), recursive = T)}
if(!dir.exists(file.path(new_dt_pt, "visits"))){dir.create(file.path(new_dt_pt, "visits"), recursive = T)}

image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
if(!dir.exists(file.path(image_path, "SI"))){dir.create(file.path(image_path, "SI"), recursive = T)}

# --> UPDATE
core_count <- 1 #4
set.seed(21)

# # should QAQC stuff be run/saved (takes longer)?
# qaqc_eval <- FALSE
########################################################################################################
# COMMON VARIABLES
########################################################################################################

# --> UPDATE
sim_n <- 30

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

project_crs <- 4326  #lat/long

########################################################################################################
# COMMON FUNCTIONS
########################################################################################################
# med=log(visit_count)
# sd=log(visit_sd)
# n = cluster_n
# max_value=28

#med and sd should be on the log-scale
log_normal_sample <- function(med, sd = visit_sd, max_value=28, n=1) {
  value <- rlnorm(n, med, sd) %>%
    round()
  # set min/max # of samples
  value <- ifelse(value<1, 1, value)
  value <- ifelse(value>max_value, max_value, value) #min(value, max_value) 
  
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
         adjusted = "adjusted")  

#Unadjusted PNC Medians
unadj_pnc_med <- readRDS(file = file.path("data", "onroad", "annie", "Additional Sampling", "UnadjPNCMedian.rds")) %>% 
  group_by(id) %>% 
  mutate(visit_num = n(),
         adjusted = "unadjusted")

#combine unadjusted and adjusted
## 5874 unique segments
pnc_med <- bind_rows(adj_pnc_med, unadj_pnc_med) %>% 
  ungroup()

saveRDS(pnc_med, file.path(new_dt_pt, "pnc_med.rds"))

# save segments used
## 5874 segments
pnc_med %>%
  distinct(id) %>%
  pull(id) %>%
  saveRDS(., file.path(new_dt_pt, "ids_included.rds"))

# filter(test, id==534) %>% View()
road_type <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
  filter(
    # use segments w/ median estimates from prior work
    id %in% pnc_med$id # 5874 road segments kept
    ) %>%
  distinct(id, road_type) 

segment_clusters_l <- readRDS(file.path("data", "onroad", "annie", "segment_clusters_updated.rds")) %>%
  select(id=location, contains("cluster")) 

segment_clusters <- segment_clusters_l %>%
  pivot_wider(names_from = "cluster_type", values_from = "cluster_value") #%>%
  # cluster3 drops some segments
  #drop_na()
   
########################################################################################################
# LOCATION ANNUAL AVGS & CLUSTER RANKINGS
########################################################################################################
unadj_pnc_summary <- unadj_pnc_med %>% 
  group_by(id) %>% 
  summarise(annual_avg = mean(median_value, na.rm=T), 
            sd = sd(median_value, na.rm=T))

adj_pnc_summary <- adj_pnc_med %>% 
  group_by(id) %>% 
  summarise(annual_avg = mean(median_value, na.rm=T), 
            sd = sd(median_value, na.rm=T))

unadj_pnc_summary_map <- unadj_pnc_summary %>% 
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


# rank clusters  
##cluster road type 
cluster_road_type_rank <- segment_clusters_l %>% 
  left_join(road_type) %>% 
  group_by(cluster_type, cluster_value) %>% 
  mutate(segments_in_cluster = n()) %>%# View()
  ungroup() %>% 
  group_by(cluster_type, cluster_value, road_type, segments_in_cluster) %>% 
  summarize(road_type_n = n()) %>%  
  mutate(#percent = freq/segments_in_cluster*100
    road_type_prop = road_type_n/segments_in_cluster,
    road_type_weight = ifelse(road_type=="A2", 0.60,
                              ifelse(road_type=="A3", 0.30, 
                                     ifelse(road_type=="A4", 0.10))),
    road_type_prop_weight = road_type_prop*road_type_weight) %>% 
  group_by(cluster_type, cluster_value) %>%
  summarize(rank0 = sum(road_type_prop_weight)) %>% 
  group_by(cluster_type) %>%
  arrange(desc(rank0)) %>%
  mutate(rank=row_number()) %>%
  ungroup() %>%
  select(cluster_type, cluster_value, road_type_rank=rank)
   
  
 
# assign_rank1 <- cluster_road_type %>% 
#   # clusters w A2s
#   filter(!is.na(A2)) %>% 
#   group_by(cluster_type#, cluster_value
#            ) %>%
#   arrange(desc(A2)) %>% 
#   #group_by(cluster_type) %>%
#   mutate(rank = row_number())  
 
cluster1_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster1") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)
cluster2_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster2") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)
cluster3_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster3") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)
 
# assign_rank2 <- cluster_road_type %>% 
#   # clusters w/o a prior rank
#   filter(is.na(A2)) %>% 
#   group_by(cluster_type#, cluster_value
#            ) %>%
#   arrange(desc(A3)) %>%  
#   #ungroup() %>% 
#   #group_by(cluster_type) %>%
#   mutate(rank = row_number(), 
#          #rank = rank+20
#          
#     # --> UPDATE CLUSTER TO CLUSTER1  
#          rank = ifelse(cluster_type=="cluster", rank + cluster1_max_rank,
#                        ifelse(cluster_type=="cluster2", rank + cluster2_max_rank,
#                               ifelse(cluster_type=="cluster3", rank + cluster3_max_rank, NA)))
#          ) 
# 
# cluster_road_type_rank <- bind_rows(assign_rank1, assign_rank2) %>% 
#   select(cluster_type, cluster_value, road_type_rank=rank)


# rank order clusters by average annual average 
## lower numbers are prioritized for more visits
cluster_rank <- unadj_pnc_summary_map %>%
  group_by(cluster_type, cluster_value) %>% 
  summarise(avg_ann_avg = mean(annual_avg, na.rm=T)) %>%
  group_by(cluster_type) %>%
  arrange(desc(avg_ann_avg)) %>% 
  mutate(sensible_rank = row_number()) %>% 
  arrange(avg_ann_avg) %>% 
  mutate(unsensible_rank = row_number(),
          cluster_value = as.numeric(as.character(cluster_value))) %>%  
  # add road type rank
  left_join(cluster_road_type_rank) %>%
  ungroup()

saveRDS(cluster_rank, file.path(new_dt_pt, "cluster_ranks.rds"))

cluster_rank1 <- cluster_rank %>%
  select(-avg_ann_avg) %>%
  pivot_longer(cols = c(contains("rank")), names_to = "cluster_approach", values_to = "rank") %>%
  mutate(cluster_approach = gsub("_rank", "", cluster_approach))

########################################################################################################
# visualize cluster rankings
cluster_rank2 <- cluster_rank %>%
  pivot_longer(cols = c(avg_ann_avg, 
                        contains("rank"))) %>% 
  left_join(segment_clusters_l, #relationship = "one-to-many" # error w/ row 12313?
            ) %>% 
  left_join(cov_mm) 

lapply(unique(cluster_rank2$name), function(x) {
  cluster_rank2 %>%
    filter(#cluster_type==x
      name==x
           ) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
    ggplot(aes(col=value)) + 
    geom_sf() + 
    facet_wrap(~name+cluster_type) +
    theme_bw() + 
    theme(panel.grid.major = element_blank()
          ) + 
    labs(#col="Rank"
         )
  }) %>%
  ggarrange(plotlist = .)

ggsave(file.path(image_path, "SI", "cluster_road_type_map.png"), width = 8, height = 12)

########################################################################################################
# DESIGNS WITHOUT SPATIAL STRUCTURE
########################################################################################################

# No Spatial Structure designs
sampling_combos <- expand.grid(
  adjusted = adjusted_vars,
  visit_count = c(visit_count2, visit_count1),
  balanced = c("balanced", "unbalanced"),
  hours = c("all hours", "business hours")
  ) # %>% slice_sample(n = 10)

saveRDS(sampling_combos, file.path(new_dt_pt, "nonspatial_sampling_combo_list.rda"))

################################################################################################
# function samples a dataset and returns random samples (rows) following various designs

# df <- pnc_med %>%
#     filter(adjusted == temp$adjusted,
#            hour %in% sampling_hours,
#            dow2 %in% sampling_days)
# visit_count <- temp$visit_count
# balanced <- temp$balanced

one_campaign <- function(df, 
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
  return(one_sample)
}

many_campaigns <- function(sims=sim_n, df, ...) {
  mclapply(1:sims, mc.cores = core_count, function(s) {
    one_campaign(df, ...) %>%
      mutate(campaign = s)}) %>%
    bind_rows()
  }

################################################################################################
# x=16
# --> could separate each design & set a unique seed for each "design"
message("running non-spatially clustered analyses")

set.seed(1)
#nonspatial_visit_samples <- 
lapply(1:nrow(sampling_combos), 
                                  #8:9,
                                   function(x) {
  temp <- sampling_combos[x,]
  design_label <- paste(first(temp$adjusted), first(temp$visit_count), first(temp$balanced), first(temp$hours), sep = "_") %>%
    gsub(" ", "", .)
  
  message(paste0(capture.output(temp), collapse = "\n"))
  
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
    many_campaigns(df = ., visit_count = temp$visit_count, balanced = temp$balanced) %>%
    mutate(
      adjusted = temp$adjusted,
      design = temp$balanced,
      visits = paste0(temp$visit_count, " visits"), #approximate visit count for unbalanced designs
      version = temp$hours)
    
    annual_averages <- my_samples %>%
      group_by(id, adjusted, actual_visits, campaign, design, visits, version) %>%
      summarize(annual_mean = mean(median_value, na.rm=T)) %>%
      ungroup()
    
    message("saving samples")
    # save separate files since this is large
    saveRDS(my_samples, file.path(new_dt_pt, "visits", paste0("visits_nonspatial_", design_label, ".rds")))
    saveRDS(annual_averages, file.path(new_dt_pt, paste0("nonspatial_site_avgs_", design_label, ".rds")))
    
  
  }) #%>%
  # bind_rows() %>%
  # ungroup()

# message("saving samples")
# saveRDS(nonspatial_visit_samples, file.path(new_dt_pt, "nonspatial_visit_samples.rds"))

# calculate annual averages
# nonspatial_site_avgs <- nonspatial_visit_samples %>%
#   group_by(id, adjusted, actual_visits, campaign, design, visits, version) %>%
#   summarize(annual_mean = mean(median_value, na.rm=T)) %>%
#   ungroup()
# 
# saveRDS(nonspatial_site_avgs, file.path(new_dt_pt, "nonspatial_site_avgs.rds"))

########################################################################################################
# UNBALANCED CLUSTERED SAMPLING DESIGNS
########################################################################################################
sampling_combos_random_clusters <-expand.grid(
  adjusted = adjusted_vars,
  visit_count = c(visit_count2, visit_count1),
  balanced = c("unbalanced"),  
  hours = c("all hours", "business hours"),
  #design = c("clustered", "sensible spatial", "unsensible spatial", "road type")
  cluster_approach = c("random", "sensible", "unsensible", "road_type"),
  cluster_type = unique(segment_clusters_l$cluster_type)
  ) # %>% slice_sample(n = 10)

saveRDS(sampling_combos_random_clusters, file.path(new_dt_pt, "clustered_sampling_combo_list.rda"))

########################################################################################################
# df <- pnc_med_clusters %>%
#   filter(adjusted == "adjusted",
#          hour %in% c(0:23),
#          #dow2 %in% sampling_days,
#          cluster_type == "cluster2")
# visit_count <- 4
# cluster_approach. = "unsensible"

one_campaign_clustered <- function(df, visit_count, cluster_approach.) {
  
  df <- df %>% ungroup()
  
  cluster_type. <- unique(df$cluster_type) 
  cluster_n <- length(unique(df$cluster_value))
  samples_per_cluster <- log_normal_sample(med=log(visit_count), sd=log(visit_sd), n = cluster_n)
  
  cluster_ranks <- cluster_rank1 %>%
    filter(cluster_type==cluster_type.) %>%
    arrange(cluster_value)
  
  # select cluster samples
  ## random clusters
  if(cluster_approach. == "random") {
    cluster_samples <- distinct(cluster_ranks, cluster_type, cluster_value) %>%
      mutate(visit_samples = samples_per_cluster)
  } else {
    # spatially-structured clusters
    cluster_samples <- cluster_ranks %>%
      filter(cluster_approach == cluster_approach.) %>%
      arrange(rank) %>%
      mutate(visit_samples = sort(samples_per_cluster, decreasing=T))
    }
   
  cluster_samples <- select(cluster_samples, cluster_type, cluster_value, visit_samples)
  
  df <- left_join(df, cluster_samples, by=c("cluster_type", "cluster_value")) %>% 
    ungroup()
  
  # randomly select samples for each location at a time
  # u=group_split(df, id)[[1]]
  one_sample <- lapply(group_split(df, id), function(u){
      
    cluster_visits <- unique(u$visit_samples)
    
    slice_sample(u, n= cluster_visits, replace=T) %>%
      mutate(actual_visits = n())
    }) %>%
    bind_rows()
    
    return(one_sample)
  }

many_campaigns_clustered <- function(sims=sim_n, df, ...) {
  mclapply(1:sims, mc.cores = core_count, function(s) {
    one_campaign_clustered(df, ...) %>%
      mutate(campaign = s)}) %>%
    bind_rows()
}

########################################################################################################
pnc_med_clusters <- pnc_med %>%
  left_join(segment_clusters, by="id") %>% 
  pivot_longer(cols = contains("cluster"), names_to = "cluster_type", values_to = "cluster_value")


# df <- pnc_med_clusters %>%
#   filter(adjusted == "adjusted",
#          hour %in% c(0:23),
#          #dow2 %in% sampling_days,
#          cluster_type == "cluster2")

# x=5
message("running spatially clustered analyses")

set.seed(21)
#cluster_visit_samples <- 
lapply(1:nrow(sampling_combos_random_clusters), 
                                  #c(4:5,12:13),
                                        function(x) {
                                     temp <- sampling_combos_random_clusters[x,]
                                     
                                     message(paste0(capture.output(temp), collapse = "\n"))
                                     
                                     # all vs business hours/days
                                     if(temp$hours == "all hours"){
                                       sampling_hours <- all_hours
                                       sampling_days <- all_days} 
                                     if(temp$hours == "business hours"){
                                       sampling_hours <- business_hours
                                       sampling_days <- business_days} 
                                     
                                     my_samples <- pnc_med_clusters %>%
                                       filter(adjusted == temp$adjusted,
                                              hour %in% sampling_hours,
                                              dow2 %in% sampling_days,
                                              cluster_type == temp$cluster_type 
                                              ) %>% 
                                       many_campaigns_clustered(df = ., visit_count = temp$visit_count,cluster_approach.=temp$cluster_approach) %>%
                                       mutate(
                                         adjusted = temp$adjusted,
                                         design = temp$cluster_approach,
                                         visits = paste0(temp$visit_count, " visits"), #approximate visit count for unbalanced designs
                                         version = temp$hours,
                                         cluster_type = temp$cluster_type
                                         )
                                     
                                     annual_averages <- my_samples %>%
                                       group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
                                       summarize(annual_mean = mean(median_value, na.rm=T)) %>%
                                       ungroup()
                                     
                                      
                                     message("saving samples")
                                     # save separate files since this is large
                                     saveRDS(my_samples, file.path(new_dt_pt, "visits", paste0("visits_clustered_", design_label, ".rds")))
                                     saveRDS(annual_averages, file.path(new_dt_pt, paste0("site_avgs_clustered_", design_label,".rds")))
                                     
                                     
                                   })# %>%
# #   bind_rows() %>%
# #   ungroup()
# # 
# # message("saving samples")
# # 
# # saveRDS(cluster_visit_samples, file.path(new_dt_pt, "cluster_visit_samples.rds"))
# 
# # # check counts
# # cluster_visit_samples %>%
# #   filter(design==first(design)) %>%
# #   distinct(id, dow2, adjusted, cluster_type, cluster_value, visit_samples, actual_visits, campaign, design, visits, version) %>%
# #   group_by_at(vars(-id)) %>%
# #   summarize(ids = length(unique(id))) %>% View()
# 
# # calculate annual averages
# cluster_site_avgs <- cluster_visit_samples %>%
#   group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
#   summarize(annual_mean = mean(median_value, na.rm=T)) %>%
#   ungroup()
# 
# saveRDS(cluster_site_avgs, file.path(new_dt_pt, "cluster_site_avgs.rds"))

########################################################################################################
# DONE
########################################################################################################
message("DONE running r0_sampling.R")



