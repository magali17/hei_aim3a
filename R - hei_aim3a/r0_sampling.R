# script samples visit-level on-road data following different sampling designs; calculates segment annual averages

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

pacman::p_load(tidyverse,
               ggpubr, #ggarrange()
               tsibble, DescTools,
               parallel, #future.apply, purrr,
               sf#, tmap, mapview #added 2/29/24
               )

dt_path <- file.path("data", "onroad", "annie", "Additional Sampling")

#save updated estimates
new_dt_pt <- file.path("data", "onroad", "annie", "v2")
if(!dir.exists(file.path(new_dt_pt))){dir.create(file.path(new_dt_pt), recursive = T)}

design_types <- c("nonspatial", "clustered", "routes")
saveRDS(design_types, file.path(new_dt_pt, "design_types_list.rds"))

lapply(design_types, function(f){if(!dir.exists(file.path(new_dt_pt, "visits", f))){dir.create(file.path(new_dt_pt, "visits", f), recursive = T)}})
lapply(design_types, function(f){if(!dir.exists(file.path(new_dt_pt, "site_avgs", f))){dir.create(file.path(new_dt_pt, "site_avgs", f), recursive = T)}})

image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
if(!dir.exists(file.path(image_path, "SI"))){dir.create(file.path(image_path, "SI"), recursive = T)}

core_count <- 4 #6 sometimes crashes clustered sampling?
set.seed(21)

########################################################################################################
# COMMON VARIABLES
########################################################################################################
sim_n <- 30

all_hours <- c(0:23)
business_hours <- c(9:17)
#minimum proportion of a route that should be within BHs for it to be considered a BH route
bsns_coverage_threshold <- 0.6

all_days <- c("weekday", "weekend")
business_days <- "weekday"

visit_count2 <- 12
visit_count1 <- 4
 
visit_sd <- 2 # use log(visit_sd)

adjusted_vars <- c("adjusted", "unadjusted")

project_crs <- 4326  #lat/long

########################################################################################################
# COMMON FUNCTIONS
########################################################################################################
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
cov_mm <- readRDS(file=file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "cov_onroad_preprocessed.rds")) %>% 
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

###################################
# QC: routes
## 443 segments/locations are linked to more than one route
unadj_pnc_med %>%
  mutate(route = substr(runname, 12,14)) %>%
  group_by(id) %>%
  summarize(routes = paste(unique(route), collapse = ", "),
            n = length(unique(route))) %>%
  #locations sampled on more than one route
  filter(n>1) %>%
  nrow()

# id's per run
##
segments_visited_per_run <- unadj_pnc_med %>%
  group_by(runname) %>%
  summarize(n = length(unique(id))) %>%
  summarize(no_runs=n(),
            min = min(n),
            q05 = quantile(n, 0.05),
            q10 = quantile(n, 0.10),
            q25 = quantile(n, 0.25),
            mean = mean(n),
            q75 = quantile(n, 0.75),
            q90 = quantile(n, 0.90),
            q95 = quantile(n, 0.95),
            max = max(n))

# drop routes with few visits (e.g., makeup routes?)
routes_to_sample <- unadj_pnc_med %>%
  mutate(bsns_hours = ifelse(dow2 == business_days & hour %in% business_hours, 1, 0)) %>%
  group_by(runname) %>%
  summarize(no_ids = length(id),
            no_unique_ids = length(unique(id)),
            # proportion of segments visited during business hours
            proportion_bsns_hours = mean(bsns_hours)
            ) %>% #filter(proportion_bsns_hours>=0.5) %>% View()
  # drop runs with a low # of unique segments visits (<the 10th percentile of 280 unique segments)
  filter(no_unique_ids>segments_visited_per_run$q10) %>%
  mutate(route = substr(runname, 12,14))

saveRDS(routes_to_sample, file.path(new_dt_pt, "routes_to_sample.rds"))

# # left with 239/266 (90%) routes
# nrow(routes_to_sample)/length(unique(unadj_pnc_med$runname))

# # plot showing hours sampled per run 
# unadj_pnc_med %>%
#   mutate(route = substr(runname, 12,14)) %>%
#   distinct(runname, date, dow2, hour, route) %>%
#   
#   ggplot(aes(x=hour, y=runname, col=route)) + 
#   facet_wrap(~dow2, scales="free") + 
#   geom_vline(xintercept = c("09", "17"), linetype=2, alpha=0.5) + 
#   geom_point(size=0.5) 
  

# # for 50% business coverage: 7-9 runs per route
# # for 60% business coverage: 6-9 runs per route
# # for 80% business coverage: 4-8 runs per route
# routes_to_sample %>%
#   filter(proportion_bsns_hours>=bsns_coverage_threshold) %>%
#   group_by(route) %>%
#   summarize(n=n())

###################################

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
  select(id, contains("cluster")) 

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
   
cluster1_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster1") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)
cluster2_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster2") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)
cluster3_max_rank <- filter(cluster_road_type_rank, cluster_type=="cluster3") %>% filter(road_type_rank==max(road_type_rank)) %>% pull(road_type_rank)

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

# lapply(unique(cluster_rank2$name), function(x) {
#   cluster_rank2 %>%
#     filter(#cluster_type==x
#       name==x
#            ) %>%
#     st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
#     ggplot(aes(col=value)) + 
#     geom_sf() + 
#     facet_wrap(~name+cluster_type) +
#     theme_bw() + 
#     theme(panel.grid.major = element_blank()
#           ) + 
#     labs(#col="Rank"
#          )
#   }) %>%
#   ggarrange(plotlist = .)
# 
# ggsave(file.path(image_path, "SI", "cluster_road_type_map.png"), width = 8, height = 12)

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
message("running non-spatially clustered analyses")

set.seed(1)

lapply(1:nrow(sampling_combos), function(x) {
  temp <- sampling_combos[x,]
  design_label <- paste(first(temp$adjusted), first(temp$visit_count), first(temp$balanced), first(temp$hours), sep = "_") %>% gsub(" ", "", .)
  visit_file <- file.path(new_dt_pt, "visits", "nonspatial", paste0(design_label, ".rds"))
  annual_file <- file.path(new_dt_pt, "site_avgs", "nonspatial", paste0(design_label, ".rds"))
  
  if(!file.exists(visit_file) | !file.exists(annual_file)) {
    message(paste("running sampling design:", design_label))
    #message(paste0(capture.output(temp), collapse = "\n"))
    
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
      ungroup() %>% suppressMessages()
    
    message("saving samples")
    # save separate files since this is large
    saveRDS(my_samples, visit_file)
    saveRDS(annual_averages, annual_file)
    
  } else {
    message(paste("Files already exist for", design_label))
  }
  })  


########################################################################################################
# UNBALANCED CLUSTERED SAMPLING DESIGNS
########################################################################################################
sampling_combos_random_clusters <-expand.grid(
  adjusted = adjusted_vars,
  visit_count = c(visit_count2, visit_count1),
  balanced = c("unbalanced"),  
  hours = c("all hours", "business hours"),
  cluster_approach = c("random", "sensible", "unsensible", "road_type"),
  cluster_type = unique(segment_clusters_l$cluster_type)
  )  

saveRDS(sampling_combos_random_clusters, file.path(new_dt_pt, "clustered_sampling_combo_list.rda"))

########################################################################################################

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

message("running spatially clustered analyses")

set.seed(21)
# x=4
lapply(1:nrow(sampling_combos_random_clusters), function(x) {
                                     temp <- sampling_combos_random_clusters[x,]

                                     design_label <- paste(first(temp$adjusted), first(temp$visit_count), first(temp$balanced), first(temp$hours), first(temp$cluster_approach), first(temp$cluster_type), sep = "_") %>%
                                       gsub(" ", "", .)

                                     visit_file <- file.path(new_dt_pt, "visits", "clustered", paste0(design_label, ".rds"))
                                     annual_file <- file.path(new_dt_pt, "site_avgs", "clustered", paste0(design_label, ".rds"))

                                     #message(paste0(capture.output(temp), collapse = "\n"))

                                     if(!file.exists(visit_file) |
                                        !file.exists(annual_file)) {

                                       message(paste("running sampling design:", design_label))

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
                                         many_campaigns_clustered(df = ., visit_count = temp$visit_count, cluster_approach.=temp$cluster_approach) %>%
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
                                         ungroup() %>% suppressMessages()


                                       message("saving samples")
                                       # save separate files since this is large
                                       saveRDS(my_samples, visit_file)
                                       saveRDS(annual_averages, annual_file)
                                     } else {
                                       message(paste("Files already exist for", design_label))
                                     }
                                   })

########################################################################################################
# ROUTE SAMPLING
########################################################################################################
# Like the "balanced" design but sampling is based on runname/date so that it incorporates spatial & temporal structure realistic of field campaigns
# most campaigns can't test this b/c driving routes may not be fixed or repeated many times
# sampling designs
sampling_combos_routes <- expand.grid(
  adjusted = adjusted_vars,
  #visit_count = c(visit_count2, visit_count1),
  visit_count = seq(4,20,4),
  balanced = c("balanced"),
  hours = c("all hours", "business hours")) 

saveRDS(sampling_combos_routes, file.path(new_dt_pt, "sampling_combos_routes_list.rda"))

########################################################################################################
# visit_dt = pnc_med
# temp <- sampling_combos_routes[4,]
# adjusted. = temp$adjusted
# hours = temp$hours #"all hours" #"business hours"
# visit_count= temp$visit_count
# bsns_coverage_threshold.=bsns_coverage_threshold
# set.seed(1)

one_campaign_by_route <- function(visit_dt, adjusted., hours, visit_count,
                                  bsns_coverage_threshold.=bsns_coverage_threshold #this becomes irrelevant if "all hours" design
                                  ) {
  
  # all vs business hours/days
  if(hours == "all hours"){ temp_routes <- routes_to_sample } 
  # routes with most samples during BH
  if(hours == "business hours"){ temp_routes <- filter(routes_to_sample, proportion_bsns_hours>bsns_coverage_threshold.) } 
  
  # note that route usually but don't always have the same id's
  ## so some id's are sampled more b/c they are on multiple routes and sometimes sampled multiple times in a run (e.g., at the beginning & end of a run; e.g., id 6791) 
  ## while others are sampled less (e.g., 3947 is sampled 8x)
  sampling_routes <- temp_routes %>%
    group_by(route) %>%
    slice_sample(n= visit_count, replace=T)
  
  visit_dt <- visit_dt %>% 
    filter(adjusted == adjusted.,
           runname %in% sampling_routes$runname) #%>% 
  
  # some sampling routes may be sampled twice (& segments are sometimes sampled multiple times in one route)
  visit_dt <- left_join(sampling_routes, visit_dt, by="runname"#, relationship = "many-to-many"
                        ) %>% 
    ungroup() %>%
    mutate(actual_visits = visit_count,
           segment_visits_per_campaign = n())
  
  # QC
  # visit_dt %>% distinct(runname) %>% group_by(route) %>% summarize(n())
  # QC - also checked that # routes & id's matched. looks good
  return(visit_dt)
}

many_campaigns_by_route <- function(sims=sim_n, df, ...) {
  mclapply(1:sims, mc.cores = core_count, function(s) {
    one_campaign_by_route(df, ...) %>%
      mutate(campaign = s)}) %>%
    bind_rows()
}
########################################################################################################
message("running route sampling analyses")

set.seed(1)
# x=4
lapply(1:nrow(sampling_combos_routes), function(x) {
  temp <- sampling_combos_routes[x,]
  design_label <- paste(first(temp$adjusted), first(temp$visit_count), first(temp$balanced), first(temp$hours), sep = "_") %>% gsub(" ", "", .)
  visit_file <- file.path(new_dt_pt, "visits", "routes", paste0(design_label, ".rds"))
  annual_file <- file.path(new_dt_pt, "site_avgs", "routes", paste0(design_label, ".rds"))

  message(paste0(capture.output(temp), collapse = "\n"))
  
  if(!file.exists(visit_file) | !file.exists(annual_file)) {
    message(paste("running sampling design:", design_label))
    
    my_samples <- many_campaigns_by_route(df = pnc_med,
                                          adjusted. = temp$adjusted,
                                          hours = temp$hours,
                                          visit_count=temp$visit_count)  %>%
      mutate(
        adjusted = temp$adjusted,
        design = "route",  
        visits = paste0(temp$visit_count, " visits"), #approximate visit count for unbalanced designs
        version = paste("by route", temp$hours))
    
    annual_averages <- my_samples %>%
      group_by(id, adjusted, actual_visits, segment_visits_per_campaign, campaign, design, visits, version) %>%
      summarize(annual_mean = mean(median_value, na.rm=T)) %>%
      ungroup() %>% suppressMessages()
    
    message("saving samples")
    saveRDS(my_samples, visit_file)
    saveRDS(annual_averages, annual_file)
    
  } else{
    message(paste("Files already exist for", design_label))
  }
})  

########################################################################################################
# DONE
########################################################################################################
message("DONE running r0_sampling.R")
