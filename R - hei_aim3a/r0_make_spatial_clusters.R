
##################################################################################################
# SETUP
##################################################################################################
knitr::opts_chunk$set(echo = F, 
                      cache=F, cache.comments = F, 
                      message = F, warning = F, 
                      tidy.opts=list(width.cutoff=60), tidy=TRUE)  

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))}

pacman::p_load(tidyverse, kableExtra, sf) 

set.seed(1)

image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
if(!dir.exists(file.path(image_path, "SI"))){dir.create(file.path(image_path, "SI"), recursive = T)}

##################################################################################################
# DATA
##################################################################################################
project_crs <- 4326  #lat/long
#m_crs <- 32148

# ids in analysis. 5878
keep_ids <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
  filter(road_type != "A1", # don't use A1_flag b/c some A1 roads have A1_flag=0 for some reason
         exclude_flag==0)  

cov <- readRDS(file.path("data", "onroad", "dr0364d_20230331_modified.rda")) %>%
  filter(location %in% keep_ids) %>%
  select(location, latitude, longitude) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F)  

##################################################################################################
# MAKE CLUSTERS
##################################################################################################
# create spatial clusters (Annie's original clusters have spatial overlap, possibly b/c of sampling times - make up routes, forward/backwards routes)
set.seed(99)
k_clusters <- cov %>%
  select(latitude, longitude) %>%
  st_drop_geometry() %>%
  kmeans(., 60)

set.seed(99)
k_clusters2 <- cov %>%
  select(latitude, longitude) %>%
  st_drop_geometry() %>%
  kmeans(., 20)

print("cluster1 size")
summary(k_clusters$size)

print("cluster2 size")
summary(k_clusters2$size)

cov_new_clusters <- cov %>%
  mutate(cluster1 = k_clusters$cluster,
         cluster2 = k_clusters2$cluster) %>%  
  pivot_longer(cols=contains("cluster"), names_to = "cluster_type", values_to = "cluster_value") %>%
  group_by(cluster_type, cluster_value) %>%
  mutate(no_segments = n()) 

# save new clusters
cov_new_clusters %>%
  st_drop_geometry() %>%
  saveRDS(file.path("data", "onroad", "annie", "segment_clusters_updated.rds"))

##################################################################################################
# MAP
##################################################################################################
cov_new_clusters %>%
  mutate(cluster_value = as.factor(cluster_value)) %>%
  ggplot(aes(col=cluster_value)) +
  geom_sf(size=0.5, show.legend=F) +
  facet_wrap(~cluster_type) +
  theme_bw() + 
  theme(panel.grid.major = element_blank())  

ggsave(file.path(image_path, "SI", "cluster_map_updated.png"), width = 8, height = 8)

