
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

##################################################################################################
# DATA
##################################################################################################
project_crs <- 4326  #lat/long
#m_crs <- 32148

cov <- readRDS(file.path("data", "onroad", "dr0364d_20230331_modified.rda")) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>% #st_transform(m_crs)  
  select(location, latitude, longitude)

##################################################################################################
# MAKE CLUSTERS
##################################################################################################
# create spatial clusters (Annie's original clusters have spatial overlap, possibly b/c of sampling times - make up routes, forward/backwards routes)
set.seed(99)
k_clusters <- cov %>%
  # st_transform(m_crs) %>%
  # cbind(st_coordinates(.)) %>%
  select(latitude, longitude, #X, Y
  ) %>%
  st_drop_geometry() %>%
  kmeans(., 60)

print("cluster size")
summary(k_clusters$size)

cov_new_clusters <- cov %>%
  mutate(cluster = k_clusters$cluster) %>%  
  group_by(cluster) %>%
  mutate(no_segments = n()) 

# save new clusters
cov_new_clusters %>%
  st_drop_geometry() %>%
  saveRDS(file.path("data", "onroad", "annie", "segment_clusters_updated.rds"))

##################################################################################################
# MAP
##################################################################################################

cov_new_clusters %>%
  mutate(cluster = as.factor(cluster)) %>%
  ggplot(aes(col=cluster)) +
  geom_sf(size=0.5, show.legend=F) +
  #facet_wrap(~cluster) +
  theme_bw() + 
  theme(panel.grid.major = element_blank())  
