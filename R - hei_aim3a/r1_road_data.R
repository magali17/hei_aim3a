# script creates the onroad modeling dataset needed later & a model crosswalk

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
# latest_dt_version <- "v4"
# saveRDS(latest_dt_version, file.pat)
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

# #load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))
# if(!dir.exists(file.path(dt_path, "UK Predictions"))){dir.create(file.path(dt_path, "UK Predictions"))}

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
message("loading datasets")

#road_locations_used <- readRDS(file.path("data", "onroad", "annie", "cov_onroad_preprocessed.rds")) %>% pull(native_id)
ids_included <- readRDS(file.path("data", "onroad", "annie", "v2", "ids_included.rds")) 
road_locations_used <- readRDS(file.path("data", "onroad", "annie", "OnRoad Paper Code Data", "data", "All_Onroad_12.20.rds")) %>%
  distinct(id, native_id) %>%
  filter(id %in% ids_included)

cov <- read.csv(file.path("data", "onroad", "dr0364d_20230331.txt")) %>%
  filter(native_id %in% road_locations_used$native_id) %>%
  mutate(native_id = as.character(native_id),
         location = substr(native_id, nchar(native_id)-3, nchar(native_id)),
         location = as.numeric(location)) %>%  
  generate_new_vars() %>%
  select(location, latitude, longitude, all_of(cov_names))
saveRDS(cov, file.path("data", "onroad", "dr0364d_20230331_modified.rda"))

## 5874 locations
onroad_ns <- readRDS(file.path("data", "onroad", "annie", "v2", "nonspatial_site_avgs.rds" #"PNC_nonspatial_annavgs.rds"
                               )) %>%
  mutate(spatial_code = "sn")
onroad_s <- readRDS(file.path("data", "onroad", "annie", "v2", "cluster_site_avgs.rds" #"PNC_spatial_annavgs.rds"
                              )) %>%
  mutate(spatial_code = "sy")

temporal_adjustments <- readRDS(file.path("data", "onroad", "annie", "v2", "temp_adj_site_avgs.rds")) %>%
  mutate(spatial_code = ifelse(design %in% c("sensible", "unsensible", "road_type"), "sy", "sn"))

onroad0 <- bind_rows(onroad_ns, onroad_s) %>%
  rbind(temporal_adjustments) %>%
  rename(location=id,
         value = annual_mean) %>%
  # log transform pollutant concentrations before modeling
  mutate(value = ifelse(value <= 0, 1, value),
         value = log(value),
         variable = "pnc_noscreen") 

##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################
message("creating model crosswalks")

cw <- onroad0 %>% 
  distinct(spatial_code, design, version, visits, campaign, adjusted, cluster_type) %>%  
  arrange(spatial_code, design, version, visits, campaign, adjusted, cluster_type) %>% 
  mutate(
    #cluster_code = ifelse(is.na(cluster_type), "cNA", cluster_type),
    cluster_code = gsub("luster", "", cluster_type),
    
    design_code = case_when( 
      design=="balanced" ~ "bal",
      design=="unbalanced" ~ "random",
      design=="random" ~ "random",
      design=="sensible" ~ "sen",
      design=="unsensible" ~ "unsen",
      design=="road_type" ~ "road"
    ),
    
    version_code = case_when(
      grepl("all", version) ~ "al",
      grepl("business", version) ~ "bh"),
    
    visit_code = readr::parse_number(visits),
    visit_code = str_pad(visit_code, 2, pad = "0"), 
    visit_code = paste0("v", visit_code),
    
    adjusted_code = ifelse(adjusted=="adjusted", "adj", "unadj"),
    
    model = paste("r", 
                  #spatial_code,
                  design_code,
                  cluster_code,
                  version_code,
                  visit_code,
                  adjusted_code,
                  str_pad(campaign, 2, pad = "0"), 
                  sep = "_"),
    model_no = row_number())

# View(cw %>% filter(campaign==1))

save_new_cw <- TRUE
if(save_new_cw==TRUE) {write.csv(cw, file.path(dt_path, "onroad_model_cw_20240313.csv"), row.names = F)}

onroad1 <- left_join(onroad0, cw) %>%
  select(location, value, model, variable, design_code) %>% 
  left_join(cov) 

onroad <- onroad1 %>% 
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) 

message("saving onroad modeling data")
saveRDS(onroad, file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda"))

##################################################################################################
# SEPARATE DATA FOR MODELING LATER
##################################################################################################
# test <- onroad %>%
#   mutate(spatial = ifelse(grepl("r_sy_", model), TRUE, FALSE),
#          adjusted = ifelse(grepl("adjy_", model), TRUE, FALSE)) 
# 
# # sp = TRUE
# # adj = TRUE
# 
# for(sp in c(TRUE, FALSE)) {
#   for(adj in c(TRUE, FALSE)) {
#     
#     test %>%
#       filter(spatial==sp,
#              adjusted==adj) %>%
#       group_by(model) %>%
#       mutate(model_no2 = cur_group_id()) %>%
#       
#       ungroup() %>%
#       arrange(desc(model_no2)) %>%
#       
#       saveRDS(file.path(dt_path, "Selected Campaigns", paste0("onroad_modeling_data_SP_",sp, "_ADJ_", adj, ".rda") ))
#     }
#   }
##################################################################################################
message("saving smaller onroad files")

# onroad <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda"))

# --> ERROR IN FILE.... EMPTY STRING??
lapply(group_split(onroad, design_code), function(x) {
  design <- unique(onroad$design_code)
  saveRDS(x, file.path(dt_path, "Selected Campaigns", paste0("onroad_modeling_data_", design, ".rda") ))
})

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R1_ROAD_DATA.R")
