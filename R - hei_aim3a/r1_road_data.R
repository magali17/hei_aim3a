# script creates a model crosswalk & modeling_data files (adds covariates etc.)   

# RESULTS are in: Output/v3_20230321/onroad/ and .../modeling_data/ subdirectory

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

pacman::p_load(tidyverse, sf # UK-PLS MODEL
)    

source("functions.R")
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
dt_path_onroad <- file.path(dt_path, "onroad")
if(!dir.exists(file.path(dt_path_onroad, "modeling_data"))){dir.create(file.path(dt_path_onroad, "modeling_data"), recursive = T)}

temp_adj_path <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")

# load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))

set.seed(1)

##################################################################################################
# speed thigns up
testing_mode <- FALSE #reduce visit files
save_new_cw <- FALSE #true when e.g., add new versions (e.g., include more visit files)
run_qc <- FALSE # design counts etc.
overwrite_modeling_data <- FALSE # TRUE when e.g. have new designs you want added

##################################################################################################
# DATA
##################################################################################################
message("loading datasets")

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
design_types <- readRDS(file.path("data", "onroad", "annie", "v2", "design_types_list.rds"))
# x=design_types[3]
onroad0 <- lapply(design_types, function(x){
  file_names <- list.files(file.path("data", "onroad", "annie", "v2", "site_avgs", x))  
  # 1 file per design group/type
  if(testing_mode==TRUE){file_names <- file_names[3:6]}
  
  lapply(file_names, function(f){readRDS(file.path("data", "onroad", "annie", "v2", "site_avgs", x, f))}) %>% bind_rows()
  }) %>%
  bind_rows()  

# unique(onroad0$version)
# onroad0 %>% filter(id==first(id), campaign==first(campaign)) %>% View()

# temporal adjustments
## using a fixed site (PTRAK UFP~NO2 model based on collocations) [this is different than the stationary temp adj!]
temporal_adjustments1 <- readRDS(file.path(temp_adj_path, "site_avgs", "temp_adj1.rds"))
## using the underwrite ptrak approach (only load main analysis file) # "hr3_pct1"
temporal_adjustments <- readRDS(file.path(temp_adj_path, "site_avgs", "temp_adj2_no_hwy_hr3_pct1.rds")) %>%
  select(names(temporal_adjustments1)) %>%
  bind_rows(temporal_adjustments1) %>%
  
  #--> TEMP, shouldnt matter later
  mutate(version = gsub("by route ", "", version))

rm(temporal_adjustments1)

onroad0 <- onroad0 %>% 
  bind_rows(temporal_adjustments) %>%
  rename(location=id,
         value = annual_mean) %>%
  # log transform pollutant concentrations before modeling
  mutate(value = ifelse(value <= 0, 1, value),
         value = log(value),
         variable = "pnc_noscreen")

##################################################################################################
#  QC - check that model #s etc. make sense
##################################################################################################
message("summary/QC checks")

# check that numbers make sense after r0_temporal_adjustment.R finishes. # looks good?
if(run_qc ==TRUE) {
  
  design_counts1 <- onroad0 %>% 
    group_by(design) %>% 
    summarize(no_clusters = length(unique(cluster_type)),
              clusters = paste(unique(cluster_type), collapse = ", "),
              
              no_versions = length(unique(version)),
              version = paste(unique(version), collapse = ", "),
              
              no_visits = length(unique(visits)),
              visits = paste(unique(visits), collapse = ", ") %>% gsub(" visits", "", .),
              
              no_adjusted = length(unique(adjusted)),
              adjusted = paste(unique(adjusted), collapse = ", "))
  
  write.csv(design_counts1, file.path(dt_path_onroad, "design_counts1.csv"), row.names = F)
  
  # looks good. segment & campaign #s are stable
  design_counts2 <- onroad0 %>%
    group_by(design, cluster_type, version, visits, adjusted) %>%
    summarize(no_segments = length(unique(location)),
              no_campaigns = length(unique(campaign)),
              no_rows=n())
  
  write.csv(design_counts2, file.path(dt_path_onroad, "design_counts2.csv"), row.names = F)
  
}


##################################################################################################
#  MODEL CROSSWALK
##################################################################################################
message("creating crosswalks")

if(save_new_cw==TRUE) {
  
  cw <- onroad0 %>%
    distinct(design, cluster_type, version, visits, adjusted, campaign) %>%
    arrange(design, cluster_type, version, visits, adjusted, campaign) %>%
    #arrange(design, version, visits, campaign, adjusted, cluster_type) %>%
    mutate(
      design_code = case_when(
        design=="balanced" ~ "bal",
        design=="unbalanced" ~ "random",
        design=="random" ~ "random",
        design=="sensible" ~ "sen",
        design=="unsensible" ~ "unsen",
        design=="road_type" ~ "road",
        design=="route" ~ "route"),
      
      cluster_code = gsub("luster", "", cluster_type),
      
      version_code = case_when(
        grepl("all", version) ~ "al",
        version == "business hours" ~ "bh",
        version == "business hours temp adj 1" ~ "bhadj1",
        version == "business hours temp adj 1 random" ~ "bhadj1r",
        version == "business hours temp adj 2" ~ "bhadj2",
        version == "business hours temp adj 2 random" ~ "bhadj2r",
        TRUE ~ version),
      version_code = gsub(" ", "", version_code),
      
      visit_code = readr::parse_number(visits),
      visit_code = str_pad(visit_code, 2, pad = "0"),
      visit_code = paste0("v", visit_code),
  
      adjusted_code = ifelse(adjusted=="adjusted", "adj", "unadj"),
  
      model = paste("r",
                    design_code,
                    cluster_code,
                    version_code,
                    visit_code,
                    adjusted_code,
                    str_pad(campaign, 2, pad = "0"),
                    sep = "_"),
      model_no = row_number())
  
  write.csv(cw, file.path(dt_path_onroad, "onroad_model_cw.csv"), row.names = F)
  
} else {
  cw <- read.csv(file.path(dt_path_onroad, "onroad_model_cw.csv"))
  }


# View(cw %>% filter(campaign==1)) #270 combinations (x30 each)


if(file.exists(file.path(dt_path_onroad, "modeling_data", "all.rda")) & 
   overwrite_modeling_data ==FALSE) {
  
  message("loading all modeling data")
  
  onroad <- readRDS(file.path(dt_path_onroad, "modeling_data", "all.rda"))
  
  } else {
    message("creating modeling data")
    
    onroad0 <- left_join(onroad0, cw) %>%
      select(location, value, model, variable, design_code) 
    
    onroad1 <- onroad0 %>%
      left_join(cov, by="location")
    
    onroad <- onroad1 %>%
      # prep for modeling
      st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
      st_transform(m_crs)
    
    message("saving onroad modeling data")
    saveRDS(onroad, file.path(dt_path_onroad, "modeling_data", "all.rda"))
  } 


##################################################################################################
# SEPARATE DATA FOR MODELING LATER
##################################################################################################
#lapply(group_split(onroad0, design_code), function(x) {
lapply(group_split(onroad, design_code), function(x) {
  design <- first(x$design_code)
  message(design)
  
  if(!file.exists(file.path(dt_path_onroad, "modeling_data", paste0(design, ".rda"))) |
     overwrite_modeling_data ==TRUE) {
    # temp <- left_join(x, cov, by="location") %>%
    #   # prep for modeling
    #   st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
    #   st_transform(m_crs)
    # 
    # saveRDS(temp, file.path(dt_path_onroad, "modeling_data", paste0(design, ".rda")))
    x %>%
      saveRDS(., file.path(dt_path_onroad, "modeling_data", paste0(design, ".rda")))
  }
  
 
})


##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R1_ROAD_DATA.R")
