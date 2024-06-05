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
dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
if(!dir.exists(file.path(dt_path, "onroad"))){dir.create(file.path(dt_path, "onroad"), recursive = T)}

dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")

# load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))

set.seed(1)

# # main underwrite temporal adjustment approach
# main_bg <- "hr3_pct1"
##################################################################################################
# speed thigns up
testing_mode <- TRUE #reduce visit files
save_new_cw <- TRUE #true when e.g., add new versions (e.g., include more visit files)

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
# x=design_types[1]
onroad0 <- lapply(design_types, function(x){
  file_names <- list.files(file.path("data", "onroad", "annie", "v2", "site_avgs", x))  
  # 1 file per design group/type
  if(testing_mode==TRUE){file_names <- file_names[1]}
  
  lapply(file_names, function(f){readRDS(file.path("data", "onroad", "annie", "v2", "site_avgs", x, f))}) %>% bind_rows()
  }) %>%
  bind_rows()  

# onroad0 %>% filter(id==first(id)) %>% View()

# temporal adjustments
## using a fixed site (PTRAK UFP~NO2 model based on collocations) [this is different than the stationary temp adj!]
temporal_adjustments1 <- readRDS(file.path(dt_pt2, "site_avgs", "temp_adj1.rds"))
## using the underwrite ptrak approach (only load main analysis file)
temporal_adjustments <- readRDS(file.path(dt_pt2, "site_avgs", "temp_adj2_no_hwy_hr3_pct1.rds")) %>%
  select(names(temporal_adjustments1)) %>%
  bind_rows(temporal_adjustments1)

rm(temporal_adjustments1)

onroad0 <- onroad0 %>% 
  bind_rows(temporal_adjustments) %>%
  rename(location=id,
         value = annual_mean) %>%
  # log transform pollutant concentrations before modeling
  mutate(value = ifelse(value <= 0, 1, value),
         value = log(value),
         variable = "pnc_noscreen")

#onroad0 <- mutate(onroad0, version = gsub("by route ", "", version))

# onroad0 %>% filter(location==first(location)) %>% View()
##################################################################################################
#  MODEL CROSSWALK
##################################################################################################
message("creating model crosswalks")

#--> CHECK THAT CROSSWALK INCLUDES ROUTES & MAKES OK SENSE

# distinct(onroad0, design, version, visits, campaign, adjusted, cluster_type) %>% View()
# onroad0 %>%distinct(version)  

cw <- onroad0 %>%
  distinct(design, version, visits, campaign, adjusted, cluster_type) %>%
  arrange(design, version, visits, campaign, adjusted, cluster_type) %>%
  mutate(
    cluster_code = gsub("luster", "", cluster_type),

    design_code = case_when(
      design=="balanced" ~ "bal",
      design=="unbalanced" ~ "random",
      design=="random" ~ "random",
      design=="sensible" ~ "sen",
      design=="unsensible" ~ "unsen",
      design=="road_type" ~ "road",
      design=="route" ~ "route"),

    # version_code = case_when(
    #   grepl("all", version) ~ "al",
    #   grepl("business", version) ~ "bh"),
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

# View(cw %>% filter(campaign==1))

if(save_new_cw==TRUE) {write.csv(cw, file.path(dt_path, "onroad_model_cw_20240604.csv"), row.names = F)}

onroad0 <- left_join(onroad0, cw) %>%
  select(location, value, model, variable, design_code) 

onroad1 <- onroad0 %>%
  left_join(cov, by="location")

onroad <- onroad1 %>%
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs)

message("saving onroad modeling data")
saveRDS(onroad, file.path(dt_path, #"Selected Campaigns", #"onroad_modeling_data_20240313.rda" "onroad_modeling_data_20240604.rda"
                          "onroad", "modeling_data", "all.rda"))

##################################################################################################
# SEPARATE DATA FOR MODELING LATER
##################################################################################################


lapply(group_split(onroad0, design_code), function(x) {
  design <- first(x$design_code)
  message(design)
  
  temp <- left_join(x, cov, by="location") %>%
    # prep for modeling
    st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
    st_transform(m_crs)
    
  #saveRDS(temp, file.path(dt_path, "Selected Campaigns", paste0("onroad_modeling_data_", design, ".rda") ))
  saveRDS(temp, file.path(dt_path, "onroad", "modeling_data", paste0(design, ".rda")))
})

  






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
# # message("loading onroad_modeling_data_20240313.rda")
# # onroad <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda"))
# 
# message("saving smaller onroad files")
# 
# lapply(group_split(onroad, design_code), function(x) {
#   design <- unique(x$design_code)
#   
#   message(design)
#   
#   saveRDS(x, file.path(dt_path, "Selected Campaigns", paste0("onroad_modeling_data_", design, ".rda") ))
# })

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R1_ROAD_DATA.R")
