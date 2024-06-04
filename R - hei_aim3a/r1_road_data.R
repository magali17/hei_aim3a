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
dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")

# load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))

set.seed(1)

# # main underwrite temporal adjustment approach
# main_bg <- "hr3_pct1"
##################################################################################################
# speed thigns up
testing_mode <- TRUE #reduce visit files

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

# --> TO DO: CHECK THAT INDIVUDUAL FILES CODE WORKS & ROUTES ARE ADDED  [ACTUAL_VISITS IS OFF W/ TEMPORAL ADJ FILES]

design_types <- readRDS(file.path("data", "onroad", "annie", "v2", "design_types_list.rds"))
# x=design_types[1]
onroad0 <- lapply(design_types, function(x){
  file_names <- list.files(file.path("data", "onroad", "annie", "v2", "site_avgs", x))  
  
  if(testing_mode==TRUE){file_names <- file_names[1]}
  
  lapply(file_names, function(f){readRDS(file.path("data", "onroad", "annie", "v2", "site_avgs", x, f))}) %>% bind_rows()
  }) %>%
  bind_rows()  


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

##################################################################################################
#  MODEL CROSSWALK
##################################################################################################
message("creating model crosswalks")

#--> CHECK THAT CROSSWALK INCLUDES ROUTES & MAKES OK SENSE

# distinct(onroad0, spatial_code, design, version, visits, campaign, adjusted, cluster_type) %>% View()

cw <- onroad0 %>%
  distinct(#spatial_code, 
           design, version, visits, campaign, adjusted, cluster_type) %>%
  arrange(#spatial_code, 
    design, version, visits, campaign, adjusted, cluster_type) %>%
  mutate(
    #cluster_code = ifelse(is.na(cluster_type), "cNA", cluster_type),
    cluster_code = gsub("luster", "", cluster_type),

    design_code = case_when(
      design=="balanced" ~ "bal",
      design=="unbalanced" ~ "random",
      design=="random" ~ "random",
      design=="sensible" ~ "sen",
      design=="unsensible" ~ "unsen",
      design=="road_type" ~ "road",
      design=="route" ~ "route"),

    version_code = case_when(
      grepl("all", version) ~ "al",
      grepl("business", version) ~ "bh"),
    #version_code = ifelse(grepl("route", version) ~ paste0(version_code, "_route"), version_code),

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

save_new_cw <- TRUE
if(save_new_cw==TRUE) {write.csv(cw, file.path(dt_path, "onroad_model_cw_20240507.csv"), row.names = F)}

onroad1 <- left_join(onroad0, cw) %>%
  select(location, value, model, variable, design_code) %>%
  left_join(cov)

onroad <- onroad1 %>%
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs)

message("saving onroad modeling data")
saveRDS(onroad, file.path(dt_path, "Selected Campaigns", #"onroad_modeling_data_20240313.rda"
                          "onroad_modeling_data_20240507.rda"
                          ))

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
# message("loading onroad_modeling_data_20240313.rda")
# onroad <- readRDS(file.path(dt_path, "Selected Campaigns", "onroad_modeling_data_20240313.rda"))

message("saving smaller onroad files")

lapply(group_split(onroad, design_code), function(x) {
  design <- unique(x$design_code)
  
  message(design)
  
  saveRDS(x, file.path(dt_path, "Selected Campaigns", paste0("onroad_modeling_data_", design, ".rda") ))
})

##################################################################################################
# DONE
##################################################################################################
message("DONE WITH R1_ROAD_DATA.R")
