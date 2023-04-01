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

# #load the prediction workspace
load(file.path(dt_path, "uk_workspace.rdata"))
# if(!dir.exists(file.path(dt_path, "UK Predictions"))){dir.create(file.path(dt_path, "UK Predictions"))}

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
message("loading data")

## 5884 locations used. does not include all necessary covariates used (e.g. pop10, bus)
road_locations_used <- readRDS(file.path("data", "onroad", "annie", "cov_onroad_preprocessed.rds")) %>%
  pull(native_id)

cov <- read.csv(file.path("data", "onroad", "dr0364d_20230331.txt")) %>%
  filter(native_id %in% road_locations_used) %>%
  mutate(native_id = as.character(native_id),
         location = substr(native_id, nchar(native_id)-3, nchar(native_id)),
         location = as.numeric(location)) %>%
  generate_new_vars() %>%
  select(location, latitude, longitude, all_of(cov_names))

## 5874 locations
onroad_ns <- readRDS(file.path("data", "onroad", "annie", "PNC_nonspatial_annavgs.rds")) %>%
  mutate(spatial_code = "sn")
onroad_s <- readRDS(file.path("data", "onroad", "annie", "PNC_spatial_annavgs.rds")) %>%
  mutate(spatial_code = "sy")
onroad0 <- rbind(onroad_ns, onroad_s) %>%
  rename(location=id,
         value = annual_mean) %>%
  # log transform pollutant concentrations before modeling
  mutate(value = ifelse(value== 0, 1, value),
         value = log(value),
         variable = "pnc_noscreen"
  ) 

### TEST 
#distinct(onroad_ns, design, version, visits, adjusted)
# distinct(onroad_s, design, visits, version, adjusted )

#rm(onroad_ns, onroad_s)

# # stationary data; for out-of-sample validation
# stationary <- filter(annual,
#                      design=="full",
#                      variable=="pnc_noscreen")

##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################
cw <- onroad0 %>% 
  distinct(spatial_code, design, version, visits, campaign, adjusted) %>%  
  arrange(spatial_code, design, version, visits, campaign, adjusted) %>%  
  mutate(
    design_code = case_when( 
      design=="balanced" ~ "baly",
      design=="unbalanced" ~ "baln",
      design=="clustered" ~ "clus",
      design=="sensible spatial" ~ "seny",
      design=="unsensible spatial" ~ "senn",
      design=="road type" ~ "road"
    ),
    
    version_code = case_when(
      grepl("all", version) ~ "al",
      grepl("business", version) ~ "bh"),
    
    visit_code = readr::parse_number(visits),
    visit_code = str_pad(visit_code, 2, pad = "0"), 
    visit_code = paste0("v", visit_code),
    
    adjusted_code = ifelse(adjusted=="adjusted", "adjy", "adjn"),
    
    model = paste("r", 
                  spatial_code,
                  design_code,
                  version_code,
                  visit_code,
                  adjusted_code,
                  str_pad(campaign, 2, pad = "0"), 
                  sep = "_"),
    model_no = row_number())

# View(cw %>% filter(campaign==1))

save_new_cw <- TRUE
if(save_new_cw==TRUE) {write.csv(cw, file.path(dt_path, "onroad_model_cw.csv"), row.names = F)}

onroad1 <- left_join(onroad0, cw) %>%
  select(location, value, model, variable) %>% 
  left_join(cov) 

onroad <- onroad1 %>% 
  # prep for modeling
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) 

saveRDS(onroad, file.path(dt_path, "Selected Campaigns", "onroad_modeling_data.rda"))

