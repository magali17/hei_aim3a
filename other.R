
##################################################################################################
# SETUP
##################################################################################################
tictoc::tic()

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse,
               parallel, #mclapply; detectCores()
               future.apply, #future_replicate()
               lubridate, # %within%
               sf
)    

source("functions.R")

##################################################################################################
# CLEAN COHORT FILE
##################################################################################################

monitoring_area <- readRDS(file.path("data", "GIS", "monitoring_land_zero_water_shp.rda"))  
lat_long_crs <- 4326

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
#cov_names <- readRDS(file.path(dt_path, "cov_names.rda"))

# 30653 rows in original file
cohort0 <- read.csv(file.path("data", "dr0357_cohort_covar_20220404.csv")) %>%
  # drop 3 duplicate rows
  distinct()# %>%
  # drop 1795 rows w/ missing modeling covariates - get dropped below anyways
  #drop_na(all_of(starts_with(cov_names)))

# 28858 remaining rows

# only keep locations in monitoring region
cohort0$in_monitoring_area <- suppressMessages(
  cohort0 %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs) %>%
    st_intersects(., monitoring_area, sparse = F) %>%
    apply(., 1, any)
)

# 25810 rows remain - locations in monitoring region w/ full modeling covariates
cohort0 <- filter(cohort0, in_monitoring_area)
saveRDS(cohort0, file.path("data", "dr0357_cohort_covar_20220404_in_mm_area.rda"))

#apply(is.na(cohort0), 2, sum) %>% as.data.frame() %>% filter(.>0)

#length(unique(cohort$location_id))


##################################################################################################
# COMPARE NS TOTAL CONC
##################################################################################################


dustin <- read_csv(file.path("data", "tr0090_averaged_stops.csv")) %>%
  filter(grepl("PMSCAN_", instrument_id)) %>%
  select(-c(instrument_id, primary_instrument, mean_value)) %>%
  mutate(variable = if_else(variable=="total.conc", "ns_total_conc", as.character(variable)),
         
         time = ymd_hms(time, tz = "PST8PDT"),
         ) %>%
  
  filter(variable== "ns_total_conc") %>%
  
  rename(value = median_value) %>%
  #winsorize median values
  group_by(variable, location) %>%
  winsorize_fn(value = "value") %>%
  ungroup() %>%
  select(-value) %>%
  #use winsorized values for all subsequent data description
  rename(value_dustin=win_value) %>%
  # make bin count times the same as total_conc
  group_by(location, stop_id) %>%
  #mutate(time = time[variable=="ns_total_conc"]) %>%
  ungroup() %>%
  
  select(time, value_dustin)

magali <- readRDS(file.path(#act_campaign_path, 
  "data",
  "stop_data_win_medians.rda")) %>%
  #drop duplicate UFP instruments
  filter(variable == "ns_total_conc") %>%
  rename(value_magali=value)  %>%
  mutate(
    #time = ymd_hms(time, tz = "PST8PDT"),
  ) %>%
  
  select(#time, 
         value_magali)

# --> has time zone merging issues
#comp <- full_join(magali, dustin)
comp <- cbind(magali, dustin) %>%
  mutate(diff = value_dustin - value_magali)

# looks good - values are almost identical. 
summarize(comp, diff = mean(diff))
   


##################################################################################################
# CHECK FILES
##################################################################################################
# 8 design versions (2 fewer visits, 4 seasons, 2 reduced hours) x 13 pollutant variables x 30 campaigns each = 3,120
# + 1 all data design x 13 pollutant variables x 1 campaign = 13
# Total = 3133

selected_campaigns <- readRDS(file.path("Output", "Selected Campaigns", "selected_campaigns.rda"))
nrow(selected_campaigns) #3133 models

selected_campaigns %>%
  group_by(variable) %>%
  summarize(
    campaigns = length(unique(campaign)),
    #designs = length(unique(design)),
    versions = length(unique(version)),
    # campaigns = paste(sort(unique(campaign)), collapse = ", "),
    # designs = paste(sort(unique(design)), collapse = ", "),
    # versions = paste(sort(unique(version)), collapse = ", "),
  )  

# why is this not 9*13*30 = 3510 ???

# selected_campaigns %>%
#   distinct(variable, design, version, campaign) %>% 
#   arrange(variable, design, version, campaign) %>% View()
     

# missing <- selected_campaigns %>% 
#   filter(campaign_id %in% c(1612, 1614, 1616, 1618, 1620, 2342, 2344, 2346, 2348, 2350, 2352, 2354, 2356, 2358, 2360, 2362, 2364, 2366, 2368, 2370, 2372, 2374, 2376, 2378, 2380, 2382, 2384, 2386, 2388, 2390, 2392, 2394, 2396, 2398, 2400, 3121, 3123, 3125, 3127, 3129, 3131, 3133, 3135, 3137, 3139, 3141, 3143, 3145, 3147, 3149, 3151, 3153, 3155, 3157, 3159, 3161, 3163, 3165, 3167, 3169, 3171, 3173, 3175, 3177, 3179, 3902, 3904, 3906, 3908, 3910, 3912, 3914, 3916, 3918, 3920, 3922, 3924, 3926, 3928, 3930, 3932, 3934, 3936, 3938, 3940, 3942, 3944, 3946, 3948, 3950, 3952, 3954, 3956, 3958, 3960, 4682, 4684, 4686, 4688, 4690, 4692, 4694, 4696, 4698, 4700, 4702, 4704, 4706, 4708, 4710, 4712, 4714, 4716, 4718, 4720, 4722, 4724, 4726, 4728, 4730, 4732, 4734, 4736, 4738, 4740, 5462, 5464, 5466, 5468, 5470, 5472, 5474, 5476, 5478, 5480, 5482, 5484, 5486, 5488, 5490, 5492, 5494, 5496, 5498, 5500, 5502, 5504, 5506, 5508, 5510, 5512, 5514, 5516, 5518, 5520, 6242))

# 156 missing campaigns, all for NO2
#missing %>% distinct(variable, design, version, campaign)  %>% arrange(variable, design, version, campaign) %>% View()


# 4045088 total missing fro csv that are in the rda

# 4045088/156 = 25,930.05 # not 25810? some locations most also be missing from another campagin in the dataset


 