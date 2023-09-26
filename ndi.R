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
               ggmap, sf, ggspatial #mapping...adding scales, N arrows
)    

source("functions.R")

anjum_fp <- file.path("~", "OneDrive - UW", "Documents", "DEOHS", "Projects", "Other people", "Anjum - NSES")

##################################################################################################
# LOAD DATA
##################################################################################################
# outcome & covariate data
health_dt_path <-file.path("data", "issue_12", "issue_012_degapped_0113.rda")

if(file.exists(health_dt_path)) {health0 <- readRDS(health_dt_path)} else {
  health0 <- haven::read_sas(gsub(".rda", ".sas7bdat", health_dt_path) , NULL)
  saveRDS(health0, health_dt_path)
}

# baseline data 
health <- filter(health0, VISIT==0)


# NDI locations
ndi <- read_csv(file.path(anjum_fp, "NSES_MESA_INDEX_1990_2017_TVlong.csv")) %>%   
  filter(YEAR==2010) %>%
  rename(GEO_ID_TRT = TRACTID10) %>%
  select(-"...1")

tracts <- st_read(file.path(anjum_fp, "2010_census_tract", "2010_Census_Tracts_for_King_County_-_Conflated_to_Parcels___tracts10_area",  "2010_Census_Tracts_for_King_County_-_Conflated_to_Parcels___tracts10_area.shp")) %>% 
  right_join(ndi)

##################################################################################################
# plots 
##################################################################################################
test <- health %>%
  select(nses_z_cx, income, #livingsb, 
         degree, race,hispanic) %>% #, degree, race, male, hispanic,education,)
  mutate(
    income =  recode_factor(factor(income),
                            "A" = "1.5",
                            "B" = "2.5",
                            "C" = "3.5",
                            "D" = "4.5",
                            "E" = "5.5",
                            "F" = "6.5",
                            "9" = "NA"
    ),
    income = as.numeric(as.character(income)),
    
    #livingsb = ifelse(livingsb==9, NA, livingsb)
    # livingsb =  recode_factor(factor(livingsb),
    #                         "1" = "spouse",
    #                         "2" = "spouse+relatives",
    #                         "3" = "relatives/friends",
    #                         "4" = "unrelated persons",
    #                         "5" = "nursing home",
    #                         "6" = "alone",
    #                         "9" = "NA"
    # ),
    race_white = ifelse(race == 1, 1, 0),
    # make unknown degree=9 "none" 
    degree = ifelse(degree == 9, 0, degree),
    # combine GED and HS 
    degree = ifelse(degree %in% c(1:2), 1, degree),
    degree = factor(degree),
  ) %>%
  select(-race) %>%
  mutate_at(vars(c("income", "degree", "hispanic", "race_white")), ~as.factor(.))

# higher NDI is more disadvantage. so makes sense here...
test %>%
  pivot_longer(-nses_z_cx) %>%  
  ggplot( aes(y=nses_z_cx, x=value)) + 
  facet_wrap(~name, scales = "free") + 
  geom_boxplot()

##################################################################################################
# maps
##################################################################################################
print("higher NDI values are associated with higher disadvantage (i.e., worse)")
tracts %>%
  ggplot(aes(fill=NSES_z_cx)) + 
  geom_sf() + 
  scale_fill_gradient2(
    high = "red",
    mid = "white",
    low = "blue"
  )








  