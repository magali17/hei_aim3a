# script temporally adjusts onroad data

##################################################################################################
# SETUP
##################################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse, lubridate, splines #, #bs() ns()
               )    

source("functions.R")
#dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
dt_pt <- file.path("data", "onroad", "annie", "v2")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
message("loading data")

# using fixed-site temporal adjustments previously developed in 1.1_temporal_adjustment.Rmd
# using the winsorized adjusted values, as before
fixed_site_temp_adj <- readRDS(file.path("data", "epa_data_mart", "wa_county_nox_temp_adjustment.rda")) %>%
  select(time, ufp_adjustment = diff_adjustment_winsorize)

# all visit medians
pnc_meds <- readRDS(file.path(dt_pt, "pnc_med.rds"))

# BH samples
v1 <- readRDS(file.path(dt_pt, "nonspatial_visit_samples.rds"))
v2 <- readRDS(file.path(dt_pt, "cluster_visit_samples.rds")) %>%
  select(-visit_samples)

visits <- bind_rows(v1, v2) %>%
  filter(version == "business hours") %>%
  select(-c(visit_num, runname)) 

##################################################################################################
# 1. FIXED SITE TEMPORAL ADJUSTMENT
##################################################################################################
message("running fixed site temporal adjustment")

visits_adj1 <- visits %>%
  mutate(time = ymd_h(paste(date, hour))) %>%
  # add temporal adjustment
  left_join(fixed_site_temp_adj, by="time") %>%
  mutate(median_value_adjusted = median_value + ufp_adjustment,
         version = paste(version, "temp adj 1"))

saveRDS(visits_adj1, file.path(dt_pt, "bh_visits_fixed_site_temporal_adj.rds")) 

annual_adj1 <- visits_adj1 %>%
  group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup()  

# temp file
saveRDS(annual_adj1, file.path(dt_pt, "bh_site_avgs_fixed_site_temporal_adj.rds"))

##################################################################################################
# 2. BACKGROUND ADJUSTMENT USING THE COLLECTED DATA
##################################################################################################
# e.g., see Hankey 2015 LUR... method summary:
# "(1) subtracting instantaneous background concentration estimates (via the underwrite function) from all
# instrument-reported concentrations, (2) calculating mean reference-site (i.e., background) measurements among all
# sampling days, and (3) adding the mean reference-site concentrations (from step 2) to the underwrite adjusted
# concentrations from step 1."
# 
# approach gap: does not adjust for overnight or weekend hours that are never measured in BH/RH designs

message("running background/local adjustment using the collected data")
set.seed(1)

low_conc <- 0.01 #0.05
# x = group_split(visits, adjusted, campaign, design, visits, cluster_type)[[1]]
visits_adj2 <- lapply(group_split(visits, adjusted, campaign, design, visits, cluster_type), function(x){
  temp <- x %>%
    mutate(hour = as.numeric(hour)) %>%
    group_by(hour) %>%
    mutate(quantile_background = quantile(median_value, low_conc)) %>% 
    ungroup()
  
  # make the hourly adjustments smoother
  temp_hrs <- distinct(temp, hour, quantile_background)
  smooth_fit <- lm(quantile_background ~ ns(hour, knots=c(12, 15)), data = temp_hrs)
  temp_hrs <- temp_hrs %>%
    mutate(smooth_background = predict(smooth_fit, newdata=.),
           # long-term average estimate based on background concentrations from the collected campaign data
           lta_background = mean(smooth_background))
     
  # apply hourly adjustments, add the LTA background back in
  temp <- left_join(temp, temp_hrs, by=c("hour", "quantile_background")) %>%
    mutate(local_conc = median_value - smooth_background,
           median_value_adjusted = local_conc + lta_background)
  }) %>%
  bind_rows() %>%
  mutate(version = paste(version, "temp adj 2"))

saveRDS(visits_adj2, file.path(dt_pt, "bh_visits_background_temporal_adj.rds")) 

annual_adj2 <- visits_adj2 %>%
  group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
  summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
  ungroup() 

# temp file
saveRDS(annual_adj2, file.path(dt_pt, "bh_site_avgs_background_temporal_adj.rds"))

##################################################################################################
# SAVE DATA
##################################################################################################
rbind(annual_adj1, annual_adj2) %>%
  saveRDS(., file.path(dt_pt, "temp_adj_site_avgs.rds"))

##################################################################################################
# VISUALIZE ADJUSTMENTS
##################################################################################################

# # visualize adjustments
# temp_hrs %>% 
#   pivot_longer(cols = contains("background")) %>%
#   ggplot(aes(x=hour, y=value, col=name)) + 
#   geom_point() +
#   labs(y="background Conc Estimate (pt/cm3)")
#   #geom_smooth(method = lm, formula = y ~ splines::bs(x, 1))  




##################################################################################################
# DONE
##################################################################################################
message("DONE RUNNING R0_TEMPORAL_ADJUSTMENT.R")



