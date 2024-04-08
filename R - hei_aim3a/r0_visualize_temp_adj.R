

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

pacman::p_load(tidyverse, lubridate#, zoo,
               )    

source("functions.R")
dt_pt <- file.path("data", "onroad", "annie", "v2")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
dt_out <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "qc", "road")
if(!dir.exists(dt_out)){dir.create(dt_out, recursive = T)}

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
# 1sec data with rolling quantiles
road_dt <- readRDS(file.path(dt_pt, "underwrite_temp_adj_all_1s_data.rda"))
road_dt_no_hwy <- readRDS(file.path(dt_pt, "underwrite_temp_adj_all_1s_data_no_hwy.rda"))

# hourly adjustments
underwrite_adj <- readRDS(file.path(dt_pt, "underwrite_temp_adj.rda")) %>%

  # --> TEMP
  filter(background_adj == "hr1_pct1")

underwrite_adj_no_hwy <- readRDS(file.path(dt_pt, "underwrite_temp_adj_no_hwy.rda"))

# adjusted visits
visits_adj2 <- readRDS(file.path(dt_pt, "bh_visits_fixed_site_temporal_adj_uw.rds"))
visits_adj2_no_hwy <- readRDS(file.path(dt_pt, "bh_visits_fixed_site_temporal_adj_uw_no_hwy.rds")) 
# adjusted annual averages
annual_adj2 <- readRDS(file.path(dt_pt, "TEMP_bh_site_avgs_uw_adj.rds")) %>%
  
  # --> TEMP
  filter(background_adj == "hr1_pct1")

annual_adj2_no_hwy <- readRDS(file.path(dt_pt, "TEMP_bh_site_avgs_uw_adj_no_hwy.rds")) 

##################################################################################################
# FUNCTIONS
##################################################################################################
summarize_values <- function(dt, val) {
  temp <- dt %>%
    rename(val=all_of(val)) %>%
    summarize(
      n = n(),
      campaigns = length(unique(campaign)),
      segments = length(unique(id)),
      min=min(val, na.rm = T),
      Q25=quantile(val, 0.25, na.rm = T),
      Q50=quantile(val, 0.50, na.rm = T),
      Q75=quantile(val, 0.75, na.rm = T),
      max=max(val, na.rm = T),
      less0 = sum(val<=0, na.rm = T),
      prop_less0 = less0/n,
      missing = sum(is.na(val)),
      prop_missing = missing/n
      )
  
  names(temp)[names(temp)=="val"] <- val
  
  return(temp)
}

##################################################################################################
# INVESTIGATE ANNUAL AVERAGES: NEGATIVE, NA, NaN
##################################################################################################
# --> missing adjustments?
# underwrite_adj 

# negative & missing annual averages
test <- annual_adj2 %>%
  filter(is.na(annual_mean) | annual_mean <=0) 





# annual averages are < 0
annual_adj2 %>%
  group_by(design, background_adj, visits, cluster_type) %>%
  summarize_values(., val= "annual_mean") %>% View()

annual_adj2 %>%
  filter(adjusted == "adjusted", 
         cluster_type %in% c("cluster2", NA),
         !grepl("pct30", background_adj)
         ) %>%
  
  ggplot(aes(x=design, col=background_adj, y=annual_mean)) + 
  facet_wrap(~visits+cluster_type) + 
  geom_hline(yintercept = 0, linetype=2) +
  geom_boxplot() + 
  labs()
  




# --> COMPARE CONC VISITS VS ADJUSTMENTS






##################################################################################################
# VISUALIZE TEMPORAL ADJUSTMENT PATTERNS 
##################################################################################################
# --> TIME SERIES: ONROAD MEASURES VS 'BACKGROUND' (E.G., Brantley Fig 5)




##################################################################################################
# --> plot percentiles: dots w/ smoother
# --> add example of ~1 wk period
# --> LS: consider overlaying same route driven on same time…driving same structure




##################################################################################################
# hourly adjustments
# adj_lvls <- apply(expand.grid(paste0("hr", windows/3600), 
#                               paste0("_pct", quantiles*100)), 
#                   1, paste, collapse="")


underwrite_adj %>%
  drop_na(hour) %>%
  mutate(background_adj = factor(background_adj, levels=adj_lvls)) %>%
  # couple of day examples
  filter(runname %in% unique(underwrite_adj$runname)[31:36]) %>%  
  
  ggplot(aes(x=hour, y=avg_hourly_adj, group=background_adj, col=background_adj)) + 
  facet_wrap(facets = vars(runname)) +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
  geom_point() + 
  geom_line() + 
  scale_color_ordinal()

ggsave(file.path(dt_out, "temp_adj2_example.png"), width = 10, height = 6)

# time series
underwrite_adj %>%
  drop_na(hour) %>%
  mutate(background_adj = factor(background_adj, levels=adj_lvls)) %>%
  
  ggplot(aes(x=time, y=avg_hourly_adj)) + 
  facet_wrap(facets = vars(background_adj)) +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
  geom_point(alpha=0.4, aes(col=background_adj)) + 
  geom_smooth()

ggsave(file.path(dt_out, "temp_adj2_time_series.png"), width = 14, height = 8)




##################################################################################################
# QC - VISUALIZE SPATIAL ADJUSTMENT PATTERNS 
##################################################################################################
# --> maps to make sure background doesn't vary by space (e.g., near vs away from hwys). use 1sec rolling quantiles?










##################################################################################################
# DONE
##################################################################################################

