

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
underwrite_adj <- readRDS(file.path(dt_pt, "underwrite_temp_adj.rda"))
underwrite_adj_no_hwy <- readRDS(file.path(dt_pt, "underwrite_temp_adj_no_hwy.rda"))

# --> ADD no_hwy files

# adjusted visits
visits_adj2 <- readRDS(file.path(dt_pt, "bh_visits_fixed_site_temporal_adj_uw.rds")) 
# adjusted annual averages
annual_adj2 <- readRDS(file.path(dt_pt, "TEMP_bh_site_avgs_uw_adj.rds")) 

##################################################################################################
# INVESTIGATE NEGATIVE ANNUAL AVERAGES
##################################################################################################
# --> QC: check that no or few annual averages are < 0
summary(annual_adj2$annual_mean)
prop.table(table(annual_adj2$annual_mean<=0))


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

