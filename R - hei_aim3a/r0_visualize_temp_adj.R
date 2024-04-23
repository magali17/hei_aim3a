
# --> TO DO: SEARCH FOR 'TEMP/TO DO' (E.G., remove things that helped run code faster)

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
# dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240408")
dt_pt2 <- file.path("data", "onroad", "annie", "v2", "temporal_adj", "20240421")
image_path <- file.path("..", "..", "Manuscript", "Images", "v4", "other", "road")
dt_out <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")), "qc", "road")
if(!dir.exists(dt_out)){dir.create(dt_out, recursive = T)}

set.seed(1)

##################################################################################################
# DATA
##################################################################################################
# --> TO DO: COMBINE SMALLER HWY/NO HWY DATASETS?

# 1sec data with rolling quantiles
road_dt <- readRDS(file.path(dt_pt2, "underwrite_temp_adj_all_1s_data.rda"))
road_dt_no_hwy <- readRDS(file.path(dt_pt2, "underwrite_temp_adj_all_1s_data_no_hwy.rda"))

# hourly adjustments
underwrite_adj <- bind_rows(readRDS(file.path(dt_pt2, "underwrite_temp_adj.rda")) %>% mutate(road_types = "all road types"),
                            readRDS(file.path(dt_pt2, "underwrite_temp_adj_no_hwy.rda") %>% mutate(road_types = "no highways"))) %>%
                              
                              # --> TEMP
                              filter(background_adj == "hr1_pct5")

# adjusted visits
visits_adj2 <- readRDS(file.path(dt_pt2, "bh_visits_fixed_site_temporal_adj_uw.rds"))
visits_adj2_no_hwy <- readRDS(file.path(dt_pt2, "bh_visits_fixed_site_temporal_adj_uw_no_hwy.rds")) 

# adjusted annual averages (n=30 campaigns per design-version)
# annual_adj <- readRDS(file.path(dt_pt2, "TEMP_bh_site_avgs_uw_adj.rds")) %>%
#   mutate(road_types = "all road types")

annual_adj2 <- bind_rows(readRDS(file.path(dt_pt2, "site_avgs_uw_adj_no_hwy.rds")) %>% mutate(road_types = "no highways"),
                         readRDS(file.path(dt_pt2, "site_avgs_uw_adj.rds")) %>% mutate(road_types = "all road types")) %>%

  # --> TEMP
  filter(!cluster_type %in% c("cluster2", "cluster3"),
         !design %in% c("unbalanced", "unsensible", "road_type"),
         background_adj == "hr1_pct5"
         ) 
   

# segment lat/long/road type
segment_info <- file.path(dt_pt, "segment_lat_long.rda")

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
# INVESTIGATE ANNUAL AVERAGES & NEGATIVE VALUES
##################################################################################################
 annual_avg_by_visits_plot <- function(dt) {
  #set.seed(1)
  dt %>%
    #slice_sample(n=.1*nrow(.), ) %>%
    mutate(actual_visits = factor(actual_visits),
           visits = paste0("median ", visits, " per location")) %>%
    group_by(visits, actual_visits) %>%
    mutate(no_annual_avgs=n()) %>%
    
    ggplot(aes(x=actual_visits, y=annual_mean, #col=  #design  #cluster_type #background_adj
    )) + 
    facet_wrap(~background_adj+road_types #~design+background_adj
               ) +
    geom_hline(yintercept = 0, linetype=2, col="red") +
    geom_hline(yintercept = 1e3, linetype=2) +
    geom_boxplot(aes(col=adjusted)) +
    labs(x = "Actual Visits per Location",
         y = "Estimated Site Annual Average",
         col = "Plume")
   }
##################################################################################################
# more liely to have negative, more variable site averages results when collect few visits
# and for plume adjusted approaches that slightly reduce concentrations
# more common w/ 4 visit designs b/c there are more total annual averages on the low end (more common to only have 1 visit)

print("distribution of estimated annual averages after background adjustments during BH following various clustering & non-clustered sampling approahces (collapses plume adjusted & unadjusted)")
print("dashed black line is at 1e3")

annual_adj2 %>%
  annual_avg_by_visits_plot()

ggsave(file.path(dt_out, "annual_conc_vs_visit_num.png"), width = 14, height = 8)

# # #table
# annual_adj2 %>%
#   group_by(road_types, actual_visits, adjusted, background_adj, #design, cluster_type
#            ) %>%
#   summarize_values(val="annual_mean")


# --> what are the sites with negative annual averages? sites with large negative hourly adjustments & low visit concentrations?
negative_sites <- annual_adj2 %>%
  filter(annual_mean<=0) #%>%
  #select() %>%
  #left_join(select(visits_adj2_no_hwy, ))




##################################################################################################
# VISUALIZE TEMPORAL ADJUSTMENT PATTERNS 
##################################################################################################
# time series
underwrite_adj %>%
  #drop_na(hour) %>%
  #mutate(background_adj = factor(background_adj, levels=adj_lvls)) %>%
  
  ggplot(aes(x=time, y=avg_hourly_adj)) + 
  facet_wrap(facets = vars(background_adj)) +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
  geom_point(alpha=0.4, aes(col=background_adj)) + 
  geom_smooth()

ggsave(file.path(dt_out, "temp_adj2_time_series.png"), width = 14, height = 8)

# by DOW
underwrite_adj %>%
  mutate(dow = wday(date, label=T, week_start = "Monday")) %>%
  ggplot(aes(x=dow, y=avg_hourly_adj, col=background_adj, group=dow)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) 

ggsave(file.path(dt_out, "temp_adj2_dow.png"), width = 14, height = 8)
 
# --> why isn't there anything 5-8 AM? 

# by hour
underwrite_adj %>%
  mutate(hour = factor(hour, levels=c(1:23, 0))) %>%
  ggplot(aes(x=hour, y=avg_hourly_adj, col=background_adj, group=hour)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) 

ggsave(file.path(dt_out, "temp_adj2_hour.png"), width = 14, height = 8)


# --> by hour in a time series: date, hour (show that temp adj doesn't make sense b/c conc collected vary a Lot...but is this a space issue??)




##################################################################################################
# 1-sec DATA 
##################################################################################################
# --> color by road vs hwy?

road_dt %>%
  drop_na(ufp) %>%
  ggplot(aes(y=ufp)) +
  geom_boxplot() + 
  geom_hline(yintercept = c(0, 100, 100e3), linetype=2, col="red") + 
  scale_y_log10() + 
  labs(y="1-sec PNC (pt/cm3) measurement on-road")

ggsave(file.path(dt_out, "1sec_data.png"), width = 8, height = 8)

summary(road_dt$ufp)


# --> segment-level boxplots (winsorize?)





##################################################################################################
# time series fn

# --> update 'UFP' col labels
# --> linetype hwy vs non-hwy?

time_series_plot <- function(runs) {
  road_dt %>%
    filter(runname %in% runs) %>%
    drop_na() %>%
    left_join(select(underwrite_adj, runname, hour, hr_avg =bg_hour_avg)) %>%  
    pivot_wider(names_from = background_adj, values_from = background) %>% 
    pivot_longer(cols = c(ufp, contains("pct"), hr_avg), names_to = "UFP") %>%
    mutate(UFP = ifelse(UFP == "ufp", "measurement", paste("background", UFP))) %>%
    
    ggplot(aes(x=time)) +
    facet_wrap(~runname, scales="free_x") +
    geom_line(aes(y=value, col=UFP))  
  }

##################################################################################################

# TIME SERIES: ONROAD MEASURES VS 'BACKGROUND' (E.G., Brantley Fig 5)
# --> some EXTREME HOUR-LONG PERIODS? is this fixed w/ a wider ~3hr window?

set.seed(1)
time_series_plot(runs = sample(unique(underwrite_adj$runname), 4))
ggsave(file.path(dt_out, "time_series_sample.png"), width = 12, height = 8)


# --> TEMP. day w/ super high readings 500e3
high_runs <- road_dt %>% filter(ufp >450e3) %>% distinct(runname) %>% pull()
time_series_plot(high_runs)
ggsave(file.path(dt_out, "time_series_high_runs.png"), width = 12, height = 8)







# --> add time series plot of visits vs hourly adjustments (like above) 




##################################################################################################
# --> ?add example of ~1 wk period
# --> LS: consider overlaying same route driven on same time…driving same structure











##################################################################################################
# QC - VISUALIZE SPATIAL ADJUSTMENT PATTERNS 
##################################################################################################
# --> maps to make sure background doesn't vary by space (e.g., near vs away from hwys). use 1sec rolling quantiles?








##################################################################################################
# OVERNIGHT DATA
##################################################################################################
# --> BOXPLOTS W/ X AXIS TIME: START AT  5AM, GO THROUGH 12 AM?




##################################################################################################
# DONE
##################################################################################################

