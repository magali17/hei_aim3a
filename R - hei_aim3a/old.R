
##################################################################################################
# FROM r0_temporal_adjustment.R
##################################################################################################
# # Overnight data
# overnight <- readRDS(file.path("data", "Overnight Collocations", "overnight_data_2023-09-11.rda")) %>%
#   #most of the data is at BH
#   filter(
#     # 10W data is only for March 2020; WILCOX data is for after the study period
#     site == "AQSBH",
#     variable == "ns_total_conc",
#     # most (86.5%) of the data comes from PMSCAN_3 (vs PMSCAN_2)
#     # drop PMSCAN_2. It is used less & produces higher readings when collocated in June that would probably need to be calibrated
#     instrument_id=="PMSCAN_3") %>%
#   rename(time=timeint) %>%
#   mutate(date = date(time),
#          month = month(time, label=TRUE),
#          month = factor(month, ordered = F),
#          dow = wday(time, label=T, week_start = "Monday"),
#          dow = factor(dow, ordered = F),
#          weekday = ifelse(dow %in% c("Sat", "Sun"), "weekend", "weekday"),
#          hour = hour(time),
#          hour = factor(hour),
#   ) #%>%
# 
#   # --> don't average?
# 
#   # # hourly averages
#   # group_by(site, date, hour, month, dow, weekday, variable) %>%
#   # summarize(value = mean(value, na.rm = T)) %>%
#   # ungroup()

# ##################################################################################################
# # 2. BACKGROUND ADJUSTMENT USING THE COLLECTED DATA
# ##################################################################################################
# # e.g., see Hankey 2015 LUR... method summary:
# # "(1) subtracting instantaneous background concentration estimates (via the underwrite function) from all
# # instrument-reported concentrations, (2) calculating mean reference-site (i.e., background) measurements among all
# # sampling days, and (3) adding the mean reference-site concentrations (from step 2) to the underwrite adjusted
# # concentrations from step 1."
# # 
# # approach gap: does not adjust for overnight or weekend hours that are never measured in BH/RH designs
# 
# message("running background/local adjustment using the collected data")
# set.seed(1)
# 
# low_conc <- 0.01 #0.05
# 
# 
# # --> UPDATE
# 
# # x = group_split(visits, adjusted, campaign, design, visits, cluster_type)[[1]]
# visits_adj2 <- lapply(group_split(visits, adjusted, campaign, design, visits, cluster_type), function(x){
#   temp <- x %>%
#     mutate(hour = as.numeric(hour)) %>%
#     group_by(hour) %>%
#     mutate(quantile_background = quantile(median_value, low_conc)) %>% 
#     ungroup()
#   
#   # make the hourly adjustments smoother
#   temp_hrs <- distinct(temp, hour, quantile_background)
#   smooth_fit <- lm(quantile_background ~ ns(hour, knots=c(12, 15)), data = temp_hrs)
#   temp_hrs <- temp_hrs %>%
#     mutate(smooth_background = predict(smooth_fit, newdata=.),
#            # long-term average estimate based on background concentrations from the collected campaign data
#            lta_background = mean(smooth_background))
#      
#   # apply hourly adjustments, add the LTA background back in
#   temp <- left_join(temp, temp_hrs, by=c("hour", "quantile_background")) %>%
#     mutate(local_conc = median_value - smooth_background,
#            median_value_adjusted = local_conc + lta_background)
#   }) %>%
#   bind_rows() %>%
#   mutate(version = paste(bh_version, "temp adj 2"))
# 
# saveRDS(visits_adj2, file.path(dt_pt, "bh_visits_background_temporal_adj.rds")) 
# 
# annual_adj2 <- visits_adj2 %>%
#   group_by(id, adjusted, actual_visits, campaign, design, visits, version, cluster_type, cluster_value) %>%
#   summarize(annual_mean = mean(median_value_adjusted, na.rm=T)) %>%
#   ungroup() 
# 
# # temp file
# saveRDS(annual_adj2, file.path(dt_pt, "bh_site_avgs_background_temporal_adj.rds"))

##################################################################################################


