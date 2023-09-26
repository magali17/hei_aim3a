# designs for Sun-Young Kim's work
# these designs are for the unscreened P-TRAK

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

pacman::p_load(tidyverse, kableExtra,
               parallel, #mclapply; detectCores()
               future.apply, #future_replicate()
               lubridate, # %within%
               pls, gstat, sf # UK-PLS MODEL
)    

# ggplot settings
theme_set(theme_bw())
theme_update(legend.position = "bottom")


source("functions.R")
latest_version <- "v3_20230321" 

#load the prediction workspace
dt_path <- file.path("Output", latest_version)
load(file.path(dt_path, "uk_workspace.rdata"))


set.seed(1)

## for future.apply::future_replicate()  
# availableCores() #8 
plan(multisession, workers = 6)

dt_path_sun <- file.path(dt_path, "sun_validations")
if(!dir.exists(file.path(dt_path_sun))) {dir.create(dt_path_sun, recursive = T)}

##################################################################################################
# COMMON VARIABLES
##################################################################################################
keep_vars <- c("pnc_noscreen")

# number of simulations
sim_n <- 30

# number of samples for fewer hours, seasons, reduced balance
fewer_hrs_seasons_n <- 12

# sims for Sun-Young Kim
fewer_sites <- c(seq(100, 250, 50))

##################################################################################################
# UPLOAD DATA
##################################################################################################
stops_w <- readRDS(file.path(dt_path, "stops_used.rda")) #%>% filter()

true_annual <- stops_w %>%
  group_by(location) %>%
  # note, visit # is wrong here b/c there are NAs
  mutate(visits = n()) %>%
  summarize_at(all_of(vars(keep_vars, visits)), ~mean(., na.rm=T)) %>%
  mutate(
    campaign = 1,
    design = "full",
    version = "all training data"
  ) %>%
  ungroup()

##################################################################################################
# FUNCTIONS
##################################################################################################
one_sample_avg <- function(my_list, my_sampling_fn) {
  result <- suppressWarnings( 
    mclapply(my_list, mc.cores = 6, FUN = my_sampling_fn) %>%
      #unlist results 
      bind_rows() %>%
      group_by(location) %>%
      mutate(visits = n()) %>%
      #calculate annual average
      summarize_at(all_of(c(keep_vars, "visits")), ~mean(., na.rm=T)))
  
  return(result)
} 


##################################################################################################
# SAMPLING DESIGNS
##################################################################################################

message ("DESIGNS FOR SUN")
message("fewer sites")

site_n2 <- fewer_sites
set.seed(1)

fewer_sites_df <- data.frame()

for(i in 1:sim_n) {
  temp <- lapply(site_n2, function(x) {
    #sample sites
    sample_sites <- sample(unique(stops_w$location), size = x, replace = F)
    
    ##sample visits
    filter(stops_w, location %in% sample_sites) %>%
      group_by(location) %>%
      # slice_sample(n = v, replace=T) %>%
      mutate(visits = n()) %>%
      #calculate annual average
      summarize_at(all_of(c(keep_vars, "visits")), ~mean(., na.rm=T)) %>%
      mutate(version = paste0(x, "_sites"))
  }) %>%
    bind_rows() %>%
    ungroup() %>%
    mutate(campaign = i,
           design = "fewer sites",
           #spatial_temporal = "spatial"
           )
  
  fewer_sites_df <- rbind(fewer_sites_df, temp)
  
}

# names(fewer_sites_df)

##################################################################################################
message("weekdays")
set.seed(1)

weekdays_df <- future_replicate(n = sim_n,
                                simplify = F,
                                expr = one_sample_avg(my_list = group_split(stops_w, location), 
                                                      my_sampling_fn = function(x) {
                                                        x %>% filter(tow2 == "weekday") %>%
                                                          slice_sample(n=fewer_hrs_seasons_n, replace=T)
                                                      })) %>%
  bind_rows() %>%
  group_by(location) %>%
  mutate(
    campaign = row_number(),
    version = "weekdays",
    design = "fewer days") %>%
  as.data.frame()


##################################################################################################
# combine datsets
annual_training_set_sun <- rbind(true_annual,
                                 fewer_sites_df, 
                                 weekdays_df) 

saveRDS(annual_training_set_sun, file.path(dt_path_sun, "annual_training_set_sun.rda"))



##################################################################################################
# RANDOM FOLDS FOR CV
##################################################################################################
annual_sun <- annual_training_set_sun %>% #readRDS(file.path(dt_path, "annual_training_set.rda")) %>%
  #add covariates
  left_join(modeling_covars) %>%
  #convert to sf
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) %>%
  # log transform before modeling
  # some PNC annual avgs are 0, which causes issues logging values
  mutate_at(vars(all_of(keep_vars)), ~ifelse(.== 0, 1, .)) %>%
  mutate_at(vars(all_of(keep_vars)), ~log(.)) %>%
  gather("variable", "value", keep_vars) %>%
  relocate(variable, value, .before = longitude) %>%
  
  drop_na(value) %>% 
  
  st_as_sf()
############################################################################################################
random_fold2 <- function(df, k.=k, object_is_st="yes") {
  #make sure temporal sims receive same fold designation
  set.seed(2)
  
  if(object_is_st=="yes") {df <- st_drop_geometry(df)  }
  
  result <- df %>% 
    distinct(location, #spatial_temporal, 
             design, version, campaign) %>%
    mutate(random_fold = sample(1:k.,size = nrow(.), replace = T ))
  
  return(result)
}

message("generating random folds")

random_fold_df <- lapply(group_split(annual_sun, #stops_w, #spatial_temporal, 
                                     design, version, campaign),
                         function(x) random_fold2(x, k.=k)) %>%
  bind_rows()

#join fold to annual 
annual_sun <- suppressMessages(left_join(annual_sun, random_fold_df)) %>%
  select(random_fold, everything())

saveRDS(annual_sun, file.path(dt_path_sun, "annual_training_set2_sun.rda"))

##################################################################################################
# CV
##################################################################################################
print(paste0("Running random ", k, " FCV"))

set.seed(1)
cv_predictions0 <- #mclapply(group_split(annual_sun, design, version, campaign, variable), 
  lapply(group_split(annual_sun, design, version, campaign, variable), 
                            #mc.cores = use_cores, 
                            FUN = do_cv, fold_name = "random_fold") %>%
  bind_rows() 


cv_predictions <- cv_predictions0 %>% 
  #select(all_of(common_vars)) %>%
  mutate(out_of_sample = "CV") #%>% drop_na(prediction)


##################################################################################################
# OUT-OF-SAMPLE VALIDATION FOR FEWER SITES DESIGNS
##################################################################################################
# ?? VALIDATE at all other sites

site_names <- unique(annual_sun$location)
oos_df <- annual_sun %>%
  filter(design== "full")

print("test set validation")

oos_predictions0 <- data.frame()

set.seed(1)

# x=group_split(filter(annual_sun, design == "fewer sites"), design, version, campaign)[[1]]

oos_predictions0 <- #mclapply(group_split(filter(annual_sun, variable == i), design, version, campaign),
    lapply(group_split(filter(annual_sun, design == "fewer sites"), design, version, campaign),
                 #mc.cores = use_cores,
                 function(x) {
                   
                   training_sites <- unique(x$location)
                   
                   oos_sites <- oos_df %>%
                     filter(!location %in% training_sites)
                   
                   df = uk_pls(modeling_data = x, new_data = oos_sites) %>%
                     #fn has binding issues later if don't drop geom
                     st_drop_geometry() %>%
                     #add info to new dataset about the prediction model
                     mutate(
                       #spatial_temporal = first(x$spatial_temporal),
                       design = first(x$design),
                       version  = first(x$version),
                       campaign = first(x$campaign)
                     )
                 }) %>%
    bind_rows()

oos_predictions <- oos_predictions0 %>%
  #select(all_of(common_vars)) %>%
  mutate(out_of_sample = "External")



##################################################################################################
predictions_sun <- rbind(cv_predictions,
                         oos_predictions)

message("saving predictions")
saveRDS(predictions_sun, file.path(dt_path_sun, "predictions_sun.rda"))

# ADD REFERENCE VALUES
validation_stats <- readRDS(file.path(dt_path, "validation_stats_fn.rda"))

predictions_estimates_sun <- predictions_sun %>%
  left_join(select(oos_df, location, estimate = value), by="location") %>%
  mutate(reference = "gs_estimate") %>%
  mutate_at(vars(contains(c("estimate", "prediction", "value"))), ~exp(.))



##################################################################################################
# MODEL PERFORMANCE
##################################################################################################
message("calculating performance statistics")

model_perf0 <- lapply(group_split(predictions_estimates_sun, campaign, design, version, #variable, 
                                    out_of_sample, reference
                                    ), 
                        #mc.cores = use_cores,
                        validation_stats, prediction = "prediction", reference = "estimate") %>%
  bind_rows()


model_perf_overall <- lapply(group_split(mutate(predictions_estimates_sun, out_of_sample="CV+External"), campaign, design, version, #variable, 
                                  out_of_sample, reference
), 
#mc.cores = use_cores,
validation_stats, prediction = "prediction", reference = "estimate") %>%
  bind_rows() %>%
  # numbers are the same for other designs that only have CG predictions
  filter(design == "fewer sites")


model_perf <- rbind(model_perf0, model_perf_overall) %>% 
  rename(validation = out_of_sample) %>%
  # will use the values from the other script. These are the same
  filter(design != "full")

##################################################################################################
# SUMMARIZE PERFORMANCE
##################################################################################################
model_perf_other_designs <- readRDS(file.path(dt_path, "model_eval.rda")) %>%
  filter(variable %in% keep_vars,
        reference== "gs_estimate",
        #drop 4 season
        version != "4"
        ) %>%
  mutate(
    validation = "CV",
    no_sites = 309
  )

#distinct(model_perf_other_designs, design, version)

all_stats <- rbind(model_perf,
                   select(model_perf_other_designs, names(model_perf))) %>%
  mutate(validation = factor(validation, levels = c("CV", "External", "CV+External")),
         version = gsub(" 309_sites", "", version)
         )



# table
model_perf_stats <- all_stats %>%
  arrange(validation) %>%
  group_by(design, version, validation, validation_sites_per_campaign = no_sites) %>%
  summarize(
    n_campaign_performances = n(),
    Min = min(MSE_based_R2),
    Q25 = quantile(MSE_based_R2, 0.25),
    Median = median(MSE_based_R2),
    Mean = mean(MSE_based_R2),
    Q75 = quantile(MSE_based_R2, 0.75),
    SD = sd(MSE_based_R2),
    Max = max(MSE_based_R2)
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round(., 2))

model_perf_stats %>%
  kable(caption = "Distribution of MSE-based R2 Across Campaigns (N=30 per design-version)") %>%
  kable_styling()

write.csv(model_perf_stats, file.path(dt_path_sun, "sampling_design_stats.csv"), row.names = F)


# plot
## does OOS generally eprform better than the CV b/c the model used to build it is larger (vs 80% of the data?)
ref_performance <- filter(all_stats, design == "full") %>% pull(MSE_based_R2)

# note, CV for designs that use all sites is based on 80% of 309 sites, which is ~ 247 sites, so similar to the 250 site external performance
all_stats %>%
  ggplot(aes(x=version, y=MSE_based_R2, col=validation)) + 
  facet_wrap(~design, scales = "free_x") +
  geom_hline(yintercept = ref_performance, linetype=2, alpha=0.5) +
  geom_boxplot()

ggsave(file.path(dt_path_sun, "mse_based_r2_plot.png"), width = 10, height = 7)

##################################################################################################
# notes
## balanced seasons, fewer days, fewr hours samples 12 visits/site


##################################################################################################

message("done with validation_for_sun.R")



