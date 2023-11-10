
######################################################################
# SETUP
######################################################################
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, kableExtra, broom)    

set.seed(1)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

output_data_path <- file.path(dt_path, "error")
if(!file.exists(output_data_path)) {dir.create(output_data_path, recursive = T)}

######################################################################
# LOAD DATA
######################################################################
main_pollutants <- readRDS(file.path(dt_path, "epi", "main_pollutants.rda"))
main_pollutant1 <- c("ns_total_conc")

# cross-sectional health-exposure data - includes exposures from 500 bootstrapped exposure models
cs <- readRDS(file.path(dt_path, "epi", "dt_for_cross_sectional_analysis_error.rda"))
model_covars <- readRDS(file.path(dt_path, "epi", "model_covars.rda"))

# reference health models using all the data
ref_models0 <- read.csv(file.path(dt_path, "model_cw.csv")) %>%
  filter(variable %in% main_pollutants,
         design == "full") %>%
  select(variable, model)
ref_models <- ref_models0 %>% pull(model)

# health model with the all data exposure model
# ---> use this model vs what is in issue 12??
## all models with stationary data
models0 <- readRDS(file.path(dt_path, "epi", "models.rda")) #%>% filter()

## only keep ref models
# x=ref_model0[[1]]
models <- lapply(models0, function(x) {if(as.character(x$model) %in% ref_models) {x}})  
models <- models[sapply(models,function(x) !is.null(x))]
names(models) <- ref_models

# modeling units
load(file.path(output_data_path, "modeling_units.rdata"))

# site predictions & true estimates from the full campaign in this study (from 2.3_uk_cv.R)
monitoring_sites <- readRDS(file.path(dt_path, "UK Predictions", "all_predictions.rda")) %>%
  filter(design == "full",
         variable %in% main_pollutants) %>%
  select(location, variable, prediction, gs_estimate) %>%
  #transform units to match modeling units
  mutate_at(vars(prediction, gs_estimate), ~./pnc_units) %>%
  #add model name
  left_join(ref_models0)
unique_monitoring_sites <- unique(monitoring_sites$location)
######################################################################
# COMMON VARIABLES
######################################################################

sim_n <- 10 #500
cohort_n <-500#5e3

######################################################################
# 1. PARAMETRIC

######################################################################
# FULL MODEL COEFFICIENTS & RESIDUALS
######################################################################
# save model coefficients 
## x = models[[1]]
model_coefs <- lapply(models, function(x) {
  temp <- tidy(x) %>%
    select(term, estimate) %>%
    mutate(model = as.character(x$model))}) %>%
  bind_rows()

# get the SD of each model's residuals
# --> weird that the residuals are almost identical? or maybe the units are just small?
model_residuals <- lapply(models, function(x){
  tibble(residual = x$residuals,
         model = x$model)}) %>%
  bind_rows() %>%
  group_by(model) %>%
  summarize(sd = sd(residual))

######################################################################
# SIMULATE 'CASI' SCORE AT MONITORING LOCATIONS & RUN 'HEALTH' MODELS
######################################################################
cs_person <- mean_cohort_covariate_values <- cs %>%
  select(c(study_id, all_of(model_covars))) %>%
  distinct()

# mean/mode model covariate values
# --> could just use 0 since this is centered?
mean_age <- mean(cs_person$visit_age_centered75)
year_mode <- table(cs_person$year2) %>% as.data.frame() %>%
  filter(Freq == max(Freq)) %>%
  pull(Var1)
mean_sex <-  mean(cs_person$male)
degree_mode <- table(cs_person$degree) %>% as.data.frame() %>%
  filter(Freq == max(Freq)) %>%
  pull(Var1)

# add average covariate values to monitoring data
monitoring_sites <- monitoring_sites %>%
  mutate(
    visit_age_centered75 = mean_age,
    year2 = year_mode,
    male = mean_sex,
    degree = degree_mode
  )


# --> TEMP: CHANGE LATER?  looking just at "ns_total_conc" here
monitoring_sites <- filter(monitoring_sites, variable== main_pollutant1)
this_pollutant <- filter(ref_models0, variable==main_pollutant1)
this_model <-models[[this_pollutant$model]]
residual_sd <- model_residuals$sd[model_residuals$model==this_pollutant$model] 

# x=1
set.seed(1)
betas <- lapply(1:sim_n, function(x){
  betas_bs <- tibble(n = x,
                  variable = main_pollutant1,
                  obs_beta = NA,
                  pred_beta =  NA)
    
    
  sites_sample_bs <- monitoring_sites %>%
    sample_n(size = cohort_n, replace = T) %>%
    # --> use this here? seems cyclical to then use it below to estiamte a beta
    rename(avg_0_5_yr = gs_estimate) %>%
    mutate(
      #predicted CASI
      simulated_casi = predict(this_model, newdata=.),
      #add noise
      simulated_casi = simulated_casi + rnorm(n=cohort_n, mean=0, sd=residual_sd)) %>%
    # rename back to its original name
    rename(gs_estimate = avg_0_5_yr)
  
  # estimated betas using site estimates
  betas_bs$obs_beta <- lm(simulated_casi ~ gs_estimate, data = sites_sample_bs) %>%
   tidy() %>%
   filter(term=="gs_estimate") %>%
   pull(estimate)
  
  # estimated betas using site predictions
  betas_bs$pred_beta <- lm(simulated_casi ~ prediction, data = sites_sample_bs) %>%
    tidy() %>%
    filter(term=="prediction") %>%
    pull(estimate)
  
  betas_bs
  }) %>%
  bind_rows()


######################################################################
# BIAS
######################################################################

bias <- betas %>%
  mutate(bias = pred_beta - obs_beta) %>%
  summarize(min = min(bias),
            Q25 = quantile(bias, 0.25),
            median = median(bias),
            
            # --> only want this?
            mean = mean(bias),
            
            Q75 = quantile(bias, 0.75),
            IQR = IQR(bias),
            sd = sd(bias),
            max = max(bias)
            )
bias
saveRDS(bias, file.path(output_data_path, "bias_estimate.rda"))









######################################################################
# 2. NON-PARAMETRIC

######################################################################
#  
######################################################################
# Amanda:
#   k <- 1
# for (i in unique(issue16$model)){ # There are 500 of these
#   this_model <- merge(issue12, issue16[model == i]
#                       this_sample <- sample(this_model, size = 5000, replace = TRUE)
#                       beta_type1[k] <- lm(casi ~ exposure + z, data = this_model)
#                       beta_type2[k] <- lm(casi~exposure+z, data = this_sample)
# }


# --> USE ISSUE 16- this is a different analysis??



