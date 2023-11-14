######################################################################
# NOTES
######################################################################
# --> is this the reference methods paper? Keller et al. 2017 (https://pubmed.ncbi.nlm.nih.gov/28099267/I )
#  or is this? Szpiro & Pacioreck 2013 (https://pubmed.ncbi.nlm.nih.gov/24764691/)

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

image_path <- file.path("..", "Manuscript", "Images", "v4", "other", "error") 
if(!file.exists(image_path)) {dir.create(image_path, recursive = T)}
# ggplot settings
theme_set(theme_bw())
theme_update(legend.position = "bottom")

######################################################################
# LOAD DATA
######################################################################
main_pollutants <- readRDS(file.path(dt_path, "epi", "main_pollutants.rda"))
# --> just look at this here?
main_pollutant1 <- c("ns_total_conc")

# modeling units (1,900 for PNC)
load(file.path(output_data_path, "modeling_units.rdata"))

# merged issue 12 [clean health dataset] & issue 16 (for non-parametric, bootstrapped measurement models)
cs <- readRDS(file.path(dt_path, "epi", "dt_for_cross_sectional_analysis_error.rda")) %>%
  select(-starts_with("avg_"), -visitdt, avg_0_5_yr) %>%
  #transform units to match modeling units
  mutate_at(vars(avg_0_5_yr), ~./pnc_units)

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

models_fp <- file.path(dt_path, "epi", "ref_models.rda")

if(file.exists(models_fp)) {
  models <- readRDS(models_fp)
  } else { 
    # note: this file has to be re-run & re-saved if anything changes with the models (e.g., covariates, data)
    models0 <- readRDS(file.path(dt_path, "epi", "models.rda")) 
  
    ## only keep reference (all data) models
    # x=ref_model0[[1]]
    models <- lapply(models0, function(x) {if(as.character(x$model) %in% ref_models) {x}})  
    rm(models0)
    models <- models[sapply(models,function(x) !is.null(x))]
    names(models) <- ref_models
    saveRDS(models, models_fp) 
}

# --> may need to update this "true" beta model
true_beta <- models$s_nstot_all_01 %>% tidy() %>%
  filter(term == "avg_0_5_yr") %>%
  pull(estimate)

# monitoring site predictions & true estimates from the full campaign in this study (from 2.3_uk_cv.R)
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
sim_n <- 500
cohort_n <- length(unique(cs$study_id)) #5409

######################################################################
# 1. NON-PARAMETRIC
# investigates classical-like measurement error that is associated with uncertainty in the exposure surface
# from Keller 2017: "The non-parametric bootstrap resamples monitor locations to reflect variation in the 
#    predicted exposure surface derived from different monitor locations and [participants] 
#    to capture sampling variability in the epidemiologic analysis arising from different subjects."
######################################################################
set.seed(1)
betas_np <- lapply(unique(cs$model), function(x) {
  betas <- tibble(model = x,
                     variable = main_pollutant1, #issue 16 uses total NS
                     beta_type1 = NA,
                     beta_type2 =  NA)
  
  health_model <- as.formula(paste("casi_irt ~ avg_0_5_yr" , "+", paste(model_covars, collapse = "+")))
  
  this_exposure_model <- filter(cs, model == x)
  this_cohort_sample <- sample_n(this_exposure_model, size = cohort_n, replace = T)  
  
  # health effects for the ~5k existing participants
  betas$beta_type1 <- lm(health_model, data = this_exposure_model) %>%
    tidy() %>%
    filter(term=="avg_0_5_yr") %>%
    pull(estimate)
  # health effects for 5k bootstrapped participants
  betas$beta_type2 <- lm(health_model, data = this_cohort_sample) %>%
    tidy() %>%
    filter(term=="avg_0_5_yr") %>%
    pull(estimate)
  
  betas
  }) %>%
  bind_rows()

saveRDS(betas_np, file.path(output_data_path, "beta_nonparametric.rda"))

######################################################################
# SUMMARIZE VARIABILITY
######################################################################
betas_np_long <- betas_np %>%
  pivot_longer(starts_with("beta_")) %>%
  mutate(
    bootstrap = "non-parametric",
    description = ifelse(name=="beta_type1", "health inference estimate from different monitor locations and sampling times",
                         ifelse(name=="beta_type2", "health inference estimate from different monitor locations, sampling times, and subjects", NA))) 

beta_variability <- betas_np_long %>%
  group_by(bootstrap, description) %>%
  summarize(
    N = n(),
    Min = min(value),
    Q25 = quantile(value, 0.25),
    Median = median(value),
    Mean = mean(value),
    Q75 = quantile(value, 0.75),
    IQR = IQR(value),
    SD = sd(value),
    Max = max(value)
  )

beta_variability

######################################################################
# RESULTS PLOT
######################################################################
print("Non-parametric analysis comparing the estimated association between PNC (per 1,900 pt/cm3) and CASI-IRT across bootstrapped samples with changing monitoring locations, sampling times, and subjects")
betas_np_long %>%
  mutate(description = ifelse(name=="beta_type1", "monitor locations + sampling times",
                         ifelse(name=="beta_type2", "monitor locations + sampling times + subjects", NA))) %>%
  ggplot(aes(x=description, y=value)) + 
  geom_hline(yintercept = true_beta, linetype=2) +
  geom_boxplot() +
  labs(y="Beta",
       x = "Added Variability"
       )
ggsave(file.path(image_path, "non_parametric_betas.png"), width = 6, height = 6)

######################################################################
# 2. PARAMETRIC
# looks at the impact of exposure measurement error on the health effect estimate
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
model_residuals <- lapply(models, function(x){
  tibble(residual = x$residuals,
         model = x$model)}) %>%
  bind_rows() %>%
  group_by(model) %>%
  summarize(sd = sd(residual))

######################################################################
# SIMULATE 'CASI' SCORE AT MONITORING LOCATIONS & RUN 'HEALTH' MODELS
######################################################################
cs_person <- cs %>%
  select(c(study_id, all_of(model_covars))) %>%
  distinct()

# mean/mode model covariate values
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
  mutate(visit_age_centered75 = mean_age,
         year2 = year_mode,
         male = mean_sex,
         degree = degree_mode)

# will only be looking at "ns_total_conc" in this analysis
monitoring_sites <- filter(monitoring_sites, variable== main_pollutant1)
this_pollutant <- filter(ref_models0, variable==main_pollutant1)
this_model <-models[[this_pollutant$model]]
residual_sd <- model_residuals$sd[model_residuals$model==this_pollutant$model] 

set.seed(1)
betas <- lapply(1:sim_n, function(x){
  betas_bs <- tibble(n = x,
                     variable = main_pollutant1,
                     obs_beta = NA,
                     pred_beta =  NA)
  
  sites_sample_bs <- monitoring_sites %>%
    sample_n(size = cohort_n, replace = T) %>%
    # the resulting beta from lm() later should be similar to the "all data" estimate since casi is simulated using this exposure
    rename(avg_0_5_yr = gs_estimate) %>%
    mutate(
      #predicted CASI
      simulated_casi = predict(this_model, newdata=.),
      #add noise
      simulated_casi = simulated_casi + rnorm(n=cohort_n, mean=0, sd=residual_sd)) %>%
    # rename exposure variable back to its original name
    rename(gs_estimate = avg_0_5_yr)
  
  # estimated beta using site estimates
  ## no need to add other covariates b/c they are identical across the sample. We are interested in differences in the betas
  ## this should produce a similar beta as the "all data" beta estimate
  betas_bs$obs_beta <- lm(simulated_casi ~ gs_estimate, data = sites_sample_bs) %>%
    tidy() %>%
    filter(term=="gs_estimate") %>%
    pull(estimate)
  
  # estimated beta using site predictions (measurement error added by using predictions)
  betas_bs$pred_beta <- lm(simulated_casi ~ prediction, data = sites_sample_bs) %>%
    tidy() %>%
    filter(term=="prediction") %>%
    pull(estimate)
  
  betas_bs 
}) %>%
  bind_rows()

saveRDS(betas, file.path(output_data_path, "betas_parametric.rda"))

######################################################################
# RESULTS PLOT
######################################################################
print("estimated association between PNC (per 1,900 pt/cm3) and CASI-IRT, adjusted for age, calendar year, sex, education")
line_bounds <- 0.3
betas %>%
  ggplot(aes(x=obs_beta, y=pred_beta)) + 
  geom_abline(slope = 1, intercept = 0, linetype=2, alpha=0.5) + 
  geom_abline(slope = c(1-line_bounds, 1+line_bounds), intercept = 0, linetype=3, alpha=0.3) + 
  geom_point() + 
  geom_smooth() + 
  #make x y equal size
  theme(aspect.ratio = 1) +
  labs(x = "Beta (observed PNC)", y= "Beta (predicted PNC)",
       caption = paste0("1-1 and +-", line_bounds*100, "% line")
  )

ggsave(file.path(image_path, "parametric_beta_obs_pred.png"), width = 6, height = 6)
######################################################################
# BIAS
######################################################################
bias <- betas %>%
  mutate(bias = pred_beta - obs_beta,
         bootstrap = "parametric",
         description = "health inference bias from using predicted (vs measured) PNC") %>%
  group_by(bootstrap, description) %>%
  summarize(N = n(),
            Min = min(bias),
            Q25 = quantile(bias, 0.25),
            Median = median(bias),
            # only really want the mean?
            Mean = mean(bias),
            Q75 = quantile(bias, 0.75),
            IQR = IQR(bias),
            SD = sd(bias),
            Max = max(bias)
  )
bias
######################################################################
# SAVE SUMMARY BETA RESULTS
######################################################################

rbind(beta_variability, bias) %>% 
  select(Bootstrap = bootstrap, Description = description, Mean, SD, everything()) %>%
  #mutate_if(is.numeric, ~format(., format="e", digits = 3))  %>%
  
  write.csv(., file.path(output_data_path, "beta_summary.csv"), row.names = F)
 





