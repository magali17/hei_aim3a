
######################################################################
# SETUP
######################################################################
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, parallel,
               broom, #tidy()
               sandwich, lmtest # robust SE
               )    

set.seed(1)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
output_data_path <- file.path(dt_path, "epi", "20241108" #"20240725"
                              )
if(!dir.exists(output_data_path)) {dir.create(output_data_path, recursive = T)}

use_cores <- 1 #4
######################################################################
# LOAD DATA
######################################################################
main_pollutants <-c( 
  "ns_total_conc", "ns_10_100",
  "pnc_noscreen", #onroad & ML models use ptrak
  "no2" #HEI wants these
  )

saveRDS(main_pollutants, file.path(output_data_path, "main_pollutants.rda"))

main_pollutants_models <-c("nstot", "ns10100", "pncnoscreen", "no2")

model_covars <- readRDS(file.path(output_data_path, "model_covars.rda"))
# extended adjusted models
model_covars_extended <- readRDS(file.path(output_data_path, "model_covars_extended.rda"))
ap_prediction <- "avg_0_5_yr"

# modeling units - using same as Nancy et al. 2023  
pm25_units <- 1 
pnc_units <- 1900 
no2_units <- 3 

save(pm25_units, pnc_units, no2_units, file= file.path(output_data_path, "modeling_units.rdata"))
#####################################################################################
# STATIONARY DATA
cs0 <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis.rda")) %>%
  filter(grepl(paste(main_pollutants_models, collapse = "|"), model))

# cs0 %>% filter(grepl("_01", model), study_id==first(study_id)) %>% View()

cs <- cs0 %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality"))),
         #keep NS & P-TRAK exposure estimate from main epi model from issue 12 (for comparision against the all-data HEI model)
         cum_exp_ufp_10_42_MM_05_yr, cum_exp_ufp_20_1k_MM_05_yr,
         # full onroad ptrak model (primary model, scale 0.99, truncate 0.75)
         cum_exp_pnc_onrd_MM_05_yr
         ) %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("_ns|_pnc", model), avg_0_5_yr/pnc_units,
                             ifelse(grepl("no2", model), avg_0_5_yr/no2_units, NA)))

# data with issue 12 epi models (for reference), includes all-data stationary & plume adjusted on-road
cs_issue12_models <- select(cs, -c(avg_0_5_yr, model)) %>%
  distinct() %>%
  pivot_longer(contains("cum_exp_"), values_to = "avg_0_5_yr", names_to = "model") %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("_ufp_|_pnc_", model), avg_0_5_yr/pnc_units,
                             ifelse(grepl("no2", model), avg_0_5_yr/no2_units, NA)))

#####################################################################################
# NON-STATIONARY DATA
cs_r <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis_road.rda")) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality")))) %>%
  mutate(variable = "pnc_noscreen",
         # modeling units
         avg_0_5_yr =  avg_0_5_yr/pnc_units)

#####################################################################################
# ML MODELS
cw_ml <- read.csv(file.path(dt_path, "model_ml_cw.csv")) %>%
  mutate(model = gsub("<poll>", "", Issue.17.Model),
         model = paste0(ufp_pollutant, model),
         model = str_trim(model),
         variable = "pnc_noscreen") %>%
  select(model, method=Method, variable)

write.csv(cw_ml, file.path(dt_path, "model_ml_cw2.csv"), row.names = F)

cs_ml <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis_machine_learning.rda")) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality")))) %>%
  # modeling units
  mutate(avg_0_5_yr =  avg_0_5_yr/pnc_units)

#####################################################################################
# LCM MODELS
cw_lcm_no2 <- read.csv(file.path(dt_path, "model_cw_lcm_no2.csv")) %>%
  select(ST.Model.Label., SP.Model.Label., Description.)

cw_lcm <- read.csv(file.path(dt_path, "model_cw_lcm0.csv")) %>%
  mutate(ST.Model.Label. = ifelse(ST.Model.Label.=="", NA, ST.Model.Label.)) %>%
  bind_rows(cw_lcm_no2) %>%
  pivot_longer(cols = c(ST.Model.Label., SP.Model.Label.), names_to = "model_type", values_to = "model") %>%
  drop_na(model) %>%
  rename(method = Description., ) %>%
  mutate(model_type = substr(model_type, 1,2),
         model = str_trim(model),
         model = gsub(" ", "", model),
         method = str_trim(method)) %>%
  # add full SP models to CW
  add_row(method = c("Full Model", "Full Model"),
          model_type = c("ST", "SP"),
          model = c("full_pm25_ST", "full_pm25_SP")
          )

write.csv(cw_lcm, file.path(dt_path, "model_cw_lcm.csv"), row.names = F)

cs_lcm <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis_lcm.rda")) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality")))) %>%
  #mutate(variable = "pnc_noscreen")
  #modeling units
  mutate(avg_0_5_yr = ifelse(grepl("pm25", model), avg_0_5_yr/pm25_units,
                           ifelse(grepl("no2", model), avg_0_5_yr/no2_units, NA)))

# filter(cs_lcm, study_id==first(study_id)) %>% View()

######################################################################
# EPI MODELS
######################################################################
# using robust SEs for lm() (10/9/24) 
lm_fn <- function(df, ap_prediction. = ap_prediction, model_covars. = model_covars) {
  # Fit the linear model
  lm_formula <- as.formula(paste("casi_irt ~", ap_prediction., "+", paste(model_covars., collapse = "+")))
  
  result <- lm(lm_formula, data = df)
  
  # Calculate robust covariance matrix estimators
  ## type HC3 is common and the default
  robust_se <- vcovHC(result, type = "HC3")
  
  # Update the model summary with robust standard errors 
  robust_result <- coeftest(result, vcov = robust_se)
  
  # Save the model name in the result object
  result$model <- first(df$model)
  
  # Return the robust result and the original model object
  list('result' = result, 
       'robust_result' = robust_result)
}

######################################################################
# FN RETURNS MODEL COEFFICIENT SUMMARY
# dt=models 
get_model_results <- function(dt) {
  
  # x=dt[[1]]
  mclapply(dt, mc.cores=use_cores, function(x) {
  temp <- data.frame(
    model = x$result$model,
    est = as.vector(coef(x$robust_result)[ap_prediction]),
    lower = confint(x$robust_result)[ap_prediction, 1],
    upper = confint(x$robust_result)[ap_prediction, 2],
    se = x$robust_result[ap_prediction, "Std. Error"],
    n=nobs(x$result)
    )
  }) %>%
  bind_rows() %>%
  mutate(significant = ifelse((lower <0 & upper <0) | 
                                (lower >0 & upper >0), TRUE, FALSE))
  } 

######################################################################
get_model_results_all_coefs <- function(dt) {
 
  mclapply(dt, mc.cores=use_cores, function(x){
    tidy(x$robust_result, conf.int = T) %>%
      mutate(model = x$result$model,
             n=nobs(x$result)) %>%
      rename(lower = conf.low, 
             upper=conf.high, 
             est=estimate, 
             se=std.error)
  }) %>%
    bind_rows() %>%
      mutate(significant = ifelse((lower <0 & upper <0) |
                                    (lower >0 & upper >0), TRUE, FALSE))
  }

######################################################################
# STATIONARY DATA
message("running STATIONARY models...")
models <- mclapply(group_split(cs, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models, file.path(output_data_path, "models.rda"))

message("saving model coeficients...")
model_coefs0 <- get_model_results(models)
saveRDS(model_coefs0, file.path(output_data_path, "model_coefs.rda"))
# same as above but raw for all coefficients
model_coefs_all <- get_model_results_all_coefs(models)
saveRDS(model_coefs_all, file.path(output_data_path, "model_coefs_all.rda"))

###########
# issue 12 reference models: NS & P-TRAK
## note that these are developed slighly differntly (see roadside epi paper, e.g., 3 PLS components...)

## reduced models
message("running ISSUE 12 reference epi models...")
models_issue12 <- mclapply(group_split(cs_issue12_models, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_issue12, file.path(output_data_path, "models_issue12.rda"))

message("saving model coeficients...")
model_coefs_issue12 <- get_model_results(models_issue12)
saveRDS(model_coefs_issue12, file.path(output_data_path, "model_coefs_issue12.rda"))

# same as above but raw for all coefficients
model_coefs_all_issue12 <- get_model_results_all_coefs(models_issue12)
saveRDS(model_coefs_all_issue12, file.path(output_data_path, "model_coefs_issue12_all.rda"))

## EXTENDED MODELS
message("...running extended models")
models_issue12_extended <- mclapply(group_split(cs_issue12_models, model), mc.cores=use_cores, function(x) {lm_fn(df=x, model_covars. = model_covars_extended)})
saveRDS(models_issue12_extended, file.path(output_data_path, "models_issue12_extended.rda"))

message("saving model coeficients...")
model_coefs_all_issue12_extended <- get_model_results_all_coefs(models_issue12_extended)
saveRDS(model_coefs_all_issue12_extended, file.path(output_data_path, "model_coefs_issue12_all_extended.rda"))


####################################
# 5/20/24. extended models
message("running extended models...")
models_extended <- mclapply(group_split(cs, model), mc.cores=use_cores, function(x) {lm_fn(df=x, model_covars. = model_covars_extended)})
saveRDS(models_extended, file.path(output_data_path, "models_extended.rda"))

message("saving model coeficients...")
model_coefs0_extended <- get_model_results(models_extended)
saveRDS(model_coefs0_extended, file.path(output_data_path, "model_coefs_extended.rda"))

# same as above but raw for all coefficients
model_coefs_extended_all <- get_model_results_all_coefs(models_extended)
saveRDS(model_coefs_extended_all, file.path(output_data_path, "model_coefs_extended_all.rda"))

####################################
# 7/3/24. 2010+ cohort
message("running 2010+ models...")
models_2010 <- mclapply(group_split(cs, model), mc.cores=use_cores, function(x) {
  x %>%
    filter(as.numeric(as.character(year2)) >=2010) %>%
    lm_fn(df=., model_covars. = model_covars_extended)
  })
saveRDS(models_2010, file.path(output_data_path, "models_2010.rda"))

message("saving model coeficients...")
model_coefs_extended_2010 <- get_model_results_all_coefs(models_2010)
saveRDS(model_coefs_extended_2010, file.path(output_data_path, "model_coefs_extended_2010.rda"))

######################################################################
# NON-STATIONARY (ROAD) DATA
message("running NON-STATIONARY models...")
models_r <- mclapply(group_split(cs_r, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_r, file.path(output_data_path, "models_road.rda"))

message("saving model coeficients...")
model_coefs_r <- get_model_results(models_r)  
saveRDS(model_coefs_r, file.path(output_data_path, "model_coefs_road.rda"))

# same as above but raw for all coefficients
models_r_all <- get_model_results_all_coefs(models_r)
saveRDS(models_r_all, file.path(output_data_path, "models_r_all.rda")) 

####################################
# 5/20/24. extended models
message("running extended models...")
models_r_extended <- mclapply(group_split(cs_r, model), mc.cores=use_cores, function(x) {lm_fn(df=x, model_covars. = model_covars_extended)})
saveRDS(models_r_extended, file.path(output_data_path, "models_road_extended.rda"))

message("saving model coeficients...")
model_coefs_r_extended <- get_model_results(models_r_extended)  
saveRDS(model_coefs_r_extended, file.path(output_data_path, "model_coefs_road_extended.rda"))

######################################################################
# MACHINE LEARNING EXPOSURE MODELS
message("running MACHINE LEARNING models...")
models_ml <- mclapply(group_split(cs_ml, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_ml, file.path(output_data_path, "models_ml.rda"))

message("saving model coeficients...")
model_coefs_ml <- get_model_results(models_ml)
saveRDS(model_coefs_ml, file.path(output_data_path, "model_coefs_ml.rda"))

# same as above but raw for all coefficients
models_ml_all <- get_model_results_all_coefs(models_ml)
saveRDS(models_ml_all, file.path(output_data_path, "models_ml_all.rda"))

#####################################################################################
# LCM MODELS
message("running LCM models...")
models_lcm <- mclapply(group_split(cs_lcm, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_lcm, file.path(output_data_path, "models_lcm.rda"))

message("saving model coeficients...")
model_coefs_lcm <-  get_model_results(models_lcm)
saveRDS(model_coefs_lcm, file.path(output_data_path, "model_coefs_lcm.rda"))

# same as above but raw for all coefficients
models_lcm_all <- get_model_results_all_coefs(models_lcm)
saveRDS(models_lcm_all, file.path(output_data_path, "models_lcm_all.rda"))

######################################################################

message("done with script")
