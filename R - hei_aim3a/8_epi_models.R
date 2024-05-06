
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
pacman::p_load(tidyverse, parallel)    

set.seed(1)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))
output_data_path <- file.path(dt_path, "epi")

use_cores <- 1#4
######################################################################
# LOAD DATA
######################################################################
main_pollutants <-c( 
  "ns_total_conc", "ns_10_100",
  "pnc_noscreen" #onroad & ML models use ptrak
  )

saveRDS(main_pollutants, file.path(output_data_path, "main_pollutants.rda"))

model_covars <- readRDS(file.path(output_data_path, "model_covars.rda"))
ap_prediction <- "avg_0_5_yr"

# modeling units - using same as Nancy et al. 2023  
pm25_units <- 1 
pnc_units <- 1900#1000
no2_units <- 3 #5

save(pm25_units, pnc_units, no2_units, file= file.path(output_data_path, "modeling_units.rdata"))
#####################################################################################
# STATIONARY DATA

#additional designs added later. include descriptions & model eval
## note that fewer hour designs are only for total NS, so have to use the old ones for sensitivity analyses 
campaign_descriptions_other_designs <- readRDS(file.path(dt_path, "other_designs_model_eval.rda"))  %>%
  left_join(read.csv(file.path(dt_path, "other_designs_model_cw.csv"))  ) %>%
  mutate(reference = "gs_estimate")

campaign_descriptions0 <- readRDS(file.path(dt_path, "Selected Campaigns", "selected_campaigns.rda"))

# I accidentally dropped these other pollutants from the temporal adjustment, so using the original 30 campaigns for these variables. Shoudl be OK since all campaigns were selected randomly anayway
campaign_descriptions_fewer_hrs_sensitivity <- campaign_descriptions0 %>%
  filter(design == "fewer hours" & variable %in% c("pnc_noscreen", "ns_10_100"))
  
campaign_descriptions <- campaign_descriptions0 %>%
  filter(design %in% c("fewer total stops", "full")) %>%
  bind_rows(campaign_descriptions_fewer_hrs_sensitivity) %>%
  select(-performance) %>%
  bind_rows(select(campaign_descriptions_other_designs, names(.))) %>%
  filter(variable %in% main_pollutants) 

saveRDS(campaign_descriptions, file.path(dt_path, "Selected Campaigns", "selected_campaigns_v2.rda"))

cs0 <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis.rda")) 
cs <- cs0 %>%
  filter(model %in% campaign_descriptions$model) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality"))),
         #keep NS & P-TRAK exposure estimate from main epi model from issue 12 (for comparision against the all-data HEI model)
         cum_exp_ufp_10_42_MM_05_yr, cum_exp_ufp_20_1k_MM_05_yr
         ) %>%
  left_join(select(campaign_descriptions, model, variable), by="model") %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("ns_|pnc_", variable), avg_0_5_yr/pnc_units,
                              ifelse(grepl("no2", variable), avg_0_5_yr/no2_units, NA)))

# data with issue 12 epi models (for reference)
cs_issue12_models <- select(cs, -c(avg_0_5_yr, model, variable)) %>%
  distinct() %>%
  pivot_longer(contains("cum_exp_"), values_to = "avg_0_5_yr", names_to = "model") %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("_ufp_", model), avg_0_5_yr/pnc_units,
                             ifelse(grepl("no2", model), avg_0_5_yr/no2_units, NA)))

#####################################################################################
# NON-STATIONARY DATA
cw_r <- read.csv(file.path(dt_path, "onroad_model_cw.csv"))

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
cw_lcm <- read.csv(file.path(dt_path, "model_cw_lcm0.csv")) %>%
  mutate(ST.Model.Label. = ifelse(ST.Model.Label.=="", NA, ST.Model.Label.)) %>%
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
  mutate(variable = "pnc_noscreen")

######################################################################
# EPI MODELS
######################################################################
# df = group_split(cs, model)[[1]]
lm_fn <- function(df, ap_prediction.=ap_prediction, model_covars. = model_covars) {
  result <- lm(as.formula(paste("casi_irt ~", ap_prediction., "+", paste(model_covars., collapse = "+"))), data = df)
  #save model
  result$model <- first(df$model)
  return(result)
}

######################################################################
# FN RETURNS MODEL COEFFICIENT SUMMARY
get_model_results <- function(dt) {
  
  mclapply(dt, mc.cores=use_cores, function(x) {
  temp <- data.frame(
    model = x$model,
    est = as.vector(coef(x)[ap_prediction]),
    lower = confint(x)[ap_prediction, 1],
    upper = confint(x)[ap_prediction, 2],
    se = coef(summary(x))[ap_prediction, "Std. Error"],
    n=nobs(x)
    )
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
model_coefs <- left_join(model_coefs0, campaign_descriptions)
saveRDS(model_coefs, file.path(output_data_path, "model_coefs.rda"))


# issue 12 reference models: NS & P-TRAK
message("running ISSUE 12 reference epi models...")
models_issue12 <- mclapply(group_split(cs_issue12_models, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_issue12, file.path(output_data_path, "models_issue12.rda"))

message("saving model coeficients...")
model_coefs_issue12 <- get_model_results(models_issue12)
saveRDS(model_coefs_issue12, file.path(output_data_path, "model_coefs_issue12.rda"))

######################################################################
# NON-STATIONARY (ROAD) DATA
message("running NON-STATIONARY models...")
models_r <- mclapply(group_split(cs_r, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_r, file.path(output_data_path, "models_road.rda"))

message("saving model coeficients...")
model_coefs_r <- get_model_results(models_r) %>%
  left_join(cw_r)
saveRDS(model_coefs_r, file.path(output_data_path, "model_coefs_road.rda"))

######################################################################
# MACHINE LEARNING EXPOSURE MODELS
message("running MACHINE LEARNING models...")
models_ml <- mclapply(group_split(cs_ml, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_ml, file.path(output_data_path, "models_ml.rda"))

message("saving model coeficients...")
model_coefs_ml <-  get_model_results(models_ml) %>%
  left_join(cw_ml)
saveRDS(model_coefs_ml, file.path(output_data_path, "model_coefs_ml.rda"))

#####################################################################################
# LCM MODELS
message("running LCM models...")
models_lcm <- mclapply(group_split(cs_lcm, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
saveRDS(models_lcm, file.path(output_data_path, "models_lcm.rda"))

message("saving model coeficients...")
model_coefs_lcm <-  get_model_results(models_lcm) %>%
  left_join(cw_lcm)
saveRDS(model_coefs_lcm, file.path(output_data_path, "model_coefs_lcm.rda"))

######################################################################

message("done with script")
