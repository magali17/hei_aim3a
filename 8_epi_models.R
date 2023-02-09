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

#source("functions.R")
output_data_path <- file.path("Output", "epi")

use_cores <- 4

######################################################################
# LOAD DATA
######################################################################
main_pollutants <-c(#"no2", 
  "ns_total_conc",
  "ns_11.5", "ns_64.9",
  "ns_20.5", "ns_27.4", "ns_36.5", "ns_48.7", "ns_86.6", "ns_115.5", "ns_154.0"
  )

saveRDS(main_pollutants, file.path(output_data_path, "main_pollutants.rda"))

model_covars <- readRDS(file.path(output_data_path, "model_covars.rda"))
ap_prediction <- "avg_0_5_yr"

# modeling units
pnc_units <- 1000
no2_units <- 5

campaign_descriptions <- readRDS(file.path("Output", "Selected Campaigns", "selected_campaigns.rda")) %>%
  mutate(model_id = paste0("mb_", campaign_id)) %>%
  select(-campaign_id) %>%
  
  filter(variable %in% main_pollutants &
         # drop repetitive design
         !(design == "balanced seasons" & version=="4"))

saveRDS(campaign_descriptions, file.path("Output", "Selected Campaigns", "selected_campaigns_v2.rda"))

cs <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis.rda")) %>%
  rename(model_id = model) %>%
  filter(model_id %in% campaign_descriptions$model_id) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality")))) %>%

  left_join(select(campaign_descriptions, model_id, variable), by = "model_id") %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("ns_", variable), avg_0_5_yr/pnc_units,
                              ifelse(grepl("no2", variable), avg_0_5_yr/no2_units, NA)))

######################################################################
# MAIN MODEL
######################################################################
lm_fn <- function(df, ap_prediction.=ap_prediction, model_covars. = model_covars) {
  result <- lm(as.formula(paste("casi_irt ~", ap_prediction., "+", paste(model_covars., collapse = "+"))), 
               data = df #, weights =
  )
  #save model_id
  result$model_id <- first(df$model_id)
  return(result)
}


message("running models...")
models <- mclapply(group_split(cs, model_id), 
                   mc.cores=use_cores, 
                   function(x) {lm_fn(df=x)})

saveRDS(models, file.path(output_data_path, "models.rda"))

message("saving model coeficients...")
# save coefficient estimates
model_coefs0 <- mclapply(models, mc.cores=use_cores, function(x) {
  temp <- data.frame(
    model_id = x$model_id,
    est = as.vector(coef(x)[ap_prediction]),
    lower = confint(x)[ap_prediction, 1],
    upper = confint(x)[ap_prediction, 2],
    se = coef(summary(x))[ap_prediction, "Std. Error"])
}) %>%
  bind_rows() %>%
  mutate(significant = ifelse((lower <0 & upper <0) | 
                                (lower >0 & upper >0), TRUE, FALSE))

model_coefs <- left_join(model_coefs0, campaign_descriptions, by = "model_id")

saveRDS(model_coefs, file.path(output_data_path, "model_coefs.rda"))

message("done with script")
