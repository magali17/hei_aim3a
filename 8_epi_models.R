# --> NEED TO UPDATE dt_path when get new dataset

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

use_cores <- 4
######################################################################
# LOAD DATA
######################################################################

# --> TEMP: update this to include ptrak later

main_pollutants <-c( 
  "ns_total_conc", "ns_10_100",
  "ns_11.5", "ns_64.9",
  "ns_20.5", "ns_27.4", "ns_36.5", "ns_48.7", "ns_86.6", "ns_115.5", "ns_154.0",
  "pnc_noscreen" #onroad data use ptrak
  )

saveRDS(main_pollutants, file.path(output_data_path, "main_pollutants.rda"))

model_covars <- readRDS(file.path(output_data_path, "model_covars.rda"))
ap_prediction <- "avg_0_5_yr"

# modeling units
pnc_units <- 1000
no2_units <- 5

campaign_descriptions <- readRDS(file.path(dt_path, "Selected Campaigns", "selected_campaigns.rda")) %>%
  filter(variable %in% main_pollutants &
         # drop repetitive design
         !(design == "balanced seasons" & version=="4"))

saveRDS(campaign_descriptions, file.path(dt_path, "Selected Campaigns", "selected_campaigns_v2.rda"))

cs <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis.rda")) %>%
  filter(model %in% campaign_descriptions$model) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality")))) %>%
  left_join(select(campaign_descriptions, model, variable), by="model") %>%
  # modeling units
  mutate(avg_0_5_yr = ifelse(grepl("ns_|pnc_", variable), avg_0_5_yr/pnc_units,
                              ifelse(grepl("no2", variable), avg_0_5_yr/no2_units, NA)))

######################################################################
# MAIN MODEL
######################################################################
# df = group_split(cs, model)[[1]]
lm_fn <- function(df, ap_prediction.=ap_prediction, model_covars. = model_covars) {
  result <- lm(as.formula(paste("casi_irt ~", ap_prediction., "+", paste(model_covars., collapse = "+"))), data = df)
  #save model
  result$model <- first(df$model)
  return(result)
}


message("running models...")
models <- mclapply(group_split(cs, model), 
                   mc.cores=use_cores, 
                   function(x) {lm_fn(df=x)})

saveRDS(models, file.path(output_data_path, "models.rda"))

message("saving model coeficients...")
# save coefficient estimates
model_coefs0 <- mclapply(models, mc.cores=use_cores, function(x) {
  temp <- data.frame(
    model = x$model,
    est = as.vector(coef(x)[ap_prediction]),
    lower = confint(x)[ap_prediction, 1],
    upper = confint(x)[ap_prediction, 2],
    se = coef(summary(x))[ap_prediction, "Std. Error"])}) %>%
  bind_rows() %>%
  mutate(significant = ifelse((lower <0 & upper <0) | 
                                (lower >0 & upper >0), TRUE, FALSE))

model_coefs <- left_join(model_coefs0, campaign_descriptions)

saveRDS(model_coefs, file.path(output_data_path, "model_coefs.rda"))


message("done with script")
