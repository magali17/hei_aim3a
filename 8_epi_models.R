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
campaign_descriptions <- readRDS("Output/Selected Campaigns/selected_campaigns.rda") %>%
  mutate(model_id = paste0("mb_", campaign_id)) %>%
  select(-campaign_id)

cs <- readRDS(file.path(output_data_path, "dt_for_cross_sectional_analysis.rda")) %>%
  select(-c(ends_with(c("MM_05_yr", "coverage", "quality"))))

model_covars <- readRDS(file.path(output_data_path, "model_covars.rda"))

ap_prediction <- "avg_0_5_yr"
######################################################################
# MAIN MODEL
######################################################################
lm_fn <- function(df, ap_prediction.=ap_prediction, model_covars. = model_covars) {
  result <- lm(as.formula(paste("casi_irt ~", ap_prediction., "+", paste(model_covars., collapse = "+"))), 
               data = df #, weights =
  )
  #save model_id
  result$model_id <- first(df$model)
  return(result)
}

message("running models...")
models <- mclapply(group_split(cs, model), mc.cores=use_cores, function(x) {lm_fn(df=x)})
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


# --> check that results make sense - e.g. rows aren't just duplicates

model_coefs <- left_join(model_coefs0, campaign_descriptions, by = "model_id")

saveRDS(model_coefs, file.path(output_data_path, "model_coefs.rda"))

message("done with script")
