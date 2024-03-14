#script purpose: evaluate UK-PLS model performances

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

pacman::p_load(tidyverse,
               parallel, #mclapply; detectCores()
               sf,  
               units # set_units()
)    

use_cores <- 5
set.seed(1)

dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

# # crosswalk location
# wc_path <- file.path("Output", "model_crosswalks")
# if(!dir.exists(wc_path)){dir.create(wc_path)}

##################################################################################################
# LOAD DATA
##################################################################################################
# mapping variables
project_crs <- 4326  #lat/long
m_crs <- 32148
 
# uk predictions
predictions <- readRDS(file.path(dt_path, "UK Predictions", "all_predictions.rda")) %>% 
  pivot_longer(contains("estimate"), names_to = "reference", values_to = "estimate")  

##################################################################################################
# CV STATS FUNCTION
##################################################################################################

# Fn returns RMSE and MSE-based R2 for a given dataset
validation_stats <- function(dt, prediction, reference){

  # MSE of predictions
  MSE_pred <- mean((dt[[reference]] - dt[[prediction]])^2)
  # MSE of observations (for R2 denominator)
  MSE_obs <- mean((dt[[reference]] - mean(dt[[reference]]))^2)
  
  RMSE = sqrt(MSE_pred)
  MSE_based_R2 = max(1 - MSE_pred/MSE_obs, 0)
  # alternative gives same mse-based R2
  # caret::R2(pred = dt$prediction,obs =dt$estimate, form = "traditional")
  reg_based_R2 = cor(dt[[reference]], dt[[prediction]], method = "pearson")^2
  
  result <- distinct(dt, campaign, design, version, variable, out_of_sample, reference) %>%
    mutate(
      no_sites = nrow(dt),
      RMSE = RMSE,
      MSE_based_R2 = MSE_based_R2,
      reg_based_R2 = reg_based_R2
    )
  
  return(result)
  
}

saveRDS(validation_stats, file.path(dt_path, "validation_stats_fn.rda"))
##################################################################################################
# don't do traditional assessment for spatial clustering - distance analysis
message("calculating performance statistics")
 

model_perf0 <- mclapply(group_split(predictions, campaign, design, version, variable, out_of_sample, reference), 
                       mc.cores = use_cores,
                       validation_stats, prediction = "prediction", reference = "estimate") %>%
  bind_rows()




##################################################################################################
#  MODEL CROSSWALK 
##################################################################################################
# distinct(model_perf0, variable), 
#          design, version, 
#          campaign
#          )

cw <- model_perf0 %>% 
  select(variable, design, version, campaign) %>% 
  distinct() %>%  
  arrange(variable, design, version, campaign) %>%
  mutate(
    var_code = gsub("\\.\\d", "", variable),
    var_code = gsub("_","", var_code),
    var_code = ifelse(var_code=="nstotalconc", "nstot", 
                      #ifelse(var_code=="ns10100", "ns100", 
                      var_code #)
    ),
    
    #var_code = case_when(
    # grepl("10_100", variable) ~ "ns100",
    # grepl("11.5", variable) ~ "ns011",
    # grepl("115.5", variable) ~ "ns115",
    # grepl("15.4", variable) ~ "ns015",
    # grepl("ns_20.5", variable) ~ "ns020",
    # grepl("ns_27.4", variable) ~ "ns027",
    # grepl("ns_27.4", variable) ~ "ns027",
    # variable == "ns_36.5" ~ "ns036",
    # variable == "ns_48.7" ~ "ns048",
    # variable == "ns_64.9" ~ "ns064",
    # variable == "ns_86.6" ~ "ns086",
    # variable == "ns_total_conc" ~ "nstot",
    # 
    # # --> CHECK. OK?
    # variable == "pnc_noscreen" ~ "ptrak",
    # TRUE ~NA),
    
    version_code = case_when(
      grepl("season", design) & version==1 ~"s1",
      grepl("season", design) & version==2 ~"s2",
      grepl("season", design) & version==3 ~"s3",
      grepl("season", design) & version==4 ~"s4",

      version=="rush" ~"rh",
      version=="business" ~"bh",
      grepl("12_visits", version) ~ "v12",
      grepl("6_visits", version) ~ "v06",
      grepl("4_visits", version) ~ "v04",

      grepl("full", design) ~ "all" #, TRUE~NA
      ),
    
    model = paste("s", #stationary
                  var_code, 
                  version_code, 
                  str_pad(campaign, 2, pad = "0"), 
                  sep = "_"),
    model_no = row_number())

write.csv(cw, file.path(dt_path, "model_cw.csv"), row.names = F)
##################################################################################################


# label performance order 
model_perf <- model_perf0 %>% 
  group_by(design, version, variable, out_of_sample, reference) %>%
  arrange(MSE_based_R2) %>%
  mutate(performance = row_number()) %>%
  arrange(design, version, variable, out_of_sample, performance) %>%
  ungroup() %>%
  left_join(select(cw, -contains(c("code", "model_no"))), by = c("campaign", "design", "version", "variable"))
  
##################################################################################################
# SAVE DATA
##################################################################################################
select(model_perf , -no_sites) %>%
  saveRDS(., file.path(dt_path, "model_eval.rda"))

message("done with 3_model_eval.R")



##################################################################################################
#### TEST - check that test & CV results are similar. # looks as expected. CV performs worse than the test set.
# 

# library(ggplot2)
# model_eval <-readRDS(file.path(#"Output", "v1_20230131", 
#   dt_path,
#                                "model_eval.rda"))
# model_eval %>%
#   filter(grepl("total", variable),
#          reference == "gs_estimate"
#          ) %>%
#   ggplot(aes(x=version, y=MSE_based_R2, col=out_of_sample)) +
#   facet_wrap(~design, scales="free_x") +
#   geom_boxplot()
#ggsave(file.path("..", "Manuscript", ""))
