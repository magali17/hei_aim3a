
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

pacman::p_load(#tidyverse
  dplyr, readr, lubridate
)    

prediction_path <- file.path("Output", "UK Predictions", 
                             "cohort"
                             #"grid"
)

if(!dir.exists(file.path(prediction_path, "KP"))){dir.create(file.path(prediction_path, "KP"))}

##################################################################################################
# PREP PREDICTION FILE FOR KP
##################################################################################################

var_names <- readRDS(file.path("Output", "keep_vars.rda"))
# # TEMP - having issues w/ last bin
# var_names <- setdiff(var_names, "ns_205.4")

grp1_vars <- c("no2", "ns_total_conc")
grp2_vars <- setdiff(var_names, grp1_vars)



predictions0 <- lapply(var_names, 
                       function(x) {
                         read_csv(file.path(prediction_path, x, "predictions.csv"), show_col_types = FALSE)
                       }) %>%
  bind_rows() 

predictions <- predictions0 %>%
  mutate(
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01 "),
    end_date = ymd("2021-07-09 "),
    model = paste0("mb_", campaign_id)
  ) %>%
  select(location_id, start_date, end_date, model, 
         variable,
         prediction)


#write.csv(predictions, file.path(prediction_path, "KP", "predictions_no2_total_pnc.csv"), row.names = F)
#write_csv(predictions, file.path(prediction_path, "KP", "predictions_no2_total_pnc.csv.gz"))

saveRDS(predictions, file.path(prediction_path, "KP", "predictions_all.rda"))

predictions %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_path, "KP", "predictions_all.csv"))

filter(predictions, variable %in% grp1_vars) %>% # distinct(variable)
  select(-variable) %>%
  write_csv(., file.path(prediction_path, "KP", "predictions_grp1.csv"))

filter(predictions, variable %in% grp2_vars) %>%
  select(-variable) %>%
  write_csv(., file.path(prediction_path, "KP", "predictions_grp2.csv"))


##################################################################################################
# QC check
##################################################################################################
#can't view everything on excel. Looks good in R.
df <- readRDS(file.path(prediction_path, "KP", "predictions_all.rda")) %>%
  arrange(location_id)

#df_small <- slice(df, 1:1e3)

df0 <- df %>% group_by(location_id, variable) %>% summarize(n=n(), models = length(unique(model))) %>% ungroup()

#dups <- c(34496703, 34496704, 34496708)
dups <- filter(df0, n == max(n)) %>%
  distinct(location_id) %>% pull()

df1 <- filter(df, location_id %in% dups)


####
cohort0 <- read.csv("../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv")
cohort_dups <- cohort0 %>% group_by(location_id) %>% summarize(n = n()) %>% filter(n>1) %>% pull(location_id)

cohort <- filter(cohort0, location_id %in% dups) %>%
  arrange(location_id)



