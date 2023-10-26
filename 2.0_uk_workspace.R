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
               kableExtra, 
               parallel, #mclapply; detectCores()
               pls, 
               gstat, #variogram()
               sf, #for spatial data; st_
               units
)    

set.seed(1)

source("file_paths.R")


dt_path <- file.path("Output", readRDS(file.path("Output", "latest_dt_version.rda")))

var_names <- readRDS(file.path(dt_path, "keep_vars.rda"))

# ## for future.apply::future_replicate()
# plan(multisession, workers = 6)
use_cores <- 5

image_path <- file.path("..", "Manuscript", "Images")


##################################################################################################
# LOAD DATA
##################################################################################################
# mapping variables
# save coordinate systems as variables
project_crs <- 4326  #lat/long
m_crs <- 32148
# Lambert Conic projection (meters)
lambert_proj <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

##################################################################################################
# 
training_set <- readRDS(file.path(hei_aim1a_path, "mm_cov_train_set_hei.rda")) 
modeling_covars <- readRDS(file.path(hei_aim1a_path, "mm_cov_test_set.rda")) %>%
  select(names(training_set)) %>%
  rbind(training_set)

saveRDS(modeling_covars, file.path("data", "stop_modeling_covars.rda"))

# mm annual estimates
annual <- readRDS(file.path(dt_path, "annual_training_set.rda")) %>%
  #add covariates
  left_join(modeling_covars) %>%
  #convert to sf
  st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
  st_transform(m_crs) %>%
  # log transform before modeling
  # some PNC annual avgs are 0, which causes issues logging values
  mutate_at(vars(all_of(var_names)), ~ifelse(.== 0, 1, .)) %>%
  mutate_at(vars(all_of(var_names)), ~log(.)) %>%
  gather("variable", "value", var_names) %>%
  relocate(variable, value, .before = longitude) %>%
  
  drop_na(value) %>% 
  # # 3 largest bins have mostly 0s, which turn to -Inf when you log-transform
  # filter(!value %in% c(Inf, -Inf)) %>%
  
  st_as_sf()
  

# ## same for test set
# annual_test_set <- readRDS(file.path("Output", "annual_test_set.rda")) %>%
#   #add covariates
#   left_join(readRDS(file.path(hei_aim1a_path, "mm_cov_test_set.rda"))) %>%
#   #convert to sf
#   st_as_sf(coords = c('longitude', 'latitude'), crs=project_crs, remove = F) %>%
#   st_transform(m_crs) %>%
#   # log transform annual averages before modeling
#   mutate(value = ifelse(value==0, 1, value),
#          value = log(value))  
#   
# 
# saveRDS(annual_test_set, file.path("Output", "annual_test_set2.rda"))


##################################################################################################
# COMMON VARIABLES
##################################################################################################

# for modeling 
cov_names <- st_drop_geometry(annual) %>% ungroup() %>%
  select(log_m_to_a1:last_col()) %>% names() # 188 covariates
saveRDS(cov_names, file.path(dt_path, "cov_names.rda"))
##################################################################################################
# TEST
cov_train <- readRDS(file.path(#"..", "..", 
  "~/OneDrive - UW/Documents/Post Doc/Study Projects/ACT TRAP MM",
  "ACT HEI Supp", "act_hei_aim1a", "Output", "mm_cov_train_set.rda")) %>%
  select(log_m_to_a1:last_col()) %>%
  names()

# cov_train[!cov_train %in% cov_names] # "em_CO_s03000" is missing here, but it's mostly 0s anyway
# may be due to more restrictive hei1a work - e.g., within routes coun't have low variability

##################################################################################################
pls_comp_n <- 2 #our campaign used 3
saveRDS(pls_comp_n, file.path(dt_path, "pls_comp_n.rda"))

#k-folds for CV
k <- 5  

##################################################################################################
# SETUP
##################################################################################################

# # everything look fairly normally distributed after log transformations  
# annual %>%
#   filter(grepl("full", design)) %>%
#   ggplot(aes(x=value)) +
#   facet_wrap(~variable, scales="free") +
#   geom_histogram() +
#   labs(title = "Distribution of site annual average concentrations")

##################################################################################################
# CREATE RANDOM VALIDATION FOLDS FOR EACH VARIABLE-DESIGN-VERSION
##################################################################################################

random_fold <- function(df, k.=k, object_is_st="yes") {
  #make sure temporal sims receive same fold designation
  set.seed(2)
  
  if(object_is_st=="yes") {df <- st_drop_geometry(df)  }
  
  result <- df %>% 
    distinct(location, spatial_temporal, design, version, campaign) %>%
    mutate(random_fold = sample(1:k.,size = nrow(.), replace = T ))
 
  return(result)
}

############################################################################################################
message("generating random folds")

random_fold_df <- lapply(group_split(annual, spatial_temporal, design, version, campaign),
                         function(x) random_fold(x, k.=k)) %>%
  bind_rows()

#join fold to annual 
annual <- suppressMessages(left_join(annual, random_fold_df)) %>%
  select(random_fold, everything())

saveRDS(annual, file.path(dt_path, "annual_training_set2.rda"))


############################################################################################################
# UK-PLS FUNCTION 
############################################################################################################
# fn returns UK-PLS predictions. inputs are two spatial objects (simple features). fn automatically transforms these to a lambert projection

# modeling_data = x
# new_data = stationary
# cov_names. = cov_names  #covariates to be used in modeling
# pls_comp_n. = pls_comp_n
# fn_result = "predictions"
# var_choice = ""

#TEST - SINCE POP10 IS MISSING FROM ANNIE'S ONROAD DATA
# cov_names. = cov_names[str_detect(cov_names, "pop10_", negate = T)]


#####
uk_pls <- function(modeling_data, # data for fitting pls-uk models
                   new_data, #prediction locations
                   cov_names. = cov_names,  #covariates to be used in modeling
                   pls_comp_n. = pls_comp_n, #pls components to use
                   fn_result = "predictions", #can be: "predictions" or "models"; return the model predictions or the model fit information
                   # optional: selects the best variogram fit, unless otherwise stated
                   var_choice = "" #Exp
) {
  
  set.seed(1)
  
  #lambert projection for UK model
  lambert_proj <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  ############################################################################################################
  # fit PLS model to estimate fewer components from geocovariates
  
  pls_model <- plsr(as.formula(paste('value ~', paste(cov_names., collapse = "+"))),
                    data = modeling_data, ncomp = pls_comp_n., scale=T, center=T)
  
  #extract compoent scores for UK
  modeling_data_scores <- predict(pls_model, type = "scores") %>% data.frame() %>%  
    # add location & value info
    cbind(data.frame(select(modeling_data, -all_of(cov_names.)))) %>%
    #convert back to sf. geom is dropped otherwise
    st_as_sf()
  
  new_data_scores <- predict(pls_model, type = "scores", newdata = new_data) %>% data.frame() %>%  
    # add location & value info
    cbind(data.frame(select(new_data, -all_of(cov_names.)))) %>%
    #convert back to sf. geom is dropped otherwise
    st_as_sf()
  
  ############################################################################################################
  # fit UK models & predict at new locations
  
  # UK formula 
  uk_formula <- as.formula(paste("value ~", paste0("Comp.", 1:pls_comp_n., collapse = "+")))
  
  # estimate the variogram model: fit a variogram model, offering to the function several different model options (exponential, spherical, and Matern):
  # using lambert coordinates b/c vertical/horizontal units represent the same jump
  # the default distance in gstat is 1/3 of the maximum distance (use cutoff option to change this)
  v_uk <- variogram(uk_formula, st_transform(modeling_data_scores, lambert_proj) )
  
  
  if(var_choice == "") {
    #select the best fitting variogram
    m_uk <- fit.variogram(v_uk, vgm(c("Exp", "Sph", "Mat")))
  } else {
    # or select a specific variogram if it's not left blank
    m_uk <- fit.variogram(v_uk, vgm(var_choice))
  }
  
  #make sure Exp/Sph range estimate is at least 0 when little/no correlation in the data 
  m_uk$range[2] <- max(m_uk$range[2], 1)
  
  # fit UK to the modeling data and predict at the new data locations
  uk_model <- krige(formula = uk_formula, st_transform(modeling_data_scores, lambert_proj), 
                    newdata = st_transform(new_data_scores, lambert_proj), 
                    model = m_uk)
  
  #save predictions
  predictions <- select(new_data, -all_of(cov_names.)) %>%
    mutate(prediction = uk_model$var1.pred)
  
  #return(result)
  # return the desired output: either the predictions or the modeling specifications
  if(fn_result == "predictions") {return(predictions)}
  if(fn_result == "models") {
    result = list(
      variable = first(modeling_data$variable),
      pls_model = pls_model, 
      variogram_model = m_uk
    )
    return(result)
  }
  
}

saveRDS(uk_pls, file.path(dt_path, "UK Predictions", "uk_pls_model.rda"))

##################################################################################################
# CV function
##################################################################################################
# function returns cross-valited predictions for a given dataset
# x = group_split(annual, spatial_temporal, design, version, campaign, variable)[[24]]
# fold_name = "random_fold"
do_cv <- function (x, fold_name) {
  
  #code to make sure this fn works even if folds don't have all numbers in a sequence (e.g., if too few sites)
  k = sort(unique(x[[fold_name]]))
  
  df <- data.frame()
  
  for(f in k) {
    modeling_data0 = filter(x, !!as.symbol(fold_name) != f)
    new_data0 = filter(x, !!as.symbol(fold_name) == f)
    
    temp <- uk_pls(modeling_data = modeling_data0, new_data = new_data0) %>% st_drop_geometry() 
    df <- rbind(df, temp)
  }
  
  return(df)
}



##################################################################################################
# COMMON VARIABLES
##################################################################################################

common_vars <- c("location", "route", "visits", "campaign", "design", "version", "spatial_temporal", "variable", "prediction")


##################################################################################################
# SAVE WORKSPACE
##################################################################################################

save.image(file.path(dt_path, "uk_workspace.rdata"))

message("done with 2.0_uk_workspace.R")
