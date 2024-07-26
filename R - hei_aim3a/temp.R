# temporary script 

#############################################################################################################
pacman::p_load(tidyverse, parallel, broom)    
use_cores <- 1#4

get_model_results_all_coefs <- function(dt) {
  
  mclapply(dt, mc.cores=use_cores, function(x){
    tidy(x, conf.int = T) %>%
      mutate(model = x$model,
             n=nobs(x)) %>%
      rename(lower = conf.low, upper=conf.high, est=estimate, se=std.error)
  }) %>%
    bind_rows() %>%
    mutate(significant = ifelse((lower <0 & upper <0) |
                                  (lower >0 & upper >0), TRUE, FALSE))
}

message("reading in 'models_road_extended.rda'")
models_r_extended <- readRDS(file.path("Output", "v3_20230321", "epi", "20240725", "models_road_extended.rda"))

message("getting & saving all coefs")
models_r_extended_all <- get_model_results_all_coefs(models_r_extended)
saveRDS(models_r_extended_all, file.path("Output", "v3_20230321", "epi", "20240725", "models_r_extended_all.rda"))  


#############################################################################################################