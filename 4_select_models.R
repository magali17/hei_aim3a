#script purpose: select models

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

pacman::p_load(tidyverse#,
               #parallel, #mclapply; detectCores()
               #sf,  
               #units # set_units()
)    

set.seed(1)

source("file_paths.R")
source("0_functions.R")


##################################################################################################
# LOAD DATA
##################################################################################################

model_eval0 <- readRDS(file.path(hei_aim1a_path, "model_eval.rda"))
#model_eval <- filter(model_eval0,)

