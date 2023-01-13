# Rscript 5.2_summarize_predictions.R Output/'UK Predictions'/cohort/ns_total_conc

################################################################################
# SETUP
################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load the required libraries for: plotting, modeling, spatial features, script timing
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, ggpubr, pls, gstat, sf, ggspatial, tictoc, tools,parallel)

###########################################################################################
# TAKE IN USER ARGUMENTS 
###########################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)
# user_arguments <-c("Output/UK Predictions/grid/ns_27.4")

if (length(user_arguments) !=1) {
  print("Usage error. Enter: 1. the location of the predictions. Usage:")
  print("rscript 5.2_summarize_predictions.R <prediction_file_path>")
  stop()
}

# new covariate file
prediction_directory <- user_arguments[1]
print(paste("running", prediction_directory))

new_predictions <- read_csv(file.path(prediction_directory, "predictions.csv"), show_col_types = F)

monitoring_area <- readRDS(file.path("Output", "GIS", "monitoring_land_zero_water_shp.rda"))  

###########################################################################################
# GENERATE SUMMARY FIGURES AND MAPS OF THE NEW PREDICTIONS
###########################################################################################
print("Generating prediction summary figures and maps...")

# 1. prediction histograms 
ggplot(data=new_predictions, aes(x=prediction)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_histogram(bins=30) +
  labs(title = "Prediction Histograms")

ggsave(file.path(prediction_directory, "prediction_histograms.png"), height = 10, width = 8)

# 2. prediction maps at all of the locations
p <- list()

for (i in unique(new_predictions$variable)) {
  
  df <- filter(new_predictions, variable == i)
  
  p[[i]] <- ggplot() +
    geom_sf(data=monitoring_area)  +
    geom_point(data= df, aes(x=longitude, y=latitude, col=prediction), alpha=0.6) + 
    facet_wrap(~variable) +
    scale_colour_gradient(name = "Conc", low = "yellow", high = "red") +
    # add scales & N arrow
    annotation_scale(location = "tr", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
    annotation_north_arrow(location = "tr", pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
    theme_bw() +
    coord_sf(expand = F) 
}

ggarrange(plotlist = p) %>% 
  annotate_figure(top = "UK-PLS predictions at all locations")

ggsave(file.path(prediction_directory, "all_predictions.png"), height = 16, width = 16)

# 3. prediction maps only at locations in monitoring area
p <- list()

for (i in unique(new_predictions$variable)) {
  
  df <- filter(new_predictions,
               in_monitoring_area == TRUE,
               variable == i)
  
  p[[i]] <- ggplot() +
    geom_sf(data=monitoring_area)  +
    geom_point(data= df, aes(x=longitude, y=latitude, col=prediction), alpha=0.6) + 
    facet_wrap(~variable) + 
    scale_colour_gradient(name = "Conc", low = "yellow", high = "red") +
    # add scales & N arrow 
    annotation_scale(location = "tr", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
    annotation_north_arrow(location = "tr", pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
    theme_bw() +
    coord_sf(expand = F)
}

ggarrange(plotlist = p) %>% 
  annotate_figure(top = "UK-PLS predictions at locations in monitoring area")

ggsave(file.path(prediction_directory, "monitoring_predictions.png"), height = 16, width = 16)

print("Figures and maps saved.")
print("DONE")

