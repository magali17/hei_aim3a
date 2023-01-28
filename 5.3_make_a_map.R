# this script generates presentation and manuscript-quality maps

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
pacman::p_load(tidyverse, ggpubr, 
               gstat, # idw()
               ggmap, sf, ggspatial #mapping...adding scales, N arrows
               )

set.seed(1)

#load your personal file paths here
source("file_paths.R") 
grid_path <- hei_aim3a_path
image_path <- joel_p01

if(!dir.exists(file.path(image_path))){dir.create(file.path(image_path))}
#########################################################################################################
# Load Data
#########################################################################################################
# mapping
crs_m <- 32148
crs_deg <- 4326 #WGS84. in decimal degrees
## PROJ.4 string for crs_deg
crs_deg_proj4 <- "+proj=longlat +datum=WGS84 +no_defs"

#shapefiles
monitoring_area_shp <- readRDS(file.path(grid_path, "GIS", "monitoring_area_shp.rda")) %>%
  #convert from 4269 
  st_transform(crs_deg)
monitoring_land_shp <- readRDS(file.path(grid_path, "GIS", "monitoring_land_shp.rda"))

# campaign crosswalk - full campaigns
cw <- readRDS(file.path(grid_path, "Selected Campaigns", "selected_campaigns2.rda")) %>%
  filter(design=="full") %>%
  select(variable, campaign_id)


#grid predictions
ns_11.5 <- read_csv(file.path(grid_path, "UK Predictions", "Grid", "predictions.csv"))
ns_15.4 <- read_csv(file.path(grid_path, "UK Predictions", "Grid", "predictions-2.csv"))
ns_64.9 <- read_csv(file.path(grid_path, "UK Predictions", "Grid", "predictions-3.csv"))

ns <- rbind(ns_11.5, ns_15.4, ns_64.9) %>%
  filter(campaign_id %in% cw$campaign_id) %>%
  select(location_id, latitude, longitude, in_monitoring_area, variable, prediction) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove=F, crs = crs_deg)
  
pollutants <- unique(ns$variable)
# only look at a few example bins
pollutants <- setdiff(pollutants, "ns_15.4")

#########################################################################################################
# Interpolate Grid
#########################################################################################################
grid_resolution <- 0.003
grid_idp <- 1.5 
grid_nmax <- 10
idw_file <- file.path(image_path, "interpolation", paste0("pred_idw_res", grid_resolution, "_idp", grid_idp, "_nmax", grid_nmax, ".rda"))
# create a finer interpolated grid with the desired attributes (above) if one does not already exist
if(!file.exists(idw_file)){
  
  # 1. create a finer grid
  finer_grid <- #st_as_sf(grid_predictions0, coords = c("longitude", "latitude"),  crs= crs_deg) %>%
    ns %>%
    # make a rectangular box w/ evenly spaced points at ~500 m resolution
    st_bbox() %>% st_as_sfc() %>%
    st_make_grid(cellsize = grid_resolution, what = "centers") %>% 
    # view in df format
    st_as_sf()  
  
  # 2. interpollate to the finer grid 
  idw_df0 <- lapply(pollutants, function(x) {
    
    idw_result = idw(formula = prediction~1,  
                     locations = filter(ns, variable ==x), 
                     newdata = finer_grid,  
                     #smaller inverse distnace powers produce more smoothing (i.e., give points furthewr away larger weights). default is the max, 2.0.
                     idp=grid_idp, 
                     # how many nearby points should be used to smooth
                     nmax=grid_nmax #maxdist=0.1
    ) %>%
      mutate(variable = x) 
  }) %>%
    bind_rows() %>% 
    cbind(., st_coordinates(.)) #%>% variable_relabel(keep_original_var = T)
  
  # # check that things worked
  # ggplot(data=idw_df0) + geom_raster(aes(x=X, y=Y, fill=var1.pred), interpolate = T) +
  #   geom_sf(data=monitoring_land_shp, inherit.aes = F, alpha=0)
  
  # 3. only keep predictions in monitoring region & not in water
  idw_df <- st_intersection(idw_df0, monitoring_land_shp)
  
  # save file
  saveRDS(idw_df, idw_file)

  # if file already exists, read it in
  } else {idw_df <- readRDS(idw_file)}

#########################################################################################################
# Make a Map!
#########################################################################################################
## Prep

# map labels
map_x_labels <- c(seq(-122.5, -121.9, 0.2)) #0.2
map_y_labels <- c(seq(47.2, 48, 0.2)) #%>% format(nsmall=1)

#need bbox w/ lat/long coordinates
bbox <- st_bbox(st_transform(st_buffer(st_transform(monitoring_area_shp, crs_m), 10000), crs_deg))
names(bbox) <- c("left", "bottom", "right", "top")
# basic map w/ airport symbol
zm_lvl <- 10
base_map <- suppressMessages(get_stamenmap( bbox = bbox, zoom = zm_lvl, maptype = "toner-lite"))
# layer map w/ roads & airport
roads <- suppressMessages(get_stamenmap(bbox = bbox,zoom = zm_lvl, maptype = "toner-hybrid"))
# ggmap(ggmap = roads)

#########################################################################################################
## Plot
p <- lapply(pollutants, function(x) {
  ggmap(ggmap = base_map, #darken = c(.5, "white")
        #darken = c(.2, "white")
        ) +
    # plot predictions as a raster
    geom_raster(data= filter(idw_df, variable==x), # data= idw_df,
                inherit.aes = F, aes(fill=var1.pred, x=X, y=Y), interpolate = T, alpha=0.93)  +
    # make ggmap (projects the data using the coord_map('mercator') projection)and geom_raster compatible. geom_tile() is an alternative but get strange outline
    coord_cartesian() +
    #inset_ggmap(ggmap = roads) +
    geom_sf(data=monitoring_land_shp, inherit.aes = F, alpha=0, size=0.1) +
    scale_fill_gradient(name = "Conc", low = "yellow", high = "red") +
    facet_wrap(~variable)  +
    # add scales & N arrow 
    annotation_scale(location = "tr") +
    annotation_scale(location = "tr", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
    annotation_north_arrow(location = "tr", pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
    theme_bw() +
    theme(
      legend.justification=c(0,1),  
      legend.position=c(0,1),  
      legend.background =  element_blank()) +
    coord_sf(expand = F) +
    scale_x_continuous(breaks = map_x_labels, labels = map_x_labels ) +
    scale_y_continuous(breaks = map_y_labels,
                       labels = format(map_y_labels,digits = 1, nsmall = 1)) +
    #add attribution/reference to bottom left
    geom_text(aes(x=-Inf, y=-Inf, hjust=-0.01, vjust=-0.3, 
                  label= "Map tiles by Stamen Design, under CC BY 3.0. \nData by OpenStreetMap, under ODbL."), size=2.5) +
    labs(x = "Longitude",
         y = "Latitude") 
  }) %>%
  ggarrange(plotlist = .)

p

ggsave(file.path(image_path, paste0("prediction_map_pnc_idp", grid_idp, ".png")),  height = 10, width = 11)

