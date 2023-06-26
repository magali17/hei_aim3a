
#################################################################################################################
# SETUP
#################################################################################################################
knitr::opts_chunk$set(echo = F, 
                      cache=F, cache.comments = F, 
                      message = F, warning = F, 
                      tidy.opts=list(width.cutoff=60), tidy=TRUE#, fig.height = 10, fig.width = 10
)  

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

#library(pacman)
pacman::p_load(tidyverse, 
               # PRISMAstatement,table1,
               # knitr, kableExtra,
               ggpubr, #ggarrange()
               ggmap, sf, #mapping
               ggspatial, #mapping, adding scales, N arrows...
               spData, # us_states data - mapping WA state
               cowplot #layering ggplots. ggdraw(), draw_plot()
)    

#dt_path <- file.path("Output", "v1_20230131")

set.seed(1)

#source("functions.R")
#output_data_path <- file.path(dt_path, "epi")

image_path <- file.path("..", "Manuscript", "Images", "v3")

#if(!dir.exists(file.path(image_path, "other", "presentation"))) {dir.create(file.path(image_path, "other", "presentation"), recursive = T)}
if(!dir.exists(file.path(image_path, "other", "poster"))) {dir.create(file.path(image_path, "other", "poster"), recursive = T)}
if(!dir.exists(file.path(image_path, "SI"))) {dir.create(file.path(image_path, "SI"), recursive = T)}

#for general maps
general_map_fp <- file.path("maps")
if(!dir.exists(general_map_fp)) {dir.create(general_map_fp, recursive = T)}

# ggplot settings
theme_set(theme_bw())
theme_update(legend.position = "bottom")

fig_w <- 3.5
fig_l <-6
#################################################################################################################
# DATA
#################################################################################################################
project_crs <- 4326  #lat/long
m_crs <- 32148 #meters

monitoring_area <- readRDS(file.path("data", "GIS", "monitoring_area_shp.rda"))

stops <- readRDS(file.path("data", "location_lat_long.rda")) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs= project_crs, remove=F)


#routes
routes <- read_sf(file.path("data", "GIS", "Routes 190910", "All routes.shp"))  %>%
  mutate(Route = as.factor(Route))

# locations of major trap sources
markers <- readRDS(file.path("data", "GIS", "markers.rda")) %>%
  mutate(point = ifelse(point==3, 12, point)) %>%
  
  # ===> FIX UW lat/long
  add_row(x=122.3, y=47.65, marker = "UW", point=2)

#AQS sites
aqs <- read_sf(file.path("data", "GIS", "AQS Sites", "PSCAA_stations.shp"))



#jittered cohort locations
cohort <- readRDS(file.path("data", "GIS", "jittered_cohort.rda")) %>%
  filter(in_monitoring_area)
  

annual_avg <- read.csv(file.path("data", "tr0090_annual_averages.csv")) %>%
  filter(annual == "mean_of_win_medians",
         variable =="total.conc")



# UW colors
uw_purple <- "#4b2e83"
uw_gold1 <- "#b7a57a" # web: e8e3d3
uw_gold2 <- "#85754d"

#################################################################################################################
# MAP
#################################################################################################################
# BBOX
##make box little bigger than monitoring area
bbox <- st_bbox(st_transform(st_buffer(st_transform(monitoring_area, m_crs), 7e3), project_crs))
#bbox <- st_bbox(monitoring_area)

names(bbox) <- c("left", "bottom", "right", "top")

# --> TEMP
#map0 <- ggplot()

#detach(package:ggmap, unload=TRUE)
#detach(package:ggplot2, unload=TRUE)

# --> need this to make sure annotate is from ggmpa, not ggplot2?
conflicted::conflict_prefer("annotate", "ggmap")

# background map
map0 <- suppressMessages(get_stamenmap(bbox = bbox, 
                                       zoom = 11,
                                       #maptype = "terrain"
                                       maptype = "toner-lite" #has airport symbol
                                       )
                         ) %>%
  # Make basic map image from the tiles
  ggmap(ggmap = .) + theme_void()

## usage example: 
# map0 

#################################################################################################################
# MAIN MAP
map1 <- map0 + 
  # #monitoring area
  geom_sf(data = monitoring_area, aes(), fill = "#e8e3d3",
          inherit.aes = F, lwd = 0.1, alpha = 0.05) +
  
  #monitoring stops
  geom_sf(data = stops, inherit.aes = F, size=1.5, col = uw_purple, 
          #aes(col="Monitoring\nLocation")
          ) + 
    
  # #major TRAP sources other than roads
  # geom_point(data = markers, inherit.aes = F, aes(x=x, y=y, shape=marker), size=2) +
  
  # add scale & N arrow to top
  geom_sf(data = monitoring_area, inherit.aes = FALSE, alpha=0, lwd = 0) +
  annotation_scale(data = monitoring_area, location = "tl") +
  annotation_scale(data = monitoring_area, location = "tl", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
  annotation_north_arrow(location = "tl",
                         #point towards North Pole
                         which_north = "true",
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering
  ) +
  theme_bw() +
  #theme_void() +
   theme(axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank()#,
         # axis.title.y=element_blank(),
         # axis.text.y=element_blank(),
         # axis.ticks.y=element_blank()
     
     
  #   legend.justification=c(1,1),
  #   legend.position=c(1,1.02), # 1,1
  #   legend.key = element_rect(colour = NA, fill = NA),
  #   #legend.background =  element_blank()
  #   legend.background=element_rect(fill = alpha("white", 0))
   ) + 
  
  scale_shape_manual(values = c(2,1, markers$point)) +
  
  # #order how legend items are presented
  # guides(col = guide_legend(order = 2),
  #        shape = guide_legend(order = 3), 
  #        fill = guide_legend(order = 1)) +
  #add attribution/reference to bottom left
  geom_text(aes(x=-Inf, y=-Inf, hjust=-0.01, vjust=-0.5,
                label= "Map tiles by Stamen Design, under CC BY 3.0. \nData by OpenStreetMap, under ODbL."), size=3) +
  labs( 
    x = "Longitude",
    y = "Latitude",
    #fill = "",
    col = "Monitoring\nLocation",
    shape = "",  
    caption = ""
  ) 

map1

#################################################################################################################
# INSET MAP

# example code: https://geocompr.github.io/post/2019/ggplot2-inset-maps/ 

data("us_states", package = "spData")

wa_map <- us_states %>%
  filter(NAME == "Washington") %>%
  #make sure this is in the same crs 
  st_transform(project_crs)

wa_centroid <- st_coordinates(st_centroid(wa_map))

inset_map <- ggplot() + 
  geom_sf(data=wa_map, fill = uw_gold2,
          alpha= 0.5
          ) + 
  #geom_sf(data=bbox0, fill = "white", col="blue", alpha=0.5) + 
  geom_sf(data = monitoring_area, #aes(),
          fill = uw_purple,
          # UW gold
          #fill = "#b7a57a", 
          #reduce/eliminate outline
          lwd = 0.1,  
          #alpha = 0.7,
  ) +
  theme_void() + 
  #add "WA" label
  geom_text(aes(x = wa_centroid[1], y = wa_centroid[2]),
            label = "WA", 
            size=4
  )

# inset_map

# use cowplot to layer both maps
ggdraw() +
  draw_plot(map1) +
  draw_plot(inset_map, 
            # The distance along a (0,1) x- or y- axis to draw the left/bottom edge of the plot
            x = 0.65, y = 0.002, 
            #x = 0.25, y = 0.005, 
            # The width and height of the plot expressed as proportion of the entire ggdraw object
            width = 0.23, height = 0.23)


ggsave(file.path(image_path, "stops_map.jpg"), width = fig_w, height = fig_l #width = 4.5, height = 8
       )


#################################################################################################################
# PRESENTATION MAPS
#################################################################################################################
################## monitoring area ################## 
map_a <- map0 + 
  #monitoring area
  geom_sf(data = monitoring_area, aes(), fill = "#e8e3d3",
          inherit.aes = F, lwd = 0.1, alpha = 0.05) +
  
  # #monitoring stops
  # geom_sf(data = stops, inherit.aes = F, size=1.5, col = uw_purple, 
  #         #aes(col="Monitoring\nLocation")
  # ) + 
  
  # #major TRAP sources other than roads
  # geom_point(data = markers, inherit.aes = F, aes(x=x, y=y, shape=marker), size=2) +
  
  # # add scale & N arrow to top
  # geom_sf(data = monitoring_area, inherit.aes = FALSE, alpha=0, lwd = 0) +
  # annotation_scale(data = monitoring_area, location = "tl") +
  # annotation_scale(data = monitoring_area, location = "tl", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
  # annotation_north_arrow(location = "tl",
  #                        #point towards North Pole
  #                        which_north = "true",
  #                        pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +

# add scales & N arrow 
  annotation_scale(location = "tr") +
  annotation_scale(location = "tr", unit_category ="imperial", pad_y = unit(0.55, "cm")) +
  
  annotation_north_arrow(location = "tr", pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +  

  theme_bw() +
  #theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  
  scale_shape_manual(values = c(2,1, markers$point)) +
  
  # #order how legend items are presented
  # guides(col = guide_legend(order = 2),
  #        shape = guide_legend(order = 3), 
  #        fill = guide_legend(order = 1)) +
  #add attribution/reference to bottom left
  geom_text(aes(x=-Inf, y=-Inf, hjust=-0.01, vjust=-0.5,
                label= "Map tiles by Stamen Design, under CC BY 3.0. \nData by OpenStreetMap, under ODbL."), size=2) +
  labs( 
    x = "Longitude",
    y = "Latitude",
    #fill = "",
    col = "Monitoring\nLocation",
    shape = "",  
    caption = ""
  ) 


################## add cohort locations ################## 
map_b <- map_a + 
  geom_point(data = cohort, aes(x=longitude_deg, y=latitude_deg), col = uw_gold1, size=0.5, alpha=0.05) #+ 
  # --> doesn't work??  
  #geom_label(data=markers, aes(x=x, y=y, label=marker), inherit.aes = F)
  
map_b
ggsave(file.path(general_map_fp, "map1_jittered_cohort.jpg"), width = fig_w, height = fig_l
       )

################## add stops ################## 
map_c <- map_b + 
  #monitoring stops
  geom_sf(data = stops, inherit.aes = F, col = uw_purple) 

map_c #+
  #geom_label(data=markers, aes(x=x, y=y, label=marker), inherit.aes = F)
  #geom_sf(data = st_as_sf(markers, coords=c("x", "y")) )

ggsave(file.path(general_map_fp, "map2_cohort_and_stops.jpg"), width = fig_w, height = fig_l)

################## add routes ################## 
map_d <- map_c +
geom_sf(data = routes, inherit.aes = F, alpha=0.5, col = uw_purple)

map_d
ggsave(file.path(general_map_fp, "map3_cohort_routes.jpg"), width = fig_w, height = fig_l)


################## AQS comparison ################## 
map_e <- map_d +
  geom_sf(data = aqs, inherit.aes = F, size=4)
   
  #geom_label(data = aqs, aes(x=longitude, y=latitude, label=descriptio), nudge_x = .04)

map_e
ggsave(file.path(general_map_fp, "map4_aqs_sites.jpg"), width = fig_w, height = fig_l)

################## Annual Averages ################## 

map_a + 
  geom_point(data=annual_avg, aes(x=longitude, y=latitude, col=value)) + 
  scale_color_gradient(name = "Conc", low = "yellow", high = "red") + 
  theme(
    legend.justification=c(0,1),  
    legend.position=c(0,1),  
    legend.background =  element_blank()
  ) 

ggsave(file.path(general_map_fp, "map5_annual_avg_est.jpg"), width = fig_w, height = fig_l)

