
######################################################################################
# DATA
######################################################################################
knitr::opts_chunk$set(echo = F, 
                      cache=F, cache.comments = F, 
                      message = F, warning = F, 
                      tidy.opts=list(width.cutoff=60), tidy=TRUE)  

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse)    

image_path <- file.path("..", "Manuscript", "Images", "ca_nox")
if(!dir.exists(image_path)) {dir.create(image_path)}

set.seed(1)

# ggplot settings
theme_set(theme_bw())
theme_update(legend.position = "bottom")
######################################################################################
# DATA
######################################################################################
# --> ERROR
#cov <- readRDS(file.path("data", "ca_nox", "site_covariates.rda"))


# unique(sites$native_id)
# table(unique(sites$native_id) %in% unique(nox$native_id))

nox0 <- readRDS(file.path("data", "ca_nox", "nox_hourly.rda")) %>%
  filter(grepl("NOx", Parameter.Name)) %>%
  # right_join(geo_vars_2few[c("native_id", "County")]) %>% 
  # left_join(geo_vars_shp_few[c("native_id", "Land.Use", "Location.Setting")]) %>%
  group_by(native_id) %>%
  mutate(sd = sd(Sample.Measurement),
         weekday = ifelse(weekday==TRUE, "weekday", "weekend")
         ) %>%
  ungroup() %>%
  mutate(variability = ifelse(sd >= quantile(sd, 0.50), "High Variability", "Low Variability"),
  ) #%>% inner_join(sites)

sites0 <- read.csv(file.path("data", "ca_nox", "aqs_sites.csv"), stringsAsFactors = F) %>%
  filter(
    State.Name == "California",
    #City.Name== "Los Angeles"
    #County.Name == "Los Angeles"
  ) %>%
  
  mutate(native_id = paste0(6,
                            str_pad(string =  County.Code, width = 3, side = "left", pad = 0),
                            str_pad(string =  Site.Number, width = 4, side = "left", pad = 0))) 

nox <- inner_join(nox0, sites0)

sites <- filter(sites0, native_id %in% nox$native_id)

write.csv(sites, file.path("Output", "other", "ca_nox_sites.csv" ), row.names = F)

# few_sites <- filter(sites,
#                     Latitude <34.3 & Longitude< -118)

######################################################################################
# LOCATIONS
######################################################################################
#nox %>%
#few_sites %>% 
sites %>%
  filter(#Location.Setting %in% c("SUBURBAN", "URBAN AND CENTER CITY"),
         #City.Name == "Los Angeles"
         #Latitude <34.3 & Longitude< -118
         ) %>%
  
  ggplot(aes(x=Longitude, y=Latitude,  col=Location.Setting,
             #label=Site.Number,
             label = City.Name#Local.Site.Name #native_id
             )) + 
  geom_point() + 
  geom_label()


######################################################################################
# FIGURES
######################################################################################

# --> add BH lines 
# BH misses earlier time when the largest differences between a "background" and "urban" site might occur? 

nox %>%
  filter(City.Name %in% c("Los Angeles")) %>%
  ggplot(aes(x=hour, y= Sample.Measurement, col=Local.Site.Name,group=Local.Site.Name,
             linetype=Location.Setting)) +
  geom_smooth(#se=F, 
              size=0.5) +
  #guides(colour = FALSE) +
  facet_grid(season~weekday, switch = "both") +
  labs(x = "Hour",
       y = "Conc (ppb)",
       col="",
       title = "Sites in LA"
  )

ggsave(file.path(image_path, "hourly_sample_LA.png"), width = 8, height = 6)



nox %>%
  filter(County.Name %in% c("Los Angeles"),
    City.Name %in% c(#"Glendora", 
      "Azusa", #these are ~4 mi from eachother
      "Pomona", #glendora & pomona are ~7 mi apart
      "Pasadena"#,
      
      #"Santa Clarita"
    ),
    
    # Local.Site.Name %in% c("Los Angeles-North Main Street",
    #                        "Compton",
    #                        #"Pico Rivera #2",
    #                        "Reseda"
    #                        )
    
    
  ) %>%
  ggplot(aes(x=hour, y= Sample.Measurement, col=Local.Site.Name,group=Local.Site.Name,
             linetype=Location.Setting)) +
  geom_smooth(#se=F, 
    size=0.5) +
  #guides(colour = FALSE) +
  facet_grid(season~weekday, switch = "both") +
  labs(x = "Hour",
       y = "Conc (ppb)",
       col="",
       title = "Selected sites in LA County"
  )

ggsave(file.path(image_path, "hourly_sample2.png"), width = 8, height = 6)

