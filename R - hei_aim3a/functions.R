#######################################################################################################################
# add poster ggplot theme - larger text
#######################################################################################################################
# # --> error - doesn't work
# poster_theme2 <- function(plot, axis=16, title=20, strip=16, legend=20#,...
#                           ) {
#   plot +
#     theme(axis.text=element_text(size=axis),
#           axis.title=element_text(size=title,face="bold"),
#           strip.text = element_text(size = strip),
#           legend.text = element_text(size=legend),
#           #...
#           )
#   }

poster_theme <- theme(axis.text.x=element_text(size=22),
                      axis.text.y=element_text(size=26),
                      axis.title=element_text(size=32,face="bold"),
                      strip.text = element_text(size = 28),
                      legend.text = element_text(size=32),
                      
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank()
                      )
  
  
label_size <- 8  

#######################################################################################################################
# UW colors
#######################################################################################################################
uw_spirit_purple <- "#4b2e83"
uw_husky_purple <-"#32006e"
uw_husky_gold <- "#b7a57a" # web: e8e3d3
uw_heritage_gold <- "#85754d"
uw_spirit_gold <- "#ffc700"
uw_accent_green <- "#aadb1e"
uw_accent_lavender <- "#c5b4e3"
uw_accent_pink <- "#e93cac"
uw_accent_teal <-"#2ad2c9"


#######################################################################################################################
# winsorize
#######################################################################################################################
# variable = road_dt$ufp
# min_value = quantile(road_dt$ufp, 0.05, na.rm = T) 
# max_value = quantile(road_dt$ufp, 0.95, na.rm = T) 

# returns a new variable, 'win_value', which winsorizes an original variable, 'value', based on a quantile from the original variable
winsorize <- function(variable, minval, maxval) {

  ifelse(variable > maxval, maxval,
         ifelse(variable < minval, minval, variable)) %>%
    unlist()  
}

#######################################################################################################################
# label designs
#######################################################################################################################
label_designs <- function(dt) {
  
  
  site_type_levels <- readRDS(file.path(dt_path, "site_type_levels.rda")) %>% rev()
  site_type_levels_alt <- gsub("H ", "H", site_type_levels) %>% gsub("L ", "L", .)
  
  dt <- dt %>% mutate(
    design = ifelse(design == "full", "All Data", 
                    ifelse(design == "balanced seasons", "Fewer Seasons",
                           ifelse(design == "fewer total stops", "Fewer Visits", 
                                  ifelse(design == "site type", "Unbalanced Visits", design)))),
    design = str_to_title(design),
    design = factor(design, levels = c("All Data",
                                       "Fewer Total Stops",
                                       "Fewer Visits",
                                       "Balanced Seasons",
                                       "Fewer Seasons",
                                       "Fewer Hours",
                                       "Unbalanced Visits", "Site Type"
                                       )),
    
    version = ifelse(version == "all training data", "all data", version),
    # shorten name
    version = gsub("temp ", "", version),
    version = gsub("business", "Bus", version),
    version = gsub("H ", "H", version),
    version = gsub("L ", "L", version),
    version = str_to_title(version),
    version = ifelse(version=="12_visits 309_sites", "12", #"3,700",
                     ifelse(version=="6_visits 309_sites", "6", #"1,850",
                            ifelse(version=="4_visits 309_sites", "4", #"1,200",
                                   version))),
    version = factor(version, levels = c("All Data", 1:3, "4", "6", "12", 
                                         "Business", "Business Adj", "Business Adj 1", "Business Adj 2", "Bus", "Bus Adj", "Bus Adj 1", "Bus Adj 2",
                                         "Business\nTemporally\nAdjusted",
                                         
                                         "Rush","Rush Adj", "Rush Adj 1", "Rush Adj 2",
                                         "Rush\nTemporally\nAdjusted",
                                         site_type_levels, site_type_levels_alt
    ))) 
  
  return(dt)
}


#######################################################################################################################
# label NS bins and NO2
#######################################################################################################################
# dt = campaign_descriptions

label_pollutants <- function(dt, label = "ufp_midpoint") {
  
  if(label == "ufp_midpoint") {
    dt <- dt %>% mutate(
      instrument = ifelse(grepl("pnc_", variable), "P-TRAK",
                          ifelse(grepl("ns_", variable), "NanoScan", NA)),
      instrument = factor(instrument, levels = c("NanoScan", "P-TRAK")),
    
      variable = case_when(
        variable=="ns_total_conc" ~ "Total",
        variable=="ns_10_100" ~ "10-100 nm",  
        
        variable=="pnc_noscreen" ~ "20-1,000 nm",
        
        variable=="ns_11.5" ~ "12 nm",  
        variable=="ns_15.4" ~ "15 nm",  
        variable=="ns_20.5" ~ "21 nm",
        variable=="ns_27.4" ~ "27 nm",
        variable=="ns_36.5" ~ "37 nm",
        variable=="ns_48.7" ~ "49 nm",
        variable=="ns_64.9" ~ "65 nm",
        variable=="ns_86.6" ~ "87 nm",
        variable=="ns_115.5" ~ "116 nm",
        variable=="ns_154.0" ~ "154 nm",
        variable=="ns_205.4" ~ "205 nm",
        
        variable=="no2" ~ "NO2 (ppb)"),
      variable = factor(variable, 
                        levels = c("Total",
                                   "10-420 nm",  
                                   "10-100 nm",  
                                   "20-1,000 nm",
                                   
                                   "NO2 (ppb)",
                                   
                                   "12 nm",  
                                   "15 nm",  
                                   "21 nm",
                                   "27 nm",
                                   "37 nm",
                                   "49 nm",
                                   "65 nm",
                                   "87 nm",
                                   "116 nm",
                                   "154 nm",
                                   "205 nm"))
    )}
  
  if(label == "ufp_range") {
    dt <- dt %>% 
      mutate(
        instrument = ifelse(grepl("pnc_", variable), "P-TRAK",
                            ifelse(grepl("ns_", variable), "NanoScan", NA)),
        instrument = factor(instrument, levels = c("NanoScan", "P-TRAK")),
        
      variable = case_when(
        variable=="ns_total_conc" ~ "10-420 nm",
        variable=="ns_10_100" ~ "10-100 nm",  
        variable=="ns_11.5" ~ "10-13 nm",
        
        variable=="pnc_noscreen" ~ "20-1,000 nm",
        
        variable=="ns_15.4" ~ "13-18 nm",
        variable=="ns_20.5" ~ "18-24 nm",
        variable=="ns_27.4" ~ "24-32 nm",
        variable=="ns_36.5" ~ "32-42 nm",
        variable=="ns_48.7" ~ "42-56 nm",
        variable=="ns_64.9" ~ "56-75 nm",
        variable=="ns_86.6" ~ "75-100 nm",
        variable=="ns_115.5" ~ "100-133 nm",
        variable=="ns_154.0" ~ "133-178 nm",
        variable=="ns_205.4" ~ "178-237 nm",
        
        variable=="no2" ~ "NO2 (ppb)"),
      
      variable = factor(variable, 
                        levels = c("Total",
                                  "10-420 nm",
                                  
                                  "10-100 nm",
                                  "20-1,000 nm",
                                  
                                  "NO2 (ppb)",
                                  
                                  "10-13 nm",
                                  "13-18 nm",
                                  "18-24 nm",
                                  "24-32 nm",
                                  "32-42 nm",
                                  "42-56 nm",
                                  "56-75 nm",
                                  "75-100 nm",
                                  "100-133 nm",
                                  "133-178 nm",
                                  "178-237 nm"))
      )}

  return(dt)
}

######################################################################################################################
# returns a new variable, 'win_value', which winsorizes an original variable, 'value', based on a quantile from the original variable
######################################################################################################################
winsorize_fn <- function(dt, value, trim_quantile =0.05) {
  
  dt1 <- dt %>%
    group_by(variable, location) %>%
    mutate(
      win_value = ifelse(!!as.symbol(value) > quantile(!!as.symbol(value), 1-trim_quantile), quantile(!!as.symbol(value), 1-trim_quantile), 
                         ifelse(!!as.symbol(value) < quantile(!!as.symbol(value), trim_quantile), quantile(!!as.symbol(value), trim_quantile),
                                !!as.symbol(value)))
    )
  
}




###########################################################################################
# GENERATE NEW COVARIATES FOR NEW DATASETS
###########################################################################################
# created some new proximity variables  
# log transform land proximity variables (e.g., distance to roadways)

combine_a23_ll <- function(df) {
  #find buffers for a2-3 length variables
  buffers <- str_subset(names(df), "ll_a[2:3]") %>% str_extract("s[0:9].*")
  
  #for each buffer, calculate sum of a2+a3 length
  for (i in seq_along(buffers)) {
    old_vars <- paste0(c("ll_a2_", "ll_a3_"), buffers[i])
    new_var <- paste0("ll_a23_", buffers[i])
    
    df[new_var] <- apply(df[old_vars], 1, sum)
  }
  return(df)
}

generate_new_vars <- function(df) {
  # for the NO2 covariate, use the average levels from several available years
  no2_behr_vars <- c("no2_behr_2005","no2_behr_2006", "no2_behr_2007")
  
  df <- df %>%
    rowwise() %>%
    mutate(m_to_a123 = min(m_to_a1, m_to_a2, m_to_a3),
           m_to_a23 = min(m_to_a2, m_to_a3),
           no2_behr = mean(!!as.symbol(no2_behr_vars))
    ) %>%
    ungroup() %>%
    #make min distance 1 m before log transforming
    mutate_at(vars(starts_with("m_to_")), ~ifelse(.==0, 1, .) %>% log(.)) %>%
    rename_at(vars(starts_with("m_to_")), ~gsub("m_to_", "log_m_to_", .)) %>%
    # calculate sum of a2 and a3 roads in each buffer
    combine_a23_ll()
}

#######################################################################################################################
# functions to prep for PLS  
#######################################################################################################################

## e.g., drop covariates that don't vary enough, etc.

log_transform_distance <- function(all.data, lowerBound=10, removeOrig=FALSE)
{
  distance.vars <- grep("^m_to_", colnames(all.data))
  new.varnames <- c()
  for (i in distance.vars)
  {
    newcol.index <- 1 + ncol(all.data)
    all.data[, newcol.index] <- log( sapply( all.data[, i], function(x) { max(lowerBound, x) } ) )
    colnames(all.data)[newcol.index] <- paste('log_', colnames(all.data)[i], sep='')
  }
  if (removeOrig) all.data <- all.data[, -distance.vars]
  return (all.data)
}


fail_most_common_value <- function(mon.data, vars.all, thres=0.2)
{
  thres <- dim(mon.data[,vars.all])[1]*thres
  
  fail <- apply( mon.data[,vars.all], 2, function(x) {
    tmp <- split(x,x)
    most.common.value <- tmp[[which.max(sapply(tmp, length))]][1]
    sum(x != most.common.value, na.rm=T) } ) < thres
  fail <- names(mon.data[,vars.all])[fail]
  return(fail)
}


fail_low_landuse <- function(mon.data, vars.all, lowValue=10) 
{
  lu.vars <- grep("^rlu_|^lu", vars.all, value=T)
  fail <- sapply(lu.vars, function(x) return (max(mon.data[, x]) < lowValue))
  fail <- names(fail)[grep("TRUE", fail)]
  return (fail)
}



#######################################################################################################################
# fn returns CV PLS predictions for EACH Campaign  
#######################################################################################################################

n_comp <- 2

cv_pls_p <- function(dt,
                     y_val,
                     x_predictors,
                     #CV folds
                     k,
                     #k = 10,
                     # PLS component no.
                     pls_comp = n_comp,
                     label = "" 
) {  
  
  
  dt <- dt %>% rename(y_val = y_val) %>%
    drop_na() %>%
    # place to save predictions
    mutate(cv_p = NA) %>%
    as.data.frame()
  
  # place to save cv predictions
  dt2 <- data.frame()
    #want the same test set sites across diff designs for any given campaign (note: test sets do change across campaigns, but in the same way across designs. could move this seed into the next for() loop to use same training/test sets every single time)
    set.seed(1)  
    
    # loop through each campaign (e.g., n=30)
    for (i in seq_len(max(dt$Campaign))) {
      #i=1
      
      #1 Campaign at a time
      dt_temp <- dt %>% #dt_temp_pollutant
        filter(Campaign == i)
      
      #create folds for test/train set
      set = sample(c(1:k), size = nrow(dt_temp), replace = T)
      
      # loop through each fold (e.g., k=10)
      for(f in seq_len(k)) {
        #f=1
        
        train_grp <- set != f
        
        dt_temp_train <- dt_temp %>% filter(train_grp)  
        dt_temp_test <- dt_temp %>% filter(!train_grp)   
        
        #fit PLS to training data
        pls_train <- plsr(y_val ~.,
                          data=dt_temp_train[,c("y_val", x_predictors)], 
                          ncomp = pls_comp,
                          scale=T)
        
        #save CV predictions for each test set
        dt_temp$cv_p[!train_grp] <- predict(object = pls_train,
                                            newdata = dt_temp_test
                                            ) %>%
          as.data.frame() %>%
          #make into a vector
          pull()
      }
      
      dt2 <- rbind(dt2, dt_temp) 
      
    }
    
  #}
  
  #change back to original name
  names(dt2)[names(dt2) == "y_val"] <- y_val
  
  #option to relabel prediction variable
  if(label != "") {
    names(dt2)[names(dt2) == "cv_p"] <- label
    }
  
  return(dt2)
  
}



#######################################################################################################################
# fn returns design-sampling-type-verson names (data in long format) as separate columns 
#######################################################################################################################
#str_extract(cv_p_names, "[a-z]+_hr")

separate_design_type_v <- function(dt, 
                                   var = "Design" #,  designs
) {
  
  dt <- dt %>%
    rename(var = var)
  
  result <- dt %>%
    mutate(
      Samples_per_site = factor(gsub("n", "", str_extract(var, "n[0-9]+"))),
      Version = factor(gsub("v", "", str_extract(var, "v[0-9]"))),
      Season_duration = factor(str_extract(var, "[0-9]wk")),
      Type = ifelse(grepl("true", var), "Long-Term", "Short-Term"),
      Hours = factor(str_to_title(gsub("_hr", "", str_extract(var, "[a-z]+_hr")))),
       
       
    )
  
  #change ver name back
  names(result) [grepl("var", names(result))] <- var
  
  return(result)
}



#######################################################################################################################
# facet_wrap_equal() and facet_grid_equal() functions act like facet_wrap() and facet_grid() in ggplot but it sets the axes ranges (min/max) of each facet to same scale so that the 1-1 line is always down the middle :D !!
#######################################################################################################################
# code source: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/ 

FacetEqualWrap <- ggproto(
  "FacetEqualWrap", FacetWrap,
  
  train_scales = function(self, x_scales, y_scales, layout, data, params) {
    
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetWrap, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X <- layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y <- layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)

facet_wrap_equal <- function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  ggproto(NULL, FacetEqualWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}


#same as above but for facet_grid()
FacetEqualGrid <- ggproto(
  "FacetEqualGrid", FacetGrid,
  
  train_scales = function(self, x_scales, y_scales, layout, data, params) {
    
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetGrid, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X <- layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y <- layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)

facet_grid_equal <- function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_grid(...)
  
  ggproto(NULL, FacetEqualGrid,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}
 


#######################################################################################################################
# fns calculate basic model performance statistics
#######################################################################################################################


#returns mse_fn
mse_fn <- function(obs, pred){
  mean((obs - pred)^2)
}

rmse_fn <- function(obs, pred){
  sqrt(mean((obs - pred)^2))
}

#returns mse_fn-based R2
# for simplicity, use this in future: caret::R2(pred = pred, obs = obs, form='traditional')
r2_mse_based <- function(obs, pred) {
  mse_fn.est <- mse_fn(obs, pred)
  r2 <- 1- mse_fn.est/mean((obs - mean(obs))^2)
  max(0, r2)
}  




#######################################################################################################################
# fn adds sesason to a given dataset with a date variable. Uses typical equinox/solstice dates
#######################################################################################################################


add_season <- function(dt, .date_var) {
  
  pacman::p_load(lubridate)
  
  # dt <- aqs_daily
  # .date_var <- "Date.Local"
  
  winter <- "-12-21" #usually winter starts on 21st, sometimes on 22nd 
  spring <- "-03-20"
  summer <- "-06-21" #usually summer starts on 21st, sometimes on 22nd 
  fall <- "-09-23" #usually fall starts on 22nd, sometimes on 23nd. Using 23rd for 2019 mobile monitoring campaign 
  
  dt <- dt %>%
    rename(date_var = .date_var) %>%
    #make sure variable is in date format
    mutate(date_var = as.Date(date_var),
           season = factor(ifelse((date_var >= ymd(paste0((year(date_var)-1), winter)) & date_var < ymd(paste0(year(date_var), spring))) |
                                    date_var >= ymd(paste0(year(date_var), winter)), "winter",
                                  ifelse(date_var >= ymd(paste0(year(date_var), spring)) &
                                           date_var < ymd(paste0(year(date_var), summer)), "spring",
                                         ifelse(date_var >= ymd(paste0(year(date_var), summer)) &
                                                  date_var < ymd(paste0(year(date_var), fall)), "summer", 
                                                ifelse( date_var >= ymd(paste0(year(date_var), fall)) &
                                                          date_var < ymd(paste0(year(date_var), winter)), "fall", 
                                                        NA)))), 
                           levels = c("spring", "summer", "fall", "winter"))
    )
  
  #change time variable back to what it was originally
  names(dt)[names(dt) %in% "date_var"] <- .date_var
  
  return(dt)
  
}


######################################################################################################################
# * function for a standard boxplot with different whisker definitions to avoid plotting extreme/outlier points
#function takes df and returns summary statistics for plotting alternative boxplots with quantiles: 10, 25, 50, 75 and 90. this reduces the plotting of outliers, which are typically problematic when dealign with large datasets. 
######################################################################################################################

alt_boxplot <- function(df, var, min_q=0.025, max_q=0.975){
  df <- df %>%
    rename(var = all_of(var)) %>%
    
    #calculate quantiles
    summarize(
      N = n(),
      Min = min(var),
      Qmin = quantile(var, min_q),
      Q25 = quantile(var, 0.25),
      Q50 = quantile(var, 0.50),
      Q75 = quantile(var, 0.75),
      Qmax = quantile(var, max_q),
      Max = max(var)
    )
  
  names(df)[names(df)==var] <- var
  
  return(df) 
  
}

##################################################################################################
summary_table <- function(df, var){
  df <- df %>%
    rename(var = all_of(var)) %>%
    
    #calculate quantiles
    summarize(
      N = n(),
      Min = min(var),
      Q05 = quantile(var, 0.05),
      Q10 = quantile(var, 0.10),
      Q25 = quantile(var, 0.25),
      Q50 = quantile(var, 0.50),
      Q75 = quantile(var, 0.75),
      Q90 = quantile(var, 0.90),
      Q95 = quantile(var, 0.95),
      Max = max(var)
    )
  
  names(df)[names(df)==var] <- var
  
  return(df) 
  
}

################################################################################################
# fn returns coordinates for a different transformation. it convertes the dataset into a spatial object, calculates coordinates for a diff refernce system, converts these to a df, and attaches these to the original coordinates
# tutorial: https://ryanpeek.org/2017-08-03-converting-XY-data-with-sf-package/ 
################################################################################################
add_crs <- function(
  dt,
  original_crs, original_coords,
  new_crs, new_coord_names = c("long", "lat") 
) {
  
  library(sf)
  
  #convert flat file to spatial file, give it the original CRS
  dt_sp <- st_as_sf(dt, coords = original_coords, crs = original_crs)
  
  #convert to different CRS, and save the coordinates
  new_coords <- st_transform(dt_sp, crs = new_crs) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(new_x = X, 
           new_y = Y) %>%
    mutate(new_crs = new_crs)  
  
  #rename new columns
  names(new_coords) <- c(new_coord_names, paste0(c(new_coord_names, "crs"), collapse = "_"))
  
  # add new coords to original dt
  dt2 <- cbind(dt, new_coords)
  
  
  return(dt2)
  
}


################################################################################################
# CLEAN PREDICTIONS FOR KP
################################################################################################

clean_predictions <- function(dt) {
  predictions <- dt %>%
    # only predict at locations in the monitoring area w/o NAs
    filter(in_monitoring_area) %>%
    mutate(
      # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
      start_date = ymd("1988-01-01 "),
      end_date = ymd("2021-07-09 ")) %>%
    select(location_id, start_date, end_date, model, 
           variable,
           prediction) 
  
  
}

