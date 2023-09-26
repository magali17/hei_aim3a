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

pacman::p_load(httr, #get data from URL
               jsonlite, #structure the data
               dplyr
)


#source("functions.R")
set.seed(1)


##################################################################################################
# GET EPA DATA
##################################################################################################

epa_dt_path <- file.path("data", "epa_data_mart")
if(!dir.exists(epa_dt_path)) {dir.create(epa_dt_path)}

email <- rstudioapi::askForPassword("user name")
key <- rstudioapi::askForPassword("password/key")

# bdate <- "20170101"  #"19900101"
# edate <- "20171231"  #"20230101"
bdate <-"20190222"
edate <-"20200221"#"20200317"

# queries
state_list_query <- "https://aqs.epa.gov/data/api/list/states?"
parameter_list_query <- "https://aqs.epa.gov/data/api/list/classes?"
specific_parameter_list_query <-"https://aqs.epa.gov/data/api/list/parametersByClass?"
monitors_by_county_query <- "https://aqs.epa.gov/data/api/monitors/byCounty?"
counties_by_state <- "https://aqs.epa.gov/data/api/list/countiesByState?"
sites_by_county_query <- "https://aqs.epa.gov/data/api/list/sitesByCounty?"

#monitor by state
monitor_query <- "https://aqs.epa.gov/data/api/monitors/byState?" #by state
#specific monitor
site_query <- "https://aqs.epa.gov/data/api/monitors/bySite?"

annual_data_query <- "https://aqs.epa.gov/data/api/annualData/byState?" 
annual_data_by_site <- "https://aqs.epa.gov/data/api/annualData/bySite?"
daily_data_query <- "https://aqs.epa.gov/data/api/dailyData/byState?"  #quarterlyData

##################################################################
# BASIC LISTS & FNS
# check that server is working
server_status <- GET("https://aqs.epa.gov/data/api/metaData/isAvailable") %>%http_status()
if(grepl("Success", server_status$message)){
  message("Success: Server is running")
} else {
  server_status$message}


# base_query=data_by_county
# param= paste(nox_param, collapse = ",")
# bdate=bdate
# edate="20191231"
# state=wa_st
# county=king_county
# 
# dt <- GET(base_query, query = list(email = email, key=key, param= paste(nox_param, collapse = ","), bdate=bdate, edate=edate,state=wa_st,county=king_county))

dt_name = "wa_county_nox" 
get_lists <- function(base_query, save_data=TRUE, dt_name,  ...) {
  dt <- GET(base_query, query = list(email = email, key=key, ...))
  
  (status <- http_status(dt)$message)
  if(!grepl("Success", status)) {stop(status)}
  
  dt <- dt %>%
    httr::content(as="text") %>%
    jsonlite::fromJSON() #%>% purrr::pluck(2)
  
  dt <- dt$Data
  
  if(save_data==TRUE) {
    message("saving EPA data")
    saveRDS(dt,file.path(epa_dt_path, paste0(dt_name, ".rda")))
  }
  
  return(dt)
}

# save if state, parameter lists if don't exist already
lapply(c("states", "specific_parameters", "counties_by_state", "sites_by_county"), function(x) {
  message(paste("getting", x, "data"))
  dt_file <- file.path(epa_dt_path, paste0(x, ".rda"))
  if(!file.exists(dt_file) & x=="states"){get_lists(base_query = state_list_query, dt_name = x)}
  if(!file.exists(dt_file) & x=="counties_by_state"){get_lists(base_query = counties_by_state, dt_name = x, state=wa_st)}
  if(!file.exists(dt_file) & x=="sites_by_county"){get_lists(base_query = monitors_by_county_query, dt_name = x, state=wa_st, county=king_county)}
  if(!file.exists(dt_file) & x=="specific_parameters"){get_lists(base_query = specific_parameter_list_query, dt_name = x, pc="ALL")}
})

message("loading saved EPA data")
states <- readRDS(file.path(epa_dt_path, "states.rda"))
#parameters <- readRDS(file.path(epa_dt_path, "parameters.rda"))
specific_parameters <- readRDS(file.path(epa_dt_path, "specific_parameters.rda"))
counties_in_wa <- readRDS(file.path(epa_dt_path, "monitors_by_county.rda"))

nox_param <- specific_parameters %>%
  filter(grepl("NO2|NOx", value_represented)) %>%
  pull(code) # "42602" "42603"

bc_param <- specific_parameters %>%
  filter(grepl("Black Carbon", value_represented)) %>%
  pull(code) # "85313" "88313" "88317"

wa_st <- states %>%
  filter(value_represented=="Washington") %>%
  pull(code) #53

king_county <- counties_in_wa %>%
  filter(value_represented=="King") %>%
  pull(code)# "033"
###############################################################
data_by_county <- "https://aqs.epa.gov/data/api/sampleData/byCounty?"

nox_2019 <- get_lists(data_by_county, param= paste(nox_param, collapse = ","),
                      bdate="20190101", edate="20191231", #edate, 
                      state=wa_st, county=king_county,
                      save_data=FALSE)

nox_2020 <- get_lists(data_by_county, param= paste(nox_param, collapse = ","),
                      bdate="20200101", edate= "20201212" , #edate, 
                      state=wa_st, county=king_county,
                      save_data=FALSE)

wa_county_nox <- rbind(nox_2019, nox_2020) %>%
  mutate(site_name = ifelse(site_number=="0030", "10th & Weller", 
                       ifelse(site_number=="0080", "Beacon Hill", NA)))
    
saveRDS(wa_county_nox, file.path(epa_dt_path, paste0("wa_county_nox", ".rda")))


