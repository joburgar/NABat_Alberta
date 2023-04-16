#####################################################################################
# download_weather_ECCC.R
# script to download and summarise nightly weather data from ECCC
# written by Joanna Burgar - 04-April-2021
#####################################################################################

# .libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# https://docs.ropensci.org/weathercan/articles/articles/use_with_tidyverse.html
# install.packages("weathercan", 
#                  repos = c("https://ropensci.r-universe.dev", 
#                            "https://cloud.r-project.org"))
library(weathercan)
library(tidyverse)
library(lubridate)

setwd("/Volumes/LaCie_2TB/NABat_Alberta/Input")

Year_interest <- year(as.Date("2022-01-01"))
class(Year_interest)

stn <- read.csv("NABat_Station_Covariates.csv", header=T, colClasses=c("character"),
                 stringsAsFactors = TRUE,  na.string=c("","NA", "#N/A")) %>% type_convert()
stn <- stn %>% filter(!is.na(Longitude))
stn <- stn %>% filter(X2022==1)

ECCC.stn <- stn[c("Latitude","Longitude")]
tail(stn)

### function to download weather stations
download_weather_stn <- function(ECCC.stn.to.find = coords, Year_interest = 2022){

  ECCC.stn.id.list <- stations_search(coords = ECCC.stn.to.find, 
                                      interval = "hour", starts_latest=Year_interest-1, ends_earliest=Year_interest,
                                      quiet = T)
  
  ECCC.stn.id <- as_tibble(ECCC.stn.id.list)
  ECCC.stn.id <- ECCC.stn.id %>% arrange(distance)
  
  stn.to.use <- as.numeric(ECCC.stn.id[1,3])

  return(stn.to.use)
}

NABat_stns <- vector("list")
for(i in 1:nrow(ECCC.stn)){
  NABat_stns[i] <- download_weather_stn(ECCC.stn.to.find = ECCC.stn[i,])
}

NABat_stns <- unlist(NABat_stns)
NABat_stns_unique <- unique(NABat_stns)
NABat_stns_unique[18]

stn$ECCC.stn <- NABat_stns

for(i in 19:length(NABat_stns_unique)){
  
  stn.weather <- weather_dl(station_ids = NABat_stns_unique[i], 
                            start = paste0(Year_interest,"-04-01"), end = paste0(Year_interest,"-10-31"))  %>% 
    select(station_name, station_id, date, time, year, month, day, hour, temp, rel_hum, wind_spd)
  
  stn.weather <- as.data.frame(stn.weather)
  if(exists("NABat.weather")==FALSE){
    NABat.weather <- stn.weather
  } else {
    NABat.weather <- rbind(stn.weather, NABat.weather)
  }
}

stn %>% filter(ECCC.stn==30724) # not available in hours need to go further to find
stations_search(coords =c(stn %>% filter(ECCC.stn==30724) %>% select(Latitude, Longitude)),dist=200, interval="hour")

far.stn <- weather_dl(station_ids = 49490, 
                      start = paste0(Year_interest,"-04-01"), end = paste0(Year_interest,"-10-31"))  %>% 
  select(station_name, station_id, date, time, year, month, day, hour, temp, rel_hum, wind_spd)
far.stn <- as.data.frame(far.stn)

stn$ECCC.stn <- case_when(stn$ECCC.stn==30724 ~ 49490, TRUE ~ stn$ECCC.stn)

NABat.weather <- rbind(far.stn, NABat.weather)

NABat.weather %>% count(station_id)
glimpse(NABat_weather)
NABat.weather  %>% summarise(min(hour),  max(hour))

morning <- c("00:00","01:00", "02:00", "03:00", "04:00", "05:00")

NABat.weather <- NABat.weather %>% mutate(SurveyNight = case_when(hour %in% morning ~ date-1,
                                                    TRUE ~ as.Date(date)))

use <- c("18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00", "02:00","03:00","04:00","05:00")
NABat_weather_night <- NABat.weather %>% filter(hour %in% use)


NABat_weather_night %>% group_by(SurveyNight) %>% summarise(min(time), max(time))
NABat_weather_night %>% group_by(station_id, SurveyNight) %>% summarise_at(vars(temp:wind_spd), list(min, mean, max))

NABat_nightly_weather_sum <- NABat_weather_night %>% group_by(station_id, SurveyNight) %>% summarise_at(vars(temp:wind_spd), list(min, mean, max))
stn.id <- NABat_nightly_weather_sum %>% count(station_id)

write.csv(stn, "NABat_Station_Covariates_2022_weatherstn.csv")
write.csv(NABat_nightly_weather_sum, paste0("NABat_",Year_interest,"_nightly_weather_sum.csv"), row.names = FALSE)
