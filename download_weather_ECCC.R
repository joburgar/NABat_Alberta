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

setwd("/Volumes/LaCie/NABat_Alberta/Input")

Year_interest <- year(as.Date("2021-01-01"))

stn <- read.csv("NABat_Station_Covariates.csv", header=T, colClasses=c("character"),
                 stringsAsFactors = TRUE,  na.string=c("","NA", "#N/A")) %>% type_convert()
stn <- stn %>% filter(!is.na(Longitude))

ECCC.stn <- stn[c("Latitude","Longitude")]
glimpse(ECCC.stn)

ECCC.stn.id <- vector('list',nrow(ECCC.stn))
for (i in 1:nrow(ECCC.stn)){
  ECCC.stn.id.list <- stations_search(coords = ECCC.stn[1,], 
                    interval = "hour", starts_latest=Year_interest-1, ends_earliest=Year_interest,
                    quiet = T) 
  ECCC.stn.id[i] <- ECCC.stn.id.list[1,c("station_id")]
}

ECCC.stn$station_id <- unlist(ECCC.stn.id)
NABat.stn.id <- unique(ECCC.stn$station_id)

NABat_weather <- weather_dl(station_ids = NABat.stn.id, 
                                 start = paste0(Year_interest,"-04-01"), end = paste0(Year_interest,"-10-31"))  %>% 
  select(station_name, station_id, date, time, year, month, day, hour, temp, rel_hum, wind_spd)

ECCC.stn$id <- rownames(ECCC.stn)
stn$id <- rownames(stn)
stn$stn.id <- ECCC.stn$station_id[match(stn$id, ECCC.stn$id)]

# stn %>% filter(stn.id %in% no.weather)
# 
# no.weather.coords <- ECCC.stn %>% filter(station_id %in% no.weather) %>% select(Latitude, Longitude)
  
# stations_search(coords = no.weather.coords[1,1:2],
#                 interval = "hour", starts_latest=2020-04-01, ends_earliest=2020-10-31,
#                 quiet = T)


NABat_weather %>% count(station_id)
glimpse(NABat_weather)
NABat_weather %>% summarise(min(hour),  max(hour))

morning <- c("00:00","01:00", "02:00", "03:00", "04:00", "05:00")

NABat_weather <- NABat_weather %>% mutate(SurveyNight = case_when(hour %in% morning ~ date-1,
                                                    TRUE ~ as.Date(date)))

use <- c("18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00", "02:00","03:00","04:00","05:00")
NABat_weather_night <- NABat_weather %>% filter(hour %in% use)


NABat_weather_night %>% group_by(SurveyNight) %>% summarise(min(time), max(time))
NABat_weather_night %>% group_by(station_id, SurveyNight) %>% summarise_at(vars(temp:wind_spd), list(min, mean, max))

NABat_nightly_weather_sum <- NABat_weather_night %>% group_by(station_id, SurveyNight) %>% summarise_at(vars(temp:wind_spd), list(min, mean, max))
stn.id <- NABat_nightly_weather_sum %>% count(station_id)

# stn$stn.id_2020 <- stn.id_2020$n[match(stn$stn.id, stn.id_2020$station_id)]
# as.data.frame(stn %>% select(Orig_Name, stn.id_2020))

write.csv(stn %>% dplyr::select(-stn.id_2021), "NABat_Station_Covariates_2021_weatherstn.csv")
write.csv(NABat_nightly_weather_sum, paste0("NABat_",Year_interest,"_nightly_weather_sum.csv"), row.names = FALSE)
