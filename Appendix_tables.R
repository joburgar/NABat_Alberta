###--- this script generates tables for annual GRTS specific appendices
# to be used with NABat_Annual_Report_Appendices.Rmd, read in once bulk of report code has been run
# or just load in the "NABat_Annual_Submission.RDS" rather than run report 

#Load Packages
list.of.packages <- c("data.table", "knitr", "tidyr")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# save.image("NABat_Annual_Submission.RDS")
# load("NABat_Annual_Submission.RDS")

# # Define the GRTS.Cell.ID and Year of interest if subsetting for year, studyarea
Year_interest <- year(as.Date("2023-01-01"))
 
load(paste("NABat_Annual_Report_",Year_interest,".RDS", sep=""))

#############################################################################
# filter to GRTS.Cell.ID of interest if running appendices
# unique.GRTS.Cell.ID <- "3667"

# sta <- sta %>% filter(GRTS.Cell.ID==GRTS_interest)
# dat_summary <- dat_summary %>% filter(GRTS.Cell.ID==GRTS_interest)
# call_count <- call_count %>% filter(GRTS.Cell.ID==GRTS_interest)


### now run through (either for individual or all GRTS cells) - still by year
GRTS.yrs.surveyed <- sta %>% filter(X2023==1) %>% group_by(GRTS.Cell.ID) %>% summarise(max = max(num.years))

cov.prop %>% filter(Cov=="Natural.Region") %>% arrange(prop.stn)


dat_summary <- droplevels(dat_summary)
call_count <- droplevels(call_count)
# eff <- droplevels(eff)
sta <- droplevels(sta)
# names(eff)
# call_count %>% group_by(Natural.Region) %>% summarise(mean(Count))

call_count.Sp <- call_count %>% group_by(Classification, SurveyNight, Volancy)
call_count.Sp <- droplevels(call_count.Sp)
call_count.Sp$GRTS.Cell.ID <- as.character(call_count.Sp$GRTS.Cell.ID)

stns.surveyed <- call_count %>% group_by(Natural.Region, Location.Name, SurveyNight, Volancy) %>% summarise(Count =sum(Count))
# stns.surveyed %>% ungroup() %>% summarise(mean(Count), min(Count), max(Count), se(Count))
# stns.surveyed %>% arrange(Count) %>% tail()
# stns.surveyed %>% filter(Count>999) %>% count(Location.Name)
# stns.surveyed %>% group_by(Natural.Region) %>% summarise(mean(Count), se(Count))
# stns.surveyed %>% group_by(Natural.Region, Volancy) %>% summarise(mean(Count), se(Count))

call_count %>% group_by(Location.Name) %>% summarise(Count = sum(Count)) %>% arrange(Count) %>% print(n=153)

nights.surveyed <- dat_summaryT %>% ungroup ()%>% group_by(Location.Name) %>% count(SurveyNight)
nights.surveyed$Count <- 1
nights.surveyed <- nights.surveyed %>% group_by(Location.Name) %>% count(Count)
nights.surveyed %>% ungroup() %>% summarise(mean(n), min(n), max(n), se(n))
call_count.Sp %>% ungroup() 
  
## Need to create Appendix, one appendix for each GRTS Cell
# use the NABat site meta and call submission templates as required rows
# may include slightly more detailed covariate data in report, but not for NABat submission
# include tables in report as first appendix, along with datasheets as recommendation for future use
# with cover page = map (inset of Alberta? or just refer to Fig 1?)
## Stationary Point data sheet: Surveyor details, Site Locations, Recording Details, Detector Details

## Environmental data sheet with rows as dates and columns as Max, Min, Mean
# for three sections: Nightly Temp (degree), Nightly Rel Hum (%); Wind speed (km/h)

## sta covariates: NR, LUT, HF + dist, Road + dist, Water + dist (for each station)
## graph: Mean nightly call sequences - passive survey (y axis), species / species group (with fill = station)
## table: Total bat call sequences - passive survey 
# rows = survey nights, colums = species / species group and each station has own section

#knitr::include_graphics(c(paste("./Output/Maps/GRTSID_",i,"_map.png",sep="")))

### Water type and distance
# sta %>% filter(is.na(Waterbody.Distance)) %>% count(Stream.Type)
# some streams are closer than waterbodies, perhaps change waterbody type to stream?
# yes - stream and water have similar features - consolidate into one called "Water" with the closest distance being the type/distance attributed
# sta %>% filter(Stream.Distance<200 | Waterbody.Distance<200) %>% group_by(Stream.Type, Waterbody.Type) %>%
# select(Location.Name, Stream.Distance, Waterbody.Distance, Stream.Type, Waterbody.Type)

# sta$Water.Distance <- with(sta, pmin(Stream.Distance, Waterbody.Distance, na.rm=T))
sta$Water.Distance <- sta$Waterbody.Distance
# sta <- sta %>% filter(GRTS.Cell.ID %in% GRTS_interest)

# sta$Water.Type <- case_when(sta$Waterbody.Distance < sta$Stream.Distance ~ as.character(sta$Waterbody.Type),
#                             TRUE ~ as.character(sta$Stream.Type))
# as.data.frame(sta %>% group_by(Water.Type) %>% select(Waterbody.Type, Stream.Type, Waterbody.Distance, Stream.Distance, Water.Distance))

sta$Water.Type <- sta$Waterbody.Type

###--- Appendix Table 1
## Stationary Point data sheet: Surveyor details, Site Locations, Recording Details, Detector Details
## for three sections: Nightly Temp (degree), Nightly Rel Hum (%); Wind speed (km/h)
## sta covariates: NR, LUT, HF + dist, Road + dist, Water + dist (for each station)

Appendix.Table1 <- sta[c("GRTS.Cell.ID","Location.Name", "Orig_Name","Latitude", "Longitude","Waterbody.Distance","Waterbody.Type","Road.Distance",
                         "Road.Type", "Land.Use.Type","Land.Unit.Code")] 

Appendix.Table1 <- left_join(Appendix.Table1, eff %>% select(Location.Name, Contact, Deployment.ID))
Appendix.Table1 <- Appendix.Table1 %>% 
  filter(Deployment.ID==Year_interest) %>%
  dplyr::select(-Deployment.ID)

# add in start and end dates for surveys, based on when recordings started and stopped (Survey Night)
survey.dates <- call_count.Sp %>% group_by(Location.Name) %>% summarise(Survey.Start.Time=min(SurveyNight), Survey.End.Time=max(SurveyNight))

Appendix.Table1 <- left_join(Appendix.Table1, survey.dates)

# add in null columns (NABat template)
xx <- c("Detector","Detector.Serial.Number", "Microphone","Microphone.Serial.Number","Microphone.Orientation",
        "Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter","Weather.Proofing","Unusual.Occurrences")
Appendix.Table1[xx] <- as.character(NA)

# put in the same order as NABat template
Appendix.Table1 <- Appendix.Table1[c("GRTS.Cell.ID","Location.Name", "Orig_Name","Latitude", "Longitude","Survey.Start.Time", 
                                     "Survey.End.Time", "Detector", "Detector.Serial.Number", "Microphone","Microphone.Serial.Number",
                                     "Microphone.Orientation","Microphone.Height","Clutter.Distance","Clutter.Type", "Percent.Clutter",
                                     "Waterbody.Distance","Waterbody.Type","Road.Distance","Road.Type",
                                     "Land.Use.Type","Land.Unit.Code","Contact","Weather.Proofing","Unusual.Occurrences")]

###--- Appendix Table 2

## Environmental data sheet with rows as dates and columns as Max, Min
# use other code for creating weather data and import as csv
staweather <- read.csv(paste0("Input/NABat_Station_Covariates_",Year_interest,"_weatherstn.csv"))
weather <- read.csv(paste0("Input/NABat_",Year_interest,"_nightly_weather_sum.csv"))
head(weather)
colnames(weather) <- c("ECCC.stn.id", "SurveyNight", "Min.Tmp", "Min.RH", "Min.WS", "Mean.Tmp", "Mean.RH", "Mean.WS", "Max.Tmp", "Max.RH", "Max.WS")
weather$SurveyNight <- ymd(weather$SurveyNight)
nightly.env.cov <- dat_summary[c("Location.Name","SurveyNight")]
nightly.env.cov$ECCC.stn.id <- staweather$ECCC.stn[match(nightly.env.cov$Location.Name, staweather$LocName)]
nightly.env.cov <- nightly.env.cov %>% count(Location.Name, SurveyNight, ECCC.stn.id)
nightly.env.cov <- nightly.env.cov %>% select(-n)
nightly.env.cov$SurveyNight <- as.Date(nightly.env.cov$SurveyNight)
weather$SurveyNight <- as.Date(weather$SurveyNight)
nightly.env.cov <- left_join(nightly.env.cov, weather, by = c("SurveyNight","ECCC.stn.id"))
# summary(nightly.env.cov)
# nrow(nightly.env.cov)
# glimpse(dat_summary)
# glimpse(nightly.env.cov)

colnames(nightly.env.cov) <- c("Location.Name","SurveyNight","ECCC.stn.id","min_temp","min_hum","min_wind","mean_temp","mean_hum","mean_wind","max_temp","max_hum","max_wind")

# # if the weather is correctly captured from Alberta eBat output can simply use this code
# nightly.env.cov <- dat_summary %>% group_by(Location.Name,SurveyNight) %>%
  # summarise(across(min_temp:max_wind, ~mean(.x, na.rm=TRUE)))


Appendix.Table2 <- nightly.env.cov[c("Location.Name","SurveyNight","min_temp","max_temp","min_hum","max_hum","min_wind","max_wind")] #%>% ungroup
# opts <- options(knitr.kable.NA = "")
# knitr::kable(Appendix.Table2 %>% filter(grepl(i, Location.Name)) %>% select(-Location.Name),
#              caption = paste("Table 2 - NABat Survey Weather Conditions for GRTS Cell",i),
#              digits=1)
# options(opts)
# Weather for appendices
# GRTS.Cell.ID <- unique.GRTS.Cell.ID
# write.table(Appendix.Table2 %>% filter(grepl(GRTS.Cell.ID, Location.Name)), paste("GRTS_",GRTS.Cell.ID,"_Appendix.Table2.csv", sep=""),na = "",row.names = FALSE,sep = ",")