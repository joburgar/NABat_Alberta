# Creating files for NABat upload
###--- this script formats data ready for NABat submission
# load in the "NABat_Annual_Report_YEAR.RDS" rather than run report 

#Load Packages
list.of.packages <- c("data.table", "tidyverse", "lunar", "zoo", "lubridate")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# # Define the GRTS.Cell.ID and Year of interest if subsetting for year, studyarea
# GRTS_interest <- "922"
Year_interest <- year(as.Date("2023-01-01"))

# depending on what has been previously been run, load either annual report data or submission data
# load("NABat_Annual_Report_201522.RDS")
load(paste("NABat_Annual_Report_",Year_interest,".RDS", sep=""))
# load(paste0("/NABat_",Year_interest,"_data_for_submission.RDS"))

###- Appendix Table 3 - Nightly call counts
###---recode Classification to "AutoID" and use NABat 4-6 letter codes for NABat submission
#25k|40k|40kMyo|ANPA|ANPAEPFU|ANTPAL|ARJA|ARTJAM|BRACAV|BRCA|CHME|CHOMEX|CORA|CORRAF|CORTO|COTO|COTOVI|DIEC|DIPECA|
#EPFU|EPFULABO|EPFULANO|EPFUMYLU|EPTFUS|EUDMAC|EUFL|EUMA|EUMFLO|EUMPER|EUMUND|EUPE|EUUN|HiF|HighF|IDIPHY|IDPH|LABL|
#LABLPAHE|LABO|LABOLASE|LABOMYLU|LABONYHU|LABOPESU|LACI|LACILANO|LACITABR|LAEG|LAIN|LAMI|LANO|LANOTABR|LASBLO|LASBOR|
#LASCIN|LASE|LASEGA|LASINT|LASMIN|LASNOC|LASSEM|LASXAN|LAXA|LEMY|LENI|LEPNIV|LEPYER|LESP|LEYE|LUSO|LoF|LowF|MACA|MACCAL|
#MOLMOL|MOME|MOMO|MORMEG|MYAR|MYAU|MYCA|MYCAMYCI|MYCAMYYU|MYCI|MYCIMYVO|MYEV|MYEVMYTH|MYGR|MYKE|MYLE|MYLU|MYLUMYCI|
#MYLUMYSE|MYLUMYVO|MYOAUR|MYOAUS|MYOC|MYOCAL|MYOCIL|MYOEVO|MYOGRI|MYOKEE|MYOLEI|MYOLUC|MYOOCC|MYOSEP|MYOSOD|MYOTHY|MYOVEL|
#MYOVOL|MYOYUM|MYSE|MYSO|MYTH|MYVE|MYVO|MYYU|NOCLEP|NOISE|NOLE|NOTBAT|NYCFEM|NYCHUM|NYCMAC|NYFE|NYHU|NYMA|NYSP|NoID|PAHE|PARHES|PERSUB|PESU|STERUF|STRU|TABR|TADBRA|HiLo

SA_call_data <- dat_time %>% select(Location.Name, Filename, Timep, SurveyNight, Classification)
SA_call_data %>% filter(is.na(Timep)) # no NA Timep rows

SA_call_data$Timep <- case_when(is.na(SA_call_data$Timep) ~
                                     as.POSIXct(strptime(substr(SA_call_data$Filename,19,24),tz=tz, format="%H%M%S")),
                                     TRUE ~ as.POSIXct(SA_call_data$Timep))

SA_call_data$Timep <- case_when(is.na(SA_call_data$Timep) ~
                                     as.POSIXct(strptime(substr(SA_call_data$Filename,21,26),tz=tz, format="%H%M%S")),
                                   TRUE ~ as.POSIXct(SA_call_data$Timep))
glimpse(SA_call_data)

# create survey start date and survey end date from recording dates (may not be accurate in eff dataframe)
SA_call_data$SurveyYear <- year(SA_call_data$SurveyNight)
Survey.Dates <- SA_call_data %>% group_by(Location.Name,SurveyYear) %>% summarise(Survey.Start.Date = min(SurveyNight), Survey.End.Date = max(SurveyNight)+1)
SA_call_data$SurveyDate <- as.Date(SA_call_data$SurveyNight)
summary(SA_call_data$Timep)

SA_call_data$Time <- as.character(SA_call_data$Timep, "%H:%M:%S")

SA_call_data %>% as_tibble() %>% print(n=50)

# if the dates are mixed up, use this code below to fix SurveyDate
# # create proper Audio Time Recording Stamp (right now Timep has date Timep was created, not audio date)
# SA_call_data$SurveyDate <- case_when(SA_call_data$Timep > paste(Sys.Date(),"12:00:00") ~ SA_call_data$SurveyNight,
#                                      SA_call_data$Timep < paste(Sys.Date(),"12:00:00") ~ SA_call_data$SurveyNight + 1,
#                                      TRUE ~ as.Date(SA_call_data$SurveyNight))
# 
# # SA_call_data %>% as_tibble() %>% print(n=50)

SA_call_data$Time <- as.character(SA_call_data$Timep, "%H:%M:%S")

SA_call_data$Classification <- as.factor(SA_call_data$Classification %>%
                                        recode(`EPFU-LANO` = "EPFULANO", `LABO-MYLU` = "LABOMYLU", `Myotis 40k` = "40kMyo", MYEV.MYSE = "40kMyo",
                                               LowF = "LoF", HighF = "HiF", unknown = "NoID", noise = "NOISE"))

# to update MYEV and MYSE based on geography (N is MYSE and S is MYEV)
SA_call_data$Classification <- case_when(SA_call_data$Classification=="MYEV-MYSE" & SA_call_data$Location.Name=="139715_NE_01" ~ "MYEV",
                                          SA_call_data$Classification=="MYEV-MYSE" & SA_call_data$Location.Name!="139715_NE_01" ~ "MYSE",
                                          TRUE ~ as.character(SA_call_data$Classification))

SA_call_data %>% count(Classification)

SA_call_data2 <- left_join(SA_call_data, Survey.Dates, by = c("SurveyYear", "Location.Name"))
SA_call_data2$Survey.End.Date <- SA_call_data2$Survey.End.Date+1 # have the survey end the following day so that entries from the early morning make it in

# if the weather is correctly captured from Alberta eBat output can simply use this code
# nightly.env.cov <- dat_summary %>% group_by(Location.Name,SurveyNight) %>%
#   summarise(across(min_temp:max_wind, ~mean(.x, na.rm=TRUE)))
# summary(nightly.env.cov)

# if weather is downloaded from ECCC weathercan package, use this code

# weather1 <- read.csv("Input/NABat_2015_nightly_weather_sum.csv")
# weather2 <- read.csv("Input/NABat_2016_nightly_weather_sum.csv")
# weather3 <- read.csv("Input/NABat_2017_nightly_weather_sum.csv")
# weather4 <- read.csv("Input/NABat_2018_nightly_weather_sum.csv")
# weather5 <- read.csv("Input/NABat_2019_nightly_weather_sum.csv")
# weather6 <- read.csv("Input/NABat_2020_nightly_weather_sum.csv")
# weather7 <- read.csv("Input/NABat_2021_nightly_weather_sum.csv")
# weather8 <- read.csv("Input/NABat_2022_nightly_weather_sum.csv")
# weather <- bind_rows(weather1,weather2,weather3,weather4,weather5,weather6,weather7,weather8)
# weather <- weather %>% select(-X)
# weather %>% as_tibble()
weather <- read.csv(paste0("Input/NABat_",Year_interest,"_nightly_weather_sum.csv"))

staweather <- read.csv("Input/NABat_Station_Covariates_2023_weatherstn.csv")
colnames(weather) <- c("ECCC.stn.id", "SurveyNight", "Min.Tmp", "Min.RH", "Min.WS", "Mean.Tmp", "Mean.RH", "Mean.WS", "Max.Tmp", "Max.RH", "Max.WS")
weather$SurveyNight <- ymd(weather$SurveyNight)
nightly.env.cov <- dat_summary[c("Location.Name","SurveyNight")]
nightly.env.cov$ECCC.stn.id <- staweather$ECCC.stn[match(nightly.env.cov$Location.Name, staweather$LocName)]
nightly.env.cov <- nightly.env.cov %>% count(Location.Name, SurveyNight, ECCC.stn.id)
nightly.env.cov <- nightly.env.cov %>% select(-n)
nightly.env.cov <- left_join(nightly.env.cov, weather, by = c("SurveyNight","ECCC.stn.id"))
summary(nightly.env.cov)
nrow(nightly.env.cov)
glimpse(SA_call_data2)
glimpse(nightly.env.cov)

Bulk_call_data <- left_join(SA_call_data2 %>% dplyr::select(-SurveyYear), nightly.env.cov, by = c("SurveyNight", "Location.Name"))
Bulk_call_data$SurveyDate <- format(as.Date(Bulk_call_data$SurveyDate), '%m/%d/%Y')
Bulk_call_data$`Audio Recording Time` <- paste(Bulk_call_data$SurveyDate, Bulk_call_data$Time)
names(Bulk_call_data)

Bulk_call_data <- Bulk_call_data %>% select(Location.Name, Min.Tmp, Max.Tmp, Min.RH, Max.RH, Min.WS, Max.WS, Filename, `Audio Recording Time`, Classification, Survey.Start.Date, Survey.End.Date)

glimpse(Bulk_call_data)
colnames(Bulk_call_data) <- c("| Location Name", "Nightly Low Temperature", "Nightly High Temperature", "Nightly Low Relative Humidity", "Nightly High Relative Humidity",
                              "Nightly Low Wind Speed", "Nightly High Wind Speed", "Audio Recording Name", "Audio Recording Time", "Auto Id", "Survey Start Time", "Survey End Time")

                              
# add in null columns (NABat template)
xx <- c("Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Software Type", "Manual Id","Species List", "Manual Vetter")
Bulk_call_data[xx] <- NA

names(Bulk_call_data)
Bulk_call_data <- Bulk_call_data[c("| Location Name", "Survey Start Time", "Survey End Time", "Nightly Low Temperature", "Nightly High Temperature", "Nightly Low Relative Humidity",
                                   "Nightly High Relative Humidity", "Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Wind Speed", "Nightly High Wind Speed",
                                   "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Audio Recording Name", "Audio Recording Time", "Software Type","Auto Id", "Manual Id","Species List","Manual Vetter" )]

Bulk_call_data$`Software Type` <- "Alberta eBat"
Bulk_call_data$`Species List` <- "Alberta_01"

# NABat submission requires that the following fields are numeric (i.e., float) 
# Nightly Low Temperature	Nightly High Temperature	Nightly Low Relative Humidity	Nightly High Relative Humidity
# Nightly Low Wind Speed	Nightly High Wind Speed	Nightly Low Cloud Cover	Nightly High Cloud Cover
glimpse(Bulk_call_data)
cols.as.numeric <- c("Nightly Low Temperature",	"Nightly High Temperature",	"Nightly Low Relative Humidity",	"Nightly High Relative Humidity",
                     "Nightly Low Wind Speed",	"Nightly High Wind Speed",	"Nightly Low Cloud Cover",	"Nightly High Cloud Cover")

Bulk_call_data[cols.as.numeric]<- sapply(Bulk_call_data[cols.as.numeric],as.numeric)

cols.as.character <- c("| Location Name","Nightly High Weather Event",	"Nightly Low Weather Event",	"Auto Id", "Manual Id", "Manual Vetter")
Bulk_call_data[cols.as.character]<- sapply(Bulk_call_data[cols.as.character],as.character)

sapply(Bulk_call_data, class)
glimpse(Bulk_call_data)

# need to format dates to mm/dd/YYYY
Bulk_call_data$`Survey Start Time` <- paste(format(as.Date(Bulk_call_data$`Survey Start Time`), '%m/%d/%Y'), "12:00:00")
Bulk_call_data$`Survey End Time` <- paste(format(as.Date(Bulk_call_data$`Survey End Time`), '%m/%d/%Y'), "12:00:00")
Bulk_call_data %>% as_tibble()

###--- If want to exclude submissions based within some NP
# NABat_NPsubmit <- eff %>% filter(Land.Unit.Code %in% c("BANP","JANP","WLNP")) %>% count(Location.Name)
# Bulk_call_data_tosubmit <- Bulk_call_data %>% filter(!`Location Name` %in% NABat_NPsubmit$Location.Name)
# Bulk_call_data_tosubmit %>% filter(`Location Name` %in% NABat_NPsubmit$Location.Name) # check if it worked
# glimpse(Bulk_call_data_tosubmit)

# write.table(Bulk_call_data,"NABat_submit/Bulk_call_data_ALL.csv",na = "",row.names = FALSE,sep = ",")

# Export in annual batches
for(i in seq_along(Year_interest)){
  write.table(Bulk_call_data %>% filter(grepl(Year_interest[i],`Survey Start Time`)), paste0("NABat_submit/Bulk_call_data_",Year_interest[i],".csv"),
              na = "",row.names = FALSE,sep = ",")
  }

###--- Bulk Stationary Acoustic Meta Template
#- provide contacts with bulk data for review and reference

sta <- read.csv("Input/NABat_Station_Covariates.csv")
names(sta)
Appendix.Table1 <- sta[c("GRTSCellID","LocName", "Orig.Name","Latitude", "Longitude","WB.dist","WB_type","RD.dist",
                         "RD_Type", "Land.Cover","LandUnitCo")] 

colnames(Appendix.Table1) <- c("GRTS.Cell.ID","Location.Name", "Orig_Name","Latitude", "Longitude","Waterbody.Distance","Waterbody.Type","Road.Distance",
                         "Road.Type", "Land.Use.Type","Land.Unit.Code")

eff %>% group_by(Location.Name) %>% count(Contact)

Appendix.Table1$Contact <- eff$Contact[match(Appendix.Table1$Location.Name, eff$Location.Name)]
Appendix.Table1 <- left_join(Appendix.Table1, eff %>% select(Location.Name, Contact, Deployment.ID))
Appendix.Table1 <- Appendix.Table1 %>% 
  filter(Deployment.ID==Year_interest) %>%
  dplyr::select(-Deployment.ID)

Survey.Dates %>% group_by(Location.Name) %>% arrange(Survey.Start.Date) %>% filter(row_number()==1)
Survey.Dates %>% group_by(Location.Name, SurveyYear) %>% summarise(Survey.Start.Date = min(Survey.Start.Date), max(Survey.End.Date))


Appendix.Table1 <- left_join(Appendix.Table1, Survey.Dates)


# glimpse(Appendix.Table1)
# issues when exported = 1. Developed (need to change to Urban); 2. Forest-mixed not accepted (change to Forest-conifer); 3. BANP GRTS as 132485 should be 132458

# add in null columns (NABat template)
xx <- c("Detector","Detector.Serial.Number", "Microphone","Microphone.Serial.Number","Microphone.Orientation",
        "Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter","Weather.Proofing","Unusual.Occurrences")
Appendix.Table1[xx] <- as.character(NA)

# put in the same order as NABat template
names(Appendix.Table1)
Appendix.Table1 <- Appendix.Table1[c("GRTS.Cell.ID","Location.Name", "Orig_Name","Latitude", "Longitude","Survey.Start.Date", 
                                     "Survey.End.Date", "Detector", "Detector.Serial.Number", "Microphone","Microphone.Serial.Number",
                                     "Microphone.Orientation","Microphone.Height","Clutter.Distance","Clutter.Type",
                                     "Waterbody.Distance","Waterbody.Type","Percent.Clutter","Road.Distance","Road.Type",
                                     "Land.Use.Type","Land.Unit.Code","Contact","Weather.Proofing","Unusual.Occurrences")]

Appendix.Table1 %>% count(Land.Use.Type)
Appendix.Table1$Land.Use.Type <- Appendix.Table1$Land.Use.Type %>% recode(Developed = "Urban", `Forest-mixed` = "Forest-conifer") %>% tolower()

Bulk_site_meta <- Appendix.Table1 %>% select(-Road.Distance,-Road.Type, -Orig_Name)
names(Bulk_site_meta)
glimpse(Bulk_site_meta)
colnames(Bulk_site_meta) <- c("| GRTS Cell Id", "Location Name","Latitude", "Longitude", "Survey Start Time", "Survey End Time", "Detector", "Detector Serial Number",
                               "Microphone", "Microphone Serial Number", "Microphone Orientation", "Microphone Height (meters)", "Distance to Nearest Clutter (meters)", "Clutter Type",
                               "Distance to Nearest Water (meters)", "Water Type", "Percent Clutter", "Broad Habitat Type", "Land Unit Code","Contact","Weather Proofing", "Unusual Occurrences" )

Bulk_site_meta$`Weather Proofing` <- as.logical(Bulk_site_meta$`Weather Proofing`)

cols.as.numeric <- c("Microphone Height (meters)",	"Distance to Nearest Clutter (meters)",	"Percent Clutter")
Bulk_site_meta[cols.as.numeric]<- sapply(Bulk_site_meta[cols.as.numeric],as.numeric)
Bulk_site_meta$`Land Unit Code` <- recode(Bulk_site_meta$`Land Unit Code`, "BNP"="BANP", "JNP"="JANP","LPR"="LOPR","LAR"="LOAR",
                                          "NSR"="NOSR","RDR"="REDR","SSR"="SOSR","UAR"="UPAR","UPR"="UPPR")
glimpse(Bulk_site_meta)
nrow(Bulk_site_meta)
# Export
# write.table(Bulk_site_meta, "NABat_submit/Bulk_site_meta_ALL.csv",na = "",row.names = FALSE,sep = ",")

write.table(Bulk_site_meta, paste0("NABat_submit/Bulk_site_meta_",Year_interest,".csv"),na = "",row.names = FALSE,sep = ",")

# write.table(Bulk_site_meta, "Bulk_site_meta.csv",na = "",row.names = FALSE,sep = ",")


# # Write files for bio review
# Export in bio batches
Bulk_site_meta %>% count(Contact)
bio_contact <- c("allison","barb", "Brett", "hurtado","hughes","olson","david","jenna","helena","Unrhuh","julie","lisa","Steenweg","nina","saakje","sandi")
for(i in seq_along(bio_contact)){
  write.table(Bulk_site_meta %>% filter(grepl(bio_contact[i],Contact)),paste0("NABat_submit/Bulk_site_meta_2023_",bio_contact[i],".csv"), na = "",row.names = FALSE,sep = ",")
}

###--- Bulk Stationary Acoustic Data Template
#- provide contacts with bulk data for review and reference
glimpse(Bulk_call_data)
contact_cells <- Bulk_site_meta %>% select(Contact, `Location Name`)

for(i in seq_along(bio_contact)){
  write.table(Bulk_call_data %>% filter(`| Location Name` %in% contact_cells[grepl(bio_contact[i], contact_cells$Contact),]$`Location Name`),
              paste0("NABat_submit/Bulk_call_data_2023_",bio_contact[i],".csv"), 
              na = "",row.names = FALSE,sep = ",")
}

getwd()
rm(list=setdiff(ls(), c("Bulk_call_data","Bulk_site_meta")))

##############################################################################################