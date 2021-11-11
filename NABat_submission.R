# Creating files for NABat upload
###--- this script formats data ready for NABat submission
# to be used with NABat_Annual_Report_Appendices.Rmd and Appendix_maps.R
# or just load in the "NABat_data_for_submission.RDS" rather than run report 

#Load Packages
list.of.packages <- c("data.table", "tidyverse", "lunar", "zoo", "lubridate")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# save.image("NABat_data_for_submission.RDS")
# load("NABat_data_for_submission.RDS")

###- Appendix Table 3 - Nightly call counts
###---recode Classification to "AutoID" and use NABat 4-6 letter codes for NABat submission
#25k|40k|40kMyo|ANPA|ANPAEPFU|ANTPAL|ARJA|ARTJAM|BRACAV|BRCA|CHME|CHOMEX|CORA|CORRAF|CORTO|COTO|COTOVI|DIEC|DIPECA|
#EPFU|EPFULABO|EPFULANO|EPFUMYLU|EPTFUS|EUDMAC|EUFL|EUMA|EUMFLO|EUMPER|EUMUND|EUPE|EUUN|HiF|HighF|IDIPHY|IDPH|LABL|
#LABLPAHE|LABO|LABOLASE|LABOMYLU|LABONYHU|LABOPESU|LACI|LACILANO|LACITABR|LAEG|LAIN|LAMI|LANO|LANOTABR|LASBLO|LASBOR|
#LASCIN|LASE|LASEGA|LASINT|LASMIN|LASNOC|LASSEM|LASXAN|LAXA|LEMY|LENI|LEPNIV|LEPYER|LESP|LEYE|LUSO|LoF|LowF|MACA|MACCAL|
#MOLMOL|MOME|MOMO|MORMEG|MYAR|MYAU|MYCA|MYCAMYCI|MYCAMYYU|MYCI|MYCIMYVO|MYEV|MYEVMYTH|MYGR|MYKE|MYLE|MYLU|MYLUMYCI|
#MYLUMYSE|MYLUMYVO|MYOAUR|MYOAUS|MYOC|MYOCAL|MYOCIL|MYOEVO|MYOGRI|MYOKEE|MYOLEI|MYOLUC|MYOOCC|MYOSEP|MYOSOD|MYOTHY|MYOVEL|
#MYOVOL|MYOYUM|MYSE|MYSO|MYTH|MYVE|MYVO|MYYU|NOCLEP|NOISE|NOLE|NOTBAT|NYCFEM|NYCHUM|NYCMAC|NYFE|NYHU|NYMA|NYSP|NoID|PAHE|PARHES|PERSUB|PESU|STERUF|STRU|TABR|TADBRA|HiLo

SA_call_meta <- dat_time %>% select(Location.Name, Filename, Timep, SurveyNight, Classification)
SA_call_meta$Timep <- case_when(is.na(SA_call_meta$Timep) ~
                                     as.POSIXct(strptime(substr(SA_call_meta$Filename,19,24),tz=tz, format="%H%M%S")),
                                     TRUE ~ as.POSIXct(SA_call_meta$Timep))

SA_call_meta$Timep <- case_when(is.na(SA_call_meta$Timep) ~
                                     as.POSIXct(strptime(substr(SA_call_meta$Filename,21,26),tz=tz, format="%H%M%S")),
                                   TRUE ~ as.POSIXct(SA_call_meta$Timep))


# create survey start date and survey end date from recording dates (may not be accurate in eff dataframe)
SA_call_meta$SurveyYear <- year(SA_call_meta$SurveyNight)
Survey.Dates <- SA_call_meta %>% group_by(Location.Name,SurveyYear) %>% summarise(Survey.Start.Date = min(SurveyNight), Survey.End.Date = max(SurveyNight)+1)
SA_call_meta$SurveyDate <- as.Date(SA_call_meta$SurveyNight)
summary(SA_call_meta$Timep)

# create proper Audio Time Recording Stamp (right now Timep has date Timep was created, not audio date)
SA_call_meta$SurveyDate <- case_when(SA_call_meta$Timep > paste(Sys.Date(),"12:00:00") ~ SA_call_meta$SurveyNight,
                                     SA_call_meta$Timep < paste(Sys.Date(),"12:00:00") ~ SA_call_meta$SurveyNight + 1,
                                     TRUE ~ as.Date(SA_call_meta$SurveyNight))

SA_call_meta$Time <- as.character(SA_call_meta$Timep, "%H:%M:%S")

SA_call_meta$Classification <- as.factor(SA_call_meta$Classification %>%
                                        recode(`EPFU-LANO` = "EPFULANO", `LABO-MYLU` = "LABOMYLU", `Myotis 40k` = "40kMyo", MYEV.MYSE = "40kMyo",unknown = "NoID", noise = "NOISE"))

glimpse(SA_call_meta)
SA_call_meta2 <- left_join(SA_call_meta, Survey.Dates, by = c("SurveyYear", "Location.Name"))

Bulk_call_meta <- left_join(SA_call_meta2 %>% dplyr::select(-SurveyYear), nightly.env.cov, by = c("SurveyNight", "Location.Name"))
Bulk_call_meta$SurveyDate <- format(as.Date(Bulk_call_meta$SurveyDate), '%d/%m/%Y')
Bulk_call_meta$`Audio Recording Time` <- paste(Bulk_call_meta$SurveyDate, Bulk_call_meta$Time)
glimpse(Bulk_call_meta)
Bulk_call_meta <- Bulk_call_meta %>% select(Location.Name, Min.Tmp, Max.Tmp, Min.RH, Max.RH, Min.WS, Max.WS, Filename, `Audio Recording Time`, Classification, Survey.Start.Date, Survey.End.Date)


colnames(Bulk_call_meta) <- c("Location Name", "Nightly Low Temperature", "Nightly High Temperature", "Nightly Low Relative Humidity", "Nightly High Relative Humidity",
                              "Nightly Low Wind Speed", "Nightly High Wind Speed", "Audio Recording Name", "Audio Recording Time", "Auto Id", "Survey Start Time", "Survey End Time")

# add in null columns (NABat template)
xx <- c("Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Software Type", "Manual Id","Species List")
Bulk_call_meta[xx] <- NA

names(Bulk_call_meta)
Bulk_call_meta <- Bulk_call_meta[c("Location Name", "Survey Start Time", "Survey End Time", "Nightly Low Temperature", "Nightly High Temperature", "Nightly Low Relative Humidity",
                                   "Nightly High Relative Humidity", "Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Wind Speed", "Nightly High Wind Speed",
                                   "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Audio Recording Name", "Audio Recording Time", "Software Type","Auto Id", "Manual Id","Species List")]

Bulk_call_meta$`Software Type` <- "Alberta eBat"
Bulk_call_meta$`Species List` <- "Alberta_01"

# NABat submission requires that the following fields are numeric (i.e., float) 
# Nightly Low Temperature	Nightly High Temperature	Nightly Low Relative Humidity	Nightly High Relative Humidity
# Nightly Low Wind Speed	Nightly High Wind Speed	Nightly Low Cloud Cover	Nightly High Cloud Cover
glimpse(Bulk_call_meta)
cols.as.numeric <- c("Nightly Low Temperature",	"Nightly High Temperature",	"Nightly Low Relative Humidity",	"Nightly High Relative Humidity",
                     "Nightly Low Wind Speed",	"Nightly High Wind Speed",	"Nightly Low Cloud Cover",	"Nightly High Cloud Cover")

Bulk_call_meta[cols.as.numeric]<- sapply(Bulk_call_meta[cols.as.numeric],as.numeric)

cols.as.character <- c("Location Name","Nightly High Weather Event",	"Nightly Low Weather Event",	"Auto Id", "Manual Id")
Bulk_call_meta[cols.as.character]<- sapply(Bulk_call_meta[cols.as.character],as.character)

sapply(Bulk_call_meta, class)
glimpse(Bulk_call_meta)

# need to format dates to mm/dd/YYYY
Bulk_call_meta$`Survey Start Time` <- format(as.Date(Bulk_call_meta$`Survey Start Time`), '%d/%m/%Y')
Bulk_call_meta$`Survey End Time` <- format(as.Date(Bulk_call_meta$`Survey End Time`), '%d/%m/%Y')

# also have bulk call meta for all stations outside of NP (except Elk Island)
###--- If want to exclude submissions based within some NP
# NABat_NPsubmit <- eff %>% filter(Land.Unit.Code %in% c("BANP","JANP","WLNP")) %>% count(Location.Name)
# Bulk_call_meta_tosubmit <- Bulk_call_meta %>% filter(!`Location Name` %in% NABat_NPsubmit$Location.Name)
# Bulk_call_meta_tosubmit %>% filter(`Location Name` %in% NABat_NPsubmit$Location.Name) # check if it worked
# glimpse(Bulk_call_meta_tosubmit)

# Export in annual batches
for(i in seq_along(Year_interest)){
write.csv(Bulk_call_meta %>% filter(grepl(Year_interest[i],`Survey Start Time`)), paste0("NABat_submit/Bulk_call_meta_",Year_interest[i],".csv"), row.names = FALSE) # only the sites to submit
}
# write.table(Bulk_call_meta_tosubmit, "Bulk_call_meta.csv",na = "",row.names = FALSE,sep = ",")

###--- Bulk Stationary Acoustic Meta Template
#- provide contacts with bulk data for review and reference
# glimpse(Appendix.Table1)
Bulk_site_meta <- Appendix.Table1 %>% select(-Road.Distance,-Road.Type,-Human.Footprint.Distance,-Human.Footprint.Sublayer,-Human.Footprint.Type, -Orig.Name)
colnames(Bulk_site_meta) <- c("GRTS Cell Id", "Location Name","Latitude", "Longitude", "Survey Start Time", "Survey End Time", "Detector", "Detector Serial Number",
                               "Microphone", "Microphone Serial Number", "Microphone Orientation", "Microphone Height (meters)", "Distance to Nearest Clutter (meters)", "Clutter Type",
                               "Distance to Nearest Water (meters)", "Water Type", "Percent Clutter", "Broad Habitat Type", "Land Unit Code","Contact","Weather Proofing", "Unusual Occurrences" )

Bulk_site_meta$`Microphone Height (meters)` <- as.numeric(Bulk_site_meta$`Microphone Height (meters)`)
Bulk_site_meta$`Distance to Nearest Clutter (meters)` <- as.numeric(Bulk_site_meta$`Distance to Nearest Clutter (meters)`)
Bulk_site_meta$`Percent Clutter` <- as.numeric(Bulk_site_meta$`Percent Clutter`)
Bulk_site_meta$`Weather Proofing` <- as.logical(Bulk_site_meta$`Weather Proofing`)

# write.table(Bulk_site_meta, "Bulk_site_meta.csv",na = "",row.names = FALSE,sep = ",")


# # Write files for bio review
write.table(Bulk_site_meta %>% filter(grepl("barb",Contact)),"Bulk_site_meta_BNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("Brett",Contact)),"Bulk_site_meta_LAR.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("hurtado",Contact)),"Bulk_site_meta_UoA.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("hughes",Contact)),"Bulk_site_meta_PR1.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("olson",Contact)),"Bulk_site_meta_Cori.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("david",Contact)),"Bulk_site_meta_EINP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("helena",Contact)),"Bulk_site_meta_WLNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("Unruh",Contact)),"Bulk_site_meta_RDR.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("lisa",Contact)),"Bulk_site_meta_Lisa.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("Steenweg",Contact)),"Bulk_site_meta_PR2.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("saakje",Contact)),"Bulk_site_meta_JNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_site_meta %>% filter(grepl("sandi",Contact)),"Bulk_site_meta_MH.csv", na = "",row.names = FALSE,sep = ",")


###--- Bulk Stationary Acoustic Data Template
#- provide contacts with bulk data for review and reference
glimpse(Bulk_call_meta)

write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("barb", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_BNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("Brett", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_LAR.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("hurtado", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_UofA.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("hughes", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_PR1.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("olson", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_Cori.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("david", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_EINP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("helena", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_WLNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("Unruh", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_RDR.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("lisa", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_Lisa.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("Steenweg", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_PR2.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("saakje", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_JNP.csv", na = "",row.names = FALSE,sep = ",")
write.table(Bulk_call_meta %>% filter(`Location Name` %in% contact_cells[grepl("sandi", contact_cells$Contact),]$`Location Name`),
          "Bulk_call_meta_MH.csv", na = "",row.names = FALSE,sep = ",")

##############################################################################################