###--- this script generates individual annual GRTS specific appendices
# to be used with NABat_Annual_Report.Rmd, read in once bulk of report code has been run

dat_count <- droplevels(dat_count)
dat_summary <- droplevels(dat_summary)
dat_time <- droplevels(dat_time)
eff <- droplevels(eff)
sta <- droplevels(sta)

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
sta$GRTS.Cell.ID
i = 296805

#knitr::include_graphics(c(paste("./Output/Maps/GRTSID_",i,"_map.png",sep="")))

names(sta)

# Station (unique survey bat location) data
nrow(sta)
# sta %>% filter(GRTS.Cell.ID == i) # now only 113 stations

# modify the GIS covariates to suit the analysis
### Road type and distance
sta %>% count(Road.Type) # change road type to ordinal scale from 1 = no road to 8 = divided paved road
sta$Road.Type <- as.character(sta$Road.Type)

# if Road is within 100 m, keep as Road.Type.Ord but if >200 then change to 1, i.e., "No Road"
sta$Road.Type <- case_when(sta$Road.Distance > 200 ~ "No Road",
                            TRUE ~ as.character(sta$Road.Type))
# recode to ordinal scale, also changing NA to No Road as it meant no road within 1000 m
sta$Road.Type.Ord <- sta$Road.Type %>% replace_na("No Road") %>%
  recode("No Road"=1, "Driveway"=2,"Truck Trail"=2, "Unimproved Road"=3, "One Lane Gravel Road"=4,
         "Two Lane Gravel Road"=5, "Two Lane Undivided Paved Road"=6, "Divided Paved Road"=7)

### Water type and distance
sta %>% filter(is.na(Waterbody.Distance)) %>% count(Stream.Type)
# some streams are closer than waterbodies, perhaps change waterbody type to stream?
# yes - stream and water have similar features - consolidate into one called "Water" with the closest distance being the type/distance attributed
sta %>% filter(Stream.Distance<200 | Waterbody.Distance<200) %>% group_by(Stream.Type, Waterbody.Type) %>%
  select(Location.Name, Stream.Distance, Waterbody.Distance, Stream.Type, Waterbody.Type)

sta$Water.Distance <- with(sta, pmin(Stream.Distance, Waterbody.Distance, na.rm=T))
# to remove NAs, will change this to ordinal scale later as actually >1000 but unknown
sta$Water.Distance <- replace_na(sta$Water.Distance, 1001)

sta$Water.Type <- case_when(sta$Waterbody.Distance < sta$Stream.Distance ~ as.character(sta$Waterbody.Type),
                             TRUE ~ as.character(sta$Stream.Type))
as.data.frame(sta %>% group_by(Water.Type) %>% select(Waterbody.Type, Stream.Type, Waterbody.Distance, Stream.Distance, Water.Distance))


### Human Footprint type and distance
as.data.frame(sta %>% count(Human.Footprint.Type)) # consolidate to sublayer from ABMI HF 2014 metadata, similar as 2018 report
sta$Human.Footprint.Distance <- replace_na(sta$Human.Footprint.Distance, 1001)
sta$Human.Footprint.Sublayer <- ifelse(sta$Human.Footprint.Type %in%
                                          c("BORROWPIT-DRY","BORROWPITS","CONVENTIONAL-SEISMIC","GRVL-SAND-PIT","PIPELINE",
                                            "RIS-RECLAIMED-PERMANENT","RIS-SOIL-REPLACED","WELL-ABAND","WELL-OIL"), "Industrial",
                                        ifelse(sta$Human.Footprint.Type %in% c("CROP","CULTIVATION_ABANDONED","ROUGH_PASTURE", "TAME_PASTURE"), "Agriculture",
                                               ifelse(sta$Human.Footprint.Type %in% c("CLEARING-UNKNOWN", "GREENSPACE","RECREATION", "FACILITY-UNKNOWN"), "Greenspace" ,
                                                      ifelse(sta$Human.Footprint.Type %in% c("HARVEST-AREA"), "Forestry",
                                                             ifelse(sta$Human.Footprint.Type %in% c("TRAIL", "VEGETATED-EDGE-ROADS"), "Transportation",
                                                                    ifelse(sta$Human.Footprint.Type %in% c("RURAL-RESIDENCE", "URBAN-RESIDENCE"), "Residence", NA))))))

sta$Human.Footprint.Sublayer <- case_when(sta$Human.Footprint.Distance > 200 ~ "No Human Footprint",
                                           TRUE ~ as.character(sta$Human.Footprint.Sublayer))


sta.table <- sta %>%
  select(Location.Name, Natural.Region, Land.Use.Type, Human.Footprint.Sublayer, Human.Footprint.Type, Human.Footprint.Distance, Road.Type, Road.Distance, Water.Type, Water.Distance)
  colnames(sta.table) <- c("Station", "Original Name", "NP", "LUC", "# Yrs Srvyd", "Natural Region", "Land Use Type","HF Layer", "HF Type", "HF Dist", "Road HMSC", "Road Type", "Road Dist", "Water HMSC", "Water Type", "Water Dist")

###--- NABat required fields  
NABat_site_meta_template <- read.csv("Input/Bulk_Stationary_Acoustic_Meta_Template.csv")

NABat_site_call_template <- read.csv("Input/Bulk_Stationary_Acoustic_Data_Template.csv")
NABat_site_call_template[,1]

#-Appendix Table 1
## Stationary Point data sheet: Surveyor details, Site Locations, Recording Details, Detector Details
# for three sections: Nightly Temp (degree), Nightly Rel Hum (%); Wind speed (km/h)
## sta covariates: NR, LUT, HF + dist, Road + dist, Water + dist (for each station)
NABat_site_meta_template[,1]

names(sta)
Appendix.Table1 <- sta[c("GRTS.Cell.ID","Location.Name", "Latitude", "Longitude","Water.Distance","Water.Type","Road.Distance",
                        "Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                        "Land.Use.Type","Land.Unit.Code")] 

Appendix.Table1 <- left_join(Appendix.Table1, eff %>% select(Location.Name, Contact, Survey.Start.Time, Survey.End.Time))
# create 4 character Land Unit Code term - add second letter of first word to LUC with only 3 characters
levels(Appendix.Table1$Land.Unit.Code)
Appendix.Table1$Land.Unit.Code <- recode(Appendix.Table1$Land.Unit.Code, "BNP"="BANP", "JNP"="JANP","LPR"="LOPR",
                                         "NSR"="NOSR","RDR"="REDR","SSR"="SOSR","UAR"="UPAR","UPR"="UPPR")

# add in null columns (NABat template)
xx <- c("Detector","Detector.Serial.Number", "Microphone","Microphone.Serial.Number","Microphone.Orientation",
        "Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter","Weather.Proofing","Unusual.Occurrences")
Appendix.Table1[xx] <- NA

# put in the same order as NABat template
names(Appendix.Table1)
Appendix.Table1 <- Appendix.Table1[c("GRTS.Cell.ID","Location.Name", "Latitude", "Longitude","Survey.Start.Time", 
                                     "Survey.End.Time", "Detector", "Detector.Serial.Number", "Microphone","Microphone.Serial.Number",
                                     "Microphone.Orientation","Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter",
                                     "Water.Distance","Water.Type","Road.Distance","Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                                     "Land.Use.Type","Land.Unit.Code","Contact","Weather.Proofing","Unusual.Occurrences")]

opts <- options(knitr.kable.NA = "")
knitr::kable(t(Appendix.Table1 %>% filter(GRTS.Cell.ID==i) %>% select(-GRTS.Cell.ID)),
             caption = "Table 1 - NABat Site Metadata",
             digits=0)
options(opts)

#- Appendix Table 2
## Environmental data sheet with rows as dates and columns as Max, Min, Mean
