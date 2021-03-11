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


# sta.table <- sta %>%
#   select(Location.Name, Natural.Region, Land.Use.Type, Human.Footprint.Sublayer, Human.Footprint.Type, Human.Footprint.Distance, Road.Type, Road.Distance, Water.Type, Water.Distance)
# colnames(sta.table) <- c("Station", "Natural Region", "Land Use Type","HF Layer", "HF Type", "HF Dist", "Road Type", "Road Dist",  "Water Type", "Water Dist")

###--- NABat required fields  
NABat_site_meta_template <- read.csv("Input/Bulk_Stationary_Acoustic_Meta_Template.csv")

NABat_site_call_template <- read.csv("Input/Bulk_Stationary_Acoustic_Data_Template.csv")

#-Appendix Table 1
## Stationary Point data sheet: Surveyor details, Site Locations, Recording Details, Detector Details
# for three sections: Nightly Temp (degree), Nightly Rel Hum (%); Wind speed (km/h)
## sta covariates: NR, LUT, HF + dist, Road + dist, Water + dist (for each station)
NABat_site_meta_template[,1]

names(sta)
Appendix.Table1 <- sta[c("GRTS.Cell.ID","Location.Name", "Orig.Name","Latitude", "Longitude","Water.Distance","Water.Type","Road.Distance",
                        "Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                        "Land.Use.Type","Land.Unit.Code")] 

Appendix.Table1 <- left_join(Appendix.Table1, eff %>% select(Location.Name, Contact, Survey.Start.Time, Survey.End.Time))

# add in null columns (NABat template)
xx <- c("Detector","Detector.Serial.Number", "Microphone","Microphone.Serial.Number","Microphone.Orientation",
        "Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter","Weather.Proofing","Unusual.Occurrences")
Appendix.Table1[xx] <- NA

# put in the same order as NABat template
names(Appendix.Table1)
Appendix.Table1 <- Appendix.Table1[c("GRTS.Cell.ID","Location.Name", "Orig.Name","Latitude", "Longitude","Survey.Start.Time", 
                                     "Survey.End.Time", "Detector", "Detector.Serial.Number", "Microphone","Microphone.Serial.Number",
                                     "Microphone.Orientation","Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter",
                                     "Water.Distance","Water.Type","Road.Distance","Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                                     "Land.Use.Type","Land.Unit.Code","Contact","Weather.Proofing","Unusual.Occurrences")]

opts <- options(knitr.kable.NA = "")
knitr::kable(t(Appendix.Table1 %>% filter(GRTS.Cell.ID==i) %>% select(-GRTS.Cell.ID)),
             caption = "Table 1 - NABat Site Metadata",
             digits=0)
options(opts)

# split the NABat call template into two: Appendix 2 = weather, Appendix 3 = nightly call counts
NABat_site_call_template[,1]

#- Appendix Table 2

## Environmental data sheet with rows as dates and columns as Max, Min
# Create environmental covariate
nightly.env.cov <- dat_summary %>% group_by(Location.Name,SurveyNight) %>% dplyr::summarise_at(c("max_temp","max_hum","max_wind","min_temp","min_hum","min_wind"), list(Mean = mean))
nightly.env.cov <- nightly.env.cov %>% rename(Max.Temp = "max_temp_Mean", Max.Hum = "max_hum_Mean", Max.Wind = "max_wind_Mean", 
                                              Min.Temp = "min_temp_Mean", Min.Hum = "min_hum_Mean", Min.Wind = "min_wind_Mean")

Appendix.Table2 <- nightly.env.cov[c("Location.Name","SurveyNight","Min.Temp","Max.Temp","Min.Hum","Max.Hum","Min.Wind","Max.Wind")] %>% ungroup
opts <- options(knitr.kable.NA = "")
knitr::kable(Appendix.Table2 %>% filter(grepl(i, Location.Name)) %>% select(-Location.Name),
             caption = paste("Table 2 - NABat Survey Weather Conditions for GRTS Cell",i),
             digits=1)
options(opts)


#- Appendix Figure 1
# only graph species / species groups with >100 calls
sp.to.use <- call_count.Sp %>% group_by(GRTS.Cell.ID, Classification) %>% summarise(Sum = sum(Count)) %>% filter(Sum>100)

# group data for overall mean nightly bat calls by year with volancy
app.Calls <- call_count.Sp%>% group_by(GRTS.Cell.ID, Classification, Deployment.ID) %>% summarise(Mean = mean(Count), SE = se(Count))

app.calls.Sp <- app.Calls %>% filter(GRTS.Cell.ID==i) %>%
  filter(Classification %in% sp.to.use$Classification) %>%
  ggplot(aes(x = Classification, y = Mean, fill="GRTS.Cell.ID"))+
  geom_point(colour="white", shape=21, size=4)+
  scale_fill_manual(values=unique(col.catVol[1])) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Mean ± 1 SE Nightly Bat Calls"))) +
  geom_linerange(aes(Classification, ymin = Mean-SE, ymax = Mean+SE)) +
  theme_classic()+
  theme(axis.text.y = element_text(size=12), axis.title.x = element_blank())+
  theme(axis.text.x = element_text(colour = "black", size = 10)) +
  theme(legend.position = "none", legend.title = element_blank())+
  facet_wrap(~Deployment.ID)
app.calls.Sp


# Figure 2

app.hist.calls <- ggplot(data = call_count.Sp %>% filter(GRTS.Cell.ID==i), aes(x = Classification, y = Count, fill= Location.Name)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Paired")+
  theme_classic() + xlab("Species / Species Group") + ylab("Total Bat Calls") + 
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +   theme(axis.title.x = element_blank())+
  facet_wrap(~Deployment.ID)
app.hist.calls


#- Appendix Table 3

## Nightly call counts
unique(dat_count$Classification)
tail(dat_count)
Appendix.Table3 <- dat_count %>% group_by(Location.Name, SurveyNight, Classification) %>% summarise(Call.Count = sum(Count))
glimpse(Appendix.Table3)
Appendix.Table3 <- Appendix.Table3 %>% pivot_wider(names_from = Classification, values_from = Call.Count)
names(Appendix.Table3)
Appendix.Table3 <- Appendix.Table3[,c("Location.Name","SurveyNight","EPFU", "EPFU-LANO", "LANO", "LACI", "LABO", "LABO-MYLU", "MYLU",
                                      "MYCA", "MYCI", "MYEV","MYEV-MYSE", "MYSE", "MYVO", "Myotis 40k",
                                      "unknown","noise")]
colnames(Appendix.Table3)[1:2] <- c("Station", "Date")

opts <- options(knitr.kable.NA = "")
knitr::kable(Appendix.Table3 %>% filter(grepl(i, Station)),
             caption = paste("Table 2 - NABat Nightly Call Counts for GRTS Cell",i),
             digits=1)
options(opts)

###---recode Classification to "AutoID" and use NABat 4-6 letter codes for NABat submission
#25k|40k|40kMyo|ANPA|ANPAEPFU|ANTPAL|ARJA|ARTJAM|BRACAV|BRCA|CHME|CHOMEX|CORA|CORRAF|CORTO|COTO|COTOVI|DIEC|DIPECA|
#EPFU|EPFULABO|EPFULANO|EPFUMYLU|EPTFUS|EUDMAC|EUFL|EUMA|EUMFLO|EUMPER|EUMUND|EUPE|EUUN|HiF|HighF|IDIPHY|IDPH|LABL|
#LABLPAHE|LABO|LABOLASE|LABOMYLU|LABONYHU|LABOPESU|LACI|LACILANO|LACITABR|LAEG|LAIN|LAMI|LANO|LANOTABR|LASBLO|LASBOR|
#LASCIN|LASE|LASEGA|LASINT|LASMIN|LASNOC|LASSEM|LASXAN|LAXA|LEMY|LENI|LEPNIV|LEPYER|LESP|LEYE|LUSO|LoF|LowF|MACA|MACCAL|
#MOLMOL|MOME|MOMO|MORMEG|MYAR|MYAU|MYCA|MYCAMYCI|MYCAMYYU|MYCI|MYCIMYVO|MYEV|MYEVMYTH|MYGR|MYKE|MYLE|MYLU|MYLUMYCI|
#MYLUMYSE|MYLUMYVO|MYOAUR|MYOAUS|MYOC|MYOCAL|MYOCIL|MYOEVO|MYOGRI|MYOKEE|MYOLEI|MYOLUC|MYOOCC|MYOSEP|MYOSOD|MYOTHY|MYOVEL|
#MYOVOL|MYOYUM|MYSE|MYSO|MYTH|MYVE|MYVO|MYYU|NOCLEP|NOISE|NOLE|NOTBAT|NYCFEM|NYCHUM|NYCMAC|NYFE|NYHU|NYMA|NYSP|NoID|PAHE|PARHES|PERSUB|PESU|STERUF|STRU|TABR|TADBRA|HiLo
