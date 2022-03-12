###--- this script generates individual annual GRTS specific appendices
# to be used with NABat_Annual_Report_Appendices.Rmd, read in once bulk of report code has been run
# or just load in the "NABat_Annual_Submission.RDS" rather than run report 

#Load Packages
list.of.packages <- c("data.table", "leaflet", "tidyverse", "lunar", "zoo", "colortools", "lubridate", "camtrapR", "circular", "RColorBrewer", "Cairo", "viridis", "knitr", "sf","osmdata", "ggspatial", "ggmap","gridExtra", "grid", "weathercan", "data.table")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# save.image("NABat_Annual_Submission.RDS")
# load("NABat_Annual_Submission.RDS")

# # Define the GRTS.Cell.ID and Year of interest if subsetting for year, studyarea
# GRTS_interest <- "922"
# Year_interest <- year(as.Date("2015-01-01"))
# 
# load(paste("NABat_Annual_Report_",Year_interest,".RDS", sep=""))


#############################################################################

dat_count <- droplevels(dat_count)
dat_summary <- droplevels(dat_summary)
dat_time <- droplevels(dat_time)
eff <- droplevels(eff)
sta <- droplevels(sta)
names(eff)

call_count.Sp <- call_count %>% group_by(Classification, Deployment.ID, Volancy)
call_count.Sp <- droplevels(call_count.Sp)
call_count.Sp$GRTS.Cell.ID <- as.character(call_count.Sp$GRTS.Cell.ID)

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

# Station (unique survey bat location) data
# nrow(sta)
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
# sta %>% filter(is.na(Waterbody.Distance)) %>% count(Stream.Type)
# some streams are closer than waterbodies, perhaps change waterbody type to stream?
# yes - stream and water have similar features - consolidate into one called "Water" with the closest distance being the type/distance attributed
# sta %>% filter(Stream.Distance<200 | Waterbody.Distance<200) %>% group_by(Stream.Type, Waterbody.Type) %>%
# select(Location.Name, Stream.Distance, Waterbody.Distance, Stream.Type, Waterbody.Type)

# sta$Water.Distance <- with(sta, pmin(Stream.Distance, Waterbody.Distance, na.rm=T))
# to remove NAs, will change this to ordinal scale later as actually >1000 but unknown
sta$Water.Distance <- sta$Waterbody.Distance
sta$Water.Distance <- replace_na(sta$Water.Distance, 1001)

# sta$Water.Type <- case_when(sta$Waterbody.Distance < sta$Stream.Distance ~ as.character(sta$Waterbody.Type),
#                             TRUE ~ as.character(sta$Stream.Type))
# as.data.frame(sta %>% group_by(Water.Type) %>% select(Waterbody.Type, Stream.Type, Waterbody.Distance, Stream.Distance, Water.Distance))

sta$Water.Type <- sta$Waterbody.Type
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

# ###--- NABat required fields  
# NABat_site_meta_template <- read.csv("Input/Bulk_Stationary_Acoustic_Meta_Template.csv")
# 
# NABat_site_call_template <- read.csv("Input/Bulk_Stationary_Acoustic_Data_Template.csv")

#-Appendix Table 1
## Stationary Point data sheet: Surveyor details, Site Locations, Recording Details, Detector Details
# for three sections: Nightly Temp (degree), Nightly Rel Hum (%); Wind speed (km/h)
## sta covariates: NR, LUT, HF + dist, Road + dist, Water + dist (for each station)
# NABat_site_meta_template[,1]

Appendix.Table1 <- sta[c("GRTS.Cell.ID","Location.Name", "Orig.Name","Latitude", "Longitude","Water.Distance","Water.Type","Road.Distance",
                         "Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                         "Land.Use.Type","Land.Unit.Code")] 
names(sta)
nrow(Appendix.Table1)
Appendix.Table1 <- left_join(Appendix.Table1, eff %>% select(Location.Name, Contact, Survey.Start.Time, Survey.End.Time, Deployment.ID))
Appendix.Table1 <- Appendix.Table1 %>% 
  # filter(Deployment.ID==Year_interest) %>% 
  dplyr::select(-Deployment.ID)
# add in null columns (NABat template)
xx <- c("Detector","Detector.Serial.Number", "Microphone","Microphone.Serial.Number","Microphone.Orientation",
        "Microphone.Height","Clutter.Distance","Clutter.Type","Percent.Clutter","Weather.Proofing","Unusual.Occurrences")
Appendix.Table1[xx] <- as.character(NA)

# put in the same order as NABat template
Appendix.Table1 <- Appendix.Table1[c("GRTS.Cell.ID","Location.Name", "Orig.Name","Latitude", "Longitude","Survey.Start.Time", 
                                     "Survey.End.Time", "Detector", "Detector.Serial.Number", "Microphone","Microphone.Serial.Number",
                                     "Microphone.Orientation","Microphone.Height","Clutter.Distance","Clutter.Type",
                                     "Water.Distance","Water.Type","Percent.Clutter","Road.Distance","Road.Type", "Human.Footprint.Distance", "Human.Footprint.Sublayer", "Human.Footprint.Type",
                                     "Land.Use.Type","Land.Unit.Code","Contact","Weather.Proofing","Unusual.Occurrences")]
# unique(Appendix.Table1$GRTS.Cell.ID)
# i="296805"
# opts <- options(knitr.kable.NA = "")
# knitr::kable(t(Appendix.Table1 %>% filter(GRTS.Cell.ID==i) %>% select(-GRTS.Cell.ID)),
#              caption = "Table 1 - NABat Site Metadata",
#              digits=0)
# options(opts)

# split the NABat call template into two: Appendix 2 = weather, Appendix 3 = nightly call counts
#NABat_site_call_template[,1]

#- Appendix Table 2

## Environmental data sheet with rows as dates and columns as Max, Min
# Create environmental covariate
# load weather data
# library(weathercan) # if not already loaded
Year_interest <- c(2021)
weather <- vector('list', seq_along(Year_interest))
for(i in 1:length(Year_interest)){
  weather_list <- read.csv(paste0("Input/NABat_",Year_interest[i],"_nightly_weather_sum.csv"), header=T)
  weather[[i]] <- weather_list
}
glimpse(weather)
ßweather <- rbindlist(weather, use.names = TRUE, fill=TRUE)
weather <- weather %>% select(-X)

ECCC.stn <- sta[c("Latitude","Longitude")]

ECCC.stn.id <- vector('list',nrow(ECCC.stn))
for (i in 1:nrow(ECCC.stn)){
  ECCC.stn.id.list <- stations_search(coords = ECCC.stn[i,],
                                      interval = "hour", starts_latest=Year_interest[1], ends_earliest=Year_interest[6]+1,
                                      quiet = T)
  ECCC.stn.id[i] <- ECCC.stn.id.list[1,c("station_id")]
}

sta$ECCC_stn_id <- unlist(ECCC.stn.id)

head(weather)
colnames(weather) <- c("ECCC.stn.id", "SurveyNight", "Min.Tmp", "Min.RH", "Min.WS", "Mean.Tmp", "Mean.RH", "Mean.WS", "Max.Tmp", "Max.RH", "Max.WS")
weather$SurveyNight <- ymd(weather$SurveyNight)
nightly.env.cov <- dat_summary[c("Location.Name","SurveyNight")]
nightly.env.cov$ECCC.stn.id <- sta$ECCC_stn_id[match(nightly.env.cov$Location.Name, sta$Location.Name)]
nightly.env.cov <- nightly.env.cov %>% count(Location.Name, SurveyNight, ECCC.stn.id)
nightly.env.cov <- nightly.env.cov %>% select(-n)

nightly.env.cov <- left_join(nightly.env.cov, weather, by = c("SurveyNight","ECCC.stn.id"))


Appendix.Table2 <- nightly.env.cov[c("Location.Name","SurveyNight","Min.Tmp","Max.Tmp","Min.RH","Max.RH","Min.WS","Max.WS")] #%>% ungroup
# opts <- options(knitr.kable.NA = "")
# knitr::kable(Appendix.Table2 %>% filter(grepl(i, Location.Name)) %>% select(-Location.Name),
#              caption = paste("Table 2 - NABat Survey Weather Conditions for GRTS Cell",i),
#              digits=1)
# options(opts)
# Weather for appendices
# GRTS.Cell.ID <- unique.GRTS.Cell.ID
# write.table(Appendix.Table2 %>% filter(grepl(GRTS.Cell.ID, Location.Name)), paste("GRTS_",GRTS.Cell.ID,"_Appendix.Table2.csv", sep=""),na = "",row.names = FALSE,sep = ",")




###--- NABat Grid and Station maps
# to be used with NABat_Annual_Report.Rmd

###--- create map of GRTS and NABat stations
NABatDir = c("/Users/joburgar/Documents/NABat/GIS/")

NABat_grid <- read_sf(dsn = NABatDir,layer = "master_sample_Alberta")
#st_geometry(NABat_grid) # no ESPG
NABat_grid <- st_transform(NABat_grid, crs=4326) # now espg 4326
#NABat_grid

###--- create sf object of sta (Alberta NABat sites)
NABat.sf <- st_as_sf(sta, coords=c("Longitude", "Latitude"), crs = 4326)
#NABat_grid <- NABat_grid %>% filter(GRTS_ID %in% sta$GRTS.Cell.ID)

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
AB.NABat_3857 <- st_transform(NABat.sf, 3857)
AB.NABat_3857_coords <- as.data.frame(st_coordinates(AB.NABat_3857))
AB.NABat_3857_coords$GRTS.Cell.ID <- AB.NABat_3857$GRTS.Cell.ID
AB.NABat_3857_coords$Location.Name <- AB.NABat_3857$Location.Name

NABat_grid_3857 <- st_transform(NABat_grid %>% filter(GRTS_ID %in% sta$GRTS.Cell.ID), 3857)

# NABat stations within NABat grid cells - looks like it's loading correctly
ggplot()+
  geom_sf(data = NABat_grid) +
  geom_sf(data = NABat.sf, pch=19) +
  geom_sf(data=NABat_grid_3857, col="blue")+
  scale_shape_identity()+
  coord_sf()

# read.table("google_key.txt")
# register_google(key = google_key)


# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}


GRTS.plot <- function(GRTS.Cell.ID = GRTS.Cell.ID, v.just=1.2, h.just=1) {
  
  AB.GRTS_bb <- as.vector(st_bbox(NABat_grid_3857 %>% filter(GRTS_ID==GRTS.Cell.ID)))
  
  left <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[1])
  bottom <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[2])
  right <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[3])
  top <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[4])
  
  GRTS_map <- get_map(c(left = left ,
                        bottom = bottom,
                        right = right,
                        top =  top),
                      color = "color")
  
  # Use the function in a loop to create all GRTS grid/station maps
  map_GRTS <- ggmap_bbox(GRTS_map)
  
  GRTS.plot <- ggmap(map_GRTS) + 
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = NABat_grid_3857 %>% filter(GRTS_ID==GRTS.Cell.ID), col="grey14", fill = NA, inherit.aes = FALSE) +
    geom_sf(data = AB.NABat_3857 %>% filter(GRTS.Cell.ID==GRTS.Cell.ID), col="darkred", fill="red", cex = 3, inherit.aes = FALSE) +
    geom_text(data=AB.NABat_3857_coords %>% filter(GRTS.Cell.ID==GRTS.Cell.ID),
              aes(x=X ,y=Y, label=Location.Name, fontface="bold"), vjust=-v.just, size=4, hjust=h.just) +
    theme(legend.position = "none", text=element_text(size=13)) +
    theme(axis.text.y=element_blank(),axis.text.x=element_blank(),
          axis.ticks=element_blank()) +
    labs(y = "", x = "")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=3))
  
  return(GRTS.plot)
  
}



# for(i in slices){
# GRTS_map <- GRTS.plot(GRTS.Cell.ID = GRTS.Cell.ID, v.just=-2, h.just=-0.1)
#   ggsave(file=paste("Output/Maps/GRTSID_",i,"_map.png",sep=""))

######################################
#- overall AB map
theme_set(theme_bw())
nr <- read_sf(dsn = paste(NABatDir,"/Natural_Regions_Subregions_of_Alberta", sep=""),
              layer = "Natural_Regions_Subregions_of_Alberta")

NR.NRNAME <-NR %>% group_by(NRNAME) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))


nr$area <- st_area(nr)

Alberta <-
  nr %>%
  summarise(area = sum(area))

unique(nr$NRNAME)
RM <-nr %>%
  filter(NRNAME =="Rocky Mountain") %>%
  summarise(area = sum(area))
RM$NR <- "Rocky Mountain"

BO <-nr %>%
  filter(NRNAME =="Boreal") %>%
  summarise(area = sum(area))
BO$NR <- "Boreal"

PA <-nr %>%
  filter(NRNAME =="Parkland") %>%
  summarise(area = sum(area))
PA$NR <- "Parkland"

GA <-nr %>%
  filter(NRNAME =="Grassland") %>%
  summarise(area = sum(area))
GA$NR <- "Grassland"

CS <-nr %>%
  filter(NRNAME =="Canadian Shield") %>%
  summarise(area = sum(area))
CS$NR <- "Canadian Shield"

FH <-nr %>%
  filter(NRNAME =="Foothills") %>%
  summarise(area = sum(area))
FH$NR <- "Foothills"

NR <- rbind(RM, BO, PA, GA, CS, FH)

rm(nr)
# Two.DT <- unique(eff %>% filter(grepl("churtado|EINP", Contact)) %>% select(GRTS.Cell.ID))
# One.DT <-   unique(eff %>% filter(!grepl("churtado|EINP", Contact)) %>% select(GRTS.Cell.ID))
# 
# NABat_DT1_3857 <- st_transform(NABat_grid %>% filter(GRTS_ID %in% One.DT$GRTS.Cell.ID), 3857)
# NABat_DT2_3857 <- st_transform(NABat_grid %>% filter(GRTS_ID %in% Two.DT$GRTS.Cell.ID), 3857)
# 
# 
# Fig_mobile.plot <- ggplot() +
#   geom_sf(data = Alberta) +
#   geom_sf(data = NR, mapping = aes(fill=NR), lwd=0) +
#   scale_fill_manual(name = "Natural Regions",
#                     values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
#   geom_sf(data = NABat_DT1_3857, col="blue", lwd=1) +
#   geom_sf(data = NABat_DT2_3857, col="black", lwd=1) +
#   annotation_scale(location = "bl", width_hint = 0.4, bar_cols = c("grey", "white")) +
#   coord_sf() +
#   theme_minimal()
# 
# Cairo(file="Fig_mobile.plot.PNG", 
#       type="png",
#       width=1500, 
#       height=2000, 
#       pointsize=14,
#       bg="white",
#       dpi=300)
# Fig_mobile.plot
# dev.off()

# for(i in slices){
#  inset.plot <- ggplot() + 
#     geom_sf(data = Alberta) +
#     geom_sf(data = NR, mapping = aes(fill=NR), lwd=0) + 
#     scale_fill_manual(name = "Natural Regions",
#                       values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
#     geom_sf(data = NABat_grid, col="azure2", lwd=0.8) +
#     geom_sf(data = NABat_grid %>% filter(GRTS_ID==i), col="black", lwd=1) +
#     #annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
#     coord_sf() +
#     theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))+
#     theme(legend.position = "none")
#   ggsave(file=paste("Output/Maps/Inset_GRTSID_",i,"_map.png",sep=""))
# }
# 
# 
# 
# grid.newpage()
# vpb_ <- viewport(width = 1, height = 1, x = 0.4, y = 0.5)  # the larger map
# vpa_ <- viewport(width = 0.5, height = 0.4, x = 0.85, y = 0.8)  # the inset in upper right
# 
# for(i in slices){
#   grid.newpage()
#   vpb_ <- viewport(width = 1, height = 1, x = 0.4, y = 0.5)  # the larger map
#   vpa_ <- viewport(width = 0.5, height = 0.4, x = 0.85, y = 0.8)  # the inset in upper right
#   print(GRTS.plot(GRTS.Cell.ID = i, h.just=0),
#       vp = vpb_)
#   print(ggplot() +
#         geom_sf(data = Alberta) +
#         geom_sf(data = NR, mapping = aes(fill=NR), lwd=0) +
#         scale_fill_manual(name = "Natural Regions",
#                           values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
#         geom_sf(data = NABat_grid, col="azure2", lwd=0.8) +
#         geom_sf(data = NABat_grid %>% filter(GRTS_ID==i), col="black", lwd=1) +
#         #annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
#         coord_sf() +
#         theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))+
#         theme(legend.position = "none"),
#       vp = vpa_)
#   ggsave(file=paste("Output/Maps/Combined_GRTSID_",i,"_map.png",sep=""))
#   #dev.off()
# }
