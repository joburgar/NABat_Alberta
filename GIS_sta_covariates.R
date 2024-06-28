###--- this script generates distance to feature values and type of value for station covariates

# Load Packages
list.of.packages <- c("tidyverse", "sf", "ggspatial","nngeo","units","Cairo")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

se <- function(x) sqrt(var(x)/length(x))
#############################################################################################
# Read station covariates csv
# setwd("//Volumes/LaCie/NABat_Alberta/Input/")
sta <- read.csv("Input/NABat_Station_Covariates.csv", header=T, na.string=c("","NA", "<NA>",-1))
sta %>% as_tibble
tail(sta)

# eff <- read.csv("Input/NABat_Deployment_Data_2023.csv")
# eff %>% as_tibble
# num.yrs.survey <- eff %>% group_by(GRTS.Cell.ID,Location.Name, Orig.Name) %>% count(Deployment.ID)
# num.yrs.survey <- num.yrs.survey %>% pivot_wider(names_from = Deployment.ID, values_from=n, values_fill = 0)
# num.yrs.survey <- num.yrs.survey[c("GRTS.Cell.ID","Location.Name", "Orig.Name","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")]
# 
# num.yrs.survey[rowSums(num.yrs.survey[,4:12] > 1) > 0, ] # 2020 has duplicates because deployed twice at same site so not true duplicates
# num.yrs.survey$`2020` <- ifelse(num.yrs.survey$`2020` > 0,1,0)
names(sta)
sta$num.years <-rowSums(sta[18:27])
sta %>% ungroup() %>% summarise(min(num.years), mean(num.years),max(num.years), se(num.years))
# min(num.years)` `mean(num.years)` `max(num.years)` `se(num.years)`
#             1              2                9          0.091
yrs.srvyed <- sta %>% ungroup() %>% count(num.years)
yrs.srvyed %>% filter(num.years > 4) %>% summarise(sum(n))
sta %>% summarise(sum(num.years))
count(sta)
sta %>% filter(num.years > 4)
# num.years     n
# 1         1 284
# 2         2  28
# 3         3  24
# 4         4  32
# 5         5  12
# 6         6  17
# 7         7   5
# 8         8   3
# 9         9   7

# GRTS.yrs.survey <- eff %>% group_by(GRTS.Cell.ID) %>% count(Deployment.ID)
# GRTS.yrs.survey <- GRTS.yrs.survey %>% pivot_wider(names_from = Deployment.ID, values_from=n, values_fill = 0)
# GRTS.yrs.survey <- GRTS.yrs.survey[c("GRTS.Cell.ID","2014","2015","2016","2017","2018","2019","2020","2021","2022")]
# 
# GRTS.yrs.survey$`2014` <- ifelse(GRTS.yrs.survey$`2014` > 0,1,0)
# GRTS.yrs.survey$`2015` <- ifelse(GRTS.yrs.survey$`2015` > 0,1,0)
# GRTS.yrs.survey$`2016` <- ifelse(GRTS.yrs.survey$`2016` > 0,1,0)
# GRTS.yrs.survey$`2017` <- ifelse(GRTS.yrs.survey$`2017` > 0,1,0)
# GRTS.yrs.survey$`2018` <- ifelse(GRTS.yrs.survey$`2018` > 0,1,0)
# GRTS.yrs.survey$`2019` <- ifelse(GRTS.yrs.survey$`2019` > 0,1,0)
# GRTS.yrs.survey$`2020` <- ifelse(GRTS.yrs.survey$`2020` > 0,1,0)
# GRTS.yrs.survey$`2021` <- ifelse(GRTS.yrs.survey$`2021` > 0,1,0)
# GRTS.yrs.survey$`2022` <- ifelse(GRTS.yrs.survey$`2022` > 0,1,0)
# GRTS.yrs.survey$`2023` <- ifelse(GRTS.yrs.survey$`2023` > 0,1,0)

###########
# na.sta <- sta_sf %>% filter(is.na(`2014`))
# eff %>% filter(Location.Name %in% na.sta$LocName)

# sta <- left_join(sta, num.yrs.survey, by=c("LocName"= "Location.Name"))

sta$NP <- as.factor(ifelse(sta$LandUnitCo %in% c("BNP", "JNP", "WBNP", "WLNP", "EINP"), "In", "Out")) %>% relevel(ref="In")
sta %>% as_tibble
# convert sta to spatial layer, in latitude / longitude (crs = 4326)
tail(sta)

# just run for sites you still need
sta.needs <- sta %>% filter(is.na(NSRNAME))
sta_sf <- st_as_sf(sta.needs, coords = c("Longitude","Latitude"), crs = 4326) 
sta_sf <- st_as_sf(sta, coords = c("Longitude","Latitude"), crs = 4326) 
sta_sf <- st_transform(sta_sf, crs=3400) # convert to NAD83 / Alberta 10-TM (Forest) for consistency with Alberta layers and metre unit

# quick plotting check
ggplot()+
  geom_sf(data = sta_sf) 

nrow(sta_sf) # 412
nrow(sta_sf %>% filter(X2023==1)) #124 sites surveyed in 2023 that need sta data
GRTS.sryvd.2023 <- sta_sf %>% filter(X2023==1) %>% count(GRTSCellID) %>% st_drop_geometry()# 75 GRTS cells surveyed
GRTS.sryvd.2023 %>% summarise(mean(n), min(n), max(n), se(n))
# mean(n) min(n) max(n)      se(n)
# 1.675676      1      4 0.1020125 # sites surveyed within a GRTS cell in 2023
GRTS.sryvd.2023 %>% count(n)
# n nn
# 1 39
# 2 25
# 3  5
# 4  5


sta_sf %>% filter(X2023==1) %>% summarise(min(num.years), mean(num.years), max(num.years), se(num.years)) %>% st_drop_geometry()
# min(num.years) mean(num.years) max(num.years) se(num.years)
# 1        2.774194              9     0.2160122

GRTS.num.years.2023 <- sta_sf %>% filter(X2023==1) %>% group_by(GRTS.Cell.ID) %>% count(num.years) %>% st_drop_geometry()
GRTS.num.years.2023.sum <- GRTS.num.years.2023 %>% group_by(GRTS.Cell.ID) %>% arrange(desc(num.years)) %>% filter(row_number()==1)
GRTS.num.years.2023.sum %>% ungroup() %>% summarise(min(num.years), max(num.years), mean(num.years), se(num.years))
# `min(num.years)` `max(num.years)` `mean(num.years)` `se(num.years)`
#               1                9              2.54          0.258
GRTS.num.years.2023.sum %>% ungroup() %>% count(num.years)
# num.years     n
#          1    42
#          2     3
#          3     6
#          4    14
#          5     2
#          6     1
#          7     1
#          8     2
#          9     3

# create a distance matrix
sta_2023 <- sta_sf %>% filter(X2023==1) %>% st_transform(3400)
dist.all <- st_distance(sta_2023, sta_2023, by_element = FALSE) 
write.csv(dist.all, "DistanceMatrixTable_2023.csv", row.names = FALSE)

#############################################################################################
# Set GIS Dir for uploading GIS layers
GISDir <- c("/Volumes/LaCie_2TB/NABat/GIS")

# Read in Alberta GIS layers and determine distance to bat survey stations

# Landcover Polygons 2010 - ABMI, their source data: ABMI Remote Sensing Group 2013, based on the EOSD and NLWIS 2000 raster datasets and on hydrography and access GIS layers from the Government of Alberta. Update to 2010 based on ABMI Human Footprint dataset.
LC <- st_read(paste(GISDir,"/2010LanCoverShapeFiles", sep=""), layer="Lancover_Polygons_2010")
sta_sf <- st_join(sta_sf, LC %>% dplyr::select(LC_class), left=TRUE)
sta_sf %>% filter(is.na(Land.Cover))
tail(sta_sf)


sta_sf$Land.Cover <- as.factor(sta_sf$LC_class.y)
levels(sta_sf$Land.Cover)
sta_sf$Land.Cover <- sta_sf$LC_class.y %>% recode("20"="Water", "33"="Exposed Land", "34"="Developed", "50"="Shrubland", "110"="Grassland",
                                            "120"="Agriculture", "210"="Coniferous Forest", "220"="Broadleaf Forest", "230"="Mixed Forest")
glimpse(sta_sf)

# # Human Footprint 2018 - ABMI Human Footprint (gdb)
# st_layers("/Users/joburgar/Documents/NABat/GIS/HFI_2018_v1.gdb")
# HF_res <- st_read("/Users/joburgar/Documents/NABat/GIS/HFI_2018_v1.gdb",layer="o15_Residentials_HFI2018")

# Natural_Regions_Subregions_of_Alberta
NR <- st_read(paste(GISDir,"/Natural_Regions_Subregions_of_Alberta", sep=""), layer="Natural_Regions_Subregions_of_Alberta")
# NR.dist <- st_nn(sta_sf, NR %>% st_transform(crs=3400), k=1, returnDist = T) 
# names(NR)
sta_sf <- st_join(sta_sf %>% dplyr::select(-NSRNAME, -NRNAME), NR %>% dplyr::select(NSRNAME, NRNAME), left=TRUE)


# Land-Use Framework Regions
LU <- st_read(GISDir, layer="LUF_AB")
sta_sf <- st_join(sta_sf %>% dplyr::select(-LUF_NAME), LU %>% dplyr::select(LUF_NAME), left=TRUE)

# Water - AltaLis 
# Base Waterbody Polygon: Base Features, obtained from AltaLis, by Alberta Environment and Parks, GoA
# Base Stream and Flow Representation:  Base Features, obtained from AltaLis, by Alberta Environment and Parks, GoA
WB <- st_read(paste(GISDir,"/bf_hydrography_05-03-2018/", sep=""), layer="BaseWaterbodyPolygon")
WB.dist <- st_nn(sta_sf, WB %>% st_transform(crs=3400), k=1, returnDist = T) 

sta_sf$WB.dist <- unlist(WB.dist$dist)
sta_sf$WB_type <- unlist(WB.dist$nn)
sta_sf$WB_type <- WB$FEATURE_TY[match(sta_sf$WB_type,rownames(LC))]

# Access - AltaLis 
# Cutline
# CL <- st_read(paste(GISDir,"/Access", sep=""), layer="Cutline")
# CL.dist <- st_nn(sta_sf, CL %>% st_transform(crs=3400), k=1, returnDist = T) 
# 
# sta_sf$CL.dist <- unlist(CL.dist$dist)
# sta_sf$CL_date <- unlist(CL.dist$nn)
# sta_sf$CL_date <- CL$FEATURE__2[match(sta_sf$CL_date,rownames(CL))]

# Road
RD <- st_read(paste(GISDir,"/Access", sep=""), layer="RoaD")
RD.dist <- st_nn(sta_sf, RD %>% st_transform(crs=3400), k=1, returnDist = T) 

sta_sf$RD.dist <- unlist(RD.dist$dist)
sta_sf$RD_Type <- unlist(RD.dist$nn)
sta_sf$RD_Type <- RD$ROAD_CLASS[match(sta_sf$RD_Type,rownames(RD))]

write.csv (sta_sf %>% st_drop_geometry(), "NABat_Station_Covariates_newsites.csv", row.names = FALSE)

save.image("GIS.sta.covariates.RData")
#load("GIS.sta.covariates.RData")

# rm(WB,WS,LC,RD,CL)
coords <- st_coordinates(sta_sf %>% st_transform(4326))
sta_sf$Longitude <- coords[,1]
sta_sf$Latitude <- coords[,2]

write.csv (sta_sf %>% st_drop_geometry(), "NABat_Station_Covariates.csv", row.names = FALSE)
st_write(sta_sf %>% 
           dplyr::select(GRTS.Cell.ID, LocName, Orig_Name, X2014,X2015,X2016,X2017,X2018,X2019,X2020,X2021,X2022,X2023,num.years), "NABat_Station_Covariates.shp", delete_layer = TRUE)


ggplot()+
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% unique(GRTS.num.years.2022.sum$GRTS.Cell.ID)))

st_write(NABat_grid %>%
           filter(GRTS_ID %in% unique(GRTS.num.years.2022.sum$GRTS.Cell.ID)),"NABat_AB2022_grids.shp") 

st_write(NABat_grid %>%
           filter(GRTS_ID %in% unique(sta_sf$GRTS.Cell.ID)),"NABat_AB_grids.shp") 


# sta <- read.csv("NABat_Station_Covariates.csv")
# sta_sf$Surveyed2021 <- sta$Surveyed2021
###--- Create provincial map
NR.NRNAME <-NR %>% group_by(NRNAME) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
NR.NRNAME$area <- st_area(NR.NRNAME)

Alberta <-
  NR.NRNAME %>%
  summarise(area = sum(area))

###--- create map of GRTS and NABat stations
sta <- read.csv("Input/NABat_Station_Covariates.csv")
sta_sf <- st_as_sf(sta, coords = c("Longitude","Latitude"), crs = 4326)

sta_sf %>% filter(LandUnitCo=="WLNP")

prev.survey <- sta_sf %>% filter(X2023==0) %>% count(GRTS.Cell.ID) %>% st_drop_geometry()
this.survey <- sta_sf %>% filter(X2023==1) %>% count(GRTS.Cell.ID) %>% st_drop_geometry()


NABatDir = c("/Volumes/LaCie_2TB/NABat/GIS/")
NABat_grid <- read_sf(dsn = NABatDir,layer = "master_sample_Alberta")
NABat_grid <- st_transform(NABat_grid, crs=4326) # now espg 4326

Fig_provincial.plot <- ggplot() + 
  geom_sf(data = Alberta %>%st_transform(crs=4326)) +
  geom_sf(data = NR.NRNAME %>% st_transform(crs=4326), mapping=aes(fill=NRNAME), lwd=0) +
  scale_fill_manual(name = "Natural Regions",
                    values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% prev.survey$GRTS.Cell.ID), col="azure2", lwd=0.8) +
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% this.survey$GRTS.Cell.ID), col="black", lwd=1) +
  annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
  coord_sf() +
  theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))

Cairo(file="Fig_provincial.plot_2023.PNG",
      type="png",
      width=1500,
      height=2000,
      pointsize=14,
      bg="white",
      dpi=300)
Fig_provincial.plot
dev.off()

Fig_provincial_yrssrvyd.plot <- ggplot() + 
  geom_sf(data = Alberta %>%st_transform(crs=4326)) +
  geom_sf(data = NR.NRNAME %>% st_transform(crs=4326), mapping=aes(fill=NRNAME), lwd=0) +
  scale_fill_manual(name = "Natural Regions",
                    values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
  geom_sf(data = sta_sf, aes(col=num.years)) +
  annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
  coord_sf() +
  theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))

Cairo(file="Fig_provincial.plot.yrssryvd_2023.PNG",
      type="png",
      width=1500,
      height=2000,
      pointsize=14,
      bg="white",
      dpi=300)
Fig_provincial_yrssrvyd.plot
dev.off()

grts.year <- sta_sf %>% filter(X2023==1)
plot.yr <- 2023

Fig_provincial_year <- ggplot() + 
  geom_sf(data = Alberta %>%st_transform(crs=4326)) +
  geom_sf(data = NR.NRNAME %>% st_transform(crs=4326), mapping=aes(fill=NRNAME), lwd=0) +
  scale_fill_manual(name = "Natural Regions",
                    values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% grts.year$GRTS.Cell.ID), col="black", lwd=0.8) +
  annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
  ggtitle(paste0("NABat Grid Cells Surveyed in ",plot.yr))+
  coord_sf() +
  theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))

Cairo(file=paste0("Fig_provincial_",plot.yr,".PNG"),
      type="png",width=1500,height=2000,pointsize=14,bg="white",dpi=300)
Fig_provincial_year
dev.off()

####---
sta_sf %>% filter(LandUnitCo=="WLNP") %>% filter(NABat_Samp=="Yes")


ggplot() + 
  geom_sf(data = Alberta %>%st_transform(crs=4326)) +
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% grts.year$GRTS.Cell.ID), col="black", lwd=0.8) +
  annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
  ggtitle(paste0("NABat Grid Cells Surveyed in ",plot.yr))+
  coord_sf() +
  theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))

#####--- Create distance matrix
# library(units)
# sta_sf <- st_transform(sta_sf, crs=3400) # convert to NAD83 / Alberta 10-TM (Forest) for consistency with Alberta layers and metre unit

sta_2023 <- sta_sf %>% filter(X2023==1) %>% st_transform(crs=3400)
nrow(sta_2023)
nrow(sta_sf)

ggplot()+
  geom_sf(data=sta_2023)

stn_dist <- as.data.frame(st_distance(sta_2023, sta_2023, by_element = FALSE))
# library(units)
stn_dist <- drop_units(stn_dist)
head(stn_dist)
stn_dist[stn_dist==0]<- NA
write.csv(stn_dist, "DistanceMatrixTable_2023.csv")

save.image("GIS.sta.covariates.RData")
#load("GIS.sta.covariates.RData")

#####--- Find mobile cells and covariates
mobile_meta <- read.csv("Input/NABat_Deployment_Data_DT_2023.csv", header=T)
glimpse(mobile_meta)
summary(mobile_meta)
mobile_sf <- st_as_sf(mobile_meta, coords = c("Longitude","Latitude"), crs = 4326)
mobile_sf <- mobile_sf %>% st_transform(crs=3400)

ggplot()+
  geom_sf(data=mobile_sf)

# Natural_Regions_Subregions_of_Alberta
NR <- st_read(paste(GISDir,"/Natural_Regions_Subregions_of_Alberta", sep=""), layer="Natural_Regions_Subregions_of_Alberta")
NR.dist <- st_nn(mobile_sf, NR %>% st_transform(crs=3400), k=1, returnDist = T) 
mobile_sf <- st_join(mobile_sf, NR %>% select(NSRNAME, NRNAME), left=TRUE)

# Land-Use Framework Regions
# LU <- st_read(GISDir, layer="LUF_AB") %>% st_transform(crs=3400)
# mobile_sf <- st_join(mobile_sf, LU %>% select(LUF_NAME), left=TRUE)
# mobile_sf$LUF_NAME
# mobile_meta$Land.Unit.Code[1:9] <- "LOAR"
# mobile_meta$Land.Unit.Code[11:12] <- "UPAR"

mobile_sf <- st_join(mobile_sf %>% st_transform(crs=4326), NABat_grid %>% select(GRTS_ID), left=TRUE)
mobile_sf$GRTS.Cell.ID <- mobile_sf$GRTS_ID
mobile_sf$Location.Name <- paste0(mobile_sf$GRTS.Cell.ID,"_DT")

mobile_sf$Latitude <- mobile_meta$Latitude
mobile_sf$Longitude <- mobile_meta$Longitude
# write.csv(mobile_sf %>% st_drop_geometry(), "Input/NABat_Deployment_Data_DT_2021_complete.csv")


mobile_survey_per_GRTS <- mobile_sf %>% filter(SurveyNum!=4) %>% count(GRTS.Cell.ID) %>% st_drop_geometry()
# mobile_survey_per_GRTS <- mobile_meta %>% count(GRTS.Cell.ID)
NABat_grid <- NABat_grid %>% st_transform(crs=4326)

# Figure for report 
Fig_provincial.plot_mobile <- ggplot() + 
  geom_sf(data = Alberta %>%st_transform(crs=4326)) +
  geom_sf(data = NR.NRNAME %>% st_transform(crs=4326), mapping=aes(fill=NRNAME), lwd=0) +
  scale_fill_manual(name = "Natural Regions",
                    values=c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333"))+
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% mobile_survey_per_GRTS[mobile_survey_per_GRTS$n==1,]$GRTS.Cell.ID), col="blue", lwd=0.8) +
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% mobile_survey_per_GRTS[mobile_survey_per_GRTS$n==2,]$GRTS.Cell.ID), col="black", lwd=0.8) +
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% mobile_survey_per_GRTS[mobile_survey_per_GRTS$n==3,]$GRTS.Cell.ID), col="red", lwd=0.8) +
  geom_sf(data = NABat_grid %>% filter(GRTS_ID %in% mobile_survey_per_GRTS[mobile_survey_per_GRTS$n==4,]$GRTS.Cell.ID), col= "white", lwd=0.8) +
  annotation_scale(location = "bl",bar_cols = c("grey", "white")) +
  coord_sf() +
  theme(axis.text.x = element_text(size=5), axis.text.y =element_text(size=5))

Cairo(file="Fig_provincial.plot_2023_mobile.PNG",
      type="png",
      width=1500,
      height=2000,
      pointsize=14,
      bg="white",
      dpi=300)
Fig_provincial.plot_mobile
dev.off()

