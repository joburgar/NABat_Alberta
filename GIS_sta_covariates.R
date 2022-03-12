###--- this script generates distance to feature values and type of value for station covariates

# Load Packages
list.of.packages <- c("tidyverse", "sf", "ggspatial","nngeo","units","Cairo")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#############################################################################################
# Read station covariates csv
sta <- read.csv("Input/NABat_Station_Covariates.csv", header=T, na.string=c("","NA", "<NA>",-1)) %>%
  dplyr::select(Land.Unit.Code, GRTS.Cell.ID, Location.Name, Cardinal_D, Stn_Num, Orig.Name, NABat.Sample, 
                Surveyed.2021, Yrs.Surveyed_2021, Natural.Sub.Region, Natural.Region,
                # LC_class, WTRBODY_TY, DIST_WTRBD, STREAM_TYP, Dist_Strea, RoadType, DistRoad_M, HF_TYPE, Dist_to_HF, 
                Latitude, Longitude)
dim(sta)
names(sta)

sta$NP <- as.factor(ifelse(sta$Land.Unit.Code %in% c("BNP", "JNP", "WBNP", "WLNP", "EINP"), "In", "Out")) %>% relevel(ref="In")

# convert sta to spatial layer, in latitude / longitude (crs = 4326)
sta_sf <- st_as_sf(sta, coords = c("Longitude","Latitude"), crs = 4326) 
sta_sf <- st_transform(sta_sf, crs=3400) # convert to NAD83 / Alberta 10-TM (Forest) for consistency with Alberta layers and metre unit

# quick plotting check
ggplot()+
  geom_sf(data = sta_sf) 

#############################################################################################
# Set GIS Dir for uploading GIS layers
GISDir <- c("/Volumes/LaCie/NABat/GIS")

# Read in Alberta GIS layers and determine distance to bat survey stations

# Landcover Polygons 2010 - ABMI, their source data: ABMI Remote Sensing Group 2013, based on the EOSD and NLWIS 2000 raster datasets and on hydrography and access GIS layers from the Government of Alberta. Update to 2010 based on ABMI Human Footprint dataset.
LC <- st_read(paste(GISDir,"/2010LanCoverShapeFiles", sep=""), layer="Lancover_Polygons_2010")
# what about st_join (left=TRUE)? might be faster...
LC.dist <- st_nn(sta_sf, LC, k=1, returnDist = T) 

sta_sf$LC_dist <- unlist(LC.dist$dist)
sta_sf$LC_type <- unlist(LC.dist$nn)
sta_sf$LC_type <- LC$LC_class[match(sta_sf$LC_type,rownames(LC))]
summary(sta_sf)

sta$Land.Cover <- as.factor(sta$LC_type)
levels(sta$Land.Cover)
sta$Land.Cover <- sta$Land.Cover %>% recode("20"="Water", "33"="Exposed Land", "34"="Developed", "50"="Shrubland", "110"="Grassland",
                                            "120"="Agriculture", "210"="Coniferous Forest", "220"="Broadleaf Forest", "230"="Mixed Forest")


# # Human Footprint 2018 - ABMI Human Footprint (gdb)
# st_layers("/Users/joburgar/Documents/NABat/GIS/HFI_2018_v1.gdb")
# HF_res <- st_read("/Users/joburgar/Documents/NABat/GIS/HFI_2018_v1.gdb",layer="o15_Residentials_HFI2018")

# Natural_Regions_Subregions_of_Alberta
NR <- st_read(paste(GISDir,"/Natural_Regions_Subregions_of_Alberta", sep=""), layer="Natural_Regions_Subregions_of_Alberta")
NR.dist <- st_nn(sta_sf, NR %>% st_transform(crs=3400), k=1, returnDist = T) 
names(NR)
sta_sf <- st_join(sta_sf, NR %>% select(NSRNAME, NRNAME), left=TRUE)

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

save.image("GIS.sta.covariates.RData")
#load("GIS.sta.covariates.RData")

# rm(WB,WS,LC,RD,CL)
write.csv (sta_sf %>% st_drop_geometry(), "./Input/NABat_Station_Covariates.csv", row.names = FALSE)

###--- for housekeeping remove big files
rm(CL,LC,RD,WB)

###--- Create provincial map
NR.NRNAME <-NR %>% group_by(NRNAME) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
NR.NRNAME$area <- st_area(NR.NRNAME)

Alberta <-
  NR.NRNAME %>%
  summarise(area = sum(area))

ggplot()+
  geom_sf(data=NR.NRNAME, aes(fill=NRNAME, colour=NRNAME))+
  geom_sf(data=sta_sf %>% filter(Surveyed.2021=="no"), col="white")+
  geom_sf(data=sta_sf %>% filter(Surveyed.2021=="yes"), col="black")+
  theme_minimal()

prev.survey <- sta_sf %>% filter(Surveyed.2021=="no") %>% count(GRTS.Cell.ID) %>% st_drop_geometry()
this.survey <- sta_sf %>% filter(Surveyed.2021=="yes") %>% count(GRTS.Cell.ID) %>% st_drop_geometry()

###--- create map of GRTS and NABat stations
NABatDir = c("/Volumes/LaCie/NABat/GIS/")
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

Cairo(file="Output/Fig_provincial.plot.PNG",
      type="png",
      width=1500,
      height=2000,
      pointsize=14,
      bg="white",
      dpi=300)
Fig_provincial.plot
dev.off()

#####--- Create distance matrix
sta_2021 <- sta_sf %>% filter(Surveyed.2021=="yes") %>% st_transform(crs=3400)

ggplot()+
  geom_sf(data=sta_2021)

stn_dist <- as.data.frame(st_distance(sta_2021, sta_2021, by_element = FALSE))
stn_dist <- drop_units(stn_dist)
head(stn_dist)
stn_dist[stn_dist==0]<- NA
stn_dist_means <- rowMeans(stn_dist, na.rm = T)
stn_dist_min <- apply(stn_dist, 1, FUN=min, na.rm = T)
stn_dist_max <- apply(stn_dist, 1, FUN=max, na.rm = T)
mean(stn_dist_means/1000)
mean(stn_dist_min/1000); min(stn_dist_min/1000); max(stn_dist_min/1000)
# [1] 6.306125
# [1] 0.02224549
# [1] 35.48178
mean(stn_dist_max/1000)

theme_set(theme_bw())

Alberta <-
  nr %>%
  summarise(area = sum(area))

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

