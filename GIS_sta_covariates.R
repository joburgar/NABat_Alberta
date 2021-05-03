###--- this script generates distance to feature values and type of value for station covariates

# Load Packages
list.of.packages <- c("tidyverse", "sf", "nngeo")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#############################################################################################
# Read station covariates csv
sta <- read.csv("Input/NABat_Station_Covariates.csv", header=T, na.string=c("","NA", "<NA>",-1)) %>%
  dplyr::select(OBJECTID, LandUnitCo, GRTSCellID, LocName, Cardinal_D, Stn_Num, Orig_Name, NABat_Samp, YrsSurveye, NSRNAME, NRNAME,
                # LC_class, WTRBODY_TY, DIST_WTRBD, STREAM_TYP, Dist_Strea, RoadType, DistRoad_M, HF_TYPE, Dist_to_HF, 
                Latitude, Longitude) %>%
  rename(FID = OBJECTID, Land.Unit.Code=LandUnitCo, GRTS.Cell.ID=GRTSCellID, Location.Name=LocName,Orig.Name=Orig_Name, NABat.Sample=NABat_Samp,
         # Land.Cover=LC_class, Waterbody.Type=WTRBODY_TY, Waterbody.Distance=DIST_WTRBD, Stream.Type=STREAM_TYP, Stream.Distance=Dist_Strea, Road.Type=RoadType, Road.Distance=DistRoad_M,
         # Human.Footprint.Type=HF_TYPE, Human.Footprint.Distance=Dist_to_HF,
         Yrs.Surveyed=YrsSurveye, Natural.Sub.Region=NSRNAME, Natural.Region=NRNAME)
sta$NP <- as.factor(ifelse(sta$Land.Unit.Code %in% c("BNP", "JNP", "WBNP", "WLNP", "EINP"), "In", "Out")) %>% relevel(ref="In")

# convert sta to spatial layer, in latitude / longitude (crs = 4326)
sta_sf <- st_as_sf(sta, coords = c("Longitude","Latitude"), crs = 4326) 
sta_sf <- st_transform(sta_sf, crs=3400) # convert to NAD83 / Alberta 10-TM (Forest) for consistency with Alberta layers and metre unit

# quick plotting check
ggplot()+
  geom_sf(data = sta_sf) 

#############################################################################################
# Set GIS Dir for uploading GIS layers
GISDir <- c("/Users/joburgar/Documents/NABat/GIS")

# Read in Alberta GIS layers and determine distance to bat survey stations

# Landcover Polygons 2010 - ABMI, their source data: ABMI Remote Sensing Group 2013, based on the EOSD and NLWIS 2000 raster datasets and on hydrography and access GIS layers from the Government of Alberta. Update to 2010 based on ABMI Human Footprint dataset.
LC <- st_read(paste(GISDir,"/2010LanCoverShapeFiles", sep=""), layer="Lancover_Polygons_2010")
LC.dist <- st_nn(sta_sf, LC, k=1, returnDist = T) 

sta_sf$LC_dist <- unlist(LC.dist$dist)
sta_sf$LC_type <- unlist(LC.dist$nn)
sta_sf$LC_type <- LC$LC_class[match(sta_sf$LC_type,rownames(LC))]
summary(sta_sf)

save.image("GIS.sta.covariates.RData")
load("GIS.sta.covariates.RData")
