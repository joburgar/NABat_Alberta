######################################################################################################
# NABat_Find_Grid_Cells.R
# created by Joanna Burgar, 2-June-2020
# matching NABat grid cells to locations
######################################################################################################

######################################################################################################
# LOAD PACKAGES (BEGINNING)
######################################################################################################
library(rgdal)   # loading shapefiles
library(tidyverse)   # data manipulation
library(raster)  # for mapping (e.g., determining extent of layers)
library(rgeos)   # for mapping (e.g., determining CRS)
library(ggplot2) # plotting maps
library(Cairo)   # printing maps for report quality output
library(sf)      # for mapping
library(ggspatial)
######################################################################################################
# LOAD PACKAGES (END)
######################################################################################################

######################################################################################################
# SET DIRECTORIES (BEGINNING)
######################################################################################################
NABatDir = c("/Volumes/LaCie_2TB/NABat/GIS/")
######################################################################################################
# SET DIRECTORIES (END)
######################################################################################################

######################################################################################################
# LOAD NABAT & COORDINATE DATA (BEGINNING)
######################################################################################################
NABat_sites <- read_sf(dsn = NABatDir,layer = "master_sample_Alberta")
st_geometry(NABat_sites) # no ESPG
NABat_sites <- st_transform(NABat_sites, crs=4326) # now espg 4326

# plot NABat sites to check loaded properly
ggplot() + 
  geom_sf(data = NABat_sites)

######################################################################################################
# LOAD NABAT & COORDINATE DATA (END)
######################################################################################################

######################################################################################################
# CREATE FUNCTIONS (BEGINING)
######################################################################################################
# site needs to be in coord dataframe format,

fn.Site_to_Buffer_spatial_utm <- function(coord=coord, dist=10){
  
  coord.sp <- st_as_sf(coord, coords=c("Easting","Northing"), crs=26911)
  coord.sp <- st_transform(coord.sp, crs=3780) # # convert to utm for buffer
  coord.sp.buffer <- st_buffer(coord.sp, dist=dist)
  coord.sp.buffer <- st_transform(coord.sp.buffer, crs=4326) # convert back to espg to match with NABat
  return(coord.sp.buffer)
}

fn.Site_to_Buffer_spatial <- function(coord=coord, dist=10){
  
  coord.sp <- st_as_sf(coord, coords=c("Longitude","Latitude"), crs=4326) 
  coord.sp <- st_transform(coord.sp, crs=3780) # # convert to utm for buffer
  coord.sp.buffer <- st_buffer(coord.sp, dist=dist)
  coord.sp.buffer <- st_transform(coord.sp.buffer, crs=4326) # convert back to espg to match with NABat
  return(coord.sp.buffer)
}

fn.Find_Grid_Cells <- function(buffered_site=buffered_site){
  site_NABat.sf <- st_crop(NABat_sites, buffered_site)
  site_NABat.sf <- site_NABat.sf %>% arrange(GRTS_ID)
  site_NABat_Priority <- as.data.frame(site_NABat.sf %>% count(GRTS_ID))
  site_NABat_Priority$Priority <- seq(1,nrow(site_NABat_Priority))
  site_NABat.sf$Priority <- site_NABat_Priority$Priority[match(site_NABat.sf$GRTS_ID, site_NABat_Priority$GRTS_ID)]
  return(site_NABat.sf)
}

fn.Coord_to_Grid <- function(input.dd=input.dd, dist=10){
  coord <- as.data.frame(t(input.dd)) # need to run this if  c("lat", "lon") format
  colnames(coord) <- c("Latitude", "Longitude")
  input_buffer <- fn.Site_to_Buffer_spatial(coord = coord, dist=dist) # m units
  input_GridCell <- fn.Find_Grid_Cells(input_buffer)
  return(input_GridCell)
}
######################################################################################################
# CREATE FUNCTIONS (END)
######################################################################################################

######################################################################################################
# LOAD Bat DATA (START)
######################################################################################################
getwd()
name_of_file <- c("Bayne_Metadata2023_wGRTS")
df <- read.csv(paste0(name_of_file,".csv")) # read in file if multiple locations needing Location ID

# enusre Latitude and Longitude columns
as_tibble(df)

df <- rename(df, Longitude = Long) # may need to adjust depending on long / lat names
df <- rename(df, Latitude = Lat)
df <- rename(df, Site = location)


df <- df %>% filter(Site %in% c("BAT-23-W-534"))
# add Grid Cells (or check if already there) 
# will produce a warning
df$GRTS_ID <- NA
for(i in 1:nrow(df)){ # works for all but row 74
  input_buffer <- fn.Site_to_Buffer_spatial(coord = df[i,c("Longitude","Latitude")], dist=10) # m units
  input_GridCell <- fn.Find_Grid_Cells(input_buffer)
  df[i,]$GRTS_ID <- input_GridCell$GRTS_ID
}

df %>% filter(is.na(GRTS_ID))

df_sf <- st_as_sf(df, coords=c("Longitude","Latitude"), crs=4326)
df_sf$GRTS_ID <- as.factor(df_sf$GRTS_ID)
df_sf$GRTS_ID <- as.factor(df_sf$GRTS_Cell_ID)

df_GRTS_ID <- sort(as.factor(unique(df_sf$GRTS_ID)))

ggplot() + 
  geom_sf(data = df_sf, cex=2) +
  geom_sf(data = NABat_sites %>% filter(GRTS_ID %in% df_GRTS_ID), 
          aes(col=as.factor(GRTS_ID)),fill=NA)+
  geom_sf_label(data = df_sf, aes(label = Site, hjust=1.2))


# check to see if Location.Name already exists
exLoc <- read.csv("Bulk_site_meta_ALL.csv")
glimpse(exLoc)
class(exLoc)

exLoc %>% filter(Location.Name=="9811_NE_01")

# check to see if the Grid Cells have already been surveyed
exLoc$Year <- substr(exLoc$Survey.Start.Time, start = 1, stop = 4)

exCells <- exLoc %>% filter(GRTS.Cell.Id %in% unique(df_sf$GRTS_ID)) %>% group_by(GRTS.Cell.Id) %>% count(Location.Name)
exLoc_sf <- st_as_sf(exLoc, coords=c("Longitude","Latitude"), crs=4326) 

newname <- c("9811_NE_01")
filter(exLoc, Location.Name == newname) # if it comes up as null then check on map to see if truly available
exLoc %>% filter(GRTS.Cell.ID=="9811")

ggplot() + 
  geom_sf(data = exLoc_sf %>% filter(GRTS.Cell.Id %in% unique(exCells$GRTS.Cell.Id)), cex=2, col="red") +
  geom_sf_label(data = exLoc_sf %>% filter(GRTS.Cell.Id %in% unique(exCells$GRTS.Cell.Id)), aes(label = GRTS.Cell.Id, hjust=1.2))+
  geom_sf(data = df_sf, cex=2) +
  geom_sf_label(data = df_sf, aes(label = Site, hjust=1.2))

#######################
# individual checking - alter code as necessary
df_GRTS_ID
write.csv(df,"Bayne_Metadata2023_wGRTS.csv", row.names = FALSE)


cell_to_check <- df_GRTS_ID[4]
exLoc_sf %>% filter(GRTS.Cell.Id == cell_to_check) %>% count(Location.Name)
ggplot() + 
  geom_sf(data = NABat_sites %>% filter(GRTS_ID == cell_to_check),
          aes(col=as.factor(GRTS_ID)),fill=NA)+
  geom_sf(data = exLoc_sf %>% filter(GRTS.Cell.Id == cell_to_check), cex=2, col="red")+
  geom_sf(data = df_sf %>% filter(GRTS_ID == cell_to_check), cex=2) +
  geom_sf_label(data = df_sf %>% filter(GRTS_ID == cell_to_check), aes(label = Site),
                nudge_x = 0.005, nudge_y = -0.005)

site_to_check <- "199047"
cell_to_check <- "219731"
ggplot() + 
  geom_sf(data = exLoc_sf %>% filter(GRTS.Cell.Id == cell_to_check), cex=2, col="red")+
  geom_sf_label(data = exLoc_sf %>% filter(GRTS.Cell.Id == cell_to_check), aes(label = Location.Name, hjust=1.2))+
  geom_sf(data = NABat_sites %>% filter(GRTS_ID==cell_to_check), aes(col=as.factor(GRTS_ID)),fill=NA)+
  geom_sf(data = df_sf %>% filter(Site == site_to_check), cex=2) 
