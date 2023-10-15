###--- this script generates tables for annual GRTS specific appendices
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
Year_interest <- year(as.Date("2022-01-01"))

load(paste("NABat_Annual_Report_",Year_interest,".RDS", sep=""))

#############################################################################

###--- create map of GRTS and NABat stations
NABatDir = c("/Volumes/LaCie_2TB/NABat/GIS/")
NABat_grid <- read_sf(dsn = NABatDir,layer = "master_sample_Alberta")
st_geometry(NABat_grid) # no ESPG
NABat_grid <- st_transform(NABat_grid, crs=4326) # now espg 4326

###--- create sf object of sta (Alberta NABat sites)
NABat.sf <- st_as_sf(sta, coords=c("Longitude", "Latitude"), crs = 4326)

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
AB.NABat_3857 <- st_transform(NABat.sf, 3857)
AB.NABat_3857_coords <- as.data.frame(st_coordinates(AB.NABat_3857))
AB.NABat_3857_coords$GRTS.Cell.ID <- AB.NABat_3857$GRTS.Cell.ID
AB.NABat_3857_coords$Location.Name <- AB.NABat_3857$Location.Name

NABat_grid_3857 <- st_transform(NABat_grid %>% filter(GRTS_ID %in% sta$GRTS.Cell.ID), 3857)

# NABat stations within NABat grid cells - looks like it's loading correctly
# ggplot()+
#   geom_sf(data = NABat_grid) +
#   geom_sf(data = NABat.sf, pch=19) +
#   geom_sf(data=NABat_grid_3857, col="blue")+
#   scale_shape_identity()+
#   coord_sf()

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
  # GRTS.Cell.ID = 23146
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

# # Run for each inset map to have prior to running appendices
sta$GRTS.Cell.ID
slices <- unique(sta$GRTS.Cell.ID)
class(slices)
for(i in 1:length(slices)){
  GRTS.Cell.ID <- slices[i]
  print(GRTS.Cell.ID)
  GRTS_map <- GRTS.plot(GRTS.Cell.ID = GRTS.Cell.ID, v.just=-2, h.just=-0.1)
  ggsave(file=paste("Output/Maps/GRTSID_",i,"_map.png",sep=""))
}


######################################
###--- overall AB map
# theme_set(theme_bw())
# nr <- read_sf(dsn = paste(NABatDir,"/Natural_Regions_Subregions_of_Alberta", sep=""),
#               layer = "Natural_Regions_Subregions_of_Alberta")
# 
# NR.NRNAME <-nr %>% group_by(NRNAME) %>%
#   summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
#   summarise(across(geometry, ~ st_combine(.)))
# 
# nr$area <- st_area(nr)
# 
# Alberta <-
#   nr %>%
#   summarise(area = sum(area))
# 
# unique(nr$NRNAME)
# RM <-nr %>%
#   filter(NRNAME =="Rocky Mountain") %>%
#   summarise(area = sum(area))
# RM$NR <- "Rocky Mountain"
# 
# BO <-nr %>%
#   filter(NRNAME =="Boreal") %>%
#   summarise(area = sum(area))
# BO$NR <- "Boreal"
# 
# PA <-nr %>%
#   filter(NRNAME =="Parkland") %>%
#   summarise(area = sum(area))
# PA$NR <- "Parkland"
# 
# GA <-nr %>%
#   filter(NRNAME =="Grassland") %>%
#   summarise(area = sum(area))
# GA$NR <- "Grassland"
# 
# CS <-nr %>%
#   filter(NRNAME =="Canadian Shield") %>%
#   summarise(area = sum(area))
# CS$NR <- "Canadian Shield"
# 
# FH <-nr %>%
#   filter(NRNAME =="Foothills") %>%
#   summarise(area = sum(area))
# FH$NR <- "Foothills"
# 
# NR <- rbind(RM, BO, PA, GA, CS, FH)
# NR
# 
# NABat_AppendixMap_sf = list(NR = NR, Alberta=Alberta)
# saveRDS(NABat_AppendixMap_sf, "NABat_AppendixMap_sf.RDS")
# rm(list=ls())

NABat_AppendixMap_sf <- readRDS("NABat_AppendixMap_sf.RDS")

Alberta <- NABat_AppendixMap_sf$Alberta
NR <- NABat_AppendixMap_sf$NR
