###--- NABat Grid and Station maps
# to be used with NABat_Annual_Report.Rmd

###--- create map of GRTS and NABat stations
NABatDir = c("/Users/joburgars/Documents/NABat/GIS/")

NABat_grid <- read_sf(dsn = NABatDir,layer = "master_sample_Alberta")
#st_geometry(NABat_grid) # no ESPG
NABat_grid <- st_transform(NABat_grid, crs=4326) # now espg 4326
#NABat_grid

###--- create sf object of sta (Alberta NABat sites)
NABat.sf <- st_as_sf(sta, coords=c("Longitude", "Latitude"), crs = 4326)
NABat_grid <- NABat_grid %>% filter(GRTS_ID %in% sta$GRTS.Cell.ID)


# NABat stations within NABat grid cells - looks like it's loading correctly
# ggplot()+
#   geom_sf(data = NABat_grid) +
#   geom_sf(data = NABat.sf, pch=19) +
#   scale_shape_identity()+
#   coord_sf()


# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
AB.NABat_3857 <- st_transform(NABat.sf, 3857)
AB.NABat_3857_coords <- as.data.frame(st_coordinates(AB.NABat_3857))
AB.NABat_3857_coords$GRTS.Cell.ID <- AB.NABat_3857$GRTS.Cell.ID
AB.NABat_3857_coords$Location.Name <- AB.NABat_3857$Location.Name

NABat_grid_3857 <- st_transform(NABat_grid %>% filter(GRTS_ID %in% sta$GRTS.Cell.ID), 3857)

# read.table("google_key.txt")
register_google(key = google_key)


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


GRTS.plot <- function(GRTS.Cell.ID = GRTS.Cell.ID) {
  
  AB.GRTS_bb <- as.vector(st_bbox(NABat_grid_3857 %>% filter(GRTS_ID==GRTS.Cell.ID)))
  
  left <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[1])
  bottom <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[2])
  right <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[3])
  top <- as.numeric(st_bbox(NABat_grid %>% filter(GRTS_ID==GRTS.Cell.ID))[4])
  
  GRTS_map <- get_map(c(left = left ,
                        bottom = bottom,
                        right = right,
                        top =  top),
                      color = "bw")
  
  # Use the function in a loop to create all GRTS grid/station maps
  map_GRTS <- ggmap_bbox(GRTS_map)
  
  GRTS.plot <- ggmap(map_GRTS) + 
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = NABat_grid_3857 %>% filter(GRTS_ID==GRTS.Cell.ID), col="grey14", fill = NA, inherit.aes = FALSE) +
    geom_sf(data = AB.NABat_3857 %>% filter(GRTS.Cell.ID==GRTS.Cell.ID), col="darkred", fill="red", cex = 3, inherit.aes = FALSE) +
    geom_text(data=AB.NABat_3857_coords %>% filter(GRTS.Cell.ID==GRTS.Cell.ID), 
              aes(x=X ,y=Y, label=Location.Name, fontface="bold"), vjust=-1.2, size=4) +
    theme(legend.position = "none", text=element_text(size=13)) +
    theme(axis.text.y=element_blank(),axis.text.x=element_blank(),
          axis.ticks=element_blank()) +
    labs(y = "", x = "")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=3))
  
  return(GRTS.plot)
  
}

slices <- unique(sta$GRTS.Cell.ID)
length(slices)

# for(i in slices){
#   GRTS_map <- GRTS.plot(GRTS.Cell.ID = i)
#   Cairo(file=paste("Output/Maps/GRTSID_",i,"_map.png",sep=""), 
#         type="png",
#         width=2800, 
#         height=2000, 
#         pointsize=18,
#         bg="white",
#         dpi=300)
#   GRTS_map
#   dev.off()
# }  

# for some reason it doesn't work as loop, but works if I specify each i ?!?!?