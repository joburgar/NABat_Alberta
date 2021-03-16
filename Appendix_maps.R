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
#   GRTS_map <- GRTS.plot(GRTS.Cell.ID = i, v.just=-2, h.just=-0.1)
#   ggsave(file=paste("Output/Maps/GRTSID_",i,"_map.png",sep=""))

######################################
#- overall AB map
theme_set(theme_bw())
nr <- read_sf(dsn = paste(NABatDir,"/Natural_Regions_Subregions_of_Alberta", sep=""),
              layer = "Natural_Regions_Subregions_of_Alberta")

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
