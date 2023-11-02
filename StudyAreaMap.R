#Kayla Blincow
#6/25/2021
#UPDATED 12/16/2022

#UPDATED 9/21/2023 Adjusting figure based on reviewer comments

#Study Map

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(maps)
library(ggmap)
library(mapdata)
library(ggsn)
library(ggspatial)
library(rgdal)
library(sp)
library(terra)
library(tidyterra)
library(sf)

#load my data
d <- read.csv("Data/GSB_detections_2022.csv", header = T)

#get unique stations from that data
stations <- d %>% dplyr::select(Station, Latitude, Longitude) %>% 
  distinct()

#load tagging location dataset
tagloc <- read.csv("Data/tagginglocations.csv", header = T)

#other receivers/bigger picture
bigd <- read.csv("Data/OtherReceivers.csv", header = T)
bigd$region <- "Other"
bigd[bigd$Station == "LJK 20" | bigd$Station == "LJK 3" | bigd$Station == "LJ",]$region <- "LJ"

#bathymetry data
bathy <- read.csv("Data/MapData/sdNorth.xyz", header = F)



# set up a raster geometry, here deriving an extent from your data
bathy <- filter(bathy, V3<=0 & V1 >= -117.4 & V1 <= -117.18 & V2 >= 32.75 & V2 <= 32.95)

e <- ext(apply(bathy[,1:2], 2, range))

# set up the raster, for example
r <- rast(e, ncol= 240, nrow = 230)
bathy2 <- terra::rasterize(as.matrix(bathy[, 1:2]), r, bathy[,3], fun=mean)
plot(bathy2)



####Make Ze Maps!####

#get data situated


#pull/format MPA boundary data
mpasf <- readOGR("Data/MPAmapfiles/MPA_CA_StateAndFederal_180905.shp")
mpasf_df <- spTransform(mpasf, CRS("+proj=longlat +datum=WGS84"))

#turn it into a dataframe that ggplot will understand and filter for just the
#MPAs I care about
lj_mpasf <- broom::tidy(mpasf_df, region = "FULLNAME") %>% 
  filter(id == "Matlahuayl State Marine Reserve" |
           id == "San Diego-Scripps Coastal State Marine Conservation Area" |
           id == "South La Jolla State Marine Reserve" |
           id == "South La Jolla State Marine Conservation Area")

#add info to LJ stations
TP <- c("TP",	32.89223,	-117.26051)
stations <- rbind(stations, TP)
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)

stations$range <- NA
stations[stations$Station == "LJK20" |
           stations$Station == "LJK 19" |
           stations$Station == "LJK 11" |
           stations$Station == "LJK 3" |
           stations$Station == "LJK 7" |
           stations$Station == "Charles -  LJK", ]$range <- "Y"
stations$core <- NA
stations[stations$Station == "TP" |
           stations$Station == "Del Mar Mooring", ]$core <- "out"
stations[stations$Station != "TP" &
           stations$Station != "Del Mar Mooring", ]$core <- "in"


#pull our base map (smaller)
oashmap <- readOGR("Data/MapData/PacOcean/PACIFIC_OCEAN.shp")
oashmap_df1 <- spTransform(oashmap, CRS("+proj=longlat +datum=WGS84"))
oashmap_df <- broom::tidy(oashmap_df1)


#clip the bathymetry based on the ocean data
bathy2a <- crop(bathy2, vect(oashmap_df1), mask = T)


#pull our base map (bigger)
#load ocean data and set it up
world <- map_data("world")
bestcoast <- filter(world, region == "Mexico" | region == "USA")



####Bigger Area Map####
xlabs <- c(#"135.0°W",
           "130.0°W",
           "125.0°W",
           "120.0°W",
           "115.0°W")

ylabs <- c("20.0°N",
           "25.0°N",
           "30.0°N",
           "35.0°N")

base <- ggplot() +
  theme_classic() +
  geom_polygon(data = bestcoast, aes(x = long, y = lat, group = group), 
               fill = "gray60", color = "black") +
  annotate("segment",
           x = min(stations$Longitude),
           xend = -120,
           y = median(stations$Latitude),
           yend = 32.5,
           arrow = arrow(),
           linewidth = 1,
           color = "darkred") +
  geom_point(data = bigd, aes(x = Longitude, y = Latitude, color = region),
             size = 2.5) +
  scale_color_manual(values = c("red", "gray20")) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none") +
  coord_sf(xlim = c(-133, -112), ylim = c(20,36)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = seq(-130, -115, by = 5), 
                     labels = xlabs) +
  scale_y_continuous(breaks = seq(20, 35, by = 5),
                     labels = ylabs)

base

####Smaller Area Map####
zoom <- ggplot() +
  # geom_polygon(data = oashmap_df, aes(x = long, y = lat, group = piece), 
  #              fill = "white", color = "black") +
  geom_spatraster(data = bathy2a,  
                  show.legend = T) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "gray50", linewidth = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "San Diego-Scripps Coastal State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "gray50", linewidth = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "black", linewidth = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "black", linewidth = 1.2) +
  geom_point(data = stations, aes(y = Latitude, x = Longitude),
             size = 3, shape = 21, fill = NA) +
  geom_point(data = dplyr::filter(stations, range == "Y"), 
             aes(x = Longitude, y = Latitude), shape = 21, 
             size = 3, fill = "gray20") +
  geom_point(data = tagloc, aes(y = Lat, x = Long),
             shape = 8, size = 3,) +
  #scale_fill_gradientn(colors = terrain.colors(10)) +
  scale_fill_hypso_c(
    palette = "gmt_globe_bathy",
    breaks = c(-600, -400, -200, -100, -50)) +
  labs(x = "", y = "", fill = "Depth (m)") +
  coord_sf(xlim = c(-117.35, -117.2), ylim = c(32.78, 32.93)) +
  scale_x_continuous(breaks = seq(-117.35, -117.2, by = 0.03)) +
  scalebar(dist = 2, dist_unit = "km", x.min = -117.345, 
           x.max = -117.25, y.min = 32.781, y.max = 32.83, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl") +
  ggtitle("La Jolla Region") +
  annotate("text", y = 32.92955, x = -117.305, label = "Del Mar") +
  annotate("text", y= 32.89223,	x = -117.28, label = "Torrey Pines") +
  theme_classic() +
  theme(
        panel.border = element_rect(colour = "black", fill=NA, size = 1.5),
        panel.background = element_rect(fill = "gray60"),
        plot.background = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.8, .5),
        legend.background = element_rect(fill = NA),
        legend.key.height = unit(1, units = "cm"))
zoom

library(patchwork)

base + inset_element(zoom, left = 0.01, top = 0.8, 
                     bottom = 0.01, right = 0.7)






png(file="Figures/2022Figs/StudyMap_Reviews.png",
    width = 3250,
    height = 2500,
    res = 300)

base + inset_element(zoom, left = 0.01, top = 0.8, 
                     bottom = 0.01, right = 0.7)

dev.off()





#OTHER STUFF
array <- ggmap(mymap2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "San Diego-Scripps Coastal State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_point(data = stations, aes(y = Latitude, x = Longitude),
             size = 3) +
  labs(x = "", y = "") +
  scalebar(dist = 2, dist_unit = "km", x.min = -117.40, 
           x.max = -117.35, y.min = 32.77, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl") 


png(file="Figures/array.png",
    width = 3000,
    height = 2800,
    res = 300)

array

dev.off()
