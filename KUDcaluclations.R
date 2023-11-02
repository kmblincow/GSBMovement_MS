#Kayla Blincow
#8/12/2022

#Calculating GSB KUDs

#The purpose of this script is to calculate the kernel utilization distributions
#for the GSBs in spawning and non-spawning seasons, and then to assess the 
#overlap with MPAs.

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(adehabitatHR)
library(rgdal)
library(rgeos)
library(maps)
library(ggmap)
library(mapdata)
library(maptools)
library(ggsn)
library(ggspatial)
library(sp)
library(sf)
library(patchwork)

#load ze data!
d <- read.csv("Data/GSB_detections_2022.csv", header = T)

#split data based on spawning/non-spawning season
sp <- filter(d, Month > 4 & Month < 10 & 
               Station != "Del Mar Mooring" &
               Transmitter != "A69-1601-56706")
nsp <- filter(d, (Month < 5 | Month > 9) & 
                Station != "Del Mar Mooring" &
                Transmitter != "A69-1601-56706")

#Calculate COAs
coacalc <- function(dat){
  dat$coatime <- floor_date(ymd_hms(dat$DateTime),
                            unit = "30 minutes") #can adjust time window as needed
  
  coa <- dat %>% group_by(Transmitter, coatime) %>% 
    mutate(mlong = mean(Longitude),
           mlat = mean(Latitude),
           nrec = length(unique(Station))) %>% 
    distinct(Transmitter, coatime,.keep_all = TRUE) %>% 
    data.frame()
  
  coa
}

coa_sp <- coacalc(sp)
coa_nsp <- coacalc(nsp)


#Calculate KUDs
kudcalc <- function(dat){
  kud_df <- dplyr::select(dat, Transmitter, mlong, mlat) %>% 
    rename(id = Transmitter, x = mlong, y = mlat)
  
  #convert to spatial points dataframe
  coordinates(kud_df) <- c("x", "y")
  proj4string(kud_df) <- CRS("+proj=longlat +datum=NAD83")
  kud_df2 <- spTransform(kud_df, CRS("+proj=utm +zone=11 ellps=WGS84"))
  
  #calculate KUD
  kud1 <- kernelUD(kud_df2, grid = 1000, extent = 5)
  kud1
}

kud_sp <- kudcalc(coa_sp)
kud_nsp <- kudcalc(coa_nsp)

HR95_sp <- getverticeshr(kud_sp, percent = 95, unout = "m2")
HR50_sp <- getverticeshr(kud_sp, percent = 50, unout = "m2")

HR95_nsp <- getverticeshr(kud_nsp, percent = 95, unout = "m2")
HR50_nsp <- getverticeshr(kud_nsp, percent = 50, unout = "m2")

#load ocean data and set it up
oashmap <- readOGR("Data/MapData/PacOcean/PACIFIC_OCEAN.shp")
oashmap_join <- spTransform(oashmap, CRS("+proj=utm +zone=11 +ellps=WGS84 +units=m +no_defs"))
oash_cut <- st_as_sf(oashmap_join)

oashmap_df1 <- spTransform(oashmap, CRS("+proj=longlat +datum=WGS84"))
oashmap_df <- broom::tidy(oashmap_df1)


#cut land mass
HR95_sp_cut <- st_as_sf(HR95_sp)
HR95_sp_oashcut <- st_intersection(HR95_sp_cut, oash_cut)

HR50_sp_cut <- st_as_sf(HR50_sp)
HR50_sp_oashcut <- st_intersection(HR50_sp_cut, oash_cut)

HR95_nsp_cut <- st_as_sf(HR95_nsp)
HR95_nsp_oashcut <- st_intersection(HR95_nsp_cut, oash_cut)

HR50_nsp_cut <- st_as_sf(HR50_nsp)
HR50_nsp_oashcut <- st_intersection(HR50_nsp_cut, oash_cut)



# #convert spatial polygons to dataframes
# HR95_spgg <- spTransform(HR95_sp, "+proj=longlat +datum=WGS84") %>% fortify()
# HR50_spgg <- spTransform(HR50_sp, "+proj=longlat +datum=WGS84") %>% fortify()
# 
# HR95_nspgg <- spTransform(HR95_nsp, "+proj=longlat +datum=WGS84") %>% fortify()
# HR50_nspgg <- spTransform(HR50_nsp, "+proj=longlat +datum=WGS84") %>% fortify()

#plot the things

#create receiver object
array <- dplyr::select(d, Station, Latitude, Longitude, MPA) %>%
  distinct()


#pull/format MPA boundary data
mpasf <- readOGR("Data/MPAmapfiles/MPA_CA_StateAndFederal_180905.shp")
mpasf_df <- spTransform(mpasf, CRS("+proj=longlat +datum=WGS84"))

#turn it into a dataframe that ggplot will understand and filter for just the
#MPAs I care about
lj_mpasf <- broom::tidy(mpasf_df, region = "FULLNAME") %>% 
  filter(id == "Matlahuayl State Marine Reserve" |
           id == "San Diego-Scripps Coastal State Marine Conservation Area" |
           id == "South La Jolla State Marine Reserve" |
           id == "South La Jolla State Marine Conservation Area") %>% 
  rename(ID = id)

#load tagging location dataset
tagloc <- read.csv("Data/tagginglocations.csv", header = T)


#make a base map to build off of
my_Map <- ggplot() +
  theme_classic() +
  geom_polygon(data = oashmap_df, aes(x = long, y = lat, group = piece), 
               fill = "white", color = NA) +
  geom_point(data = array, aes(x = Longitude, y = Latitude), 
             color = "black", alpha = 0.5, size = 0.75) +
  geom_polygon(data = filter(lj_mpasf, 
                             ID == "South La Jolla State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "gray70", size = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             ID == "San Diego-Scripps Coastal State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "gray70", size = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             ID == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "black", size = 1.2) +
  geom_polygon(data = filter(lj_mpasf, 
                             ID == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "black", size = 1.2) +
  labs(x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim = c(-117.35, -117.2), ylim = c(32.78, 32.88)) +
  ggsn::scalebar(transform = T, dist =1, dist_unit = "km", height = 0.01,
                 x.min = -117.35, x.max = -117.2, y.min = 32.78, y.max = 32.88,
                 location = "bottomleft", model = "WGS84", st.size =3) +
  ggsn::north( x.min = -117.35, x.max = -117.2, y.min = 32.78, y.max = 32.88,
               symbol = 12, scale = 0.08) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_rect(fill = "gray60")) 
  
my_Map


#combine into single sf object
HR95_sp_oashcut$Level <- "95% KUD"
HR95_sp_oashcut$Season <- "Spawning"

HR50_sp_oashcut$Level <- "50% KUD"
HR50_sp_oashcut$Season <- "Spawning"

HR95_nsp_oashcut$Level <- "95% KUD"
HR95_nsp_oashcut$Season <- "Non-Spawning"

HR50_nsp_oashcut$Level <- "50% KUD"
HR50_nsp_oashcut$Season <- "Non-Spawning"


KUDs <- rbind(HR95_sp_oashcut, HR50_sp_oashcut, HR95_nsp_oashcut, HR50_nsp_oashcut)

mapfunction <- function(shortID, longID, letter, legendoption){
  my_Map +
    geom_sf(data = filter(KUDs, id == longID), aes(alpha = Level, fill = Season, color = Season)) +
    scale_color_manual(values = c("darkgoldenrod1", "mediumpurple")) +
    scale_fill_manual(values = c("darkgoldenrod1", "mediumpurple")) +
    scale_alpha_manual(values = c(0.6, 0.2)) +
    # geom_sf(data = filter(HR50_sp_oashcut, id == longID), 
    #         color = "mediumpurple", fill = "mediumpurple", alpha = 0.6) +
    # geom_sf(data = filter(HR95_nsp_oashcut, id == longID), 
    #         fill = "darkgoldenrod1", color = "darkgoldenrod1", alpha = 0.25) +
    # geom_sf(data = filter(HR50_nsp_oashcut, id == longID), 
    #         fill = "darkgoldenrod1", color = "darkgoldenrod1", alpha = 0.6) +
    geom_point(data = filter(tagloc, ID == shortID), aes(x = Long, y = Lat),
               shape = 8, size = 3) +
    coord_sf(xlim = c(-117.35, -117.2), ylim = c(32.78, 32.88),
             crs = CRS("+proj=longlat +datum=WGS84")) +
    scale_x_continuous(breaks = seq(-117.34, -117.20, by = .04)) +
    annotate("text", x = -117.342, y = 32.88, label = letter) +
    guides(alpha = guide_legend(override.aes = list(fill = "gray20"))) +
    theme(legend.position = legendoption)
}



p27063 <- mapfunction(shortID = 27063,
                      longID = "A69-1601-27063", 
                      letter = "(d) Tag #3",
                      legendoption = "none") 
p27063 

p27070 <- mapfunction(shortID = 27070,
                      longID = "A69-1601-27070", 
                      letter = "(f) Tag #7",
                      legendoption = "right")
p27070

p56704 <- mapfunction(shortID = 56704,
                      longID = "A69-1601-56704", 
                      letter = "(e) Tag #4",
                      legendoption = "none")
p56704

p56705 <- mapfunction(shortID = 56705,
                      longID = "A69-1601-56705", 
                      letter = "(b) Tag #1",
                      legendoption = "none")
p56705

p56711 <- mapfunction(shortID = 56711,
                      longID = "A69-1601-56711", 
                      letter = "(c) Tag #2",
                      legendoption = "none")
p56711 


abacusdata <- d %>% filter(Station != "Del Mar Mooring") %>% 
  filter(Transmitter != "A69-1601-56706") %>% 
  group_by(Transmitter, Date) %>% 
  summarize(date = first(Date))

abacusdata$Transmitter <- factor(abacusdata$Transmitter, 
                                 levels = c("A69-1601-56705", 
                                            "A69-1601-56711",
                                            "A69-1601-27063",
                                            "A69-1601-56704",
                                            "A69-1601-27070"),
                                 labels = c("Tag #1", "Tag #2", "Tag #3", 
                                            "Tag #4", "Tag #7")) 

ab <- ggplot() +
  geom_point(data = abacusdata, aes(x = as.Date(Date), y = Transmitter)) +
  annotate("segment", x = as.Date("2018-08-14"), xend = as.Date("2018-08-14"),
           y = 0.75, yend = 1.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-08"), xend = as.Date("2018-11-08"),
           y = 1.75, yend = 2.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-15"), xend = as.Date("2018-11-15"),
           y = 2.75, yend = 3.25, size = 1) +
  annotate("segment", x = as.Date("2019-07-21"), xend = as.Date("2019-07-21"),
           y = 3.75, yend = 4.25, size = 1) +
  annotate("segment", x = as.Date("2019-10-25"), xend = as.Date("2019-10-25"),
           y = 4.75, yend = 5.25, size = 1) +
  annotate("text", x = as.Date("2018-06-15"), y = 5.25, label = "(a)") +
  scale_x_date(date_labels = "%b %Y", 
               breaks = "5 months") +
  labs(x = "Time", y = "Tag Number") +
  theme_classic()

p <- (ab)/ (p56705 + p56711 + p27063)/ (p56704 + p27070)

p2  <- (p56705 + p56711)/(p27063 + p56704)/(p27070 + theme(legend.position = "bottom") + ab)


layout <- "
ABC
ADE
AFG
"

p3 <- (ab + coord_flip()) + p56705 + p56711 + p27063 + p56704 + (p27070) + 
  guide_area() +
  plot_layout(design = layout, guides = "collect")

p3


#going with this one...
layout <- "
AA
BC
DE
FG
"




p4 <- (ab + theme(axis.title = element_text(size = 15),
                  axis.text = element_text(size = 12))) + 
  (p56705 + theme(axis.title.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title.y = element_text(size = 15))) +
  (p56711 + theme(axis.title = element_blank(),
                  axis.text = element_text(size = 12))) + 
  (p27063 + theme(axis.title.x = element_blank(),
                 axis.text = element_text(size = 12),
                 axis.title.y = element_text(size = 15))) +
  (p56704 + theme(axis.title.y = element_blank(),
                 axis.text = element_text(size = 12),
                 axis.title.x = element_text(size = 15))) +
  (p27070 + theme(axis.title = element_text(size = 15),
               axis.text = element_text(size = 12),
               legend.key.size = unit(1, "cm"),
               # legend.key.height = unit(1, "cm"),
               # legend.key.width = unit(1, "cm"),
               legend.title = element_text(size=15),
               legend.text = element_text(size=12))) + 
  guide_area() +
  plot_layout(design = layout, guides = "collect",
              heights = c(1, 2, 2, 2))

p4


# png(file="Figures/2022Figs/KUDs_ReviewsTest.png",
#     width = 3500,
#     height = 3000,
#     res = 300)
# 
# p3
# 
# dev.off()

png(file="Figures/2022Figs/KUDs_ReviewsTest2.png",
    width = 3200,
    height = 4000,
    res = 300)

p4

dev.off()

#KUD sizes
st_area(HR95_nsp_oashcut)
# Units: [m^2]
# [1]  2083693  1055038  1161325  2042754 10086834
st_area(HR50_nsp_oashcut)
# Units: [m^2]
# [1]  310732.9  146920.8  181128.4  482101.6 1787536.0
st_area(HR95_sp_oashcut)
# Units: [m^2]
# [1] 5165587 4914899 1938990 2706165 9795832
st_area(HR50_sp_oashcut)
# Units: [m^2]
# [1]  542517.5  694180.4  299777.9  441208.2 1589826.3


#Percent KUD overlap with MPAs
MPAs <- mpasf[mpasf$FULLNAME == "Matlahuayl State Marine Reserve" |
                mpasf$FULLNAME == "South La Jolla State Marine Reserve",]
MPAs_join <- spTransform(MPAs, CRS("+proj=utm +zone=11 +ellps=WGS84 +units=m +no_defs"))
MPAs_sf <- st_as_sf(MPAs_join)                

#calculate intersections
HR95_sp_MPAcut <- st_intersection(MPAs_sf,HR95_sp_oashcut)
HR50_sp_MPAcut <- st_intersection(MPAs_sf,HR50_sp_oashcut)
HR95_nsp_MPAcut <- st_intersection(MPAs_sf,HR95_nsp_oashcut)
HR50_nsp_MPAcut <- st_intersection(MPAs_sf,HR50_nsp_oashcut)



# % KUD overlap with MPAs (95% spawning season)
(st_area(HR95_sp_MPAcut[1,]) + st_area(HR95_sp_MPAcut[2,]))/(st_area(HR95_sp_oashcut[1,]))
#27063: 0.2479045
(st_area(HR95_sp_MPAcut[3,]) + st_area(HR95_sp_MPAcut[4,]))/(st_area(HR95_sp_oashcut[2,]))
#27070: 0.8845196
#56704: 0
#56705: 0
(st_area(HR95_sp_MPAcut[5,]))/(st_area(HR95_sp_oashcut[5,]))
#56711: 0.3259954

sum(st_area(HR95_sp_MPAcut))/(sum(st_area(HR95_sp_oashcut)))
#total: 0.3603273



# % KUD overlap with MPAs (50% spawning season)
#27063: 0
st_area(HR50_sp_MPAcut)/(st_area(HR50_sp_oashcut[2,]))
#27070: 1
#56704: 0
#56705: 0
#56711: 0

sum(st_area(HR50_sp_MPAcut))/(sum(st_area(HR50_sp_oashcut)))
#total: 0.20111105

# % KUD overlap with MPAs (95% non-spawning season)
st_area(HR95_nsp_MPAcut[1,])/(st_area(HR95_nsp_oashcut[1,]))
#27063: 0.02353719
st_area(HR95_nsp_MPAcut[2,])/(st_area(HR95_nsp_oashcut[2,]))
#27070: 1
#56704: 0
#56705: 0
st_area(HR95_nsp_MPAcut[3,])/(st_area(HR95_nsp_oashcut[5,]))
#56711: 0.4121744

sum(st_area(HR95_nsp_MPAcut))/(sum(st_area(HR95_nsp_oashcut)))
#total: 0.3215066

# % KUD overlap with MPAs (50% non-spawning season)
#27063: 0
st_area(HR50_nsp_MPAcut[1,])/(st_area(HR50_nsp_oashcut[2,]))
#27070: 1
#56704: 0
#56705: 0
st_area(HR50_nsp_MPAcut[2,])/(st_area(HR50_nsp_oashcut[5,]))
#56711: 0.06524615

sum(st_area(HR50_nsp_MPAcut))/(sum(st_area(HR50_nsp_oashcut)))
#total: 0.09135921



















#NOT SURE I WANT TO INCLUDE THE BELOW STUFF...

#look at day/night KUDs in spawning and nonspawning
spday <- filter(sp, dielp == "day")
spnight <- filter(sp, dielp == "night")
spdusk <- filter(sp, dielp == "dusk")

nspday <- filter(nsp, dielp == "day")
nspnight <- filter(nsp, dielp == "night")
nspdusk <- filter(nsp, dielp == "dusk")

#make coas
coa_spday <- coacalc(spday)
coa_spnight <- coacalc(spnight)
coa_spdusk <- coacalc(spdusk)

coa_nspday <- coacalc(nspday)
coa_nspnight <- coacalc(nspnight)
coa_nspdusk <- coacalc(nspdusk)

#calculate KUDs
kud_spday <- kudcalc(coa_spday)
kud_spnight <- kudcalc(coa_spnight)
kud_spdusk <- kudcalc(coa_spdusk)

kud_nspday <- kudcalc(coa_nspday)
kud_nspnight <- kudcalc(coa_nspnight)
kud_nspdusk <- kudcalc(coa_nspdusk)

#find 95/50 KUD
HR95_spday <- getverticeshr(kud_spday, percent = 95, unout = "m2")
HR50_spday <- getverticeshr(kud_spday, percent = 50, unout = "m2")

HR95_spnight <- getverticeshr(kud_spnight, percent = 95, unout = "m2")
HR50_spnight <- getverticeshr(kud_spnight, percent = 50, unout = "m2")

HR95_spdusk <- getverticeshr(kud_spdusk, percent = 95, unout = "m2")
HR50_spdusk <- getverticeshr(kud_spdusk, percent = 50, unout = "m2")

HR95_nspday <- getverticeshr(kud_nspday, percent = 95, unout = "m2")
HR50_nspday <- getverticeshr(kud_nspday, percent = 50, unout = "m2")

HR95_nspnight <- getverticeshr(kud_nspnight, percent = 95, unout = "m2")
HR50_nspnight <- getverticeshr(kud_nspnight, percent = 50, unout = "m2")

HR95_nspdusk <- getverticeshr(kud_nspdusk, percent = 95, unout = "m2")
HR50_nspdusk <- getverticeshr(kud_nspdusk, percent = 50, unout = "m2")


#convert spatial polygons to dataframes
HR95_spggday <- spTransform(HR95_spday, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_spggday <- spTransform(HR50_spday, "+proj=longlat +datum=WGS84") %>% fortify()

HR95_spggnight <- spTransform(HR95_spnight, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_spggnight <- spTransform(HR50_spnight, "+proj=longlat +datum=WGS84") %>% fortify()

HR95_spggdusk <- spTransform(HR95_spdusk, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_spggdusk <- spTransform(HR50_spdusk, "+proj=longlat +datum=WGS84") %>% fortify()

HR95_nspggday <- spTransform(HR95_nspday, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_nspggday <- spTransform(HR50_nspday, "+proj=longlat +datum=WGS84") %>% fortify()

HR95_nspggnight <- spTransform(HR95_nspnight, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_nspggnight <- spTransform(HR50_nspnight, "+proj=longlat +datum=WGS84") %>% fortify()

HR95_nspggdusk <- spTransform(HR95_nspdusk, "+proj=longlat +datum=WGS84") %>% fortify()
HR50_nspggdusk <- spTransform(HR50_nspdusk, "+proj=longlat +datum=WGS84") %>% fortify()

#plot the things

#create receiver object
array <- dplyr::select(d, Station, Latitude, Longitude, MPA) %>%
  distinct()

#plot the KUDs
spawn <- my_Map + 
  # geom_polygon(data = HR95_spggday, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "darkgoldenrod1", fill = "darkgoldenrod1") +
  geom_polygon(data = HR50_spggday, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "darkgoldenrod1", fill = "darkgoldenrod1") +
  # geom_polygon(data = HR95_spggnight, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "deepskyblue", fill = "deepskyblue") +
  geom_polygon(data = HR50_spggnight, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "deepskyblue", fill = "deepskyblue") +
  # geom_polygon(data = HR95_spggdusk, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "aquamarine3", fill = "aquamarine3") +
  geom_polygon(data = HR50_spggdusk, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "aquamarine3", fill = "aquamarine3") +
  facet_wrap(~id)

nspawn <- my_Map + 
  # geom_polygon(data = HR95_nspggday, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "darkgoldenrod1", fill = "darkgoldenrod1") +
  geom_polygon(data = HR50_nspggday, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "darkgoldenrod1", fill = "darkgoldenrod1") +
  # geom_polygon(data = HR95_nspggnight, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "deepskyblue", fill = "deepskyblue") +
  geom_polygon(data = HR50_nspggnight, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "deepskyblue", fill = "deepskyblue") +
  # geom_polygon(data = HR95_nspggdusk, aes(x = long, y = lat, group = piece), 
  #              alpha = 0.3, color = "aquamarine3", fill = "aquamarine3") +
  geom_polygon(data = HR50_nspggdusk, aes(x = long, y = lat, group = piece), 
               alpha = 0.5, color = "aquamarine3", fill = "aquamarine3") +
  facet_wrap(~id)
