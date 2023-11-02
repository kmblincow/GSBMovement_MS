#Kayla Blincow
#9/4/2023

#Woohoo Del Mar Transit speed/Other Reviewer Comment Questions

#The purpose of this script is to estimate the rate of movement 
#of Woohoo for it's transit up to Del Mar.

#Just adding on other reviewer comment quick questions.

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(geosphere)
library(lme4)
library(lmerTest)
library(sjPlot)

#read my data
d <- read.csv("Data/GSB_detections_2022.csv", header = T)

d <- filter(d, Transmitter == "A69-1601-56711")


#check that real quick
unique(d$Station)
check <- d %>% group_by(Date, Station) %>% 
  summarize(n = n())
check2 <- filter(d, Date == as.Date("2019-08-20") | Date == as.Date("2019-08-19"))
#20900s between detections

#Woohoo days at liberty just La Jolla
difftime(ymd("2018-11-10"), ymd("2019-08-19")) #283 days (inclusive of these guys)
179/283#array residency

#Calculate distance between LJK7 and Del Mar
stations <- d %>% dplyr::select(Station, Latitude, Longitude) %>% 
  filter(Station == "Del Mar Mooring" | Station == "LJK 7") %>% 
  distinct()

distHaversine(stations[1,c(3,2)], stations[2, c(3,2)])


#last receivers for fish that peaced out
d <- read.csv("Data/GSB_detections_2022.csv", header = T)

d %>% dplyr::select(Transmitter, Station, Date) %>% 
  group_by(Transmitter) %>% 
  slice_tail(n = 5) %>% 
  group_split

#56711 (Tag 2): Del Mar (upper upper - like it's Del Mar)
#56705 (Tag 1): LJK 4/LJK 6 (final day) - (Upper Left Corner)
#56704 (Tag 4): LJK 12 - mid kelp edge
#27070 (Tag 7): LJK 39 - (lower right corner)
#27063 (Tag 3): LJK 4 - (Upper Left corner)

#56706 (Tag 6): (not full) LJK 10

stationsfull <- d %>% dplyr::select(Station, Longitude, Latitude) %>% 
  distinct()
ggplot(stationsfull, aes(x = Longitude, y = Latitude, label = Station)) +
  geom_text()
