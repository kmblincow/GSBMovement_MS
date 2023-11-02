#Kayla Blincow
#6/18/2021

#UPDATE: 12/12/2022

#Tagging summary table calculations.

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(lubridate)

#load the data
d <- read.csv("Data/GSB_detections_2022.csv", header = T)

#calculate days at liberty
d$Date <- as.Date(d$Date)
d$tagdate <- as.Date(d$tagdate)

#for 6 tags still here
total <- unique(max(d$Date) - d$tagdate + 1) %>% sort()

mean(c(1437,1351,1344,1096,1095,1094,1000))
sd(c(1437,1351,1344,1096,1095,1094,1000))

#look at just La Jolla
LJ <- filter(d, Station != "Del Mar Mooring")

#calculate total number of days the fish were detected in the array
LJ %>% group_by(Transmitter) %>% 
  summarize(n = n_distinct(Date)) 

mean(c(1426,623,864,202,0,1,271))
sd(c(1426,623,864,202,0,1,271))

#residency
#56705
425/1437
#56711
179/1351
#27063
767/1344
#56704
202/1096
#56706
1/1094
#27070
187/1000

mean(c(0.296,0.132,0.571,0.184,0,0.001,0.187))
sd(c(0.296,0.132,0.571,0.184,0,0.001,0.187))

#Detection REsidency Index
LJ %>% group_by(Transmitter) %>% 
  summarize(DAL = max(Date) - min(Date) + 1)
mean(c(0,864,271,202,1426,1,283))
sd(c(0,864,271,202,1426,1,283))

#56705
425/1426
#56711
179/283
#27063
767/864
#56704
202/202
#56706
1/1
#27070
187/271

mean(c(0.298,0.633,0.888,1,0,1,.69))
sd(c(0.298,0.633,0.888,1,0,1,.69))

#calculate number of stations detected for each fish
LJ %>% group_by(Transmitter) %>% 
  summarize(n_distinct(Station))

mean(c(21,22,14,14,1,25,0))
sd(c(21,22,14,14,1,25,0))

#abacus plots
#order Stations From North to South
stationlevels <- c("Pacific Beach NEW", "LJK20", "LJK 19", "LJK 43", 
                   "LJK 42", 
                   "LJK 18", "LJK 17", "LJK 41",
                   "LJK 24", "LJK 40", "LJK 15", "LJK 39",
                   "LJK 14", "LJK 13", "LJK 12", "LJK 11",
                   "LJK 32", 
                   "LJK 10", "LJK 33", "LJK 9", "LJK 34",
                   "LJK 8", "LJK 7", "LJK 6", 
                   "PL - North LJ", "LJK 5", 
                   "LJK 4",  "LJK 3", "Charles -  LJK", "LJK 1",
                   "LJ (La Jolla Shores)")

stationnames <- c("PB", "LJK 20", "LJK 19", "LJK 43", 
                  "LJK 42", 
                  "LJK 18", "LJK 17", "LJK 41",
                  "LJK 24", "LJK 40", "LJK 15", "LJK 39",
                  "LJK 14", "LJK 13", "LJK 12", "LJK 11",
                  "LJK 32", 
                  "LJK 10", "LJK 33", "LJK 9", "LJK 34",
                  "LJK 8", "LJK 7", "LJK 6", 
                  "Deep LJ", "LJK 5", 
                  "LJK 4",  "LJK 3", "Charles", "LJK 1",
                  "LJ")

LJ$Station <- factor(LJ$Station, levels = stationlevels, labels = stationnames)

p2 <- ggplot(data = filter(LJ, Transmitter != "A69-1601-56706"), 
             aes(x = as.Date(Date), y = Station)) +
  geom_point() +
  facet_wrap(~Transmitter) +
  theme_classic()

png(file="Figures/StationAbacus.png",
    width = 2000,
    height = 1700,
    res = 300)
p2
dev.off()

LJ$Transmitter <- factor(LJ$Transmitter, labels = c("27063", "27070", 
                                                    "56704", "56705",
                                                    "56706", "56711"))

p <- ggplot(filter(LJ, Transmitter != "56706"), 
       aes(x = as.Date(Date), y = Transmitter)) +
  geom_point(shape = 16) +
  annotate("segment", x = as.Date("2018-08-14"), xend = as.Date("2018-08-14"),
           y = 3.75, yend = 4.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-08"), xend = as.Date("2018-11-08"),
           y = 4.75, yend = 5.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-15"), xend = as.Date("2018-11-15"),
           y = 0.75, yend = 1.25, size = 1) +
  annotate("segment", x = as.Date("2019-07-21"), xend = as.Date("2019-07-21"),
           y = 2.75, yend = 3.25, size = 1) +
  annotate("segment", x = as.Date("2019-10-25"), xend = as.Date("2019-10-25"),
           y = 1.75, yend = 2.25, size = 1) +
  labs(x = "Time", y = "Tag Number") +
  scale_x_date(date_labels = "%b %Y", 
               breaks = "6 months") +
  theme_classic()

png(file="Figures/TagAbacus.png",
    width = 2000,
    height = 1700,
    res = 300)
p
dev.off()

#Summary of Woohoo's trip to Del Mar
WH_dm <- d %>% filter(Transmitter == "A69-1601-56711") %>% 
  mutate(DateHour = paste(Date, Hour)) %>% 
  group_by(Station, DateHour) %>% 
  summarize(n_det = n()) %>% 
  as.data.frame()

#Create column with La Jolla v Del Mar
WH_dm$reg <- NA
WH_dm[WH_dm$Station == "Del Mar Mooring",]$reg <- 0
WH_dm[WH_dm$Station != "Del Mar Mooring", ]$reg <- 1


#turn date/hour column into time
WH_dm$DateHour <- ymd_h(WH_dm$DateHour)

#get full sequence of dates/hours
ts <- seq(min(WH_dm$DateHour), max(WH_dm$DateHour), by = "hour") %>% 
  as.data.frame()

names(ts) <- "DateHour"

#join to fill zeroes in gaps
WH_dm1 <- left_join(ts, WH_dm)
WH_dm1[is.na(WH_dm1$reg),]$reg <- 2


WH_dm1$reg <- factor(WH_dm1$reg, levels = c(1, 0, 2), 
                    labels = c("La Jolla", "Del Mar", "Not Detected"))

WH_dm1[is.na(WH_dm1$n_det),]$n_det <- 0

#calculate rolling average of detections per hour
WH_dm2 <- WH_dm1 %>% mutate(ravg = zoo::rollmean(n_det, k = 12, fill = NA))



p3 <- ggplot(WH_dm2, aes(x = DateHour, y = n_det)) +
  geom_point(aes(shape = reg, color = reg)) +
  geom_line(aes(x = DateHour, y = ravg), alpha = 0.7) +
  labs(x = "Time", y = "# of Detections Each Hour", color = "", shape = "") +
  scale_color_manual(values = c("#8961b3",
                                 "#969e4a",
                                 "gray50")) +
  scale_x_datetime(date_labels = "%b %Y", 
                   breaks = "3 months") +
  scale_y_continuous(limits = c(0, 80)) +
  theme_classic() +
  theme(legend.position = "bottom")

png(file="Figures/2022Figs/DelMar.png",
    width = 2500,
    height = 1700,
    res = 300)
p3
dev.off()
