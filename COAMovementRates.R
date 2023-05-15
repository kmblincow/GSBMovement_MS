#Kayla Blincow
#08/11/2022

#COA Rate of Movement Analysis

#INCLUDES DELTA-GLMM MODEL AND FIGURE 5

#The goal of this script is to calcualte centers of acitivty for the GSBs and 
#use that information to then calculate rates of movement that we can use to 
#compare their movement across seasons.

#This will replace the non-proximal receiver analysis from the GSB movement chapter.

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

d <- filter(d, MPA != "DelMar")

#Calculate COAs
d$coatime <- floor_date(ymd_hms(d$DateTime_PST),
                        unit = "10 minutes") #can adjust time window as needed

coa <- d %>% group_by(Transmitter, coatime) %>% 
  summarize(mlong = mean(Longitude),
         mlat = mean(Latitude),
         nrec = length(unique(Station))) %>% 
  distinct(Transmitter, coatime,.keep_all = TRUE) %>% 
  data.frame()

#look for consecutive detections (need to fish to be detected to get accurate measure)
#calculate lag of 6 10 minute increments, and filter for ones that are in 120 minutes,
#Then rep(1:12, length.out =?)
coa <- coa %>% arrange(Transmitter, coatime)


#SPLIT THE DATAFRAME BASED ON TRANSMITTER AND THEN BY INSTANCES WHERE THE DIFFERENCE BETWEEN THE INTERVAL OF
#ONE ROW AND THE NEXT IS GREATER IS NOT EQUAL TO 10 MINUTES
#THIS WILL GENERATE A LIST OF DATAFRAMES WITH CONSECUTIVE TIME PERIODS
test2 <- coa %>% group_by(Transmitter) %>% 
  split(cumsum(c(1, diff(coa$coatime) != 10)))



#FILTER OUT DATAFRAMES IN THE LIST THAT DON'T HAVE AT LEAST 12 INTERVALS REPRESENTED
test2a <- purrr::map(test2, ~ filter(., nrow(.) > 5))

#CREATE A LONG LIST OF ID VALUES 
groups <- rep(1:50, 6) %>% sort()

#CREATE A UNIQUE ID FOR EACH GROUPING OF TIME INTERVALS (BY 6)
for(i in 1:length(test2a)){
  test2a[[i]]$ID <- NA
  for(j in 1:nrow(test2a[[i]])){
    test2a[[i]]$ID[j] <- paste0(i, "_", groups[j], sep = "")  
  }
}

#COMBINE LIST BACK INTO A LARGE DATAFRAME
test2b <- test2a %>% bind_rows()

#FILTER OUT INTERVAL GROUPS THAT DON'T HAVE 6 VALUES
test2c <- test2b %>% group_by(ID) %>% 
  summarize(nrows = n()) %>% 
  filter(nrows == 6)

#REJOIN TEST2C AND TEST2B SO THAT WE ONLY HAVE IDS WITH 6 CONSECUTIVE INTERVALS
coa.final <- left_join(test2c, test2b)



#calculate distance
coa.final$dist <- 0 
coa_list <- coa.final %>% group_split(ID) 

for(i in 1:length(coa_list)){
  coa_list[[i]]$dist[2:nrow(coa_list[[i]])] <- distHaversine(p1 = cbind(coa_list[[i]]$mlong, 
                                                                        coa_list[[i]]$mlat))
}

coa_dist <- bind_rows(coa_list)

hist(coa_dist$dist,100)

mvmtrate <- coa_dist %>% group_by(ID, Transmitter) %>% 
  summarize(disthr = sum(dist),
            starttime = first(coatime)) %>% 
  mutate(Month = factor(month(starttime)),
         Hour = hour(starttime),
         Date = date(starttime))


#add diel period and other information back into the data
otherinfo <- d %>% dplyr::select(Date, Hour, lunar, dielp, lobster, season)
otherinfo$Date <- as.Date(otherinfo$Date)
otherinfo <- distinct(otherinfo) %>% arrange(Date, Hour)

otherinfo$dielp2 <- NA

for(i in 1:(nrow(otherinfo)-1)){
  if((otherinfo$Date[i] == otherinfo$Date[i+1]) & (otherinfo$Hour[i] == otherinfo$Hour[i+1])){
    otherinfo$dielp2[i] <- otherinfo$dielp[i+1]
  } else {
    otherinfo$dielp2[i] <- otherinfo$dielp[i]
  }
}

otherinfo <- otherinfo %>% dplyr::select(-dielp)
otherinfo[is.na(otherinfo$dielp2),]$dielp2 <- "night"

otherinfo <- distinct(otherinfo)

mvmtrate1 <- left_join(mvmtrate, otherinfo)

#this should be a dataframe with hourly movement rates




#take a looksie at what I've done
hist(mvmtrate1$disthr)

summary(mvmtrate1$disthr) #zero inflated

#deltaglmm modelling
mvmtrate1$binary <- 0
mvmtrate1[mvmtrate1$disthr > 0,]$binary <- 1

mvmtrate1$Month <- factor(mvmtrate1$Month, levels = c(1:12))

#try with random slope effects 
m1 <- glmer(binary ~ dielp2 + Month + (Month|Transmitter) + (dielp2|Transmitter),
             family = binomial(), data = mvmtrate1)
#well that doesn't work... DOES NOT CONVERGE :(

#random intercept only
m1a <- glmer(binary ~ dielp2 + Month + (1|Transmitter), family = binomial,
             data = mvmtrate1)
summary(m1a)
plot_model(m1a, type = "eff")

MuMIn::r.squaredGLMM(m1a)

p_bin_diel <- plot_model(m1a, type = "eff")$dielp2
p_bin_month <- plot_model(m1a, type = "eff")$Month




#just positive movement rates
mvmtrate1a <- filter(mvmtrate1, binary == 1)
mvmtrate1a$Month <- factor(mvmtrate1a$Month, levels = c(1:12))

m2a <- lmer(disthr ~ dielp2 + Month + (1|Transmitter), data = mvmtrate1a)


summary(m2a)

MuMIn::r.squaredGLMM(m2a)

plot_model(m2a, type = "diag") #looks good minus the extremes on the qqplot
plot_model(m2a, type = "eff")
p_rate_diel <- plot_model(m2a, type = "eff")$dielp2
p_rate_month <- plot_model(m2a, type = "eff")$Month

MuMIn::r.squaredGLMM(m1a)
MuMIn::r.squaredGLMM(m2a)

#clean up my plots
p2 <- p_bin_diel +
  theme_classic() +
  labs(x = "",
       y = "Predicted Probabiliy of Non-Zero Movement Rate",
       title = "") +
  scale_y_continuous(labels = seq(0.40, 0.80, by = 0.10)) +
  annotate(geom = "text", x = 0.75, y = .8, label = "(d)")

p3 <- p_bin_month +
  theme_classic() +
  labs(x = "",
       y = "Predicted Probabiliy of Non-Zero Movement Rate",
       title = "") +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 10.5) +
  scale_y_continuous(breaks = seq(.30, .80, by = 0.1), labels = seq(0.3, 0.80, by = 0.10)) +
  annotate(geom = "text", x = 0.75, y = .82, label = "(c)")
  


p4 <- p_rate_diel +  
  theme_classic() +
  labs(x = "Diel Period",
       y = "Predicted Movement Rate (m/hr)",
       title = "") +
  annotate(geom = "text", x = 0.75, y = 780, label = "(f)")
  
p5 <- p_rate_month +  
  theme_classic() +
  labs(x = "Month",
       y = "Predicted Movement Rate (m/hr)",
       title = "") +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 10.5) +
  annotate(geom = "text", x = 0.75, y = 950, label = "(e)")


  
#look at number of detections
#need to include hours that had no detections
tags <- unique(d$Transmitter)
d$DateTime_PST <- ymd_hms(d$DateTime_PST, tz = "US/Pacific")

times <- list()
for(i in 1:length(tags)){
  min <- min(floor_date(filter(d, Transmitter == tags[i])$DateTime_PST, unit = "hour"))
  max <- max(floor_date(filter(d, Transmitter == tags[i])$DateTime_PST, unit = "hour"))
  times[[i]] <- data.frame(Transmitter = tags[i], DateHour = seq.POSIXt(from = min, to = max, by = "hour"))
}

times <- bind_rows(times)
times$Date <- date(times$DateHour)
times$Hour <- hour(times$DateHour)
times$Month <- month(times$DateHour)

d$DateHour <- ymd_h(paste(d$Date, d$Hour))

d <- arrange(d, Transmitter, DateTime_PST)


detectionsummary <- d %>% group_by(Transmitter, Date, dielp, Month, Hour) %>% 
  summarize(ndet = n()) 

detectionsummary$Date <- ymd(detectionsummary$Date)


fulld <- left_join(times, detectionsummary)
fulld[is.na(fulld$ndet),]$ndet <- 0

#assign dielp to NAs
fulld$dielp2 <- fulld$dielp
fulld[is.na(fulld$dielp2),]$dielp2 <- "nope"

fulld[fulld$dielp2 == "nope" & fulld$Hour < 5,]$dielp2 <- "night"
fulld[fulld$dielp2 == "nope" & fulld$Hour > 18,]$dielp2 <- "night"
fulld[fulld$dielp2 == "nope" & fulld$Hour > 6 & fulld$Hour <17,]$dielp2 <- "day"

for(i in 2:(nrow(fulld)-1)){
  if(fulld$dielp2[i] == "nope" & fulld$dielp2[i-1] == "dusk"){
    fulld$dielp2[i] <- "night"
  } else if(fulld$dielp2[i] == "nope" & fulld$dielp2[i+1] == "dusk"){
    fulld$dielp2[i] <- "day"
    } else if(fulld$dielp2[i] == "nope" & fulld$dielp2[i-1] == "dawn"){
        fulld$dielp2[i] <- "day"
      } else if(fulld$dielp2[i] == "nope" & fulld$dielp2[i+1] == "dawn"){
          fulld$dielp2[i] <- "night"
        } else {
            fulld$dielp2[i] <- fulld$dielp2[i]
      }
}

for(i in 2:(nrow(fulld)-1)){
  if(fulld$dielp2[i] == "nope" & (fulld$dielp2[i-1] == fulld$dielp2[i+1])){
    fulld$dielp2[i] <- fulld$dielp2[i+1]
  } else if(fulld$dielp2[i] == "nope" & fulld$dielp2[i-1] == "day" & fulld$dielp2[i+1] == "night") {
    fulld$dielp2[i] <- "dusk"
  } else if(fulld$dielp2[i] == "nope" & fulld$dielp2[i-1] == "night" & fulld$dielp2[i+1] == "day") {
    fulld$dielp2[i] <- "dawn"
  } else {
    fulld$dielp2[i] <- fulld$dielp2[i]
  }
}

fulld[fulld$dielp2 == "nope" & (fulld$Hour == 5 | fulld$Hour == 6),]$dielp2 <- "dawn"
fulld[fulld$dielp2 == "nope" & (fulld$Hour == 17 | fulld$Hour == 18),]$dielp2 <- "dusk"


#calculate detection rate per hour
fulld2 <- fulld  %>% 
  group_by(Transmitter, Date, dielp2, Month) %>% 
  summarize(detrate = sum(ndet)/n_distinct(Hour))

mndetections <- fulld2 %>% group_by(Month) %>% 
  summarize(mndet = mean(detrate),
            sddet = sd(detrate),
            sedet = sddet/sqrt(n()))
fulld2$Month <- factor(fulld2$Month, levels = 1:12,
                                 labels = 1:12)

p1 <- ggplot(fulld2, aes(x = Month, y = detrate)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.8) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 10.5) +
  labs(x = "",
       y = "Detections/Hr") +
  annotate(geom = "text", x = 0.75, y = 65, label = "(a)") +
  theme_classic() +
  theme(legend.position = "none")



p6 <- ggplot(fulld2, aes(x = dielp2, y = detrate)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.8) +
  labs(x = "",
       y = "Detections/Hr") +
  annotate(geom = "text", x = 0.65, y = 70, label = "(b)") +
  theme_classic()


  
library(patchwork)

fullp <- (p1 + p6)/ (p3 + p2) / (p5 +p4)

png(file="Figures/2022Figs/MovementSummary.png",
    width = 5000,
    height = 3700,
    res = 300)

fullp

dev.off()



#NOT SURE I EVEN NEED THIS MODEL... SO GOING TO ABANDON THIS HERE FOR NOW
hist(fulld2$detrate)
hist(log(fulld2$detrate))
m3 <- lmer(log(detrate) ~ Month + dielp + MPA + Month*MPA + dielp*MPA + (1|Transmitter), 
           data = detectionsummary)
summary(m3)

plot_model(m3, type = "diag")
plot_model(m3, type = "int")


#OLD THINGS
#calculate movement rate over period of day
mvmtrate <- coa_dist %>% group_by(Transmitter, Date, dielp) %>% 
  summarize(totdist = sum(dist),
            rate = totdist/n_distinct(Hour),
            month = first(Month))

#plot it
ggplot(filter(mvmtrate, rate > 0)) +
  geom_point(aes(x = factor(month), y = rate, color = Transmitter)) +
  geom_boxplot(aes(x = factor(month), y = rate),
               alpha = 0.3) +
  facet_wrap(~dielp)

ggplot(filter(mvmtrate, rate > 0)) +
  geom_point(aes(x = factor(month), y = rate, color = dielp)) +
  geom_boxplot(aes(x = factor(month), y = rate),
               alpha = 0.3)
  

#split model
mvmtrate$binary <- 0
mvmtrate[mvmtrate$rate > 0,]$binary <- 1

mvmtrate$month <- factor(mvmtrate$month)

m1 <- glmer(binary ~ dielp + month + (1|Transmitter), 
            family = binomial, data = mvmtrate)
summary(m1)
plot_model(m1, type = "eff")
