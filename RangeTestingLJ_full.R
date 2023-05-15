#Kayla Blincow
#6/13/2021

#Range Testing, calculating distance between VR100 points and receiver

#then pulling in the final data and plotting the results


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(geosphere)


# #load my data
# d <- read.csv("LJRangeTest/RangeDistance.csv", header = T)
# 
# #calculate the distance between VR100 reads (lat/long) and receiver position (LatR, LongR)
# 
# #create longitude/latitude objects
# recs <- d %>%  
#   group_by(Receiver) %>% 
#   distinct(LatR, LongR)
# 
# VR100_20 <- d %>% 
#   filter(Receiver == "LJK20") %>%
#   select(Long, Lat)
# 
# VR100_19 <- d %>% 
#   filter(Receiver == "LJK19") %>%
#   select(Long, Lat)
# 
# VR100_11 <- d %>% 
#   filter(Receiver == "LJK11") %>%
#   select(Long, Lat)
# 
# VR100_3 <- d %>% 
#   filter(Receiver == "LJK3") %>%
#   select(Long, Lat)
# 
# VR100_Charles <- d %>% 
#   filter(Receiver == "Charles") %>%
#   select(Long, Lat)
# 
# VR100_7 <- d %>% 
#   filter(Receiver == "LJK7") %>%
#   select(Long, Lat)
# 
# 
# dist_20 <- distm(recs[1,c(3,2)], VR100_20)
# dist_19 <- distm(recs[2,c(3,2)], VR100_19)
# dist_11 <- distm(recs[3,c(3,2)], VR100_11)
# dist_3 <- distm(recs[4,c(3,2)], VR100_3)
# dist_Charles <- distm(recs[5,c(3,2)], VR100_Charles)
# dist_7 <- distm(recs[6,c(3,2)], VR100_7)
# 
# d$dist <- c(dist_20, dist_19, dist_11, dist_3, dist_Charles, dist_7)
# 
# #write the csv
# write.csv(d, "RangeDistance.csv")


#now actually plot the range test
library(lme4)
library(sjPlot)
library(glmmTMB)
library(merTools)
library(ggeffects)

d2 <- read.csv("LJRangeTest/LJRangeTestData.csv", header = T)

d2$Receiver <- as.factor(d2$Receiver)

d2$Dist_sc <- scale(d2$Dist)

#build the glm (random effect of receiver in distance slope)
#start with removing receivers we didn't actually intend to test
g <- glmer(RecDet ~ Dist + (0 + Dist|Receiver), family = "binomial", d2) 
summary(g)


# At what distance is detection probability 50%?
predict(g, re.form = NA, newdata = data.frame(Dist = seq(218, 219, 0.1))) %>% 
  plogis()
seq(218, 219, 0.1)[4]

#218.3

#create prediction data for plotting
rec <- rep(unique(d2$Receiver), 505) %>% sort()
newdata <- data.frame(Dist = rep(seq(0, 504, 1), 6),
                     Receiver = rec)

#predictions for individual random effects of slope for each receiver
pred <- predict(g, newdata) %>% plogis()

#predictions for global mean for all receivers
pred2 <- predict(g, re.form = NA, newdata = newdata) %>% plogis

#convert those predictions to plottable data
plot_d <- data.frame(est = pred, dist = newdata$Dist,
                     Receiver = rec)
plot_d2 <- data.frame(est = pred2, dist = newdata$Dist)

# #get prediction intervals for global mean
# prediction <- predictInterval(g, which = "fixed", newdata = newdata, 
#                               level = 0.95) 
# int_upper <- prediction[,2] %>% plogis
# int_lower <- prediction[,3] %>% plogis
# est_d <- prediction[,1] %>% plogis
# 
# int_d <- data.frame(est = est_d,
#                     upper = int_upper, 
#                     lower = int_lower, 
#                     dist = newdata$Dist)
# 
# 
# plotREsim(REsim(g, n.sims = 100), stat = 'median', sd = TRUE)


#get confidence interval for global mean
CFd <- ggpredict(g, interval = "confidence", type = "fixed",
                 terms = "Dist [all]")
CFd <- CFd %>% as.data.frame()

#Change receiver labels
d2$Receiver <- factor(d2$Receiver, labels = c("RangeTest1", "RangeTest2",
                                              "RangeTest3", "RangeTest4",
                                              "RangeTest5", "RangeTest6"))
plot_d$Receiver <- factor(plot_d$Receiver, labels = c("RangeTest1", "RangeTest2",
                                                      "RangeTest3", "RangeTest4",
                                                      "RangeTest5", "RangeTest6"))

p <- ggplot() +
  geom_ribbon(data = CFd, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  geom_point(data = d2, aes(x = Dist, y = RecDet),
             alpha = 0.7) +
  # geom_line(data = plot_d, aes(x = dist, y = est, color = Receiver),
  #           alpha = 0.7) +
  geom_line(data = plot_d2, aes(x = dist, y = est), size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_vline(xintercept = 218.3, linetype = "dotted") +
  theme_classic() +
  scale_color_manual(values = rep("gray50", 6)) +
  labs(x = "Distance (m)", y = "Probability of Detection") +
  theme(legend.position = "none")


p

png(file="Figures/2022Figs/RangeTestAnalysis.png",
    width = 2000,
    height = 1700,
    res = 300)
p
dev.off()
