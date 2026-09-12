# not sure which of these I actually need. Keep for now.

library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)
library(ggpubr)
library(FSA)
library(rcompanion)
library(tweedie)
library(statmod)
library(extrafont)
loadfonts(device = "all", quiet = TRUE) 
library(rcartocolor)

################################################################################
#Set Up data frame 
################################################################################
urch <- read.csv("Data/urch_clean.csv")

################################################################################
#NMDS and PERMANOVA
################################################################################
# Need to decide which of the following to do 

#1. by subhabitat (UPZ, NPZ, AZ) for all
#2. by subhabitat by site
#3. by subhabitat by cape

#a. include: urchin data: urchins, empty pits, 
#b. include: community algal canopy (just algal canopy + urchin cover)
#c. include: community primary space (urchin cover + primary + canopy spp.)

#1. by subhabitat
######### all zones ############################################################

#2. by site / cape per subhabitat


#these first few all work. Just not sure I need them. 

################################################################################
#PRIMARY SPACE ONLY 
################################################################################

######### nonpit zone by site and cape #########################################
# this code looks at just community primary space + canopy. no urchin cover ####
NonPit <- urch %>%
  filter(Subhabitat == "NPZ")
 #        SiteCode != "BB")

#**change this line **
com<- NonPit %>%
  # this line
  select(-AvailableBareRock)
com <- com [, 47:55]
com<- com %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

Sqcom <- sqrt(com)

#temporary for unclean data
Sqcom <- Sqcom %>% select(where(~ !any(is.na(.))))

#make Bray
Sim <- vegdist(Sqcom, method = "bray")

nmds <- metaMDS(Sim, k = 2, trymax = 40)

nmds_coords <- as.data.frame(scores(nmds, display = "sites"))

#**change this line **
NonPit1 <- cbind(NonPit,nmds_coords)

# plot by Cape
NonPitplotCape <- ggplot(data=NonPit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Nonpit Subhabitat primary sapce nMDS by Cape")
plot(NonPitplotCape)


ggsave(filename = "Figures/nMDS_primaryspace_Nonpit_byCape.png",
       plot = NonPitplotCape , width = 8, height = 6, dpi = 300)

# plot by Site
NonPitplotSite <- ggplot(
  data = NonPit1,
  aes(x = NMDS1, y = NMDS2, color = SiteCode)
) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Nonpit Subhabitat NMDS by Site")

plot(NonPitplotSite)

ggsave(filename = "Figures/nonpitSiteNMDS.png",
       plot = NonPitplotSite , width = 8, height = 6, dpi = 300)

############## permanovas #####################################################
dispersion <- betadisper(Sim, NonPit$SiteCode, type="centroid")
plot(dispersion)
ggsave(filename = "Figures/dispersion.png",
       plot = dispersion , width = 8, height = 6, dpi = 300)
anova(dispersion)

TukeyHSD(dispersion)


NonPit_perma <- adonis2(
  Sim ~ Cape + SiteCode,
  data = NonPit,
  permutations = 999
)

NonPit_perma
# 
# 
# ########## pit zone ############################################################
# #pit zone nmds
# 
 Pit <- urch %>%
  filter(Subhabitat %in% c("UPZ"))
# 
# #nmds doing something....need to check these and what this means. 
# #I think it drops urchin data and looks only at primary cover and canopy.. 
# #question do I keep urchin cover?
# 
# pitcom<- Pit %>% 
#   select(-AvailableBareRock)
# pitcom2 <- pitcom [, 47:55]
# pitcom2<- pitcom2 %>%
#   mutate(across(everything(), ~replace_na(.x, 0)))
# 
# PitSq <- sqrt(pitcom2)
# 
# #temporary for unclean data
# PitSq_noNA <- PitSq %>% select(where(~ !any(is.na(.))))
# 
# #make Bray 
# PitSim <- vegdist(PitSq_noNA, method = "bray")
# 
# Pitnmds <- metaMDS(PitSim, k = 2, trymax = 40)
# 
# Pitnmds_coords <- as.data.frame(scores(Pitnmds, display = "sites"))
# 
# Pit1 <- cbind(Pit,Pitnmds_coords)
# 
# # plot by Cape 
# PitplotCape <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
#   geom_point(size = 1) +
#   stat_ellipse(linewidth = .5) +
#   labs(title = "Pit Subhabitat NMDS by Cape")
# plot(PitplotCape)
# 
# ggsave(filename = "Figures/pitNMDS.png", 
#        plot = PitplotCape , width = 8, height = 6, dpi = 300)
# 
# # plot by Site
# PitplotSite <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=SiteCode )) +
#   geom_point(size = 1) +
#   stat_ellipse(linewidth = .5) +
#   labs(title = "Pit Subhabitat NMDS by Site")
# plot(PitplotSite)
# 
# ggsave(filename = "Figures/pitSiteNMDS.png", 
#        plot = PitplotSite , width = 8, height = 6, dpi = 300)
# 
# ############## permanovas #####################################################
# dispersion <- betadisper(PitSim, Pit$SiteCode, type="centroid")
# plot(dispersion)
# ggsave(filename = "Figures/Pitdispersion.png", 
#        plot = dispersion , width = 8, height = 6, dpi = 300)
# anova(dispersion)
# 
# TukeyHSD(dispersion)
# 
# Pit_perma <- adonis2(
#   PitSim ~ Cape + SiteCode,
#   data = Pit,
#   permutations = 999
# )
# 
# Pit_perma
# 
# ########## algal zone by cape ##################################################
# #algal zone nmds
# 
# Algal <- urch %>%
#   filter(Subhabitat %in% c("AZ"))
# 
# #nmds doing something....need to check these and what this means. 
# #I think it drops urchin data and looks only at primary cover and canopy.. 
# #question do I keep urchin cover?
# 
# Agcom<- Algal %>% 
#   select(-44)
# agcom2 <- Agcom [, 35:56]
# agcom2<- agcom2 %>%
#   mutate(across(everything(), ~replace_na(.x, 0)))
# 
# AgSq <- sqrt(agcom2)
# 
# #temporary for unclean data
# AgSq_noNA <- AgSq %>% select(where(~ !any(is.na(.))))
# 
# #make Bray 
# AgSim <- vegdist(AgSq_noNA, method = "bray")
# 
# Agnmds <- metaMDS(AgSim, k = 2, trymax = 40)
# 
# Agnmds_coords <- as.data.frame(scores(Agnmds, display = "sites"))
# 
# Alg1 <- cbind(Algal,Agnmds_coords)
# 
# # plot by Cape 
# AlgplotCape <- ggplot(data = Alg1, aes(x = NMDS1, y = NMDS2, color = Cape)) +
#   geom_point(size = 1) +
#   stat_ellipse(linewidth = .5) +
#   labs(title = "Algal Subhabitat NMDS by Cape")
# plot(AlgplotCape)
# 
# ggsave(filename = "Figures/algalNMDS.png", 
#        plot = AlgplotCape , width = 8, height = 6, dpi = 300)
# 
# # plot by Site
# AlgplotSite <- ggplot(data = Alg1, aes(x = NMDS1, y = NMDS2, color = SiteCode)) +
#   geom_point(size = 1) +
#   stat_ellipse(linewidth = .5)  +
#   labs(title = "Algal Subhabitat NMDS by Site")
# plot(AlgplotSite)
# 
# ggsave(filename = "Figures/algalsiteNMDS.png", 
#        plot = AlgplotSite , width = 8, height = 6, dpi = 300)

################################################################################
# PERMANOVA for urchin density, pit density, occupancy, drift kelp, canopy
################################################################################
#1. Nonpit Zone

#2. Pit Zone
pitperma<- Pit %>% 
  select(TotalAdultUrchins, CreviceUrchins, OpenUrchins, JuvenileUrchins, 
         EmptyPits, PitsPresent, TotalAttachedDrift, TotalCanopy)

pitperma<- pitperma %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

pitperma<- pitperma[rowSums(pitperma) > 0, ]


PitpermaSq <- sqrt(pitperma)

#temporary for unclean data
PitpermaSq_noNA <- PitpermaSq %>% select(where(~ !any(is.na(.))))

#make Bray 
PitSim2 <- vegdist(PitpermaSq_noNA, method = "bray")

Pitnmds2 <- metaMDS(PitSim2, k = 2, trymax = 40)

Pitnmds_coords2 <- as.data.frame(scores(Pitnmds2, display = "sites"))

#remove rows where 0 
#hich(rowSums(PitpermaSq_noNA, na.rm = TRUE) == 0)
Pit <- Pit[-c(11, 15, 134), ]

Pit2 <- cbind(Pit,Pitnmds_coords2)

# plot by Cape 
PitCape <- ggplot(data=Pit2, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Pit Subhabitat Urchin Data by Cape")
plot(PitCape)

ggsave(filename = "Figures/pitNMDS.png", 
       plot = PitplotCape , width = 8, height = 6, dpi = 300)

# plot by Site
PitplotSite <- ggplot(data=Pit2, aes(x=NMDS1, y=NMDS2, color=SiteCode )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Pit Subhabitat NMDS by Site")
plot(PitplotSite)

ggsave(filename = "Figures/pitSiteNMDS.png", 
       plot = PitplotSite , width = 8, height = 6, dpi = 300)

############## permanovas #####################################################
dispersion <- betadisper(PitSim2, Pit$SiteCode, type="centroid")
plot(dispersion)
ggsave(filename = "Figures/dispersion.png", 
       plot = dispersion , width = 8, height = 6, dpi = 300)
anova(dispersion)


TukeyHSD(dispersion)


NonPit_perma <- adonis2(
  NonPitSim ~ Cape + SiteCode,
  data = NonPit,
  permutations = 999
)

NonPit_perma






