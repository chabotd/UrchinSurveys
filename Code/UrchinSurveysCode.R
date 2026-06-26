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


################################################################################
#Set Up data frame 
################################################################################
urch <- read.csv("Data/WorkingUrchinSurveyData.csv")

#get rid of rows below 
urch <- urch %>%
  dplyr::slice(1:715)
# get rid of rows with NAs for now (will go back and photo-ID)
urch <- urch %>%
  filter(!Call_Number %in% c(
    "CB_2026_UPZ_2_3",
    "CB_2026_UPZ_2_4",
    "CB_2026_UPZ_2_5"
  ))

urch <- urch %>%
  filter(!Call_Number %in% c(
    "SC_2026_AZ_1_1",
    "SC_2026_AZ_1_2",
    "SC_2026_AZ_1_3",
    "SC_2026_AZ_1_4",
    "SC_2026_AZ_1_5",
    "SC_2026_AZ_2_1",
    "SC_2026_AZ_2_2",
    "SC_2026_AZ_2_3",
    "SC_2026_AZ_2_4",
    "SC_2026_AZ_2_5", 
    "CB_2026_AZ_2_4"
  ))
#code to make something as.numeric
#str(urch
urch$SandCobble <-as.numeric(urch$SandCobble)

# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))
#rename zone to subhabitat

urch<- urch %>%
  rename(Subhabitat = Zone)

#add TOTAL number urchins (Juvs + Adults)

urch <- urch %>% mutate(TotalUrchins = TotalAdultUrchins + JuvenileUrchins)

##if not using Cali data use this.
#urch<- urch %>%
#  filter(!(SiteCode %in% c("CMS", "CMN")))

# add column for TotalPits and Ratio of Empty:Full
urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)
urch <- urch %>% mutate(TotalPits = EmptyPits + PittedUrchins)

# add column for percent occupancy 
urch <- urch %>%
  mutate(
    PercentOccupancy = (PittedUrchins / TotalPits) * 100
  )

#susceptibility 
urch$Connectivity <- with(urch, case_when(
  SiteCode %in% c("SC", "CB", "WC", "CP", "RP") ~ "Susceptible",
  SiteCode %in% c("BB", "SB", "FC") ~ "Unlikely",
  SiteCode %in% c("YB", "SH") ~ "Extremely Unlikely",
  TRUE ~ NA_character_  # for any sites not listed
))

# add nonpits together (IF ONLY LOOKING AT PITTED)
urch<- urch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

urch$PercentNonPitted <- (urch$NonPit/ urch$TotalAdultUrchins) * 100

# add cryptic urchins together (IF ONLY LOOKING AT OPEN)
urch <- urch %>% mutate(Cryptic = PittedUrchins + CreviceUrchins)

urch$PercentCryptic <- (urch$Cryptic/ urch$TotalUrchins) * 100

####### just know that Crevice count twice 
#& CANNOT compare Cryptic to NonPit; only Open! 


# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

#drop avail bare rock since not clean
urch <- urch %>%
  select(-AvailableBareRock)


################################################################################
#NMDS 
################################################################################
#nonpit zone nmds

NonPit <- urch %>%
  filter(Subhabitat %in% c("NPZ"))

#code to make something as.numeric
#str(NonPit)
NonPit$SandCobble <-as.numeric(NonPit$SandCobble)

#nmds doing something....
nonpitcom<- NonPit %>% 
  select(-44)
nonpitcom2 <- nonpitcom [, 35:56]
nonpitcom2<- nonpitcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

NonPitSq <- sqrt(nonpitcom2)

#temporary for unclean data
NonPitSq_noNA <- NonPitSq %>% select(where(~ !any(is.na(.))))

#make Bray 
NonPitSim <- vegdist(NonPitSq_noNA, method = "bray")

NonPitnmds <- metaMDS(NonPitSim, k = 2, trymax = 40)

NonPitnmds_coords <- as.data.frame(scores(NonPitnmds, display = "sites"))

NonPit1 <- cbind(NonPit,NonPitnmds_coords)

# plot by Cape 
NonPitplotCape <- ggplot(data=NonPit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Nonpit Subhabitat NMDS by Cape")
plot(NonPitplotCape)


ggsave(filename = "Figures/nonpitNMDS.png", 
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

##########pit zone
################################################################################
#pit zone nmds

Pit <- urch %>%
  filter(Subhabitat %in% c("UPZ"))

#nmds doing something....need to check these and what this means. 
#I think it drops urchin data and looks only at primary cover and canopy.. 
#question do I keep urchin cover?

pitcom<- Pit %>% 
  select(-44)
pitcom2 <- pitcom [, 35:56]
pitcom2<- pitcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

PitSq <- sqrt(pitcom2)

#temporary for unclean data
PitSq_noNA <- PitSq %>% select(where(~ !any(is.na(.))))

#make Bray 
PitSim <- vegdist(PitSq_noNA, method = "bray")

Pitnmds <- metaMDS(PitSim, k = 2, trymax = 40)

Pitnmds_coords <- as.data.frame(scores(Pitnmds, display = "sites"))

Pit1 <- cbind(Pit,Pitnmds_coords)

# plot by Cape 
PitplotCape <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Pit Subhabitat NMDS by Cape")
plot(PitplotCape)

ggsave(filename = "Figures/pitNMDS.png", 
       plot = PitplotCape , width = 8, height = 6, dpi = 300)

# plot by Site
PitplotSite <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=SiteCode )) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Pit Subhabitat NMDS by Site")
plot(PitplotSite)

ggsave(filename = "Figures/pitSiteNMDS.png", 
       plot = PitplotSite , width = 8, height = 6, dpi = 300)

##########algal zone by cape 
################################################################################
#algal zone nmds

Algal <- urch %>%
  filter(Subhabitat %in% c("AZ"))

#nmds doing something....need to check these and what this means. 
#I think it drops urchin data and looks only at primary cover and canopy.. 
#question do I keep urchin cover?

Agcom<- Algal %>% 
  select(-44)
agcom2 <- Agcom [, 35:56]
agcom2<- agcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

AgSq <- sqrt(agcom2)

#temporary for unclean data
AgSq_noNA <- AgSq %>% select(where(~ !any(is.na(.))))

#make Bray 
AgSim <- vegdist(AgSq_noNA, method = "bray")

Agnmds <- metaMDS(AgSim, k = 2, trymax = 40)

Agnmds_coords <- as.data.frame(scores(Agnmds, display = "sites"))

Alg1 <- cbind(Algal,Agnmds_coords)

# plot by Cape 
AlgplotCape <- ggplot(data = Alg1, aes(x = NMDS1, y = NMDS2, color = Cape)) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5) +
  labs(title = "Algal Subhabitat NMDS by Cape")
plot(AlgplotCape)

ggsave(filename = "Figures/algalNMDS.png", 
       plot = AlgplotCape , width = 8, height = 6, dpi = 300)

# plot by Site
AlgplotSite <- ggplot(data = Alg1, aes(x = NMDS1, y = NMDS2, color = SiteCode)) +
  geom_point(size = 1) +
  stat_ellipse(linewidth = .5)  +
  labs(title = "Algal Subhabitat NMDS by Site")
plot(AlgplotSite)

ggsave(filename = "Figures/algalsiteNMDS.png", 
       plot = AlgplotSite , width = 8, height = 6, dpi = 300)
