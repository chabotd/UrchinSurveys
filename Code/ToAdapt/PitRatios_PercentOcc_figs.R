library(tidyverse)
library(dplyr)
library(vegan)
#library(multcompView)
library(ggplot2)
#library(ggpubr)
#library(FSA)
#library(rcompanion)
library(tweedie)
library(statmod)
#library(cowplot)
#library(patchwork)
#library(maps)
#library(mapdata)
#library(ggrepel)
#library(ggspatial)


################################################################################
#Set Up data frame 
################################################################################
urch<- read.csv("Data/urch_clean.csv")

OnlyUrch <-  urch %>%
  filter(Subhabitat %in% c("NPZ", "UPZ"))


#################percent occupancy figures

summary(OnlyUrch$PercentOccupancy)

percent<- ggplot(OnlyUrch, aes(x = SiteCode, y = PercentOccupancy, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Percent Occupied"
  ) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

#################pit:nonpit ratios per zone

Pit <-  urch %>%
  filter(Subhabitat %in% c( "UPZ"))

summary(Pit$PercentPitted)

percent<- ggplot(Pit, aes(x = SiteCode, y = PercentPitted, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Percent of Pitted urchins in Pit subhabitat"
  ) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

###################nonpit

NonPit <-  urch %>%
  filter(Subhabitat %in% c( "NPZ"))

summary(NonPit$PercentPitted)

percent<- ggplot(NonPit, aes(x = SiteCode, y = PercentPitted, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Percent of Pitted urchins in NonPit subhabitat"
  ) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()


