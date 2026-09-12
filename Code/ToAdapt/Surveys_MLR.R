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
urch_clean <- read.csv("Data/urch_clean.csv")


# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))


################################################################################
#Upwelling MLR
################################################################################

######### Bring in data ########################################################
beuti <- read.csv("Data/BEUTI_monthly.csv")
cuti <- read.csv("Data/CUTI_monthly.csv")

######### set up ########################################################

######### set up ########################################################

# region, 
# site,
# temperatures,
# El Niño Southern Oscillation, 
# productivity, 
# upwelling indices, 
# substrate
# distance to known urchin barren or kelp forest may all play a role
# ################################################################################
# # Another MLR
################################################################################


