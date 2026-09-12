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
urch <- read.csv("Data/WorkingUrchinSurveyData.csv")
