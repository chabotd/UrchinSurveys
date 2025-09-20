library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)

urch <- read.csv("Data/SURVEY_Urchin_Survey_Data_2025.csv")

#look at how many pitted urchins
summary(urch$PittedUrchins)
