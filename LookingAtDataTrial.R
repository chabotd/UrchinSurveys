library(tidyverse)
library(dplyr)

urch <- read.csv("Data/UrchinData_Trial.csv")
View(urch)

summary(urch$Count.pitted.urchins)
Pit.urch <- urch$Count.pitted.urchins

boxplot(Pit.urch, filter = )

pit.urch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

pit.urch.2 <- urch %>%
  filter(Zone %in% c("NPZ"))

boxplot(pit.urch.2$Count.pitted.urchins)
        