library(tidyverse)
library(dplyr)

urch <- read.csv("Data/UrchinData_2024.csv")

#look at how many pitted urchins
summary(urch$PittedUrchins)

#see the spread of pitted in both UPZ and NPZ
BothPitUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

boxplot(BothPitUrch$PittedUrchins)

#just the NPZ pitted urchins
NPZUrch <- urch %>%
  filter(Zone %in% c("NPZ"))

boxplot(NPZUrch$PittedUrchins)

#ratio of empty pits to full pits

urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
          "UrchinSize2", "UrchinSize3", "UrchinSize4", "UrchinSize5")]))

t.test(MeanTest ~ Zone, data = BothPitUrch)
ggplot(BothPitUrch, aes(x = Zone, y = MeanTest)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Size Differences",
       x = "Type of Subhabitat",
       y = "Mean Urchin Test Size (cm)") +
  theme_minimal()

urch_aov <- aov(MeanTest~SiteCode, data=urch)
anova(urch_aov)

canopy_aov <- aov(Total.Canopy~Zone, data=urch)
anova(canopy_aov)

      