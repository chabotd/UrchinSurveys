library(tidyverse)
library(dplyr)
library(vegan)

urch <- read.csv("Data/UrchinData2024.csv")

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

#NMDS practice 

Pit <- urch %>%
  filter(Zone %in% c("UPZ"))

#drop indiv. cols
pitcom <- Pit %>% 
  select(-Initials.Entered, Date.Entered, SiteCode)

#drop position columns
pitcom<- Pit %>% select(-c(1:13, 15:44, 58))
pitcom <- data.frame(lapply(pitcom, as.numeric))

urch_matrix <- as.matrix(pitcom)


#replace NA with 0
urch_matrix[is.na(urch_matrix)] <- 0
#remove rows where all are 0

urch_matrix <- urch_matrix[rowSums(urch_matrix) != 0, ]  # Remove rows where all values are 0

pit_nmds=metaMDS(urch_matrix, # Our community-by-species matrix
        k=2) 

stressplot(pit_nmds)

plot(pit_nmds)
ordiplot(pit_nmds,type="n")
orditorp(pit_nmds,display="species",col="red",air=0.01)
orditorp(pit_nmds,display="sites",cex=1.25,air=0.01)