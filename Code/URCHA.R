library(tidyverse)
library(dplyr)
library(vegan)

urch <- read.csv("Data/UrchinData2024.csv")
#hi Delaney! - Ella
#try 2

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

#ANOVA test of mean test size per site
urch_aov <- aov(MeanTest~SiteCode, data=urch)
anova(urch_aov) 
summary(anova(urch_aov))
plot(urch_aov)

boxplot(MeanTest ~ SiteCode, data=urch, col='lavender', 
        main = "ANOVA test of Mean Test size per Site",
        xlab = "Site Code", ylab = "Mean Test Size (cm)"
)

TukeyHSD(urch_aov, conf.level=.95)

#Canopy cover all sites

canopy_aov <- aov(Total.Canopy~Zone, data=urch)
anova(canopy_aov)

boxplot(Total.Canopy~Zone, data=urch, col='lavender', 
        main = "Percent Cover Canopy Kelp",
        xlab = "Zone", ylab = "Percent Cover Canopy Kelp"
)

TukeyHSD(canopy_aov, conf.level=.95)

# bare rock cover

barerock_aov <- aov(Bare.rock~Zone, data=urch)
anova(barerock_aov)

boxplot(Bare.rock~Zone, data=urch, col='lavender', 
        main = "Percent Cover Bare rock",
        xlab = "Zone", ylab = "Percent Cover Bare rock"
)

TukeyHSD(barerock_aov, conf.level=.95)


ggplot(bothpiturch, aes(x = Zone, y = MeanTest)) +  
  geom_boxplot(fill = "lightblue") +  
  labs(title = "Mean Urchin Test Sizes by Zone", 
       x = "Urchin Subhabitat", 
       y = "Mean size of test urchins in cm") + 
  theme_minimal()

######################Camilla
mean(urch$TotalAttachedDrift)

summary(urch$TotalAttachedDrift)

favstats(urch$TotalAttachedDrift)

onlyalgal <- urch%>%
  filter(Zone %in% c("AZ"))

algpit <- urch %>%
  filter(Zone %in% c("UPZ", "PZ"))

algnonpit <- urch %>%
  filter(Zone %in% c("NPZ"))

algbothurch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ","AZ"))

urch <- urch %>%
  mutate()

canopy_gg <- urch %>%
  filter(Zone %in% c("UPZ", "AZ", "NPZ"))

ggplot(canopy_gg, aes(x = Zone, y = Total.Canopy)) + 
  geom_boxplot(fill = "lavender") + 
  labs(title = "Mean Percent Total Canopy Cover by Zone", 
       x = "Urchin Subhabitat", y = "Mean Percent Total Canopy Cover") + 
  theme_minimal()

ggplot(algbothurch, aes(x = Zone, y = TotalAttachedDrift)) +  
  geom_boxplot(fill = "lavender") +  
  labs(title = "Mean Percentage of Drift Kelp by Zone", 
       x = "Urchin Subhabitat", 
       y = "Mean Percentage of Attached Drift") + 
  theme_minimal()

ggplot(algbothurch, aes(x = Zone, y = RatioPitUrch)) + 
  geom_boxplot(fill = "purple") + 
  labs(title = "Mean Number of Pitted Urchins by Zone", 
       x = "Urchin Subhabitat", 
       y = "Mean Number of Pitted Urchins") + 
  theme_minimal()

#NMDS practice 

Pit <- urch %>%
  filter(Zone %in% c("UPZ")) %>%
  filter(Mastocarpus!="4?") 
Pit$Mastocarpus <-as.numeric(Pit$Mastocarpus)

NonPit <- urch %>%
  filter(Zone %in% c("NPZ"))

# #drop position columns
# pitcom<- Pit %>% select(-c(1:44, 58))
# pitcom <- data.frame(lapply(pitcom, as.numeric))
# 
# urch_matrix <- as.matrix(pitcom)
# 
# 
# #replace NA with 0
# urch_matrix[is.na(urch_matrix)] <- 0
# #remove rows where all are 0
# 
# urch_matrix <- urch_matrix[rowSums(urch_matrix) != 0, ]  # Remove rows where all values are 0
# 
# pit_nmds=metaMDS(urch_matrix, # Our community-by-species matrix
#         k=2) 
# 
# stressplot(pit_nmds)
# 
# plot(pit_nmds)
# ordiplot(pit_nmds,type="n")
# orditorp(pit_nmds,display="species",col="red",air=0.01)
# orditorp(pit_nmds,display="sites",cex=1.25,air=0.01)

#sarahs code
# Subset the data to include columns with RecInx classes

#same as pit com (19-58 community matrix)
pitcom<- Pit %>% 
  select(-44)
 pitcom2 <- pitcom [, 35:56]

 
 #drop row with question mark
 #now this is my community matrix

# Apply square root transformation (I used shade plots and draftsman in primer to visualise different transformations - sqrt seemed good. )
PitSq <- sqrt(pitcom2)


# Calculate Bray-Curtis dissimilarity matrix
PitSim <- vegdist(PitSq, method = "bray")

# Perform NMDS using the Bray-Curtis dissimilarity matrix
Pitnmds <- metaMDS(PitSim, k = 2, trymax = 40)


# Extract NMDS coordinates
Pitnmds_coords <- as.data.frame(scores(Pitnmds, display = "sites"))

# Combine NMDS coordinates with the original metadata 
Pit1 <- cbind(Pit,Pitnmds_coords)

#if you want to flip coords...then 
# RecInxDist_BySiteYear2 <-RecInxDist_BySiteYear1 %>%
#   #mutate(NMDS2 = -1*NMDS2) %>% #invert nMDS 2 so that earlier years are in bottom left of plot
#   mutate(NMDS1 = -1*NMDS1)

Pitplot <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=SiteCode )) +
  geom_point()
plot(Pitplot)

PitplotCape <- ggplot(data=Pit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(PitplotCape)
