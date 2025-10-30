library(tidyverse)
library(dplyr)
library(vegan)
inslibrary(mosaic)
library(multcompView)
library(ggplot2)

urch <- read.csv("Data/UrchinData2024.csv")

#look at how many pitted urchins
summary(urch$PittedUrchins)

#see the spread of pitted in both UPZ and NPZ
BothPitUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

<<<<<<< HEAD
boxplot(BothPitUrch$PittedUrchins, col='lightblue', 
  main = "Spread of Pitted Urchins in UPZ and NPZ Combined",
  xlab = "", ylab = "Pitted Urchin Count"
)

=======
#are there urchins at SH and YB
capepep <- urch %>%
  filter(SiteCode %in% c("YB", "SH"))

boxplot(BothPitUrch$PittedUrchins)
>>>>>>> 74ab9b149f44a91c37ea8dc225c59814b156c06a

#just the NPZ pitted urchins
NPZUrch <- urch %>%
  filter(Zone %in% c("NPZ"))

boxplot(NPZUrch$PittedUrchins, col='pink',
  main = "Spread of Pitted Urchin Counts in Non-Pit Zones",
  xlab = "", ylab = "Pitted Urchin Count"
)
        

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

# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))


boxplot(MeanTest ~ SiteCode, data=urch, col='lavender', 
        main = "ANOVA test of Mean Test size per Site",
        xlab = "Site Code", ylab = "Mean Test Size (cm)"
)

TukeyHSD(urch_aov, conf.level=.95)

canopy_aov <- aov(Total.Canopy~Zone, data=urch)
anova(canopy_aov)

boxplot(Total.Canopy~Zone, data=urch, col='lavender', 
        main = "Percent Cover Canopy Kelp",
        xlab = "Zone", ylab = "Percent Cover Canopy Kelp"
)

  #why is NPZ in the graph twice?

TukeyHSD(canopy_aov, conf.level=.95)

##########################################################
#Canopy cover all sites- test 4 Sept 2025
##########################################################
#remove AZ- 
OnlyUrch <- urch %>%
  filter(Zone %in% c("NPZ", "UPZ"))

# combine open and crevice into Nonpit

OnlyUrch <- OnlyUrch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

#by site

canopy_aov <- aov(Total.Canopy~SiteCode, data=OnlyUrch)
anova(canopy_aov)

boxplot(Total.Canopy~SiteCode, data=OnlyUrch, col='darkgreen', 
        main = "Percent Cover Canopy Kelp at Each Site",
        xlab = "Site Code", ylab = "Percent Cover Canopy Kelp"
)

TukeyHSD(canopy_aov, conf.level=.95)

##########################################################
#Now with just pitted urchin numbers
##########################################################

#by site

pitted_urch_aov <- aov(PittedUrchins~SiteCode, data=OnlyUrch)
anova(pitted_urch_aov)

# Reorder the levels of sites 
OnlyUrch$SiteCode <- factor(OnlyUrch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))

boxplot(PittedUrchins~SiteCode, data=OnlyUrch, col='lavender', 
        main = "Pitted Urchins by Site",
        xlab = "Site Code", ylab = "Pitted Urchin Abundance"
)

##########################################################
#Now with just non-pitted urchin numbers
##########################################################

#by site

nonpitted_urch_aov <- aov(NonPit~SiteCode, data=OnlyUrch)
anova(nonpitted_urch_aov)

boxplot(NonPit~SiteCode, data=OnlyUrch, col='purple', 
        main = "Nonpitted Urchins by Site",
        xlab = "Site Code", ylab = "Nonpitted Urchin Abundance"
)


TukeyHSD(nonpitted_urch_aov, conf.level=.95)


##########################################################
#end
##########################################################


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

   #Ella: One-way ANOVA + Tukey for the ANOVA - significance
anova_model <- aov(Total.Canopy ~ Zone, data = canopy_gg)
summary(anova_model)

TukeyHSD(anova_model)
tukey <- TukeyHSD(anova_model)

  #Ella: adding A and B to the plot to show significance 
tukey_letters <- multcompLetters4(anova_model, tukey)

      #Turn into a data frame so ggplot can use it, calc mean canopy per zone
letters_df <- data.frame(
  Zone = names(tukey_letters$Zone$Letters),
  Letters = tukey_letters$Zone$Letters
)

zone_means <- canopy_gg %>%
  group_by(Zone) %>%
  summarise(max_cover = max(Total.Canopy))

plot_labels <- merge(zone_means, letters_df, by = "Zone")

      #make new plot w/ letters attached to the max value
ggplot(canopy_gg, aes(x = Zone, y = Total.Canopy)) + 
  geom_boxplot(fill = "lavender") + 
  labs(title = "Mean Percent Total Canopy Cover by Zone", 
       x = "Urchin Subhabitat", y = "Mean Percent Total Canopy Cover") + 
  theme_minimal() +
  geom_text(data = plot_labels, aes(x = Zone, y = max_cover + 5, label = Letters), 
            inherit.aes = FALSE)





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
    #"Error: object 'RatioPitUrch' not found"



#NMDS practice 

#all by zone

urch$Mastocarpus <-as.numeric(urch$Mastocarpus)

Urchcom<- urch %>% 
  select(-44)
Urchcom2 <- Urchcom [, 35:56]
Urchcom2<- Urchcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

UrchSq <- sqrt(Urchcom2)

# Calculate Bray-Curtis dissimilarity matrix
UrchSim <- vegdist(UrchSq, method = "bray")

# Perform NMDS using the Bray-Curtis dissimilarity matrix
Urchnmds <- metaMDS(UrchSim, k = 2, trymax = 40)
Urchnmds_coords <- as.data.frame(scores(Urchnmds, display = "sites"))

# Combine NMDS coordinates with the original metadata 
Urch1 <- cbind(urch,Urchnmds_coords)
UrchplotCape <- ggplot(data=Urch1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(UrchplotCape)

#by site?

WC<- urch %>%
  filter(SiteCode %in% c("WC"))
WC$Mastocarpus <-as.numeric(WC$Mastocarpus)

WCcom<- WC %>% 
  select(-44)
WCcom2 <- WCcom [, 35:56]
WCcom2<- WCcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

WCSq <- sqrt(WCcom2)

# Calculate Bray-Curtis dissimilarity matrix
WCSim <- vegdist(WCSq, method = "bray")

# Perform NMDS using the Bray-Curtis dissimilarity matrix
WCnmds <- metaMDS(WCSim, k = 2, trymax = 40)
WCnmds_coords <- as.data.frame(scores(WCnmds, display = "sites"))

# Combine NMDS coordinates with the original metadata 
WC1 <- cbind(WC,WCnmds_coords)
WCplotCape <- ggplot(data=WC1, aes(x=NMDS1, y=NMDS2, color=Zone )) +
  geom_point()
plot(WCplotCape)

#algae
Alg <- urch %>%
  filter(Zone %in% c("AZ"))
Alg$Mastocarpus <-as.numeric(Alg$Mastocarpus)

algcom<- Alg %>% 
  select(-44)
algcom2 <- algcom [, 35:56]
algcom2<- algcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

AlgSq <- sqrt(algcom2)

# Calculate Bray-Curtis dissimilarity matrix
NonPitSim <- vegdist(NonPitSq, method = "bray")

# Perform NMDS using the Bray-Curtis dissimilarity matrix
NonPitnmds <- metaMDS(NonPitSim, k = 2, trymax = 40)
NonPitnmds_coords <- as.data.frame(scores(NonPitnmds, display = "sites"))

# Combine NMDS coordinates with the original metadata 
NonPit1 <- cbind(NonPit,NonPitnmds_coords)
NonPitplotCape <- ggplot(data=NonPit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(NonPitplotCape)
#nonpit
NonPit <- urch %>%
  filter(Zone %in% c("NPZ"))
NonPit$Mastocarpus <-as.numeric(NonPit$Mastocarpus)

nonpitcom<- NonPit %>% 
  select(-44)
nonpitcom2 <- nonpitcom [, 35:56]
nonpitcom2<- nonpitcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

NonPitSq <- sqrt(nonpitcom2)

# Calculate Bray-Curtis dissimilarity matrix
NonPitSim <- vegdist(NonPitSq, method = "bray")

# Perform NMDS using the Bray-Curtis dissimilarity matrix
NonPitnmds <- metaMDS(NonPitSim, k = 2, trymax = 40)
NonPitnmds_coords <- as.data.frame(scores(NonPitnmds, display = "sites"))

# Combine NMDS coordinates with the original metadata 
NonPit1 <- cbind(NonPit,NonPitnmds_coords)
NonPitplotCape <- ggplot(data=NonPit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(NonPitplotCape)

#if you want to flip coords...then 
# RecInxDist_BySiteYear2 <-RecInxDist_BySiteYear1 %>%
#   #mutate(NMDS2 = -1*NMDS2) %>% #invert nMDS 2 so that earlier years are in bottom left of plot
#   mutate(NMDS1 = -1*NMDS1)


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

Pit <- urch %>%
  filter(Zone %in% c("UPZ")) %>%
  filter(Mastocarpus!="4?") 
Pit$Mastocarpus <-as.numeric(Pit$Mastocarpus)
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

#########################
#linear model for understory algae
##########################
summary(lm(PittedUrchins ~ Understory.algae, data = Pit))

ggplot(Pit, aes(x = PittedUrchins, y = Understory.algae)) + 
  geom_point() + stat_smooth(method = 'lm', se=FALSE) 


summary(lm(PittedUrchins ~ Understory.algae, data = NPZUrch))

ggplot(NPZUrch, aes(x = PittedUrchins, y = Understory.algae, color=Cape)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) 

#try a Poisson model?

poisson_model <- glm(PittedUrchins ~ Understory.algae, data = NPZUrch, family = poisson)
summary(poisson_model)
NPZUrch$predicted <- predict(poisson_model, type = "response")
ggplot(NPZUrch, aes(x = Understory.algae, y = PittedUrchins)) +
  geom_point() +  # Scatterplot of actual data
  geom_line(aes(y = predicted), color = "blue") +  # Fitted Poisson regression line
  labs(title = "Poisson Regression of Pitted Urchins vs Understory Algae",
       x = "Understory Algae",
       y = "Predicted Pitted Urchins")

#what about a 

#Attached Drift

summary(lm(PittedUrchins ~ TotalAttachedDrift, data = Pit)) 

ggplot(Pit, aes(x = PittedUrchins, y = TotalAttachedDrift, col = SiteCode)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) 

#write a .csv file for Boiler urchins only

CapeArago <- urch %>%
  filter(SiteCode %in% c("SC", "SB"))

write.csv(CapeArago,"CapeAragoUrchins.csv")
