setwd("C:/Users/shant/Documents/ursa_urchin_data")
library(tidyverse)

urch <- read.csv("UrchinData_2024_Clean.csv")
View(urch)

summary(urch$PittedUrchins)
summary(urch$TotalUrchins)
summary(urch$JuvenileUrchins)

BothPitUrch <- urch %>%
  filter(Zone %in%c("UPZ"))

boxplot(BothPitUrch$PittedUrchins,
        col = "lightblue")

OpenUrch <- urch %>%
  filter(Zone %in%c("NPZ"))

boxplot(OpenUrch$PittedUrchins,
        col = "lavender")

urch <- urch %>%
  mutate(RatioPits=EmptyPits/PitsPresent)

View(urch)

#nonpit zone nmds

NonPit <- urch %>%
  filter(Zone %in% c("NPZ"))

NonPit$Mastocarpus <-as.numeric(NonPit$Mastocarpus)


nonpitcom<- NonPit %>% 
  select(-44)
nonpitcom2 <- nonpitcom [, 35:56]
nonpitcom2<- nonpitcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

NonPitSq <- sqrt(nonpitcom2)

NonPitSim <- vegdist(NonPitSq, method = "bray")

NonPitnmds <- metaMDS(NonPitSim, k = 2, trymax = 40)

NonPitnmds_coords <- as.data.frame(scores(NonPitnmds, display = "sites"))

NonPit1 <- cbind(NonPit,NonPitnmds_coords)

NonPitplotCape <- ggplot(data=NonPit1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(NonPitplotCape)

#algal zone nmds

Algal <- urch %>%
  filter(Zone %in% c("AZ"))

Algal$Mastocarpus <-as.numeric(Algal$Mastocarpus)

algalcom<- Algal %>% 
  select(-44)
algalcom2 <- algalcom [, 35:56]
algalcom2<- algalcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

AlgalSq <- sqrt(algalcom2)

AlgalSim <- vegdist(AlgalSq, method = "bray")

Algalnmds <- metaMDS(AlgalSim, k = 2, trymax = 40)

Algalnmds_coords <- as.data.frame(scores(Algalnmds, display = "sites"))

Algal1 <- cbind(Algal,Algalnmds_coords)

AlgalplotCape <- ggplot(data=Algal1, aes(x=NMDS1, y=NMDS2, color=Cape )) +
  geom_point()
plot(AlgalplotCape)

#by zone

urch$Mastocarpus <-as.numeric(urch$Mastocarpus)

urchcom<- urch %>% 
  select(-44)
urchcom2 <- urchcom [, 35:56]
urchcom2<- urchcom2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

urchSq <- sqrt(urchcom2)

urchSim <- vegdist(urchSq, method = "bray")

urchnmds <- metaMDS(urchSim, k = 2, trymax = 40)

urchnmds_coords <- as.data.frame(scores(urchnmds, display = "sites"))

urch1 <- cbind(urch,urchnmds_coords)

urchplotCape <- ggplot(data=urch1, aes(x=NMDS1, y=NMDS2, color=Zone )) +
  geom_point()+
  stat_ellipse(type = "t", linetype = "solid", linewidth = 0.5) 
  scale_color_manual(values = c("#FF0000","#FF9933","#00CC33",
                                            "#00CCFF","#0000FF","#FF33FF"))

  
plot(urchplotCape)

