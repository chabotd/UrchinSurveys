library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)

################################################################################
urch <- read.csv("Data/UrchinSurveys.csv")

# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))

# add column for TotalPits and Ratio of Empty:Full
urch <- urch %>% mutate(RatioPits = EmptyPits/TotalPits)
urch <- urch %>% mutate(TotalPits = EmptyPits + PittedUrchins)

# add column for percent occupancy 
urch <- urch %>%
  mutate(
    PercentOccupancy = (PittedUrchins / TotalPits) * 100
  )

#add column for nonpits
urch <-urch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

#add column for mean size
urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))

# don't look at AZ-- urchin-dominated zone only. 
OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

PitUrch <- urch %>%
  filter(Zone %in% c("UPZ"))

#### Ratio of pits in pit zones #############################################

glm_model <- glm(Total.Canopy + 0.01 ~ RatioPits, data =
                   PitUrch, family = gaussian(link = "log"))
summary(glm_model)

PitUrch$CanopyAdj <- PitUrch$Total.Canopy + 0.01
plot1 <- ggplot(PitUrch, aes(x = RatioPits, y = CanopyAdj)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = 
                gaussian(link = "log")), se = TRUE, color = "darkgreen") +
  labs(title = "Empty:Present Pit Ratio vs Kelp Canopy",
       x = "Empty:Present Pit Ratio", y = "Adjusted Canopy (%)") +
  theme_minimal()

ggsave(filename = "Figures/glmEmptyPits_vs_KelpCanopy.png", 
       plot = plot1 , width = 8, height = 6, dpi = 300)

#percent occupancy 

glm_model2 <- glm(Total.Canopy + 0.01 ~ PercentOccupancy, data =
                   PitUrch, family = gaussian(link = "log"))
summary(glm_model2)

PitUrch$CanopyAdj <- PitUrch$Total.Canopy + 0.01
plot2 <- ggplot(PitUrch, aes(x = PercentOccupancy, y = CanopyAdj)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = 
                                                   gaussian(link = "log")), se = TRUE, color = "darkgreen") +
  labs(title = "Percent of Pits Occupied by Urchins vs Kelp Canopy",
       x = "Percent of Pits Occupied", y = "Adjusted Canopy (%)") +
  theme_minimal()

ggsave(filename = "Figures/glmEmptyPits_vs_KelpCanopy.png", 
       plot = plot1 , width = 8, height = 6, dpi = 300)

########################################################################
#below is working - not sure if still needed.
########################################################################
ratio <- ggplot(PitUrch, aes(x = RatioPits, y = Total.Canopy)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy',
       x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')

lm_ratio <- lm(Total.Canopy ~ RatioPits, data = PitUrch)
summary(lm_ratio)
anova(lm_ratio)

ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy.png", 
       plot = ratio , width = 8, height = 6, dpi = 300)


Pit2024<- PitUrch %>%
  filter(Year %in% c("2024"))

ratio2024 <- ggplot(Pit2024, aes(x = RatioPits, y = Total.Canopy)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy 2024',
       x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')

ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy2024.png", 
       plot = ratio2024 , width = 8, height = 6, dpi = 300)

lm_ratio2024 <- lm(Total.Canopy ~ RatioPits, data = Pit2024)
anova(lm_ratio2024)

Pit2025<- PitUrch %>%
  filter(Year %in% c("2025"))

ratio2025 <- ggplot(Pit2025, aes(x = RatioPits, y = Total.Canopy)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy 2025',
       x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')

ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy2025.png", 
       plot = ratio2025 , width = 8, height = 6, dpi = 300)

lm_ratio2025 <- lm(Total.Canopy ~ RatioPits, data = Pit2025)
anova(lm_ratio2025)

########checking more ######################################################
# Histogram of Total.Canopy for each year
check1 <- ggplot(Pit2024, aes(x = Total.Canopy)) + 
  geom_histogram(binwidth = 5, fill = "skyblue") + 
  labs(title = "2024 Canopy Distribution")

ggsave(filename = "Figures/Check1.png", 
       plot = check1 , width = 8, height = 6, dpi = 300)

check2 <-ggplot(Pit2025, aes(x = Total.Canopy)) + 
  geom_histogram(binwidth = 5, fill = "salmon") + 
  labs(title = "2025 Canopy Distribution")

ggsave(filename = "Figures/Check2.png", 
       plot = check2 , width = 8, height = 6, dpi = 300)

# Density plot for RatioPits
check3 <-ggplot(Pit2024, aes(x = RatioPits)) + 
  geom_density(fill = "lightgreen") + 
  labs(title = "2024 RatioPits Distribution")

ggsave(filename = "Figures/Check3.png", 
       plot = check3 , width = 8, height = 6, dpi = 300)

check4 <-ggplot(Pit2025, aes(x = RatioPits)) + 
  geom_density(fill = "orange") + 
  labs(title = "2025 RatioPits Distribution")

ggsave(filename = "Figures/Check4.png", 
       plot = check4 , width = 8, height = 6, dpi = 300)

################################################################################
#try another type of test#######################################################

glm_model <- glm(Total.Canopy + 0.01 ~ RatioPits, data =
                   Pit2024_clean, family = gaussian(link = "log"))
summary(glm_model)

Pit2024_clean$CanopyAdj <- Pit2024_clean$Total.Canopy + 0.01
ggplot(Pit2024_clean, aes(x = RatioPits, y = CanopyAdj)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = gaussian(link = "log")), se = TRUE, color = "blue") +
  labs(title = "GLM Fit: Empty:Present Pit Ratio vs Kelp Canopy",
       x = "Empty:Present Pit Ratio", y = "Adjusted Canopy (%)") +
  theme_minimal()


Pit2024_clean <- Pit2024 %>% filter(is.finite(RatioPits) & is.finite(Total.Canopy))
