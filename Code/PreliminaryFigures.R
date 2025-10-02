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
# add column for Empty:Present Pits
urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)

#add column for mean size

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))

# don't look at AZ-- urchin-dominated zone only. 
OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))
# add nonpits together

OnlyUrch <- OnlyUrch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

PitUrch <- urch %>%
  filter(Zone %in% c("UPZ"))

urch2024<- urch %>%
  filter(Year %in% c("2024"))

PitUrch2024 <- urch2024 %>%
  filter(Zone %in% c("UPZ"))

urch2025<- urch %>%
  filter(Year %in% c("2025"))

PitUrch2025 <- urch2025 %>%
  filter(Zone %in% c("UPZ"))
################################################################################
#Size diffs 
################################################################################
# Between sub-habitats 

OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

t.test(MeanTest ~ Zone, data = OnlyUrch)
ggplot(OnlyUrch, aes(x = Zone, y = MeanTest)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Size Differences",
       x = "Type of Subhabitat",
       y = "Mean Urchin Test Size (cm)") +
  theme_minimal()

# between years (can't do yet)
ggplot(OnlyUrch, aes(x = factor(Year), y = MeanTest)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Size Differences",
       x = "Year",
       y = "Mean Urchin Test Size (cm)") +
  theme_minimal()

################################################################################
#Major diffs between years
################################################################################
# total urchins in urchin-dominated plots 
t.test(TotalUrchins ~ factor(Year), data = OnlyUrch)
totalurch <- ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Count Year Differences",
       x = "Year",
       y = "Total Urchins") +
  theme_minimal()

ggsave(filename = "Figures/TotalUrchin.png", 
       plot = totalurch , width = 8, height = 6, dpi = 300)


# algal canopy in urchin-dominated plots

t.test(Total.Canopy ~ factor(Year), data = OnlyUrch)
ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Canopy Year Differences",
       x = "Year",
       y = "Total Canopy") +
  theme_minimal()

# changes in juveniles (yes!)

t.test(JuvenileUrchins ~ factor(Year), data = OnlyUrch)
juvies <- ggplot(OnlyUrch, aes(x = factor(Year), y = JuvenileUrchins)) +
  geom_boxplot(fill = "violet") +
  labs(title = "Juvenile Count Differences",
       x = "Year",
       y = "Number of Juveniles per Plot") +
  theme_minimal()

ggsave(filename = "Figures/juvies.png", 
       plot = juvies , width = 8, height = 6, dpi = 300)

# changes in numbers of pits 

t.test(PitsPresent ~ factor(Year), data = OnlyUrch)
ggplot(OnlyUrch, aes(x = factor(Year), y = PitsPresent)) +
  geom_boxplot(fill = "violet") +
  labs(title = "Pits by Year",
       x = "Year",
       y = "Number of Pits present in a plot") +
  theme_minimal()

aov(PitsPresent ~ SiteCode, data = PitUrch)
ggplot(PitUrch, aes(x = SiteCode, y = PitsPresent)) +
  geom_boxplot(fill = "violet") +
  labs(title = "Pits by Site",
       x = "Site",
       y = "Number of Pits present in a plot") +
  theme_minimal()

#2024
ggplot(PitUrch2024, aes(x = SiteCode, y = PitsPresent)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Pits by Site 2024",
       x = "Site",
       y = "Number of Pits present in a plot 2024") +
  theme_minimal()
#2025

ggplot(PitUrch2025, aes(x = SiteCode, y = PitsPresent)) +
  geom_boxplot(fill = "violet") +
  labs(title = "Pits by Site 2025",
       x = "Site",
       y = "Number of Pits present in a plot 2025") +
  theme_minimal()

#empty pits and canopy (no effect)

ggplot(PitUrch, aes(x = EmptyPits, y = Total.Canopy)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Empty Pits vs Kelp Canopy',
       x = 'Empty Pits Present', y = 'Total Canopy (%)')

lm_ratio <- lm(Total.Canopy ~ EmptyPits, data = PitUrch)

anova(lm_ratio)

ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy.png", 
       plot = , width = 8, height = 6, dpi = 300)



################################################################################
# linear regression canopy and urchins 
################################################################################
#for all urchins
ggplot(OnlyUrch, aes(x=TotalUrchins, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Total Urchins to Total Canopy',
       x='Urchin Count', y='Total Canopy (percent)')

lm_model <- lm(Total.Canopy ~ TotalUrchins, data = OnlyUrch)

plot(lm_model)#inspect the resids vs fits to check model- mostly oK
summary(lm_model)
anova(lm_model)

# by cape
ggplot(OnlyUrch, aes(x=TotalUrchins, y=Total.Canopy, color = Cape)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Total Urchins to Total Canopy by Cape',
       x='Urchin Count', y='Total Canopy (percent)')

# for pitted urchins

PitCanopy <- ggplot(OnlyUrch, aes(x=PittedUrchins, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Pitted Urchins to Total Canopy',
       x='Pitted Urchin Count', y='Total Canopy (percent)')

ggsave(filename = "Figures/Pitted_Canopy.png", 
       plot = PitCanopy , width = 8, height = 6, dpi = 300)

lm_modelPit <- lm(Total.Canopy ~ PittedUrchins, data = OnlyUrch)
anova(lm_modelPit)

#for non pitted urchins
NotPitCanopy <- ggplot(OnlyUrch, aes(x=NonPit, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Nonpitted Urchins to Total Canopy',
       x='Nonpitted Urchin Count', y='Total Canopy (percent)')

lm_modelNonPit <- lm(Total.Canopy ~ NonPit, data = OnlyUrch)
anova(lm_modelNonPit)

ggsave(filename = "Figures/NonPitted_Canopy.png", 
       plot = NotPitCanopy , width = 8, height = 6, dpi = 300)

#### Ratio of pits in pit zones #############################################

# don't look at AZ or NPZ-- urchin-PIT dominated zone only. 

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

# combine plots together ####################################################
library(patchwork)
plot1 <- ggplot(OnlyUrch, aes(x = PittedUrchins, y = Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Pitted Urchins vs Kelp Canopy')

plot2 <- ggplot(OnlyUrch, aes(x = NonPit, y = Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Nonpitted Urchins vs Kelp Canopy')

plot1 + plot2

################################################################################
# 
################################################################################



                
                

