library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)

################################################################################
urch <- read.csv("Data/SURVEY_Urchin_Survey_Data_2025.csv")

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
ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Count Year Differences",
       x = "Year",
       y = "Total Urchins") +
  theme_minimal()

# algal canopy in urchin-dominated plots

t.test(Total.Canopy ~ factor(Year), data = OnlyUrch)
ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Canopy Year Differences",
       x = "Year",
       y = "Total Canopy") +
  theme_minimal()

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

ggplot(OnlyUrch, aes(x=PittedUrchins, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Pitted Urchins to Total Canopy',
       x='Pitted Urchin Count', y='Total Canopy (percent)')

lm_modelPit <- lm(Total.Canopy ~ PittedUrchins, data = OnlyUrch)
anova(lm_modelPit)

#for non pitted urchins
ggplot(OnlyUrch, aes(x=NonPit, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Nonpitted Urchins to Total Canopy',
       x='Nonpitted Urchin Count', y='Total Canopy (percent)')

lm_modelNonPit <- lm(Total.Canopy ~ NonPit, data = OnlyUrch)
anova(lm_modelNonPit)


