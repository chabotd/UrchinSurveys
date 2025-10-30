library(tidyverse)
library(dplyr)
library(vegan)

urch <- read.csv("Data/UrchinData2024.csv")

#look at how many pitted urchins
summary(urch$PittedUrchins)

#see the spread of pitted in both UPZ and NPZ
BothPitUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

#are there urchins at SH and YB
capepep <- urch %>%
  filter(SiteCode %in% c("YB", "SH"))

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
