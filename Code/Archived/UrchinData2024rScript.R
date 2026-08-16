setwd("C:/Users/noram/Documents/URSA")
library(tidyverse)

urch <- read.csv("UrchinData_2024.csv")
view(urch)

summary(urch$PittedUrchins)
summary(urch$TotalUrchins)

BothPitUrch <- urch %>%
  filter(Zone%in%c("UPZ","NPZ"))
boxplot(BothPitUrch$PittedUrchins)

OpenUrch <- urch %>% 
  filter(Zone%in%c("NPZ"))
boxplot(OpenUrch$PittedUrchins)

urch <- urch %>% 
  mutate(RatioPits=EmptyPits/PitsPresent)

View(urch)

urch <- urch %>%
  mutate(MeanTest= rowMeans(urch[,c("UrchinSize1","UrchinSize2","UrchinSize3","UrchinSize4", "UrchinSize5")]))
View(urch)

#How to do a t test
t.test(MeanTest ~ Zone,data=BothPitUrch)

ggplot(BothPitUrch, aes(x = Zone, y = MeanTest)) + geom_boxplot(fill = "lavender") + labs(
                      x = "Type of Subhabitat", 
                      y = "Mean Test Size in Centimeters") + theme_minimal()

#anova for sites
urch_aov <- aov(MeanTest ~ SiteCode, data=urch)
anova(urch_aov) 

#finding total canopy across the three zones
canopy_aov <- aov(Total.Canopy~Zone, data=urch)
anova(canopy_aov)

#ANOVA test of mean test size per site
urch_aov <- aov(MeanTest~SiteCode, data=urch)
anova(urch_aov) 
summary(anova(urch_aov))
plot(urch_aov)

boxplot(MeanTest ~ SiteCode, data=urch, col='lavender', 
        main = "ANOVA of Mean Test size per Site",
        xlab = "Site Code", ylab = "Mean Test Size (cm)"
        )
urch_aov <- aov(MeanTest~SiteCode, data=urch)
anova(urch_aov) 
summary(anova(urch_aov))

#filtering, only looking at columns with the exact data and two ANOVAs
Pit <- urch %>%
  filter(Zone %in% c("UPZ"))
piturch_aov <- aov(MeanTest~SiteCode, data=Pit)
anova(piturch_aov) 
summary(anova(piturch_aov))

#organizing by latitude
Pit$SiteCode <- factor(Pit$SiteCode, 
                       levels = Pit$SiteCode[order(Pit$Latitude)])

NonPit$SiteCode <- factor(NonPit$SiteCode, levels = c("BB", "FC", "YB", "SH",
                                                 "SB", "SC", 
                                                "CB", "RP", "WC", 
                                                "CP", "CMN", "CMS"))

boxplot(MeanTest ~ SiteCode, data=Pit, ylim= c(3, 8), col='lavender', 
        xlab = "Site Code", ylab = "Mean Test Size (cm)"
)

NonPit <- urch %>%
  filter(Zone %in% c("NPZ")) 
nonpiturch_aov <- aov(MeanTest~SiteCode, data=NonPit)
anova(nonpiturch_aov) 
summary(anova(nonpiturch_aov))

boxplot(MeanTest ~ SiteCode, data=NonPit, col='lavender', 
        xlab = "Site Code", ylab = "Mean Test Size (cm)"
)
#tukey test? 
#CMS-CB  -1.34590909 -2.42406091 -0.26775727 0.0034352
#WC-CMS   0.93617647 -0.01130738  1.88366033 0.0558820
#FC-CB   -1.81890909 -3.36804336 -0.26977482 0.0081797

TukeyHSD(urch_aov, conf.level = 0.95)

#for testing cleaned code
boxplot(Total.Canopy ~ Zone, data=urch, col='lavender', 
                   main = "Total Canopy per Zone",
                   xlab = "Zone", ylab = "Canopy")
t.test(Total.Canopy ~ Zone, data=urch)

#linear regression
model = lm(MeanTest ~ Total.Canopy, data=BothPitUrch)
summary(model)

BothPitUrch <- urch %>%
  filter(Zone%in%c("UPZ","NPZ"))

boxplot(BothPitUrch$PittedUrchins)
library(ggplot2)

ggplot(data=BothPitUrch, aes(x = Total.Canopy, y = MeanTest)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Linear Regression: Total Canopy vs. Mean Test ",
       x = "Percent of Total Canopy Coverage",
       y = "Mean Test Size (cm)")

#nonpit zones
OpenUrch <- urch %>% 
  filter(Zone%in%c("NPZ"))
ggplot(data=OpenUrch, aes(x = Total.Canopy, y = MeanTest)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(
       x = "Percent of Total Canopy Coverage",
       y = "Mean Test Size (cm)")
NPmodel = lm(MeanTest ~ Total.Canopy, data=OpenUrch)
summary(NPmodel)

#pit zones
Pit <- urch %>%
  filter(Zone %in% c("UPZ"))
ggplot(data=Pit, aes(x = Total.Canopy, y = MeanTest)) +
  geom_point() + scale_x_continuous(limits = c(0, 50)) +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(
       x = "Percent of Total Canopy Coverage",
       y = "Mean Test Size (cm)")
Pmodel = lm(MeanTest ~ Total.Canopy, data=Pit)
summary(Pmodel)

#Summary statistics for Pmodel linear reg.
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   4.332242   0.100029  43.310  < 2e-16 ***
  Total.Canopy -0.024399   0.008747  -2.789  0.00661 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7375 on 79 degrees of freedom
(39 observations deleted due to missingness)
Multiple R-squared:  0.08966,	Adjusted R-squared:  0.07814 
F-statistic: 7.781 on 1 and 79 DF,  p-value: 0.006615

#summary stats for NPmodel linear reg
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.706129   0.123843  38.001   <2e-16 ***
  Total.Canopy 0.011328   0.006267   1.808   0.0747 .  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.977 on 75 degrees of freedom
(13 observations deleted due to missingness)
Multiple R-squared:  0.04175,	Adjusted R-squared:  0.02898 
F-statistic: 3.268 on 1 and 75 DF,  p-value: 0.07466

#summary stats for non-pit ANOVA
Response: MeanTest
Df Sum Sq Mean Sq F value    Pr(>F)    
SiteCode   8 33.590  4.1987  6.9443 1.195e-06 ***
  Residuals 68 41.115  0.6046 

#summary for pitted ANOVA
Response: MeanTest
Df Sum Sq Mean Sq F value   Pr(>F)   
SiteCode  10 13.961 1.39609  2.9406 0.003887 **
  Residuals 70 33.234 0.47477





