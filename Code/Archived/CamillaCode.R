setwd("C:/Users/camil/Downloads/URCHA")
library(tidyverse)

urch <- read.csv("UrchinData_2024_Clean.csv")

View(urch)


#by Site FOR DRIFT
Scat_DriftPitSite <- ggplot(urch, aes(x=PittedUrchins, y=TotalAttachedDrift, color = SiteCode)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) +
  labs(title='Percent Attached Drift vs Pitted Urchin Frequencies - Site',
       x='pitted urchin frequency', y='attached drift (percent)')
plot(Scat_DriftPitSite)
ggsave("Scat_DriftPitSite.png", Scat_DriftPitSite)

lm_model10 <- lm(TotalAttachedDrift ~ PittedUrchins * SiteCode, data = urch)

anova(lm_model10)

summary(lm_model10)

#All pits vs drift FOR DRIFT
Scat_DriftPitCape <-ggplot(urch, aes(x=PittedUrchins, y=TotalAttachedDrift)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Percent Attached Drift vs Pitted Urchin Frequencies - Cape',
       x='pitted urchin frequency', y='attached drift (percent)')
plot(Scat_DriftPit)
ggsave("Scat_DriftPit.png", Scat_DriftPit)


#by Cape FOR DRIFT
Scat_DriftPitCape <- ggplot(urch, aes(x=PittedUrchins, y=TotalAttachedDrift, color = Cape)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Percent Attached Drift vs Pitted Urchin Frequencies - Cape',
       x='pitted urchin frequency', y='attached drift (percent)')
plot(Scat_DriftPitCape)
ggsave("Scat_DriftPitCape.png", Scat_DriftPitCape)

lm_model20 <- lm(TotalAttachedDrift ~ PittedUrchins * Cape, data = urch)

summary(lm_model20)
anova(lm_model20)




        
#omit AZ




ggplot(bothpiturch, aes(x=PittedUrchins, y=TotalAttachedDrift, color = Cape)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Comparing Percent Attached Drift to Pitted Urchin Frequencies by Cape',
       x='pitted urchin frequency', y='attached drift (percent)')

lm_model200 <- lm(TotalAttachedDrift ~ PittedUrchins * Cape, data = bothpiturch)
plot(lm_model200)#inspect the resids vs fits to check model- mostly oK

summary(lm_model200)
library(car)
anova(lm_model200)
