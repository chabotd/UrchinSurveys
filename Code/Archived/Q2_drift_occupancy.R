library(tidyverse)
#library(dplyr)
library(vegan)
#library(multcompView)
library(ggplot2)
#library(ggpubr)
#library(FSA)
#library(rcompanion)
#library(tweedie)
library(statmod)
library(viridis)
library(kableExtra)
library(cowplot)

################################################################################
#Set Up data frame 
################################################################################
urch <- read.csv("Data/urch_clean.csv")

#Cali first 
#urch$SiteCode <- factor(urch$SiteCode, levels=c("CMS", "CMN", "CP", "WC" , 
#                                                "RP", "CB", "SC", "SB", "SH", 
#                                               "YB", "BB", "FC"))

urch$SiteCode <- factor(urch$SiteCode, levels = c( "FC", "BB", "YB", "SH", "SB", 
                                                   "SC", "CB","RP", "WC", "CP", 
                                                   "CMN", "CMS"))

site_cols <- c(
  "BB" = "#30123BFF",
  "FC" = "#4454C4FF",
  "YB" = "#4490FEFF",
  "SH" = "#1FC8DEFF",
  "SB" = "#29EFA2FF",
  "SC" = "#7DFF56FF",
  "CB" = "#C1F334FF",
  "RP" = "#F1CA3AFF",
  "WC" = "#FE922AFF",
  "CP" = "#EA4F0DFF",
  "CMN" = "#BE2102FF",
  "CMS" = "#7A0403FF"
)

View(urch)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

#remove YB and SH
NoPerpetua <- OnlyUrch %>%
  filter(!(SiteCode %in% c("SH", "YB")))

################################################################################
# Q2 : Percent filled pits and attached drift in UPZ 
################################################################################
# upz only
UPZ <- urch %>%
  filter(Subhabitat== "UPZ")

# UPZ_clean <- UPZ %>% 
#   filter(!is.na(PercentOccupancy),
#          !is.na(TotalAttachedDrift))

glm_tweedie_drift <- glm(
  PercentOccupancy ~ TotalAttachedDrift,
  data = UPZ_clean,
  family = tweedie(var.power = 1.5, link.power = 0)
)

UPZ_clean$PredictedDrift <- predict(glm_tweedie_drift, type = "response")

####################### faceted full set up

DriftPlot <- ggplot(UPZ_clean, aes(x = TotalAttachedDrift, y = PercentOccupancy, color = SiteCode)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedDrift), linewidth = 1) +
  facet_wrap(~ SiteCode) +
  scale_color_manual(values = c("#30123BFF","#4454C4FF" ,"#4490FEFF", "#1FC8DEFF","#29EFA2FF", "#7DFF56FF", "#C1F334FF",
                                "#F1CA3AFF","#FE922AFF" ,"#EA4F0DFF" ,"#BE2102FF", "#7A0403FF")) +
  labs(
    x = "Total Attached Drift per 0.25m²",
    y = "Percent Occupancy",
    color = "Site"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/Q2/DriftPits.png", 
       plot =DriftPlot  , width = 8, height = 6, dpi = 300)

####### Try as a loop for each indiv. plot 
sites2 <- unique(UPZ_clean$SiteCode)

plots2 <- lapply(sites2, function(s) {
  
  dat <- UPZ_clean %>% filter(SiteCode == s)
  
  ggplot(dat, aes(x = TotalAttachedDrift, y = PercentOccupancy, color = SiteCode)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = PredictedDrift), linewidth = 1) +
    scale_color_manual(values = site_cols) +
    labs(
      title = s,
      x = "Total Attached Drift per 0.25m²",
      y = "Percent Occupancy",
      color = "Site"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal()
})


for (i in seq_along(sites2)) {
  ggsave(
    filename = paste0("Figures/Surveys/Q2/", sites2[i], "_pits_drift.png"),
    plot = plots2[[i]],
    width = 10,
    height = 5
  )
}


#############look at sitewide pit occupancies 

UPZ_nopits <- NoPerpetua%>%
  filter(PitsPresent>0)

UPZ_nopits <- UPZ_nopits%>%
  filter(PercentOccupancy<=100)

kruskal.test(PercentOccupancy ~ SiteCode, data = NoPerpetua)

plocc <- ggplot(UPZ_nopits, aes(x = SiteCode, y = PercentOccupancy, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  #facet_wrap(~ SiteCode) +
  labs(
    x = "Site",
    y = "Percent of Pits Occupied by Urchins in 0.25m²"
  ) +
  scale_fill_manual(values = site_cols) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) 

ggsave(filename = "Figures/Surveys/Q1_kelp_subhabitat.png", 
       plot = q1sub , width = 8, height = 6, dpi = 300)

########################## linera model

plot <- ggplot(UPZ_nopits, aes(x=TotalAttachedDrift, y=PercentOccupancy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) +
  labs(title=' Percent Occupancy vs Percent Attached Drift - Site',
       x='Total Drift kelp attached to urchins (% cover)', y='percent of pits filled by urchins')

ggsave("Scat_DriftPitSite.png", Scat_DriftPitSite)

lm_model10 <- lm(PercentOccupancy ~ TotalAttachedDrift, data = UPZ_nopits)

anova(lm_model10)

summary(lm_model10)
cor.test(UPZ_nopits$TotalAttachedDrift, UPZ_nopits$PercentOccupancy, method="pearson")

ggsave(filename = "Figures/Surveys/Q2_drift2.png", 
       plot = plot , width = 8, height = 6, dpi = 300)