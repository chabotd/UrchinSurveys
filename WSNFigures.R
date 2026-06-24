library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)
library(ggpubr)
library(FSA)
library(rcompanion)
library(tweedie)
library(statmod)
library(cowplot)
library(patchwork)
library(maps)
library(mapdata)
library(ggrepel)
library(ggspatial)


################################################################################
#Set Up data frame 
################################################################################
urch <- read.csv("Data/UrchinSurveys.csv")

# Reorder the levels of sites if on x axis 
#urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
#                                                  "SC", "CB","RP", "WC", "CP", 
 #                                                 "CMN", "CMS"))
# Reorder the levels of sites if on y axis 
urch$SiteCode <- factor(urch$SiteCode, levels=c("CMS", "CMN", "CP", "WC" , 
                                                "RP", "CB", "SC", "SB", "SH", 
                                                "YB", "BB", "FC"))
# add column for Empty:Present Pits
urch<- urch %>%
  rename(TotalPits = PitsPresent)
# urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)

#rename zone to subhabitat

urch<- urch %>%
  rename(Subhabitat = Zone)

#Remove CMN and CMS if not using Cali data 

urch<- urch %>%
  filter(!(SiteCode %in% c("CMS", "CMN")))

# add column for TotalPits and Ratio of Empty:Full
urch <- urch %>% mutate(RatioPits = EmptyPits/TotalPits)
urch <- urch %>% mutate(TotalPits = EmptyPits + PittedUrchins)

# add column for percent occupancy 
urch <- urch %>%
  mutate(
    PercentOccupancy = (PittedUrchins / TotalPits) * 100
  )

urch$Connectivity <- with(urch, case_when(
  SiteCode %in% c("CB", "WC", "CP", "RP") ~ "Susceptible",
  SiteCode %in% c("SC", "SB", "BB", "FC") ~ "Unlikely",
  SiteCode %in% c("YB", "SH") ~ "None",
  TRUE ~ NA_character_  # for any sites not listed
))

#and add levels to combine none and unlikely - 
#this one may need to be adjusted as I change figures

urch$Connectivity <- as.character(urch$Connectivity)
urch$Connectivity[urch$Connectivity %in% c("Unlikely", "None")] <- "Not Likely"
urch$Connectivity <- factor(urch$Connectivity)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

# add nonpits together
OnlyUrch <- OnlyUrch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

OnlyUrch$PercentNonPitted <- (OnlyUrch$NonPit/ OnlyUrch$TotalUrchins) * 100

# pitted + crevice together 

OnlyUrch <- OnlyUrch %>% mutate(Cryptic = PittedUrchins + CreviceUrchins)

# pitted + crevice together 


################################################################################
######## for if we want to look only at certain years or certain habitats 
################################################################################

PitUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ"))

urch2024<- urch%>%
filter(Year %in% c("2024"))

PitUrch2024 <- urch2024 %>%
  filter(Subhabitat %in% c("UPZ"))

urch2025<- urch %>%
filter(Year %in% c("2025"))

PitUrch2025 <- urch2025 %>%
  filter(Subhabitat %in% c("UPZ"))

urch2024 <- OnlyUrch%>%
  filter(Year %in% c("2024"))

urch2025 <- OnlyUrch%>%
  filter(Year %in% c("2025"))
################################################################################
######## Map
################################################################################

#1. import 

SiteList <- read.csv("Data/IntertidalUrchinSitesOregon.csv")


long <- SiteList$Longitude
lat <- SiteList$Latitude
Site <-SiteList$Site
Reg <-SiteList$Region
Reg<-factor(Reg, levels=c('Cape Foulweather','Cape Perpetua','Cape Arago',
                          'Cape Blanco', 'Southern Oregon'))

SiteCode <-SiteList$SiteCode

states <- map_data("state")

map <-ggplot(data = SiteList, 
             aes(x=long, 
                 y = lat, color = Susceptibility)) +   
  geom_text_repel(
    aes(x = long, y = lat, label = SiteCode, color = Susceptibility),
    direction = "y", 
    nudge_x = -125, 
    min.segment.length = 0, 
    seed = 42, 
    box.padding = 0.5,
    size = 5
  ) +
  geom_polygon(data = states, color = "black", fill = "gray90", aes(group=group)) + 
  geom_point(size = 3, shape = 16) +
  scale_color_manual(values= c("#577590","#90be6d","#f8961e", "#f94144", "#555599")) +
  coord_fixed(1.3) +
  theme_classic()+
  labs(x="Longitude", y = "Latitude") +
  scale_x_continuous(breaks = seq(-126,-116,2)) +
  scale_y_continuous(breaks = seq(31,47,1)) +
  coord_map(xlim = c(-126,-116), ylim = c(40,47))+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


plot(map)

ggsave(filename = "Figures/SusepMap.png", 
       plot =map  , width = 8, height = 6, dpi = 300)

################################################################################
######## Figure 1: Cryptic and Open Urchin Densities by Site
################################################################################

wilcox.test(OnlyUrch$OpenUrchins, OnlyUrch$Cryptic)

OnlyUrch_long <- pivot_longer(OnlyUrch, cols = c(OpenUrchins, Cryptic), 
                              names_to = "Type", values_to = "Count")

ggplot(OnlyUrch_long, aes(x = Type, y = Count, fill = Type)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(x = "Urchin Type", y = "Density (count per 0.25m²)") +
  theme_minimal()


#Non-Cryptic

kruskal.test(NonPit ~ SiteCode, data = OnlyUrch)

pl1 <- ggplot(OnlyUrch, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/UrchinDensitiesbySiteOpen.png", 
       plot =pl1  , width = 8, height = 6, dpi = 300)

##########################################################
#Cryptic
##########################################################

kruskal.test(Cryptic ~ SiteCode, data = OnlyUrch)

pl2 <- ggplot(OnlyUrch, aes(x = SiteCode, y = Cryptic, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/UrchinDensitiesbySiteCrpytic.png", 
       plot =pl2  , width = 8, height = 6, dpi = 300)

###############################################################################
######## Figure 2: Urchin migration possible vs unlikely 
################################################################################

#non pit
kruskal.test(NonPit ~ Connectivity, data = OnlyUrch)

pl3 <- ggplot(OnlyUrch, aes(x = Connectivity, y = NonPit, fill= Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Urchin Migration Possibility",
    y = "Non- Cryptic Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = c("NotLikely" = "orange2", "Susceptible" = "darkolivegreen4")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/OpenConnectivity.png", 
       plot =pl3  , width = 8, height = 6, dpi = 300)
# cryptic 

kruskal.test(Cryptic ~ Connectivity, data = OnlyUrch)

pl4 <-ggplot(OnlyUrch, aes(x = Connectivity, y = Cryptic, fill = Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Urchin Migration Possibility",
    y = "Cryptic Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = c("NotLikely" = "orange2", "Susceptible" = "darkolivegreen4")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/CrpyConnectivity.png", 
       plot =pl4  , width = 8, height = 6, dpi = 300)


################################################################################
######## Figure 2: GLM of Canopy and Urchin Count by Subhabitat
################################################################################

# by subhabitat 
# make model

glm_tweedie <- glm(Total.Canopy ~ TotalUrchins * Subhabitat,
                   data = OnlyUrch,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie)


OnlyUrch$PredictedCanopy <- predict(glm_tweedie, type = "response")

ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = Subhabitat)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth= 1) +
  labs(
    x = "Urchin Density",
    y = "KelpCanopy (%)",
    color = "Subhabitat"
  ) +
  theme_minimal()

pred_data <- OnlyUrch %>%
  select(TotalUrchins, Subhabitat) %>%
  distinct() %>%
  arrange(Subhabitat, TotalUrchins)

preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

# Create a named vector for relabeling
subhabitat_labels <- c(
  NPZ = "Non-Pit Urchin-Dominated Subhabitat",
  UPZ = "Pit Urchin-Dominated Subhabitat"
)

# Plot
twed <- ggplot() +
  geom_point(data = OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = Subhabitat), alpha = 0.6) +
  geom_line(data = pred_data, aes(x = TotalUrchins, y = fit), color = "black", size = 1) +
  geom_ribbon(data = pred_data, aes(x = TotalUrchins, ymin = lower, ymax = upper), fill = "gray", alpha = 0.3) +
  facet_wrap(~ Subhabitat, labeller = labeller(Subhabitat = subhabitat_labels)) +
  labs(
    x = "Urchin Density",
    y = "Kelp Canopy (% cover)"
  ) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    strip.text = element_text(size = 12, face = "bold")  # Resize facet labels
  )

ggsave(filename = "Figures/TweedieWSN.png", 
       plot =twed  , width = 8, height = 6, dpi = 300)

################################################################################
######## Figure 3: Kelp Canopy by Site
################################################################################

kruskal.test(Total.Canopy ~ SiteCode, data = OnlyUrch)
wilcox.test(Total.Canopy ~ factor(Year), data = OnlyUrch)

inset_plot <- ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy, fill = 
                                     Year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  labs(
    title = "All Sites",
    x = "Year",
    y = "Kelp Canopy (% cover)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))

main_plot <- ggplot(OnlyUrch, aes(x = SiteCode, y = Total.Canopy, fill = 
                                         SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  labs(
    x = "Site",
    y = "Total Canopy (%)"
  ) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    strip.text = element_text(size = 12, face = "bold")) +
  coord_flip()

# try another type of plot?

final_plot <- main_plot + inset_plot + plot_layout(widths = c(3, 1))

ggsave("Figures/combined_plot.png", width = 8, height = 6, dpi = 300)

################################################################################
######## Figure 4: Urchin Densities by Site
################################################################################

kruskal.test(TotalUrchins ~ SiteCode, data = OnlyUrch)
wilcox.test(TotalUrchins ~ factor(Year), data = OnlyUrch)

inset_plot_urch <- ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins, fill = 
                                     Year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  labs(
    title = "All Sites",
    x = "Year",
    y = "Urchin Density (count per 0.25m²) "
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))

main_plot_urch <- ggplot(OnlyUrch, aes(x = SiteCode, y = TotalUrchins, fill = 
                                    SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  labs(
    x = "Site",
    y = "Urchin Density (count per 0.25m²)"
  ) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c(
    "BB" = "orange2",
    "FC" = "orange2",
    "SC" = "orange2",
    "SB" = "orange2",
    "YB" = "royalblue4",
    "SH" = "royalblue4",
    "CB" = "darkolivegreen4",
    "RP" = "darkolivegreen4",
    "CP" = "darkolivegreen4",
    "WC" = "darkolivegreen4"))+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    strip.text = element_text(size = 12, face = "bold")) +
  coord_flip()

# try another type of plot?

final_plot_urch <- main_plot_urch + inset_plot_urch + plot_layout(widths = c(3, 1))

ggsave("Figures/Urchcombined_plot.png", width = 8, height = 6, dpi = 300)

