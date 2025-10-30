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
  SiteCode %in% c("SC", "CB", "WC", "CP", "RP") ~ "Susceptible",
  SiteCode %in% c("BB", "SB", "FC") ~ "Unlikely",
  SiteCode %in% c("YB", "SH") ~ "Extremely Unlikely",
  TRUE ~ NA_character_  # for any sites not listed
))
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

