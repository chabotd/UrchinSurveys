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

site_names <- c(
  "BB" = "Boiler Bay",
  "FC" = "Fogarty Creek",
  "YB" = "Yachats Beach",
  "SH" = "Strawberry Hill",
  "SB" = "Sunset Bay",
  "SC" = "South Cove",
  "CB" = "Cape Blanco",
  "RP" = "Rocky Point",
  "WC" = "Whiskey Creek",
  "CP" = "Chetco Point",
  "CMN" = "Cape Mendocino North",
  "CMS" = "Cape Mendocino South"
)


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

behav_subhab_cols <- c(
  "PittedUrchins" = "#EA4F0DFF",
  "Cryptic" = "#F1CA3AFF",
  "CreviceUrchins" = "#29EFA2FF",
  "NonPit" = "#4490FEFF",
  "OpenUrchins" = "purple"
)

View(urch)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

# upz only
UPZ <- urch %>%
  filter(Subhabitat== "UPZ")

# upz only
NPZ <- urch %>%
  filter(Subhabitat== "NPZ")

# az only
AZ <- urch%>%
  filter(Subhabitat=="AZ")

#drop SB 2024 from dataset as no true NPZ existed. 
NPZ <- NPZ %>%
  filter(!(SiteCode == "SB" & Year == 2024))


# #remove YB and SH
# NoPerpetua <- OnlyUrch %>%
#   filter(!(SiteCode %in% c("SH", "YB")))

# pivot longer to get behavior 
plotdat <- OnlyUrch %>%
  pivot_longer(
    cols = c(Cryptic, OpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  )

npz_long <- NPZ %>%
  pivot_longer(
    cols = c(PittedUrchins, CreviceUrchins, OpenUrchins, Cryptic, NonPit),
    names_to = "Category",
    values_to = "Count"
  )

upz_long <- UPZ %>%
  pivot_longer(
    cols = c(PittedUrchins, CreviceUrchins, OpenUrchins, Cryptic, NonPit),
    names_to = "Category",
    values_to = "Count"
  )

az_long <- AZ %>%
  pivot_longer(
    cols = c(PittedUrchins, CreviceUrchins, OpenUrchins, Cryptic, NonPit),
    names_to = "Category",
    values_to = "Count"
  )

# plots to look at mean urch counts 
upz_summary <- upz_long %>%
  group_by(SiteCode, Category) %>%
  summarise(
    mean_count = mean(Count, na.rm = TRUE),
    se_count = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

upz1 <- ggplot(upz_summary, aes(x = SiteCode, y = mean_count, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_fill_manual(values = behav_subhab_cols) +
  labs(
    title = "Urchin-pit Dominated Subhabitat",
    x = "Site",
    y = "Mean Urchin Count ± SE",
    fill = "Category"
  ) +
  theme_minimal()

npz_summary <- npz_long %>%
  group_by(SiteCode, Category) %>%
  summarise(
    mean_count = mean(Count, na.rm = TRUE),
    se_count = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

npz1 <- ggplot(npz_summary, aes(x = SiteCode, y = mean_count, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_fill_manual(values = behav_subhab_cols) +
  labs(
    title = "Nonpit Urchin Dominated Subhabitat",
    x = "Site",
    y = "Mean Urchin Count ± SE",
    fill = "Category"
  ) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/Exploratory/upz1.png", 
       plot =upz1  , width = 8, height = 6, dpi = 300)
ggsave(filename = "Figures/Surveys/Exploratory/npz1.png", 
       plot =npz1  , width = 8, height = 6, dpi = 300)


az_summary <- az_long %>%
  group_by(SiteCode, Category) %>%
  summarise(
    mean_count = mean(Count, na.rm = TRUE),
    se_count = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

az1 <- ggplot(az_summary, aes(x = SiteCode, y = mean_count, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_fill_manual(values = behav_subhab_cols) +
  labs(
    title = "Algal-Dominated Subhabitat",
    x = "Site",
    y = "Mean Urchin Count ± SE",
    fill = "Category"
  ) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/Exploratory/az1.png", 
       plot =az1  , width = 8, height = 6, dpi = 300)
