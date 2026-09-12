library(tidyverse)
#library(dplyr)
library(vegan)
#library(multcompView)
library(ggplot2)
#library(ggpubr)
library(FSA)
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
  "FC" = "#30123BFF",
  "BB" = "#4454C4FF",
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

connect <- c(
  "BB" = "#F1CA3AFF",
  "FC" = "#F1CA3AFF",
  "SC" = "darkolivegreen4",
  "SB" = "orange2",
  "YB" = "royalblue4",
  "SH" = "royalblue4",
  "CB" = "darkolivegreen4",
  "RP" = "orange2",
  "CP" = "darkolivegreen4",
  "WC" = "darkolivegreen4")

View(urch)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

#remove YB and SH
NoPerpetua <- OnlyUrch %>%
  filter(!(SiteCode %in% c("SH", "YB")))
################################################################################
# Rock hardness mean and SE
################################################################################
rock<- read.csv("Data/RelativeRockHardness.csv")
View(rock)
summary(rock)

rock %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TimeToDrill, na.rm = TRUE),
    SE   = sd(TimeToDrill, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )

rock %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(TimeToDrill, na.rm = TRUE),
    SE   = sd(TimeToDrill, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(
    Mean = round(Mean, 2),
    SE = round(SE, 2)
  ) %>%
  kable(
    format = "html",
    col.names = c("Site", "Mean Time to Drill (s)", "SE"),
    align = c("l", "r", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE)

################################################################################
#Q1 Kelp Abundance Diffs subhabitats
################################################################################
kruskal.test(TotalCanopy ~ Subhabitat, data = OnlyUrch)

# mean canopy diffs
OnlyUrch %>%
group_by(Subhabitat) %>%
  summarise(
    Mean = mean(TotalCanopy, na.rm = TRUE),
    SE   = sd(TotalCanopy, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )

# plot 
q1sub <- ggplot(OnlyUrch, aes(x = Subhabitat, y = TotalCanopy, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
 #facet_wrap(~ SiteCode) +
  labs(
    x = "Subhabitat and Site",
    y = "Percent Cover of Canopy-Forming Kelp per 0.25m²"
  ) +
  scale_fill_manual(values = site_cols) +
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

################################################################################
#Q1 Kelp Abundance Diffs -- urch behavior
################################################################################

# pivot longer to get behavior 
plotdat <- NoPerpetua %>%
  pivot_longer(
    cols = c(Cryptic, OpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  )


glm_tweedie_cryp <- glm(TotalCanopy ~ Cryptic,
                   data = NoPerpetua,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie_cryp)

glm_tweedie_open <- glm(TotalCanopy ~ OpenUrchins,
                        data = NoPerpetua,
                        family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie_open

        
anova(glm_tweedie_open, glm_tweedie_cryp, test="LRT")


NoPerpetua$PredictedCanopyCryp <- predict(glm_tweedie_cryp, type = "response")
NoPerpetua$PredictedCanopyOpen <- predict(glm_tweedie_open, type = "response")

####################### faceted full set up

p_cryptic <- ggplot(NoPerpetua, aes(x = Cryptic, y = TotalCanopy, color = SiteCode)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopyCryp), linewidth = 1) +
  facet_wrap(~ SiteCode) +
  scale_color_manual(values = site_cols) +
  labs(
    x = "Cryptic Urchin Density",
    y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
    color = "Site"
  ) +
  theme_minimal()

p_noncryptic <- ggplot(NoPerpetua, aes(x = OpenUrchins, y = TotalCanopy, color = SiteCode)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopyOpen), linewidth = 1) +
  facet_wrap(~ SiteCode) +
  scale_color_manual(values = site_cols) +
  labs(
    x = "Noncryptic Urchin Density",
    y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
    color = "Site"
  ) +
  theme_minimal()


combined <- plot_grid(
  p_cryptic,
  p_noncryptic,
  labels = c("A", "B"),
  ncol = 2,
  align = "h"
)

ggsave(filename = "Figures/Surveys/behavior_glm.png", 
       plot =combined  , width = 8, height = 6, dpi = 300)

####### Try as a loop for each indiv. plot 

sites <- unique(NoPerpetua$SiteCode)

plots <- lapply(sites, function(s) {
  
  dat <- NoPerpetua %>% filter(SiteCode == s)
  
  p_cryptic <- ggplot(dat, aes(x = Cryptic, y = TotalCanopy)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = PredictedCanopyCryp), linewidth = 1) +
    labs(
      x = "Cryptic Urchin Density",
      y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
      color = "Site"
    ) +
    theme_minimal()
  
  p_noncryptic <- ggplot(dat, aes(x = OpenUrchins, y = TotalCanopy)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = PredictedCanopyOpen), linewidth = 1) +
    labs(
      x = "Noncryptic Urchin Density",
      y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
      color = "Site"
    ) +
    theme_minimal()
  
  plot_grid(p_cryptic, p_noncryptic, labels = c(sites_full_names[s])
, ncol = 2)
    })

for (i in seq_along(sites)) {
  ggsave(
    filename = paste0("Figures/Surveys/Q1/", sites[i], "_cryptic_noncryptic.png"),
    plot = plots[[i]],
    width = 10,
    height = 5
  )
}


################################################################################
# Q3 : densities of cryptic / noncryptic urchins and susceptibility to migration 
################################################################################
#only oregon urchin dom plots

#remove YB and SH
OregonUrch<- OnlyUrch %>%
  filter(!(SiteCode %in% c("CMS", "CMN")))

# if on y-axis: 
OregonUrch$SiteCode <- factor(OregonUrch$SiteCode, levels=c("CP", "WC" , 
                                                "RP", "CB", "SC", "SB", "SH", 
                                                "YB", "BB", "FC"))

kruskal.test(NonPit ~ SiteCode, data = OregonUrch)
dunnTest(NonPit ~ SiteCode, data = OregonUrch, method = "holm")

pl1 <- ggplot(OregonUrch, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  # geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Open Urchin Density (count per 0.25m²) in both subhabitats"
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

ggsave(filename = "Temp/Q3/UrchinDensitiesbySiteOpen.png", 
       plot =pl1  , width = 8, height = 6, dpi = 300)

#only UPZ
OregonUPZ<- OregonUrch %>%
  filter(Subhabitat=="UPZ")

kruskal.test(NonPit ~ SiteCode, data = OregonUPZ)

pl4 <- ggplot(OregonUPZ, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  # geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Open Urchin Density (count per 0.25m²) in UPZ"
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

ggsave(filename = "Temp/Q3/UrchinDensitiesbySiteOpeninUPZ.png", 
       plot =pl4  , width = 8, height = 6, dpi = 300)
############################
#only UPZ
OregonNPZ<- OregonUrch %>%
  filter(Subhabitat=="NPZ")

kruskal.test(NonPit ~ SiteCode, data = OregonNPZ)

pl5 <- ggplot(OregonNPZ, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  # geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Site",
    y = "Open Urchin Density (count per 0.25m²) in NPZ"
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

ggsave(filename = "Temp/Q3/UrchinDensitiesbySiteOpeninNPZ.png", 
       plot =pl5  , width = 8, height = 6, dpi = 300)

############################
kruskal.test(NonPit ~ Connectivity, data = OregonUrch)

pl3 <- ggplot(OregonUrch, aes(x = Connectivity, y = NonPit, fill= Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Urchin Migration Possibility",
    y = "Non- Cryptic Urchin Density (count per 0.25m²)"
  ) +
  scale_color_manual(values = connect) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()


ggsave(filename = "Temp/Q3/UrchinDensitiesbyConnectivityOpen.png", 
       plot =pl3  , width = 8, height = 6, dpi = 300)
