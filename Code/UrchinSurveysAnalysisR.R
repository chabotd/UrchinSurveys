library(tidyverse)
#library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)
#library(ggpubr)
library(FSA)
library(rcompanion)
library(tweedie)
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

susept <- c(
  "Unlikely" = "cornflowerblue",
  "Extremely Unlikely" = "grey3",
  "Susceptible" = "tomato2")

View(urch)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

#remove Mendo
oregon <- OnlyUrch %>%
  filter(!(SiteCode %in% c("CMS", "CMN")))


##############################################################################
#Q1 FIG 1 - overall unlikley vs susept open urch diffs
##############################################################################
kruskal.test(OpenUrchins ~ Connectivity, data = oregon)

# can I do pairwise?
pairwise.wilcox.test(
  x = oregon$OpenUrchins,
  g = oregon$Connectivity,
  p.adjust.method = "fdr"
)

pl3 <- ggplot(oregon, aes(x = Connectivity, y = NonPit, fill= Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Urchin Migration Possibility",
    y = "Noncryptic Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = susept) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()


ggsave(filename = "Figures/Surveys/Q1/UrchinDensitiesbyConnectivityOpen.png", 
       plot =pl3  , width = 8, height = 6, dpi = 300)

##############################################################################
#Q1 FIG 2 - overall sitewide open urchin densities 
##############################################################################

oregon$SiteCode <- factor(oregon$SiteCode, levels=c("CP", "WC" , 
                                                    "RP", "CB", "SC", "SB", 
                                                    "SH", "YB", "BB", "FC"))


################################################################################
# Q2 Kelp Abundance Diffs in Subhabitats 
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
  labs(
    x = "Subhabitat and Site",
    y = "Percent Cover of Canopy-Forming Kelp per 0.25m²"
  ) +
  scale_fill_manual(values = site_cols) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) 

ggsave(filename = "Figures/Surveys/Q1_kelp_subhabitat.png", 
       plot = q1sub , width = 8, height = 6, dpi = 300)

################################################################################
#Q3 Kelp Abundance Diffs
#  -- Urchin behavior and kelp
################################################################################
######## FIG 6 CANOPY FORMING
#########################

# Model 1: Cryptic
m_cryp <- glm(
  TotalCanopy ~ Cryptic,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_cryp)

# Model 2: Open
m_open <- glm(
  TotalCanopy ~ OpenUrchins,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_open)


plotdat <- OnlyUrch %>%
  mutate(
    PredCryp = predict(m_cryp, type = "response"),
    PredOpen = predict(m_open, type = "response")
  ) %>%
  pivot_longer(
    cols = c(Cryptic, OpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) %>%
  mutate(
    PredictedCanopy = ifelse(
      UrchinBehavior == "Cryptic", 
      PredCryp,
      PredOpen
    )
  )

plot7 <- ggplot(plotdat, aes(x = UrchinDensity, y = TotalCanopy, color = UrchinBehavior)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Cryptic" = "#EA4F0DFF",
      "OpenUrchins" = "#4490FEFF"
    )
  ) +
  labs(
    x = "Urchin Density",
    y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
    color = "UrchinBehavior"
  ) +
  theme_minimal()

######### ANCOVA on model

ancova_fit <- aov(TotalCanopy ~ UrchinDensity + UrchinBehavior, data = plotdat)
summary(ancova_fit)


###########         #################     ###############     ############
#try log transforming
##########        ##############              #############################
OnlyUrch <- OnlyUrch %>%
  mutate(
    logCryptic = log1p(Cryptic),
    logOpenUrchins = log1p(OpenUrchins),
    logCanopy = log1p(TotalCanopy)
  )

# Model 1: Cryptic
m_cryp <- glm(
  logCanopy ~ logCryptic,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_cryp)

# Model 2: Open
m_open <- glm(
  logCanopy ~ logOpenUrchins,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_open)

plotdat <- OnlyUrch %>%
  mutate(
    PredCryp = predict(m_cryp, type = "response"),
    PredOpen = predict(m_open, type = "response")
  ) %>%
  pivot_longer(
    cols = c(logCryptic, logOpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) %>%
  mutate(
    UrchinBehavior = ifelse(
      UrchinBehavior == "logCryptic",
      "Cryptic",
      "OpenUrchins"
    ),
    PredictedCanopy = ifelse(
      UrchinBehavior == "Cryptic",
      PredCryp,
      PredOpen
    )
  )

plot8 <- ggplot(plotdat, aes(x = UrchinDensity, y = logCanopy, color = UrchinBehavior)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Cryptic" = "#EA4F0DFF",
      "OpenUrchins" = "#4490FEFF"
    )
  ) +
  labs(
    x = "log Urchin Density",
    y = "log Percent Cover of Canopy-Forming Kelp per 0.25m²",
    color = "UrchinBehavior"
  ) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/behavior_glm_log.png", 
       plot =plot8  , width = 8, height = 6, dpi = 300)



######### ANCOVA on log model

ancova_fit <- aov(logCanopy ~ UrchinDensity +UrchinBehavior, data = plotdat)
summary(ancova_fit)

# look at residuals 
resid_vals <- residuals(ancova_fit)
shapiro.test(resid_vals)

# qq plot 
qqnorm(resid_vals); qqline(resid_vals)

######## UNDERSTORY + substrate covariate 
#############################################################################
# these have probs
OnlyUrch <- OnlyUrch %>%
  filter(!Call_Number %in% c("CB_2026_UPZ_2_2", "FC_2026_UPZ_1_2"))

############################################################################
# Pits versus Nonpits - nonlog 
##########################
# Model 1: Pits
m_pit <- glm(
  UnderstoryAlgae ~ PittedUrchins,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_pit)
# not significant

# Model 2: Nonpit
m_nonpit <- glm(
  UnderstoryAlgae ~ NonPit,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_nonpit)
# significant 

plotdat <- OnlyUrch %>%
  mutate(
    PredPits = predict(m_pit, type = "response"),
    PredNonpit = predict(m_nonpit, type = "response")
  ) %>%
  pivot_longer(
    cols = c(PittedUrchins, NonPit),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) %>%
  mutate(
    PredictedUnderstory = ifelse(
      UrchinBehavior == "PittedUrchins",
      PredPits,
      PredNonpit
    )
  )

plotpit <- ggplot(plotdat, aes(x = UrchinDensity, y = UnderstoryAlgae, color = UrchinBehavior)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUnderstory), linewidth = 1) +
  scale_color_manual(
    values = c(
      "PittedUrchins" = "orange",
      "NonPit" = "cornflowerblue"
    )
  ) +
  labs(
    x = "Urchin Density",
    y = "Percent Cover of Understory Algae per 0.25m²",
    color = "Urchin Behavior"
  ) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/behavior_glm_underpit.png", 
       plot =plotpit  , width = 8, height = 6, dpi = 300)

######### ANCOVA on non log

ancova_fit <- aov(UnderstoryAlgae ~ UrchinDensity + UrchinBehavior, data = plotdat)
summary(ancova_fit)

resid_vals <- residuals(ancova_fit)
shapiro.test(resid_vals)

#not normal at all red flag

#significant for density but not behavior
# need to add in substrate to model 

# Pits versus Nonpits - log transformation 
##########################

OnlyUrch <- OnlyUrch %>%
  mutate(
    logPitted = log1p(PittedUrchins),
    logNonpit = log1p(NonPit),
    logUnderstory = log1p(UnderstoryAlgae)
  )

# Model 1: Pits
m_pit <- glm(
  logUnderstory ~ logPitted,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_pit)
# significant 

# Model 2: Nonpit
m_nonpit <- glm(
  logUnderstory ~ logNonpit,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_nonpit)

#significant 

plotdat_pit <- OnlyUrch %>%
  mutate(
    PredPit = predict(m_pit, type = "response"),
    PredNonpit = predict(m_nonpit, type = "response")
  ) %>%
  pivot_longer(
    cols = c(logPitted, logNonpit),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) %>%
  mutate(
    UrchinBehavior = ifelse(
      UrchinBehavior == "logPitted",
      "Pitted",
      "Nonpitted"
    ),
    PredictedUnderstory = ifelse(
      UrchinBehavior == "Pitted",
      PredPit,
      PredNonpit
    )
  )


plotpit <- ggplot(plotdat_pit, aes(x = UrchinDensity, y = logUnderstory, color = UrchinBehavior)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUnderstory), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Pitted" = "orange",
      "Nonpitted" = "cornflowerblue"
    )
  ) +
  labs(
    x = "log Urchin Density",
    y = "log Percent Cover of Understory Algae per 0.25m²",
    color = "Urchin Behavior"
  ) +
  theme_minimal()

ggsave(filename = "Figures/Surveys/behavior_glm_underpit.png", 
       plot =plotpit  , width = 8, height = 6, dpi = 300)


ancova_fit <- aov(logUnderstory ~ UrchinDensity + UrchinBehavior, data = plotdat_pit)
summary(ancova_fit)

# sg for density; not significant for behavior

# look at residuals 
resid_vals <- residuals(ancova_fit)
shapiro.test(resid_vals)

# qq plot 
qqnorm(resid_vals); qqline(resid_vals)

# assumption violated !!

##################Cryptic vs Noncrpytic 
#########non-log 
#################################################################

# Model 1: Cryptic
m_cryp <- glm(
  UnderstoryAlgae ~ Cryptic,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_cryp)

# Model 2: Open
m_open <- glm(
  UnderstoryAlgae ~ OpenUrchins,
  data = OnlyUrch,
  family = tweedie(var.power = 1.5, link.power = 0)
)
summary(m_open)

plotdat <- OnlyUrch %>%
  mutate(
    PredCryp = predict(m_cryp, type = "response"),
    PredOpen = predict(m_open, type = "response")
  ) %>%
  pivot_longer(
    cols = c(Cryptic, OpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) %>%
  mutate(
    PredictedUnderstory = ifelse(
      UrchinBehavior == "Cryptic",
      PredCryp,
      PredOpen
    )
  )

plot6 <- ggplot(plotdat, aes(x = UrchinDensity, y = UnderstoryAlgae, color = UrchinBehavior)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUnderstory), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Cryptic" = "#EA4F0DFF",
      "OpenUrchins" = "#4490FEFF"
    )
  ) +
  labs(
    x = "Urchin Density",
    y = "Percent Cover of Understory Algae per 0.25m²",
    color = "Urchin Behavior"
  ) +
  theme_minimal()


ggsave(filename = "Figures/Surveys/behavior_glm_under.png", 
       plot =plot6  , width = 8, height = 6, dpi = 300)

################################################################################
# New Q1: densities of cryptic / noncryptic urchins and susceptibility to migration Old Q3 
################################################################################
oneway <- aov(OpenUrchins~ SiteCode, data = OnlyUrch)
summary(oneway)

# look at residuals 
resid_vals <- residuals(oneway)
shapiro.test(resid_vals)

# qq plot 
qqnorm(resid_vals); qqline(resid_vals)


tuk <- TukeyHSD(oneway)

# Extract p-values for SiteCode comparisons
tuk_p <- tuk$SiteCode[, "p adj"]

# Convert to compact letter display
letters <- multcompLetters(tuk_p)
letters$Letters

cld_df <- data.frame(
  SiteCode = names(letters$Letters),
  Letters = letters$Letters
)

# MAKE PLOT 

plQ1 <- ggplot(OnlyUrch, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_text(data = cld_df,
            aes(x = SiteCode, y = max(OnlyUrch$OpenUrchins, na.rm = TRUE) + 2,
                label = Letters),
            size = 6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
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
    "WC" = "darkolivegreen4",
    "CMN" = "lightgrey",
    "CMS" = "lightgrey"
  )) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/Surveys/Q1/UrchinDensitiesbySiteOpen.png", 
       plot =plQ1  , width = 8, height = 6, dpi = 300)

################################################################################
# NPZ
################################################################################
NPZ<- OnlyUrch %>%
  filter(Subhabitat=="NPZ")

NPZoneway <- aov(OpenUrchins~ SiteCode, data = NPZ)
summary(NPZoneway)

NPZtuk <- TukeyHSD(NPZoneway)

# Extract p-values for SiteCode comparisons
NPZtuk_p <- NPZtuk$SiteCode[, "p adj"]

# Convert to compact letter display
NPZletters <- multcompLetters(NPZtuk_p)
NPZletters$Letters

NPZcld_df <- data.frame(
  SiteCode = names(NPZletters$Letters),
  Letters = NPZletters$Letters
)

# MAKE PLOT 

plQ1_2 <- ggplot(NPZ, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_text(data = NPZcld_df,
            aes(x = SiteCode, y = max(NPZ$OpenUrchins, na.rm = TRUE) + 2,
                label = Letters),
            size = 6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  labs(
    x = "Site",
    y = "Open Urchin Density (count per 0.25m²) in nonpit urchin dominated subhabitat"
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
    "WC" = "darkolivegreen4",
    "CMN" = "lightgrey",
    "CMS" = "lightgrey"
  ))+
    theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      ) +
      coord_flip()
  coord_flip()
  
  ggsave(filename = "Figures/Surveys/Q1/OpenUrchinDensities_NPZ.png", 
         plot =plQ1_2  , width = 8, height = 6, dpi = 300)

  ################################################################################
  # UPZ
  ################################################################################
  UPZ<- oregon %>%
    filter(Subhabitat=="UPZ")
  
  UPZoneway <- aov(OpenUrchins~ SiteCode, data = UPZ)
  summary(UPZoneway)
  
  UPZtuk <- TukeyHSD(UPZoneway)
  
  # Extract p-values for SiteCode comparisons
  PZtuk_p <- UPZtuk$SiteCode[, "p adj"]
  
  # Convert to compact letter display
  PZletters <- multcompLetters(PZtuk_p)
  PZletters$Letters
  
  PZcld_df <- data.frame(
    SiteCode = names(PZletters$Letters),
    Letters = PZletters$Letters
  )
  
  # MAKE PLOT 
  
  plQ1_3 <- ggplot(UPZ, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_text(data = PZcld_df,
              aes(x = SiteCode, y = max(NPZ$OpenUrchins, na.rm = TRUE) + 2,
                  label = Letters),
              size = 6) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                 fill = "white", color = "black") +
    labs(
      x = "Site",
      y = "Open Urchin Density (count per 0.25m²) in urchin pit dominated subhabitat"
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
      "WC" = "darkolivegreen4"
    ))+
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    ) +
    coord_flip()
  
  
  ggsave(filename = "Figures/Surveys/Q1/OpenUrchinDensities_UPZ.png", 
         plot =plQ1_3  , width = 8, height = 6, dpi = 300)
  
  # NPZ

############ KRUSKAL -WALLIS  VERSION 
# 
# #only oregon urchin dom plots
# OnlyUrch$SiteCode <- factor(OnlyUrch$SiteCode, levels=c("CMS", "CMN", "CP", "WC" , 
#                                                 "RP", "CB", "SC", "SB", "SH", 
#                                                "YB", "BB", "FC"))
# 
# kruskal.test(OpenUrchins~ SiteCode, data = OnlyUrch)
# # have to create matrix properly to
# 
# # Megan's function 
# tri.to.squ<-function(x)
# {
#   rn<-row.names(x)
#   cn<-colnames(x)
#   an<-unique(c(cn,rn))
#   myval<-x[!is.na(x)]
#   mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
#   for(ext in 1:length(cn))
#   {
#     for(int in 1:length(rn))
#     {
#       if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
#       mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
#       mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
#     }
#     
#   }
#   return(mymat)
# }
# 
# ############################
# #NonPit Urchins
# 
# ## Kruskal Wallace test 
# kruskal.test()
# ## Test is significant.
# 
# ## Pairwise Wilcoxin Rank Sum test for crows.
# open <- pairwise.wilcox.test(
#   x = OnlyUrch$OpenUrchins,
#   g = OnlyUrch$SiteCode,
#   p.adjust.method = "fdr"
# )
# 
# ## Convert the p-value output table into a matrix.
# open <- data.matrix(open$p.value)
# 
# ## Use the function to make the p-value matrix symmetrical.
# open <- tri.to.squ(open)
# 
# ## Generate letters to represent significant differences in crow abundance between sites.
# open_letters <- multcompLetters(open,compare="<=", threshold=0.05, Letters=letters)
# 
# #NonPit Urchins
# 
# ## Kruskal Wallace test 
# kruskal.test()
# ## Test is significant.
# 
# ## Pairwise Wilcoxin Rank Sum test for crows.
# open <- pairwise.wilcox.test(
#   x = OnlyUrch$OpenUrchins,
#   g = OnlyUrch$SiteCode,
#   p.adjust.method = "fdr"
# )
# 
# ## Convert the p-value output table into a matrix.
# open <- data.matrix(open$p.value)
# 
# ## Use the function to make the p-value matrix symmetrical.
# open <- tri.to.squ(open)
# 
# ## Generate letters to represent significant differences in crow abundance between sites.
# open_letters <- multcompLetters(open,compare="<=", threshold=0.05, Letters=letters)
# 
# 
# 
# ###################CREATE PLOT#################################################
# 
# # if on y-axis: 
# OnlyUrch$SiteCode <- factor(OnlyUrch$SiteCode, levels=c("CMS", "CMN", "CP", "WC" , 
#                                                             "RP", "CB", "SC", "SB", "SH" ,
#                                                             "YB", "BB", "FC"))
# 
# pl1 <- ggplot(OnlyUrch, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.6) +
#   stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
#                fill = "white", color = "black") +
#   labs(
#     x = "Site",
#     y = "Open Urchin Density (count per 0.25m²) in both subhabitats"
#   ) +
#   scale_fill_manual(values = c(
#     "BB" = "orange2",
#     "FC" = "orange2",
#     "SC" = "orange2",
#     "SB" = "orange2",
#     "YB" = "royalblue4",
#     "SH" = "royalblue4",
#     "CB" = "darkolivegreen4",
#     "RP" = "darkolivegreen4",
#     "CP" = "darkolivegreen4",
#     "WC" = "darkolivegreen4",
#     "CMN" = "lightgrey",
#     "CMS" = "lightgrey"
#   )) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.title.x = element_text(size = 18),
#     axis.title.y = element_text(size = 18),
#     axis.text.x = element_text(size = 15),
#     axis.text.y = element_text(size = 15)
#   ) +
#   coord_flip()
# 
# 
# ggsave(filename = "Temp/Q3/UrchinDensitiesbySiteOpen.png", 
#        plot =pl1  , width = 8, height = 6, dpi = 300)
# 
# #only UPZ
# OregonUPZ<- OregonUrch %>%
#   filter(Subhabitat=="UPZ")
# 
# kruskal.test(NonPit ~ SiteCode, data = OregonUPZ)
# 
# # haave to create matrix properly to
# 
# pw <- pairwise.wilcox.test(
#   x = OregonUPZ$OpenUrchins,
#   g = OregonUPZ$SiteCode,
#   p.adjust.method = "fdr"
# )
# 
# tri <- pw$p.value
# sites <- sort(unique(OregonUPZ$SiteCode))
# 
# full <- matrix(NA, length(sites), length(sites),
#                dimnames = list(sites, sites))
# 
# full[rownames(tri), colnames(tri)] <- tri
# full[colnames(tri), rownames(tri)] <- t(tri)
# 
# full[is.na(full)] <- 1
# 
# letters <- multcompLetters(full)$Letters
# letters_df <- data.frame(SiteCode = names(letters),
#                          Letter = letters)
# 
# plot_df <- OregonUPZ %>%
#   left_join(letters_df, by = "SiteCode")
# 
# 
# 
# pl4 <- ggplot(plot_df, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.6) +
#   geom_text(
#     aes(label = Letter),
#     y = max(plot_df$OpenUrchins, na.rm = TRUE) * 1.01,
#     size = 6
#   ) +
#   stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
#                fill = "white", color = "black") +
#   labs(
#     x = "Site",
#     y = "Open Urchin Density (count per 0.25m²) in Urchin Pit Dominated Subhabiat"
#   ) +
#   scale_fill_manual(values = c(
#     "BB" = "orange2",
#     "FC" = "orange2",
#     "SC" = "orange2",
#     "SB" = "orange2",
#     "YB" = "royalblue4",
#     "SH" = "royalblue4",
#     "CB" = "darkolivegreen4",
#     "RP" = "darkolivegreen4",
#     "CP" = "darkolivegreen4",
#     "WC" = "darkolivegreen4",
#     "CMN" = "lightgrey",
#     "CMS" = "lightgrey"
#   )) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.title.x = element_text(size = 18),
#     axis.title.y = element_text(size = 18),
#     axis.text.x = element_text(size = 15),
#     axis.text.y = element_text(size = 15)
#   ) +
#   coord_flip()
# 
# ggsave(filename = "Temp/Q3/UrchinDensitiesbySiteOpeninUPZ.png", 
#        plot =pl4  , width = 8, height = 6, dpi = 300)
###############################################################################
#only UPZ
###############################################################################
OregonNPZ<- oregon %>%
  filter(Subhabitat=="NPZ")

kruskal.test(NonPit ~ SiteCode, data = OregonNPZ)


pw <- pairwise.wilcox.test(
  x = OregonNPZ$OpenUrchins,
  g = OregonNPZ$SiteCode,
  p.adjust.method = "fdr"
)

tri <- pw$p.value
sites <- sort(unique(OregonNPZ$SiteCode))

full <- matrix(NA, length(sites), length(sites),
               dimnames = list(sites, sites))

full[rownames(tri), colnames(tri)] <- tri
full[colnames(tri), rownames(tri)] <- t(tri)

full[is.na(full)] <- 1

letters <- multcompLetters(full)$Letters
letters_df <- data.frame(SiteCode = names(letters),
                         Letter = letters)

plot_df <- OregonNPZ %>%
  left_join(letters_df, by = "SiteCode")

# don't forget to run reorder sites

pl5 <- ggplot(plot_df, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_text(
    aes(label = Letter),
    y = max(plot_df$OpenUrchins, na.rm = TRUE) * 1.01,
    size = 6
  ) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  labs(
    x = "Site",
    y = "Open Urchin Density (count per 0.25m²) in Nonpit Urchin Dominated Subhabiat"
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
    "WC" = "darkolivegreen4",
    "CMN" = "lightgrey",
    "CMS" = "lightgrey"
  )) +
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
