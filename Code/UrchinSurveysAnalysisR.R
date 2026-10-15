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
library(mgcv)

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
  "BB" = "#F1CA3AFF", #sedimented 
  "FC" = "#F1CA3AFF", #sedimented 
  "SC" = "darkolivegreen4", #connected
  "SB" = "#F1CA3AFF", #sedimented
  "YB" = "royalblue4", #not connected
  "SH" = "royalblue4", #not connected
  "CB" = "darkolivegreen4", #connected
  "RP" = "darkolivegreen2", # sedimentation possible? 
  "CP" = "darkolivegreen4", #connected
  "WC" = "darkolivegreen4") #connected

sus <- c(
  "Unlikely" = "cornflowerblue",
  "Extremely Unlikely" = "grey3",
  "Susceptible" = "tomato2")

my_theme <- function() {
  # Set base theme and font family =============================================
  theme_minimal(
    base_family = "Libre Franklin"
  ) +
    # Overwrite base theme defaults ============================================
  theme(
    # Text elements ==========================================================
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "#333333",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "#999999",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 13,
      color = "#777777",
      margin = margin(t = 15),
      hjust = 0
    ),
    axis.text = element_text(
      size = 11,
      color = "#333333"
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # Line elements ==========================================================
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed",
      linewidth = 0.15,
      color = "#999999"
    ),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(
      linetype = "solid",
      linewidth = 0.25,
      color = "#999999"
    ),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.ticks.length.x = unit(4, units = "pt")
  )
}

View(urch)

# don't look at AZ-- urchin-dominated zones only. 
OnlyUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ", "NPZ"))

#remove Mendo
oregon <- OnlyUrch %>%
  filter(!(SiteCode %in% c("CMS", "CMN")))

#only oregon urchin dom plots
# oregon$SiteCode <- factor(oregon$SiteCode, levels=c("CP", "WC" ,
                                                    # "RP", "CB", "SC", "SB", "SH",
                                                    # "YB", "BB", "FC"))
##############################################################################
#Q1 FIG 1 - overall unlikley vs susept open urch diffs
##############################################################################
# Megan's function (to create matrix)
tri.to.squ<-function(x)
{
  rn<-row.names(x)
  cn<-colnames(x)
  an<-unique(c(cn,rn))
  myval<-x[!is.na(x)]
  mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
    
  }
  return(mymat)
}

kruskal.test(OpenUrchins ~ Connectivity, data = oregon)

#re order sus.
oregon$Connectivity <- factor(oregon$Connectivity, levels = c( "Extremely Unlikely", 
"Unlikely", "Susceptible"))

# create matrix for letters 
pw <- pairwise.wilcox.test(
  x = oregon$OpenUrchins,
  g = oregon$Connectivity,
  p.adjust.method = "fdr"
)
# 
tri <- pw$p.value
sites <- sort(unique(oregon$Connectivity))
# 
full <- matrix(NA, length(sites), length(sites),
               dimnames = list(sites, sites))
# 
full[rownames(tri), colnames(tri)] <- tri
full[colnames(tri), rownames(tri)] <- t(tri)
# 
full[is.na(full)] <- 1
# 
letters <- multcompLetters(full)$Letters
letters_df <- data.frame(Connectivity = names(letters),
                         Letter = letters)
# 
plot_df <- oregon %>%
  left_join(letters_df, by = "Connectivity")

plot_df$Connectivity <- factor(plot_df$Connectivity, 
                               levels = c( "Extremely Unlikely", 
                                      "Unlikely", "Susceptible"))
# 
# 

pl3 <- ggplot(plot_df, aes(x = Connectivity, y = NonPit, fill= Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_text(
    aes(label = Letter),
    y = max(plot_df$OpenUrchins, na.rm = TRUE) * 1.01,
    size = 6
  ) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Urchin Migration Possibility",
    y = "Noncryptic Urchin Density (count per 0.25m²)"
  ) +
  scale_fill_manual(values = sus) +
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

################################################################################
# Q1: FIGS 2, 3, 4 densities of cryptic / noncryptic urchins and susceptibility to migration Old Q3 
################################################################################
# Megan's function (to create matrix)
tri.to.squ<-function(x)
{
  rn<-row.names(x)
  cn<-colnames(x)
  an<-unique(c(cn,rn))
  myval<-x[!is.na(x)]
  mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
    
  }
  return(mymat)
}
#
###############################################################################
#Q1 FIG 2 - overall sitewide open urchin densities 
##############################################################################
# ## Kruskal Wallace test
kruskal.test(OpenUrchins~ SiteCode, data = oregon)
# ## Test is significant.
#
# ## Pairwise Wilcoxin Rank Sum test for open urchins by site 
open <- pairwise.wilcox.test(
  x = oregon$OpenUrchins,
  g = oregon$SiteCode,
  p.adjust.method = "fdr"
)

#do I need the below?????
# ## Convert the p-value output table into a matrix.
open <- data.matrix(open$p.value)

## Use the function to make the p-value matrix symmetrical.
open <- tri.to.squ(open)

# ## Generate letters to represent significant differences in urchin abundance between sites.
open_letters <- multcompLetters(open,compare="<=", threshold=0.05, Letters=letters)


# # now have to create matrix properly to
pw <- pairwise.wilcox.test(
  x = oregon$OpenUrchins,
  g = oregon$SiteCode,
  p.adjust.method = "fdr"
)
# 
tri <- pw$p.value
sites <- sort(unique(oregon$SiteCode))
# 
full <- matrix(NA, length(sites), length(sites),
               dimnames = list(sites, sites))
# 
full[rownames(tri), colnames(tri)] <- tri
full[colnames(tri), rownames(tri)] <- t(tri)
# 
full[is.na(full)] <- 1
# 
letters <- multcompLetters(full)$Letters
letters_df <- data.frame(SiteCode = names(letters),
                         Letter = letters)
# 
plot_df <- oregon %>%
  left_join(letters_df, by = "SiteCode")

plot_df$SiteCode <- factor(plot_df$SiteCode, levels=c("CP", "WC" ,
                                                      "RP", "CB", "SC", "SB", "SH",
                                                      "YB", "BB", "FC"))
# 
# ###################CREATE PLOT#################################################
######## FOR ALL SITES ####################
# 
# 
pl1 <- ggplot(plot_df, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
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
    y = "Open Urchin Density (count per 0.25m²) in both subhabitats"
  ) +
  scale_fill_manual(values = connect) +
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
       plot =pl1  , width = 8, height = 6, dpi = 300)

######## UPZ ONLY ############################################################
# FIG. NO. 3
##############################################################################
OregonUPZ<- oregon %>%
  filter(Subhabitat=="UPZ")
# 
OregonUPZ$SiteCode <- factor(OregonUPZ$SiteCode, levels=c("CP", "WC" ,
                                                          "RP", "CB", "SC", "SB", "SH",
                                                          "YB", "BB", "FC"))
kruskal.test(NonPit ~ SiteCode, data = OregonUPZ)
# 
# # haave to create matrix properly to
pw <- pairwise.wilcox.test(
  x = OregonUPZ$OpenUrchins,
  g = OregonUPZ$SiteCode,
  p.adjust.method = "fdr"
)
# 
tri <- pw$p.value
sites <- sort(unique(OregonUPZ$SiteCode))
# 
full <- matrix(NA, length(sites), length(sites),
               dimnames = list(sites, sites))
# 
full[rownames(tri), colnames(tri)] <- tri
full[colnames(tri), rownames(tri)] <- t(tri)
# 
full[is.na(full)] <- 1
# 
letters <- multcompLetters(full)$Letters
letters_df <- data.frame(SiteCode = names(letters),
                         Letter = letters)
# 
plot_df <- OregonUPZ %>%
  left_join(letters_df, by = "SiteCode")

plot_df$SiteCode <- factor(plot_df$SiteCode, levels=c("CP", "WC" ,
                                                      "RP", "CB", "SC", "SB", "SH",
                                                      "YB", "BB", "FC"))
# 
# 
# 
pl4 <- ggplot(plot_df, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
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
    y = "Open Urchin Density (count per 0.25m²) in Urchin Pit Dominated Subhabiat"
  ) +
  scale_fill_manual(values = connect) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()

ggsave(filename = "Figures/Surveys/Q1/UrchinDensitiesbySiteOpeninUPZ.png", 
       plot =pl4  , width = 8, height = 6, dpi = 300)
###########only NPZ#############################################################
#FIG. NO. 4
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

plot_df$SiteCode <- factor(plot_df$SiteCode, levels=c("CP", "WC" ,
                                                      "RP", "CB", "SC", "SB", "SH",
                                                      "YB", "BB", "FC"))

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
  scale_fill_manual(values = connect) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  ) +
  coord_flip()


ggsave(filename = "Figures/Surveys/Q1/UrchinDensitiesbySiteOpeninNPZ.png", 
       plot =pl5  , width = 8, height = 6, dpi = 300)


################################################################################
# Q2 FIG. NO. 1 Kelp Abundance Diffs in Subhabitats 
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
gam_dat<- OnlyUrch %>%
mutate(
  PredCryp = predict(m_cryp, type = "response"),
  PredOpen = predict(m_open, type = "response")
) %>%
  pivot_longer(
    cols = c(Cryptic, OpenUrchins),
    names_to = "UrchinBehavior",
    values_to = "UrchinDensity"
  ) 

gam_dat$UrchinBehavior <- as.factor(gam_dat$UrchinBehavior)


#GAM FROM ZECH
#Generic model structure

#kelp_canopy ~ gam(s(urchin_density) + Site + Behavior, family = tw) # tw for Tweedie distribution

# Generalized Additive Model (GAM) structure and diagnostics

model_urch_den <- gam(data = gam_dat, method = "REML", family = tw,
                       TotalCanopy ~ UrchinBehavior + SiteCode +
                         s(UrchinDensity, bs = "tp", k=7, by = UrchinBehavior)) # set k to integers from 1 to 10ish and see what fits best
summary(model_urch_den)
plot(model_urch_den, residuals=TRUE)
gam.check(model_urch_den)

# GAM plot

urch_den_plot <-
  ggplot(data = gam_dat, aes(x=UrchinDensity, y=TotalCanopy, color = UrchinBehavior)) +
  geom_point(shape = 20, alpha = 0.4, show.legend = F) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k=3), se = F, # makes per beahvior model fit
              linewidth = 1.25, method.args = list(family = tw)) +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k=2), se = F, # makes overall model fit
   #           linewidth = 2, method.args = list(family = tw), color = "black") +
  facet_wrap(~ SiteCode) +
  scale_color_manual(
    values = c(
      "Cryptic" = "#EA4F0DFF",
      "OpenUrchins" = "#4490FEFF"
    )
  ) +
  scale_x_continuous(name = bquote("Urchin Density"),
                    limits = c(0, 50)) +
  scale_y_continuous(name = bquote("Kelp Canopy % cover")) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

                    
ggsave(filename = "Figures/Surveys/urch_gam.png", 
       plot =urch_den_plot  , width = 8, height = 6, dpi = 300)


# try something else

# Generalized Additive Model (GAM) structure and diagnostics

model_urch_den <- gam(data = gam_dat, method = "REML", family = tw,
                      TotalCanopy ~ UrchinBehavior + SiteCode +
                        s(UrchinDensity, bs = "tp", k=7, by = UrchinBehavior)) # set k to integers from 1 to 10ish and see what fits best
summary(model_urch_den)
plot(model_urch_den, residuals=TRUE)
gam.check(model_urch_den)

# GAM plot

urch_den_plot <-
  ggplot(data = gam_dat, aes(x=UrchinDensity, y=TotalCanopy, color = New_Substrate)) +
  geom_point(shape = 20, alpha = 0.4, show.legend = F) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k=7), se = F, # makes per beahvior model fit
              linewidth = 1.25, method.args = list(family = tw)) +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k=2), se = F, # makes overall model fit
  #           linewidth = 2, method.args = list(family = tw), color = "black") +
 # facet_wrap(~ SiteCode) +
 # scale_color_manual(site_cols) +
  scale_x_continuous(name = bquote("Urchin Density"),
                     limits = c(0, 50)) +
  scale_y_continuous(name = bquote("Kelp Canopy % cover")) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

