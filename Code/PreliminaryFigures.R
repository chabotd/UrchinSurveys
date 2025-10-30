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


################################################################################
#Set Up data frame 
################################################################################
urch <- read.csv("Data/UrchinSurveys.csv")

# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))
# add column for Empty:Present Pits
urch<- urch %>%
  rename(TotalPits = PitsPresent)
# urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)

#add column for mean size

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))
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



################################################################################
######## for if we want to look only at certain years or certain habitats 
################################################################################

PitUrch <- urch %>%
  filter(Subhabitat %in% c("UPZ"))

#urch2024<- urch%>%
  filter(Year %in% c("2024"))

PitUrch2024 <- urch2024 %>%
  filter(Subhabitat %in% c("UPZ"))

#urch2025<- urch %>%
  filter(Year %in% c("2025"))

PitUrch2025 <- urch2025 %>%
  filter(Subhabitat %in% c("UPZ"))

urch2024 <- OnlyUrch%>%
  filter(Year %in% c("2024"))

urch2025 <- OnlyUrch%>%
  filter(Year %in% c("2025"))


################################################################################
#kelp canopy between subhabs
################################################################################

# check for normality: 
#- do not run above change from zone to subhabitat

# QQ plots
qqnorm(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "UPZ"]); 
qqline(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "UPZ"])
qqnorm(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "NPZ"]); 
qqline(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "NPZ"])

# Shapiro-Wilk test
shapiro.test(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "UPZ"])
shapiro.test(OnlyUrch$Total.Canopy[OnlyUrch$Zone == "NPZ"])

# data are not normally distributed: need wilcoxin rank sum test. 

wilcox.test(Total.Canopy ~ Subhabitat, data = OnlyUrch)
# Calculate medians and IQR for each Zone
summary_stats <- OnlyUrch %>%
  group_by(Subhabitat) %>%
  summarise(
    median_canopy = median(Total.Canopy, na.rm = TRUE),
    lower = quantile(Total.Canopy, 0.25, na.rm = TRUE),
    upper = quantile(Total.Canopy, 0.75, na.rm = TRUE)
  )

# Create the bar plot
mediankelp <- ggplot(summary_stats, aes(x = Subhabitat, y = median_canopy, 
                                        fill = Subhabitat)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Median Kelp Canopy by Subhabitat",
    x = "Subhabitat",
    y = "Median Total Canopy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/mediankelp.png", 
       plot = mediankelp , width = 8, height = 6, dpi = 300)

#jitterplot 

medianjitter2 <- ggplot(OnlyUrch, aes(x = Subhabitat, y = Total.Canopy, fill = 
                                        Subhabitat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white",
               color = "black") +
  labs(
    title = "Percent Kelp Canopy by Subhabitat",
    x = "Subhabitat",
    y = "Kelp Canopy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(filename = "Figures/medianjitter2.png", 
       plot = medianjitter2 , width = 8, height = 6, dpi = 300)

# by year and by sub hab

yearSub <- ggplot(OnlyUrch, aes(x = Subhabitat, y = Total.Canopy, 
                                fill = Subhabitat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Percent Kelp Canopy by Subhabitat and Year",
    x = "Subhabitat",
    y = "Total.Canopy",
    fill = "Subhabitat"
  ) +
  facet_wrap(~ Year) +
  theme_minimal()

ggsave(filename = "Figures/canopybyyear.png", 
       plot = yearSub , width = 8, height = 6, dpi = 300)

#by year Wilcox 

wilcox.test(Total.Canopy ~ Subhabitat, data = urch2024)
wilcox.test(Total.Canopy ~ Subhabitat, data = urch2025)

################################################################################
#pitted numbers and nonpitted numbers by site 
################################################################################



################################################################################
#urch count between subhabs
################################################################################


wilcox.test(TotalUrchins ~ Subhabitat, data = OnlyUrch)
wilcox.test(TotalUrchins ~ Subhabitat, data = urch2024)
wilcox.test(TotalUrchins ~ Subhabitat, data = urch2025)
# Calculate medians and IQR for each Zone
summary_stats <- OnlyUrch %>%
  group_by(Subhabitat) %>%
  summarise(
    median_urch = median(TotalUrchins, na.rm = TRUE),
    lower = quantile(TotalUrchins, 0.25, na.rm = TRUE),
    upper = quantile(TotalUrchins, 0.75, na.rm = TRUE)
  )

# Create the bar plot with median 
medianurch <- ggplot(summary_stats, aes(x = Subhabitat, y = median_urch, fill = 
                                          Subhabitat)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Median Urchin Count by Subhabitat",
    x = "Subhabitat",
    y = "Median Number of Urchins Per Plot"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#create boxplot with jitter plots 

medianjitter <- ggplot(OnlyUrch, aes(x = Subhabitat, y = TotalUrchins, fill = 
                                       Subhabitat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Urchin Count by Subhabitat",
    x = "Subhabitat",
    y = "Urchin Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# by year facet

subhaburchyear <- ggplot(OnlyUrch, aes(x = Subhabitat, y = TotalUrchins, fill = 
                                       Subhabitat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  labs(
    title = "Urchin Count by Subhabitat",
    x = "Subhabitat",
    y = "Urchin Count"
  ) +
  theme_minimal() +
  facet_wrap(~ Year) +
  theme(legend.position = "none")

ggsave(filename = "Figures/subhaburchyear.png", 
       plot = subhaburchyear , width = 8, height = 6, dpi = 300)


medianjitter <- ggplot(OnlyUrch, aes(x = Subhabitat, y = TotalUrchins, fill = 
                                       Subhabitat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white",
               color = "black") +
  labs(
    title = "Urchin Count by Subhabitat",
    x = "Subhabitat",
    y = "Urchin Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/medianjitter.png", 
       plot = medianjitter , width = 8, height = 6, dpi = 300)

# calculate mean and SE if wanted
summary_stats <- OnlyUrch %>%
  group_by(Subhabitat) %>%
  summarise(
    mean_urch = mean(TotalUrchins, na.rm = TRUE),
    se = sd(TotalUrchins, na.rm = TRUE) / sqrt(n())
  )

ggplot(summary_stats, aes(x = Subhabitat, y = mean_urch, fill = Subhabitat)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_urch - se, ymax = mean_urch + se), width = 0.2) +
  labs(
    title = "Mean Urchin Count by Subhabitat",
    x = "Subhabitat",
    y = "Mean Urchin Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# calculate CI if wanted 

summary_stats <- OnlyUrch %>%
  group_by(Subhabitat) %>%
  summarise(
    mean_urch = mean(TotalUrchins, na.rm = TRUE),
    se = sd(TotalUrchins, na.rm = TRUE) / sqrt(n()),
    lower = mean_urch - 1.96 * se,
    upper = mean_urch + 1.96 * se
  )


ggplot(summary_stats, aes(x = Subhabitat, y = mean_urch, fill = Subhabitat)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Mean Urchin Count by Subhabitat",
    subtitle = "Error bars represent 95% confidence intervals",
    x = "Subhabitat",
    y = "Mean Urchin Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################
#Size diffs 
################################################################################
# Between sub-habitats 

t.test(MeanTest ~ Subhabitat, data = OnlyUrch)
ggplot(OnlyUrch, aes(x = Zone, y = MeanTest)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Size Differences",
       x = "Type of Subhabitat",
       y = "Mean Urchin Test Size (cm)") +
  theme_minimal()

# between years (can't do yet)
ggplot(OnlyUrch, aes(x = factor(Year), y = MeanTest)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Size Differences",
       x = "Year",
       y = "Mean Urchin Test Size (cm)") +
  theme_minimal()

################################################################################
#Major diffs between years
################################################################################
# total urchins in urchin-dominated plots 

wilcox.test(TotalUrchins ~ factor(Year), data = OnlyUrch)

yearurch <- ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins, fill = Year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  labs(
    title = "Urchin Count by Year",
    x = "Year",
    y = "Urchin Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/yearurch.png", 
       plot = yearurch , width = 8, height = 6, dpi = 300)

#old t-test
# t.test(TotalUrchins ~ factor(Year), data = OnlyUrch)
# totalurch <- ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins)) +
#   geom_boxplot(fill = "lavender") +
#   labs(title = "Urchin Count Year Differences",
#        x = "Year",
#        y = "Total Urchins") +
#   theme_minimal()
# 
# ggsave(filename = "Figures/TotalUrchin.png", 
#        plot = totalurch , width = 8, height = 6, dpi = 300)


# algal canopy in urchin-dominated plots

wilcox.test(Total.Canopy ~ factor(Year), data = OnlyUrch)

canopy_year <- ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy, fill = 
                                      Year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", 
               color = "black") +
  labs(
    title = "Kelp Canopy by Year",
    x = "Year",
    y = "Kelp Canopy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/CanopyYear.png",
       plot = canopy_year, width =8, height=6, dpi=300)

# assumptions not met for t test 
# t.test(Total.Canopy ~ factor(Year), data = OnlyUrch)
# ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy)) +
#   geom_boxplot(fill = "darkgreen") +
#   labs(title = "Canopy Year Differences",
#        x = "Year",
#        y = "Total Canopy") +
#   theme_minimal()

# changes in juveniles?

wilcox.test(JuvenileUrchins ~ factor(Year), data = OnlyUrch)
juvies <- ggplot(OnlyUrch, aes(x = factor(Year), y = JuvenileUrchins)) +
  geom_boxplot(fill = "violet") +
  labs(title = "Juvenile Count Differences",
       x = "Year",
       y = "Number of Juveniles per Plot") +
  theme_minimal()

ggsave(filename = "Figures/juvies.png", 
       plot = juvies , width = 8, height = 6, dpi = 300)

################################################################################
# linear regression canopy and urchins 
################################################################################
#for all urchins
ggplot(OnlyUrch, aes(x=logUrchins, y=logCanopy) +
   geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Total Urchins to Total Canopy',
       x='Urchin Count', y='Total Canopy (percent)')
# lm_model <- lm(Total.Canopy ~ TotalUrchins, data = OnlyUrch)

######## ANCOVA to see relationship differs between your two zones
#("UPZ" and "NPZ"), you'll want to expand it into an ANCOVA model
#by including Zone and its interaction with TotalUrchins.

OnlyUrch$Subhabitat <- as.factor(OnlyUrch$Subhabitat)
ancova_model <- lm(Total.Canopy ~ TotalUrchins * Subhabitat, data = OnlyUrch)
summary(ancova_model)

 ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = Subhabitat)) +
   geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log")), se = FALSE) +
  labs(title = "Kelp Canopy vs. Urchin Abundance by Subhabitat",
        x = "Total Urchins",
        y = "Total Canopy (%)")



# ancova_plot <- ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = Subhabitat)) +
#   geom_point(size = 2, alpha = 0.7) +
#   stat_smooth(method = "lm", se = FALSE) +
#   labs(
#     title = "ANCOVA: Urchin Count vs Kelp Canopy by Subhabitat",
#     x = "Urchin Count",
#     y = "Total Canopy (%)",
#     color = "Subhabitat"
#   ) +
#   theme_minimal()
# 
# ggsave("Figures/ANCOVA_Urchins_Canopy.png", plot = ancova_plot, 
#        width = 8, height = 6, dpi = 300)

# #check assumptions QQ plot
# hist(residuals(ancova_model))
qqnorm(residuals(ancova_model)); qqline(residuals(ancova_model))
# 
# #### looks ok in middle- tails have some drop off-- 
# #indicating heavy tailed - extreme values more likely
# plot(ancova_model, which = 1)  # Residuals vs Fitted
# 
# #try a log transformation
# 
 OnlyUrch$logUrchins <- log(OnlyUrch$TotalUrchins + 1)
 lm_log_model <- lm(Total.Canopy ~ logUrchins * Subhabitat, data = OnlyUrch)
# 
# hist(residuals(lm_log_model))
qqnorm(residuals(lm_log_model)); qqline(residuals(lm_log_model))
# 
# ## still not good
# 
# OnlyUrch$sqrtUrchins <- sqrt(OnlyUrch$TotalUrchins)
# lm_sqrt_model <- lm(Total.Canopy ~ sqrtUrchins * Subhabitat, data = OnlyUrch)
# 
# hist(residuals(lm_sqrt_model))
# qqnorm(residuals(lm_sqrt_model)); qqline(residuals(lm_sqrt_model))
# 
# #Cooks Distance
# 
# plot(ancova_model, which = 4)  # Cook's distance
# 
# # canopy instead?
# 
OnlyUrch$logCanopy <- log(OnlyUrch$Total.Canopy + 1)
 lm_log_response <- lm(logCanopy ~ TotalUrchins * Subhabitat, data = OnlyUrch)
# 
# hist(residuals(lm_log_response))
 qqnorm(residuals(lm_log_response)); qqline(residuals(lm_log_response))

# better maybe? 
 #both logged
OnlyUrch$logUrchins <- log(OnlyUrch$TotalUrchins + 1)
lm_log2_response <- lm(logCanopy ~ logUrchins * Subhabitat, data = OnlyUrch)
qqnorm(residuals(lm_log2_response)); qqline(residuals(lm_log2_response))

ancova_modellog <- lm(logCanopy ~ logUrchins * Subhabitat, data = OnlyUrch)
summary(ancova_modellog)

ggplot(OnlyUrch, aes(x = logUrchins, y = logCanopy, color = Subhabitat)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log")), se = FALSE) +
  labs(title = "Log Kelp Canopy vs. Log Urchin Abundance by Subhabitat",
       x = "Log Total Urchins",
       y = "Log Total Canopy (%)")


#try a GLM instead

# OnlyUrch$Canopy_Pos <- OnlyUrch$Total.Canopy + 0.01
# 
# glm_model <- glm(Canopy_Pos ~ TotalUrchins * Subhabitat,
#                  data = OnlyUrch,
#                  family = Gamma(link = "log"))
# summary(glm_model)
# 
# ggplot(OnlyUrch, aes(x = TotalUrchins, y = log(Total.Canopy + 1), color 
#= Subhabitat)) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Log-Transformed Kelp Canopy vs. Urchin Abundance",
#        x = "Total Urchins",
#        y = "log(Total Canopy + 1)") +
#   theme_minimal()

# did not work well

#####Tweedie

# Fit a Tweedie GLM
glm_tweedie <- glm(Total.Canopy ~ TotalUrchins * Subhabitat,
                   data = OnlyUrch,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie)


OnlyUrch$PredictedCanopy <- predict(glm_tweedie, type = "response")


ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = Subhabitat)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth= 1) +
  labs(
    title = "Tweedie GLM: Urchin Count vs Kelp Canopy by Subhabitat",
    x = "Urchin Count",
    y = "Observed and Predicted Canopy (%)",
    color = "Subhabitat"
  ) +
  theme_minimal()

# for fun:

pred_data <- OnlyUrch %>%
  select(TotalUrchins, Subhabitat) %>%
  distinct() %>%
  arrange(Subhabitat, TotalUrchins)

preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

urch_tweedie_plot<- ggplot() +
  # Actual data points
  geom_point(data = OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy, color = 
                                    Subhabitat), alpha = 0.6) +
  
  # Prediction line
  geom_line(data = pred_data, aes(x = TotalUrchins, y = fit), color = "black", 
            size = 1) +
  
  # Confidence interval ribbon
  geom_ribbon(data = pred_data, aes(x = TotalUrchins, ymin = lower, ymax = upper), 
              fill = "gray", alpha = 0.3) +
  
  # Facet by Zone
  facet_wrap(~ Subhabitat) +
  
  labs(
    title = "Tweedie GLM: Urchin Count vs Kelp Canopy by Subhabitat",
    x = "Urchin Count",
    y = "Predicted Canopy (%)"
  ) +
  theme_minimal()

ggsave("Figures/Tweedie_GLM_Canopy_Plot.png", plot = urch_tweedie_plot, 
       width = 10, height = 6, dpi = 300)

################################################################################
# #Kruskal Wallis Tests
################################################################################

##########################################################
#Canopy cover all sites- test 4 Sept 2025
##########################################################

#by site

kruskal.test(Total.Canopy ~ SiteCode, data = OnlyUrch)

CanopySite <- ggplot(OnlyUrch, aes(x = SiteCode, y = Total.Canopy, fill = 
                                     SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Kelp Canopy by Site",
    x = "Site",
    y = "Total Canopy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pairwise.wilcox.test(OnlyUrch$Total.Canopy, OnlyUrch$SiteCode, p.adjust.method = 
                       "BH")

ggsave(filename = "Figures/CanopySite.png", 
       plot = CanopySite , width = 8, height = 6, dpi = 300)

#by year:

CanopySiteYear <- ggplot(OnlyUrch, aes(x = SiteCode, y = Total.Canopy, fill = 
                                     SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  labs(
    title = "Kelp Canopy by Site",
    x = "Site",
    y = "Total Canopy (%)"
  ) +
  facet_wrap(~ Year) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/CanopySiteyear.png", 
       plot = CanopySiteYear , width = 8, height = 6, dpi = 300)

###############################################################################
# cryptic
###############################################################################
glm_tweedie_cryp <- glm(Total.Canopy ~ Cryptic,
                        data = OnlyUrch,
                        family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie_cryp)


OnlyUrch$PredictedCanopy <- predict(glm_tweedie_cryp, type = "response")


ggplot(OnlyUrch, aes(x = Cryptic, y = Total.Canopy)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth= 1) +
  labs(
    x = "Urchin Density",
    y = "KelpCanopy (%)",
  ) +
  theme_minimal()

# by open

glm_tweedie_open <- glm(Total.Canopy ~ OpenUrchins,
                        data = OnlyUrch,
                        family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie_open)


OnlyUrch$PredictedCanopy <- predict(glm_tweedie_open, type = "response")


ggplot(OnlyUrch, aes(x = OpenUrchins, y = Total.Canopy)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth= 1) +
  labs(
    x = "Urchin Density",
    y = "KelpCanopy (%)",
  ) +
  theme_minimal()

#log does not work

#log

glm_tweedie <- glm(logCanopy ~ logUrchins * Subhabitat,
                   data = OnlyUrch,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie)


OnlyUrch$PredictedCanopy <- predict(glm_tweedie, type = "response")


ggplot(OnlyUrch, aes(x = logUrchins, y = logCanopy, color = Subhabitat)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedCanopy), linewidth= 1) +
  labs(
    x = "log Urchin Density",
    y = "log KelpCanopy (%)",
    color = "Subhabitat"
  ) +
  theme_minimal()

##########################################################
#Urchin Counts - all sites
##########################################################

#by site

kruskal.test(TotalUrchins ~ SiteCode, data = OnlyUrch)
kruskal.test(TotalUrchins ~ SiteCode, data = urch2024)
kruskal.test(TotalUrchins ~ SiteCode, data = urch2025)

urchcount <- ggplot(OnlyUrch, aes(x = SiteCode, y = TotalUrchins, fill = 
                                    SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(filename = "Figures/UrchCountTotal.png", 
       plot = urchcount , width = 8, height = 6, dpi = 300)
#by year facet

urchcountYear <- ggplot(OnlyUrch, aes(x = SiteCode, y = TotalUrchins, fill = 
                                    SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  labs(
    title = "Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  facet_wrap(~ Year) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/UrchCountTotalYear.png", 
       plot = urchcountYear , width = 8, height = 6, dpi = 300)

pairwise.wilcox.test(OnlyUrch$TotalUrchins, OnlyUrch$SiteCode, p.adjust.method = "BH")


urch_dunn_results <- dunnTest(TotalUrchins ~ SiteCode, data = OnlyUrch, method = "bh")

# Extract results
dunn_df_urch <- urch_dunn_results$res

dunn_cld_urch <- cldList(P.adj ~ Comparison,
                    data = urch_dunn_results$res,
                    threshold = 0.05)
# Merge CLDs with site medians for positioning
site_medians_urch <- OnlyUrch %>%
  group_by(SiteCode) %>%
  summarize(median_urch = median(TotalUrchins))

cld_labels_urch <- left_join(site_medians_urch, dunn_cld, by = c("SiteCode" = "Group"))


UrchSite <- ggplot(OnlyUrch, aes(x = SiteCode, y = TotalUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_compare_means(method = "kruskal.test", label.y = max(OnlyUrch$TotalUrchins) + 10) +  # global test
  #stat_compare_means(comparisons = pairwise_list, method = "wilcox.test") +
  labs(
    title = "Total Urchins by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

newfig2 <- UrchSite +
  geom_text(data = cld_labels_urch,
            aes(x = SiteCode, y = median_urch + 100, label = Letter),
            color = "black",
            size = 4)


ggsave(filename = "Figures/UrchinPairwise.png", 
       plot = newfig2 , width = 8, height = 6, dpi = 300)
# Dunn's test with Benjamini-Hochberg correction

# Run Dunn's test with Benjamini-Hochberg correction
dunn_results <- dunnTest(Total.Canopy ~ SiteCode, data = OnlyUrch, method = "bh")

# Extract results
dunn_df <- dunn_results$res

dunn_cld <- cldList(P.adj ~ Comparison,
                    data = dunn_results$res,
                    threshold = 0.05)
# Merge CLDs with site medians for positioning
site_medians <- OnlyUrch %>%
  group_by(SiteCode) %>%
  summarize(median_canopy = median(Total.Canopy))

cld_labels <- left_join(site_medians, dunn_cld, by = c("SiteCode" = "Group"))


CanopySite <- ggplot(OnlyUrch, aes(x = SiteCode, y = Total.Canopy, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_compare_means(method = "kruskal.test", label.y = max(OnlyUrch$Total.Canopy) + 10) +  # global test
 # stat_compare_means(comparisons = pairwise_list, method = "wilcox.test", label = "p.signif") +
  labs(
    title = "Kelp Canopy by Site",
    x = "Site",
    y = "Total Canopy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

newfig <- CanopySite +
  geom_text(data = cld_labels,
            aes(x = SiteCode, y = median_canopy + 100, label = Letter),
            color = "black",
            size = 4)

ggsave(filename = "Figures/CanopyPairwise.png", 
       plot = newfig , width = 8, height = 6, dpi = 300)
##########################################################
#NonPit - all sites
##########################################################

kruskal.test(NonPit ~ SiteCode, data = OnlyUrch)

ggplot(OnlyUrch, aes(x = SiteCode, y = NonPit, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Nonpitted Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pairwise.wilcox.test(OnlyUrch$NonPit, OnlyUrch$SiteCode, p.adjust.method = "BH")

##########################################################
#Pitted- all sites
##########################################################

kruskal.test(PittedUrchins ~ SiteCode, data = OnlyUrch)

pl2 <- ggplot(OnlyUrch, aes(x = SiteCode, y = PittedUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Pitted Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pairwise.wilcox.test(OnlyUrch$NonPit, OnlyUrch$SiteCode, p.adjust.method = "BH")

ggsave(filename = "Figures/pl2.png", 
       plot = pl2 , width = 8, height = 6, dpi = 300)
##########################################################
#Non Pitted- all sites
##########################################################

kruskal.test(OpenUrchins ~ SiteCode, data = OnlyUrch)

pl1 <- ggplot(OnlyUrch, aes(x = SiteCode, y = OpenUrchins, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Non-cryptic Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Figures/pl1.png", 
       plot = pl1 , width = 8, height = 6, dpi = 300)
#Combine? option 1

urch_long <- OnlyUrch %>%
  select(SiteCode, NonPit, PittedUrchins) %>%
  pivot_longer(cols = c(NonPit, PittedUrchins),
               names_to = "UrchinType",
               values_to = "Count")

ggplot(urch_long, aes(x = SiteCode, y = Count, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ UrchinType, scales = "free_y") +
  labs(
    title = "Urchin Counts by Site",
    subtitle = "Comparison of Pitted vs Nonpitted Urchins",
    x = "Site",
    y = "Urchin Abundance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#option 2


urchincounts <- ggplot(urch_long, aes(x = SiteCode, y = Count, fill = UrchinType)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(values = c("NonPit" = "#a6cee3", "PittedUrchins" = "#1f78b4")) +
  labs(
    title = "Urchin Counts by Site",
    x = "Site",
    y = "Urchin Abundance",
    fill = "Urchin Type"
  ) +
  theme_minimal()

ggsave(filename = "Figures/UrchinCounts.png", 
       plot = urchincounts , width = 8, height = 6, dpi = 300)

#################################################################################
#now for the pit ratio work
#################################################################################
# # do the number of pits change per year?
# wilcox.test(PitsPresent ~ factor(Year), data = OnlyUrch)
# ggplot(OnlyUrch, aes(x = factor(Year), y = PitsPresent)) +
#   geom_boxplot(fill = "violet") +
#   labs(title = "Pits by Year",
#        x = "Year",
#        y = "Number of Pits present in a plot") +
#   theme_minimal()

# aov(PitsPresent ~ SiteCode, data = PitUrch)
# ggplot(PitUrch, aes(x = SiteCode, y = PitsPresent)) +
#   geom_boxplot(fill = "violet") +
#   labs(title = "Pits by Site",
#        x = "Site",
#        y = "Number of Pits present in a plot") +
#   theme_minimal()
# 
# #2024
# ggplot(PitUrch2024, aes(x = SiteCode, y = PitsPresent)) +
#   geom_boxplot(fill = "blue") +
#   labs(title = "Pits by Site 2024",
#        x = "Site",
#        y = "Number of Pits present in a plot 2024") +
#   theme_minimal()
# #2025
# 
# ggplot(PitUrch2025, aes(x = SiteCode, y = PitsPresent)) +
#   geom_boxplot(fill = "violet") +
#   labs(title = "Pits by Site 2025",
#        x = "Site",
#        y = "Number of Pits present in a plot 2025") +
#   theme_minimal()

#empty pits and canopy (no effect)

# ggplot(PitUrch, aes(x = EmptyPits, y = Total.Canopy)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
#   labs(title = 'Empty Pits vs Kelp Canopy',
#        x = 'Empty Pits Present', y = 'Total Canopy (%)')
# 
# lm_ratio <- lm(Total.Canopy ~ EmptyPits, data = PitUrch)
# 
# anova(lm_ratio)
# 
# ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy.png", 
#        plot = , width = 8, height = 6, dpi = 300)

#### Ratio of pits in pit zones #############################################

# don't look at AZ or NPZ-- urchin-PIT dominated zone only. 


glm_tweedie <- glm(Total.Canopy ~ PercentOccupancy, data = PitUrch,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie)

# Create a new data frame for prediction
pred_data <- data.frame(PercentOccupancy = seq(min(PitUrch$PercentOccupancy, na.rm = TRUE),
                                        max(PitUrch$PercentOccupancy, na.rm = TRUE),
                                        length.out = 100))

# Predict from Tweedie GLM
preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

# Add predictions and CI to the data frame
pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

# deal with the year thing

glm_tweedie <- glm(Total.Canopy ~ PercentOccupancy * Year,
                   data = PitUrch,
                   family = tweedie(var.power = 1.5, link.power = 0))

summary(glm_tweedie)
 # try 2

pred_data <- expand.grid(
  PercentOccupancy = seq(min(PitUrch$PercentOccupancy, na.rm = TRUE),
                         max(PitUrch$PercentOccupancy, na.rm = TRUE),
                         length.out = 100),
  Year = levels(PitUrch$Year)
)

# Predict
preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

# Add predictions
pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

ggplot() +
  geom_point(data = PitUrch, aes(x = PercentOccupancy, y = Total.Canopy), alpha = 0.6, color = "darkseagreen4") +
  geom_line(data = pred_data, aes(x = PercentOccupancy, y = fit), color = "mediumpurple", size = 1) +
  geom_ribbon(data = pred_data, aes(x = PercentOccupancy, ymin = lower, ymax = upper), fill = "purple4", alpha = 0.2) +
  facet_wrap(~ Year) +
  labs(
    title = "Pit Occupancy vs Kelp Canopy by Year",
    x = "Pit Occupancy (%)",
    y = "Kelp Canopy (%)"
  ) +
  theme_minimal()


# Create prediction grid for each year
pred_data <- expand.grid(
  PercentOccupancy = seq(min(PitUrch$PercentOccupancy, na.rm = TRUE),
                         max(PitUrch$PercentOccupancy, na.rm = TRUE),
                         length.out = 100),
  Year = unique(PitUrch$Year)
)

# Predict
preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

# Add predictions
pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

pitsyear <- ggplot() +
  geom_point(data = PitUrch, aes(x = PercentOccupancy, y = Total.Canopy), alpha = 0.6, color = "darkseagreen4") +
  geom_line(data = pred_data, aes(x = PercentOccupancy, y = fit), color = "mediumpurple", size = 1) +
  geom_ribbon(data = pred_data, aes(x = PercentOccupancy, ymin = lower, ymax = upper), fill = "purple4", alpha = 0.2) +
  facet_wrap(~ Year) +
  labs(
    title = "Pit Occupancy vs Kelp Canopy by Year",
    x = "Pit Occupancy",
    y = "Kelp Canopy (%)"
  ) +
  theme_minimal()

ggsave(filename = "Figures/PitOccupancyYear.png", 
       plot = pitsyear , width = 8, height = 6, dpi = 300)

# without year facet
pits <- ggplot() +
  geom_point(data = PitUrch, aes(x = PercentOccupancy, y = Total.Canopy), alpha = 0.6, color = "darkseagreen4") +
  geom_line(data = pred_data, aes(x = PercentOccupancy, y = fit), color = "mediumpurple", size = 1) +
  geom_ribbon(data = pred_data, aes(x = PercentOccupancy, ymin = lower, ymax = upper), fill = "purple4", alpha = 0.2) +
  labs(
    title = "Pit Occupancy vs Kelp Canopy",
    x = "Pit Occupancy",
    y = "Kelp Canopy (%)"
  ) +
  facet_wrap(~ Year) +
  theme_minimal()

ggsave(filename = "Figures/PitOccupancy.png", 
       plot = pits , width = 8, height = 6, dpi = 300)


########################### attached Drift 2024 tweedie and pit occupancy 


glm_tweedie <- glm(TotalAttachedDrift ~ PercentOccupancy, data = PitUrch2024,
                   family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_tweedie)

# Create a new data frame for prediction
pred_data <- data.frame(PercentOccupancy = seq(min(PitUrch2024$PercentOccupancy, na.rm = TRUE),
                                               max(PitUrch2024$PercentOccupancy, na.rm = TRUE),
                                               length.out = 100))

# Predict from Tweedie GLM
preds <- predict(glm_tweedie, newdata = pred_data, type = "response", se.fit = TRUE)

# Add predictions and CI to the data frame
pred_data$fit <- preds$fit
pred_data$se <- preds$se.fit
pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
pred_data$upper <- pred_data$fit + 1.96 * pred_data$se

attached <- ggplot() +
  geom_point(data = PitUrch2024, aes(x = PercentOccupancy, y = TotalAttachedDrift), alpha = 0.6, color = "darkseagreen4") +
  geom_line(data = pred_data, aes(x = PercentOccupancy, y = fit), color = "mediumpurple", size = 1) +
  geom_ribbon(data = pred_data, aes(x = PercentOccupancy, ymin = lower, ymax = upper), fill = "purple4", alpha = 0.2) +
  labs(
    title = "Pit Occupancy vs Total Attached Drift",
    x = "Pit Occupancy",
    y = "Total Attached Drift (%)"
  ) +
  theme_minimal()


ggsave(filename = "Figures/PitOccupancyDrift.png", 
       plot = attached , width = 8, height = 6, dpi = 300)

# 
# ratio <- ggplot(PitUrch, aes(x = RatioPits, y = Total.Canopy)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
#   labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy',
#        x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')
# 
# lm_ratio <- lm(Total.Canopy ~ RatioPits, data = PitUrch)
# summary(lm_ratio)
# anova(lm_ratio)
# 
# ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy.png", 
#        plot = ratio , width = 8, height = 6, dpi = 300)
# 
# 
# Pit2024<- PitUrch %>%
#   filter(Year %in% c("2024"))
# 
# ratio2024 <- ggplot(Pit2024, aes(x = RatioPits, y = Total.Canopy)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
#   labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy 2024',
#        x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')
# 
# ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy2024.png", 
#        plot = ratio2024 , width = 8, height = 6, dpi = 300)
# 
# lm_ratio2024 <- lm(Total.Canopy ~ RatioPits, data = Pit2024)
# anova(lm_ratio2024)
# 
# Pit2025<- PitUrch %>%
#   filter(Year %in% c("2025"))
# 
# ratio2025 <- ggplot(Pit2025, aes(x = RatioPits, y = Total.Canopy)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
#   labs(title = 'Empty:Present Pit Ratio vs Kelp Canopy 2025',
#        x = 'Empty:Present Pit Ratio', y = 'Total Canopy (%)')
# 
# ggsave(filename = "Figures/EmptyPits_vs_KelpCanopy2025.png", 
#        plot = ratio2025 , width = 8, height = 6, dpi = 300)
# 
# lm_ratio2025 <- lm(Total.Canopy ~ RatioPits, data = Pit2025)
# anova(lm_ratio2025)
# 
# ########checking more ######################################################
# # Histogram of Total.Canopy for each year
# check1 <- ggplot(Pit2024, aes(x = Total.Canopy)) + 
#   geom_histogram(binwidth = 5, fill = "skyblue") + 
#   labs(title = "2024 Canopy Distribution")
# 
# ggsave(filename = "Figures/Check1.png", 
#        plot = check1 , width = 8, height = 6, dpi = 300)
# 
# check2 <-ggplot(Pit2025, aes(x = Total.Canopy)) + 
#   geom_histogram(binwidth = 5, fill = "salmon") + 
#   labs(title = "2025 Canopy Distribution")
# 
# ggsave(filename = "Figures/Check2.png", 
#        plot = check2 , width = 8, height = 6, dpi = 300)
# 
# # Density plot for RatioPits
# check3 <-ggplot(Pit2024, aes(x = RatioPits)) + 
#   geom_density(fill = "lightgreen") + 
#   labs(title = "2024 RatioPits Distribution")
# 
# ggsave(filename = "Figures/Check3.png", 
#        plot = check3 , width = 8, height = 6, dpi = 300)
# 
# check4 <-ggplot(Pit2025, aes(x = RatioPits)) + 
#   geom_density(fill = "orange") + 
#   labs(title = "2025 RatioPits Distribution")
# 
# ggsave(filename = "Figures/Check4.png", 
#        plot = check4 , width = 8, height = 6, dpi = 300)
# 
# ################################################################################
# #try another type of test#######################################################
# 
# glm_model <- glm(Total.Canopy + 0.01 ~ RatioPits, data =
#                    Pit2024_clean, family = gaussian(link = "log"))
# summary(glm_model)
# 
# Pit2024_clean$CanopyAdj <- Pit2024_clean$Total.Canopy + 0.01
# ggplot(Pit2024_clean, aes(x = RatioPits, y = CanopyAdj)) +
#   geom_point() +
#   stat_smooth(method = "glm", method.args = list(family = gaussian(link = "log"))
#               , se = TRUE, color = "blue") +
#   labs(title = "GLM Fit: Empty:Present Pit Ratio vs Kelp Canopy",
#        x = "Empty:Present Pit Ratio", y = "Adjusted Canopy (%)") +
#   theme_minimal()
# 
# 
# Pit2024_clean <- Pit2024 %>% filter(is.finite(RatioPits) & is.finite(Total.Canopy))
# 
# # combine plots together ####################################################
# library(patchwork)
# plot1 <- ggplot(OnlyUrch, aes(x = PittedUrchins, y = Total.Canopy)) +
#   geom_point() + stat_smooth(method = 'lm', se = FALSE) +
#   labs(title = 'Pitted Urchins vs Kelp Canopy')
# 
# plot2 <- ggplot(OnlyUrch, aes(x = NonPit, y = Total.Canopy)) +
#   geom_point() + stat_smooth(method = 'lm', se = FALSE) +
#   labs(title = 'Nonpitted Urchins vs Kelp Canopy')
# 
# plot1 + plot2

# ##############################################################################
# Pit to NonPit Ratio Testing
# #############################################################################


kruskal.test(PercentNonPitted ~ Connectivity, data = OnlyUrch)


# color by Connectivity 
plot1 <- ggplot(OnlyUrch, aes(x = Connectivity, y = PercentNonPitted, fill = 
                                    Connectivity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Susceptibility to Urchin Migration",
    y = "Percent Urchins Non-Pitted"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# color by Site
plot2 <- ggplot(OnlyUrch, aes(x = Connectivity, y = PercentNonPitted, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    x = "Susceptibility to Urchin Migration",
    y = "Percent Urchins Non-Pitted",
    fill = "Site Code"  # Label for the legend
  ) +
  theme_minimal()

# by year

plot3 <- ggplot(OnlyUrch, aes(x = Connectivity, y = PercentNonPitted, fill = SiteCode)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(
    title = "Percent of Non-Pitted Urchins by Connectivity",
    x = "Connectivity to Urchin Barrens",
    y = "Percent Non-Pitted Urchins",
    fill = "Site Code"
  ) +
  facet_wrap(~ Year) +
  theme_minimal()

ggsave(filename="Figures/PercentNonPitMigration.png",
       plot= plot2, width =8, height=6, dpi=300)
ggsave(filename="Figures/PercentNonPitMigration2.png",
       plot= plot1, width =8, height=6, dpi=300)
ggsave(filename="Figures/PercentNonPitMigration3.png",
       plot= plot3, width =8, height=6, dpi=300)


