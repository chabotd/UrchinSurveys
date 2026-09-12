library(tidyverse)
library(dplyr)
library(lubridate)
library(tweedie)
library(statmod)
################################################################################
#Read in Surveys dataset
################################################################################
# com <- read.csv("Data/CommunitySurveys/ComSurveys2020.csv")
# com2025 <- read.csv("Data/CommunitySurveys/ComSurvey2025.csv")
# com2026 <- read.csv("Data/CommunitySurveys/ComSurvey2026.csv")
# com2024 <- read.csv("Data/CommunitySurveys/ComSurvey2024.csv")
# com2023 <-read.csv("Data/CommunitySurveys/ComSurvey2023.csv")
# com2022 <- read.csv("Data/CommunitySurveys/ComSurvey2022.csv")
# 
# 
# ##########Cleaning Workflows###################################################
# 
# #check to see things are numeric or characters. 
# str(com)
# 
# #make sure all the columns are the same 
# list(
#   "2020" = names(com),
#   "2022" = names(com2022),
#   "2023" = names(com2023),
#   "2024" = names(com2024),
#   "2025" = names(com2025),
#   "2026" = names(com2026)
# )
# 
# #bring together new years
# com_all <- bind_rows(com, com2022, com2023, com2024, com2025, com2026)
# com_all<- com_all %>% 
#   filter(if_any(everything(), ~ . != "" & !is.na(.)))
# 
# 
# #now write as new .csv for export 
#write.csv(com_all, file= "Data/CommunitySurvey_Data_Master_2006-2026_2026-08-19_DJC.csv", row.names = FALSE)

# below is urchin workflow

# com_all <- read.csv("Data/CommunitySurvey_Data_Master_2006-2026_2026-08-19_DJC.csv")
# # Extract first row as new column names
# new_names <- as.character(com_all[1, ])
# 
# # Assign them as column names
# names(com_all) <- new_names
# 
# # Remove the first row
# com_all <- com_all[-1, ]
# 
# # rename all blanks w zeros
# 
# com_all[com_all == ""] <- 0
# 
# df  <- com_all %>% select(
#   Year,
#   `Strongylocentrotus purpuratus`,
#   QuadID,
#   SiteCode,
#   Zone,
#   Exposure
# )
# 
# df <- df %>%
#   filter(Zone %in% c("L"))
# 
# df <- df %>%
#   filter(!Exposure %in% c("P"))
# 
# df <- df %>%
#   filter(SiteCode %in% c("BB", "FC", "CB", "CBN", "RP", "YB", "SH"))
# 
# # Reorder the levels of sites 
# df$SiteCode <- factor(df$SiteCode, levels = c("BB", "FC", "YB", "SH", "CB","RP"))
# 
# df <- df %>%
#   mutate(SiteCode = case_when(
#     SiteCode %in% c("CB", "CBN") ~ "CB",
#     TRUE ~ SiteCode
#   ))
# 
# df <- df %>%
#   rename(UrchinCount = `Strongylocentrotus purpuratus`)
# 
# df$UrchinCount <- as.numeric(df$UrchinCount)
# df$Year <- as.numeric(df$Year)
# 
# 
# df$logUrchins <- log(df$UrchinCount + 1)
# 
# df <- df %>%
#   mutate(YearGroup = case_when(
#     Year >= 2006 & Year <= 2011 ~ "2006–2011",
#     Year >= 2012 & Year <= 2016 ~ "2012–2016",
#     Year >= 2017 & Year <= 2021 ~ "2017–2021",
#     Year >= 2022 & Year <= 2026 ~ "2022–2026",
#     TRUE ~ NA_character_
#   ))
# 
# write.csv(df, file= "Data/UrchComData.csv", row.names = FALSE)

# playing with regression
# BB

 df <- read.csv("Data/UrchComData.csv")
 
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
 
# 
# BB_df<- df %>%
#   filter(SiteCode %in% c("BB"))
# 
# lm_urch <- lm(logUrchins ~ Year, data = BB_df)
# anova(lm_urch)
# 
# ggplot(BB_df, aes(x = Year, y = logUrchins)) +
#     geom_point() +
#     stat_smooth(method = 'lm', se = FALSE, color = 'orange3') +
#     labs(title = 'Urchin Count BB by Year',
#          x = 'Year', y = 'Log Urchins')
# 
# # CBN 
# # look at CB + add
# 
# CB_df<- df %>%
#   filter(SiteCode %in% c("CB"))
# 
# lm_urch <- lm(logUrchins ~ Year, data = CB_df)
# anova(lm_urch)
# 
# ggplot(CB_df, aes(x = Year, y = logUrchins)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
#   labs(title = 'Urchin Count CB by Year',
#        x = 'Year', y = 'Log Urchins')
# 
# #FC 
# FC_df<- df %>%
#   filter(SiteCode %in% c("FC"))
# 
# lm_urch <- lm(logUrchins ~ Year, data = FC_df)
# anova(lm_urch)
# 
# ggplot(FC_df, aes(x = Year, y = logUrchins)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'purple') +
#   labs(title = 'Urchin Count FC by Year',
#        x = 'Year', y = 'Log Urchins')
# 
# # RP
# 
# RP_df<- df %>%
#   filter(SiteCode %in% c("RP"))
# 
# lm_urch <- lm(logUrchins ~ Year, data = RP_df)
# glm(formula = logUrchins ~ Year, family = "poisson", data = RP_df)
# anova(lm_urch)
# 
# ggplot(RP_df, aes(x = Year, y = logUrchins)) +
#   geom_point() +
#   stat_smooth(method = 'lm', se = FALSE, color = 'red') +
#   labs(title = 'Urchin Count RP by Year',
#        x = 'Year', y = 'Log Urchins')

########################################### 
# GLM: Tweedie
###########################################
RP_df<- df %>%
  filter(SiteCode %in% c("RP"))
glm <- glm(UrchinCount ~ Year,
                        data = RP_df,
                        family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


RP_df$PredictedUrchins <- predict(glm, type = "response")


RP <- ggplot(RP_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='red') +
  labs(title = 'Rocky Point',
    x = "Year",
    y = "Urchin Count",
  ) +
  theme_minimal()


ggsave(filename = "Figures/CommunitySurveys/RP.png", 
       plot = RP , width = 8, height = 6, dpi = 300)

# CB trying linear and quadratic models on each: 
#linear
 CB_df<- df %>%
   filter(SiteCode %in% c("CBN", "CB"))
# 
# glm <- glm(UrchinCount ~ Year,
#            data = CB_df,
#            family = tweedie(var.power = 1.5, link.power = 0))
# summary(glm)
# 
# CB_df$PredictedUrchins <- predict(glm, type = "response")

#quad
glm_quad <- glm(
  UrchinCount ~ Year + I(Year^2),
  data = CB_df,
  family = tweedie(var.power = 1.5, link.power = 0)
)

summary(glm_quad)
CB_df$PredictedUrchins <- predict(glm_quad, type = "response")

#test difs :
#anova(glm, glm_quad, test = "Chisq")


CB <- ggplot(CB_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='purple') +
  labs(title = 'Cape Blanco',
    x = "Year",
    y = "Urchin Count",
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/CB.png", 
       plot = CB , width = 8, height = 6, dpi = 300)

# BB
BB_df<- df %>%
  filter(SiteCode %in% c("BB"))

glm <- glm(UrchinCount ~ Year,
           data = BB_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


BB_df$PredictedUrchins <- predict(glm, type = "response")

#quad

glm_quad <- glm(UrchinCount ~ Year + I(Year^2),
           data = BB_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm_quad)

anova(glm, glm_quad, test = "Chisq")


BB_df$PredictedUrchins <- predict(glm, type = "response")

BB_df$PredictedUrchins <- predict(glm_quad, type = "response")


BB <- ggplot(BB_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='green4') +
  labs(title = 'Boiler Bay',
    x = "Year",
    y = "Urchin Count",
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/BB.png", 
       plot = BB , width = 8, height = 6, dpi = 300)

# FC
FC_df<- df %>%
  filter(SiteCode %in% c("FC"))

glm <- glm(UrchinCount ~ Year,
           data = FC_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


FC_df$PredictedUrchins <- predict(glm, type = "response")


FC <- ggplot(FC_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='blue3') +
  labs(title = 'Fogarty Creek',
    x = "Year",
    y = "Urchin Count",
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/FC.png", 
       plot = FC , width = 8, height = 6, dpi = 300)

FC_df %>%
  group_by(YearGroup) %>%
  summarise(MeanUrchins = mean(UrchinCount, na.rm = TRUE)) %>%
  ggplot(aes(x = YearGroup, y = MeanUrchins)) +
  geom_col(fill = "orange", alpha = 0.7) +
  geom_text(aes(label = round(MeanUrchins, 1)), vjust = -0.5) +
  labs(
    title = "Mean Urchin Count by 5-Year Group",
    x = "5-Year Group",
    y = "Mean Urchin Count"
  ) +
  theme_minimal()


# SH
SH_df<- df %>%
  filter(SiteCode %in% c("SH"))

glm <- glm(UrchinCount ~ Year,
           data = SH_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


SH_df$PredictedUrchins <- predict(glm, type = "response")


SH <- ggplot(SH_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='pink') +
  labs(title = 'Strawberry Hill',
       x = "Year",
       y = "Urchin Count",
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/SH.png", 
       plot = SH , width = 8, height = 6, dpi = 300)

# YB
YB_df<- df %>%
  filter(SiteCode %in% c("YB"))

glm <- glm(UrchinCount ~ Year,
           data = YB_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


YB_df$PredictedUrchins <- predict(glm, type = "response")


YB <- ggplot(YB_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='orange') +
  labs(title = 'Yachats Beach',
       x = "Year",
       y = "Urchin Count",
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/YB.png", 
       plot = YB , width = 8, height = 6, dpi = 300)


## Overall paterns by year

plot <- ggplot(df, aes(x = Year, y = UrchinCount, fill = SiteCode)) +
  geom_violin(alpha = 0.6) +
  facet_wrap(~ SiteCode) +
  labs(
    title = "Urchin Count Distribution by Site Across Years",
    x = "Year",
    y = "Urchin Count"
  ) +
  theme_minimal()

#try this 
library(ggridges)

ggplot(df, aes(x = UrchinCount, y = factor(Year), fill = SiteCode)) +
  geom_density_ridges(alpha = 0.7) +
  labs(
    title = "Urchin Count Density by Year and Site",
    x = "Urchin Count",
    y = "Year"
  ) +
  theme_minimal()

#################################################### try grouping into 5 year groups
# works well needs edit 6 Sept 26
plot <- df %>%
  group_by(SiteCode, YearGroup) %>%
  summarise(MeanUrchins = mean(UrchinCount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = YearGroup, y = MeanUrchins)) +
  geom_col(aes(fill = SiteCode), alpha = 0.7)+
 # geom_text(aes(label = round(MeanUrchins, 1)), vjust = -0.5) +
  facet_wrap(~ SiteCode) +
  labs(
    title = "Mean Urchin Count by 5-Year Group and Site",
    x = "5-Year Group",
    y = "Mean Urchin Density per "
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/SitewideTotals.png", 
       plot = plot , width = 8, height = 6, dpi = 300)




