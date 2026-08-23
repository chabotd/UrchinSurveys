library(tidyverse)
library(dplyr)
library(lubridate)
library(tweedie)
library(statmod)
################################################################################
#Read in Surveys dataset
################################################################################
com <- read.csv("Data/ComSurveys2020.csv")
com2025 <- read.csv("Data/ComSurvey2025.csv")
com2026 <- read.csv("Data/ComSurvey2026.csv")
com2024 <- read.csv("Data/ComSurvey2024.csv")
com2023 <-read.csv("Data/ComSurvey2023.csv")
com2022 <- read.csv("Data/ComSurvey2022.csv")


##########Cleaning Workflows###################################################

#check to see things are numeric or characters. 
str(com)

#make sure all the columns are the same 
list(
  "2020" = names(com),
  "2022" = names(com2022),
  "2023" = names(com2023),
  "2024" = names(com2024),
  "2025" = names(com2025),
  "2026" = names(com2026)
)

#bring together new years
com_all <- bind_rows(com, com2022, com2023, com2024, com2025, com2026)
com_all<- com_all %>% 
  filter(if_any(everything(), ~ . != "" & !is.na(.)))


#now write as new .csv for export 
write.csv(com_all, file= "Data/CommunitySurvey_Data_Master_2006-2026_2026-08-19_DJC.csv", row.names = FALSE)

# below is urchin workflow

# Extract first row as new column names
new_names <- as.character(com_all[1, ])

# Assign them as column names
names(com_all) <- new_names

# Remove the first row
com_all <- com_all[-1, ]

# rename all blanks w zeros

com_all[com_all == ""] <- 0

df  <- com_all %>% select(
  Year,
  `Strongylocentrotus purpuratus`,
  QuadID,
  SiteCode,
  Zone,
  Exposure
)

df <- df %>%
  filter(Zone %in% c("L"))

df <- df %>%
  rename(UrchinCount = `Strongylocentrotus purpuratus`)

df$UrchinCount <- as.numeric(df$UrchinCount)
df$Year <- as.numeric(df$Year)


df$logUrchins <- log(df$UrchinCount + 1)

# playing with regression
# BB

BB_df<- df %>%
  filter(SiteCode %in% c("BB"))

lm_urch <- lm(logUrchins ~ Year, data = BB_df)
anova(lm_urch)

ggplot(BB_df, aes(x = Year, y = logUrchins)) +
    geom_point() +
    stat_smooth(method = 'lm', se = FALSE, color = 'orange3') +
    labs(title = 'Urchin Count BB by Year',
         x = 'Year', y = 'Log Urchins')

# CBN 
# look at CB + add

CB_df<- df %>%
  filter(SiteCode %in% c("CBN", "CB"))

lm_urch <- lm(logUrchins ~ Year, data = CB_df)
anova(lm_urch)

ggplot(CB_df, aes(x = Year, y = logUrchins)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Urchin Count CB by Year',
       x = 'Year', y = 'Log Urchins')

#FC 
FC_df<- df %>%
  filter(SiteCode %in% c("FC"))

lm_urch <- lm(logUrchins ~ Year, data = FC_df)
anova(lm_urch)

ggplot(FC_df, aes(x = Year, y = logUrchins)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'purple') +
  labs(title = 'Urchin Count FC by Year',
       x = 'Year', y = 'Log Urchins')

# RP

RP_df<- df %>%
  filter(SiteCode %in% c("RP"))

lm_urch <- lm(logUrchins ~ Year, data = RP_df)
glm(formula = logUrchins ~ Year, family = "poisson", data = RP_df)
anova(lm_urch)

ggplot(RP_df, aes(x = Year, y = logUrchins)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, color = 'red') +
  labs(title = 'Urchin Count RP by Year',
       x = 'Year', y = 'Log Urchins')

########################################### try Tweedie
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

# CB
CB_df<- df %>%
  filter(SiteCode %in% c("CBN", "CB"))

glm <- glm(UrchinCount ~ Year,
           data = CB_df,
           family = tweedie(var.power = 1.5, link.power = 0))
summary(glm)


CB_df$PredictedUrchins <- predict(glm, type = "response")


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


BB <- ggplot(BB_df, aes(x = Year, y = UrchinCount)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(y = PredictedUrchins), linewidth= 1, color='green3') +
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
