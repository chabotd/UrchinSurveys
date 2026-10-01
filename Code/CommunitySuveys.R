library(tidyverse)
library(dplyr)
library(lubridate)
library(tweedie)
library(statmod)
library(ggridges)
library(kableExtra)
################################################################################
#Read in Surveys dataset
################################################################################
com <- read.csv("Data/CommunitySurveys/ComSurveys2020.csv")
com2025 <- read.csv("Data/CommunitySurveys/ComSurvey2025.csv")
com2026 <- read.csv("Data/CommunitySurveys/ComSurvey2026.csv")
com2024 <- read.csv("Data/CommunitySurveys/ComSurvey2024.csv")
com2023 <-read.csv("Data/CommunitySurveys/ComSurvey2023.csv")
com2022 <- read.csv("Data/CommunitySurveys/ComSurvey2022.csv")


# ##########Cleaning Workflows###################################################

 #check to see things are numeric or characters.
str(com)

# #make sure all the columns are the same
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

com_all <- read.csv("Data/CommunitySurvey_Data_Master_2006-2026_2026-09-13DJC.csv")
# Extract first row as new column names
 new_names <- as.character(com_all[1, ])
#
# # Assign them as column names
names(com_all) <- new_names

# Remove the first row
com_all <- com_all[-1, ]

# rename all blanks w zeros

com_all[com_all == ""] <- 0



df  <- com_all %>% select(
  Year,
  `Strongylocentrotus purpuratus`,
  `Total_Canopy_Forming`,
  QuadID,
  SiteCode,
  Zone,
  Exposure
)

df <- df %>%
  filter(Zone %in% c("L"))

df <- df %>%
   filter(!Exposure %in% c("P"))

 df <- df %>%
   filter(SiteCode %in% c("FC", "BB", "CB", "CBN", "RP", "YB", "SH", "CMEN", "CMES"))
 
 df <- df %>%
   filter(!Year =="2007")

 df <- df %>%
   rename(UrchinCount = `Strongylocentrotus purpuratus`)

 df$UrchinCount <- as.numeric(df$UrchinCount)
 df$Year <- as.numeric(df$Year)

 df$logUrchins <- log(df$UrchinCount + 1)

 df <- df %>%
   mutate(YearGroup = case_when(
     Year >= 2006 & Year <= 2011 ~ "2006–2011",
     Year >= 2012 & Year <= 2016 ~ "2012–2016",
     Year >= 2017 & Year <= 2021 ~ "2017–2021",
     Year >= 2022 & Year <= 2026 ~ "2022–2026",
     TRUE ~ NA_character_
   ))
 
 df <- df %>%
   mutate(
     YearGroupShort = gsub("^20([0-9]{2})–20([0-9]{2})$", "\\1–\\2", YearGroup)
   )
 

 write.csv(df, file= "Data/UrchComData.csv", row.names = FALSE)
###############################################################################
 # MAIN FIGS
 ##############################################################################
 df <- read.csv("Data/UrchComData.csv")
 
 df <- df %>%
   mutate(SiteCode = case_when(
     SiteCode %in% c("CB", "CBN") ~ "CB",
     TRUE ~ SiteCode
   ))
 
 df <- df %>%
   mutate(
     SiteCode = recode(
       SiteCode,
       "CMEN" = "CMN",
       "CMES" = "CMS")
   )
 
 # Reorder the levels of sites
 df$SiteCode <- factor(df$SiteCode, levels = c("FC", "BB", "YB", "SH", "CB","RP", "CMN", "CMS"))
 
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


###############################################################################
# Mean urchin count by 5 year group and site
###############################################################################
 plot1dat <- df %>%
   group_by(SiteCode, YearGroupShort) %>%
   summarise(
     mean_urch = mean(UrchinCount, na.rm = TRUE),
     sd_urch   = sd(UrchinCount, na.rm = TRUE),
     n           = sum(!is.na(UrchinCount)),
     se_urch   = sd_urch / sqrt(n),
     .groups = "drop"
   )
 
plot <- plot1dat %>%
  group_by(SiteCode, YearGroupShort) %>%
  ggplot(aes(x = YearGroupShort, y = mean_urch)) +
  geom_col(aes(fill = SiteCode), alpha = 0.7)+
  geom_errorbar(
    aes(ymin = mean_urch - se_urch,
        ymax = mean_urch + se_urch
    ),  
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ SiteCode) +
  scale_fill_manual(values = site_cols) +
  labs(
    x = "5-Year Group",
    y = "Mean Urchin Density per 0.25m²"
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/SitewideTotals.png", 
       plot = plot , width = 8, height = 6, dpi = 300)

# and for kelp 

plot2dat <- df %>%
  group_by(SiteCode, YearGroupShort) %>%
  summarise(
    mean_canopy = mean(Total_Canopy_Forming, na.rm = TRUE),
    sd_canopy   = sd(Total_Canopy_Forming, na.rm = TRUE),
    n           = sum(!is.na(Total_Canopy_Forming)),
    se_canopy   = sd_canopy / sqrt(n),
    .groups = "drop"
  )

plot2 <- plot2dat %>%
  group_by(SiteCode, YearGroupShort) %>%
  ggplot(aes(x = YearGroupShort, y = mean_canopy)) +
  geom_col(aes(fill = SiteCode), alpha = 0.7)+
  geom_errorbar(
    aes(ymin = mean_canopy - se_canopy,
        ymax = mean_canopy + se_canopy
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ SiteCode) +
  scale_fill_manual(values = site_cols) +
  labs(
    x = "5-Year Group",
    y = "Mean Canopy Forming Kelp Percent Cover per 0.25m²"
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/SitewideKelp.png", 
       plot = plot2 , width = 8, height = 6, dpi = 300)


#################################################### 
# mean kelp canopy 2024, 2025, 2026
#################################################### 
df %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Total_Canopy_Forming, na.rm = TRUE),
    SE   = sd(Total_Canopy_Forming, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )

ob <- df %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(UrchinCount, na.rm = TRUE),
    SE   = sd(UrchinCount, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )

df %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Total_Canopy_Forming, na.rm = TRUE),
    SE   = sd(Total_Canopy_Forming, na.rm = TRUE) / sqrt(n()),
  ) %>%
  mutate(
    Mean = round(Mean, 2),
    SE = round(SE, 2)
  ) %>%
  kable(
    format = "html",
    col.names = c("Site", "Mean Percent Cover Canopy Forming Kelp per 0.25m²", "SE"),
    align = c("l", "r", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE)

mean_urch <- df %>%
  group_by(YearGroup) %>%
  summarise(MeanUrchins = mean(UrchinCount, na.rm = TRUE)) %>%
  left_join(df_leters, by = "YearGroup") %>%
  ggplot(aes(x = YearGroup, y = MeanUrchins)) +
  geom_col(fill = "mediumorchid4", alpha = 0.7) +
  geom_text(aes(label = round(MeanUrchins, 1)), vjust = -0.5) +
  geom_text(
    aes(
      y = MeanUrchins + 2,   # place letters slightly above the bar
      label = Letters
    ),
    size = 6
  ) +
  labs(
    x = "5-Year Group",
    y = "Mean Urchin Density per 0.25m²"
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/allsites.png", 
       plot = mean_urch , width = 8, height = 6, dpi = 300)

# quick anova 
urch_aov <- aov(UrchinCount ~ YearGroup, data = df)
summary(urch_aov)

urchtuk <- TukeyHSD(urch_aov)

# Extract p-values for Year Group comparisons
urch_p <-urchtuk$YearGroup[, "p adj"]

# Convert to compact letter display
letters <- multcompLetters(urch_p)
letters$Letters

df_leters <- data.frame(
  YearGroup = names(letters$Letters),
  Letters = letters$Letters
)


mean_kelp_yeargr <- df %>%
  group_by(YearGroup) %>%
  summarise(MeanKelp = mean(Total_Canopy_Forming, na.rm = TRUE)) %>%
  left_join(df_leters, by = "YearGroup") %>%
  ggplot(aes(x = YearGroup, y = MeanKelp)) +
  geom_col(fill = "olivedrab4", alpha = 0.7) +
  geom_text(aes(label = round(MeanKelp, 1)), vjust = -0.5) +
  geom_text(
    aes(
      y = MeanKelp + 4,   # place letters slightly above the bar
      label = Letters
    ),
    size = 6
  ) +
  labs(
    x = "5 Year Group",
    y = "Mean Kelp Canopy Percent Cover per 0.25m²"
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/allsites_year_kelp.png", 
       plot = mean_kelp_yeargr , width = 8, height = 6, dpi = 300)

# quick anova 
kelp_aov <- aov(Total_Canopy_Forming ~ YearGroup, data = df)
summary(kelp_aov)

kelptuk <- TukeyHSD(kelp_aov)

# Extract p-values for Year Group comparisons
kelp_p <-kelptuk$YearGroup[, "p adj"]

# Convert to compact letter display
letters <- multcompLetters(kelp_p)
letters$Letters

df_leters <- data.frame(
  YearGroup = names(letters$Letters),
  Letters = letters$Letters
)

############ KELP BY YEAR

mean_kelp <- df %>%
  group_by(Year) %>%
  summarise(MeanKelp = mean(Total_Canopy_Forming, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = MeanKelp)) +
  geom_col(fill = "olivedrab4", alpha = 0.7) +
  geom_hline(yintercept = 32, linetype = "dotted", color = "black", size = 1) +
  geom_text(aes(label = round(MeanKelp, 1)), vjust = -0.5) +
  labs(
    x = "Year",
    y = "Mean Kelp Canopy Percent Cover per 0.25m²"
  ) +
  theme_minimal()

ggsave(filename = "Figures/CommunitySurveys/allsites_year_k.png", 
       plot = mean_kelp , width = 8, height = 6, dpi = 300)

####### mean for all years
df %>%
  summarise(
    Mean = mean(Total_Canopy_Forming, na.rm = TRUE),
    SE   = sd(Total_Canopy_Forming, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )

df %>%
  summarise(
    Median = median(Total_Canopy_Forming, na.rm = TRUE),
    SE   = sd(Total_Canopy_Forming, na.rm = TRUE) / sqrt(n()),
)

