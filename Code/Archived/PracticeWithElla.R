library(tidyverse)
library(dplyr)
library(vegan)
library(multcompView)
library(ggplot2)


################################################################################
urch <- read.csv("SURVEY_Urchin_Survey_Data_2025.csv")

# Reorder the levels of sites 
urch$SiteCode <- factor(urch$SiteCode, levels = c("BB", "FC", "YB", "SH", "SB", 
                                                  "SC", "CB","RP", "WC", "CP", 
                                                  "CMN", "CMS"))
# add column for Empty:Present Pits
urch <- urch %>% mutate(RatioPits = EmptyPits/PitsPresent)

#add column for mean size

urch <- urch %>% mutate(MeanTest = rowMeans(urch[, c("UrchinSize1", 
                                                     "UrchinSize2", 
                                                     "UrchinSize3", 
                                                     "UrchinSize4", 
                                                     "UrchinSize5")]))

# don't look at AZ-- urchin-dominated zone only. 
OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))
# add nonpits together

OnlyUrch <- OnlyUrch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)
################################################################################
#Size diffs 
################################################################################
# Between sub-habitats 

OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

t.test(MeanTest ~ Zone, data = OnlyUrch)
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

#box plots
################################################################################
# total urchins in urchin-dominated plots 
t.test(TotalUrchins ~ factor(Year), data = OnlyUrch)
ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins)) +
  geom_boxplot(fill = "lavender") +
  labs(title = "Urchin Count Year Differences",
       x = "Year",
       y = "Total Urchins") +
  theme_minimal()

# algal canopy in urchin-dominated plots

t.test(Total.Canopy ~ factor(Year), data = OnlyUrch)
ggplot(OnlyUrch, aes(x = factor(Year), y = Total.Canopy)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Canopy Year Differences",
       x = "Year",
       y = "Total Canopy") +
  theme_minimal()

################################################################################

# linear regression canopy and urchins 
################################################################################
#for all urchins
ggplot(OnlyUrch, aes(x=TotalUrchins, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Total Urchins to Total Canopy',
       x='Urchin Count', y='Total Canopy (percent)')

# Fit the linear model
model <- lm(Total.Canopy ~ TotalUrchins, data = OnlyUrch)

# Extract key stats
slope <- round(coef(model)[2], 3)
intercept <- round(coef(model)[1], 3)
r2 <- round(summary(model)$r.squared, 3)
pval <- signif(summary(model)$coefficients[2, 4], 3)

# Build math expression safely
eq_label <- bquote(
  italic(y) == .(intercept) + .(slope) %.% italic(x) ~ "," ~
    R^2 == .(r2) ~ "," ~ italic(p) == .(pval)
)

# Plot with regression line and equation annotation
library(ggplot2)

ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_smooth(method = 'lm', se = FALSE, color = "blue", linewidth = 1) +
  labs(
    title = 'Total Urchins to Total Canopy',
    x = 'Urchin Count',
    y = 'Total Canopy (percent)'
  ) +
  annotate(
    "text",
    x = max(OnlyUrch$TotalUrchins) * 0.28,
    y = max(OnlyUrch$Total.Canopy) * 0.95,
    label = as.character(as.expression(eq_label)),
    parse = TRUE,
    hjust = 0,
    size = 5
  ) +
  theme_minimal(base_size = 14)

#another lm?

lm_model <- lm(Total.Canopy ~ TotalUrchins, data = OnlyUrch)

plot(lm_model)#inspect the resids vs fits to check model- mostly oK
summary(lm_model)
anova(lm_model)

# by cape
ggplot(OnlyUrch, aes(x=TotalUrchins, y=Total.Canopy, color = Cape)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Total Urchins to Total Canopy by Cape',
       x='Urchin Count', y='Total Canopy (percent)')

# for pitted urchins

ggplot(OnlyUrch, aes(x=PittedUrchins, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Pitted Urchins to Total Canopy',
       x='Pitted Urchin Count', y='Total Canopy (percent)')

lm_modelPit <- lm(Total.Canopy ~ PittedUrchins, data = OnlyUrch)
anova(lm_modelPit)

#for non pitted urchins
OnlyUrch$NonPit <- OnlyUrch$TotalUrchins - OnlyUrch$PittedUrchins


ggplot(OnlyUrch, aes(x=NonPit, y=Total.Canopy)) +
  geom_point() + stat_smooth(method = 'lm', se=FALSE) + 
  labs(title='Nonpitted Urchins to Total Canopy',
       x='Nonpitted Urchin Count', y='Total Canopy (percent)')

lm_modelNonPit <- lm(Total.Canopy ~ NonPit, data = OnlyUrch)
anova(lm_modelNonPit)

# with added y= and R2 values

lm_modelNonPit <- lm(Total.Canopy ~ NonPit, data = OnlyUrch)

# Extract equation and R2 for labeling
eq <- substitute(
  italic(y) == a + b %.% italic(x) * "," ~~ italic(R)^2 ~ "=" ~ r2,
  list(
    a = format(coef(lm_modelNonPit)[1], digits = 2),
    b = format(coef(lm_modelNonPit)[2], digits = 2),
    r2 = format(summary(lm_modelNonPit)$r.squared, digits = 3)
  )
)

ggplot(OnlyUrch, aes(x = NonPit, y = Total.Canopy)) +
  geom_point(alpha = 0.7) +
  stat_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Nonpitted Urchins vs Total Canopy",
    x = "Nonpitted Urchin Count",
    y = "Total Canopy (%)"
  ) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = as.character(as.expression(eq)),
           parse = TRUE, 
           hjust = 1.1, vjust = 1.5, 
           size = 4)


##########################
#canopy cover by zone - filter into 2 datasets
OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

OnlyAlgal <- urch %>%
  filter(Zone == "AZ")

#plot for algal zone only
ggplot(OnlyAlgal, aes(x = TotalUrchins, y = Total.Canopy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Urchin vs. Algal Canopy (Algal Zone)",
       x = "Urchin Count",
       y = "Total Canopy (%)") +
  theme_minimal()

#plot for UPZ and NPZ together
ggplot(OnlyUrch, aes(x = TotalUrchins, y = Total.Canopy)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Urchin vs. Algal Canopy (Urchin Zones)",
       x = "Urchin Count",
       y = "Total Canopy (%)",
       color = "green") +
  theme_minimal()





# Oct 23 - turning box plots into bar graphs for thesis
###############################################################
library(tidyverse)
library(dplyr)
library(vegan)
library(mosaic)
library(multcompView)
library(ggplot2)

#data set
urch <- read.csv("/Users/elladreke/Git/UrchinSurveys/Data/SURVEY_Urchin_Survey_Data_2025.csv"


                 #1 = total canopy          
### OLD BOXPLOT CODE - referenced for the barplot
  #boxplot(Total.Canopy~Zone, data=urch, col='#3d6600', 
       # main = "Average Percent Cover Canopy Kelp",
       # xlab = "Zone", ylab = "Percent Cover Canopy Kelp")

#put zones in the right order/section 
urch$Zone <- factor(urch$Zone, levels = c("UPZ", "NPZ", "AZ"))

#calc mean and SE by zone for error bars
urch_summary <- urch %>%
  group_by(Zone) %>%
  summarise(
    mean_canopy = mean(Total.Canopy, na.rm = TRUE),
    se_canopy = sd(Total.Canopy, na.rm = TRUE) / sqrt(n()))

#bar graph with error bars
ggplot(urch_summary, aes(x = Zone, y = mean_canopy, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_canopy - se_canopy,
                    ymax = mean_canopy + se_canopy),
                width = 0.2) +
  scale_fill_manual(values = c("#6E8B3D", "#A0C878", "#3D6600")) +
  labs(title = "Mean Percent Total Kelp Canopy Cover by Zone",
       x = "Urchin Subhabitat",
       y = "Mean Percent Kelp Canopy Cover ± SE") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


##significance stats
canopy_aov <- aov(Total.Canopy ~ Zone, data = urch)
summary(canopy_aov)

TukeyHSD(canopy_aov)



#2 = 2024 vs 2025 urchins
#redoing t-test on avg urchin ct per plot 2024 vs 2025 into a wilcox rs and bar graph

# remove AZ to look at urchin-dominated zones only
OnlyUrch <- urch %>%
  filter(Zone %in% c("UPZ", "NPZ"))

#wilcox rank sum on total urchins in urchin-dominated plots by year:
wilcox_test <- wilcox.test(TotalUrchins ~ factor(Year), data = OnlyUrch, exact = FALSE)
wilcox_test

# add nonpit data together (open + crevice)
OnlyUrch <- OnlyUrch %>% mutate(NonPit = OpenUrchins + CreviceUrchins)

PitUrch <- urch %>%
  filter(Zone %in% c("UPZ"))

urch2024<- urch %>%
  filter(Year %in% c("2024"))

PitUrch2024 <- urch2024 %>%
  filter(Zone %in% c("UPZ"))

urch2025<- urch %>%
  filter(Year %in% c("2025"))

PitUrch2025 <- urch2025 %>%
  filter(Zone %in% c("UPZ"))

#YearUrch <- TotalUrchins %>%
  #filter(Year %in% c("2024", "2025"))


#calc SE for (works for both years)
urch_summary <- OnlyUrch %>%
  group_by(Year) %>%
  summarise(
    mean_urchins = mean(TotalUrchins, na.rm = TRUE),
    se_urchins = sd(TotalUrchins, na.rm = TRUE) / sqrt(n())
  )


#factor data by year
urch_summary$Year <- factor(urch_summary$Year, levels = c("2024", "2025"))


#bar graph w error bars
ggplot(urch_summary, aes(x = Year, y = mean_urchins, fill = Year)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_urchins - se_urchins,
                    ymax = mean_urchins + se_urchins),
                width = 0.2) +
  scale_fill_manual(values = c("#b266b2", "#800080")) +
  labs(title = "Mean Urchin Count per Quadrat by Year",
       x = "Year",
       y = "Mean Urchin Count per plot ± SE") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")



#significance stats
UrchCt_aov <- aov(TotalUrchins ~ Year, data = urch)
summary(UrchCt_aov)

TukeyHSD(UrchCt_aov)


    ## OLD BOXPLOT CODE
#ggplot(OnlyUrch, aes(x = factor(Year), y = TotalUrchins)) +
#geom_boxplot(fill = "#800080") +
#  labs(title = "Urchin Count Year Differences",
  #     x = "Year",
  #     y = "Total Urchins") +
#  theme_minimal()


#OCT 23 - making a stacked bar graph to show dominant kelp composition by site
#################
library(dplyr)
library(tidyr)
library(ggplot2)

urch <- read.csv("/Users/elladreke/Git/UrchinSurveys/Data/SURVEY_Urchin_Survey_Data_2025.csv")
algae_data <- read.delim("/Users/elladreke/Git/UrchinSurveys/Data/SURVEY_Urchin_Survey_Data_2025.csv", header = TRUE)

# 1️⃣ Identify canopy-forming algal species columns automatically (cleaned)
species_start <- which(names(urch) == "Urchin.cover") + 1
species_end   <- which(names(urch) == "Total.Canopy") - 1

species_cols <- names(urch)[species_start:species_end]
species_cols <- species_cols[!is.na(species_cols)]  # remove any NA values

# Double-check the result
print(species_cols)

# 2️⃣ Pivot data longer — create tidy dataset for plotting
urch_long <- urch %>%
  select(SiteCode, all_of(species_cols)) %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "Species",
    values_to = "Cover"
  )

# 3️⃣ Calculate average percent cover by site and species
urch_summary <- urch_long %>%
  group_by(SiteCode, Species) %>%
  summarise(mean_cover = mean(Cover, na.rm = TRUE), .groups = "drop")

# 4️⃣ Stacked bar graph showing dominant algal species by site
ggplot(urch_summary, aes(x = SiteCode, y = mean_cover, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Dominant Algal Species by Site",
    x = "Site",
    y = "Mean Percent Cover"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )




