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

#####
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


#####

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

### with added y= and R2 values

lm_modelNonPit <- lm(Total.Canopy ~ NonPit, data = OnlyUrch)

# Extract equation and R² for labeling
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


