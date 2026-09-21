# Rock hardness 
#Author Delaney Chabot 

library(kableExtra)
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
