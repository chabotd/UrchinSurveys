transect <- read.csv("Data/Reefcheck_Transects_2024.csv")

urchins <- transect %>%
  filter(Species =="Strongylocentrotus purpuratus")

# remove culled treatments

urchins <- urchins %>%
  filter(!CullingTreatment=="Culled")


# define corresponding intertidal sites 

#simpson reef = cape arago sites
#redfsh rocks = rocky point




anova <- aov(Density_m2~ Site, data = urchins)
summary(anova)

tuk <- TukeyHSD(anova)

plot <- ggplot(urchins, aes(x = Site, y = Density_m2, fill = Site)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  #geom_text(data = NPZcld_df,
  #          aes(x = SiteCode, y = max(NPZ$OpenUrchins, na.rm = TRUE) + 2,
   #             label = Letters),
  #          size = 6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  geom_hline(yintercept = 9.2, linetype = "dotted", color = "black", linewidth = 1)+
  labs(
    x = "Site",
    y = "Urchin Density (count per m²)"
  ) +
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

urchins %>%
  group_by(Site) %>%
  summarise(
    Mean = mean(Density_m2, na.rm = TRUE),
    SE   = sd(Density_m2, na.rm = TRUE) / sqrt(n()),
    CI95_low  = Mean - 1.96 * SE,
    CI95_high = Mean + 1.96 * SE
  )
