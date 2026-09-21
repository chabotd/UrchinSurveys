
urch <- read.csv("Data/urch_clean.csv")

#looping code for each site if wanted 

####### Try as a loop for each indiv. plot 

sites <- unique(NoPerpetua$SiteCode)

plots <- lapply(sites, function(s) {
  
  dat <- NoPerpetua %>% filter(SiteCode == s)
  
  p_cryptic <- ggplot(dat, aes(x = Cryptic, y = TotalCanopy)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = PredictedCanopyCryp), linewidth = 1) +
    labs(
      x = "Cryptic Urchin Density",
      y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
      color = "Site"
    ) +
    theme_minimal()
  
  p_noncryptic <- ggplot(dat, aes(x = OpenUrchins, y = TotalCanopy)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = PredictedCanopyOpen), linewidth = 1) +
    labs(
      x = "Noncryptic Urchin Density",
      y = "Percent Cover of Canopy-Forming Kelp per 0.25m²",
      color = "Site"
    ) +
    theme_minimal()
  
  plot_grid(p_cryptic, p_noncryptic, labels = c(sites_full_names[s])
            , ncol = 2)
})

for (i in seq_along(sites)) {
  ggsave(
    filename = paste0("Figures/Surveys/Q1/", sites[i], "_cryptic_noncryptic.png"),
    plot = plots[[i]],
    width = 10,
    height = 5
  )
}

