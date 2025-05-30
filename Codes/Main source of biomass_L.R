ggplot(latest_biomass_dataa, aes(x = as.factor(Season), y = log(ratio), fill = Main_source_by_season)) +
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ BIOME) +
  labs(
    title = "Main source of biomass by season",
    x = "Season", y = "Log(Biomass Ratio)",
    fill = "Main_source_by_season"
  ) 