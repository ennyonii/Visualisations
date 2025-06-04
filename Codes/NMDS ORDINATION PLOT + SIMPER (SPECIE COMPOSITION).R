species_data <- prey[, 1:64]
vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
simper_result <- simper(species_data, vec)
summary(simper_result)
# Extract SIMPER result between Forest and Caatinga
res <- simper_result[["CAATINGA_ATLANTIC"]] 
res_df <- as.data.frame(res)
View(res_df)
# Add species names as a column
res_df$species <- rownames(res_df)
top_species <- res_df[order(-res_df$average), ][1:10, ]

nmds <- metaMDS(species_data, distance = "bray", k = 2, trymax = 100)

group <- factor(vec)  
site_scores <- as.data.frame(scores(nmds, display = "sites"))
site_scores$group <- group
species_fit <- envfit(nmds, species_data, permutations = 999)
species_vectors <- as.data.frame(species_fit$vectors$arrows)

species_vectors$pvals <- species_fit$vectors$pvals
top_simper <- res_df[order(-res_df$average), ][1:10, ]
top_species <- rownames(top_simper)

top_species_vectors <- species_vectors %>%
  filter(rownames(species_vectors) %in% top_species) %>%
  mutate(species = rownames(.))
ggplot() +
  # NMDS points
  geom_point(data = site_scores, 
             aes(x = NMDS1, y = NMDS2, color = group), size = 3) +
  
  # Species arrows
  geom_segment(data = top_species_vectors, 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
  
  # Species labels
  geom_text(data = top_species_vectors, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            hjust = 0.5, vjust = -0.8, color = "darkgreen", size = 3) +
  
  # Theme
  theme_minimal() +
  labs(title = "Top Species responsible for dissimilarity in the 2 biomes",
       x = "NMDS1", y = "NMDS2", color = "Biome") +
  theme(legend.position = "right")





