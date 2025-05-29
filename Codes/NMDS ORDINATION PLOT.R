#TO CALCULATE PERMANOVA FOR  HOW SEASON AND BIOME AFFECT PREY COMPOSITION
prey <- read.csv("prey.csv")
library(vegan)

prey_df <- as.data.frame(prey)
#TO Set the first column as row names and remove it from columns
rownames(prey_df) <- prey_df[[1]]
prey_df <- prey_df[, -1]  # Drop the 'Prey' column
View(prey_df)

prey<-t(prey_df)
write.csv(prey, "prey.csv")

prey_bray<-vegan::vegdist(prey,method="bray")
nmds<-metaMDS(prey_bray)

vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
colors <- c("blue", "red")  # adjust for your groups
group_colors <- colors[vec]  # assigns color to each point

#NMDS ORDINATION PLOT
plot(nmds$points, col = as.factor(vec), pch = 19)
text(nmds$points, labels = rownames(nmds$points), cex = 0.8, pos = 3)
length(nmds$points)

adonis2(prey~vec)

nrow(prey)
length(vec)