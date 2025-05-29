library(readxl)
data_bp <- read_excel("data_bp.xlsx")
View(data_bp)
Biome <-  data_bp$Biome
Levins_index <- data_bp$Levins_index
Season <- data_bp$Season
Rainfall <- data_bp$Rainfall
library(ggplot2)
View(data_bp)

getwd()

#STAT ANALYSIS FOR THE NICHE WIDTH DEPENDENCE ON SEASON AND BIOME QUESTION
anova(lm(Levins_index~Biome)) #DEPENDENT VARIABLE *INDEPENDENT VARIABLE

anova(lm(Levins_index~Biome*Season))

anova(lm(Levins_index ~ Rainfall + Biome))

#Boxplot FOR THE ANOVA(NB~SB)
ggplot(data_bp, aes(x = Biome, y = Levins_index, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    y = "Levins_Index",
    x = "Biome",
    title = "Effect of Biome and Season on Niche Width"
  ) +
  theme_minimal()
