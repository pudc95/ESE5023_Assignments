library(tidyr)
library(dplyr)
library(ggplot2)
setwd('G:/Sustech/Work/CPER/Assignment/3')

Tyrannosaurus <- read.csv("PS3-2.csv",header = T)
Tyrannosaurus <- as_tibble(Tyrannosaurus)
head(Tyrannosaurus)

ggplot(Tyrannosaurus, aes(x = Bone, y = Oxygen, fill = Bone)) +
  geom_boxplot() +
  theme_classic()

anova_one_way <- aov(Oxygen~factor(Bone), data = Tyrannosaurus)
summary(anova_one_way)
