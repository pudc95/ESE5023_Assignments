library(tidyr)
library(dplyr)
library(ggplot2)
setwd('G:/Sustech/Work/CPER/Assignment/3')
VZ <- read.csv("PS3-3.csv",header = T)
VZ <- as_tibble(VZ)
head(VZ)

anova_one_way <- aov(Zinc~factor(women), data = VZ)
summary(anova_one_way)

ggplot(VZ, aes(x = women, y = Zinc, fill = women)) +
  geom_boxplot() +
  theme_classic()
nonvegetarians <- VZ %>% 
  gather(women, Zinc) %>% 
  filter(women == "Pregnant_nonvegetarians") %>% 
  pull(Zinc)
vegetarians <- VZ %>% 
  gather(women,  Zinc) %>% 
  filter(women == "Pregnant_vegetarians") %>% 
  pull( Zinc)
Nonpvegetarians <- VZ %>% 
  gather(women,  Zinc) %>% 
  filter(women == "Nonpregnant_vegetarians") %>% 
  pull( Zinc)
t.test(nonvegetarians,vegetarians)
t.test(Nonpvegetarians,vegetarians)