library(dplyr)
library(MASS)
library(leaps)
library(tidyr)
library(ggplot2)
#7.1
A <- InsectSprays %>% 
  gather(count, spray) %>% 
  filter(spray == "A") %>% 
  pull(count)
B <- InsectSprays %>% 
  gather(count, spray) %>% 
  filter(spray == "B") %>% 
  pull(count)
boxplot(A,B)
boxplot(A)
t.test(A, B)

#7.2
ggplot(InsectSprays, aes(x = spray, y = count, fill = spray)) +
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(count~factor(spray), data = InsectSprays)
summary(anova_one_way)

#=====7.3=====
DMSP <- read.csv("DMSP.csv", header = T)
China_Light <- DMSP$Light
Year<- DMSP$Year
plot(Year, China_Light, xlab = 'Year', ylab = 'China_Light')
fit <- lm(China_Light~Year)
summary(fit)
abline(fit, lwd = 2, col = "red")