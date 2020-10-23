setwd("D:/ESE5023")
library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
China_Light0 <- read.csv(file = "DMSP.csv", header = T)
China_Light <- as_tibble(China_Light0)
China_Light %>%
  select(Year, Light) %>%
  ggplot(aes(x=Year, y=Light)) + 
  geom_line()