library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
setwd("G:/Sustech/Work/CPER/Pre")

Asia <- read.csv(file = "ASIA1.csv", header = T)
Asia1 <- as_tibble(Asia)
Asia <- Asia1 %>%
  mutate(TROP = as.numeric(TROPOMI_HC))

TroL <- expression(paste("HCHO Column Density [",10^15, " molecules ",cm^-2,"]"))
PopL <- expression(paste("Population Density [",km^-2,"]"))
DMSPL <- expression(paste("DMSP Night Light"))

#3.1---------------------------#
plotname	       <- paste("Popu-TROP-point.png",sep="")
png(plotname, width=8, height=6, units="in", res=500)
Asia %>%
  group_by(F_CODE_DES) %>% #summarise(pop_mean=mean(population))
  ggplot(mapping = aes(y = population, x = b1_TROPOMI, color=F_CODE_DES)) + 
  geom_point(size=0.5, show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", y=PopL, x=TopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  )+
  facet_wrap(~ F_CODE_DES)
dev.off()

#3.2---------------------------#
plotname	       <- paste("DMSP-TROP-point.png",sep="")
png(plotname, width=8, height=6, units="in", res=500)
Asia %>%
  group_by(F_CODE_DES) %>% #summarise(pop_mean=mean(population))
  ggplot(mapping = aes(x = b1_DMSP_20, y = b1_TROPOMI, color=F_CODE_DES), show.legend=FALSE) + 
  geom_point(size=0.5, show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=TopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  )+
  facet_wrap(~ F_CODE_DES)
dev.off()

#3.3---------------------------#
plotname	       <- paste("DMSP-Popu-point.png",sep="")
png(plotname, width=8, height=6, units="in", res=500)
Asia %>%
  group_by(F_CODE_DES) %>% #summarise(pop_mean=mean(population))
  ggplot(mapping = aes(x = b1_DMSP_20, y = population, color=F_CODE_DES), show.legend=FALSE) + 
  geom_point(size=0.5, show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=PopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  )+
  facet_wrap(~ F_CODE_DES)
dev.off()