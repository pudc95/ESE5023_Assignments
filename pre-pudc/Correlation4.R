library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
library(fields)
setwd("G:/Sustech/Work/CPER/Pre")

Asia <- read.csv(file = "ASIA1.csv", header = T)
Asia1 <- as_tibble(Asia)
Asia <- Asia1 %>%
  mutate(TROP = as.numeric(TROPOMI_HC))

TopL <- expression(paste("HCHO Column Density [",10^15, " molecules ",cm^-2,"]"))
PopL <- expression(paste("Population Density [",km^-2,"]"))
DMSPL <- expression(paste("DMSP Night Light"))

#6.1---------------------------#
plotname	       <- paste("DMSP(log)-TRO-point.png",sep="")
png(plotname, width=8, height=6, units="in", res=500)
Asia %>%
  group_by(F_CODE_DES) %>% #summarise(pop_mean=mean(population))
  ggplot(mapping = aes(x = log10(b1_DMSP_20), y = b1_TROPOMI, color=F_CODE_DES), show.legend=FALSE) + 
  #scale_x_log10() +
  geom_point(size=0.5, show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=TopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=14, colour = "black"),
        axis.title.y=element_text(size=14, colour = "black"),
  )+
  facet_wrap(~ F_CODE_DES)
dev.off()

#6.1---------------------------#
plotname	       <- paste("POP(log)-TRO-point.png",sep="")
png(plotname, width=8, height=6, units="in", res=500)
Asia %>%
  group_by(F_CODE_DES) %>% #summarise(pop_mean=mean(population))
  ggplot(mapping = aes(x = log10(population), y = b1_TROPOMI, color=F_CODE_DES), show.legend=FALSE) + 
  #scale_x_log10() +
  geom_point(size=0.5, show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=TopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=14, colour = "black"),
        axis.title.y=element_text(size=14, colour = "black"),
  )+
  facet_wrap(~ F_CODE_DES)
dev.off()