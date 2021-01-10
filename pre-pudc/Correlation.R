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

#1.1---------------------------#
plotname	       <- paste("Population.png",sep="")
png(plotname, width=8.5, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_boxplot(mapping = aes(x = F_CODE_DES, y = population, fill = F_CODE_DES)
               , show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=" ", y="Population Density [km^-2]") +
  theme(plot.title=element_text(hjust = 0.5,size=0), #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  ) +
  coord_flip()
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()

#1.2---------------------------#
plotname	       <- paste("DMSP.png",sep="")
png(plotname, width=8.5, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_boxplot(mapping = aes(x = F_CODE_DES, y = DMSP_2012, fill = F_CODE_DES)
               , show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=" ", y="DMSP Night Light") +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  ) +
  coord_flip()
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()

#1.3---------------------------#
plotname	       <- paste("TROP.png",sep="")
png(plotname, width=8.5, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_boxplot(mapping = aes(x = F_CODE_DES, y = TROP, fill = F_CODE_DES)
               , show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=" ", y="TROPOMI HCHO Column[ug m^-2]") +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  ) +
  coord_flip()
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()

#2.1---------------------------#
plotname	       <- paste("Popu-TROP.png",sep="")
png(plotname, width=7, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_bin2d(mapping = aes(x = population, y = b1_TROPOMI)#, fill = F_CODE_DES
               , show.legend=TRUE)+
  theme_bw()+
  labs(title=" ", x=PopL, y=TroL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  ) +
  coord_flip()
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()

#2.2---------------------------#
plotname	       <- paste("DMSP-TROP.png",sep="")
png(plotname, width=6.5, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_bin2d(mapping = aes(x = b1_DMSP_20, y = b1_TROPOMI)#, fill = F_CODE_DES
             , show.legend=TRUE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=TroL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  ) +
  coord_flip()
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()

#2.3---------------------------#
plotname	       <- paste("DMSP-POP.png",sep="")
png(plotname, width=6.5, height=5.5, units="in", res=500) 
ggplot(data = Asia) + 
  geom_bin2d(mapping = aes(x = b1_DMSP_20, y = population)#, fill = F_CODE_DES
             , show.legend=TRUE)+
  theme_bw()+
  labs(title=" ", x=DMSPL, y=PopL) +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=12, colour = "black"), 
        axis.text.y=element_text(size=12, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
  )
#ggsave("myplot1.png")£¬legend.title =£¬coord_flip()
dev.off()