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

TroL <- expression(paste("HCHO Column Density [",10^15, " molecules ",cm^-2,"]"))
PopL <- expression(paste("Population Density [",km^-2,"]"))
DMSPL <- expression(paste("DMSP Night Light"))

#4.1---------------------------#
plotname	       <- paste("Popu-histogram.png",sep="")
png(plotname, width=6, height=6, units="in", res=500)
Asia %>%
  filter(population<7000)%>%
  ggplot(mapping = aes(x = population)) + 
  geom_histogram(binwidth = 50)+
  theme_bw()+
  labs(title=" ", x=PopL, y="Count") +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()

#4.2---------------------------#
plotname	       <- paste("DMSP-histogram.png",sep="")
png(plotname, width=6, height=6, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = b1_DMSP_20)) + 
  geom_histogram(binwidth = 2)+
  theme_bw()+
  labs(title=" ", x="DMSP Night Light", y="Count") +
  theme(plot.title=element_text(hjust = 0.5,size=14), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()

#4.3---------------------------#
plotname	       <- paste("TROP-histogram.png",sep="")
png(plotname, width=6, height=6, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = b1_TROPOMI)) + 
  geom_histogram(binwidth = 1,show.legend=FALSE)+
  theme_bw()+
  labs(title=" ", x=TroL, y="Count") +
  theme(plot.title=element_text(hjust = 0.5,size=0), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()


#4.4---------------------------#
plotname	       <- paste("Pop-boxplot.png",sep="")
png(plotname, width=7, height=5, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = F_CODE_DES, y = population, fill = F_CODE_DES)) + 
  geom_boxplot(show.legend=FALSE)+
  theme_bw()+
  labs(title="Mean", x=" ", y=PopL) +
  theme(plot.title=element_text(hjust = 0.5,size=14), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()


#4.5---------------------------#
plotname	       <- paste("DMSP-boxplot.png",sep="")
png(plotname, width=7, height=5, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = F_CODE_DES, y = b1_DMSP_20, fill = F_CODE_DES)) + 
  geom_boxplot(show.legend=FALSE)+
  theme_bw()+
  labs(title="Mean", x=" ", y=DMSPL) +
  theme(plot.title=element_text(hjust = 0.5,size=14), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()


#4.6---------------------------#
plotname	       <- paste("TRO-boxplot.png",sep="")
png(plotname, width=7, height=5, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = F_CODE_DES, y = b1_TROPOMI, fill = F_CODE_DES)) + 
  geom_boxplot(show.legend=FALSE)+
  theme_bw()+
  labs(title="Mean", x=" ", y=TroL) +
  theme(plot.title=element_text(hjust = 0.5,size=14), 
        #, face="bold",, fill = F_CODE_DES
        axis.text.x=element_text(angle = 0,size=10, colour = "black"), 
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
  )
dev.off()

#4.7---------------------------#
anova_one_way <- aov(b1_TROPOMI~factor(F_CODE_DES), data = Asia)
summary(anova_one_way)
anova_one_way <- aov(b1_DMSP_20~factor(F_CODE_DES), data = Asia)
summary(anova_one_way)
anova_one_way <- aov(population~factor(F_CODE_DES), data = Asia)
summary(anova_one_way)