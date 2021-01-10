library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
library(fields)
setwd("D:/ESE5023/Pre")

Asia <- read.csv(file = "ASIA1.csv", header = T)
Asia1 <- as_tibble(Asia)
Asia <- Asia1 %>%
  mutate(TROP = as.numeric(TROPOMI_HC))

TroL <- expression(paste("HCHO Column Density [",10^15, " molecules ",cm^-2,"]"))
PopL <- expression(paste("Population Density [",km^-2,"]"))
DMSPL <- expression(paste("DMSP Night Light"))


plotname	       <- paste("DMSP8-boxplot.png",sep="")
png(plotname, width=7, height=5, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = F_CODE_DES, y = b8_DMSP_20, fill = F_CODE_DES)) + 
  geom_boxplot(show.legend=FALSE)+
  theme_bw()+
  labs(title="Correlation", x=" ", y=DMSPL) +
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
#---------------------------#
plotname	       <- paste("TRO8-boxplot.png",sep="")
png(plotname, width=7, height=5, units="in", res=500)
Asia %>%
  ggplot(mapping = aes(x = F_CODE_DES, y = b8_TROPOMI, fill = F_CODE_DES)) + 
  geom_boxplot(show.legend=FALSE)+
  theme_bw()+
  labs(title="Correlation", x=" ", y=TroL) +
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

#---------------------------#
Mean_DES <- Asia %>%
  group_by(F_CODE_DES) %>%
  summarise(pop_m=mean(population),DMSP_m=mean(b1_DMSP_20),
            TRO_m=mean(b1_TROPOMI))
Mean_DES
cor(Mean_DES$pop_m, Mean_DES$TRO_m, method = "pearson")
cor(Mean_DES$pop_m, Mean_DES$TRO_m, method = "spearman")
cor(Mean_DES$pop_m, Mean_DES$TRO_m, method = "kendall")

cor(Mean_DES$DMSP_m, Mean_DES$TRO_m, method = "pearson")
cor(Mean_DES$DMSP_m, Mean_DES$TRO_m, method = "spearman")
cor(Mean_DES$DMSP_m, Mean_DES$TRO_m, method = "kendall")

cor(Mean_DES$DMSP_m, Mean_DES$pop_m, method = "pearson")
cor(Mean_DES$DMSP_m, Mean_DES$pop_m, method = "spearman")
cor(Mean_DES$DMSP_m, Mean_DES$pop_m, method = "kendall")

plotname	       <- paste("pop-tro-mean.png",sep="")
png(plotname, width=5.1, height=5, units="in", res=500)
fit <- lm(Mean_DES$pop_m ~ Mean_DES$TRO_m)
plot(Mean_DES$pop_m ~ Mean_DES$TRO_m,
     ylab = PopL,
     xlab = TopL,
     main = " ",
     pch = 20,
     cex = 1.5,
     col = "green")
abline(fit, lwd = 3, col = "red")
points(mean(Mean_DES$pop_m), mean(Mean_DES$TRO_m), pch = "+", cex = 3)
dev.off()

plotname	       <- paste("Dmsp-tro-mean.png",sep="")
png(plotname, width=5.1, height=5, units="in", res=500)
fit <- lm(Mean_DES$DMSP_m ~ Mean_DES$TRO_m)
plot(Mean_DES$DMSP_m ~ Mean_DES$TRO_m,
     ylab = DMSPL,
     xlab = TopL,
     main = " ",
     pch = 20,
     cex = 1.5,
     col = "blue")
abline(fit, lwd = 3, col = "red")
points(mean(Mean_DES$pop_m), mean(Mean_DES$TRO_m), pch = "+", cex = 3)
dev.off()

cor(Asia$population,Asia$b1_DMSP_20, method = "pearson")
cor(Asia$population,Asia$b1_TROPOMI, method = "pearson")
cor(Asia$b1_TROPOMI,Asia$b1_DMSP_20, method = "pearson")