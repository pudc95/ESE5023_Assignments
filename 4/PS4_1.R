library(ggthemes)
library(fields) 
library(maps)
library(RNetCDF)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(leaps)
library(tidyr)
library(ggplot2)

#1.1 Boxplot
head(InsectSprays)
InsectSprays %>% 
  group_by(spray) %>% 
  ggplot(aes(spray, count, fill=spray))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge())+
  labs(title="Number of insects when using different insecticides") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold",hjust = 0.5), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        )


#1.2 Time series
head(airquality)
airquality1 <- airquality %>%
  mutate(Mon = as.character(airquality$Month))
airquality1 %>%
  #filter( Month=="5"  | Month== "8" )%>%
  ggplot(aes(x = Day, y = Wind, color=Mon)) + 
  geom_point() +
  geom_line() +
  labs(title="Monthly wind speed changes", x="DAY", y="Wind Speed (m/s)") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5,size=16, face="bold"), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        ) 

#1.3 Histogram
ggplot(airquality,aes(x = Temp)) +
  geom_histogram(fill="blue",colour = "black",binwidth = 0.8)+
  xlab("Daily Temperature (¡ãF)")+
  ylab("Count") +
  labs(title="Daily Temperature Distribution") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5,size=16, face="bold"), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))



#1.4 Scatterplot
ggplot(airquality1, aes(x = Temp, y = Wind, color=Mon, shape=Mon))+
  geom_point()+
  xlab("Daily Temperature (¡ãF)")+
  ylab("Daily Wind Speed (m/s)") +
  labs(title=" Daily Temperature vs Daily Wind Speed") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5,size=16, face="bold"), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))

#1.5 Image plot
workingdir             <- "E:\Data_HCHO\TROPOMI_HCHO"
setwd(workingdir)
Lat_common <- open.nc("TROPOMI_HCHO_m.nc")
Lat_common-plot_region <- ("TROPOMI_HCHO_plot_region.Rdata")
Row_down               <- which(abs(Lat_common-plot_region[3])==min(abs(Lat_common-plot_region[3])))[1]
Row_up                 <- which(abs(Lat_common-plot_region[4])==min(abs(Lat_common-plot_region[4])))[1]
Col_left               <- which(abs(Lon_common-plot_region[1])==min(abs(Lon_common-plot_region[1])))[1]
Col_right              <- which(abs(Lon_common-plot_region[2])==min(abs(Lon_common-plot_region[2])))[1]
plotname	       <- paste("figs/TROPOMI_HCHO_May_to_Oct_Res_",as.character(res),"_PL_",as.character(pixel_limit),".png",sep="")
png(plotname, width=8.5, height=6, units="in", res=400) 
par(mar=c(4.5,3,2,1))
image.plot(Lon_common[Col_left: Col_right], Lat_common[Row_down: Row_up], 
           Average_grids[Col_left: Col_right, Row_down: Row_up]/1e15,zlim=c(zmin,zmax),
           horizontal=T,useRaster=T,
           legend.shrink=0.75,axis.args = list(cex.axis = 1.25),legend.width=1,legend.mar=2,
           legend.args=list(text=expression(paste("HCHO column density [",10^15, " molecules ",cm^-2,"]")),cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon_common[Col_left:Col_right]),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat_common[Row_down:Row_up]),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("TROPOMI_HCHO_May_to_Oct_Res_",as.character(res),"_PL_",as.character(pixel_limit),sep=""),cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)
dev.off()

