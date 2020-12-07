library(raster)
library(sp)
library(rgdal)
library(sf)
library(maps)
library(mapdata)
library(ggplot2)
setwd('D:/ESE5023/')
#1.1&1.2---------
#windspeed----
path <- "D:/ESE5023/wc2.1_2.5m_wind"
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})   
wind <- lapply(filePath, function(x){
  raster(x)})  

wind_sum<-wind$wc2.1_2.5m_wind_01.tif
for(i in 2:length(fileNames)){
  wind_sum<-wind[[i]]+wind_sum
}
wind_avg<-wind_sum/length(fileNames)

China=rgdal::readOGR('bou2_4p.shp')
Chinasolor<- China %>%
  crop(wind_avg,.)
wind_avg_crop <- Chinasolor %>%
  mask(China , na.rm=TRUE)

Crop_box <- c(70,140,10,65)
wind_avg_crop <- crop(wind_avg_crop, Crop_box)
par(mar=c(4.5,3,2,1))
plot(wind_avg_crop,
     main="Wind speed in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Wind speed [m s-1]",cex=1),
     col=heat.colors(100)
)
#srad----
path1 <- "D:/ESE5023/wc2.1_2.5m_srad"
fileNames1 <- dir(path1) 
filePath1 <- sapply(fileNames1, function(x){ 
  paste(path1,x,sep='/')})   
srad <- lapply(filePath1, function(x){
  raster(x)})  

srad_sum<-srad$wc2.1_2.5m_srad_01.tif
for(i in 2:length(fileNames1)){
  srad_sum<-srad[[i]]+srad_sum
}
srad_avg<-srad_sum/length(fileNames1)

Chinasolor1<- China %>%
  crop(srad_avg,.)
srad_avg_crop <- Chinasolor1 %>%
  mask(China , na.rm=TRUE)

Crop_box <- c(70,140,10,65)
srad_avg_crop <- crop(srad_avg_crop, Crop_box)
par(mar=c(4.5,3,2,1))
plot(srad_avg_crop,
     main="Solar radiation in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Solar radiation [kJ m-2 day-1]",cex=1),
     col=heat.colors(100)
)
#prec----
path2 <- "D:/ESE5023/wc2.1_2.5m_prec"
fileNames2 <- dir(path2) 
filePath2 <- sapply(fileNames2, function(x){ 
  paste(path2,x,sep='/')})   
prec <- lapply(filePath2, function(x){
  raster(x)})  

prec_sum<-prec$wc2.1_2.5m_prec_01.tif
for(i in 2:length(fileNames2)){
  prec_sum<-prec[[i]]+prec_sum
}
prec_avg<-prec_sum/length(fileNames2)

Chinasolor2<- China %>%
  crop(prec_avg,.)
prec_avg_crop <- Chinasolor2 %>%
  mask(China , na.rm=TRUE)

Crop_box <- c(70,140,10,65)
prec_avg_crop <- crop(prec_avg_crop, Crop_box)
par(mar=c(4.5,3,2,1))
plot(prec_avg_crop,
     main="Precipitation in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Precipitation [mm]",cex=1),
     col=heat.colors(100)
)
#1.3
exactChnWt <- wind_avg_crop
mean_wind <- mean(exactChnWt@data@values, na.rm = T)
exactChnWt@data@values[exactChnWt@data@values <= 1.5*mean_wind] <- 0
exactChnWt@data@values[exactChnWt@data@values > 1.5*mean_wind] <- 1

col = c('grey','green')
plot(exactChnWt,
     col = col,
     legend = F)
title(main=paste("Potential Locations of Wind Farms"))
legend("right",legend = c('Bad','Good'),fill = col)

#1.4
exactChnPT <- prec_avg_crop
exactChnST <- srad_avg_crop

mean_solar <- mean(exactChnST@data@values,na.rm = T)
mean_Per <- mean(exactChnPT@data@values,na.rm = T)

exactChnWt@data@values[(exactChnST@data@values <= 1.1*mean_solar) |
                         (exactChnPT@data@values >= 0.9*mean_Per)] <- 0

exactChnWt@data@values[(exactChnST@data@values > 1.1*mean_solar) &
                         (exactChnPT@data@values < 0.9*mean_Per)] <- 1

col = c('grey','green')
plot(exactChnWt,
     col = col,
     legend = F)
title(main=paste("Potential Locations of PV Farms"),)
legend("right",legend = c('Bad','Good'),fill = col)