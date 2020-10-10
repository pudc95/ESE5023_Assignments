#7.1
setwd('G:/Sustech/Work/CPER/Assignment/1')
DMSP <- read.csv("DMSP.csv", header = T)
#7.2
China_Light <- DMSP$Light
Year<- DMSP$Year
plot(Year, China_Light, type = 'l',
     col='RED', lwd = 0.5, main = 'China TNL in 1992---2012',
     xlab = "Year",ylab = 'Total night light')

median(China_Light)
mean(China_Light)
range(China_Light)
max(China_Light)
min(China_Light)