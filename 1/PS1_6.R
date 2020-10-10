#6.1
setwd('G:/Sustech/Work/CPER/Assignment/1')
Wea_data <- read.csv("2281305.csv", header = T)
Time <- as.Date(Wea_data$DATE)
VIS <- Wea_data$VIS

Disqual <- substr(VIS ,8 ,8 )
DQ <- as.numeric(Disqual)
Var <- substr(VIS ,10 , 10)
Qualvar <- substr(VIS , 12, 12)
QUA <- as.numeric(Qualvar)
#Êý¾ÝÉ¸Ñ¡
Dis <- as.numeric(substr(VIS, 1, 6))
Dis[which(Dis < 0 |Dis > 160000 | DQ != 1 | Var != 'N' | QUA != 1)] <- NA
plot(Time, Dis, type = 'l', col='RED', lwd = 0.1)


#6.2
library(ggplot2)
distance2 <- Dis
max(distance2,na.rm = T)
daycode <- as.numeric(Time)
year <- substr(Time,1,4)
year_days <- c()

sumdays <- function(a,b=Inf){
  year_days <<- c()
  for (i in unique(year)) {
    temp <- 0
    daycode1 <- daycode[which(year == i)] 
    for (j in unique(daycode1)) {
      m1 <- max(distance2[which(daycode == j)],na.rm = T)
      if(is.na(m1))
        m1 <- 0
      if(m1 >= a && m1 < b){
        temp = temp + 1
      }
    }
    year_days <<- c(year_days, temp)
  }
  return(year_days)
}

sday1 <- sumdays(0,5000)
sday2 <- sumdays(5000,10000)
sday3 <- sumdays(10000,15000)
sday4 <- sumdays(15000,20000)
sday5 <- sumdays(20000,25000)
sday6 <- sumdays(25000,30000)
sday7 <- sumdays(30000,99999)
x1 <- barplot(sday1,names.arg=unique(year),xlab="year",ylab="Days",
             main=paste("Visibility chart in" , 0,"to" ,5000 ,'(m)'))
lbls<-paste(" ",sday1)
x2 <- barplot(sday2,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 5000,"to" ,10000 ,'(m)'))
lbls<-paste(" ",sday2)
x3 <- barplot(sday3,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 10000,"to" ,15000 ,'(m)'))
lbls<-paste(" ",sday3)
x4 <- barplot(sday4,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 15000,"to" ,20000 ,'(m)'))
lbls<-paste(" ",sday4)
x5 <- barplot(sday5,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 20000,"to" ,25000 ,'(m)'))
lbls<-paste(" ",sday5)
x6 <- barplot(sday6,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 25000,"to" ,30000 ,'(m)'))
lbls<-paste(" ",sday6)
x7 <- barplot(sday7,names.arg=unique(year),xlab="year",ylab="Days",
              main=paste("Visibility chart in" , 30000,"to" ,99999 ,'(m)'))
