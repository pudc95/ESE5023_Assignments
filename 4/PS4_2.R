library(dplyr)
library(ggplot2)
library(astsa)
library(lubridate)
library(forecast)
library(tidyr)
#2.1
setwd("G:/Sustech/Work/CPER/Assignment/4")
BA_Weather  <- read.csv(file = "2281305.csv", header = T)
Weather <- as_tibble(BA_Weather)
Temp_mon_mean <- Weather %>% 
  mutate(temp = ifelse((substr(TMP,7,7) == 2 | substr(TMP,7,7) == 3 |
                          substr(TMP,7,7) == 6 | substr(TMP,7,7) == 7 |
                          substr(TMP,1,5) == '+9999'),
                       NA,as.numeric(substr(TMP,1,5)) / 10)) %>% 
  mutate(date2 = as.numeric(paste(substr(DATE,1,4),
                                  substr(DATE,6,7),
                                  sep = ""))) %>% 
  filter(date2 >= 201001 & date2 <= 202008) %>% 
  group_by(date2) %>% 
  summarise(Temp_mean = mean(temp,na.rm = T))
Temp_mon_mean %>%
  mutate(Index= c(1:128), Mon = substr(date2,5,6)) %>%
  ggplot(aes(x = Index, y = Temp_mean)) + 
  geom_point() +
  geom_line() +
  labs(title="Monthly Temperature Change", x="Month", y="Temperature (°„C)") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5,size=16, face="bold"), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
  )   

#2.2
Temp_m <- ts(Temp_mon_mean$Temp_mean, start=c(2010,1), frequency=12)
Temp <- decompose(Temp_m)
plot(Temp)
hist(Temp$random, prob=TRUE,ylim = range(0:1))
curve(dnorm(x, 
            mean=mean(Temp$random,na.rm=T),
            sd=sd(Temp$random,na.rm=T)),
      add=TRUE, 
      col="blue")

#2.3
acf(Temp_mon_mean$Temp_mean)
pacf(Temp_mon_mean$Temp_mean)
model <- auto.arima(Temp_mon_mean$Temp_mean)
model

#2.4
forecast_t <- forecast(model, 10)
forecast_t

forecastSep <- forecast_t$mean[1]
forecastSep
forecastOct <- forecast_t$mean[2]
forecastOct

Weather %>% 
  mutate(temp = ifelse((substr(TMP,7,7) == 2 | substr(TMP,7,7) == 3 |
                          substr(TMP,7,7) == 6 | substr(TMP,7,7) == 7 |
                          substr(TMP,1,5) == '+9999'),
                       NA,as.numeric(substr(TMP,1,5)) / 10)) %>% 
  mutate(date2 = as.numeric(paste(substr(DATE,1,4),
                                  substr(DATE,6,7),
                                  sep = ""))) %>% 
  filter(date2 >= 202009 & date2 <= 202010) %>% 
  group_by(date2) %>% 
  summarise(Temp_mean = mean(temp,na.rm = T))

