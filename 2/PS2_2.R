setwd("D:/ESE5023")
library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
BaoAn_Wind0 <- read.csv(file = "2281305.csv", header = T)
BaoAn_Wind <- as_tibble(BaoAn_Wind0)
Wind_Fil <- BaoAn_Wind %>%
  select(DATE, WND) %>%
  separate(WND, c("ANG","QC","TYRY", "SPEEDR","S_QC"),sep = ",") %>%
  filter(QC == 1 & S_QC==1 & SPEEDR!=9999) %>%
  filter(!is.na(SPEEDR)) %>%
  separate(DATE,c("Y","M","D","H"),sep = "[T-]") %>%
  unite("YM",Y:M,sep="-")
Wind_monthly <- Wind_Fil %>%
  group_by(YM) %>%
  summarize(SPEEDR_MM = mean(as.numeric(SPEEDR))) 
barplot(Wind_monthly$SPEEDR_MM,names.arg = as.Date(paste(Wind_monthly$YM,"01", sep = "-")),
        xlab = "Data",ylab = "Monthly Mean Wind Speed")
#plot(as.Date(paste(Wind_monthly$YM,"01", sep = "-")),Wind_monthly$SPEEDR_MM)
  #ggplot(aes(x=as.Date(paste(YM,"01", sep = "-"), format = "%Y-%m-%d"), y=SPEEDR_MM)) + 
  #geom_line(na.rm=TRUE) +
  